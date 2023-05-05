{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Aeson as A
import Network.Simple.TCP
import Network.Socket (gracefulClose)
import qualified Data.ByteString as B
import Options

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

data MainOptions = MainOptions { portnumber :: Int, clustermanagerhost :: String, clustermanagerport :: Int, clustermanagerapp :: String }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "portnumber" 8000 "The port number."
        <*> simpleOption "clustermanagerhost" "127.0.0.1" "The cluster manager host."
        <*> simpleOption "clustermanagerport" 8000 "The cluster manager port."
        <*> simpleOption "clustermanagerapp" "" "The name of the application to balance."

data Service = Service String String deriving (Eq, Ord, Show)

instance A.FromJSON Service where
    parseJSON (A.Object v) = Service
        <$> v A..: "hostname"
        <*> v A..: "port"

{- | Takes the current index value and the maximum one and return the next one.
If we are at the end of the list, we return 0.
-}
nextIndex :: Int -- ^ Current index value
          -> Int -- ^ Maximum value
          -> Int -- ^ The next index to use
nextIndex current listLength
    | current == listLength-1 = 0
    | otherwise               = current + 1

getNextServer :: TVar Int -> TVar [Service] -> STM (Maybe Service)
getNextServer sharedIndex tservices= do
    currentIndex <- readTVar sharedIndex
    services <- readTVar tservices
    let nbServers = length services
        nextIndexValue = nextIndex currentIndex nbServers
    writeTVar sharedIndex nextIndexValue
    case nbServers of
        0 -> return Nothing
        _ -> return $ Just (services !! nextIndexValue)

{- | Recursively copies data from one socket to another.
     Stops when the connection closes.
-}
transferData :: Socket -- ^ The socket we are getting the data from.
             -> Socket -- ^ The socket we are getting the data to.
             -> IO()
transferData from to = do
    maybeSomeData <- recv from 1024
    case maybeSomeData of
        Nothing       -> return ()
        (Just someData) -> do
            send to someData
            if B.null someData
                then return ()
                else transferData from to

-- TODO: Log to a distributed logging system.
updateServerList :: String -> Int -> String -> TVar [Service] -> IO ()
updateServerList h p appname services = do

    -- Connect to the cluster manager
    initReq <- parseRequest $ "http://" ++ h ++ ":"++ show p ++"/?service=" ++ appname
    -- Get the list for the service it load balances
    response <- httpJSONEither initReq
    -- FIXME : Don't throw exception if server is not reachable.

    --putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    --putStrLn $ getResponseHeader "Content-Type" response

    let eitherContent = getResponseBody response :: Either JSONException [Service]
    --putStrLn $ show  eitherContent
  
    -- Update the list
    case eitherContent of
        (Right s) -> void $ atomically $ swapTVar services s
        (Left l)  -> putStrLn $ show l

    threadDelay 60000 -- 1 minute
    updateServerList h p appname services 

handleConnection :: Socket -> String -> String -> IO ()
handleConnection connectionSocketClient addr port = do
    connect addr port $ \(connectionSocketServer, remoteAddr) -> do
        putStrLn $ "Connection established to " ++ show remoteAddr

        -- The sockets are always blocking while listening so they need to be in their own thread. 
        withAsync (transferData connectionSocketClient connectionSocketServer) $ \client2Server -> do
            putStrLn "Request sent"
            withAsync (transferData connectionSocketServer connectionSocketClient) $ \server2client -> do
                putStrLn "Response sent"
                wait client2Server
                gracefulClose connectionSocketServer 1000-- Once the client disconnected, we disconnect the server.
                wait server2client


-- | Load balances using a Round Robin strategy.
roundRobin :: IO ()
roundRobin = runCommand $ \opts _ -> do
    let portNumber = portnumber opts
        clusterManagerHost = clustermanagerhost opts
        clustermanagerPost = clustermanagerport opts
        appname = clustermanagerapp opts
    serverList <- newTVarIO [] :: IO (TVar [Service])
    forkIO $ updateServerList clusterManagerHost clustermanagerPost appname serverList
    indexServeur <- newTVarIO 0
    serve HostAny (show portNumber) $ \(connectionSocketClient, remoteAddr) -> do
        putStrLn $ "TCP connection established on the Load Balancer from " ++ show remoteAddr
        mService <- atomically $ getNextServer indexServeur serverList

        case mService of
            Nothing -> return ()
            (Just (Service addr port)) -> handleConnection connectionSocketClient addr port

main :: IO ()
main = roundRobin