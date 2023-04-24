{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Network.Simple.TCP
import Network.Socket (gracefulClose)
import qualified Data.ByteString as B
import Options

data MainOptions = MainOptions { portnumber :: Int }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "portnumber" 8000
            "The port number."

data Service = Service String String deriving (Eq, Ord, Show)

getAppServers = [ Service "127.0.0.1" "8001", Service "127.0.0.1" "8002", Service "127.0.0.1" "8003"]

{- | Takes the current index value and the maximum one and return the next one.
If we are at the end of the list, we return 0.
-}
nextIndex :: Int -- ^ Current index value
          -> Int -- ^ Maximum value
          -> Int -- ^ The next index to use
nextIndex current listLength
    | current == listLength-1 = 0
    | otherwise               = current + 1

getNextServer :: TVar Int -> STM Service
getNextServer sharedIndex = do
    currentIndex <- readTVar sharedIndex
    let nbServers = length getAppServers
        nextIndexValue = nextIndex currentIndex nbServers
    writeTVar sharedIndex nextIndexValue
    return (getAppServers !! nextIndexValue)

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

-- | Load balances using a Round Robin strategy.
roundRobin :: IO ()
roundRobin = runCommand $ \opts _ -> do
    let portNumber = portnumber opts
    indexServeur <- newTVarIO 0
    serve HostAny (show portNumber) $ \(connectionSocketClient, remoteAddr) -> do
        putStrLn $ "TCP connection established on the Load Balancer from " ++ show remoteAddr
        (Service addr port) <- atomically $ getNextServer indexServeur
        connect addr  port $ \(connectionSocketServer, remoteAddr) -> do
            putStrLn $ "Connection established to " ++ show remoteAddr

            -- The sockets are always blocking while listening so they need to be in their own thread. 
            withAsync (transferData connectionSocketClient connectionSocketServer) $ \client2Server -> do
                putStrLn "Request sent"
                withAsync (transferData connectionSocketServer connectionSocketClient) $ \server2client -> do
                    putStrLn "Response sent"
                    wait client2Server
                    gracefulClose connectionSocketServer 1000-- Once the client disconnected, we disconnect the server.
                    wait server2client
            

main :: IO ()
main = roundRobin