{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Network.Simple.TCP
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
Stops once we have reached \r\n\r\ which is the termination sequence.
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
            if B.isSuffixOf "\r\n\r\n" someData  -- This means we have an empty line so the communication is done
                then return ()
                else transferData from to

-- | Load balances using a Round Robin strategy.
roundRobin :: IO ()
roundRobin = runCommand $ \opts _ -> do
    let portNumber = portnumber opts
    indexServeur <- newTVarIO 0
    serve HostAny (show portNumber) $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "TCP connection established on the Load Balancer from " ++ show remoteAddr
        (Service addr port) <- atomically $ getNextServer indexServeur
        connect addr  port $ \(connectionSocketClient, remoteAddr) -> do
            putStrLn $ "Connection established to " ++ show remoteAddr
            transferData connectionSocket connectionSocketClient
            putStrLn "Request sent"
            transferData connectionSocketClient connectionSocket
            putStrLn "Response sent"

main :: IO ()
main = roundRobin