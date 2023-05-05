{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.String
import Data.Time.Clock.System
import Happstack.Server
import Happstack.Server.Types(nullConf, port)
import Options

data MainOptions = MainOptions { portnumber :: Int, ttl :: Int64 }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "portnumber" 8000 "The port number."
        <*> simpleOption "ttl" 120 "Time To Live in seconds. Maximum delay between heartbeats for the service to stay registered."

data ServiceInstance = ServiceInstance 
    { siName :: String
    , siHostname :: String
    , siPortnumber :: String
    } deriving (Eq, Ord, Show)

instance A.ToJSON ServiceInstance where
    -- this generates a Value
    toJSON (ServiceInstance s h p) =
        A.object ["service" A..= s, "hostname" A..= h, "port" A..= p]
    
    -- this encodes directly to a bytestring Builder
    toEncoding (ServiceInstance s h p) =
        A.pairs ("service" A..= s <> "hostname" A..= h <> "port" A..= p)

instance ToMessage ServiceInstance where
    toContentType _ = "application/json"
    toMessage = A.encode

instance ToMessage [ServiceInstance] where
    toContentType _ = "application/json"
    toMessage = A.encode

withinDelay :: Int64 -> SystemTime -> SystemTime -> Bool
withinDelay timeOutDelay (MkSystemTime currentSeconds _ ) (MkSystemTime previousSeconds _ ) = 
    currentSeconds < (previousSeconds + timeOutDelay)

{- | Remove the offline servers from the list.
     They are offline if they have a hearthbeat longer than a timeout.
-}
removeOfflineServers :: Int64 -> TVar (Map.Map ServiceInstance SystemTime) -> IO ()
removeOfflineServers timeOutDelay services = do
    currentTime <- getSystemTime
    let isntTimedOut = withinDelay timeOutDelay currentTime
    -- TODO: Log removed servers
    atomically $ modifyTVar' services (Map.filter isntTimedOut)
    threadDelay 1000
    removeOfflineServers timeOutDelay services

{- | Inserts or updates the timestamp on a ServiceInstance -}
upsertServiceList :: ServiceInstance -> TVar (Map.Map ServiceInstance SystemTime) -> IO ()
upsertServiceList service services = do
    time <- getSystemTime
    atomically $ modifyTVar' services (Map.insert service time)

{- | Removes a service instance from the list. -}
deleteService :: ServiceInstance -> TVar (Map.Map ServiceInstance SystemTime) -> IO ()
deleteService service services = atomically $ modifyTVar' services (Map.delete service)

{- | Inserts or updates a ServiceInstance based on the post request information. -}
postHeartbeat :: TVar (Map.Map ServiceInstance SystemTime) -> ServerPartT IO Response
postHeartbeat services = do
    method POST
    s <- look "service"
    h <- look "hostname"
    p <- look "port"
    liftIO $ upsertServiceList (ServiceInstance s h p) services
    ok $ toResponse ("Heartbeat updated." :: String)

filterByServiceName :: String -> Map.Map ServiceInstance SystemTime -> Map.Map ServiceInstance SystemTime
filterByServiceName name = Map.filterWithKey (\(ServiceInstance n _ _) _ -> n==name)

extractHostAndPort :: Map.Map ServiceInstance SystemTime -> [(String, String)]
extractHostAndPort serviceMap = (\(ServiceInstance _ h p) -> (h,p)) <$> Map.keys serviceMap

readServices :: String -> TVar (Map.Map ServiceInstance SystemTime) -> IO [ServiceInstance]
readServices name services = Map.keys <$> filterByServiceName name <$> (atomically $ readTVar services)

{- | Returns all the services of the GET query based on itès name parameter -}
getServices :: TVar (Map.Map ServiceInstance SystemTime) -> ServerPartT IO Response
getServices services = do
    method GET
    name <- look "service"
    hostport <- liftIO $ readServices name services
    ok $ toResponse hostport
    -- TODO: Add 204 noContent

{- | Removes a service instance from the list. -}
deleteServiceInstance :: TVar (Map.Map ServiceInstance SystemTime) -> ServerPartT IO Response
deleteServiceInstance services = do
    method DELETE
    s <- look "service"
    h <- look "hostname"
    p <- look "port"
    liftIO $ deleteService (ServiceInstance s h p) services
    ok $ toResponse ("Service instance deleted." :: String)

setBody :: ServerPart ()
setBody = do
    let bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000
    decodeBody bodyPolicy

-- The cluster manager is used to give information on what servers are available for some tasks
-- A service can be fetched, send a heartbeat, unregister
-- A consumer can get the list of instances of a specific service
main :: IO ()
main = runCommand $ \opts _ -> do
    services <- newTVarIO Map.empty
    let delay = ttl opts
    forkIO $ removeOfflineServers delay services
    let portNumber = portnumber opts
    simpleHTTP (nullConf{ port = portNumber } ) $ 
        setBody >> msum
        [ do postHeartbeat services
        , do getServices services
        , do deleteServiceInstance services
        ]
