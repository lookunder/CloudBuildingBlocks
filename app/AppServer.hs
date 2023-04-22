{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server (simpleHTTP, ok)
import Happstack.Server.Types(nullConf, port)
import Options

data MainOptions = MainOptions { portnumber :: Int }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "portnumber" 8000
            "The port number."

-- Simple app server that can listen on a specific port and return 200
main :: IO ()
main = runCommand $ \opts _ -> do
    let portNumber = portnumber opts
    simpleHTTP (nullConf{ port = portNumber } ) ( ok (show portNumber))
