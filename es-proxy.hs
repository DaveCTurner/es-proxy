{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Concurrent.Async
import Data.Conduit
import Data.Conduit.Network
import Options.Applicative
import Data.Monoid

data Config = Config
  { listenPort :: Int
  , upstreamPort :: Int
  } deriving (Show, Eq)

configInfo :: ParserInfo Config
configInfo = info config mempty

config :: Parser Config
config = Config
  <$> option auto (long "listen-port")
  <*> option auto (long "upstream-port")

main :: IO ()
main = execParser configInfo >>= \Config{..} -> do
  putStrLn $ "Proxying " <> show listenPort <> " to upstream " <> show upstreamPort

  runTCPServer (serverSettings listenPort "*") $ \serverAppData ->
    runTCPClient (clientSettings upstreamPort "127.0.0.1") $ \clientAppData -> do
      putStrLn "Opened connection"
      let serverToClient = runConduit
             $  appSource serverAppData
            =$= appSink   clientAppData
          clientToServer = runConduit
             $  appSource clientAppData
            =$= appSink   serverAppData
      void $ concurrently serverToClient clientToServer
      putStrLn "Closed connection"
