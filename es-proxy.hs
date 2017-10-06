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
  { listenPort1   :: Int
  , upstreamPort1 :: Int
  , listenPort2   :: Int
  , upstreamPort2 :: Int
  } deriving (Show, Eq)

configInfo :: ParserInfo Config
configInfo = info config mempty

config :: Parser Config
config = Config
  <$> option auto (long "listen-port-1")
  <*> option auto (long "upstream-port-1")
  <*> option auto (long "listen-port-2")
  <*> option auto (long "upstream-port-2")

main :: IO ()
main = execParser configInfo >>= \Config{..} -> do

  let proxyHalfConnection sourceAppData sinkAppData = runConduit
         $  appSource sourceAppData
        =$= appSink   sinkAppData

  let proxyConnection serverAppData clientAppData = void $ concurrently
        (proxyHalfConnection serverAppData clientAppData)
        (proxyHalfConnection clientAppData serverAppData)

  let runProxy listenPort upstreamPort = do
        putStrLn $ "Proxying " <> show listenPort <> " to upstream " <> show upstreamPort
        runTCPServer (serverSettings listenPort "*") $ \serverAppData ->
          runTCPClient (clientSettings upstreamPort "127.0.0.1") $ \clientAppData -> do
            putStrLn "Opened connection"
            proxyConnection serverAppData clientAppData
            putStrLn "Closed connection"

  void $ concurrently
    (runProxy listenPort1 upstreamPort1)
    (runProxy listenPort2 upstreamPort2)
