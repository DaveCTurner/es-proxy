{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent.Async
import Data.Conduit
import Data.Conduit.Serialization.Binary
import Data.Conduit.Network
import Options.Applicative
import Data.Monoid
import Data.Word8
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad.IO.Class

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

fixedByte :: Word8 -> Get ()
fixedByte expected = do
  actual <- getWord8
  unless (actual == expected) $ fail "fixedByte"

framed :: (Monad m, MonadThrow m) => Conduit B.ByteString m BL.ByteString
framed = conduitGet $ do
  fixedByte _E
  fixedByte _S
  l <- getWord32be
  if l == maxBound
    then return BL.empty
    else getLazyByteString (fromIntegral l)

addFraming :: Monad m => Conduit BL.ByteString m B.ByteString
addFraming = do
  yield $ B.pack [_E, _S]
  awaitForever $ \msg ->
    let l = fromIntegral $ BL.length msg
    in yield $ BL.toStrict $ runPut $ do
          putWord32be $ if l == 0 then maxBound else l
          putLazyByteString msg
          putWord8 _E
          putWord8 _S

logIncoming :: (Monad m, MonadIO m) => Conduit B.ByteString m B.ByteString
logIncoming = awaitForever $ \bs -> do
  liftIO $ putStrLn $ (printf "incoming %08x:" (B.length bs)
    <> concat [ printf " %02x" b | b <- B.unpack bs ])
  yield bs

logMessage :: (Monad m, MonadIO m) => Conduit BL.ByteString m BL.ByteString
logMessage = awaitForever $ \bs -> do
  liftIO $ putStrLn $ printf "message  %08x processed" (BL.length bs + 6)
  yield bs

main :: IO ()
main = execParser configInfo >>= \Config{..} -> do

  let proxyHalfConnection sourceAppData sinkAppData = runConduit
         $  appSource sourceAppData
        =$= logIncoming
        =$= framed
        =$= logMessage
        =$= addFraming
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
