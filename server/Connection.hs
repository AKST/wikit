{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Connection where


import qualified Network.WebSockets as WS

import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)

import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Concurrent


import API


data ConnOpts = ConnOpts { connection :: WS.Connection }

newtype Conn a = Conn { runConn :: ReaderT ConnOpts IO a }
  deriving (Functor, Applicative, Monad, MonadReader ConnOpts, MonadIO)


runConnection :: ConnOpts -> Conn a -> IO ()
runConnection options conn = forever $ do
  runReaderT (runConn conn) options 


awaitData :: WS.WebSocketsData a => Conn a
awaitData = do
  socketConnection <- asks connection
  liftIO $ WS.receiveData socketConnection


yieldData :: WS.WebSocketsData a => a -> Conn ()
yieldData response = do
  socketConnection <- asks connection
  liftIO $ WS.sendTextData socketConnection response


yieldResponse :: ToJSON a => a -> Conn ()
yieldResponse response = yieldData (encode response)


dispatch :: Conn a -> Conn ThreadId
dispatch job = do
  options <- ask
  liftIO (forkIO (void (runReaderT (runConn job) options)))

