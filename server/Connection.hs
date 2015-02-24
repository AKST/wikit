{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Connection where


import Network.URI
import qualified Network.HTTP       as HTTP 
import qualified Network.Stream     as HTTP
import qualified Network.WebSockets as WS

import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent

import API


data ConnOpts = ConnOpts { connection :: WS.Connection }

newtype Conn a = Conn { runConn :: ReaderT ConnOpts (ExceptT ConnError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader ConnOpts, MonadError ConnError, MonadIO)


withOptions :: ConnOpts -> Conn a -> IO (Either ConnError a)
withOptions options conn = runExceptT (runReaderT (runConn conn) options)


runConnection :: ConnOpts -> Conn a -> IO ()
runConnection options conn = forever $ do
  result <- withOptions options conn
  print "finished request"


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
dispatch task = ask >>= \options ->
  --
  -- forks a new thread and if it errors returns an
  -- internal error to the client, so it knowns shit
  -- went down when it didn't get what it wanted
  --
  liftIO . forkIO . void . withOptions options $
    catchError (void task) $ \result -> do
      yieldResponse (WInternalError result)
      liftIO (putStrLn ("An error occured,\n  " ++ show result))


get :: URI -> Conn (HTTP.Response ByteString)
get url = liftIO (HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET url)) >>= \case 
  Left error -> throwError (FailedConnection error)
  Right resp -> return resp
  

