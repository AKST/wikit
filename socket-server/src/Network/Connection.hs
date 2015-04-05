{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Connection (

  ConnOpts,
  Conn,
  initWS,
  withOptions,
  runConnection,
  awaitData,
  yieldData,
  yieldResponse,
  dispatchVoid,
  dispatch,
  closeConnection,
  get,
  log

) where

import Prelude hiding (log)

import System.Locale
import qualified System.Log.Logger as L

import Network.URI
import qualified Network.HTTP       as HTTP 
import qualified Network.Stream     as HTTP
import qualified Network.WebSockets as WS

import Data.API
import Data.Time 
import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)

import qualified Data.Text as T

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent

import Util.Common



data ConnOpts = ConnOpts { connection :: WS.Connection }

newtype Conn a = Conn { runConn :: ReaderT ConnOpts (ExceptT ConnError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader ConnOpts, MonadError ConnError, MonadIO)


initWS address port onConnection = do
  L.updateGlobalLogger "wikit" (L.setLevel L.INFO)
  ioLog L.INFO ("live @ ws://" ++ address ++ ":" ++ show port)

  WS.runServer address port $ \pending -> do
    connection <- WS.acceptRequest pending
    WS.forkPingThread connection 30
    --
    -- This runs forever, and wil decode incoming requests,
    -- if unsuccessful it will just return a parse error to
    -- the client, otherwise it will handle the request as normal
    -- 
    runConnection (ConnOpts connection) onConnection


withOptions :: ConnOpts -> Conn a -> IO (Either ConnError a)
withOptions options conn = runExceptT (runReaderT (runConn conn) options)


runConnection :: ConnOpts -> Conn a -> IO ()
runConnection options conn = forever (withOptions options conn)


awaitData :: WS.WebSocketsData a => Conn a
awaitData = do
  socketConnection <- asks connection
  liftIO (WS.receiveData socketConnection)

yieldData :: WS.WebSocketsData a => a -> Conn ()
yieldData response = do
  socketConnection <- asks connection
  liftIO (WS.sendTextData socketConnection response)

closeConnection :: FatalError -> Conn ()
closeConnection reason = do
  socketConnection <- asks connection
  liftIO (WS.sendClose socketConnection (encode reason)) 

yieldResponse :: ToJSON a => a -> Conn ()
yieldResponse response = yieldData (encode response)

dispatchVoid :: Conn a -> Conn ()
dispatchVoid = void . dispatch

dispatch :: Conn a -> Conn ThreadId
dispatch task = ask >>= \options ->
  --
  -- forks a new thread and if it errors returns an
  -- internal error to the client, so it knowns shit
  -- went down when it didn't get what it wanted
  --
  liftIO . forkIO . void . withOptions options $
    catchError (void task) $ \result -> do
      log L.ERROR ("An error occured,\n  " ++ show result)
      closeConnection UnknownFatalError



{-- UTILITY --}


get :: URI -> Conn (HTTP.Response ByteString)
get url = liftIO (HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET url)) >>= \case 
  Left error -> throwError (FailedConnection error)
  Right resp -> return resp
  

log :: L.Priority -> String -> Conn ()
log ltype message = liftIO (ioLog ltype message) 


ioLog :: L.Priority -> String -> IO ()
ioLog ltype message = do 
  epoch <- formatTime defaultTimeLocale "%s" <$> getCurrentTime
  L.logM "wikit.socket" ltype ("[" ++ show ltype ++ " @ " ++ epoch ++ "] "  ++ message)


