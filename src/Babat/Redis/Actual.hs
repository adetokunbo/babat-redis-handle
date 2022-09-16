{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Provides a Handle that provides remote hashtable service that actually uses Redis
-}
module Babat.Redis.Actual (
  -- * Handle creation
  new,

  -- * Configuration
  Config (..),
  parseConfig,
  parseLocator,

  -- * constants
  disabledRedisUrl,
  fallbackMaxConns,
  fallbackExpiry,

  -- * module re-exports
  module Babat.Redis.Types,
) where

import Babat.Redis.Types
import Control.Exception (throwIO)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Database.Redis (
  ConnectInfo (..),
  Connection,
  Redis,
  Reply (..),
  Status (..),
  checkedConnect,
  del,
  disconnect,
  get,
  hdel,
  hget,
  hgetall,
  hmset,
  hset,
  keys,
  parseConnectInfo,
  runRedis,
  setex,
 )


-- | Obtain a @ConnectInfo@ from a Redis Url and max connections
parseLocator :: Int -> String -> IO (Maybe ConnectInfo)
parseLocator _ l | l == disabledRedisUrl = pure Nothing
parseLocator maxConns l = do
  let parseConnectInfo' = either invalidLocator pure . parseConnectInfo
      setMaxConns x cfg = cfg {connectMaxConnections = x}
  Just . setMaxConns maxConns <$> parseConnectInfo' l


-- | Obtain a @Config@ from a Redis Url, default expiry and  max connections
parseConfig :: Integer -> Int -> String -> IO (Maybe Config)
parseConfig expiry maxConns l = do
  parseLocator maxConns l >>= \case
    Nothing -> pure Nothing
    Just info -> pure $ Just $ Config expiry info

invalidLocator :: String -> IO a
invalidLocator x = throwIO $ userError $ "REDIS connection url: " ++ x ++ " is invalid"


-- | Represents a redis URL and additional parameters to be used on its connection.
data Config = Config
  { configDefaultExpiry :: !Integer
  , configConnectInfo :: !ConnectInfo
  }
  deriving (Show)


-- | Create a new 'Handle'.
new :: MonadUnliftIO m => Config -> m (Handle m)
new config = do
  conn <- openConfigConnection config
  let inner = InnerHandle conn $ configDefaultExpiry config
  pure $
    Handle
      { hClose = hClose' conn
      , hLoadValue = hLoadValue' conn
      , hSaveValue = hSaveValue' inner
      , hLoadDict = hLoadDict' conn
      , hSaveDict = hSaveDict' inner
      , hLoadDictValue = hLoadDictValue' conn
      , hSaveDictValue = hSaveDictValue' inner
      , hDeleteKeys = hDeleteKeys' conn
      , hDeleteDictKeys = hDeleteDictKeys' conn
      , hDeleteMatchingKeys = hDeleteMatchingKeys' conn
      }


{- | The disabled redis url.

When REDIS_URL has this value, use of the internal implementation is forced.
-}
disabledRedisUrl :: String
disabledRedisUrl = "redis://redis-is-disabled"


data InnerHandle = InnerHandle
  { -- | The connection to redis
    ihConnection :: !Connection
  , -- | The expiry period
    ihDefaultExpiry :: !Integer
  }


hClose' :: MonadUnliftIO m => Connection -> m ()
hClose' = liftIO . disconnect


hLoadValue' ::
  MonadUnliftIO m =>
  Connection ->
  RemoteKey ->
  m (Either HTSException (Maybe RemoteValue))
hLoadValue' conn key = doFetch conn $ get key


hSaveValue' ::
  MonadUnliftIO m =>
  InnerHandle ->
  RemoteKey ->
  RemoteValue ->
  m (Either HTSException ())
hSaveValue' ih key value =
  doStore (ihConnection ih) $ setex key (ihDefaultExpiry ih) value


hLoadDictValue' ::
  MonadUnliftIO m =>
  Connection ->
  RemoteKey ->
  RemoteKey ->
  m (Either HTSException (Maybe RemoteValue))
hLoadDictValue' conn key dictKey = doFetch conn $ hget key dictKey


hLoadDict' ::
  MonadUnliftIO m =>
  Connection ->
  RemoteKey ->
  m (Either HTSException RemoteDict)
hLoadDict' conn key = doFetch conn $ hgetall key <&> fmap Map.fromList


hSaveDictValue' ::
  MonadUnliftIO m =>
  InnerHandle ->
  RemoteKey ->
  RemoteKey ->
  RemoteValue ->
  m (Either HTSException ())
hSaveDictValue' ih key dictKey value =
  doStore' (ihConnection ih) $ hset key dictKey value


hSaveDict' ::
  MonadUnliftIO m =>
  InnerHandle ->
  RemoteKey ->
  RemoteDict ->
  m (Either HTSException ())
hSaveDict' ih key dict = do
  _ <- hDeleteKeys' (ihConnection ih) [key]
  doStore' (ihConnection ih) $ hmset key $ Map.toList dict


hDeleteKeys' ::
  MonadUnliftIO m =>
  Connection ->
  [RemoteKey] ->
  m (Either HTSException ())
hDeleteKeys' conn ks = doStore' conn $ del ks


hDeleteMatchingKeys' ::
  MonadUnliftIO m =>
  Connection ->
  RemoteKey ->
  m (Either HTSException ())
hDeleteMatchingKeys' conn patt = do
  doFetch conn (keys patt) >>= \case
    Left e -> pure $ Left e
    Right [] -> pure $ Right ()
    Right ks -> hDeleteKeys' conn ks


hDeleteDictKeys' ::
  MonadUnliftIO m =>
  Connection ->
  RemoteKey ->
  [RemoteKey] ->
  m (Either HTSException ())
hDeleteDictKeys' conn key dictKeys = doStore' conn $ hdel key dictKeys


openConfigConnection :: MonadUnliftIO m => Config -> m Connection
openConfigConnection c = liftIO $ checkedConnect $ configConnectInfo c


fallbackExpiry :: Integer
fallbackExpiry = 129600


fallbackMaxConns :: Int
fallbackMaxConns = 10


toHTSException :: Reply -> HTSException
toHTSException = Unanticipated . Text.pack . show


doStore ::
  MonadIO m =>
  Connection ->
  Redis (Either Reply Status) ->
  m (Either HTSException ())
doStore conn action = liftIO $ leftHTS $ runRedis conn action


leftHTS :: Monad m => m (Either Reply Status) -> m (Either HTSException ())
leftHTS x =
  x >>= \case
    (Left l) -> pure $ Left $ toHTSException l
    Right Ok -> pure $ Right ()
    Right Pong -> pure $ Right ()
    Right (Status err) -> pure $ Left $ Unanticipated $ Text.pack $ show err


doStore' ::
  MonadIO m =>
  Connection ->
  Redis (Either Reply a) ->
  m (Either HTSException ())
doStore' conn action = liftIO $ leftHTS'' $ runRedis conn action


leftHTS'' :: Monad m => m (Either Reply a) -> m (Either HTSException ())
leftHTS'' x =
  x >>= \case
    (Left l) -> pure $ Left $ toHTSException l
    Right _ -> pure $ Right ()


doFetch ::
  MonadUnliftIO m =>
  Connection ->
  Redis (Either Reply a) ->
  m (Either HTSException a)
doFetch conn = liftIO . leftHTS' . runRedis conn


leftHTS' :: Monad m => m (Either Reply a) -> m (Either HTSException a)
leftHTS' = (<&> either (Left . toHTSException) Right)
