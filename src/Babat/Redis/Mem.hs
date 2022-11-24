{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Defines Handle for use in tests when the redis is not accessible.
-}
module Babat.Redis.Mem (
  -- * functions
  new,

  -- * module re-exports
  module Babat.Redis.Types,
) where

import Babat.Redis.Types
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Data.ByteString.Lazy (fromStrict)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import Redis.Glob (matches)
import UnliftIO.STM (
  STM,
  TVar,
  atomically,
  newTVarIO,
  readTVar,
  writeTVar,
 )


-- | Create a new 'Handle'.
new :: MonadUnliftIO m => m (Handle m)
new = do
  v <- liftIO $ newTVarIO (mempty, False)
  pure $
    Handle
      { hLoadValue = hLoadValue' v
      , hSaveValue = hSaveValue' v
      , hLoadDict = hLoadDict' v
      , hSaveDict = hSaveDict' v
      , hLoadDictValue = hLoadDictValue' v
      , hLoadDictPart = hLoadDictPart' v
      , hSaveDictValue = hSaveDictValue' v
      , hSaveDictPart = hSaveDictPart' v
      , hDeleteKeys = hDeleteKeys' v
      , hDeleteDictKeys = hDeleteDictKeys' v
      , hDeleteMatchingKeys = hDeleteMatchingKeys' v
      , hLengthDict = hLengthDict' v
      , hClose = hClose' v
      }


-- | Implement a @hashtable service@ as a fallback for non-production situations.
type FakeHash = Map RemoteKey FakeValue


-- | Implement a @hashtable service@ as a fallback for non-production situations.
type FakeHashVar = TVar (FakeHash, Bool)


{- | FakeValue represents a value that allows a 'Map' to act as a dictionary server
  implementation.
-}
data FakeValue
  = Dict RemoteDict
  | Simple RemoteValue


hClose' :: MonadUnliftIO m => FakeHashVar -> m ()
hClose' var = liftIO $
  atomically $ do
    (fh, _) <- readTVar var
    writeTVar var (fh, True)


hLoadValue' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  m (Either HTSException (Maybe RemoteValue))
hLoadValue' var key = withFakeHashKey var key $ \case
  Nothing -> pure $ Right Nothing
  Just (Dict _) -> pure $ Left BadKey
  Just (Simple v) -> pure $ Right $ Just v


hSaveValue' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  RemoteValue ->
  m (Either HTSException ())
hSaveValue' var key value = withFakeHash' var $ \values -> do
  updateFakeHash var $ Map.insert key (Simple value) values


hLoadDict' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  m (Either HTSException RemoteDict)
hLoadDict' var key = withFakeHashKey var key $ \case
  Nothing -> pure $ Right Map.empty
  Just (Dict v) -> pure $ Right v
  Just (Simple _) -> pure $ Left BadKey


hLoadDictPart' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  [RemoteKey] ->
  m (Either HTSException RemoteDict)
hLoadDictPart' var key dictKeys = withFakeHashKey var key $ \case
  Nothing -> pure $ Right Map.empty
  Just (Dict v) -> pure $ Right $ Map.filterWithKey (\k _ -> k `elem` dictKeys) v
  Just (Simple _) -> pure $ Left BadKey


hLengthDict' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  m (Either HTSException Natural)
hLengthDict' var key = withFakeHashKey var key $ \case
  Nothing -> pure $ Right 0
  Just (Dict v) -> pure $ Right $ fromInteger $ toInteger $ Map.size v
  Just (Simple _) -> pure $ Left BadKey


hSaveDict' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  RemoteDict ->
  m (Either HTSException ())
hSaveDict' var key d = withFakeHash' var $ \values -> do
  updateFakeHash var $ Map.insert key (Dict d) values


hSaveDictPart' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  RemoteDict ->
  m (Either HTSException ())
hSaveDictPart' var key d = withFakeHash' var $ \values ->
  case Map.lookup key values of
    Nothing -> do
      updateFakeHash var $ Map.insert key (Dict d) values
    Just (Dict d') -> do
      updateFakeHash var $ Map.insert key (Dict $ Map.union d d') values
    Just (Simple _) -> pure $ Left BadKey


updateFakeHash :: TVar (a, Bool) -> a -> STM (Either err ())
updateFakeHash var newMap = do
  writeTVar var (newMap, False)
  pure $ Right ()


hSaveDictValue' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  RemoteKey ->
  RemoteValue ->
  m (Either HTSException ())
hSaveDictValue' var key dictKey value = withFakeHash' var $ \values ->
  case Map.lookup key values of
    Nothing -> do
      updateFakeHash var $ Map.insert key (Dict $ Map.singleton dictKey value) values
    Just (Dict d) -> do
      updateFakeHash var $ Map.insert key (Dict $ Map.insert dictKey value d) values
    Just (Simple _) -> pure $ Left BadKey


hLoadDictValue' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  RemoteKey ->
  m (Either HTSException (Maybe RemoteValue))
hLoadDictValue' var key dictKey = withFakeHashKey var key $ \case
  Nothing -> pure $ Right Nothing
  Just (Dict d) -> pure $ Right $ Map.lookup dictKey d
  Just (Simple _) -> pure $ Left BadKey


hDeleteDictKeys' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  [RemoteKey] ->
  m (Either HTSException ())
hDeleteDictKeys' var key dictKeys = withFakeHash' var $ \values ->
  case Map.lookup key values of
    Nothing -> pure $ Right ()
    Just (Dict d) -> do
      let pred' = \k _ -> k `notElem` dictKeys
      writeTVar var (Map.insert key (Dict (Map.filterWithKey pred' d)) values, False)
      pure $ Right ()
    Just (Simple _) -> pure $ Left BadKey


hDeleteKeys' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  [RemoteKey] ->
  m (Either HTSException ())
hDeleteKeys' var keys = withFakeHash' var $ \values -> do
  let pred' = \k _ -> k `notElem` keys
  writeTVar var (Map.filterWithKey pred' values, False)
  pure $ Right ()


hDeleteMatchingKeys' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  m (Either HTSException ())
hDeleteMatchingKeys' var patt = withFakeHash' var $ \values -> do
  let pred' = \k _ -> not $ fromStrict k `matches` fromStrict patt
  writeTVar var (Map.filterWithKey pred' values, False)
  pure $ Right ()


withFakeHash :: MonadIO m => TVar t -> (t -> STM a) -> m a
withFakeHash v f = liftIO $ atomically $ readTVar v >>= f


withFakeHash' ::
  MonadUnliftIO m =>
  FakeHashVar ->
  (FakeHash -> STM (Either HTSException a)) ->
  m (Either HTSException a)
withFakeHash' var f = withFakeHash var $ \case
  (_, True) -> pure $ Left ConnectionClosed
  (values, _) -> f values


withFakeHashKey ::
  MonadUnliftIO m =>
  FakeHashVar ->
  RemoteKey ->
  (Maybe FakeValue -> STM (Either HTSException a)) ->
  m (Either HTSException a)
withFakeHashKey var key f = withFakeHash var $ \case
  (_, True) -> pure $ Left ConnectionClosed
  (values, _) -> f (Map.lookup key values)
