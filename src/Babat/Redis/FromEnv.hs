{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Provides access to a 'Handle' that's configured from the system enviroment.

If @REDIS_URL@ is set to known static string, the internal in-memory
implementation, otherwise the 'Handle' is configured to access the configured
REDIS instance.
-}
module Babat.Redis.FromEnv (
  -- * functions
  new,
  new',

  -- * module re-export
  module Babat.Redis.Types,
  Real.Config,
) where

import qualified Babat.Redis.Actual as Real
import qualified Babat.Redis.Mem as Fake
import Babat.Redis.Types
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import System.ReadEnvVar (lookupEnv, readEnvDef)
import UnliftIO.Exception (throwIO)


-- | Construct a 'Handle'.
new :: MonadUnliftIO m => m (Handle m)
new = new' False


{- | Construct a 'Handle'.

The 'Handle' accesses an actual Redis instance unless

- the argument is 'True'
- the REDIS_URL matches the constant indicating that the in-memory 'Handle' should be used
-}
new' :: MonadUnliftIO m => Bool -> m (Handle m)
new' True = Fake.new
new' _ =
  liftIO readEnvConfig >>= \case
    (_, Just cfg) -> Real.new cfg
    (True, Nothing) -> Fake.new
    (False, Nothing) -> liftIO $ throwIO $ userError "could not create Redis handle"


-- | Determine the @Config@ from the environment.
readEnvConfig :: IO (Bool, Maybe Real.Config)
readEnvConfig = do
  let withBool Nothing = (True, Nothing)
      withBool (Just x) = (False, Just x)

  expiry <- readEnvDef "REDIS_DEFAULT_EXPIRATION" Real.fallbackExpiry
  maxConns <- readEnvDef "REDIS_MAX_CONNECTIONS" Real.fallbackMaxConns
  lookupEnv "REDIS_URL" >>= \case
    Nothing -> pure (False, Nothing)
    Just y -> withBool <$> Real.parseConfig expiry maxConns y
