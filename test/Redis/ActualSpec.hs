{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Redis.ActualSpec (spec) where

import Babat.Redis.Actual (fallbackExpiry, new, parseConfig)
import Control.Exception (onException, throwIO)
import Data.ByteString.Char8 (unpack)
import Data.Proxy (Proxy (..))
import Redis.CheckHandle (
  Fixture (..),
  Spec,
  afterAll,
  beforeAll,
  checkHandle,
  closeFixture,
  setupFixture,
 )
import System.TmpProc.Docker.Redis
import Test.Hspec.TmpProc


spec :: Spec
spec =
  tdescribe "Using an Actual handle" $
    beforeAll setupHandles $
      afterAll closeHandles checkHandle


setupHandles :: IO (Fixture (HList '[ProcHandle TmpRedis]))
setupHandles = do
  procHandles <- startupAll $ testProc `HCons` HNil
  flip onException (terminateAll procHandles) $ do
    let redisUri = unpack $ hUri $ handleOf @"a-redis-db" Proxy procHandles
        addProcHandles x = x {fOther = procHandles}
    h <-
      parseConfig fallbackExpiry 1 redisUri >>= \case
        Nothing -> throwIO $ userError "failed to obtain handle to TmpRedis"
        Just x -> new x
    addProcHandles <$> setupFixture h


closeHandles :: Fixture (HList '[ProcHandle TmpRedis]) -> IO ()
closeHandles f = do
  closeFixture f
  terminateAll $ fOther f


testProc :: TmpRedis
testProc = TmpRedis []
