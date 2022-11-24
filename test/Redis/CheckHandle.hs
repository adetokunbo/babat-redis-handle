{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Redis.CheckHandle
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Redis.CheckHandle (
  -- * functions
  checkHandle,
  orThrowHTS,
  throwHTS,

  -- * Fixture support
  Fixture (..),
  setupFixture,
  closeFixture,

  -- * module re-eports
  module Test.Hspec,
  module Test.Hspec.Benri,
) where

import Babat.Redis.Mem
import Control.Exception (throwIO)
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.Benri


checkHandle :: SpecWith (Fixture a)
checkHandle = do
  context "with simple values" $ do
    it "should load ok" $ \f -> do
      hLoadValue (fHandle f) key1 `endsRight` Just simple1

    it "should update ok" $ \f -> do
      endsRight_ $ hSaveValue (fHandle f) key1 "changed"
      hLoadValue (fHandle f) key1 `endsRight` Just "changed"

    it "should delete matching keys correctly " $ \f -> do
      hLoadValue (fHandle f) key2 `endsRight` Just simple2
      endsRight_ $ hDeleteMatchingKeys (fHandle f) "*2"
      hLoadValue (fHandle f) key2 `endsRight` Nothing

    it "should delete ok" $ \f -> do
      hDeleteKeys (fHandle f) [key1] `endsRight` ()
      hLoadValue (fHandle f) key1 `endsRight` Nothing

  context "with dict values" $ do
    let mKey1Of f = hLoadDictValue (fHandle f) mKey1

    it "should load ok" $ \f -> do
      hLoadDict (fHandle f) mKey1 `endsRight` d1

    checkLength mKey1 4

    it "should update an indexed value ok" $ \f -> do
      endsRight_ $ hSaveDictValue (fHandle f) mKey1 key1 "changed"
      mKey1Of f key1 `endsRight` Just "changed"

    checkLength mKey1 4

    it "should add an indexed value ok" $ \f -> do
      mKey1Of f "foo" `endsRight` Nothing
      endsRight_ $ hSaveDictValue (fHandle f) mKey1 "foo" "bar"
      mKey1Of f "foo" `endsRight` Just "bar"

    checkLength mKey1 5

    it "should delete indexed values ok" $ \f -> do
      endsRight_ $ hDeleteDictKeys (fHandle f) mKey1 [key1, key2]
      mKey1Of f "foo" `endsRight` Just "bar"
      mKey1Of f key1 `endsRight` Nothing
      mKey1Of f key2 `endsRight` Nothing

    checkLength mKey1 3

    it "should update the indexed values using a dict ok" $ \f -> do
      endsRight_ $ hSaveDictPart (fHandle f) mKey1 d2
      mKey1Of f key1 `endsRight` Just simple3
      mKey1Of f key2 `endsRight` Nothing
      mKey1Of f key5 `endsRight` Just simple3

    it "should fetch a subset of the indexed values as a dict ok" $ \f -> do
      let want = Map.fromList [("foo", "bar"), (key5, simple3)]
      hLoadDictPart (fHandle f) mKey1 ["foo", key5] `endsRight` want

    it "should fetch an empty subset of the indexed values if no subkeys are given" $ \f -> do
      hLoadDictPart (fHandle f) mKey1 [] `endsRight` mempty

    it "should delete ok" $ \f -> do
      endsRight_ $ hDeleteKeys (fHandle f) [mKey1]
      hLoadDict (fHandle f) mKey1 `endsRight` Map.empty
      mKey1Of f "foo" `endsRight` Nothing

    checkLength mKey1 0


checkLength :: RemoteKey -> Natural -> SpecWith (Fixture a)
checkLength aKey n = context "and the reported length" $ do
  it "should be correct " $ \f -> do
    hLengthDict (fHandle f) aKey `endsRight` n


data Fixture a = Fixture
  { fHandle :: !(Handle IO)
  , fOther :: !a
  }


setupFixture :: Handle IO -> IO (Fixture ())
setupFixture h = do
  orThrowHTS $ hSaveValue h key1 simple1
  orThrowHTS $ hSaveValue h key2 simple2
  orThrowHTS $ hSaveDict h mKey1 d1
  pure $ Fixture h ()


closeFixture :: Fixture a -> IO ()
closeFixture f = do
  hClose $ fHandle f


key1, key2, key3, key4, key5, mKey1 :: RemoteKey
key1 = "a-simple-key-1"
key2 = "a-simple-key-2"
key3 = "another-key-1"
key4 = "another_key-2"
key5 = "yet-another-key"
mKey1 = "a-map-key-1"


simple1, simple2, simple3 :: RemoteValue
simple1 = "a-simple-value-1"
simple2 = "a-simple-value-2"
simple3 = "a-simple-value-3"


d1, d2 :: RemoteDict
d1 = Map.fromList [(key1, simple1), (key2, simple2), (key3, simple1), (key4, simple2)]
d2 = Map.fromList [(key1, simple3), (key5, simple3)]


throwHTS :: HTSException -> IO ()
throwHTS = throwIO . userError . show


orThrowHTS :: IO (Either HTSException ()) -> IO ()
orThrowHTS action = action >>= either throwHTS pure
