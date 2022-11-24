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
  endsJust_,
  endsJust,
  endsLeft,
  endsLeft_,
  endsRight,
  endsRight_,
  endsThen,
  endsNothing,

  -- * Fixture support
  Fixture (..),
  setupFixture,
  closeFixture,

  -- * module re-eports
  module Test.Hspec,
) where

import Babat.Redis.Mem
import Control.Exception (throwIO)
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import Test.Hspec


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


{- |
 @action \`endsRight\` @expected@ sets the expectation that @action@
 returns @Right@ @expected@.
-}
endsRight :: (HasCallStack, Show a, Eq a, Show b, Eq b) => IO (Either a b) -> b -> Expectation
action `endsRight` expected = action >>= (`shouldBe` Right expected)


{- |
 @action \`endsLeft\` @expected@ sets the expectation that @action@
 returns @Left@ @expected@.
-}
endsLeft ::
  (HasCallStack, Show a, Eq a, Show b, Eq b) => IO (Either a b) -> a -> Expectation
action `endsLeft` expected = action >>= (`shouldBe` Left expected)


{- |
 @action \`endsJust\`  @expected@ sets the expectation that @action@
 returns @Just@ @expected@.
-}
endsJust ::
  (HasCallStack, Show a, Eq a) => IO (Maybe a) -> a -> Expectation
action `endsJust` expected = action >>= (`shouldBe` Just expected)


{- |
 @action \`endsNothing\` expected@ sets the expectation that @action@
 returns @Nothing@.
-}
endsNothing :: (Show a, Eq a) => IO (Maybe a) -> IO ()
endsNothing action = action >>= (`shouldBe` Nothing)


{- |
 @action \`endsRight_\` sets the expectation that @action@
 returns @Right _@.
-}
endsRight_ :: (Show b1, Show b2) => IO (Either b1 b2) -> IO ()
endsRight_ action = endsThen action $ either (const False) (const True)


{- |
 @action \`endsLeft_\` sets the expectation that @action@
 returns @Left _@.
-}
endsLeft_ :: (Show b1, Show b2) => IO (Either b1 b2) -> IO ()
endsLeft_ action = endsThen action $ either (const True) (const False)


{- |
 @action \`endsJust_\` sets the expectation that @action@
 returns @Just _@.
-}
endsJust_ :: (Show a) => IO (Maybe a) -> IO ()
endsJust_ action = endsThen action $ maybe False (const True)


{- |
 @action \`endsThen\` expected@ sets the expectation that @action@
 returns @expected@.
-}
endsThen :: (Show a) => IO a -> (a -> Bool) -> IO ()
endsThen action p = action >>= (`shouldSatisfy` p)


infix 1 `endsLeft`, `endsRight`, `endsThen`, `endsJust`
