{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Redis.UsageSpec (spec) where

import Babat.Redis.Aeson
import Babat.Redis.Catalog
import Babat.Redis.Mem
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Numeric.Natural (Natural)
import Redis.CheckHandle


spec :: Spec
spec =
  describe "Using the Catalog module" $
    beforeAll (new >>= setupFixture) $ afterAll closeFixture checkSome


checkSome :: SpecWith (Fixture a)
checkSome = do
  context "enrolling" $ do
    let courseKey e = webKey $ "/courses/" <> e <> "/enrollees"
    it "should complete successfully" $ \f -> do
      endsRight_ $ enroll (fHandle f) c1 e1
      endsRight_ $ enroll (fHandle f) c2 e1
      endsRight_ $ enroll (fHandle f) c2 e2

    it "should retrieve the raw dict values ok" $ \f -> do
      hLoadDictValue (fHandle f) (courseKey c1) (webKey e1) `endsRight` Just "[false,0]"
      hLoadDictValue (fHandle f) (courseKey c2) (webKey e2) `endsRight` Just "[false,0]"

    it "should retrieve the typed values ok" $ \f -> do
      fetchRStatus (fHandle f) c1 e1 `endsRight` (False, 0)

    it "should retrieve the full dicts ok" $ \f -> do
      let want = Map.fromList [(0, c2), (1, c1), (2, c2)]
      fetchCourses (fHandle f) e1 `endsRight` want


type EnrolleeId = Text


e1, e2 :: EnrolleeId
e1 = "enrollee-1"
e2 = "enrollee-2"


type CourseCode = Text


c1, c2 :: CourseCode
c1 = "course-A"
c2 = "course-B"


type RStatus = (Bool, Natural)


instance KeyOf RStatus where
  type DictPath RStatus = "/courses/{}/enrollees"
  type Inner RStatus = EnrolleeId
  keyOf _ = webKey


instance OuterKeyOf RStatus where
  type Outer RStatus = CourseCode
  outerKeyOf _ = substWebKey


saveRStatus :: MonadIO m => Handle m -> CourseCode -> EnrolleeId -> RStatus -> m (Either HTSException ())
saveRStatus = saveDictValue'


fetchRStatus :: MonadIO m => Handle m -> CourseCode -> EnrolleeId -> m (Either HTSException RStatus)
fetchRStatus = fetchDictValue'


fetchRStatus' :: MonadIO m => Handle m -> CourseCode -> EnrolleeId -> m (Either HTSException (Maybe RStatus))
fetchRStatus' = mayFetchDictValue'


enroll :: MonadIO m => Handle m -> CourseCode -> EnrolleeId -> m (Either HTSException ())
enroll h code anId = do
  fetchRStatus' h code anId >>= \case
    Left err -> pure $ Left err
    Right (Just _) -> pure $ Right ()
    Right Nothing -> do
      addACourse code h anId >>= \case
        Left err -> pure $ Left err
        Right _ -> saveRStatus h code anId (False, 0)


instance KeyOf CourseCode where
  type DictPath CourseCode = "/tryers/{}/courses"
  type Inner CourseCode = Natural
  keyOf _ = webKey


instance OuterKeyOf CourseCode where
  type Outer CourseCode = EnrolleeId
  outerKeyOf _ = substWebKey


addACourse :: MonadIO m => CourseCode -> Handle m -> EnrolleeId -> m (Either HTSException ())
addACourse = modWholeDict' . registerCourse


type EnrolleeCourses = Map Natural CourseCode


fetchCourses :: MonadIO m => Handle m -> EnrolleeId -> m (Either HTSException EnrolleeCourses)
fetchCourses = fetchWholeDict'


registerCourse :: CourseCode -> EnrolleeCourses -> EnrolleeCourses
registerCourse code codes | code `elem` Map.elems codes = Map.insert 0 code codes
registerCourse code codes = case Map.lookupMax codes of
  Nothing -> Map.fromList [(0, code), (1, code)]
  Just (n, _) -> Map.insert (n + 1) code $ Map.insert 0 code codes
