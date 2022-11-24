{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Redis.MemSpec (spec) where

import Babat.Redis.Mem (new)
import Redis.CheckHandle (
  Spec,
  afterAll,
  beforeAll,
  checkHandle,
  closeFixture,
  describe,
  setupFixture,
 )


spec :: Spec
spec =
  describe "Using the Mem handle" $
    beforeAll (new >>= setupFixture) $
      afterAll closeFixture checkHandle
