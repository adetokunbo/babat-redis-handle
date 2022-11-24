{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Provides uniform access to a Dictionary service Handle
If no REDIS_URL is set or if REDIS_URL is set to known static string, uses an
internal in-memory implementation.
-}
module Babat.Redis (
  -- * module re-export
  module Babat.Redis.FromEnv,
) where

import Babat.Redis.FromEnv

