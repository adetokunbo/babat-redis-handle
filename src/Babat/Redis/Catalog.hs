{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Babat.Redis.Catalog
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Babat.Redis.Catalog (
  -- * @KeyOf@ and related combinators
  KeyOf (..),
  fetchDictValue,
  fetchWholeDict,
  modWholeDict,
  mayFetchDictValue,
  saveDictValue,
  saveWholeDict,

  -- * @OuterKeyOf@ and related combinators
  OuterKeyOf (..),
  fetchDictValue',
  fetchWholeDict',
  mayFetchDictValue',
  modWholeDict',
  saveDictValue',
  saveWholeDict',
) where

import Babat.Redis.Aeson (
  decodeOr',
  decodeOrGone',
  decodeWebKeyDict,
  jsonValue,
  saveWebKeyDict,
 )
import Babat.Redis.Types (HTSException (..), Handle (..), RemoteKey)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as C8
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


class KnownSymbol (DictPath v) => KeyOf v where
  type DictPath v :: Symbol
  type Inner v
  keyOf :: Proxy v -> Inner v -> RemoteKey
  dictPath :: Proxy v -> RemoteKey
  dictPath _ = C8.pack $ symbolVal @(DictPath v) Proxy


class KeyOf v => OuterKeyOf v where
  type Outer v
  outerKeyOf :: Proxy v -> Outer v -> RemoteKey -> RemoteKey


fetchDictValue ::
  forall a m.
  (Monad m, FromJSON a, KeyOf a) =>
  Handle m ->
  Inner a ->
  m (Either HTSException a)
fetchDictValue h key =
  let outer = dictPath @a Proxy
      inner = keyOf @a Proxy key
      full = outer <> "//" <> inner
   in hLoadDictValue h outer inner <&> decodeOrGone' full


mayFetchDictValue ::
  forall a m.
  (Monad m, FromJSON a, KeyOf a) =>
  Handle m ->
  Inner a ->
  m (Either HTSException (Maybe a))
mayFetchDictValue h key =
  let outer = dictPath @a Proxy
      inner = keyOf @a Proxy key
   in hLoadDictValue h outer inner <&> decodeOr'


saveDictValue ::
  forall a m.
  (Monad m, ToJSON a, KeyOf a) =>
  Handle m ->
  Inner a ->
  a ->
  m (Either HTSException ())
saveDictValue h key aDict =
  let outer = dictPath @a Proxy
      inner = keyOf @a Proxy key
   in hSaveDictValue h outer inner $ jsonValue aDict


saveWholeDict ::
  forall a m.
  (Monad m, ToJSON a, KeyOf a, ToHttpApiData (Inner a), Ord (Inner a)) =>
  Handle m ->
  Map (Inner a) a ->
  m (Either HTSException ())
saveWholeDict h = saveWebKeyDict id h (dictPath @a Proxy)


modWholeDict ::
  forall a m.
  ( Monad m
  , ToJSON a
  , FromJSON a
  , KeyOf a
  , FromHttpApiData (Inner a)
  , ToHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  (Map (Inner a) a -> Map (Inner a) a) ->
  Handle m ->
  m (Either HTSException ())
modWholeDict modDict h = do
  let key = dictPath @a Proxy
  hLoadDict h key >>= \case
    Left err -> pure $ Left err
    Right loaded -> case decodeWebKeyDict NotDecoded loaded of
      Left err -> pure $ Left err
      Right d -> saveWebKeyDict id h key $ modDict d


fetchWholeDict ::
  forall a m.
  ( Monad m
  , FromJSON a
  , KeyOf a
  , FromHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  Handle m ->
  m (Either HTSException (Map (Inner a) a))
fetchWholeDict h = do
  let key = dictPath @a Proxy
  hLoadDict h key >>= \case
    Left err -> pure $ Left err
    Right d -> pure $ decodeWebKeyDict NotDecoded d


fetchDictValue' ::
  forall a m.
  (Monad m, FromJSON a, OuterKeyOf a) =>
  Handle m ->
  Outer a ->
  Inner a ->
  m (Either HTSException a)
fetchDictValue' h outPart key =
  let outer = outerKeyOf @a Proxy outPart $ dictPath @a Proxy
      inner = keyOf @a Proxy key
      full = outer <> "//" <> inner
   in hLoadDictValue h outer inner <&> decodeOrGone' full


mayFetchDictValue' ::
  forall a m.
  (Monad m, FromJSON a, OuterKeyOf a) =>
  Handle m ->
  Outer a ->
  Inner a ->
  m (Either HTSException (Maybe a))
mayFetchDictValue' h outPart key =
  let outer = outerKeyOf @a Proxy outPart $ dictPath @a Proxy
      inner = keyOf @a Proxy key
   in hLoadDictValue h outer inner <&> decodeOr'


saveDictValue' ::
  forall a m.
  (Monad m, ToJSON a, OuterKeyOf a) =>
  Handle m ->
  Outer a ->
  Inner a ->
  a ->
  m (Either HTSException ())
saveDictValue' h outPart key aDict =
  let outer = outerKeyOf @a Proxy outPart $ dictPath @a Proxy
      inner = keyOf @a Proxy key
   in hSaveDictValue h outer inner $ jsonValue aDict


saveWholeDict' ::
  forall a m.
  (Monad m, ToJSON a, OuterKeyOf a, ToHttpApiData (Inner a), Ord (Inner a)) =>
  Handle m ->
  Outer a ->
  Map (Inner a) a ->
  m (Either HTSException ())
saveWholeDict' h outPart =
  let outer = outerKeyOf @a Proxy outPart $ dictPath @a Proxy
   in saveWebKeyDict id h outer


modWholeDict' ::
  forall a m.
  ( Monad m
  , ToJSON a
  , FromJSON a
  , OuterKeyOf a
  , FromHttpApiData (Inner a)
  , ToHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  (Map (Inner a) a -> Map (Inner a) a) ->
  Handle m ->
  Outer a ->
  m (Either HTSException ())
modWholeDict' modDict h outPart = do
  let key = outerKeyOf @a Proxy outPart $ dictPath @a Proxy
  hLoadDict h key >>= \case
    Left err -> pure $ Left err
    Right loaded -> case decodeWebKeyDict NotDecoded loaded of
      Left err -> pure $ Left err
      Right d -> saveWebKeyDict id h key $ modDict d


fetchWholeDict' ::
  forall a m.
  ( Monad m
  , FromJSON a
  , OuterKeyOf a
  , FromHttpApiData (Inner a)
  , Ord (Inner a)
  ) =>
  Handle m ->
  Outer a ->
  m (Either HTSException (Map (Inner a) a))
fetchWholeDict' h outPart = do
  let key = outerKeyOf @a Proxy outPart $ dictPath @a Proxy
  hLoadDict h key >>= \case
    Left err -> pure $ Left err
    Right d -> pure $ decodeWebKeyDict NotDecoded d
