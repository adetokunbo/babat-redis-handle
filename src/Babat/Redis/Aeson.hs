{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Functions that help use Aeson to load and save JSON data using the
dictionary service
-}
module Babat.Redis.Aeson (
  -- * decode/encode support
  decodeOr,
  decodeOr',
  decodeOrBad,
  decodeOrBad',
  jsonValue,
  jsonKey,
  webKey,
  appendWebKey,
  substWebKey,
  prependWebKey,

  -- * decode dictionaries
  decodeJsonKeyDict,
  decodeWebKeyDict,

  -- * save dictionaires using a @Handle@
  saveDict,
  saveDict',
  saveJsonKeyDict,
  saveWebKeyDict,
) where

import Babat.Redis.Types
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecodeStrict',
  encode,
 )
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


-- | Encode JSON as a remote value.
jsonValue :: ToJSON a => a -> RemoteValue
jsonValue = viaToJSON


webKey :: ToHttpApiData a => a -> RemoteKey
webKey = toHeader


substWebKey :: ToHttpApiData a => a -> RemoteKey -> RemoteKey
substWebKey x template =
  let (prefix, afterPre) = B.breakSubstring mustache template
      suffix = B.drop (B.length mustache) afterPre
      result = prefix <> webKey x <> suffix
   in if B.isPrefixOf mustache afterPre then result else template


appendWebKey :: ToHttpApiData a => RemoteKey -> a -> RemoteKey -> RemoteKey
appendWebKey sep x template = template <> sep <> webKey x


prependWebKey :: ToHttpApiData a => RemoteKey -> a -> RemoteKey -> RemoteKey
prependWebKey sep x template = webKey x <> sep <> template


mustache :: ByteString
mustache = "{}"


jsonKey :: ToJSON a => a -> RemoteKey
jsonKey = viaToJSON


decodeOrBad ::
  FromJSON b =>
  Maybe RemoteValue ->
  Either HTSException b
decodeOrBad x =
  case decodeOr NotDecoded x of
    Left err -> Left err
    Right mb -> maybe (Left BadValue) Right mb


decodeOrBad' ::
  FromJSON b =>
  Either HTSException (Maybe RemoteValue) ->
  Either HTSException b
decodeOrBad' = either Left decodeOrBad


decodeOr' ::
  FromJSON b =>
  Either HTSException (Maybe RemoteValue) ->
  Either HTSException (Maybe b)
decodeOr' = either Left (decodeOr NotDecoded)


-- | Decode a remote JSON  value, lifting decoding errors into a custom error type.
decodeOr ::
  (FromJSON a) =>
  (Text -> err) ->
  Maybe RemoteValue ->
  Either err (Maybe a)
decodeOr f = maybe (pure Nothing) (firstEither (f . Text.pack) . eitherDecodeStrict')


{- | Decode a remote dictionary serialized as JSON.

Both the key and value types are valid to deserialize as JSON.
-}
decodeJsonKeyDict ::
  (Ord a, FromJSON a, FromJSON b) =>
  (Text -> c) ->
  RemoteDict ->
  Either c (Map a b)
decodeJsonKeyDict f = firstEither f . decodeDict' decoder
  where
    decoder = firstEither Text.pack . eitherDecodeStrict'


{- | Decode a remote dictionary serialized as JSON.

The key type is deserialized as @HttpApiData@.
The value type is valid to deserialize as JSON.
-}
decodeWebKeyDict ::
  (Ord a, FromHttpApiData a, FromJSON b) =>
  (Text -> c) ->
  RemoteDict ->
  Either c (Map a b)
decodeWebKeyDict f = firstEither f . decodeDict' parseHeader


{- | Decode a remote dictionary with values serialized as JSON.

The value type is deserialized as JSON
-}
decodeDict' ::
  (Ord a, FromJSON b) =>
  (RemoteValue -> Either Text a) ->
  RemoteDict ->
  Either Text (Map a b)
decodeDict' decoder =
  let step _ _ (Left x) = Left x
      step k v (Right m) = case (decoder k, eitherDecodeStrict' v) of
        (Left x, _) -> Left x
        (_, Left y) -> Left $ Text.pack y
        (Right k', Right v') -> Right $ Map.insert k' v' m
   in Map.foldrWithKey step (Right Map.empty)


{- | Encode a remote dictionary serialized as JSON.

Both the key and value type are valid to deserialize as JSON.
-}
saveJsonKeyDict ::
  (Ord a, ToJSON a, ToJSON b, Monad m) =>
  (HTSException -> err) ->
  Handle m ->
  RemoteKey ->
  Map a b ->
  m (Either err ())
saveJsonKeyDict f = saveDict f viaToJSON


{- | Encode a remote dictionary serialized as JSON.

The key deserializes as @HttpApiData@ the value deserializes as JSON.
-}
saveWebKeyDict ::
  (Ord a, ToHttpApiData a, ToJSON b, Monad m) =>
  (HTSException -> err) ->
  Handle m ->
  RemoteKey ->
  Map a b ->
  m (Either err ())
saveWebKeyDict f = saveDict f toHeader


{- | Encode a remote dictionary with values serialized as JSON.

The key is serialized using the provided function,

The value type is serialized as JSON.

'HTSException' are lifted to another error type
-}
saveDict ::
  (Ord a, ToJSON b, Monad m) =>
  (HTSException -> err) ->
  (a -> RemoteValue) ->
  Handle m ->
  RemoteKey ->
  Map a b ->
  m (Either err ())
saveDict _ _ _ _ dict | Map.size dict == 0 = pure $ Right ()
saveDict toErr fromKey h key dict =
  let asRemote =
        Map.fromList
          . fmap (bimap fromKey viaToJSON)
          . Map.toList
   in fmap (firstEither toErr) $ hSaveDict h key $ asRemote dict


{- | Encode a remote dictionary with values serialized as JSON.

The key is serialized using the provided function,
the value type is serialized as JSON.
-}
saveDict' ::
  (Ord a, ToJSON b, Monad m) =>
  (a -> RemoteValue) ->
  Handle m ->
  RemoteKey ->
  Map a b ->
  m (Either HTSException ())
saveDict' = saveDict id


firstEither :: (err1 -> err2) -> Either err1 b -> Either err2 b
firstEither f = either (Left . f) Right


-- | Encode JSON as a remote value.
viaToJSON :: ToJSON a => a -> RemoteValue
viaToJSON = LBS.toStrict . encode
