{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Defines types available in all the @Babat.Redis@ modules
-}
module Babat.Redis.Types (
  -- * data types
  HTSException (..),
  Handle (..),

  -- * type aliases
  DeleteKeys,
  DeleteDictKeys,
  LoadDict,
  LoadDictValue,
  LoadValue,
  SaveDict,
  SaveDictValue,
  SaveValue,
  RemoteKey,
  RemoteValue,
  RemoteDict,
) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Numeric.Natural (Natural)
import UnliftIO.Exception (Exception)


-- | A handle for accessing a hashtable service.
data Handle m = Handle
  { hLoadValue :: !(LoadValue m)
  , hSaveValue :: !(SaveValue m)
  , hLoadDict :: !(LoadDict m)
  , hSaveDict :: !(SaveDict m)
  , hLoadDictValue :: !(LoadDictValue m)
  , hSaveDictValue :: !(SaveDictValue m)
  , hDeleteKeys :: !(DeleteKeys m)
  , hDeleteDictKeys :: !(DeleteDictKeys m)
  , hDeleteMatchingKeys :: !(DeleteMatchingKeys m)
  , hLengthDict :: !(RemoteKey -> m (Either HTSException Natural))
  , hClose :: !(m ())
  }


-- | Represents errors that can occur when accessing the HashTableService.
data HTSException
  = ConnectionClosed
  | Unanticipated Text
  | NotDecoded Text
  | BadKey
  | Gone RemoteKey
  deriving (Eq, Show)


instance Exception HTSException


-- | Saves value in the HashTable service.
type SaveValue m = RemoteKey -> RemoteValue -> m (Either HTSException ())


-- | Loads a value from the Hashtable service.
type LoadValue m = RemoteKey -> m (Either HTSException (Maybe RemoteValue))


-- | Loads a 'RemoteDict' in the hashtable service.
type LoadDict m = RemoteKey -> m (Either HTSException RemoteDict)


-- | Saves a 'RemoteDict' in the hashtable service.
type SaveDict m = RemoteKey -> RemoteDict -> m (Either HTSException ())


-- | Loads a value in a 'RemoteDict' stored in the hashtable service.
type LoadDictValue m = RemoteKey -> RemoteKey -> m (Either HTSException (Maybe RemoteValue))


-- | Saves a value in a 'RemoteDict' stored in the hashtable service.
type SaveDictValue m = RemoteKey -> RemoteKey -> RemoteValue -> m (Either HTSException ())


-- | Deletes values with the given keys if they are stored in the hashtable service.
type DeleteKeys m = [RemoteKey] -> m (Either HTSException ())


-- | Deletes values that match the given pattern if they are stored in the hashtable service.
type DeleteMatchingKeys m = RemoteKey -> m (Either HTSException ())


-- | Deletes values with the given keys in a dictionary they are stored in the hashtable service.
type DeleteDictKeys m = RemoteKey -> [RemoteKey] -> m (Either HTSException ())


-- | Represents a key used to place something in the hashtable service.
type RemoteKey = ByteString


-- | Represents a value saved in the hashtable service.
type RemoteValue = ByteString


-- | Represents a dictionary stored in the hashtable service.
type RemoteDict = Map RemoteKey RemoteValue
