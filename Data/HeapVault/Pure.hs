{-|
Module      : Data.HeapVault.Pure
Description : Heterogeneous map keyed on stable names.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

A map on stable names is defined along with a pure interface.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HeapVault.Pure (

      HeapVault
    , HeapVaultImage
    , HeapVaultImageId
    , HeapVaultImageConst
    , empty
    , size
    , insert
    , lookup
    , delete

    ) where

import Prelude hiding (lookup)
import Data.Proxy
import qualified Data.List as List (lookup)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.Mem.StableName
import Unsafe.Coerce

type Page = [Item]
type Item = (SomeStableName, SomeThing)

data SomeStableName where
    SomeStableName :: StableName a -> SomeStableName

instance Eq SomeStableName where
    SomeStableName sn1 == SomeStableName sn2 = eqStableName sn1 sn2

data SomeThing where
    SomeThing :: t -> SomeThing

-- | A map from stable name hashes to arbitrary types as determined by the type
--   d and the family HeapVaultImage.
newtype HeapVault (d :: *) = HeapVault {
      getHeapVault :: IntMap Page
    }

-- | An open type family to determine the type of the image of a key.
type family HeapVaultImage (d :: *) (k :: *) :: *

data HeapVaultImageId
type instance HeapVaultImage HeapVaultImageId t = t

data HeapVaultImageConst t
type instance HeapVaultImage (HeapVaultImageConst t) r = t

-- | The empty Book for a particular HeapVaultImage proxy.
empty :: Proxy d -> HeapVault d
empty _ = HeapVault IntMap.empty

size :: HeapVault d -> Int
size (HeapVault imap) = IntMap.size imap

-- | Use any value as a key in a HeapVault, so long as the HeapVaultImage type
--   is defined and matches.
insert :: StableName k -> HeapVaultImage d k -> HeapVault d -> HeapVault d
insert sn v (HeapVault imap) =
    let key = hashStableName sn
        item = (SomeStableName sn, SomeThing v)
    in  HeapVault (IntMap.insertWith (++) key [item] imap)

-- | Use any value as a key to retrieve its image from a Book.
lookup :: forall k d . StableName k -> HeapVault d -> Maybe (HeapVaultImage d k)
lookup sn (HeapVault imap) =
    let key = hashStableName sn
    in  case IntMap.lookup key imap of
            Nothing -> Nothing
            Just lst -> case List.lookup (SomeStableName sn) lst of
                Nothing -> Nothing
                -- usafeCoerce is OK so long as the Book was modified only by
                -- insert and delete
                Just (SomeThing v) -> Just (unsafeCoerce v)

-- | Delete and return the value at a key.
delete :: forall k d . StableName k -> HeapVault d -> Maybe (HeapVaultImage d k, HeapVault d)
delete sn (HeapVault imap) =
    let key = hashStableName sn
    in  case IntMap.lookup key imap of
            Nothing -> Nothing
            Just lst -> case listDelete (SomeStableName sn) lst of
                Nothing -> Nothing
                -- If the bucket is now empty, be sure to shrink the map.
                Just (SomeThing v, []) ->
                    let imap' = IntMap.delete key imap
                    in  Just (unsafeCoerce v, HeapVault imap')
                Just (SomeThing v, lst') ->
                    let imap' = IntMap.insertWith const key lst' imap
                    in  Just (unsafeCoerce v, HeapVault imap')

  where

    listDelete :: forall k v . Eq k => k -> [(k, v)] -> Maybe (v, [(k, v)])
    listDelete _ []  = Nothing
    listDelete k ((k', v) : rest)
        | k == k' = Just (v, rest)
        | otherwise = (\(final, lst) -> (final, (k', v) : lst)) <$> (listDelete k rest)
