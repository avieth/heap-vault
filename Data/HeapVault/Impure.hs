{-|
Module      : Data.HeapVault.Impure
Description : Heterogeneous map keyed on stable names.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

A map on stable names is defined along with an impure interface.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HeapVault.Impure (

      HeapVault
    , Tracer
    , HeapVaultImage
    , Pure.HeapVaultImageId
    , Pure.HeapVaultImageConst
    , empty
    , empty_trace
    , noTrace
    , stdTrace
    , insert
    , lookup
    , delete

    ) where

import Prelude hiding (lookup)
import Data.HeapVault.Pure (HeapVaultImage)
import qualified Data.HeapVault.Pure as Pure
import Data.Proxy
import Data.IORef
import System.Mem.StableName
import System.Mem.Weak

-- | A HeapVault in an IORef, which holds weak references to its values. If the
--   key *or* the value is collected, the entry for that pair is purged.
--   Changes are impure.
newtype HeapVault (d :: *) = HeapVault {
      getHeapVault :: (IORef (Pure.HeapVault (WithWeak d)), Tracer)
    }

type Tracer = String -> IO ()

noTrace :: Tracer
noTrace = const (pure ())

stdTrace :: Tracer
stdTrace = putStrLn

-- | Wraps a HeapVaultImage proxy.
data WithWeak d
type instance HeapVaultImage (WithWeak d) t = Weak (HeapVaultImage d t)

empty :: forall d . Proxy d -> IO (HeapVault d)
empty proxy = empty_trace noTrace proxy

empty_trace :: forall d . Tracer -> Proxy d -> IO (HeapVault d)
empty_trace tracer _ = do
    let pureHeapVault = Pure.empty (Proxy :: Proxy (WithWeak d))
    ref <- newIORef pureHeapVault
    pure (HeapVault (ref, tracer))

-- | Associate a key with a value, such that subsequent calls to lookup on that
--   key will return that value *unless* the value has been collected.
insert :: k -> HeapVaultImage d k -> HeapVault d -> IO ()
insert k v hv@(HeapVault (ref, tracer)) = do
    sn <- makeStableName k
    tracer ("HeapVault : inserting with hash key " ++ (show (hashStableName sn)))
    wkV <- mkWeak v v (Just (purgeValue sn hv))
    wkK <- mkWeak k k (Just (purgeKey sn hv))
    atomicModifyIORef' ref $ \pureHeapVault -> (Pure.insert sn wkV pureHeapVault, ())

-- | Remove a stable name from a HeapVault.
purge :: StableName k -> HeapVault d -> IO (Pure.HeapVault (WithWeak d))
purge sn (HeapVault (ref, _)) = atomicModifyIORef' ref $ \pureHeapVault ->
    case Pure.delete sn pureHeapVault of
        Nothing -> (pureHeapVault, pureHeapVault)
        Just (_, pureHeapVault') -> (pureHeapVault', pureHeapVault')

-- | Purge due to a dead key.
purgeKey :: StableName k -> HeapVault d -> IO ()
purgeKey sn hv@(HeapVault (_, tracer)) = do
    tracer ("HeapVault : purging due to dead key with hash " ++ (show (hashStableName sn)))
    out <- purge sn hv
    tracer ("HeapVault : size after purge " ++ show (Pure.size out))
    pure ()

-- | Purge due to a dead value.
purgeValue :: StableName k -> HeapVault d -> IO ()
purgeValue sn hv@(HeapVault (_, tracer)) = do
    tracer ("HeapVault : purging due to dead value at key with hash " ++ (show (hashStableName sn)))
    out <- purge sn hv
    tracer ("HeapVault : size after purge " ++ show (Pure.size out))
    pure ()

lookup :: k -> HeapVault d -> IO (Maybe (HeapVaultImage d k))
lookup k (HeapVault (ref, _)) = do
    sn <- makeStableName k
    book <- readIORef ref
    let maybeValue = Pure.lookup sn book
    case maybeValue of
        Nothing -> pure Nothing
        -- TBD purge if nothing? Or just wait for the finalizer? Finalizer may
        -- never run though, right?
        Just weakRef -> deRefWeak weakRef

delete :: k -> HeapVault d -> IO (Maybe (HeapVaultImage d k))
delete k (HeapVault (ref, _)) = do
    sn <- makeStableName k
    out <- atomicModifyIORef' ref $ \pureHeapVault -> case Pure.delete sn pureHeapVault of
        Nothing -> (pureHeapVault, Nothing)
        Just (v, pureHeapVault') -> (pureHeapVault', Just v)
    case out of
        Nothing -> pure Nothing
        Just weakRef -> deRefWeak weakRef
