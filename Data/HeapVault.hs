{-|
Module      : Data.HeapVault
Description : Heterogeneous map keyed on stable names.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Data.HeapVault (

      HeapVault
    , HeapVaultImage
    , HeapVaultImageId
    , HeapVaultImageConst
    , empty
    , insert
    , lookup
    , delete

    ) where

import Prelude hiding (lookup)
import Data.HeapVault.Impure
