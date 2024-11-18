{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Aftovolio.PermutationsRepresent
Copyright   :  (c) OleksandrZhabenko 2022-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Permutations data type to mark the needed permutations type from the other modules.
-}
module Aftovolio.PermutationsRepresent (
    PermutationsType (..),
    bTransform2Perms,
) where

import GHC.Base
import Text.Show

data PermutationsType = P Int deriving (Eq, Ord)

instance Show PermutationsType where
    show (P x) = "+p " `mappend` show x

bTransform2Perms :: [String] -> PermutationsType
bTransform2Perms ys
    | ys == ["1"] = P 1
    | ys == ["2"] = P 2
    | otherwise = P 0
