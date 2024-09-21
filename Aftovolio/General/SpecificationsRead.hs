-- |
-- Module      :  Aftovolio.General.SpecificationsRead
-- Copyright   :  (c) Oleksandr Zhabenko 2021-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
--  Provides functions to read data specifications for other modules from textual files.


{-# LANGUAGE NoImplicitPrelude #-}


module Aftovolio.General.SpecificationsRead where

import GHC.Base
import GHC.List
import Data.List (sort,lines)
import Data.Char (isAlpha)
import Aftovolio.RGLPK.General
import System.Environment (getArgs)
import GHC.Arr
import Text.Read
import Data.Maybe (fromMaybe,fromJust)
import GHC.Int
import Aftovolio.General.Base

charLine :: Char -> String -> Bool
charLine c = (== [c]) . take 1
{-# INLINE charLine #-}

groupBetweenChars
 :: Char  -- ^ A delimiter (can be used probably multiple times) used between different parts of the data.
 -> [String] -- ^ A list of 'String' that is partitioned using the 'String' starting with the delimiter.
 -> [[String]]
groupBetweenChars c [] = []
groupBetweenChars c xs = css : groupBetweenChars c (dropWhile (charLine c) dss)
  where (css,dss) = span (charLine c) xs

{-| An example of the needed data structure to be read correctly is in the file gwrsysExample.txt in the source tarball. 
-}
getGWritingSystem
  :: Char -- ^ A delimiter (cab be used probably multiple times) between different parts of the data file. Usually, a tilda sign \'~\'.
  -> String -- ^ Actually the 'String' that is read into the result. 
  -> GWritingSystemPRPLX -- ^ The data is used to obtain the phonetic language representation of the text.
getGWritingSystem c xs = map ((\(t1,t2) -> (sort . map (\kt -> fromJust (readPEMaybe kt::Maybe PhoneticsRepresentationPLX)) $ t2,
         read (concat t1)::Int8)) . splitAt 1) . groupBetweenChars c . lines $ xs

