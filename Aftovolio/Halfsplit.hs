{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK -show-extensions #-}

{- |
Module      :  Aftovolio.Halfsplit
Copyright   :  (c) OleksandrZhabenko 2023
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com
-}
module Aftovolio.Halfsplit where

import Data.Char (isDigit)
import Data.List hiding (foldr)
import Data.Tuple (fst)
import GHC.Base
import GHC.Enum (fromEnum)
import GHC.Int (Int8)
import GHC.Num (abs, (+), (-))
import GHC.Real (quot, quotRem)
import System.IO (getLine, putStr, putStrLn)
import Text.Read (readMaybe)
import Data.Maybe (Maybe,fromMaybe)
import Text.Show (Show (..))
import Debug.Trace

-- | Converts the data that is an instance of 'Show' typeclass to be printed in two-column way.
halfsplit ::
    (Show a, Eq b) =>
    (a -> b) ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    Int8 ->
    [a] ->
    String
halfsplit g filtering = halfsplit1G g filtering ""
{-# INLINE halfsplit #-}

{- | Converts the data that is an instance of 'Show' typeclass to be printed in two-column way with
customizable ending of each line. Filters out all groups of \'={digits}\' from the lines.
-}
halfsplit1G ::
    (Show a, Eq b) =>
    (a -> b) ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    -- | Additional 'String' added to every line before the \"\\n\" character.
    String ->
    Int8 ->
    [a] ->
    String
halfsplit1G g filtering appendstr m xs
    | null xs = []
    | otherwise =
        let (n, rr2) = quotRem (fromEnum m) (if m < 0 then -10 else 10)
            r =
                case abs rr2 of
                    1 ->
                        let us = reverse ts
                         in ((if rrr == 0 then id else (replicate l0 ' ' :)) $ map show ys, map show us)
                    2 -> (reverse y10s, t10s)
                    3 -> (y10s, reverse t10s)
                    4 -> (reverse y20s, t20s)
                    5 -> (y20s, reverse t20s)
                    _ ->
                        let us = reverse ys
                         in ((if rrr == 0 then id else (replicate l0 ' ' :)) $ map show us, map show ts)
         in (if filtering then removeChangesOfDurations else id) $
                ((\(rs, qs) -> mergePartsLine n (appendstr `mappend` "\n") rs qs) r)
                    `mappend` appendstr
  where
    (ys, ts) = splitAt l xs -- Is used for basic splitting
    (l, rrr) = length xs `quotRem` 2 -- For basic splitting
    l0 = length . show . head $ xs  -- For all cases for a whitespace placeholder
    rss = map (map show) . groupBy (\x y -> g x == g y) $ xs -- For basic groupping
    (l2, r20) = (sum . map length $ rss) `quotRem` 2
    (y100s, t10s) = splitAt l2 . concat $ rss
    y10s 
        | r20 == 0 = y100s
        | otherwise = replicate l0 ' ' : y100s
    r1ss = intersperse [replicate l0 ' '] rss -- For groupping with empty lines
    (l3, r30) = (sum . map length $ r1ss) `quotRem` 2
    (y200s, t20s) = splitAt l3 . concat $ r1ss
    y20s
        | r30 == 0 = y200s
        | otherwise = replicate l0 ' ' : y200s
 
-- | A generalized version of 'halfsplit3G' with the possibility to prepend and append strings to it. These 'String's are not filtered out for the groups of \'={digits}\' from the prepending and appending 'String's.
halfsplit2G ::
    (Show a, Eq b) =>
    (a -> b) ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    -- | Additional 'String' added to every line before the \"\\n\" character.
    String ->
    -- | A 'String' that is prepended to the 'halfsplit1G' result.
    String ->
    -- | A 'String' that is appended to the 'halfsplit1G' result.
    String ->
    Int8 ->
    [a] ->
    String
halfsplit2G g filtering appendstr prestr poststr m xs = prestr `mappend` halfsplit1G g filtering appendstr m xs `mappend` poststr
{-# INLINEABLE halfsplit2G #-}

-- | Filters out all groups of \'={digits}\' from the 'String'
removeChangesOfDurations :: String -> String
removeChangesOfDurations ('=' : xs) = removeChangesOfDurations (dropWhile isDigit xs)
removeChangesOfDurations (x : xs) = x : removeChangesOfDurations xs
removeChangesOfDurations _ = []

mergePartsLine :: Int -> String -> [String] -> [String] -> String
mergePartsLine n newlined xs ys =
    intercalate newlined
        . zipWith
            (\x y -> x `mappend` (replicate n (if n < 0 then '\t' else ' ')) `mappend` y)
            xs $
        ys

splitGroups :: Int -> [[a]] -> ([[a]], [[a]], Int)
splitGroups l tss = foldr h ([], [], 0) tss
  where
    h js (rss, mss, k)
        | k < l = (rss, js : mss, k + length js)
        | otherwise = (js : rss, mss, k + length js)

showWithSpaces :: (Show a) => Int -> a -> String
showWithSpaces n x
    | l < n = xs `mappend` replicate (n - l) ' '
    | otherwise = xs
  where
    xs = show x
    l = length xs

print23 :: Bool -> String -> String -> Int -> [String] -> IO ()
print23 filtering prestr poststr n xss = do
    putStrLn prestr
    let linez = zip xss [1 ..]
    if n >= 2 && n <= l - 1
        then do
            let linez3 =
                    (\(x : y : t : xs) -> x : (' ' : y) : (' ' : ' ' : t) : xs)
                        . map fst
                        . filter (\(ts, m) -> m `elem` [n - 1 .. n + 1]) $
                        linez
            mapM putSLn linez3 >> putStrLn poststr
        else
            ( case n of
                1 -> putStr " " >> mapM putSLn (take 2 xss)
                m ->
                    if m == l
                        then mapM putSLn ((\(x : y : xs) -> x : (' ' : y) : xs) . drop (l - 2) $ xss)
                        else pure []
            )
                >> putStrLn poststr
  where
    l = length xss
    putSLn = putStrLn . (if filtering then removeChangesOfDurations else id)

readNums :: [String] -> [Int]
readNums = concatMap readNs
  where readNs :: String -> [Int]
        readNs xs 
            | any (== '-') xs = 
                 let (ys, ts) = break (== '-') xs 
                     us = dropWhile (not . isDigit) ts
                 in [(fromMaybe 1 (readMaybe ys:: Maybe Int))..(fromMaybe 1 (readMaybe us:: Maybe Int))]
            | otherwise = [fromMaybe 1 (readMaybe xs:: Maybe Int)]
{-# INLINE readNums #-}
