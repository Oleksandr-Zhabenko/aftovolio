-- |
-- Module      :  Aftovolio.Halfsplit
-- Copyright   :  (c) OleksandrZhabenko 2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--

{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_HADDOCK -show-extensions #-}

module Aftovolio.Halfsplit where

import GHC.Base
import GHC.Enum (fromEnum)
import GHC.Real (quot,quotRem)
import GHC.Num ((+),(-),abs)
import Data.List hiding (foldr)
import GHC.Int (Int8)
import Text.Show (Show(..))
import System.IO (putStrLn,getLine,putStr)
import Data.Tuple (fst)
import Data.Char (isDigit)

-- | Converts the data that is an instance of 'Show' typeclass to be printed in two-column way.
halfsplit 
  :: (Show a, Eq b) 
  => (a -> b)
  -> Bool  -- ^ Whether to filter out all groups of \'={digits}\' from the lines.
  -> Int8 
  -> [a] 
  -> String
halfsplit g filtering = halfsplit1G g filtering "" 
{-# INLINE halfsplit #-}

-- | Converts the data that is an instance of 'Show' typeclass to be printed in two-column way with
-- customizable ending of each line. Filters out all groups of \'={digits}\' from the lines.
halfsplit1G 
  :: (Show a, Eq b) 
  => (a -> b)
  -> Bool -- ^ Whether to filter out all groups of \'={digits}\' from the lines.
  -> String -- ^ Additional 'String' added to every line before the \"\\n\" character.
  -> Int8 
  -> [a] 
  -> String
halfsplit1G g filtering appendstr m xs 
 | null xs = []
 | otherwise = 
    let (n, rr2) = quotRem (fromEnum m) (if m < 0 then -10 else 10)
        r = 
          case abs rr2 of
           1 -> let us = reverse ts in (if rrr == 0 then map show ys else replicate l0 ' ':map show ys, map show us)
           2 -> let us = (replicate (lt1 - ly1) [replicate l0 ' ']) `mappend` reverse (map reverse y1s) in (mconcat us, mconcat t1s)
           3 -> let us = (replicate (lt1 - ly1) [replicate l0 ' ']) `mappend` y1s
                    ks = reverse . map reverse $ t1s in (mconcat us, mconcat ks)
           4 -> let us = (replicate (lt2 - ly2) [replicate l0 ' ']) `mappend` reverse (map reverse y2s) in (mconcat us, mconcat t2s)
           5 -> let us = (replicate (lt2 - ly2) [replicate l0 ' ']) `mappend` y2s
                    ks = reverse . map reverse $ t2s in (mconcat us, mconcat ks)
           _ -> let us = reverse ys in (if rrr == 0 then map show us else replicate l0 ' ':map show us, map show ts) in (if filtering then removeChangesOfDurations else id) $ ((\(rs, qs) -> mergePartsLine n (appendstr `mappend` "\n") rs qs) r) `mappend` appendstr
              where (ys,ts) = splitAt l xs 
                    (l,rrr) = length xs `quotRem` 2
                    l0 = length . show . head $ xs 
                    rss = map (map show) . groupBy (\x y ->  g x == g y) $ xs
                    r1ss = intersperse [replicate l0 ' '] rss
                    l2 = (sum . map length $ rss) `quot` 2
                    l3 = (sum . map length $ r1ss) `quot` 2
                    (y1s,t1s,_) = splitGroups l2 rss
                    ly1 = sum . map length $ y1s
                    lt1 = sum . map length $ t1s
                    (y2s,t2s,_) = splitGroups l3 r1ss
                    ly2 = sum . map length $ y2s
                    lt2 = sum . map length $ t2s

-- | A generalized version of 'halfsplit3G' with the possibility to prepend and append strings to it. These 'String's are not filtered out for the groups of \'={digits}\' from the prepending and appending 'String's.
halfsplit2G 
  :: (Show a, Eq b) 
  => (a -> b)
  -> Bool -- ^ Whether to filter out all groups of \'={digits}\' from the lines.
  -> String -- ^ Additional 'String' added to every line before the \"\\n\" character.
  -> String -- ^ A 'String' that is prepended to the 'halfsplit1G' result.
  -> String -- ^ A 'String' that is appended to the 'halfsplit1G' result.
  -> Int8 
  -> [a] 
  -> String
halfsplit2G g filtering appendstr prestr poststr m xs = prestr `mappend` halfsplit1G g filtering appendstr m xs `mappend` poststr
{-# INLINABLE halfsplit2G #-}

-- | Filters out all groups of \'={digits}\' from the 'String'
removeChangesOfDurations :: String -> String
removeChangesOfDurations ('=':xs) = removeChangesOfDurations (dropWhile isDigit xs) 
removeChangesOfDurations (x:xs) = x:removeChangesOfDurations xs
removeChangesOfDurations _ = []

mergePartsLine :: Int -> String -> [String] -> [String] -> String
mergePartsLine n newlined xs ys = intercalate newlined . zipWith (\x y -> x `mappend` (replicate n (if n < 0 then '\t' else ' ')) `mappend` y) xs $ ys

splitGroups :: Int -> [[a]] -> ([[a]], [[a]], Int)
splitGroups l tss = foldr h ([],[],0) tss
   where h js (rss,mss,k)
            | k <= l = (rss, js:mss, k + length js)
            | otherwise = (js : rss, mss, k + length js)

showWithSpaces :: (Show a) => Int -> a -> String
showWithSpaces n x 
 | l < n = xs `mappend` replicate (n - l) ' '
 | otherwise = xs
    where xs = show x
          l = length xs

print23 :: Bool -> String -> String -> Int -> [String] -> IO ()
print23 filtering prestr poststr n xss = do
  putStrLn prestr
  let linez = zip xss [1..]
  if n >= 2 && n <= l - 1
      then do
          let linez3 = (\(x:y:t:xs) -> x:(' ':y):(' ':' ':t):xs) . map fst . filter (\(ts,m) -> m `elem` [n - 1..n + 1]) $ linez
          mapM putSLn linez3 >> putStrLn poststr
      else (case n of
             1 -> putStr " " >> mapM putSLn (take 2 xss)
             m -> if m == l then mapM putSLn ((\(x:y:xs) -> x:(' ':y):xs) . drop (l - 2) $ xss) else pure []) >> putStrLn poststr
         where l = length xss
               putSLn = putStrLn . (if filtering then removeChangesOfDurations else id)

