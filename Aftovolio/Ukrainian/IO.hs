{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, DeriveGeneric, StrictData #-}

module Aftovolio.Ukrainian.IO where

import GHC.Arr
import GHC.Base 
import GHC.Word
import GHC.Num (Num(..),Integer,(+),(-),(*))
import GHC.Real (Integral(..),fromIntegral,(/),rem,quotRem,round,(^))
import GHC.Generics
import GHC.Enum (fromEnum)
import Text.Show (Show(..))
import Text.Read (readMaybe)
import Data.Ord (Down(..)) 
import Data.Char (isDigit,toLower,isSpace)
import System.IO (putStrLn, FilePath,stdout,hSetNewlineMode,universalNewlineMode,getLine,appendFile,writeFile,putStr,readFile)
import qualified Rhythmicity.MarkerSeqs as R --hiding (id)
import Data.List hiding (foldr,null)
import qualified Data.List as L (null) 
import Data.Foldable (mapM_)
import Data.Maybe (isNothing,fromJust,fromMaybe) 
import Data.Tuple (fst,snd)
import Aftovolio.Ukrainian.Syllable 
import Aftovolio.Ukrainian.SyllableWord8
import Aftovolio.Ukrainian.Melodics 
import GHC.Int (Int8)
import CaseBi.Arr (getBFst')
import Aftovolio.Ukrainian.ReadDurations
import Data.Ord (comparing)
import Numeric (showFFloat)
import Aftovolio.Halfsplit
import System.Directory (readable,writable,getPermissions,Permissions(..),doesFileExist,getCurrentDirectory)
import Data.ReversedScientific
import Control.Concurrent.Async (mapConcurrently)
import Aftovolio.Tests
import Data.MinMax1 
import Aftovolio.General.Datatype3 
import Aftovolio.General.Distance
import Aftovolio.UniquenessPeriodsG
import Control.Exception
import Control.DeepSeq  

generalF
 :: Int -- ^ A power of 10. The distance value is quoted by 10 in this power if the next ['Word8'] argument is not empty. The default one is 0. The right values are in the range [0..4].
 -> Int -- ^ A 'length' of the next argument here.
 -> Compards -- ^ A value that the different options are compared with. If no command line argument \"+di\" was added, then this is a `C1` applied to the list of positive values normed by 255 (the greatest of which is 255) that the line options are compared with. If null, then the program works without comparison. The length of it must be a least common multiplier of the (number of syllables plus number of \'_digits\' groups) to work correctly. Is not used when the next 'FilePath' and 'String' arguments are not null. If \"+di\" command line argument was  provided, then this corresponds to the case of differentiation.
 -> Bool -- ^ If 'True' then adds \"<br>\" to line endings for double column output
 -> FilePath -- ^ A path to the file to save double columns output to. If empty then just prints to 'stdout'.
 -> String -- ^ If not null than instead of rhythmicity evaluation using hashes and and feets, there is computed a diversity property for the specified 'String' here using the 'selectSounds' function. For more information, see: 'https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types'
 -> (String, String)  -- ^ If the next element is not equal to -1, then the prepending and appending lines to be displayed. Used basically for working with the multiline textual input data.
 -> Int -- ^ The number of the line in the file to be read the lines from. If equal to -1 then neither reading from the file is done nor the first argument influences the processment results.
 -> FilePath -- ^ The file to read the sound representation durations from.
 -> Int 
 -> R.HashCorrections 
 -> (Int8,[Int8]) 
 -> Int 
 -> Bool 
 -> Int 
 -> Bool 
 -> Int8 
 -> (FilePath, Int) 
 -> Bool  -- ^ Whether to run tests concurrently or not. 'True' corresponds to concurrent execution that can speed up the getting results but use more resources.
 -> String -- ^ An initial string to be analysed.
 -> [String] 
 -> IO [String]
generalF power10 ldc compards html dcfile selStr (prestr, poststr) lineNmb file numTest hc (grps,mxms) k descending hashStep emptyline splitting (fs,code) concurrently initstr universalSet@(_:_:_) = do
   syllableDurationsDs <- readSyllableDurations file
   let syllN = countSyll initstr
       f ldc compards syllableDurationsDs grps mxms -- Since the version 0.12.0.0, has a possibility to evaluate diversity property.
            | L.null selStr = (if doubleFunc (L.null::[Word8]->Bool) (L.null::[Int8]-> Bool) compards then (sum . R.countHashes2G hashStep hc grps mxms) else (`quot` 10^power10) . fromIntegral . sumAbsDistNormComp compards . (if isWord8Based compards then C1 else C2 . fromSmallWord8toInt8Diff)) .  read3 (not . L.null . filter (not . isSpace)) 1.0 (mconcat . (if L.null file then case k of { 1 -> syllableDurationsD; 2 -> syllableDurationsD2; 3 -> syllableDurationsD3; 4 -> syllableDurationsD4} 
                         else  if length syllableDurationsDs >= k then syllableDurationsDs !! (k - 1) else syllableDurationsD2) . createSyllablesUkrS) 
            | otherwise = fromIntegral . diverse2GGL (selectSounds selStr) [100,101] . convertToProperUkrainianI8 . filter (\c -> not (isDigit c) && c /= '_' && c/= '=')
   hSetNewlineMode stdout universalNewlineMode
   if numTest >= 0 && numTest <= 179 && numTest /= 1 && doubleFunc (L.null::[Word8]->Bool) (L.null::[Int8]-> Bool) compards then testsOutput concurrently syllN f ldc syllableDurationsDs numTest universalSet
   else let sRepresent = zipWith (\k (x, ys) -> S k x ys) [1..] . 
                   (if descending then sortOn (\(u,w) -> (Down u, w)) else sortOn id) . map (\xss -> (f ldc compards syllableDurationsDs grps mxms xss, xss)) $ universalSet
            strOutput = force . (:[]) . halfsplit1G (\(S _ y _) -> y) (if html then "<br>" else "") (jjj splitting) $ sRepresent
                        in do
                             let lns1 = unlines strOutput
                             putStrLn lns1
                             if L.null dcfile then putStr "" 
                                            else do 
                                                exist <- doesFileExist dcfile 
                                                if exist then do 
                                                             perms <- getPermissions dcfile 
                                                             if writable perms then writeFile dcfile lns1 
                                                                               else error $ "Aftovolio.Ukrainian.IO.generalF: File " `mappend` dcfile `mappend` " is not writable!"
                                                         else do 
                                                             currdir <- getCurrentDirectory
                                                             perms <- getPermissions currdir
                                                             if writable perms then writeFile dcfile lns1 
                                                                               else error $ "Aftovolio.Ukrainian.IO.generalF: Directory of the file " `mappend` dcfile `mappend` " is not writable!"
                             let l1 = length sRepresent
                             if code == -1 
                                 then if lineNmb == -1 then return strOutput
                                      else do 
                                          print23 prestr poststr 1 [initstr]
                                          return strOutput
                                 else do 
                                       print23 prestr poststr 1 [initstr]
                                       parseLineNumber l1 >>= \num -> do
                                         permiss <- getPermissions fs
                                         let writ = writable permiss
                                             readab = readable permiss
                                         if writ && readab then outputWithFile selStr compards sRepresent file syllableDurationsDs code grps k fs num 
                                         else error "Aftovolio.Ukrainian.IO.generalF: The specified file cannot be used for appending the text! Please, specify another file!"
                                         return []
          where jjj kk = let (q1,r1) = quotRem kk (if kk < 0 then -10 else 10) in jjj' q1 r1 emptyline
                jjj' q1 r1 emptyline
                  | r1 == (-1) || r1 == (-3) = -10*q1 + (if emptyline then -5 else r1)
                  | r1 == 1 || r1 == 3 = 10*q1 + (if emptyline then 5 else r1)
                  | r1 < 0 = -10*q1 + (if emptyline then -4 else r1)
                  | otherwise = 10*q1 + (if emptyline then 4 else r1)
generalF _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ [u1] = do
      putStrLn u1
      return [u1]
generalF _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = let strOutput = ["You have specified the data and constraints on it that lead to no further possible options.", "Please, specify another data and constraints."] in do 
    putStrLn . unlines $ strOutput
    return strOutput

data AftovolioUkr = S Int Integer String deriving (Eq, Generic)

instance NFData AftovolioUkr

instance Show AftovolioUkr where
  show (S i j xs) = showBignum 7 j `mappend` " " `mappend` xs `mappend` "  " `mappend` showWithSpaces 4 i

countSyll :: String -> Int
countSyll xs = numUnderscoresSyll + (fromEnum . foldr (\x y -> if isVowel1 x then y + 1 else y) 0 . convertToProperUkrainianI8 $ xs)
  where numUnderscoresSyll = length . filter (\xs -> let (ys,ts) = splitAt 1 xs in ys == "_" && all isDigit ts && not (L.null ts)) . groupBy (\x y -> x=='_' && isDigit y) $ xs

stat1 :: Int -> (Int8,[Int8]) -> Int
stat1 n (k, ks) = fst (n `quotRemInt` fromEnum k) * length ks

parseHelp :: [String] -> (String,[String])
parseHelp xss 
  | L.null xss = ([],[])
  | otherwise = (unwords rss, uss `mappend` qss)
       where (yss,tss) = break (== "-b") xss
             (uss,wss) = break (== "+b") yss
             [qss,rss] = map (drop 1) [tss, wss]
             
outputSel :: AftovolioUkr -> Int -> String
outputSel (S x1 y1 ts) code
  | code < 0 = []
  | code == 1 || code == 11 || code == 16 = intercalate " " [show x1, ts] `mappend` "\n"
  | code == 2 || code == 12 || code == 17 = intercalate " " [show y1, ts] `mappend` "\n"
  | code == 3 || code == 13 || code == 18 = intercalate " " [show x1, ts, show y1] `mappend` "\n"
  | code == 4 || code == 14 || code == 19 = intercalate " " [show x1, show y1] `mappend` "\n"
  | otherwise = ts `mappend` "\n"

parseLineNumber :: Int -> IO Int
parseLineNumber l1 = do 
  putStrLn "Please, specify the number of the option to be written to the file specified: "
  number <- getLine
  let num = readMaybe (filter isDigit number)::Maybe Int
  if isNothing num || num > Just l1 || num == Just 0 
      then parseLineNumber l1 
      else return . fromJust $ num

{-| 'selectSounds' converts the argument after \"+ul\" command line argument into a list of  Ukrainian sound representations that is used for evaluation of \'uniqueness periods\' properties of the line. Is a modified Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.parsey0Choice from the @phonetic-languages-simplified-examples-array-0.21.0.0@ package. 
-}
selectSounds :: String -> FlowSound
selectSounds = f . sortOn id . filter (/= 101) . concatMap g . words . map (\c -> if c  == '.' then ' ' else toLower c)
    where g = getBFst' ([101::Sound8], listArray (0,41) (("1",[1,2,3,4,5,6,7,8,10,15,17,19,21,23,25,27,28,30,32,34,36,38,39,41,43,45,47,49,50,52,54,66]):("sr",[27,28,30,32,34,36]):("vd",[8,10,15,17,19,21,23,25]) :("vs",[45,47,49,50,43,52,38,66,54,39,41]) :("vw",[1..6]) :map (\(k,t) -> (k,[t])) [("\1072",1),("\1073",15),("\1074",36),("\1075",21),("\1076",17),("\1076\1078",23),("\1076\1079",8),("\1077",2),("\1078",10),("\1079",25),("\1080",5),("\1081",27),("\1082",45),("\1083",28),("\1084",30),("\1085",32),("\1086",3),("\1087",47),("\1088",34),("\1089",49),("\1089\1100",54),("\1090",50),("\1091",4),("\1092",43),("\1093",52),("\1094",38),("\1094\1100",66),("\1095",39),("\1096",41),("\1097",55),("\1100",7),("\1102",56),("\1103",57),("\1108",58),("\1110",6),("\1111",59),("\1169",19),("\8217",61)]))
          f (x:ts@(y:_)) 
            | x == y = f ts
            | otherwise = x:f ts
          f xs = xs

-- | Part of the 'generalF' for processment in case of using tests mode.
testsOutput
  :: (Show a1, Integral a1) =>
     Bool -- ^ Whether to run tests concurrently or not. 'True' corresponds to concurrent execution that can speed up the getting results but use more resources.
     -> Int
     -> (Int -> Compards -> p2 -> Int8 -> [Int8] -> String -> a1)
     -> Int
     -> p2
     -> Int
     -> [String]
     -> IO [String]
testsOutput concurrently syllN f ldc syllableDurationsDs numTest universalSet = do
     putStrLn "Feet   Val  Stat   Proxim" 
     (if concurrently 
          then mapConcurrently
          else mapM) (\(q,qs) -> 
                          let m = stat1 syllN (q,qs)
                              (min1, max1) = force . fromJust . minMax11By (comparing (f ldc (C1 []) syllableDurationsDs q qs)) $ universalSet 
                              mx = f ldc (C1 []) syllableDurationsDs q qs max1 
                              strTest = (show (fromEnum q) `mappend` "   |   " `mappend`  show mx `mappend` "     " `mappend` show m `mappend` "  -> " `mappend` 
                                  showFFloat (Just 3) (100 * fromIntegral mx / fromIntegral m) "%" `mappend` (if rem numTest 10 >= 4 
                                                                                                                          then ("\n" `mappend` min1 `mappend` "\n" `mappend` max1 `mappend` "\n")
                                                                                                                          else "")) in putStrLn strTest >> return strTest) . zip (sel2 numTest) $ (sel numTest)

-- | Part of 'generalF' for processment with a file.
outputWithFile
  :: String -- ^ If not null than instead of rhythmicity evaluation using hashes and and feets, there is computed a diversity property for the specified 'String' here using the 'selectSounds' function. For more information, see: 'https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types'
     -> Compards -- ^ A value that the different options are compared with. If no command line argument \"+di\" was added, then this is a `C1` applied to the list of positive values normed by 255 (the greatest of which is 255) that the line options are compared with. If null, then the program works without comparison. The length of it must be a least common multiplier of the (number of syllables plus number of \'_digits\' groups) to work correctly. Is not used when the next 'FilePath' and 'String' arguments are not null. If \"+di\" command line argument was  provided, then this corresponds to the case of differentiation.
     -> [AftovolioUkr]
     -> FilePath -- ^ The file to read the sound representation durations from.
     -> [[[[Sound8]]] -> [[Word8]]]
     -> Int
     -> Int8 
     -> Int
     -> FilePath -- ^ A file to be probably added output parts to.
     -> Int
     -> IO ()
outputWithFile selStr compards sRepresent file syllableDurationsDs code grps k fs num 
  | mBool && code >= 10 && code <= 19 && grps == 2 = putStrLn (mconcat [textP, "\n", breaks, "\n", show rs]) >> appendF ((if code >= 15 then mconcat [show rs, "\n", breaks, "\n"] else "") `mappend` outputS)
  | otherwise = appendF outputS
           where mBool = L.null selStr && doubleFunc (L.null::[Word8]->Bool) (L.null::[Int8]-> Bool) compards 
                 appendF = appendFile fs
                 lineOption = head . filter (\(S k _ _) -> k == num) $ sRepresent
                 textP = (\(S _ _ ts) -> ts) lineOption
--                 sylls = createSyllablesUkrS textP
                 outputS = outputSel lineOption code
                 qqs = readEq4 (mconcat . (if L.null file then case k of { 1 -> syllableDurationsD; 2 -> syllableDurationsD2; 3 -> syllableDurationsD3; 4 -> syllableDurationsD4} else if length syllableDurationsDs >= k then syllableDurationsDs !! (k - 1) else syllableDurationsD2) . createSyllablesUkrS) (map showFS . mconcat . createSyllablesUkrS) . basicSplit $ textP 
                 (breaks, rs) = R.showZerosFor2PeriodMusic qqs

