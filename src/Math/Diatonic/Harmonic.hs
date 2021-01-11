{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Math.Diatonic.Harmonic
    ( module Linear
    , findNoteFromSemitone, findIntervals, findHarmonics
    , Sortable(..), Frequency(..), harmonicsWithError
    , getHarmonic, getProducts
    ) where

import Control.Arrow (second)
import Data.Foldable
import Linear
import Text.Read (readEither)

import Math.Diatonic.Notes
import Math.Diatonic.Types
import Math.Diatonic.Util

class Sortable a where
    sort :: a -> a

instance Ord a => Sortable (V2 a) where
    sort = sortV2

instance Ord a => Sortable (V3 a) where
    sort = sortV3

instance Ord a => Sortable (V4 a) where
    sort = sortV4

findIntervals :: Fractional a => V4 a -> [a]
findIntervals (V4 a b c d) = 
    let
        p1 = abs $ (a + b) / 2
        p2 = abs $ (a - b) / 2
        p3 = abs $ (c + d) / 2
        p4 = abs $ (c - d) / 2
    in [p1, p2, p3, p4]


-- | Uses sum to product for 4 total frequency ratios.
-- Takes 3 frequencies (a,b,c) and finds the 4 possible d value frequencies
-- Such that the frequencies reduce down to a product of 4*cos(x*t)*cos(y*t)*cos(z*t)
-- This phenomenon happens for the harmonic seventh chords
-- 
-- Test

data HarmonicLocation = HarmonicLeft | HarmonicMidLeft | HarmonicMidRight | HarmonicRight
    deriving Show

data HarmonicValue a = HarmonicValue (V3 a) HarmonicLocation
    deriving Show


-- TODO: add restriction for ratios and frequencies only
harmonicsWithError :: V3 Semitone -> [(Error, V4 Semitone)]
harmonicsWithError v@(V3 a b c) = 
    let
        vh :: V4 Frequency
        vh = fmap getHarmonic $ findHarmonics (fmap sToF v)
    in map (second (V4 a b c)) $ toList $ fmap (fToS') vh

getProducts :: (Fractional a, Num a, Ord a) => HarmonicValue a -> V3 a
getProducts (HarmonicValue (V3 a b c) loc) =
    (/2) $ sortV3 $ case loc of
            HarmonicLeft -> V3 (a+b) (c-b) (c-a)
            HarmonicMidLeft -> V3 (b-a) (c+a) (c-b)
            HarmonicMidRight -> V3 (b-a) (c+b) (c-a)
            HarmonicRight -> V3 (a+b) (c+b) (c+a)


getHarmonic :: Num a => HarmonicValue a -> a
getHarmonic (HarmonicValue (V3 a b c) loc) = 
    case loc of
        HarmonicLeft -> abs $ c - b - a
        HarmonicMidLeft -> c - b + a
        HarmonicMidRight -> c + b - a
        HarmonicRight -> c + b + a

findHarmonics :: (Num a, Ord a) => V3 a -> V4 (HarmonicValue a)
findHarmonics v = 
    fmap (HarmonicValue $ sortV3 v) $ V4 HarmonicLeft HarmonicMidLeft HarmonicMidRight HarmonicRight
{-
    let 
        (V3 a b c) = sortV3 v
        d1 = c - b - a
        d2 = c - b + a
        d3 = c + b - a
        d4 = c + b + a
    in V4 d1 d2 d3 d4
      -}

{-
class Frequency a where
    toFreq :: a -> Double
    fromFreq :: Double -> a
-}


seventh :: [Rational]
seventh = [4/4, 5/4, 6/4, 7/4]

triad :: [Rational]
triad = [4/4,5/4,6/4]

{-
instance Frequency Int where
    toFreq i = 2**(fromIntegral i/12) * 440
    fromFreq f = round $ (12*) $ logBase 2 (f/440)
    -}

sumToProduct :: (Fractional a, Ord a) => a -> a -> (a,a)
sumToProduct f1 f2 = 
    let
        f1' = (f1 + f2) / 2
        f2' = abs $ f1 - f2 / 2
    in (f1', f2')

lengthTwoSubsequences :: [a] -> [(a,a)]
lengthTwoSubsequences (x:xs) = map (x,) xs ++ lengthTwoSubsequences xs
lengthTwoSubsequences [] = []

{-
generateAccidentals :: [String]
generateAccidentals = generateAccidentals' noteNames majorScaleIntervals

generateAccidentals' :: [Char] -> [Int] -> [String]
generateAccidentals' (n1:n2:ns) (2:is) = 
    [pure n1, pure n1++"#",pure n2++"b"] ++ generateAccidentals' (n2:ns) is
generateAccidentals' (n1:ns) (1:is) = 
    [pure n1] ++ generateAccidentals' ns is
generateAccidentals' _ _ = []

minorScaleIntervals :: [Int]
minorScaleIntervals = take 7 $ drop 5 $ cycle majorScaleIntervals

majorChord :: [Int]
majorChord = [0,4,7]

minorChord :: [Int]
minorChord = [0,3,7]
-}
