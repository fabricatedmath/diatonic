{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Math.Diatonic 
    ( module Linear, sortV2, sortV3, sortV4
    , toFrequency
    , findNoteFromSemitone, findIntervals, findHarmonics, findHarmonics'
    , toFrequency'
    , pick1, pick2, pick3, pick4
    , Sortable(..), Frequency(..)
    ) where

import Data.Foldable
import Linear
import Text.Read (readEither)

import Math.Diatonic.Notes

newtype Frequency = Frequency Double
    deriving (Eq, Num, Ord)

class Frequencable a where
    toFrequency :: a -> Frequency

instance Frequencable Frequency where
    toFrequency = id

instance Frequencable Semitone where
    toFrequency (Semitone s) = Frequency $ 440*2**(fromIntegral s/12)

-- can add close enough to zero check and Maybe?
instance ImperfectSemitoneable Frequency where
    toImperfectSemitone (Frequency f) = V2 lower upper
        where 
            sf = freqToSemitone f
            lower = (sf - fromIntegral (floor sf), Semitone $ floor sf)
            upper = (fromIntegral (ceiling sf) - sf, Semitone $ ceiling sf)

            freqToSemitone :: Double -> Double
            freqToSemitone f = (12*) $ logBase 2 (f/440)

class Sortable a where
    sort :: a -> a

instance Ord a => Sortable (V2 a) where
    sort = sortV2

instance Ord a => Sortable (V3 a) where
    sort = sortV3

instance Ord a => Sortable (V4 a) where
    sort = sortV4

sortV2 :: Ord a => V2 a -> V2 a
sortV2 (V2 a b) = V2 (min a b) (max a b)

sortV3 :: Ord a => V3 a -> V3 a
sortV3 (V3 a b c) = 
    let
        V2 a' b' = sortV2 $ V2 a b
        V2 mina c' = sortV2 $ V2 a' c
        V2 midb maxc = sortV2 $ V2 b' c'
    in V3 mina midb maxc

sortV4 :: Ord a => V4 a -> V4 a
sortV4 (V4 a b c d) = 
    let
        V3 a' b' c' = sortV3 $ V3 a b c
        V3 mina minb submaxc = sortV3 $ V3 a' b' d
        V2 midc maxd = sortV2 $ V2 submaxc c'
    in V4 mina minb midc maxd

findIntervals :: Fractional a => V4 a -> [a]
findIntervals (V4 a b c d) = 
    let
        p1 = abs $ (a + b) / 2
        p2 = abs $ (a - b) / 2
        p3 = abs $ (c + d) / 2
        p4 = abs $ (c - d) / 2
    in [p1, p2, p3, p4]

pick1 :: Enum a => a -> a -> [V1 a]
pick1 minv maxv = [ V1 x | x <- [minv..maxv]]

pick2 :: Enum a => a -> a -> [V2 a]
pick2 minv maxv = 
    let p0 = maxv
        p1 = pred p0
    in [ (V2 x y) | x <- [minv..p1], y <- [succ x.. p0]]

pick3 :: Enum a => a -> a -> [V3 a]
pick3 minv maxv = 
    let p0 = maxv
        p1 = pred p0
        p2 = pred p1
    in [ (V3 x y z) | x <- [minv..p2], y <- [succ x..p1], z <- [succ y..p0]]

pick4 :: Enum a => a -> a -> [V4 a]
pick4 minv maxv = 
    let p0 = maxv
        p1 = pred p0
        p2 = pred p1
        p3 = pred p2
    in [ (V4 x y z w) | x <- [minv..p3], y <- [succ x..p2], z <- [succ y..p1], w <- [succ z.. p0]]

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

findHarmonics' :: Frequencable a => V3 a -> [(Double, Semitone)]
findHarmonics' v@(V3 a b c) = 
    let
        vh = fmap toFrequency' $ findHarmonics (fmap toFrequency v)
    in concat $ fmap (toList . toImperfectSemitone) vh


toFrequency' :: Num a => HarmonicValue a -> a
toFrequency' (HarmonicValue (V3 a b c) loc) = 
    case loc of
        HarmonicLeft -> abs $ c - b - a
        HarmonicMidLeft -> c - b + a
        HarmonicMidRight -> c + b - a
        HarmonicRight -> c + b + a

findHarmonics :: (Num a, Ord a) => V3 a -> V4 (HarmonicValue a)
findHarmonics v = fmap (HarmonicValue $ sortV3 v) $ V4 HarmonicLeft HarmonicMidLeft HarmonicMidRight HarmonicRight
{-
    let 
        (V3 a b c) = sortV3 v
        d1 = c - b - a
        d2 = c - b + a
        d3 = c + b - a
        d4 = c + b + a
    in V4 d1 d2 d3 d4
      -}



notes :: [String]
notes = ["a", "a#", "b", "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#"]

findNoteFromSemitone :: Semitone -> String
findNoteFromSemitone (Semitone semitone) = 
    let noteName = notes !! noteNumber ++ show noteOctave
        noteNumber = fromIntegral semitone `mod` 12
        noteOctave = (fromIntegral $ semitone + 9) `div` 12 + 4
    in noteName

{-
class Frequency a where
    toFreq :: a -> Double
    fromFreq :: Double -> a
-}


seventh :: [Rational]
seventh = [1, 5/4, 6/4, 7/4]

triad :: [Rational]
triad = [1,5/4,6/4]

{-
instance Frequency Int where
    toFreq i = 2**(fromIntegral i/12) * 440
    fromFreq f = round $ (12*) $ logBase 2 (f/440)
    -}

sumToProduct2 :: (Fractional a, Ord a) => a -> a -> (a,a)
sumToProduct2 f1 f2 = 
    let
        f1' = (f1 + f2) / 2
        f2' = (max f1 f2 - min f1 f2) / 2
    in (f1', f2')

lengthTwoSubsequences :: [a] -> [(a,a)]
lengthTwoSubsequences (x:xs) = map (x,) xs ++ lengthTwoSubsequences xs
lengthTwoSubsequences [] = []

sumToProduct :: Double -> Double -> (Double,Double)
sumToProduct f1 f2 = 
    let
        m1 = max f1 f2
        m2 = min f1 f2
        p1 = (m1 - m2) / 2
        p2 = m2 + p1
    in (p1,p2)

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

pianoNoteSemitones :: [Int]
pianoNoteSemitones = 
    let
        lowerSemitone = round $ freqToSemitone 16.34
        upperSemitone = round $ freqToSemitone 7902.13
    in [lowerSemitone..upperSemitone]
    -}