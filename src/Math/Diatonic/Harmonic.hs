{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Math.Diatonic.Harmonic
    ( module Linear
    , findNoteFromSemitone, findHarmonics
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

data HarmonicLocation = HarmonicLeft | HarmonicMidLeft | HarmonicMidRight | HarmonicRight
    deriving Show

data HarmonicValue a = HarmonicValue (V3 a) HarmonicLocation
    deriving Show

-- TODO: add restriction for ratios and frequencies only
harmonicsWithError :: V3 Semitone -> [(Error, V4 Semitone)]
harmonicsWithError v@(V3 a b c) = map (second (V4 a b c)) $ toList $ fToS' . getHarmonic <$> findHarmonics (sToF <$> v)

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
findHarmonics v = HarmonicValue (sortV3 v) <$> placeholders 
    where placeholders = V4 HarmonicLeft HarmonicMidLeft HarmonicMidRight HarmonicRight

seventh :: [Rational]
seventh = [4/4, 5/4, 6/4, 7/4]

triad :: [Rational]
triad = [4/4,5/4,6/4]
