{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Math.Diatonic.Harmonic
    ( HarmonicValue(..), HarmonicLocation(..)
    , perfectTriad
    , harmonics
    , harmonicValue, harmonicProducts
    , harmonicsWithError -- , harmonicTension
    , harmonicProductToSum
    , module Control.Arrow
    , rationalId
    ) where

import Control.Arrow ((&&&))
import Data.Foldable
import Data.Tuple (swap)
import Linear
import Text.Read (readEither)

import Math.Diatonic.Notes
import Math.Diatonic.Semitone
import Math.Diatonic.Util

-- | Given:
--
-- @
-- V3 a b c
-- @
--
data HarmonicLocation 
    -- |
    -- @
    -- (c - b - a)
    -- @
    = HarmonicLeft
    -- |
    -- @
    -- (c - b + a)
    -- @
    | HarmonicMidLeft
    -- |
    -- @
    -- (c + b - a)
    -- @
    | HarmonicMidRight
    -- |
    -- @
    -- (c + b + a)
    -- @
    | HarmonicRight
    deriving Show

data HarmonicValue a = HarmonicValue (V3 a) HarmonicLocation
    deriving Show

-- | Gives the perfect triad with the intervals 
--
-- > V3 (4/4) (5/4) (6/4)
--
perfectTriad :: V3 Rational
perfectTriad = V3 (4/4) (5/4) (6/4)

rationalId :: Rational -> Rational
rationalId = id

-- | Harmonics
harmonics :: (Num a, Ord a) => V3 a -> V4 (HarmonicValue a)
harmonics v = HarmonicValue (sortV3 v) <$> placeholders 
    where placeholders = V4 HarmonicLeft HarmonicMidLeft HarmonicMidRight HarmonicRight

-- TODO: add restriction for ratios and frequencies only
harmonicsWithError :: V3 Semitone -> V4 (Double, V4 Semitone)
harmonicsWithError v@(V3 a b c) = (semitoneError &&& (V4 a b c)) . harmonicValue <$> harmonics v

{-
harmonicTension :: V4 Semitone -> Frequency
harmonicTension (V4 a b c d) = minDist (sToF d) $ toList $ harmonicValue <$> (harmonics $ sToF <$> V3 a b c)
    where minDist v = minimum . map (abs . subtract v)
    -}

-- | Implied Harmonic of a triple, can be 4 different values depending on 'HarmonicLocation'
harmonicValue :: Num a => HarmonicValue a -> a
harmonicValue (HarmonicValue (V3 a b c) loc) = 
    case loc of
        HarmonicLeft -> abs $ c - b - a
        HarmonicMidLeft -> c - b + a
        HarmonicMidRight -> c + b - a
        HarmonicRight -> c + b + a

-- | Implied Harmonic products
--
-- > V3 a b c
--
-- implies moving something of the form
--
-- > cos(a*t) + cos(b*t) + cos(c*t) + cos(d*t)
--
-- into
--
-- > 4*cos(x*t)*cos(y*t)*cos(z*t)
-- the harmonic guarantees this is possible
--
harmonicProducts :: (Fractional a, Num a, Ord a) => HarmonicValue a -> V3 a
harmonicProducts (HarmonicValue (V3 a b c) loc) =
    (/2) $ sortV3 $ case loc of
        HarmonicLeft -> V3 (a+b) (c-b) (c-a)
        HarmonicMidLeft -> V3 (b-a) (c+a) (c-b)
        HarmonicMidRight -> V3 (b-a) (c+b) (c-a)
        HarmonicRight -> V3 (a+b) (c+b) (c+a)

harmonicProductToSum :: Num a => V3 a -> V4 a
harmonicProductToSum (V3 a b c) = V4 f1 f2 f3 f4
    where
        f1 = abs $ c - b - a
        f2 = c - b + a
        f3 = c + b - a
        f4 = c + b + a

