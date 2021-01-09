module Main where

import Codec.Midi
import Linear 

import System.Random
import System.Random.Shuffle

import Math.Diatonic
import Math.Diatonic.Midi

filterAndSortRange :: (Foldable t, Ord a, Sortable (t a)) => (a,a) -> [t a] -> [t a]
filterAndSortRange (minv,maxv) = map sort . filter (\v -> all (>= minv) v && all (<= maxv) v)
{-
bestHarmonic :: V3 a -> V4 a
bestHarmonic triad = 
    fmap toFrequency' $ findHarmonics triad
-}

main :: IO ()
main = 
    do
        let chords = pick3 (57-12) (57+12)
        gen <- newStdGen
        let shuffledChords = shuffle' chords (length chords) gen
        exportFile "chords.mid" $ midiChords shuffledChords