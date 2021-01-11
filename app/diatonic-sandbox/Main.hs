module Main where

import Control.Arrow

import Data.Foldable
import qualified Data.List as L
import Data.Ord
import GHC.Exts (the)

import Codec.Midi
import Linear 

import System.Random
import System.Random.Shuffle

import Math.Diatonic.Harmonic
import Math.Diatonic.Midi
import Math.Diatonic.Notes
import Math.Diatonic.Types
import Math.Diatonic.Util

twoEqualNotes :: (Eq a, Foldable t) => t a -> Bool 
twoEqualNotes = go . toList
    where 
        go (x:y:xs) | x == y = True
                    | otherwise = go (y:xs)
        go _ = False

withinDistNotes :: (Eq a, Foldable t, Num a, Ord a) => a -> t a -> Bool 
withinDistNotes d = go . toList
    where 
        go (x:y:xs) | x+d >= y = True
                    | otherwise = go (y:xs)
        go _ = False


sortedNub :: (Ord a) => [a] -> [a]
sortedNub = map the . L.group . L.sort

filterAndSortRange :: (Foldable t, Ord a, Ord (t a), Sortable (t a)) => (a,a) -> [t a] -> [t a]
filterAndSortRange (minv,maxv) = sortedNub . map sort . filter (\v -> all (>= minv) v && all (<= maxv) v)

filterOn :: (Foldable t, Ord a, Ord (t a), Sortable (t a)) => (b -> t a) -> (a,a) -> [b] -> [b]
filterOn accessor (minv,maxv) = filter inRange
    where
        inRange b = let v = accessor b in all (>= minv) v && all (<= maxv) v
{-
bestHarmonic :: V3 a -> V4 a
bestHarmonic triad = 
    fmap toFrequency' $ findHarmonics triad
-}

allBestHarmonics :: (Semitone,Semitone) -> [(Error, V4 Semitone)]
allBestHarmonics  r@(lowRange, highRange) = filterOn snd r $ concatMap harmonicsWithError $ pick3Anchored lowRange highRange
{-    where
        bestHarmonic :: V3 Semitone -> V4 Semitone
        bestHarmonic = snd . minimum . findHarmonics'
        -}
{-
allHarmonics :: (Semitone,Semitone) -> [(Double, V4 Semitone)]
allHarmonics  r@(lowRange, highRange) = filterAndSortRange r $ concatMap findHarmonics' $ pick3 lowRange highRange
    where
        harmonic :: V3 Semitone -> V4 Semitone
        harmonic = snd . minimum . findHarmonics'
        -}

sortedHarmonics :: [(Error, V4 Semitone)]
sortedHarmonics = sortedNub $ map (first abs) $ map (second sortV4) $ allBestHarmonics (c5,c8)

main :: IO ()
main = 
    do
        let list = sortedNub $ map (first abs) $ map (second sortV4) $ allBestHarmonics (a3,a6)
        mapM_ print list
        return ()
        let duration = 480 * 2
        --let harmonicChords = allBestHarmonics (a3,a6)
        --mapM_ print $ map (fmap findNoteFromSemitone) harmonicChords
        gen <- newStdGen
        --let shuffledChords = shuffle' harmonicChords (length harmonicChords) gen
        --exportFile "chords.mid" $ midiChords duration $ harmonicChords
        exportFile "chords.mid" $ midiChords duration $ map snd list
        