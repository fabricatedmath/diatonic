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
import Math.Diatonic.Semitone
import Math.Diatonic.Util

twoEqualNotes :: (Eq a, Foldable t) => t a -> Bool 
twoEqualNotes = go . toList
    where 
        go (x:y:xs) 
            | x == y = True
            | otherwise = go (y:xs)
        go _ = False

withinDistNotes :: (Eq a, Foldable t, Num a, Ord a) => a -> t a -> Bool 
withinDistNotes d = go . toList
    where 
        go (x:y:xs) 
            | x+d >= y = True
            | otherwise = go (y:xs)
        go _ = False


sortedNub :: (Ord a) => [a] -> [a]
sortedNub = map the . L.group . L.sort

filterAndSortRange :: (Foldable t, Ord a, Ord (t a), Sortable (t a)) => (a,a) -> [t a] -> [t a]
filterAndSortRange (minv,maxv) = sortedNub . map sort . filter (\v -> all (>= minv) v && all (<= maxv) v)

filterOn :: (Foldable t, Ord a, Ord (t a), Sortable (t a)) => (b -> t a) -> (a,a) -> [b] -> [b]
filterOn accessor (minv,maxv) = filter inRange
    where inRange b = let v = accessor b in all (>= minv) v && all (<= maxv) v

allBestHarmonics :: (Semitone,Semitone) -> [(Double, V4 Semitone, V3 Semitone)]
allBestHarmonics  r@(lowRange, highRange) = filterOn (\(a,b,c) -> b) r $ concatMap (toList . harmonicsWithError) semitones 
    where semitones = fmap semitone' <$> pick3Anchored (-48) 24

sortedHarmonics :: [(Double, V4 Semitone, V3 Semitone)]
sortedHarmonics = sortedNub $ map (\(a,b,c) -> (abs a, sortV4 b, c)) $ allBestHarmonics (c5,c8)

main :: IO ()
main = 
    do
        let list = sortedNub $ map (\(a,b,c) -> (abs a, sortV4 b, c)) $ allBestHarmonics (a0,a8)
        mapM_ print list
        --return ()
        --let duration = 480 * 2
        --let harmonicChords = allBestHarmonics (a3,a6)
        --mapM_ print $ map (fmap findNoteFromSemitone) harmonicChords
        --gen <- newStdGen
        --let shuffledChords = shuffle' harmonicChords (length harmonicChords) gen
        --exportFile "chords.mid" $ midiChords duration $ harmonicChords
        --exportFile "chords.mid" $ midiChords duration $ map snd list
        