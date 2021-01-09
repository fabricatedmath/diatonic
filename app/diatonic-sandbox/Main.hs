module Main where

import Codec.Midi
import Linear 

import System.Random
import System.Random.Shuffle

import Math.Diatonic
import Math.Diatonic.Midi

main :: IO ()
main = 
    do
        let chords = pick4 (57-57) (57+50)
        gen <- newStdGen
        let shuffledChords = shuffle' chords (length chords) gen
        exportFile "chords.mid" $ midiChords shuffledChords