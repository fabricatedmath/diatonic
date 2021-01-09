module Main where

import Codec.Midi
import Linear 

import Math.Diatonic
import Math.Diatonic.Midi

main :: IO ()
main = 
    do
        let chords = pick4 (57-12) (57+12)
        exportFile "chords.mid" $ midiChords chords