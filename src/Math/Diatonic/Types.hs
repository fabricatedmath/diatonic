{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.Diatonic.Types where

import Control.Arrow (second)

import Linear

newtype Error = Error Double
    deriving (Eq, Num, Ord, Show)
    
notes :: [String]
notes = ["a", "a#", "b", "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#"]

findNoteFromSemitone :: Int -> String
findNoteFromSemitone semitone = 
    let noteName = notes !! noteNumber ++ show noteOctave
        noteNumber = fromIntegral semitone `mod` 12
        noteOctave = (fromIntegral $ semitone + 9) `div` 12 + 4
    in noteName

frequencyToSemitone :: Double -> Double
frequencyToSemitone f = (12*) $ logBase 2 (f/440)

newtype Frequency = 
    Frequency 
    { unFrequency :: Double
    } deriving (Eq, Floating, Fractional, Ord, Num)

instance Show Frequency where
    show (Frequency f) = 
        let
            x = frequencyToSemitone f
            wholePart = round x
            decimalPart = round . (*100) $ x - fromIntegral wholePart
            note = findNoteFromSemitone wholePart
            cents = if decimalPart >= 0 then "+" <> show decimalPart else show decimalPart
        in "(" <> note <> cents <> ")"

newtype Semitone = 
    Semitone 
    { unSemitone :: Int
    } deriving (Eq, Ord, Num, Enum)

instance Show Semitone where
    show (Semitone s) = "(" <> findNoteFromSemitone s <> ":" <> show s <> ")"

fToS :: Frequency -> Semitone
fToS (Frequency f) = Semitone $ round $ frequencyToSemitone f

fToS' :: Frequency -> (Error, Semitone)
fToS' (Frequency f) = 
    let
        x = frequencyToSemitone f
        wholePart = round x
        decimalPart = x - fromIntegral wholePart
    in (Error decimalPart, Semitone wholePart)

sToF :: Semitone -> Frequency
sToF (Semitone s) = 440*2**(fromIntegral s/12)

sToF' :: Double -> Frequency
sToF' s = Frequency $ 440*2**(s/12)

shift :: Frequency -> Semitone -> Frequency
shift (Frequency f) (Semitone s) = Frequency $ f*2**(fromIntegral s/12)

(.+) :: Semitone -> Frequency -> Frequency
(.+) = flip shift

(+.) :: Frequency -> Semitone -> Frequency
(+.) = shift