module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Frequency a where
    toFreq :: a -> Double
    fromFreq :: Double -> a

freqToSemitone :: Double -> Double
freqToSemitone f = (12*) $ logBase 2 (f/440)

instance Frequency Int where
    toFreq i = 2**(fromIntegral i/12) * 440
    fromFreq f = round $ (12*) $ logBase 2 (f/440)

sumToProduct :: Double -> Double -> (Double,Double)
sumToProduct f1 f2 = 
    let
        m1 = max f1 f2
        m2 = min f1 f2
        p1 = (m1 - m2) / 2
        p2 = m2 + p1
    in (p1,p2)

majorScaleIntervals :: [Int]
majorScaleIntervals = [2,2,1,2,2,2,1]

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