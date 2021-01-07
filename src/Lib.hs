{-# LANGUAGE TupleSections #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Frequency a where
    toFreq :: a -> Double
    fromFreq :: Double -> a

freqToSemitone :: Double -> Double
freqToSemitone f = (12*) $ logBase 2 (f/440)

seventh :: [Rational]
seventh = [1, 5/4, 6/4, 7/4]

triad :: [Rational]
triad = [1,5/4,6/4]

instance Frequency Int where
    toFreq i = 2**(fromIntegral i/12) * 440
    fromFreq f = round $ (12*) $ logBase 2 (f/440)

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

noteNames :: [Char]
noteNames = take 7 $ drop 2 $ cycle $ take 7 ['a'..]

majorScaleIntervals :: [Int]
majorScaleIntervals = [2,2,1,2,2,2,1]

generateAccidentals :: [String]
generateAccidentals = generateAccidentals' noteNames majorScaleIntervals

generateAccidentals' :: [Char] -> [Int] -> [String]
generateAccidentals' (n1:n2:ns) (2:is) = 
    [pure n1, pure n1++"_sharp",pure n2++"_flat"] ++ generateAccidentals' (n2:ns) is
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