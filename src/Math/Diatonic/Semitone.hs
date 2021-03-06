{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.Diatonic.Semitone where

import Linear

import Text.Printf

newtype Semitone = Semitone Double
    deriving (Num, Fractional, Ord, Eq)

instance Show Semitone where
    show semitone = "(" <> findNote semitone <> ", " <> printf "%0.2f" (semitoneError semitone) <> ")"

semitone :: Double -> Semitone
semitone s = Semitone $ 2**(s/12)

semitone' :: Integral a => a -> Semitone
semitone' s = Semitone $ 2**(fromIntegral s/12)

unSemitone :: Semitone -> Double
unSemitone (Semitone s) 
    | nearZero $ rsteps - steps = rsteps
    | otherwise = steps
    where
        steps = (12*) $ logBase 2 s
        rsteps = fromIntegral $ round steps

splitSemitone :: Semitone -> (Int, Double)
splitSemitone s = (rs, fromIntegral rs - steps)
    where 
        steps = unSemitone s
        rs = round steps

unSemitone' :: Semitone -> Int
unSemitone' = fst . splitSemitone

semitoneError :: Semitone -> Double
semitoneError = snd . splitSemitone

findNote :: Semitone -> String
findNote s = notes !! noteNumber ++ show noteOctave
    where
        notes :: [String]
        notes = ["a", "a#", "b", "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#"]

        semitone = unSemitone' s
        noteNumber = fromIntegral semitone `mod` 12
        noteOctave = (fromIntegral $ semitone + 9) `div` 12 + 4

toFrequency :: Semitone -> String
toFrequency (Semitone s) = printf "%0.2f" $ 440 * s