module Main where

import Data.List (intersperse)

data Accidental = Flat | Natural | Sharp
    deriving Show

accidentalToVar :: Accidental -> String
accidentalToVar Flat = pure 'b'
accidentalToVar Natural = []
accidentalToVar Sharp = pure 's'

data Note = Note Char Accidental Int
    deriving Show

data NoteWithOctave = NoteWithOctave Int Note
    deriving Show

toVarDecl :: NoteWithOctave -> [String]
toVarDecl (NoteWithOctave o (Note n a s)) = 
    let semitone = (o * 12 + s - 57)
        semi = "Semitone " <> if semitone < 0 then "(" <> show semitone <> ")" else show semitone
        name = pure n <> accidentalToVar a <> show o
    in 
        [ name <> " :: " <> "Semitone"
        , name <> " = " <> semi
        , []
        ]


generateNoteIntervals :: [Note]
generateNoteIntervals = generateNoteIntervals' 0 noteNames majorScaleIntervals
    where 
        rotateListOnce :: [a] -> [a]
        rotateListOnce (x:xs) = xs ++ [x]

        noteNames :: [Char]
        noteNames = take 8 $ drop 2 $ cycle $ take 7 ['a'..]

        majorScaleIntervals :: [Int]
        majorScaleIntervals = cycle [2,2,1,2,2,2,1]

        generateNoteIntervals' :: Int -> [Char] -> [Int] -> [Note]
        generateNoteIntervals' s (n1:n2:ns) (i:is) = 
            [ Note n1 Natural s
            , Note n1 Sharp (s+1)
            , Note n2 Flat (s-1+i)
            ] ++ generateNoteIntervals' (s+i) (n2:ns) is
        generateNoteIntervals' _ _ _ = []

main :: IO ()
main = 
    do
        let notes = generateNoteIntervals
            noteWithOctaves = concatMap (\o -> map (NoteWithOctave o) notes) [0..8]
        --mapM_ print notes
        putStr $ unlines $ concatMap toVarDecl noteWithOctaves