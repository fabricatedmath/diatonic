module Main where

import Codec.Midi

mapBanks :: Int -> Track Ticks
mapBanks c = [(0,TrackName "Stuff")] <> concatMap mapBanks' [120] <> [(0, TrackEnd)]
    where
        notes :: Track Ticks
        notes = concatMap notes' [48..60]
            where notes' i = 
                    [ (0,NoteOn {channel = c, key = i, velocity = 127})
                    , (480,NoteOff {channel = c, key = i, velocity = 127})
                    ]

        mapBanks' :: Int -> Track Ticks
        mapBanks' i = 
            [ (0,ControlChange {channel = c, controllerNumber = 0, controllerValue = 120})
            , (0,ControlChange {channel = c, controllerNumber = 32, controllerValue = 0})
            , (0,ProgramChange {channel = c, preset = 0})
            ] <> notes <> 
            [
            ]

midiTrack :: Midi
midiTrack = 
    let
        fileType = MultiTrack
        timeDiv = TicksPerBeat 480
        track0 = 
            [ (0,TrackName "Wikipedia MIDI (extended)")
            , (0,TempoChange 500000)
            , (0,TimeSignature 4 2 24 8)
            , (0,TrackEnd)
            ]
        track1 = 
            [ (0,TrackName "Piano")
            --, (0,ControlChange {channel = 1, controllerNumber = 0, controllerValue = 121})
            , (0,ControlChange {channel = 1, controllerNumber = 32, controllerValue = 0})
            , (0,ProgramChange {channel = 1, preset = 0})
            , (0,NoteOn {channel = 1, key = 45, velocity = 124})
            , (0,NoteOn {channel = 1, key = 57, velocity = 120})
            , (391,NoteOn {channel = 1, key = 45, velocity = 0})
            , (81,NoteOn {channel = 1, key = 57, velocity = 0})
            , (8,NoteOn {channel = 1, key = 60, velocity = 120})
            , (0,NoteOn {channel = 1, key = 64, velocity = 114})
            , (0,NoteOn {channel = 1, key = 45, velocity = 120})
            , (480,NoteOn {channel = 1, key = 60, velocity = 120})
            , (0,NoteOn {channel = 1, key = 64, velocity = 114})
            , (0,NoteOn {channel = 1, key = 45, velocity = 120})
            , (480,NoteOn {channel = 1, key = 60, velocity = 120})
            , (0,NoteOn {channel = 1, key = 64, velocity = 114})
            , (0,NoteOn {channel = 1, key = 45, velocity = 120})
            , (480,NoteOn {channel = 1, key = 60, velocity = 120})
            , (0,NoteOn {channel = 1, key = 64, velocity = 114})
            , (0,NoteOn {channel = 1, key = 45, velocity = 120})
            , (0, TrackEnd)
            ]
    in Midi fileType timeDiv [track0, mapBanks 0]

main :: IO ()
main = exportFile "test.mid" midiTrack