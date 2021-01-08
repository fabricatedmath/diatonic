module Main where

import Codec.Midi

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
            , (0,ControlChange {channel = 1, controllerNumber = 0, controllerValue = 121})
            , (0,ControlChange {channel = 1, controllerNumber = 32, controllerValue = 0})
            , (0,ProgramChange {channel = 1, preset = 0})
            , (69120,NoteOn {channel = 1, key = 45, velocity = 124})
            , (0,NoteOn {channel = 1, key = 57, velocity = 120})
            , (391,NoteOn {channel = 1, key = 45, velocity = 0})
            , (81,NoteOn {channel = 1, key = 57, velocity = 0})
            , (8,NoteOn {channel = 1, key = 60, velocity = 120})
            , (0,NoteOn {channel = 1, key = 64, velocity = 114})
            , (0,NoteOn {channel = 1, key = 45, velocity = 120})
            , (0, TrackEnd)
            ]
    in Midi fileType timeDiv [track0, track1]

main :: IO ()
main = exportFile "test.mid" midiTrack