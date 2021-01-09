module Math.Diatonic.Midi where

import Data.Foldable

import Codec.Midi

chordToTrack :: Foldable t => t Int -> Track Ticks
chordToTrack chords = 
    concat
        [ zipWith noteOn (0:repeat 0) . toList $ chords
        , zipWith noteOff (240:repeat 0) . toList $ chords
        ]
    where
        noteOn time semitone = (time, NoteOn {channel = 0, key = semitone, velocity = 124})
        noteOff time semitone = (time, NoteOff {channel = 0, key = semitone, velocity = 124})

midiChords :: Foldable t => [t Int] -> Midi
midiChords chords = Midi fileType timeDiv [track0, track1]
    where
        fileType = MultiTrack
        timeDiv = TicksPerBeat 480
        track0 = 
            [ (0,TrackName "Autogen Track")
            , (0,TempoChange 500000)
            , (0,TimeSignature 4 2 24 8)
            , (0,TrackEnd)
            ]
        track1 = 
            [ (0,TrackName "Piano")
            -- , (0,ControlChange {channel = 0, controllerNumber = 0, controllerValue = 121})
            , (0,ControlChange {channel = 0, controllerNumber = 32, controllerValue = 0})
            , (0,ProgramChange {channel = 0, preset = 0})
            ] <> concatMap chordToTrack chords <> 
            [ (0, TrackEnd)
            ]