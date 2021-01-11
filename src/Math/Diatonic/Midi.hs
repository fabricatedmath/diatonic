module Math.Diatonic.Midi where

import Data.Foldable

import Codec.Midi

import Math.Diatonic.Notes
import Math.Diatonic.Types

newtype MidiSemitone = 
    MidiSemitone { unMidiSemitone :: Int }

class MidiSemitoneable a where
    toMidiSemitone :: a -> MidiSemitone

instance MidiSemitoneable Semitone where
    toMidiSemitone (Semitone s) = MidiSemitone $ s + 57

instance MidiSemitoneable Frequency where
    toMidiSemitone = toMidiSemitone . fToS

type Duration = Int

chordToTrack :: (Foldable t, Functor t, MidiSemitoneable s) => Duration -> t s -> Track Ticks
chordToTrack duration chords = 
    concat
        [ zipWith noteOn (0:repeat 0) . toList $ semitoneChords
        , zipWith noteOff (duration:repeat 0) . toList $ semitoneChords
        ]
    where
        semitoneChords = fmap (unMidiSemitone . toMidiSemitone) chords
        noteOn time semitone = (time, NoteOn {channel = 0, key = semitone, velocity = 124})
        noteOff time semitone = (time, NoteOff {channel = 0, key = semitone, velocity = 124})

midiChords :: (Foldable t, Functor t, MidiSemitoneable s) => Duration -> [t s] -> Midi
midiChords duration chords = Midi fileType timeDiv [track0, track1]
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
            ] <> concatMap (chordToTrack duration) chords <> 
            [ (0, TrackEnd)
            ]