module Domain 
( B(..)
, Composition(..)
, CompositionMeta
, effects
, instruments
, Signal(..)
, Signals
, Time
, Track
, Tracks
, volume
, volumeChanges
) where

{-| 
 - Definitions and functions pertaining to the automated music
 - generation domain. The idea is to have the generator use 
 - preset patterns to continuously create ambient music. 
 - @author A.E.Veltstra
 - @since 2020-06-16T19:35:00.000EDT
 - @version 2020-07-12T00:14:00.000EDT
 -}


{- 1200 Units make 1 beat. 
 - The 0 beat (units [0..1199]) is for settings and does not get played.
 - The 1st played beat starts at 1200.
 -}
type Time = Integer

type RiffMeta = Integer
{-| Effect calculation rules:
    1. The effect on a beat is a fraction of the pattern effect.
    1. The effect on a pattern is a fraction of the track effect.
    2. The effect on a track is a fraction of the master effect.
    Thus an effect on a beat can never outweigh the master 
    (the composition).
 -}
data Signal = Pitch Char Int
              | Ppp
              | Pp
              | P
              | Mp
              | Mf
              | F
              | Ff
              | Fff
              | Drumset
              | Chorus
              | Gate
              | Overdrive
              | Phaser
              | Reverb
              | Tremolo
              | Wawah
              | Diminuendo
              | Crescendo
              | Decrescendo
              | Morendo
              | Start
              | Stop
              | Riff {
                    meta_r :: RiffMeta
                    , tracks_r :: Tracks
                }
              deriving (Show)

{-| The first Signals of a Track will preset settings. -}
data B = B Time Signal deriving (Show)
type Signals = [ Signal ]
instruments :: Signals
instruments = [ Drumset ]
volume :: Signals
volume = [ Ppp, Pp, P, Mp, Mf, F, Ff, Fff ]
effects :: Signals
effects = [ Chorus, Gate, Overdrive, Phaser, Reverb, Tremolo, Wawah ]
volumeChanges :: Signals
volumeChanges = [ Crescendo, Decrescendo, Diminuendo, Morendo ]

{-| Only 1 signal can exist at a time on a track.
    So if multiple signals must execute at the same time,
    they must go onto multiple tracks.
-}
type Track = [ B ]
type Tracks = [ Track ]

type CompositionMeta = Integer
data Composition = Composition {
                       meta_c :: CompositionMeta
                       , tracks_c :: Tracks
                   } deriving (Show)



----------

