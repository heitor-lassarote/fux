-- Copyright (c) 2021, Heitor Toledo Lassarote de Paula
--
-- This file is part of fux.
--
--     fux is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.
--
--     fux is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with fux.  If not, see <https://www.gnu.org/licenses/>.
--
-- | Types and functions to deal with pitches and pitch classes.
module Music.Fux.Types.Pitch
  ( -- * Pitch
    PitchClass (..)
  , SimplePitchClass (..)
  , Pitch (..)
  , transposeSemitones
  , simpleToComplexPitch
  , complexToSimplePitch
  , simpleToComplexPitchClass
  , complexToSimplePitchClass

    -- * Analyzing pitch
  , Solfège (..)
  , Accidental (..)
  , pitchClassAnalysis
  , solfègeAnalysis

    -- * Octave
  , Octave
  ) where

import Music.Fux.Prelude

import Data.Data (Data)
import System.Random.Stateful (Uniform (..), UniformRange (..))

-- | The absolute pitch. It's the combination of some pitch class and the octave
-- where it's located.
data Pitch pitchClass = Pitch
  { pitchClass :: pitchClass  -- ^ The musical note to which this pitch corresponds to,
  , octave     :: Octave  -- ^ The octave of this pitch.
  } deriving stock (Data, Eq, Show)

instance Ord pitchClass => Ord (Pitch pitchClass) where
  compare (Pitch pc1 o1) (Pitch pc2 o2) = compare o1 o2 <> compare pc1 pc2

instance Enum (Pitch SimplePitchClass) where
  fromEnum (Pitch o p) = fromEnum p * 12 + fromEnum o
  toEnum i = Pitch (toEnum p) (toEnum o)
    where
      (o, p) = i `quotRem` 12

instance Uniform pitchClass => Uniform (Pitch pitchClass) where
  uniformM g = Pitch <$> uniformM g <*> uniformRM (0, 8) g

-- | Simply changes the pitch class by calling 'simpleToComplexPitchClass'.
simpleToComplexPitch :: Pitch SimplePitchClass -> Pitch PitchClass
simpleToComplexPitch (Pitch n o) = Pitch (simpleToComplexPitchClass n) o

-- | Calls 'complexToSimplePitchClass' on the pitch component, but in addition,
-- also normalizes the octave, so that a C𝄫₃, for example, becomes an A♯₂.
complexToSimplePitch :: Pitch PitchClass -> Pitch SimplePitchClass
complexToSimplePitch (Pitch n o) = Pitch (complexToSimplePitchClass n) o'
  where
    o' = case n of
      Cbb -> o - 1
      Cb  -> o - 1
      Bs  -> o + 1
      Bss -> o + 1
      _   -> o

-- | Converts a simple to a complex pitch. The relation is preserved, that is,
-- sharps remain as sharps and naturals as naturals, and the base note doesn't
-- change. No attempt to add double flats, flats or double sharps is made.
simpleToComplexPitchClass :: SimplePitchClass -> PitchClass
simpleToComplexPitchClass = \case
  { C' -> C; Cs' -> Cs
  ; D' -> D; Ds' -> Ds
  ; E' -> E
  ; F' -> F; Fs' -> Fs
  ; G' -> G; Gs' -> Gs
  ; A' -> A; As' -> As
  ; B' -> B
  }

-- | "Forgets" double flats, flats and double sharps by transforming into a
-- 'SimplePitchClass', which contains only naturals and sharps.
complexToSimplePitchClass :: PitchClass -> SimplePitchClass
complexToSimplePitchClass = \case
  { Cbb -> As'; Cb -> B' ; C -> C'; Cs -> Cs'; Css -> D'
  ; Dbb -> C' ; Db -> Cs'; D -> D'; Ds -> Ds'; Dss -> E'
  ; Ebb -> D' ; Eb -> Ds'; E -> E'; Es -> F' ; Ess -> Fs'
  ; Fbb -> Ds'; Fb -> E' ; F -> F'; Fs -> Fs'; Fss -> G'
  ; Gbb -> F' ; Gb -> Fs'; G -> G'; Gs -> Gs'; Gss -> A'
  ; Abb -> G' ; Ab -> Gs'; A -> A'; As -> As'; Ass -> B'
  ; Bbb -> A' ; Bb -> As'; B -> B'; Bs -> C' ; Bss -> Cs'
  }

-- | Whole-tone, chromatic natural and accidental notes.
data PitchClass
  = Cbb | Cb | C | Cs | Css
  | Dbb | Db | D | Ds | Dss
  | Ebb | Eb | E | Es | Ess
  | Fbb | Fb | F | Fs | Fss
  | Gbb | Gb | G | Gs | Gss
  | Abb | Ab | A | As | Ass
  | Bbb | Bb | B | Bs | Bss
  deriving stock (Bounded, Data, Eq, Ord, Show)

instance Enum PitchClass where
  fromEnum = \case
    { Cbb ->  0; Cb ->  1; C ->  2; Cs ->  3; Css ->  4
    ; Dbb ->  5; Db ->  6; D ->  7; Ds ->  8; Dss ->  9
    ; Ebb -> 10; Eb -> 11; E -> 12; Es -> 13; Ess -> 14
    ; Fbb -> 15; Fb -> 16; F -> 17; Fs -> 18; Fss -> 19
    ; Gbb -> 20; Gb -> 21; G -> 22; Gs -> 23; Gss -> 24
    ; Abb -> 25; Ab -> 26; A -> 27; As -> 28; Ass -> 29
    ; Bbb -> 30; Bb -> 31; B -> 32; Bs -> 33; Bss -> 34
    }
  toEnum i = case i `mod` 35 of
    {  0 -> Cbb;  1 -> Cb;  2 -> C;  3 -> Cs;  4 -> Css
    ;  5 -> Dbb;  6 -> Db;  7 -> D;  8 -> Ds;  9 -> Dss
    ; 10 -> Ebb; 11 -> Eb; 12 -> E; 13 -> Es; 14 -> Ess
    ; 15 -> Fbb; 16 -> Fb; 17 -> F; 18 -> Fs; 19 -> Fss
    ; 20 -> Gbb; 21 -> Gb; 22 -> G; 23 -> Gs; 24 -> Gss
    ; 25 -> Abb; 26 -> Ab; 27 -> A; 28 -> As; 29 -> Ass
    ; 30 -> Bbb; 31 -> Bb; 32 -> B; 33 -> Bs; 34 -> Bss
    ; _ -> error "impossible"
    }

instance Uniform PitchClass where
  uniformM = uniformRM (minBound, maxBound)

instance UniformRange PitchClass where
  uniformRM (l, h) g = toEnum <$> uniformRM (fromEnum l, fromEnum h) g

instance Random PitchClass

-- | Whole-tone, chromatic natural and sharp notes.
--
-- This exists to simplify various operations when working with 'PitchClass',
-- such as calculating intervals.
data SimplePitchClass
  = C' | Cs'
  | D' | Ds'
  | E'
  | F' | Fs'
  | G' | Gs'
  | A' | As'
  | B'
  deriving stock (Bounded, Data, Eq, Ord, Show)

instance Enum SimplePitchClass where
  fromEnum = \case
    { C' ->  0; Cs' ->  1
    ; D' ->  2; Ds' ->  3
    ; E' ->  4
    ; F' ->  5; Fs' ->  6
    ; G' ->  7; Gs' ->  8
    ; A' ->  9; As' -> 10
    ; B' -> 11
    }
  toEnum i = case i `mod` 12 of
    {  0 -> C';  1 -> Cs'
    ;  2 -> D';  3 -> Ds'
    ;  4 -> E'
    ;  5 -> F';  6 -> Fs'
    ;  7 -> G';  8 -> Gs'
    ;  9 -> A'; 10 -> As'
    ; 11 -> B'
    ; _ -> error "impossible"
    }

-- | Transpose some pitch by the provided number of semitones. A negative number
-- transposes down and a positive number transposes up.
transposeSemitones :: Int -> Pitch SimplePitchClass -> Pitch SimplePitchClass
transposeSemitones steps pitch =
  snd $ fix (\rec (i, x) -> if i == 0 then (i, x) else rec (i - 1, f x)) (abs steps, pitch)
  where
    f | steps <  0 = pred
      | steps == 0 = id
      | otherwise  = succ

instance Uniform SimplePitchClass where
  uniformM = uniformRM (minBound, maxBound)

instance UniformRange SimplePitchClass where
  uniformRM (l, h) g = toEnum <$> uniformRM (fromEnum l, fromEnum h) g

instance Random SimplePitchClass

data Solfège
  = Do | Re | Mi | Fa | Sol | La | Si
  deriving stock (Bounded, Data, Enum, Eq, Ord, Show)

instance Uniform Solfège where
  uniformM = uniformRM (minBound, maxBound)

instance UniformRange Solfège where
  uniformRM (l, h) g = toEnum <$> uniformRM (fromEnum l, fromEnum h) g

instance Random Solfège

data Accidental
  = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
  deriving stock (Bounded, Data, Enum, Eq, Ord, Show)

instance Uniform Accidental where
  uniformM = uniformRM (minBound, maxBound)

instance UniformRange Accidental where
  uniformRM (l, h) g = toEnum <$> uniformRM (fromEnum l, fromEnum h) g

instance Random Accidental

pitchClassAnalysis :: (Solfège -> Accidental -> a) -> PitchClass -> a
pitchClassAnalysis f p = uncurry f $ bimap toEnum toEnum $ quotRem (fromEnum p) 5

solfègeAnalysis :: (PitchClass -> a) -> Solfège -> Accidental -> a
solfègeAnalysis f note accidental = f $ toEnum $ fromEnum note * 5 + fromEnum accidental

-- | The octave of the note.
type Octave = Int
