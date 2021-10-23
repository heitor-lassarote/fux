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
-- | Provides types and functions for dealing with musical primitives.

module Music.Fux.Types
  ( -- * Score
    --
    -- | Basic structures for music compositions.
    Voice (..)
  , Music (..)

    -- * Pitch
  , PitchClass (..)
  , SimplePitchClass (..)
  , Pitch (..)
  , transposeSemitones
  , simpleToComplexPitch
  , complexToSimplePitch
  , simpleToComplexPitchClass
  , complexToSimplePitchClass
  , Solfège (..)
  , Accidental (..)
  , pitchClassAnalysis
  , solfègeAnalysis

    -- * Key
  , Key (..)
  , Scale (..)

    -- * Duration
  , Duration

    -- * Octave
  , Octave

    -- * Interval
  , Interval (..)

  , pattern Pe1, pattern Mi2, pattern Ma2, pattern Mi3, pattern Ma3, pattern Pe4
  , pattern Au4, pattern Pe5, pattern Mi6, pattern Ma6, pattern Mi7, pattern Ma7
  , pattern Pe8

  , pattern Au1, pattern Di2, pattern Au2, pattern Di3, pattern Au3, pattern Di4
  , pattern Di5, pattern Au5, pattern Di6, pattern Au6, pattern Di7, pattern Au7
  , pattern Di8

  , CompoundInterval (..)
  , interval'
  , interval
  , simpleInterval

  , Sonance (..)
  , sonance'
  , sonance
  ) where

import Prelude hiding (rem)

import Music.Fux.Prelude

import Data.Data (Data)
import Data.List (elemIndex)
import System.Random
import System.Random.Stateful (Uniform (..), UniformRange (..))

-- | Represents a single melodic line.
data Voice a
  = Voice a :|: Voice a
  -- ^ Compose two voices sequentially.
  | Chord Duration (NonEmpty a)
  -- ^ A sequence of musical notes that are played simutaneously.
  | Note Duration a
  -- ^ A single musical note.
  | Rest Duration
  -- ^ A pause, as opposed to a note.
  deriving stock (Data, Eq, Foldable, Functor, Show, Traversable)

instance Semigroup (Voice a) where
  Note  0 _ <> b         = b
  Rest  0   <> b         = b
  Chord 0 _ <> b         = b
  a         <> Note  0 _ = a
  a         <> Rest  0   = a
  a         <> Chord 0 _ = a
  a         <> b         = a :|: b

instance Monoid (Voice a) where
  mempty = Rest 0

-- | The abstract syntax tree for some music score.
data Music a
  = Music a :-: Music a
  -- ^ Compose two musics in parallel.
  | Voice (Voice a)
  -- ^ A piece with a single voice.
  deriving stock (Data, Eq, Foldable, Functor, Show, Traversable)

instance Semigroup (Music a) where
  (<>) = (:-:)

instance Monoid (Music a) where
  mempty = Voice mempty

-- | The absolute pitch. It's the combination of some pitch class and the octave
-- where it's located.
data Pitch pitchClass = Pitch
  { pPitchClass :: pitchClass  -- ^ The musical note to which this pitch corresponds to,
  , pOctave     :: Octave  -- ^ The octave of this pitch.
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

data Accidental
  = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
  deriving stock (Bounded, Data, Enum, Eq, Ord, Show)

pitchClassAnalysis :: (Solfège -> Accidental -> a) -> PitchClass -> a
pitchClassAnalysis f p = uncurry f $ bimap toEnum toEnum $ quotRem (fromEnum p) 5

solfègeAnalysis :: (PitchClass -> a) -> Solfège -> Accidental -> a
solfègeAnalysis f note accidental = f $ toEnum $ fromEnum note * 5 + fromEnum accidental

-- | Represents the key for some scale, chord or signature.
data Key
  = Minor
  | Major
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Allows generating a scale for some given pitch.
class Scale pitch mode where
  type PitchClassFor pitch

  -- | Generates the standard descending scale starting from some pitch, given
  -- the scale mode.
  desScaleFor :: pitch -> mode -> [pitch]
  -- | Generates the standard ascending scale starting from some pitch, given
  -- the scale mode.
  ascScaleFor :: pitch -> mode -> [pitch]
  -- | The descending given scale from a certain pitch in the scale of the given pitch class.
  -- If the note is not part of the scale, returns an empty list.
  predScale :: PitchClassFor pitch -> mode -> pitch -> [pitch]
  -- | The ascending given scale from a certain pitch in the scale of the given pitch class.
  -- If the note is not part of the scale, returns an empty list.
  succScale :: PitchClassFor pitch -> mode -> pitch -> [pitch]

defaultDesScaleImplementationForWholeToneKey :: forall a. Enum a => a -> Key -> [a]
defaultDesScaleImplementationForWholeToneKey pitch = \case
  Minor -> dom : tim : lam : som : fam : mim : rem : defaultDesScaleImplementationForWholeToneKey dom' Minor
  Major -> doM : tiM : laM : soM : faM : miM : reM : defaultDesScaleImplementationForWholeToneKey doM' Major
  where
    p, pp :: a -> a
    p = pred
    pp = p . p
    (dom, tim, lam, som, fam, mim, rem, dom') =
      (pitch, pp dom, pp tim, p lam, pp som, pp fam, p mim, pp rem)
    (doM, tiM, laM, soM, faM, miM, reM, doM') =
      (pitch, p doM, pp tiM, pp laM, pp soM, p faM, pp miM, pp reM)

defaultAscScaleImplementationForWholeToneKey :: forall a. Enum a => a -> Key -> [a]
defaultAscScaleImplementationForWholeToneKey pitch = \case
  Minor -> dom : rem : mim : fam : som : lam : tim : defaultAscScaleImplementationForWholeToneKey dom' Minor
  Major -> doM : reM : miM : faM : soM : laM : tiM : defaultAscScaleImplementationForWholeToneKey doM' Major
  where
    s, ss :: a -> a
    s = succ
    ss = s . s
    (dom, rem, mim, fam, som, lam, tim, dom') =
      (pitch, ss dom, s rem, ss mim, ss fam, s som, ss lam, ss tim)
    (doM, reM, miM, faM, soM, laM, tiM, doM') =
      (pitch, ss doM, ss reM, s miM, ss faM, ss soM, ss laM, s tiM)

instance Scale SimplePitchClass Key where
  type PitchClassFor SimplePitchClass = SimplePitchClass

  desScaleFor = defaultDesScaleImplementationForWholeToneKey
  ascScaleFor = defaultAscScaleImplementationForWholeToneKey
  predScale base mode ref = maybe [] (flip drop d) (elemIndex ref $ take 7 d)
    where
      d = desScaleFor base mode
  succScale base mode ref = maybe [] (flip drop s) (elemIndex ref $ take 7 s)
    where
      s = ascScaleFor base mode

instance Scale (Pitch SimplePitchClass) Key where
  type PitchClassFor (Pitch SimplePitchClass) = SimplePitchClass

  desScaleFor = defaultDesScaleImplementationForWholeToneKey
  ascScaleFor = defaultAscScaleImplementationForWholeToneKey
  predScale base mode ref = case i of
    Nothing -> []
    Just i' -> drop i' (desScaleFor (Pitch base $ (if ref' <= base then id else succ) oct) mode)
    where
      i = elemIndex ref' $ take 8 d
      Pitch ref' oct = ref
      d = desScaleFor base mode
  succScale base mode ref = case i of
    Nothing -> []
    Just i' -> drop i' (ascScaleFor (Pitch base oct) mode)
    where
      i = elemIndex ref' $ take 8 s
      Pitch ref' oct = ref
      s = ascScaleFor base mode

-- | The interval in which some note or rest plays.
type Duration = Rational

-- | The octave of the note.
type Octave = Int

-- | A simple interval represents the relative distance between two notes.
--
-- The prefixes represent Augmented, Major, Perfect, Minor or Diminished.
data Interval
  = Pe1'
  | Mi2' | Ma2'
  | Mi3' | Ma3'
  | Pe4' | Au4'
  | Pe5'
  | Mi6' | Ma6'
  | Mi7' | Ma7'
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Represents a compound interval, which is like a simple interval but also
-- accounts for skips greater than an octave.
data CompoundInterval = CompoundInterval
  { ciOctaves :: Word  -- ^ How many octaves it has skipped. 0 means it's absolute.
  , ciInterval :: Interval  -- ^ The simple interval.
  } deriving stock (Bounded, Eq, Ord, Show)

pattern Pe1, Mi2, Ma2, Mi3, Ma3, Pe4, Au4, Pe5, Mi6, Ma6, Mi7, Ma7, Pe8 :: CompoundInterval
pattern Pe1 = CompoundInterval 0 Pe1'
pattern Mi2 = CompoundInterval 0 Mi2'
pattern Ma2 = CompoundInterval 0 Ma2'
pattern Mi3 = CompoundInterval 0 Mi3'
pattern Ma3 = CompoundInterval 0 Ma3'
pattern Pe4 = CompoundInterval 0 Pe4'
pattern Au4 = CompoundInterval 0 Au4'
pattern Pe5 = CompoundInterval 0 Pe5'
pattern Mi6 = CompoundInterval 0 Mi6'
pattern Ma6 = CompoundInterval 0 Ma6'
pattern Mi7 = CompoundInterval 0 Mi7'
pattern Ma7 = CompoundInterval 0 Ma7'
pattern Pe8 = CompoundInterval 1 Pe1'

pattern Au1, Di2, Au2, Di3, Au3, Di4, Di5, Au5, Di6, Au6, Di7, Au7, Di8 :: CompoundInterval
pattern Au1 = Mi2
pattern Di2 = Pe1
pattern Au2 = Mi3
pattern Di3 = Ma2
pattern Au3 = Pe4
pattern Di4 = Ma3
pattern Di5 = Au4
pattern Au5 = Mi6
pattern Di6 = Pe5
pattern Au6 = Mi7
pattern Di7 = Ma6
pattern Au7 = Pe8
pattern Di8 = Ma7

-- | Calculate the simple interval between two pitches.
interval' :: Pitch PitchClass -> Pitch PitchClass -> Interval
interval' p1 p2 = ciInterval $ interval p1 p2

-- | Calculate the compound interval between two pitches.
interval :: Pitch PitchClass -> Pitch PitchClass -> CompoundInterval
interval p1 p2 = simpleInterval (complexToSimplePitch p1) (complexToSimplePitch p2)

-- | Calculate the compound interval between two simple pitches.
simpleInterval :: Pitch SimplePitchClass -> Pitch SimplePitchClass -> CompoundInterval
simpleInterval (Pitch n1 o1) (Pitch n2 o2) =
  case compare n1 n2 of
    LT -> case compare o1 o2 of
      LT -> CompoundInterval  octaveDiff      (toEnum $      fromEnum n2 - fromEnum n1)
      EQ -> CompoundInterval  octaveDiff      (toEnum $      fromEnum n2 - fromEnum n1)
      GT -> CompoundInterval (octaveDiff - 1) (toEnum $ 12 + fromEnum n1 - fromEnum n2)
    EQ -> CompoundInterval octaveDiff Pe1'
    GT -> case compare o1 o2 of
      LT -> CompoundInterval (octaveDiff - 1) (toEnum $ 12 + fromEnum n2 - fromEnum n1)
      EQ -> CompoundInterval  octaveDiff      (toEnum $      fromEnum n1 - fromEnum n2)
      GT -> CompoundInterval (octaveDiff - 1) (toEnum $      fromEnum n1 - fromEnum n2)
  where
    octaveDiff :: Word
    octaveDiff = fromIntegral $ abs (o1 - o2)

-- | Represents whether some simple or compound interval is a perfect
-- consonance, imperfect consonance or dissonance.
data Sonance
  = Perfect  -- ^ A perfect consonance.
  | Imperfect  -- ^ An imperfect consonance.
  | Dissonance  -- ^ A perfect dissonance.
  deriving stock (Eq, Ord, Show)

-- | Calculate whether the given simple interval represents a perfect
-- consonance, imperfect consonance or dissonance.
--
-- Note: Fux considers the perfect fourth a dissonance.
sonance' :: Interval -> Sonance
sonance' = \case
  Pe1' -> Perfect
  Pe5' -> Perfect
  Mi3' -> Imperfect
  Ma3' -> Imperfect
  Mi6' -> Imperfect
  Ma6' -> Imperfect
  Mi2' -> Dissonance
  Ma2' -> Dissonance
  Pe4' -> Dissonance
  Au4' -> Dissonance
  Mi7' -> Dissonance
  Ma7' -> Dissonance

-- | Calculate whether the given compound interval represents a perfect
-- consonance, imperfect consonance or dissonance.
--
-- Note: Fux considers the perfect fourth a dissonance.
sonance :: CompoundInterval -> Sonance
sonance = sonance' . ciInterval
