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
-- | Types and functions for calculataing and dealing with intervals between
-- pitches.
module Music.Fux.Types.Interval
  (
    -- * Interval
    Interval (..)
  , GenericInterval (..)
  , toGenericInterval

  , pattern Pe1, pattern Mi2, pattern Ma2, pattern Mi3, pattern Ma3, pattern Pe4
  , pattern Au4, pattern Pe5, pattern Mi6, pattern Ma6, pattern Mi7, pattern Ma7
  , pattern Pe8

  , pattern Au1, pattern Di2, pattern Au2, pattern Di3, pattern Au3, pattern Di4
  , pattern Di5, pattern Au5, pattern Di6, pattern Au6, pattern Di7, pattern Au7
  , pattern Di8

  , pattern Tritone

  , CompoundInterval (..)
  , interval'
  , interval
  , simpleInterval

  -- * Consonance and dissonance
  , Sonance (..)
  , sonance'
  , sonance
  ) where

import Music.Fux.Types.Pitch

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

-- | A generic interval is like a simple interval, but stripped of its specifics
-- (diminished, minor, perfect, major our augmented).
data GenericInterval
  = Unison
  | Second
  | Third
  | Fourth
  | Fifth
  | Sixth
  | Seventh
  deriving stock (Bounded, Enum, Eq, Ord, Show)

toGenericInterval :: Interval -> GenericInterval
toGenericInterval = \case
  Pe1' -> Unison
  Mi2' -> Second
  Ma2' -> Second
  Mi3' -> Third
  Ma3' -> Third
  Pe4' -> Fourth
  Au4' -> Fourth
  Pe5' -> Fifth
  Mi6' -> Sixth
  Ma6' -> Sixth
  Mi7' -> Seventh
  Ma7' -> Seventh

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

pattern Tritone :: CompoundInterval
pattern Tritone = Au4

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
      LT -> CompoundInterval (octaveDiff - 1) (toEnum $      fromEnum n2 - fromEnum n1)
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
