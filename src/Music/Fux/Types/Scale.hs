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
-- | Provides the typeclass for generating scales, as well as keys and modes
-- which can be used to generate those scales (with pitches).
module Music.Fux.Types.Scale
  ( -- * Key
    Key (..)
  , Scale (..)
  ) where

import Prelude hiding (rem)

import Data.List (elemIndex)

import Music.Fux.Types.Pitch

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
