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
module Test.Music.Fux.Pitch
  ( prop_pitchClassAnalysisAfterSolfègeAnalysisIsIdentity
  , prop_solfègeAnalysisAfterPitchClassAnalysisIsIdentity
  , prop_intervalIsSymmetric
  ) where

import Music.Fux.Types

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- TODO: Refactor instances to common module when needed.
instance Arbitrary SimplePitchClass where
  arbitrary = chooseAny

instance Arbitrary PitchClass where
  arbitrary = chooseAny

instance Arbitrary Solfège where
  arbitrary = chooseAny

instance Arbitrary Accidental where
  arbitrary = chooseAny

instance Arbitrary pitchClass => Arbitrary (Pitch pitchClass) where
  arbitrary = Pitch <$> arbitrary <*> arbitrary

prop_pitchClassAnalysisAfterSolfègeAnalysisIsIdentity :: PitchClass -> Bool
prop_pitchClassAnalysisAfterSolfègeAnalysisIsIdentity pitchClass =
  uncurry (solfègeAnalysis id) (pitchClassAnalysis (,) pitchClass) == pitchClass

prop_solfègeAnalysisAfterPitchClassAnalysisIsIdentity :: Solfège -> Accidental -> Bool
prop_solfègeAnalysisAfterPitchClassAnalysisIsIdentity note accidental =
  pitchClassAnalysis (,) (solfègeAnalysis id note accidental) == (note, accidental)

prop_intervalIsSymmetric :: Pitch PitchClass -> Pitch PitchClass -> Bool
prop_intervalIsSymmetric p1 p2 = interval p1 p2 == interval p2 p1
