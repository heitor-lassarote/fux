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
-- | Types and functions for generating first species counterpoint.

module Music.Fux.Counterpoint.FirstSpecies
  ( FirstSpecies (..)
  , FirstSpeciesSettings (..)
  , generateFirstSpecies
  ) where

import Music.Fux.Prelude

import Control.Monad.Logic
import Control.Monad.Random.Strict (RandT, evalRandT)
import Control.Monad.Reader

import Music.Fux.Counterpoint.CantusFirmus
import Music.Fux.Types

newtype FirstSpecies = FirstSpecies
  { getFirstSpecies :: Music (Pitch SimplePitchClass)
  } deriving stock (Eq, Show)

data FirstSpeciesSettings = FirstSpeciesSettings
  { cantusFirmus        :: CantusFirmus
  , cantusFirmusIsAbove :: Bool
  }

data FirstSpeciesReader = FirstSpeciesReader
  { noteIx :: Word
  , prev   :: Pitch SimplePitchClass
  , prevCF :: Pitch SimplePitchClass
  }

-- | Generates a first species counterpoint of two or more voices, given some
data Motion
  = Contrary
  | Direct
  | Oblique
  -- | Parallel
  deriving stock (Eq, Show)

motion
  :: (Pitch SimplePitchClass, Pitch SimplePitchClass)
  -> (Pitch SimplePitchClass, Pitch SimplePitchClass)
  -> Motion
motion (prevA, nextA) (prevB, nextB)
  -- | intA  == intB  = Parallel
  | prevA == nextA = Oblique
  | prevB == nextB = Oblique
  | cmpA  == cmpB  = Direct
  | otherwise      = Contrary
  where
    --intA = toGenericInterval $ interval' prevA nextA
    --intB = toGenericInterval $ interval' prevB nextB
    cmpA = compare prevA nextA
    cmpB = compare prevB nextB

-- Generates a first species counterpoint of two or more voices, given some
-- cantus firmus.
--
-- According to Fux, the following rules should be employed:
-- 1. [X] Arhythmic.
-- 2. [X] Only consonances are used.
-- 3. [ ] Contrary and oblique motion should be employed as often as possible.
-- 4. [ ] More imperfect than perfect consonances must be employed.
-- 5. [X] The beginning and the end must be perfect consonances.
-- 6. [X] The penultimate bar must be a major sixth if the cantus firmus is in
--    the lowest part, and a minor third if it's in the upper part.
-- 7. [X] Any of the three motions can be used when moving from an imperfect
--    consonance to another imperfect consonance.
-- 8. [X] From an imperfect to a perfect, one must go in contrary or oblique motion.
-- 9. [X] From a perfect to an imperfect, one must go in any of the three motions.
-- 10. [X] One must only move from a perfect consonance to another in contrary or
--     oblique motion.
-- 11. [X] The counterpoint must be in the same mode as the cantus firmus.
-- 12. [X] Skipping a tritone is disallowed.
-- 13. [X] Avoid overlapping voices.
-- 14. [X] It is prohibited to skip a major sixth.
-- 15. [ ] A tenth may not be brought to an octave by contrary motion. (Optional? But going to an octave or unison with a skip is disallowed)
-- 16. [X] The unison should only appear at the beginning and the end.
-- 17. [ ] Progressing from an unison into another consonance by a skip is bad.
generateFirstSpecies :: forall g. RandomGen g => g -> FirstSpeciesSettings -> Maybe FirstSpecies
generateFirstSpecies g FirstSpeciesSettings{..} = do
  firstNote <- first' cfm
  let initState = FirstSpeciesReader 0 firstNote firstNote
  counterpoint <- runReaderT (observeT (evalRandT go g)) initState
  pure $ FirstSpecies $
    (Voice $ mconcat $ Note cfs.duration <$> cfm)
    :-:
    (Voice $ mconcat $ Note cfs.duration <$> counterpoint)
  where
    CantusFirmus cfs cfm = cantusFirmus

    go :: RandT g (LogicT (ReaderT FirstSpeciesReader Maybe)) [Pitch SimplePitchClass]
    go = loop cfm

    key = cfs.pitch.pitchClass
    prev n i = predScale key cfs.key n !! i
    next n i = succScale key cfs.key n !! i

    loop
      :: [Pitch SimplePitchClass]
      -> RandT g (LogicT (ReaderT FirstSpeciesReader Maybe)) [Pitch SimplePitchClass]
    loop [] = pure []
    loop (cfNote : cfNotes) = do
      ctx <- ask
      let p = prev ctx.prev
      let s = next ctx.prev

      note <- if
        | otherwise -> chooseWeighted @Word
          [ (6, p 1), (5, p 2), (3, p 3), (1, p 4)
          , (6, s 1), (5, s 2), (3, s 3), (1, s 4)
          , (4, cfNote)
          ]

      let
        parInterval = (bool flip id cantusFirmusIsAbove) simpleInterval cfNote note
        seqInterval = (bool flip id cantusFirmusIsAbove) simpleInterval ctx.prev note
        parSonance = sonance parInterval
        motion' = motion (ctx.prevCF, cfNote) (ctx.prev, note)

      satisfy
        [ parSonance /= Dissonance  -- 2.
        , (ctx.noteIx == 0 || ctx.noteIx == cfs.length - 1) ==> sonance parInterval == Perfect  -- 5.
        , ctx.noteIx == cfs.length - 2 ==> parInterval.interval == bool Ma6' Mi3' cantusFirmusIsAbove  -- 6.
        -- Any motion is allowed except for the direct motion into a perfect consonance.
        , parSonance == Perfect ==> motion' /= Direct  -- 7. 8. 9. 10.
        , ctx.noteIx == 0 ==> bool
            (parInterval == Pe1 || parInterval == Pe5 || parInterval == Pe8)
            (parInterval == Pe1 || parInterval == Pe8)
            cantusFirmusIsAbove  -- 11.
        , seqInterval /= Tritone  -- 12.
        , bool (cfNote <= note) (note <= cfNote) cantusFirmusIsAbove  -- 13.
        , seqInterval /= Ma6  -- 14.
        , (ctx.noteIx /= 0 && ctx.noteIx /= cfs.length - 1) ==> parInterval /= Pe1  -- 16.
        ]

      local (\r -> r
        { noteIx = ctx.noteIx + 1
        , prev = note
        , prevCF = cfNote
        })
        ((note :) <$> loop cfNotes)
