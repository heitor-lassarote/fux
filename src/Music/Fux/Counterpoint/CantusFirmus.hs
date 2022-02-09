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
-- | Types and functions for generating cantus firmi.

module Music.Fux.Counterpoint.CantusFirmus
  ( CantusFirmus (..)
  , CantusFirmusSettings (..)
  , generateCantusFirmus
  ) where

import Music.Fux.Prelude

import Control.Monad.Logic
import Control.Monad.Random.Strict (RandT, evalRandT, getRandomR)
import Control.Monad.Reader

import Music.Fux.Types

-- | A simple melody respecting the rules for the cantus firmus. The settings
-- used to generate the cantus firmus is packed for usage in other generators,
-- such as the one for first species counterpoint.
--
-- It is recommended that you generate this data type using
-- 'generateCantusFirmus' in order to avoid discrepancies between the generated
-- melody and the settings used, as not all configurations allow for the
-- generation of a well-formed cantus firmus.
data CantusFirmus = CantusFirmus
  { cfSettings :: CantusFirmusSettings  -- ^ The settings used to generate the cantus firmus.
  , cfMelody   :: [Pitch SimplePitchClass]  -- ^ The melody of the cantus firmus.
  } deriving stock (Eq, Show)

-- | Settings for the cantus firmus generator.
data CantusFirmusSettings = CantusFirmusSettings
  { cfsPitch    :: Pitch SimplePitchClass  -- ^ The first and last note of the cantus firmus.
  , cfsDuration :: Duration  -- ^ How long each note should be.
  , cfsLength   :: Word  -- ^ The quantity of notes.
  , cfsKey      :: Key  -- ^ The key in which the cantus firmus should be generated. The key signature is given together with 'cfsPitch'.
  } deriving stock (Eq, Show)

data Direction = Down | Up deriving stock Eq

-- Invariant: Notes are different.
getDirection :: Ord pitch => pitch -> pitch -> Direction
getDirection prev next
  | prev <= next = Up
  | otherwise    = Down

data CantusFirmusReader = CantusFirmusReader
  { cfrLeapIx    :: Word
  , cfrClimax    :: Pitch SimplePitchClass
  , cfrClimaxIx  :: Word
  , cfrHasClimax :: Bool
  , cfrNoteIx    :: Word
  , cfrPrev      :: Pitch SimplePitchClass
  , cfrPrevInt   :: CompoundInterval
  , cfrDirection :: Direction
  }

-- | Generates a cantus firmus with the given settings. We use the rules
-- described in [Open Music Theory](http://openmusictheory.com/firstSpecies.html)
-- for the generation of the melody.
--
-- The cantus firmus is composed with the following aspects:
-- 1. [X] Arhythmic.
-- 2. [X] Begin and end on do.
-- 3. [X] Approach final tonic by step (usually re-do, sometimes ti-do).
-- 4. [X] All note-to-note progressions are melodic consonances.
--   a. No augmented or diminished intervals.
--   b. No sevenths.
-- 5. [X] Range is no more than a tenth.
-- 6. [X] A single climax.
-- 7. [?] Clear logic connection and smooth shape from beginning to climax to ending.
-- 8. [X] Mostly stepwise motion, but with some leaps.
-- 9. [ ] No repetition.
-- 10. [X] All large leaps are followed by a step in the opposite direction.
-- 11. [X] No more than two leaps in a row.
-- 12. [X] No consecutive leaps in the same direction.
-- 13. [X] The leading tone progresses to the tonic.
-- 14. [X] In minor, the leading tone only appears in the penultimate bar.
--
-- The following tendencies are used:
-- 1. [X] Do a step with greater probability than a leap, and a small leap with
--    greater probability than a big leap.
-- 2. [X] Decrease a step with greater probability than increase a step.
-- 3. [ ] Stay in the same direction with greater probability than changing direction.
-- 4. [ ] Go towards the middle when in a extreme register.
-- 5. [X] Ascend in the first half then descend in the second half.
--
-- In Fux's recommendations:
-- 1. [X] No augmented, diminished or chromatic intervals (4.).
-- 2. [\] No intervals larger than the fifth (except the octave and the minor sixth,
--    the latter employed only in an upward direction).
-- 3. [X] Avoid too high or too low registers (5.).
-- 4. [X] Avoid successive skips in the same direction (12.).
-- 5. [X] Avoid skips which are not compensated (10.)
generateCantusFirmus :: forall g. RandomGen g => g -> CantusFirmusSettings -> Maybe CantusFirmus
generateCantusFirmus g settings@CantusFirmusSettings{..} =
  CantusFirmus settings <$> runReaderT (observeT (evalRandT go g)) initState
  where
    initState :: CantusFirmusReader
    initState = CantusFirmusReader 0 cfsPitch 0 False 0 cfsPitch Pe1 Down

    go :: RandT g (LogicT (ReaderT CantusFirmusReader Maybe)) [Pitch SimplePitchClass]
    go = do
      stepsAbove <- getRandomR (4, 9)  -- 5.
      let cfrClimaxIx = cfsLength `div` 2
      -- FIXME: Generate climax around the time, not only exactly there
      local (\r -> r
        { cfrClimax = ascScaleFor cfsPitch cfsKey !! stepsAbove
        , cfrClimaxIx
        })
        loop

    key = pPitchClass cfsPitch
    prev n i = predScale key cfsKey n !! i
    next n i = succScale key cfsKey n !! i

    loop :: RandT g (LogicT (ReaderT CantusFirmusReader Maybe)) [Pitch SimplePitchClass]
    loop = do
      CantusFirmusReader{..} <- ask
      let p = prev cfrPrev
      let s = next cfrPrev

      note <- if
        | cfrNoteIx == 0 || cfrNoteIx == cfsLength - 1 -> single cfsPitch  -- 2.
        | cfrNoteIx == cfsLength - 2 -> chooseWeighted @Word  -- 3.
          [ (6, next cfsPitch 1)
          , (4, pred cfsPitch)
          ]
        | cfrNoteIx == cfrClimaxIx -> single cfrClimax
        | cfrPrevInt >= Pe4 -> single case cfrDirection of  -- 10.
          Down -> s 1
          Up   -> p 1
        -- TODO: Make it less likely to do skips, particularly the octave.
        | cfrHasClimax -> chooseWeighted @Word
          [ (9, p 1), (5, p 2), (3, p 3), (1, p 4), (1, p 7)
          , (6, s 1), (3, s 2), (2, s 3), (1, s 4), (1, s 7)
          ]
        | otherwise -> chooseWeighted @Word
          [ (7, s 1), (4, s 2), (2, s 3), (1, s 4), (1, s 7)
          , (6, p 1), (3, p 2), (2, p 3), (1, p 4), (1, p 7)
          ]

      if cfrNoteIx == cfsLength
        then [] <$ guard cfrHasClimax  -- 6.
        else do
          let seqInterval = simpleInterval cfrPrev note
          let direction = getDirection cfrPrev note
          let isLeap = seqInterval > Ma2
          let wasLeap = cfrPrevInt > Ma2

          satisfy
            [ simpleInterval note cfrClimax <= CompoundInterval 1 Ma3'  -- 5.
            , cfrNoteIx /= cfrClimaxIx ==> note < cfrClimax  -- 6.
            , seqInterval /= Au4  -- 4.a.
            , seqInterval /= Ma7 && seqInterval /= Mi7  -- 4.b.
            , isLeap ==> cfrLeapIx <= 1  -- 11.
            , (wasLeap && isLeap) ==> direction /= cfrDirection -- 12.
            , pPitchClass cfrPrev == pred key ==> pPitchClass note == key  -- 13.
            , (cfsKey == Minor && cfrNoteIx < cfsLength - 2) ==> note /= pred cfsPitch  -- 14.
            , seqInterval <= Pe8  -- Recommendation 2.
            ]

          local (\r -> r
            { cfrNoteIx = cfrNoteIx + 1
            , cfrPrev = note
            , cfrHasClimax = cfrHasClimax || note == cfrClimax
            , cfrPrevInt = seqInterval
            , cfrDirection = direction
            , cfrLeapIx = if isLeap then cfrLeapIx + 1 else 0
            })
            ((note :) <$> loop)
