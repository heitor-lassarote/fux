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
  , generateFirstSpecies
  ) where

import Music.Fux.Prelude

import Control.Monad.Logic
import Control.Monad.Random.Strict (RandT, evalRandT)
import Control.Monad.Reader

import Music.Fux.Counterpoint.CantusFirmus
import Music.Fux.Types

newtype FirstSpecies = FirstSpecies { getFirstSpecies :: Music Pitch }
  deriving stock (Eq, Show)

data FirstSpeciesReader = FirstSpeciesReader
  { fsrNoteIx :: Word
  , fsrPrev   :: Pitch
  , fsrPrevCF :: Pitch
  }

-- Generates a first species counterpoint of two or more voices, given some
-- cantus firmus.
--
-- According to Fux, the following rules should be employed:
-- 1. [X] Arhythmic.
-- 2. [X] Only consonances are used.
-- 3. [ ] Contrary and oblique motion should be employed as often as possible.
-- 4. [ ] More imperfect than perfect consonances must be employed.
-- 5. [ ] The beginning and the end must be perfect consonances.
-- 6. [ ] The penultimate bar there must be a major sixth if the cantus firmus is in
--    the lowest part, and a minor third if it's in the upper part.
-- 7. [ ] Any of the three motions can be used when moving from an imperfect
--    consonance to another imperfect consonance.
-- 8. [ ] From an imperfect to a perfect, one must go in contrary motion.
-- 9. [ ] From a perfect to an imperfect, one must go in contrary motion.
-- 10. [ ] One must only move from a perfect consonance to another in contrary or
--     oblique motion.
-- 11. [ ] The counterpoint must be in the same mode as the cantus firmus.
-- 12. [ ] Skipping a tritone is disallowed.
-- 13. [ ] Avoid overlapping voices.
-- 14. [ ] It is prohibited to skip a major sixth.
-- 15. [ ] A tenth may not be brought to an octave by contrary motion. (Optional? But going to an octave or unison with a skip is disallowed)
-- 16. [ ] The unison should only appear at the beginning and the end.
-- 17. [ ] Progressing from an unison into another consonance is bad.
generateFirstSpecies :: forall g. RandomGen g => g -> CantusFirmus -> Maybe FirstSpecies
generateFirstSpecies g (CantusFirmus CantusFirmusSettings{..} cfm) = do
  firstNote <- first cfm
  let initState = FirstSpeciesReader 0 firstNote firstNote
  counterpoint <- runReaderT (observeT (evalRandT go g)) initState
  pure $ FirstSpecies $
    (Voice $ mconcat $ Note cfsDuration <$> cfm)
    :-:
    (Voice $ mconcat $ Note cfsDuration <$> counterpoint)
  where
    go :: RandT g (LogicT (ReaderT FirstSpeciesReader Maybe)) [Pitch]
    go = loop cfm

    key = pPitchClass cfsPitch
    prev n i = predScale key cfsKey n !! i
    next n i = succScale key cfsKey n !! i

    loop :: [Pitch] -> RandT g (LogicT (ReaderT FirstSpeciesReader Maybe)) [Pitch]
    loop [] = pure []
    loop (cfNote : cfNotes) = do
      FirstSpeciesReader{..} <- ask
      let p = prev fsrPrev
      let s = next fsrPrev

      note <- if
        | otherwise -> chooseWeighted @Word
          [ (6, p 1), (5, p 2), (3, p 3), (1, p 4)
          , (6, s 1), (5, s 2), (3, s 3), (1, s 4)
          , (1, cfNote)
          ]

      let parInterval = interval cfNote note

      satisfy
        [ sonance parInterval /= Dissonance  -- 2.
        ]

      local (\r -> r
        { fsrNoteIx = fsrNoteIx + 1
        })
        ((note :) <$> loop cfNotes)
