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
-- The original implementation is subject to the BSD-3 license provided below:
--
-- Copyright (c) 2010, Aristid Breitkreuz
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Aristid Breitkreuz nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- | Functions to shuffle arrays. Implementation adapted from https://hackage.haskell.org/package/random-extras-0.19/docs/src/Data-Random-Shuffle-Weighted.html#weightedShuffle

module Music.Util.Shuffle
  ( Excludable (..)
  , weightedShuffle
  ) where

import Control.Monad.Random
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

uniformExlusive :: (Excludable w, Ord w, Random w, MonadRandom m) => w -> w -> m w
uniformExlusive a b = case compare a b of
  LT -> getRandomR (b, bigger a)
  EQ -> error "Invalid exclusive uniform distribution"
  GT -> getRandomR (smaller a, b)

weightedShuffleCDF :: (Excludable w, Num w, Ord w, Random w, MonadRandom m) => Map w a -> m [a]
weightedShuffleCDF m
  | Map.null m = pure []
  | otherwise  = weightedChoiceExtractCDF m >>= \(m', a) -> (a :) <$> weightedShuffleCDF m'

weightedShuffle :: (Excludable w, Num w, Ord w, Random w, MonadRandom m) => [(w, a)] -> m [a]
weightedShuffle = weightedShuffleCDF . cdfMapFromList

weightedChoiceExtractCDF :: (Excludable w, Num w, Ord w, Random w, MonadRandom m) => Map w a -> m (Map w a, a)
weightedChoiceExtractCDF m
  | Map.null m         = error "empty map"
  | Map.null exceptMax = pure (exceptMax, maxE)
  | otherwise          = extract <$> uniformExlusive 0 wmax
  where
    ((wmax, maxE), exceptMax) = fromJust $ Map.maxViewWithKey m
    extract w = (a `Map.union` Map.mapKeysMonotonic (- gap) c, b)
      where
        (a, e, r') = Map.splitLookup w m
        r = maybe r' (\ex -> Map.insert w ex r') e
        ((k, b), c) = fromJust $ Map.minViewWithKey r
        gap = maybe 0 (\((k2, _), _) -> k2 - k) (Map.minViewWithKey c)

cdfMapFromList :: (Eq w, Num w) => [(w, a)] -> Map w a
cdfMapFromList =
  Map.fromAscListWith (const id)
  . scanl1 (\(w1, _) (w2, x) -> (w1 + w2, x))
  . dropWhile ((== 0) . fst)

class Excludable a where
  smaller :: a -> a
  bigger  :: a -> a

instance Excludable Int where { smaller = pred; bigger = succ }
instance Excludable Word where { smaller = pred; bigger = succ }
instance Excludable Integer where { smaller = pred; bigger = succ }

instance Excludable Float where { smaller = id; bigger = id }
instance Excludable Double where { smaller = id; bigger = id }

instance Excludable Bool where { smaller = not; bigger = not }
