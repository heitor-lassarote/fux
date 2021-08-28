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
-- | Defines and re-exports commonly used types and functions that are useful in
-- fux.

module Music.Fux.Prelude
  ( -- * 'Alternative' utilities
    choose
  , chooseWeighted
  , satisfy
  , single

    -- * Utilities
  , (==>)
  , first'
  , pass

    -- * Re-exports
  , module Exports
  ) where

import Control.Applicative as Exports (Alternative (..))
import Control.Monad as Exports ((<=<), (>=>), guard, unless, void, when)
import Control.Monad.Random.Strict as Exports (MonadRandom, Random, RandomGen)
import Data.Bifunctor as Exports
import Data.Foldable as Exports (foldl', for_, traverse_)
import Data.Function as Exports
import Data.List.NonEmpty as Exports (NonEmpty (..))
import Data.Map as Exports (Map)
import Data.Set as Exports (Set)
import Data.Traversable as Exports (for)
import Debug.Trace as Exports
import System.Random as Exports (mkStdGen)

import Music.Util.Shuffle

-- | Creates a backtracking point for each of the given elements. Calls 'empty'
-- if the collection is empty or all the 'Alternative' cases were exhausted.
choose :: (Alternative t, Foldable f) => f a -> t a
choose = foldr ((<|>) . pure) empty

-- | Similar to 'choose', but allows specifing a list of weights which will be
-- shuffled according to their weights.
chooseWeighted
  :: (Excludable w, Num w, Ord w, Random w, Alternative m, MonadRandom m)
  => [(w, a)]
  -> m a
chooseWeighted = choose <=< weightedShuffle

-- | Given some 'Alternative', will return 'empty' if any given element is 'False'.
satisfy :: (Alternative t, Foldable f) => f Bool -> t ()
satisfy = traverse_ guard

-- | A variant of 'choose' for exactly one element.
single :: Alternative t => a -> t a
single = (<|> empty) . pure

-- | The logical implication operator or _modus ponens_.
--
-- +-------+-------+-------+
-- |   p   |   q   |p ==> q|
-- +-------+-------+-------+
-- | True  | True  | True  |
-- | True  | False | False |
-- | False | True  | True  |
-- | False | False | True  |
-- +-------+-------+-------+
(==>) :: Bool -> Bool -> Bool
prerequisite ==> conclusion = not prerequisite || conclusion
infixr 1 ==>

-- | Gets the first element of some collection.
first' :: Foldable f => f a -> Maybe a
first' = foldr ((Just .) . const) Nothing

-- | An alias for `pure ()`.
pass :: Applicative f => f ()
pass = pure ()
