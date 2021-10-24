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
-- | Primitives types for manipulating music.
module Music.Fux.Types.Music
  ( -- * Score
    --
    -- | Basic structures for music compositions.
    Voice (..)
  , Music (..)

    -- * Duration
  , Duration
  ) where

import Music.Fux.Prelude

import Data.Data (Data)

-- | The interval in which some note or rest plays.
type Duration = Rational

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
