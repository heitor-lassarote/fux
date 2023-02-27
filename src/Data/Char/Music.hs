-- Copyright (c) 2021-2023, Heitor Toledo Lassarote de Paula
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
-- | This module provides music-related Unicode characters.
module Data.Char.Music
  (
    -- * Music characters
    doubleFlat, flat, natural, sharp, doubleSharp
  ) where

-- | Unicode character '𝄫' (U+1D12B).
doubleFlat :: Char
doubleFlat = '\x1D12B'

-- | Unicode character '♭' (U+266D).
flat :: Char
flat = '\x266D'

-- | Unicode character '♮' (U+266E).
natural :: Char
natural = '\x266E'

-- | Unicode character '♯' (U+266F).
sharp :: Char
sharp = '\x266F'

-- | Unicode character '𝄪' (U+1D12A).
doubleSharp :: Char
doubleSharp = '\x1D12A'
