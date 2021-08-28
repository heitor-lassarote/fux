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
-- | Miscellaneous characters for musical symbols.

module Data.Char.Music
  ( doubleFlat, flat, natural, sharp, doubleSharp
  ) where

doubleFlat, flat, natural, sharp, doubleSharp :: Char
doubleFlat  = '\x1D12B'
flat        = '\x266D'
natural     = '\x266E'
sharp       = '\x266F'
doubleSharp = '\x1D12A'
