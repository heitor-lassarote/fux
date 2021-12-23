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
module Test.Music.Fux.Parser
  ( unit_parse_simple
  ) where

import Music.Fux.Parser (parseMusic, pitch)
import Music.Fux.Prelude (NonEmpty (..))
import Music.Fux.Types

import Test.Tasty.HUnit (Assertion, (@?=))

unit_parse_simple :: Assertion
unit_parse_simple =
  let
    i = " 2 < C# 3 E \n 3 G# 3 > 4 C# 3 \n 6 r"
    o =     Voice (Chord 2 (Pitch Cs 3 :| [Pitch E 3, Pitch Gs 3]) :|: Note 4 (Pitch Cs 3))
        :-: Voice (Rest 6)
  in
  parseMusic pitch i @?= Right o
