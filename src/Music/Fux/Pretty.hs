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
-- | Provides functions for pretty-printing various types in fux.

module Music.Fux.Pretty
  ( pitchClass
  , pitch
  , music
  , voice
  ) where

import Data.Char (intToDigit)
import Data.Char.Music
import Data.Char.Small qualified as Unicode
import Data.Foldable (toList)
import Data.Ratio (denominator, numerator)
import Text.PrettyPrint hiding ((<>))

import Music.Fux.Types

script :: forall a. Integral a => (Char -> Char) -> a -> Doc
script toScript i
  | i < 0     = char (toScript '-') <> subscript -i
  | i < 10    = integerSub i
  | otherwise = go' (divMod i 10)
  where
    go :: (a, a) -> Doc
    go (d, m)
      | d == 0    = integerSub m
      | otherwise = go' (d, m)

    go' :: (a, a) -> Doc
    go' (d, m) = go (divMod d 10) <> integerSub m

    integerSub :: a -> Doc
    integerSub = char . toScript . intToDigit . fromIntegral

subscript :: Integral a => a -> Doc
subscript = script Unicode.subscript

superscript :: Integral a => a -> Doc
superscript = script Unicode.superscript

pitchClass :: PitchClass -> Doc
pitchClass = \case
  C  -> char 'C'
  Cs -> char 'C' <> char sharp
  D  -> char 'D'
  Ds -> char 'D' <> char sharp
  E  -> char 'E'
  F  -> char 'F'
  Fs -> char 'F' <> char sharp
  G  -> char 'G'
  Gs -> char 'G' <> char sharp
  A  -> char 'A'
  As -> char 'A' <> char sharp
  B  -> char 'B'

duration :: Rational -> Doc
duration r
  | denominator r == 1 = superscript $ numerator r
  | otherwise          = superscript (numerator r) <+> superscript (denominator r)

pitch :: Pitch -> Doc
pitch (Pitch pc o) = pitchClass pc <> subscript o

voice :: (a -> Doc) -> Voice a -> Doc
voice notePrinter = \case
  a :|: b -> voice notePrinter a <+> voice notePrinter b
  Chord d ns -> char '<' <> hsep (notePrinter <$> toList ns) <> char '>' <> duration d
  Note d n -> notePrinter n <> duration d
  Rest d -> char 'r' <> duration d

music :: (a -> Doc) -> Music a -> Doc
music notePrinter = \case
  a :-: b -> music notePrinter a <> char '\n' <> music notePrinter b
  Voice v -> voice notePrinter v
