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
-- | A parser for fux types.

module Music.Fux.Parser
  ( pitchClass
  , pitch
  , voice
  , music
  , parsePitch
  , parseVoice
  , parseMusic
  ) where

import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Data.Char.Music
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec hiding (sepEndBy1)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Music.Fux.Prelude (NonEmpty, first, void)
import Music.Fux.Types

type Parser = Parsec Void Text

pitchClass :: Parser PitchClass
pitchClass = label "pitch class" do
  basePitchClass <- choice
    [ C <$ char' 'C'
    , D <$ char' 'D'
    , E <$ char' 'E'
    , F <$ char' 'F'
    , G <$ char' 'G'
    , A <$ char' 'A'
    , B <$ char' 'B'
    ]
  accidental <- choice
    [ pred . pred  <$ (string "bb" <|> string (Text.singleton doubleFlat))
    , pred <$ (char 'b' <|> char flat)
    , succ . succ <$ (string "##" <|> string (Text.singleton doubleSharp))
    , succ <$ (char '#' <|> char sharp)
    , id <$ (char 'n' <|> char natural)
    , pure id
    ]
  pure $ accidental basePitchClass

duration :: Parser Duration
duration = L.decimal

rest :: Parser ()
rest = void $ label "rest" $ char' 'r'

note :: Parser a -> Parser a
note = id

chord :: forall a. Parser a -> Parser (NonEmpty a)
chord pitchParser =
  label "chord" $ between (char '<' <* hspace) (char '>') (pitchParser `sepEndBy1` hspace1)

octave :: Parser Octave
octave = label "octave" L.decimal

pitch :: Parser (Pitch PitchClass)
pitch = label "pitch" $ Pitch <$> pitchClass <*> octave

voice :: forall a. Parser a -> Parser (Voice a)
voice pitchParser = foldr1 (:|:) <$> voiceTerminal `sepEndBy1` hspace1
  where
    voiceTerminal :: Parser (Voice a)
    voiceTerminal = do
      d <- duration
      choice
        [ Note  d <$> note pitchParser
        , Chord d <$> chord pitchParser
        , Rest  d <$  rest
        ]

music :: Parser a -> Parser (Music a)
music pitchParser = foldr1 (:-:) <$> ((Voice <$> voice pitchParser) `sepEndBy1` newline)

parse' :: Parser a -> Text -> Either String a
parse' parser =
  first errorBundlePretty
  . parse (space *> parser <* eof) ""

parsePitch :: Text -> Either String (Pitch PitchClass)
parsePitch = parse' pitch

parseVoice :: Parser a -> Text -> Either String (Voice a)
parseVoice pitchParser = parse' (voice pitchParser)

parseMusic :: Parser a -> Text -> Either String (Music a)
parseMusic pitchParser = parse' (music pitchParser)
