{-# LANGUAGE TemplateHaskell #-}

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
-- | Utilities for parsing fux types at compile-time.

module Music.Fux.TH
  ( pitch
  , voice
  , music
  ) where

import Data.Data (Data)
import Data.Text (Text, pack)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (dataToPatQ, liftData)

import Music.Fux.Parser (parseMusic, parsePitch, parseVoice)
import Music.Fux.Parser qualified as Parser (pitch)

mkQQ :: Data a => String -> (Text -> Either String a) -> QuasiQuoter
mkQQ parserName parser = QuasiQuoter
  { quoteExp  = liftData . parseOrThrow
  , quotePat  = dataToPatQ (const Nothing) . parseOrThrow
  , quoteType = const $ fail $ "No " <> parserName <> " QQ for type"
  , quoteDec  = const $ fail $ "No " <> parserName <> " QQ for dec"
  }
  where
    parseOrThrow = either error id . parser . pack

pitch :: QuasiQuoter
pitch = mkQQ "pitch" parsePitch

voice :: QuasiQuoter
voice = mkQQ "voice" $ parseVoice Parser.pitch

music :: QuasiQuoter
music = mkQQ "music" $ parseMusic Parser.pitch
