{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : SneakyTerm.Tile
Maintainer  : pmidden@secure.mailbox.org
-}
module SneakyTerm.Tile where

import           ClassyPrelude
import           Control.Lens         (makeLenses)
import           SneakyTerm.ColorPair
import           SneakyTerm.Point

-- | Represents a character on the screen with
data Tile = Tile {
    _tilePosition  :: !Point
  , _tileCharacter :: !Char
  , _tileColor     :: ColorPair
  } deriving(Eq,Show,Read)

$(makeLenses ''Tile)
