{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : System.Console.SneakyTerm.Tile
Maintainer  : pmidden@secure.mailbox.org
-}
module System.Console.SneakyTerm.Tile where

import           ClassyPrelude
import           Control.Lens         (makeLenses)
import           System.Console.SneakyTerm.ColorPair
import           System.Console.SneakyTerm.PointInt

-- | Represents a character on the screen
data Tile = Tile {
    _tilePosition  :: !PointInt -- ^ Tile position
  , _tileCharacter :: !Char -- ^ Which character to display
  , _tileColor     :: ColorPair -- ^ Color for the character
  } deriving(Eq,Show,Read)

$(makeLenses ''Tile)
