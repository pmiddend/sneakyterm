{-|
Module      : SneakyTerm.Color
Maintainer  : pmidden@secure.mailbox.org
-}
module SneakyTerm.Color where

import           ClassyPrelude
import qualified UI.NCurses    as C

data Color = Red
           | Green
           | Yellow
           | Cyan
           | Blue
           | White
           | Transparent
           deriving(Eq,Show,Ord,Bounded,Enum,Read)

-- | Internal function to convert to ncurses colors
toCurses :: Color -> C.Color
toCurses Red = C.ColorRed
toCurses Green = C.ColorGreen
toCurses Blue = C.ColorBlue
toCurses White = C.ColorWhite
toCurses Transparent = C.ColorDefault
toCurses Yellow = C.ColorYellow
toCurses Cyan = C.ColorCyan
