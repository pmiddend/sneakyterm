module Main where

import           ClassyPrelude
import           Linear.V2
import           SneakyTerm.Color
import           SneakyTerm.ColorPair
import           SneakyTerm.Rect
import           SneakyTerm.TerminalMonad
import           SneakyTerm.Tile

main :: IO ()
main = runTerminal (rectFromOriginAndDim (V2 0 0) (V2 80 25)) $ do
  tmRender [Tile{_tilePosition=V2 10 10,_tileCharacter='@',_tileColor=ColorPair{_cpForeground=Red,_cpBackground=Green}}]
  _ <- tmCharEvent
  return ()
