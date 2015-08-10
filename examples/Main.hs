module Main where

import           ClassyPrelude
import           Linear.V2
import           System.Console.SneakyTerm.Color
import           System.Console.SneakyTerm.ColorPair
import           System.Console.SneakyTerm.Rect
import           System.Console.SneakyTerm.MonadTerminal
import           System.Console.SneakyTerm.Tile

main :: IO ()
main = runTerminal (rectFromOriginAndDim (V2 0 0) (V2 80 25)) $ do
  tmRender [Tile (V2 10 10) '@' (ColorPair Red Green)]
  _ <- tmCharEvent
  return ()
