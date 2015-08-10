{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : System.Console.SneakyTerm.ColorPair
Maintainer  : pmidden@secure.mailbox.org
-}
module System.Console.SneakyTerm.ColorPair where

import           ClassyPrelude
import           Control.Lens     (makeLenses)
import           System.Console.SneakyTerm.Color

data ColorPair = ColorPair {
    _cpForeground :: !Color
  , _cpBackground :: !Color
  } deriving (Eq,Show,Read,Ord)

$(makeLenses ''ColorPair)


