{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : SneakyTerm.ColorPair
Maintainer  : pmidden@secure.mailbox.org
-}
module SneakyTerm.ColorPair where

import           ClassyPrelude
import           Control.Lens     (makeLenses)
import           SneakyTerm.Color

data ColorPair = ColorPair {
    _cpForeground :: !Color
  , _cpBackground :: !Color
  } deriving (Eq,Show,Read,Ord)

$(makeLenses ''ColorPair)


