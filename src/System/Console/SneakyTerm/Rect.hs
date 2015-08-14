{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : System.Console.SneakyTerm.Rect
Maintainer  : pmidden@secure.mailbox.org
-}
module System.Console.SneakyTerm.Rect(
      Rect
    , RectInt
    , rectLeftTop
    , rectLeft
    , rectRight
    , rectTop
    , rectBottom
    , rectWidth
    , rectHeight
    , rectIntCenter
    , rectFracCenter
    , rectRightBottom
    , rectFromPoints
    , rectFromOriginAndDim
    , rectDimensions
    , rectRightTop
    , rectLeftBottom
    ) where

import           ClassyPrelude
import           Control.Lens.Getter (Getter, to, (^.))
import           Control.Lens.TH
import           Linear.V2

type RectInt = Rect Int

-- | A type representing rectangles
data Rect a = Rect {
    _rectLeftTop     :: V2 a
  , _rectRightBottom :: V2 a
  } deriving(Show,Eq,Functor)

$(makeLenses ''Rect)

rectRightTop :: Num a => Getter (Rect a) (V2 a)
rectRightTop = to (\r -> r ^. rectLeftTop + V2 (r ^. rectWidth) 0)

rectLeftBottom :: Num a => Getter (Rect a) (V2 a)
rectLeftBottom = to (\r -> r ^. rectLeftTop + V2 0 (r ^. rectHeight))

rectLeft :: Getter (Rect a) a
rectLeft = rectLeftTop . _x

rectTop :: Getter (Rect a) a
rectTop = rectLeftTop . _y

rectRight :: Getter (Rect a) a
rectRight = rectRightBottom . _x

rectBottom :: Getter (Rect a) a
rectBottom = rectRightBottom . _y

-- | Rectangle from left top and right bottom
rectFromPoints :: V2 a -> V2 a -> Rect a
rectFromPoints = Rect

-- | Get the rectangle center, given a rectangle of integral type (uses 'div')
rectIntCenter :: Integral a => Getter (Rect a) (V2 a)
rectIntCenter = to (\r -> r ^. rectLeftTop + ((`div` 2)<$> r ^. rectDimensions))

-- | Get the rectangle center, given a rectangle of fractional type (uses '(/)')
rectFracCenter :: Fractional a => Getter (Rect a) (V2 a)
rectFracCenter = to (\r -> r ^. rectLeftTop + ((/ 2)<$> r ^. rectDimensions))

-- | Rectangle from left top and a dimension
rectFromOriginAndDim :: Num a => V2 a -> V2 a -> Rect a
rectFromOriginAndDim origin dim = rectFromPoints origin (origin + dim)

rectDimensions :: Num a => Getter (Rect a) (V2 a)
rectDimensions = to rectDimensions'
 where rectDimensions' (Rect lt rb) = rb - lt

rectWidth :: Num a => Getter (Rect a) a
rectWidth = rectDimensions . _x

rectHeight :: Num a => Getter (Rect a) a
rectHeight = rectDimensions . _y
