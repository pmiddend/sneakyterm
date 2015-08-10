{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Module      : System.Console.SneakyTerm.MonadTerminal
Maintainer  : pmidden@secure.mailbox.org
-}
module System.Console.SneakyTerm.MonadTerminal(
    runTerminal
  , tmRender
  , tmCharEvent
  , MonadTerminal
  , MonadTerminalM) where

import           ClassyPrelude              hiding ((\\))
import           Control.Lens               (ix, makeLenses, to, use, view,
                                             (%=), (+=), (.=), (^.), (^?!))
import           Control.Monad.State        (MonadState)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Data.List                  ((\\))
import           Data.Map.Strict            (insert)
import           Linear.V2
import           System.Console.SneakyTerm.Color
import           System.Console.SneakyTerm.ColorPair
import           System.Console.SneakyTerm.PointInt
import           System.Console.SneakyTerm.Rect
import           System.Console.SneakyTerm.Tile
import qualified UI.NCurses                 as C

-- | A type analogous to 'MonadState' and 'MonadReader'
class Monad m => MonadTerminal m where
  tmRender :: [Tile] -> m () -- ^ Render the given tiles
  tmCharEvent :: m Char -- ^ Wait for a character on standard input (blocking)

-- | Map ColorPairs to ColorIDs (a color cache, so to speak)
type ColorMap = Map ColorPair C.ColorID

data TerminalData = TerminalData {
    _tdWindow      :: C.Window
  , _tdViewport    :: Rect Int
  , _tdPrevTiles   :: [Tile]
  , _tdColors      :: ColorMap
  , _tdNextColorId :: Int
  }

$(makeLenses ''TerminalData)

newtype MonadTerminalM a = MonadTerminalM {
    runMonadTerminal :: StateT TerminalData C.Curses a
  } deriving(Monad,MonadIO,MonadState TerminalData,Applicative,Functor)

instance (Monad m,MonadTerminal m) => MonadTerminal (StateT n m) where
  tmRender ts = lift (tmRender ts)
  tmCharEvent = lift tmCharEvent

liftCurses :: C.Curses a -> MonadTerminalM a
liftCurses  = MonadTerminalM . lift

instance MonadTerminal MonadTerminalM where
  tmRender ts = do
      currentColors <- use tdColors
      let unassignedColors = (view tileColor <$> ts) \\ keys currentColors
      forM_ (toList unassignedColors) $ \c -> do
        nextColor <- use tdNextColorId
        cid <- liftCurses $ C.newColorID (toCurses (c ^. cpForeground)) (toCurses (c ^. cpBackground)) (fromIntegral nextColor)
        tdNextColorId += 1
        tdColors %= insert c cid
      colors <- use tdColors
      prevTiles <- use tdPrevTiles
      window <- use tdWindow
      liftCurses $ C.updateWindow window $ tileDiff colors prevTiles ts
      liftCurses $ C.render
      tdPrevTiles .= ts
  tmCharEvent = do
    window <- use tdWindow
    e <- liftCurses (C.getEvent window Nothing)
    case e of
      Just (C.EventCharacter c) -> return c
      _ -> tmCharEvent

tileDiff :: ColorMap -> [Tile] -> [Tile] -> C.Update ()
tileDiff cs before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ (drawTile cs) toAdd

moveCursor :: PointInt -> C.Update ()
moveCursor (V2 x y) = C.moveCursor (fromIntegral y) (fromIntegral x)

clearTile :: Tile -> C.Update ()
clearTile t = do
  moveCursor (t ^. tilePosition)
  C.drawString " "

drawTile :: ColorMap -> Tile -> C.Update ()
drawTile colors t = do
  moveCursor (t ^. tilePosition)
  C.setColor (colors ^?! ix (t ^. tileColor))
  C.drawString [t ^. tileCharacter]

runTerminal :: Rect Int -- ^ The terminal viewport
            -> MonadTerminalM () -- ^ The actions to run inside a curses environment
            -> IO ()
runTerminal viewport a = C.runCurses $ do
  C.setEcho False
  w <- C.newWindow (viewport ^. rectHeight . to fromIntegral) (viewport ^. rectWidth . to fromIntegral) 0 0
  _ <- C.setCursorMode C.CursorInvisible
  evalStateT (runMonadTerminal a) (TerminalData w viewport mempty mempty 1)

