module Main where

import Control.Exception (bracket)
import Data.Array ((!),(//),bounds,Ix(inRange))
import Data.Maybe (isNothing)
import Graphics.Vty
import System.Random (randomRIO)

import Types
import Art
import GameLogic

data GameState = GameState
  { coord :: Coord
  , turn  :: Int
  , board :: Board
  , stash :: Maybe InHand
  }

startingGameState :: Int -> Int -> GameState
startingGameState rows cols = GameState
  { coord = (1,2)
  , turn  = 0
  , board = emptyBoard rows cols
  , stash = Nothing
  }

randomPiece :: Int -> IO InHand
randomPiece t = do
  let pieceChoices = [(Piece Grass, 27), (Piece Bush, 9), (Piece Tree, 3), (Robot, 1), (Crystal, 1), (Piece (Bear t), 3)]
  let total :: Int
      total = sum (map snd pieceChoices)
  r <- randomRIO (1,total)
  return $! select r pieceChoices
  where
  select r ((x,v):xs)
    | r <= v = x
    | otherwise = select (r-v) xs
  select _ _ = error "select: impossible"

main :: IO ()
main = bracket mkVty shutdown $ \vty -> do
  let boardRows = 6
      boardCols = 6
      gs = startingGameState boardRows boardCols
  gameLoopWithoutPiece vty gs

gameLoopWithoutPiece :: Vty -> GameState -> IO ()
gameLoopWithoutPiece vty gs = do
  p <- randomPiece (turn gs)
  gameLoop vty p gs { turn = turn gs + 1 }

gameLoop :: Vty -> InHand -> GameState -> IO ()
gameLoop vty p gs = do
    update vty Picture { pic_cursor = NoCursor
                       , pic_image  = drawGame (coord gs) p (stash gs) (board gs)
                       , pic_background = Background { background_char = ' '
                                                     , background_attr = def_attr
                                                     }
                       }
    ev <- next_event vty
    let c = coord gs
    let b = board gs
    case ev of
      EvKey k _ -> case k of
        KUp    | checkCoord (up    c) b -> gameLoop vty p gs { coord = up    (coord gs) }
        KDown  | checkCoord (down  c) b -> gameLoop vty p gs { coord = down  (coord gs) }
        KLeft  | checkCoord (left  c) b -> gameLoop vty p gs { coord = left  (coord gs) }
        KRight | checkCoord (right c) b -> gameLoop vty p gs { coord = right (coord gs) }
        KEnter | c == stashCoord        -> stashLogic vty p gs
        KEnter | isRobot p && maybe False isBear (b ! c) -> placeLogic vty (Piece Tombstone) gs
               | checkEmpty c b /= isRobot p -> placeLogic vty p gs
        KASCII 'd' | b ! c == Just BigRock -> gameLoop vty p gs { board = board gs // [(c,Nothing)] }
        KASCII 'q' -> return ()
        _ -> gameLoop vty p gs
      _ -> gameLoop vty p gs
  where
  checkCoord c b = inRange (bounds b) c
  checkEmpty c b = isNothing (b !? c)

stashLogic :: Vty -> InHand -> GameState -> IO ()
stashLogic vty p gs =
  case stash gs of
    Nothing -> gameLoopWithoutPiece vty gs {stash = Just p}
    Just s  -> gameLoop vty s gs { stash = Just p }

placeLogic :: Vty -> InHand -> GameState -> IO ()
placeLogic vty p gs = do
  let b  = board gs
  let c  = coord gs
  let b' = case p of
             Crystal -> insertCrystal c b
             Robot   -> insertPiece c Nothing  b
             Piece x -> insertPiece c (Just x) b
  b'' <- updateBears c b'
  gameLoopWithoutPiece vty gs { board = b'' }

