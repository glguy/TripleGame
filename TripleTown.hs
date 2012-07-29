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

pieceDistribution :: Int -> [(InHand,Int)]
pieceDistribution t =
  [ (Piece Grass, 27)
  , (Piece Bush, 9)
  , (Piece Tree, 3)
  , (Robot, 1)
  , (Crystal, 1)
  , (Piece (Bear t), 3)
  ]

main :: IO ()
main = bracket mkVty shutdown $ \vty ->
  let boardRows = 6
      boardCols = 6
      gs = startingGameState boardRows boardCols
  in gameLoopWithoutPiece vty gs

randomPiece :: Int -> IO InHand
randomPiece t = do
  let dist = pieceDistribution t
      total = sum (map snd dist)
  r <- randomRIO (1,total)
  return $! select r dist
  where
  select r ((x,v):xs)
    | r <= v = x
    | otherwise = select (r-v) xs
  select _ _ = error "select: impossible"

gameLoopWithoutPiece :: Vty -> GameState -> IO ()
gameLoopWithoutPiece vty gs = do
  p <- randomPiece (turn gs)
  gameLoop vty p gs { turn = turn gs + 1 }

gameLoop :: Vty -> InHand -> GameState -> IO ()
gameLoop vty p gs = do
    update vty (gamePicture (coord gs) p (stash gs) (board gs))
    ev <- next_event vty
    let c = coord gs
        b = board gs
    case ev of
      EvKey k _ -> case k of
        KUp    | checkCoord (up    c) b -> gameLoop vty p gs { coord = up    c }
        KDown  | checkCoord (down  c) b -> gameLoop vty p gs { coord = down  c }
        KLeft  | checkCoord (left  c) b -> gameLoop vty p gs { coord = left  c }
        KRight | checkCoord (right c) b -> gameLoop vty p gs { coord = right c }
        KEnter | c == stashCoord        -> stashLogic vty p gs
        KEnter | killsABear c b p       -> placeLogic vty (Piece Tombstone) gs
               | legalPlacement c b p   -> placeLogic vty p gs
        KASCII 'd' | b ! c == Just BigRock -> gameLoop vty p gs { board = board gs // [(c,Nothing)] }
        KASCII 'q' -> return ()
        _ -> gameLoop vty p gs
      _ -> gameLoop vty p gs
  where
  checkCoord     c b   = inRange (bounds b) c
  checkEmpty     c b   = isNothing (b ! c)
  legalPlacement c b p = checkEmpty c b /= isRobot p
  killsABear     c b p = isRobot p && maybe False isBear (b ! c) 

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

