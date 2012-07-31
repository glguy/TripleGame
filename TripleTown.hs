module Main where

import Control.Exception (bracket)
import Data.Array ((!),bounds,Ix(inRange))
import Data.Maybe (isNothing)
import Graphics.Vty

import Types
import Art
import GameLogic
import Shuffle

data GameState = GameState
  { coord :: Coord
  , turn  :: Int
  , board :: Board
  , stash :: Maybe InHand
  , textures :: TexturePack
  }

startingGameState :: Int -> Int -> GameState
startingGameState rows cols = GameState
  { coord = (1,2)
  , turn  = 0
  , board = emptyBoard rows cols
  , stash = Nothing
  , textures = unicodePack
  }

pieceDistribution :: [(InHand,Int)]
pieceDistribution =
  [ (Piece Grass   , 60)
  , (Piece Bush    , 15)
  , (Piece Tree    ,  2)
  , (Robot         ,  3)
  , (Crystal       ,  3)
  , (BearHand      , 15)
  , (NinjaHand     ,  1)
  ]

main :: IO ()
main = bracket mkVty shutdown $ \vty ->
  let boardRows = 6
      boardCols = 6
      gs = startingGameState boardRows boardCols
  in gameLoopWithoutPiece vty gs

randomPiece :: IO InHand
randomPiece = randomElementDist pieceDistribution

gameLoopWithoutPiece :: Vty -> GameState -> IO ()
gameLoopWithoutPiece vty gs = do
  p <- randomPiece
  gameLoop vty p gs { turn = turn gs + 1 }

gameLoop :: Vty -> InHand -> GameState -> IO ()
gameLoop vty p gs = do
    update vty (gamePicture (textures gs) (coord gs) p (stash gs) (board gs))
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
        KEnter | killsABear c b         -> placeLogic vty (Piece Tombstone) gs
               | legalPlacement c b     -> placeLogic vty p gs
        KASCII 'q' -> return ()
        _ -> gameLoop vty p gs
      _ -> gameLoop vty p gs
  where
  checkCoord     c b = inRange (bounds b) c
  checkEmpty     c b = isNothing (b ! c)
  legalPlacement c b = checkEmpty c b /= isRobot p
  killsABear     c b = isRobot p && maybe False isNinjaOrBear (b ! c) 

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
             BearHand -> insertPiece c (Just (Bear (turn gs)))  b
             NinjaHand -> insertPiece c (Just (Ninja (turn gs)))  b
             Piece x -> insertPiece c (Just x) b
  b'' <- updateBears c b'
  gameLoopWithoutPiece vty gs { board = b'' }

