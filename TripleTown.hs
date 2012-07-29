module Main where

import Control.Exception (SomeException(SomeException),catch,bracket)
import Control.Monad     (guard)
import Data.Array (Array,(!),(//),bounds,listArray,Ix(..),elems,assocs)
import Data.List (foldl', elemIndex, maximumBy, find, delete)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isNothing,isJust)
import Prelude hiding (catch)
import Graphics.Vty
import System.Random (randomRIO)

import Types
import Art
import GameLogic



readLn' :: Read a => IO a
readLn' = readLn `catch` \SomeException {} -> putStrLn "Bad parse" >> readLn'


randomPiece turn = do
  let pieceChoices = [(Piece Grass, 27), (Piece Bush, 9), (Piece Tree, 3), (Robot, 1), (Crystal, 1), (Piece (Bear turn), 3)]
  let total :: Int
      total = sum (map snd pieceChoices)
  r <- randomRIO (1,total)
  return $! select r pieceChoices
  where
  select r ((x,v):xs)
    | r <= v = x
    | otherwise = select (r-v) xs

main = bracket mkVty shutdown $ \vty -> do
  let boardRows = 6
      boardCols = 6
      startingCoord = (1,2)
      turn = 0
  gameLoopWithoutPiece vty startingCoord turn Nothing (emptyBoard boardRows boardCols)

gameLoopWithoutPiece vty c turn stash b = do
  p <- randomPiece turn
  gameLoop vty c p (turn + 1) stash b

gameLoop vty c p turn stash b = do
    update vty Picture { pic_cursor = NoCursor
                       , pic_image  = drawGame c p stash b
                       , pic_background = Background { background_char = ' '
                                                     , background_attr = def_attr
                                                     }
                       }
    ev <- next_event vty
    case ev of
      EvKey KUp    _ | checkCoord (up    c) b -> gameLoop vty (up    c) p turn stash b
      EvKey KDown  _ | checkCoord (down  c) b -> gameLoop vty (down  c) p turn stash b
      EvKey KLeft  _ | checkCoord (left  c) b -> gameLoop vty (left  c) p turn stash b
      EvKey KRight _ | checkCoord (right c) b -> gameLoop vty (right c) p turn stash b
      EvKey KEnter _ | c == stashCoord -> case stash of
                                       Nothing -> gameLoopWithoutPiece vty c turn (Just p) b
                                       Just s  -> gameLoop vty c s turn (Just p) b
      EvKey KEnter _ | isRobot p && maybe False isBear (b ! c) -> placeLogic vty c (Piece Tombstone) turn stash b
                     | checkEmpty c b /= isRobot p -> placeLogic vty c p turn stash b
      EvKey (KASCII 'd') _ | b ! c == Just BigRock -> gameLoop vty c p turn stash (b // [(c,Nothing)])
      EvKey (KASCII 'q') _ -> return ()
      _ -> gameLoop vty c p turn stash b
  where
  checkCoord c b = inRange (bounds b) c
  checkEmpty c b = isNothing (b !? c)


placeLogic vty c p turn stash b = do
  let b' = case p of
             Crystal -> insertCrystal c b
             Robot   -> insertPiece c Nothing  b
             Piece x -> insertPiece c (Just x) b
  b'' <- updateBears c b'
  gameLoopWithoutPiece vty c turn stash b''

