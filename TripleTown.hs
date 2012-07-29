module Main where

import Control.Exception (SomeException(SomeException),catch,bracket)
import Control.Monad     (guard)
import Data.Array (Array,(!),(//),bounds,listArray,Ix(..),elems,assocs)
import Data.List (foldl', elemIndex, maximumBy, find, delete)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isNothing,isJust)
import Data.Set (Set)
import Prelude hiding (catch)
import Graphics.Vty
import System.Random (randomRIO)
import qualified Data.Set as Set

import Types
import Art

-- The application logic relies on the promotion size being > 1
promotionRule :: Piece -> Maybe (Int, Maybe Piece)
promotionRule Grass        = Just (3, Just Bush)
promotionRule Bush         = Just (3, Just Tree)
promotionRule Tree         = Just (3, Just House)
promotionRule House        = Just (3, Just RedHouse)
promotionRule RedHouse     = Just (3, Just Mansion)
promotionRule Mansion      = Just (3, Just Castle)
promotionRule Castle       = Just (3, Just FlyingCastle)
promotionRule FlyingCastle = Just (4, Just TripleCastle)
promotionRule TripleCastle = Nothing
promotionRule Tombstone    = Just (3, Just Church)
promotionRule Church       = Just (3, Just Cathedral)
promotionRule Cathedral    = Just (3, Nothing)
promotionRule Rock         = Just (3, Nothing)
promotionRule Bear         = Nothing

-- | Produce a list of potentially adjacent coordinates.
-- No bounds checking is possible.
neighbors :: Coord -> [Coord]
neighbors c = delete stashCoord [up c, down c, left c, right c]

up (x,y) = (x-1,y)
down (x,y) = (x+1,y)
left (x,y) = (x,y-1)
right (x,y) = (x,y+1)

-- | Return the element stored in the array if it is
-- defined and the coordinate is contained in the array.
(!?) :: Ix i => Array i (Maybe e) -> i -> Maybe e
a !? i
  | inRange (bounds a) i = a ! i
  | otherwise            = Nothing

-- | Return the set of coordinates connected given
-- a starting location.
connectedGroup :: Coord -> Board -> Set Coord
connectedGroup c b =
  case b !? c of
    Nothing     -> Set.empty
    Just object -> search b (Just object ==) Set.empty c

search :: Board -> (Maybe Piece -> Bool) -> Set Coord -> Coord -> Set Coord
search b isMatch visited c
  | Set.member c visited || not (isMatch (b !? c)) = visited
  | otherwise = foldl' (search b isMatch) (Set.insert c visited) (neighbors c)

-- | Insert a piece into the board and reduce the board when possible.
insertPiece :: Coord -> Maybe Piece -> Board -> Board
insertPiece c mbP b = fromMaybe boardBeforePromtion mbBoardAfterPromotion
  where
  boardBeforePromtion   = b // [(c,mbP)]
  mbBoardAfterPromotion = do
    p          <- mbP
    (sz, repl) <- promotionRule p

    let clique = connectedGroup c boardBeforePromtion
    guard (Set.size clique >= sz)

    let bMinusClique = b // [(c,Nothing) | c <- Set.toList clique]
    return (insertPiece c repl bMinusClique)

-- | Insert a crystal into the board optimizing for the largest resulting
-- object and smallest initial object. If no promotion is possible, place
-- a rock.
insertCrystal :: Coord -> Board -> Board
insertCrystal c b =
  case mapMaybe (tryForPromotion b c) candidatePieces of
    [] -> insertPiece c (Just Rock) b
    xs -> selectBoard (maximumBy crystalLogic xs)
  where
  candidatePieces = mapMaybe (b !?) (neighbors c)
  crystalLogic (new1,old1,_) (new2,old2,_) = compare (new1,old2) (new2,old1)
  selectBoard (_,_,b) = b

-- | Insert a piece into the board and if promotion occurs
-- return the new pice, old piece, and resulting board
tryForPromotion :: Board -> Coord -> Piece -> Maybe (Piece,Piece,Board)
tryForPromotion b c p = do
  let b' = insertPiece c (Just p) b
  p' <- b' !? c
  guard (p /= p')
  return (p', p, b')

-- | Construct a new game board of the given dimensions with no pieces placed.
emptyBoard :: Int -> Int -> Board
emptyBoard r c = listArray ((1,1),(r,c)) (repeat Nothing)

readLn' :: Read a => IO a
readLn' = readLn `catch` \SomeException {} -> putStrLn "Bad parse" >> readLn'



randomPiece = do
  let pieceChoices = [(Piece Grass, 27), (Piece Bush, 9), (Piece Tree, 3), (Robot, 1), (Crystal, 1), (Piece Bear, 3)]
  let total :: Int
      total = sum (map snd pieceChoices)
  r <- randomRIO (1,total)
  return $! select r pieceChoices
  where
  select r ((x,v):xs)
    | r <= v = x
    | otherwise = select (r-v) xs

driver = bracket mkVty shutdown $ \vty -> do
  let boardRows = 6
      boardCols = 6
      startingCoord = (1,2)
  gameLoopWithoutPiece vty startingCoord Nothing (emptyBoard boardRows boardCols)

gameLoopWithoutPiece vty c stash b = do
  p <- randomPiece
  gameLoop vty c p stash b

gameLoop vty c p stash b = do
    update vty Picture { pic_cursor = NoCursor
                       , pic_image  = drawGame c p stash b
                       , pic_background = Background { background_char = ' '
                                                     , background_attr = def_attr
                                                     }
                       }
    ev <- next_event vty
    case ev of
      EvKey KUp    _ | checkCoord (up    c) b -> gameLoop vty (up    c) p stash b
      EvKey KDown  _ | checkCoord (down  c) b -> gameLoop vty (down  c) p stash b
      EvKey KLeft  _ | checkCoord (left  c) b -> gameLoop vty (left  c) p stash b
      EvKey KRight _ | checkCoord (right c) b -> gameLoop vty (right c) p stash b
      EvKey KEnter _ | c == stashCoord -> case stash of
                                       Nothing -> gameLoopWithoutPiece vty c (Just p) b
                                       Just s  -> gameLoop vty c s (Just p) b
      EvKey KEnter _ | checkEmpty c b /= isRobot p -> placeLogic vty c p stash b
      EvKey (KASCII 'q') _ -> return ()
      _ -> gameLoop vty c p stash b
  where
  checkCoord c b = inRange (bounds b) c
  checkEmpty c b = isNothing (b !? c)


placeLogic vty c p stash b = do
  let b' = case p of
             Crystal -> insertCrystal c b
             Robot   -> insertPiece c Nothing  b
             Piece x -> insertPiece c (Just x) b
  b'' <- updateBears b'
  gameLoopWithoutPiece vty c stash b''

updateBears :: Board -> IO Board
updateBears b = do
  let allBears = Set.fromList [c | (c, Just Bear) <- assocs b]
  (stillBears, activeBears, b') <- moveBears allBears Set.empty b
  let deadBears = findDeadBears stillBears activeBears

  return $! case Set.minView deadBears of -- should be *last* bear placed
    Nothing -> b'
    Just (bear,killBears) -> insertPiece bear (Just Tombstone) (b // [(dead, Nothing) | dead <- Set.toList killBears])

  where
  moveBears stillBears activeBears b =
    case find (hasAdjacentVacancy b) (Set.toList stillBears) of
      Nothing   -> return (stillBears, activeBears, b)
      Just bear -> do b' <- moveBear bear b
                      moveBears (Set.delete bear stillBears) (Set.insert bear activeBears) b'
    
  moveBear bear b = do
    let xs = adjacentVacancies b bear
    r <- randomRIO (0, length xs - 1)
    return (b // [(bear,Nothing),(xs!!r, Just Bear)])

hasAdjacentVacancy b c = not (null (adjacentVacancies b c))

adjacentVacancies b c = filter isValid (neighbors c)
  where
  isValid c = inRange (bounds b) c && isNothing (b ! c)

findDeadBears stillBears activeBears =
  case find adjacentToLive (Set.toList stillBears) of
    Nothing -> stillBears
    Just bear -> findDeadBears (Set.delete bear stillBears) (Set.insert bear stillBears)

  where
  adjacentToLive bear = any (`Set.member` activeBears) (neighbors bear)
