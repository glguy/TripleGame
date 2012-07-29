module GameLogic where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Array
import Data.List
import System.Random
import Control.Monad

import Types

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
promotionRule Rock         = Just (3, Just BigRock)
promotionRule BigRock      = Just (3, Nothing)
promotionRule Bear {}      = Nothing

-- | Return the set of coordinates connected given
-- a starting location.
connectedGroup :: Coord -> Board -> Set Coord
connectedGroup c b =
  case b !? c of
    Nothing     -> Set.empty
    Just object -> search b (Just object ==) Set.empty c

search :: Board -> (Maybe Piece -> Bool) -> Set Coord -> Coord -> Set Coord
search b isMatch visited c
  | not (inRange (bounds b) c) || Set.member c visited || not (isMatch (b ! c)) = visited
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
tryForPromotion :: Board -> Coord -> Piece -> Maybe (Maybe Piece,Piece,Board)
tryForPromotion b c p = do
  let b' = insertPiece c (Just p) b
  let p' = b' !? c
  guard (Just p /= p')
  return (p', p, b')

-- | Construct a new game board of the given dimensions with no pieces placed.
emptyBoard :: Int -> Int -> Board
emptyBoard r c = listArray ((1,1),(r,c)) (repeat Nothing)

bearGroup :: Board -> Coord -> Set Coord
bearGroup b = search b (maybe True isBear) Set.empty

bearCollapse :: Board -> Coord -> Board
bearCollapse b c
  | Set.null clique = b
  | not (inRange (bounds b) c) = b
  | any (\c -> isNothing (b ! c)) (Set.toList clique) = b
  | otherwise = insertPiece oldestBear (Just Tombstone) (b // [(dead, Just Tombstone) | dead <- Set.toList clique])
  where
  clique = bearGroup b c
  targetAge = maximum (mapMaybe (\c -> bearAge =<< b ! c) (Set.toList clique))
  Just oldestBear = find (\x -> (bearAge =<< (b ! x)) == Just targetAge) (Set.toList clique)

-- | Given a set of bears that have moved and a set that have
-- not attempt to move as many bears as possible.
moveBearsHelper ::
  Set Coord {- ^ Bears that have not moved -} ->
  Set Coord {- ^ Bears that have moved     -} ->
  Board ->
  IO Board
moveBearsHelper stillBears activeBears b =
  case find (hasAdjacentVacancy b) (Set.toList stillBears) of
    Nothing   -> return b
    Just bear -> do
      (bear', b') <- movePiece bear b
      moveBearsHelper (Set.delete bear stillBears) (Set.insert bear' activeBears) b'

-- | Move all bears on the board and check for local bear
-- deaths.
updateBears :: Coord -> Board -> IO Board
updateBears c b = do
  let allBears = Set.fromList [c | (c, Just (Bear _)) <- assocs b]
  b' <- moveBearsHelper allBears Set.empty b
  return $! foldl' bearCollapse b' (c : neighbors c)
  where
    
-- | Move the identified bear to a random adjacent cell
-- returning the new cell and new board
movePiece ::
  Coord {- ^ Coord of piece to move -} ->
  Board ->
  IO (Coord, Board)
movePiece c b = do
  let xs = adjacentVacancies b c
  r <- randomRIO (0, length xs - 1)
  let c' = xs !! r
  return (c', b // [(c,Nothing),(c', b ! c)])

hasAdjacentVacancy :: Board -> Coord -> Bool
hasAdjacentVacancy b c = not (null (adjacentVacancies b c))

adjacentVacancies :: Board -> Coord -> [Coord]
adjacentVacancies b c = filter isValid (neighbors c)
  where
  isValid c = inRange (bounds b) c && isNothing (b ! c)
