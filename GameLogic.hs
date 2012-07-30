-- | Support for operations directly on the Board object
-- including placing new pieces and animating the bears.
module GameLogic where

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Ord (comparing)
import Data.Set (Set)
import System.Random (randomRIO)
import qualified Data.Set as Set

import Types

-- The application logic relies on the promotion size being > 1
-- | If a promotion rule for a piece exists return it.
-- If the promotion rule replaces with a new piece it will be specified.
-- Otherwise the promotion rule deletes the piece.
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

-- | Find the set of coordinates which are connected along edges
-- and which satisfy the given predicate starting from the given
-- coordinate.
search ::
  Board                 {- ^ Board to search       -} ->
  (Maybe Piece -> Bool) {- ^ Predicate function    -} ->
  Set Coord             {- ^ Known good nodes      -} ->
  Coord                 {- ^ Start start location  -} ->
  Set Coord
search b isMatch visited c
  | not (inRange (bounds b) c) = visited
  | Set.member c visited       = visited
  | not (isMatch (b ! c))      = visited
  | otherwise                  = continue
  where
  continue = foldl' (search b isMatch) (Set.insert c visited) (neighbors c)

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

    let bMinusClique = b // [(i,Nothing) | i <- Set.toList clique]
    return (insertPiece c repl bMinusClique)

-- | Insert a crystal into the board optimizing for the largest resulting
-- object and smallest initial object. If no promotion is possible, place
-- a rock.
insertCrystal :: Coord -> Board -> Board
insertCrystal c b
  | null outcomes = insertPiece c (Just Rock) b
  | otherwise     = selectBoard (maximumBy crystalLogic outcomes)
  where
  candidatePieces = mapMaybe (b !?) (neighbors c)
  outcomes        = mapMaybe (tryForPromotion b c) candidatePieces

  crystalLogic (new1,old1,_) (new2,old2,_) = compare (new1,old2) (new2,old1)
  selectBoard  (_   ,_   ,x) = x

-- | Insert a piece into the board and if promotion occurs
-- return the new pice, old piece, and resulting board
tryForPromotion :: Board -> Coord -> Piece -> Maybe (Maybe Piece,Piece,Board)
tryForPromotion b c p = do
  let b' = insertPiece c (Just p) b
  let p' = b' !? c
  guard (Just p /= p')
  return (p', p, b')

-- | Construct a new game board of the given dimensions with no pieces placed.
emptyBoard ::
  Int {- ^ Rows    -} ->
  Int {- ^ Columns -} ->
  Board
emptyBoard r c = listArray ((1,1),(r,c)) (repeat Nothing)

-- | Find the set of connected bears and empty space starting
-- at a given coordinate.
bearGroup :: Board -> Coord -> Set Coord
bearGroup b = search b (maybe True isBear) Set.empty

-- | Attempt to collapse a group of bears starting at a given coordinate
-- if and only if the group is determined to be dead.
bearCollapse :: Board -> Coord -> Board
bearCollapse b c
  | null clique                = b
  | any coordIsEmpty clique    = b
  | otherwise                  = bWithCollapse
  where
  coordAge i      = bearAge =<< b ! i
  coordIsEmpty i  = isNothing (b ! i)
  clique          = Set.toList (bearGroup b c)
  youngestBear    = maximumBy (comparing coordAge) clique
  bKilledClique   = b // [(dead, Just Tombstone) | dead <- clique]
  bWithCollapse   = insertPiece youngestBear (Just Tombstone) bKilledClique

-- | Given a set of bears that not moved, attempt to move as many
-- bears as possible.
moveBearsHelper ::
  Set Coord {- ^ Bears that have not moved -} ->
  Board ->
  IO Board
moveBearsHelper stillBears b =
  case find (hasAdjacentVacancy b) (Set.toList stillBears) of
    Nothing   -> return b
    Just bear -> do
      b' <- movePiece bear b
      moveBearsHelper (Set.delete bear stillBears) b'

-- | Move all bears on the board and check for local bear
-- deaths.
updateBears :: Coord -> Board -> IO Board
updateBears c b = do
  let allBears = Set.fromList [i | (i, Just (Bear _)) <- assocs b]
  b' <- moveBearsHelper allBears b
  return $! foldl' bearCollapse b' (c : neighbors c)
    
-- | Move the identified bear to a random adjacent cell
-- returning the new cell and new board
movePiece ::
  Coord {- ^ Coord of piece to move -} ->
  Board ->
  IO Board
movePiece c b = do
  let xs = adjacentVacancies b c
  r <- randomRIO (0, length xs - 1)
  return (b // [(c,Nothing),(xs !! r, b ! c)])

-- | Test if a coordinate is adjacent to an empty space.
hasAdjacentVacancy :: Board -> Coord -> Bool
hasAdjacentVacancy b c = not (null (adjacentVacancies b c))

-- | Return the list of empty spaces adjacent to a coordinate.
adjacentVacancies :: Board -> Coord -> [Coord]
adjacentVacancies b c = filter isValid (neighbors c)
  where
  isValid i = inRange (bounds b) i && isNothing (b ! i)
