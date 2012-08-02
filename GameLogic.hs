-- | Support for operations directly on the Board object
-- including placing new pieces and animating the bears.
module GameLogic (updateBears, insertPiece, insertCrystal, liveInfection, moveBearsHelper) where

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Shuffle

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
promotionRule Ninja {}     = Nothing

-- | Return the set of coordinates connected given
-- a starting location.
connectedGroup :: Coord -> Board -> Set Coord
connectedGroup c b = search b (b!c ==) Set.empty c

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
  | Set.member c visited       = visited
  | isMatch (b ! c)            = continue
  | otherwise                  = visited
  where
  continue = foldl' (search b isMatch) (Set.insert c visited) (neighbors b c)

-- | Insert a piece into the board and reduce the board when possible.
insertPiece :: Coord -> Maybe Piece -> Board -> Board
insertPiece c mbP b = reduceBoard (b // [(c,mbP)]) c

-- | Reduce the board when possible from the given coordinate.
reduceBoard :: Board -> Coord -> Board
reduceBoard b c = fromMaybe b mbBoardAfterPromotion
  where
  mbBoardAfterPromotion = do
    p          <- b!c
    (sz, repl) <- promotionRule p

    let clique = connectedGroup c b
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
  candidatePieces = mapMaybe (b !) (neighbors b c)
  outcomes        = mapMaybe (tryForPromotion b c) candidatePieces

  -- The greatest element is the one which creates the largest piece using
  -- the smallest piece.
  crystalLogic (new1,old1,_) (new2,old2,_) = compare (new1,old2) (new2,old1)
  selectBoard  (_   ,_   ,x) = x

-- | Insert a piece into the board and if promotion occurs
-- return the new pice, old piece, and resulting board
tryForPromotion :: Board -> Coord -> Piece -> Maybe (Maybe Piece,Piece,Board)
tryForPromotion b c p = do
  let b' = insertPiece c (Just p) b
  let p' = b' ! c
  guard (Just p /= p')
  return (p', p, b')

-- | Move all bears that can move and return
-- (still bears, moved bears, resulting board)
moveBears :: Board -> IO ([Coord],[Coord],Board)
moveBears b = moveBearsHelper allBears [] b
  where allBears = [i | (i, Just p) <- assocs b, isNinjaOrBear p]

-- | Given a set of bears that not moved, attempt to move as many
-- bears as possible.
moveBearsHelper ::
  [Coord] {- ^ Bears that have not moved -} ->
  [Coord] {- ^ Bears that have moved     -} ->
  Board ->
  IO ([Coord],[Coord], Board)
moveBearsHelper stillBears liveBears b =
  case find canMove stillBears of
    Nothing   -> return (stillBears, liveBears, b)
    Just bear -> do
      (bear', b') <- doMove bear
      moveBearsHelper
        (delete bear stillBears)
        (bear' : liveBears)
        b'
  where
  canMove c = case b ! c of
    Just (Bear  {}) -> hasAdjacentVacancy b c
    Just (Ninja {}) -> not (isFullBoard b)
    _ -> error "moveBearsHelper:canMove: impossible"

  doMove c = case b!c of
    Just (Bear  {}) -> walkPiece c b
    Just (Ninja {}) -> jumpPiece c b
    _ -> error "moveBearsHelper:doMove: impossible"


-- | Move all bears on the board and check for bear deaths.
updateBears :: Board -> IO Board
updateBears b = do
  (stillBears, movedBears, bAfterMoves) <- moveBears b
  let liveBears = liveInfection bAfterMoves movedBears
  let deadBears = stillBears \\ liveBears

  -- Do a mass update to ensure that reductions can account for
  -- all of the tombstones rather than using 'insertPiece' repeatedly
  let bWithTombstones = bAfterMoves // [(i, Just Tombstone) | i <- deadBears]

  -- Reductions must be considered in order of bear youth.
  -- Multiple reductions could occur due to bear groups being
  -- disjoint.
  let coordAge i   = bearAge =<< b ! i
  let orderedBears = sortBy (flip (comparing coordAge)) deadBears
  return $! foldl' reduceBoard bWithTombstones orderedBears

-- | Find the set of live bears given a list of bears that have moved.
liveInfection :: Board -> [Coord] -> [Coord]
liveInfection b cs =
  Set.toList (foldl' (search b (maybe False isNinjaOrBear)) Set.empty cs)
    
-- | Move the identified bear to a random adjacent cell
-- returning the new cell and new board
walkPiece :: Coord -> Board -> IO (Coord, Board)
walkPiece c b = do
  c' <- randomElement (adjacentVacancies b c)
  return (c', relocate c c' b)

-- | Move the identified piece to a random empty cell
-- returning the new cell and new board
jumpPiece :: Coord -> Board -> IO (Coord, Board)
jumpPiece c b = do
  c' <- randomElement (empties b)
  return (c', relocate c c' b)

-- | Move a piece from one cell to another leaving
-- the original cell empty.
relocate ::
  Coord {- ^ from -} -> 
  Coord {- ^ to   -} -> 
  Board ->
  Board
relocate c c' b = b // [(c,Nothing),(c', b ! c)]

-- | Test if a coordinate is adjacent to an empty space.
hasAdjacentVacancy :: Board -> Coord -> Bool
hasAdjacentVacancy b c = not (null (adjacentVacancies b c))

-- | Return the list of empty spaces adjacent to a coordinate.
adjacentVacancies :: Board -> Coord -> [Coord]
adjacentVacancies b c = filter isVacant (neighbors b c)
  where
  isVacant i = isNothing (b ! i)

empties :: Board -> [Coord]
empties b = delete stashCoord [i | (i, Nothing) <- assocs b]
