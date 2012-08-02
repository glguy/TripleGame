-- | Common types and simple operations on them.
module Types where

import Data.Array
import Data.Maybe
import Data.List (delete)

-- | Coodinates used to identify board positions.
type Coord = (Int,Int)

-- | Coordinate for "stash". This coordinate does not participate
-- in the game logic.
stashCoord :: Coord
stashCoord = (1,1)

-- | Produce a list of potentially adjacent coordinates.
neighbors :: Board -> Coord -> [Coord]
neighbors b c = filter (inRange (bounds b))
              . delete stashCoord
              $ [up c, down c, left c, right c]

-- | Move coordinates around one step.
up, down, left, right :: Coord -> Coord
up    (x,y) = (x-1,y)
down  (x,y) = (x+1,y)
left  (x,y) = (x,y-1)
right (x,y) = (x,y+1)

-- | Game board representation.
type Board = Array Coord (Maybe Piece)

-- | Test if every cell contains a piece.
isFullBoard :: Board -> Bool
isFullBoard = null . drop 1 . filter isNothing . elems
  -- This function is ugly to deal with the dead "stash" cell

-- | Construct a new game board of the given dimensions with no pieces placed.
emptyBoard ::
  Int {- ^ Rows    -} ->
  Int {- ^ Columns -} ->
  Board
emptyBoard r c = listArray ((1,1),(r,c)) (repeat Nothing)

-- | Objects that can occur on the game board.
data Piece
  = Rock | BigRock
  | Grass | Bush | Tree | House | RedHouse | Mansion | Castle | FlyingCastle | TripleCastle
  | Tombstone | Church | Cathedral | Bear Int | Ninja Int
  deriving (Eq, Show, Read, Ord)

-- | Objects that can occur in the game. This type includes includes
-- special items that can not actually exist on the board.
data InHand
  = Piece Piece
  | Crystal
  | Robot
  | BearHand
  | NinjaHand
  deriving (Eq, Show, Read)

-- | Test if holding a robot.
isRobot :: InHand -> Bool
isRobot Robot = True
isRobot _     = False

-- | Test if piece is bear-like
isNinjaOrBear :: Piece -> Bool
isNinjaOrBear Bear  {} = True
isNinjaOrBear Ninja {} = True
isNinjaOrBear _        = False

-- | Return age of piece for pieces that have one.
bearAge :: Piece -> Maybe Int
bearAge (Bear  age) = Just age
bearAge (Ninja age) = Just age
bearAge _           = Nothing
