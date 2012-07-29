module Types where

import Data.Array
import Data.Maybe
import Data.List (delete)

type Coord = (Int,Int)

stashCoord :: Coord
stashCoord = (1,1)

-- | Produce a list of potentially adjacent coordinates.
-- No bounds checking is possible.
neighbors :: Coord -> [Coord]
neighbors c = delete stashCoord [up c, down c, left c, right c]

up, down, left, right :: Coord -> Coord
up    (x,y) = (x-1,y)
down  (x,y) = (x+1,y)
left  (x,y) = (x,y-1)
right (x,y) = (x,y+1)

type Board = Array Coord (Maybe Piece)

isFullBoard :: Board -> Bool
isFullBoard = all isJust . elems

data Piece
  = Rock | BigRock | Grass | Bush | Tree | House | RedHouse | Mansion | Castle | FlyingCastle | TripleCastle
  | Tombstone | Church | Cathedral | Bear
  deriving (Eq, Show, Read, Ord)

data InHand
  = Piece Piece
  | Crystal
  | Robot
  deriving (Eq, Show, Read)

isRobot :: InHand -> Bool
isRobot Robot = True
isRobot _     = False

isBear :: Piece -> Bool
isBear Bear = True
isBear _    = False

-- | Return the element stored in the array if it is
-- defined and the coordinate is contained in the array.
(!?) :: Ix i => Array i (Maybe e) -> i -> Maybe e
a !? i
  | inRange (bounds a) i = a ! i
  | otherwise            = Nothing
