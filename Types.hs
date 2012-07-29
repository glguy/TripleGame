module Types where

import Data.Array
import Data.Maybe

type Coord = (Int,Int)
type Board = Array Coord (Maybe Piece)

stashCoord :: Coord
stashCoord = (1,1)

data Piece
  = Rock | Grass | Bush | Tree | House | RedHouse | Mansion | Castle | FlyingCastle | TripleCastle
  | Tombstone | Church | Cathedral | Bear
  deriving (Eq, Show, Read, Ord)

data InHand
  = Piece Piece
  | Crystal
  | Robot

isRobot Robot = True
isRobot _     = False

isFullBoard :: Board -> Bool
isFullBoard = all isJust . elems

