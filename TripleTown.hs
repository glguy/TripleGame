import Control.Exception (SomeException(SomeException),catch,bracket)
import Control.Monad     (guard)
import Data.Array (Array,(!),(//),bounds,listArray,Ix(..),elems)
import Data.List (foldl', elemIndex, maximumBy, transpose)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isNothing,isJust)
import Data.Set (Set)
import Prelude hiding (catch)
import Graphics.Vty
import System.Random (randomRIO)
import qualified Data.Set as Set

stashCoord = (1,1)

type Coord = (Int,Int)
type Board = Array Coord (Maybe Piece)

data Piece
  = Rock | Grass | Bush | Tree | House | RedHouse | Mansion | Castle | FlyingCastle | TripleCastle
  | Tombstone | Church | Cathedral
  deriving (Eq, Show, Read, Ord)

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

-- | Produce a list of potentially adjacent coordinates.
-- No bounds checking is possible.
neighbors :: Coord -> [Coord]
neighbors c = [up c, down c, left c, right c]

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
    Just object -> search Set.empty c
     where
     search visited c
       | Set.member c visited || b !? c /= Just object = visited
       | otherwise = foldl' search (Set.insert c visited) (neighbors c)

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

isFullBoard :: Board -> Bool
isFullBoard = all isJust . elems

readLn' :: Read a => IO a
readLn' = readLn `catch` \SomeException {} -> putStrLn "Bad parse" >> readLn'


data InHand
  = Piece Piece
  | Crystal
  | Robot

isRobot Robot = True
isRobot _     = False

randomPiece = do
  let pieceChoices = [Grass, Bush, Tree]
  loop pieceChoices
  where
  loop [] = do
    r <- randomRIO (0,1::Int)
    return $! if r == 0
                then Crystal
                else Robot
  loop (x:xs) = do
    r <- randomRIO (0, 2 :: Int)
    if r == 0 then loop xs else return (Piece x)

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
  gameLoopWithoutPiece vty c stash b'

--
-- Drawing Functions
--

drawGame :: Coord -> InHand -> Maybe InHand -> Board -> Image
drawGame c p stash b =
 (if isFullBoard b then doneImage else drawCurrent p)
 <->
 string def_attr "=================="
 <->
 drawBoard c stash b


doneImage :: Image
doneImage = string def_attr "Game Over"

drawCurrent p = (string def_attr "Current: "
             <-> string def_attr (heldText p))
            <|> pieceImage def_attr p

-- | Draw board with a highlighted piece
drawBoard :: Coord -> Maybe InHand -> Board -> Image
drawBoard cur stash b = vert_cat [draw_row r | r <- [rMin..rMax]]
  where
  ((rMin,cMin),(rMax,cMax)) = bounds b
  draw_row r = horiz_cat [draw_cell r c <|> char def_attr ' ' | c <- [cMin..cMax]]

  draw_cell r c = maybe (emptyImage a) (pieceImage a) p
    where
    a | (r,c) == cur = with_style def_attr reverse_video
      | (r,c) == stashCoord = with_back_color def_attr magenta
      | otherwise = def_attr
    p | stashCoord == (r,c) = stash
      | otherwise = fmap Piece (b ! (r,c))

renderPiece :: Piece -> Char
renderPiece Grass        = 'G'
renderPiece Bush         = 'B'
renderPiece Tree         = 'T'
renderPiece House        = 'H'
renderPiece RedHouse     = 'R'
renderPiece Mansion      = 'M'
renderPiece Castle       = 'C'
renderPiece FlyingCastle = 'F'
renderPiece TripleCastle = '!'
renderPiece Tombstone    = 't'
renderPiece Church       = 'c'
renderPiece Cathedral    = 'a'
renderPiece Rock         = 'r'

emptyImage attr = stringsToImage attr emptySquare
emptySquare = ["   ",
               "   ",
               " . "]

stringsToImage attr xs = vert_cat (map (string attr) xs)

pieceImage attr p = stringsToImage attr (pieceGraphic p)

pieceGraphic Robot =
                  ["^ ^",
                   "o_o",
                   "|||"]

pieceGraphic Crystal =
                  [" ^ ",
                   "|||",
                   "\\_/"]
                  
pieceGraphic (Piece p) = case p of
  Rock         -> ["   ",
                   "   ",
                   "(@)"]
  Grass        -> ["   ",
                   "   ",
                   "\\|/"]
  Bush         -> ["   ",
                   "   ",
                   "o8o"]
  Tree         -> ["o8o",
                   " | ",
                   " | "]
  House        -> ["   ",
                   " _ ",
                   "/o\\"]
  RedHouse     -> [" _ ",
                   "/_\\",
                   "|o|"]
  Mansion      -> ["/_\\",
                   "|o|",
                   "|o|"]
  Castle       -> ["   ",
                   "   ",
                   "MmM"]
  FlyingCastle -> ["   ",
                   "MmM",
                   "|X|"]
  TripleCastle -> ["MmM",
                   "|X|",
                   "|X|"]
  Tombstone    -> ["   ",
                   " _ ",
                   "| |"]
  Church       -> ["_|_",
                   " | ",
                   "| |"]
  Cathedral    -> ["=|=",
                   "/|\\",
                   "| |"]

heldText Robot = "Robot"
heldText Crystal = "Crystal"
heldText (Piece p) = case p of
  Rock         -> "Rock"
  Grass        -> "Grass"
  Bush         -> "Bush"
  Tree         -> "Tree"
  House        -> "House"
  RedHouse     -> "Red house"
  Mansion      -> "Mansion"
  Castle       -> "Castle"
  FlyingCastle -> "Flying castle"
  TripleCastle -> "Triple castle"
  Tombstone    -> "Tombstone"
  Church       -> "Church"
  Cathedral    -> "Cathedral"
