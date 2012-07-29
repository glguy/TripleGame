module Art where

import Graphics.Vty
import Data.Array

import Types
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
  Bear         -> ["b.d",
                   "/o\\",
                   "_|_"]

heldText Robot = "Robot"
heldText Crystal = "Crystal"
heldText (Piece p) = case p of
  Bear         -> "Bear"
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
