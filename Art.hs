module Art where

import Graphics.Vty
import Data.Array

import Types
--
-- Drawing Functions
--

gamePicture :: Coord -> InHand -> Maybe InHand -> Board -> Picture
gamePicture c p s b = Picture
  { pic_cursor = NoCursor
  , pic_image  = drawGame c p s b
  , pic_background = Background
      { background_char = ' '
      , background_attr = def_attr
      }
  }

drawGame :: Coord -> InHand -> Maybe InHand -> Board -> Image
drawGame c p stash b =
 (if isFullBoard b then doneImage else drawCurrent p)
 <->
 string def_attr "=================="
 <->
 drawBoard c stash b


doneImage :: Image
doneImage = string def_attr "Game Over"

drawCurrent :: InHand -> Image
drawCurrent p = (string def_attr "Current: "
             <-> string def_attr (heldText p))
            <|> pieceGraphic def_attr p

-- | Draw board with a highlighted piece
drawBoard :: Coord -> Maybe InHand -> Board -> Image
drawBoard cur stash b = vert_cat [draw_row r | r <- [rMin..rMax]]
  where
  ((rMin,cMin),(rMax,cMax)) = bounds b
  draw_row r = horiz_cat [draw_cell r c <|> char def_attr ' ' | c <- [cMin..cMax]]

  draw_cell r c = maybe (emptySquare a) (pieceGraphic a) p
    where
    a | (r,c) == cur = with_style def_attr reverse_video
      | (r,c) == stashCoord = with_back_color def_attr magenta
      | otherwise = def_attr
    p | stashCoord == (r,c) = stash
      | otherwise = fmap Piece (b ! (r,c))

emptySquare :: Attr -> Image
emptySquare attr = stringsToImage attr 
  ["   ",
   "   ",
   " . "]

stringsToImage :: Attr -> [String] -> Image
stringsToImage attr xs = vert_cat (map (string attr) xs)

brown, gray, orange :: Color
brown  = Color240 107
gray   = Color240 214
orange = Color240 180
                  
pieceGraphic :: Attr -> InHand -> Image
pieceGraphic attr inh =
 let aux c xs = vert_cat ((map (string (with_fore_color attr c))) xs) in
  case inh of
    Robot -> aux orange
                  ["^ ^",
                   "o_o",
                   "|||"]
    Crystal -> aux cyan
                  [" ^ ",
                   "|||",
                   "\\_/"]
    Piece p ->
     case p of
      Rock         -> aux white
                      ["   ",
                       "   ",
                       "(@)"]
      BigRock      -> aux white
                      ["   ",
                       "(@)",
                       "(@)"]
      Grass        -> aux green
                      ["   ",
                       "   ",
                       "\\|/"]
      Bush         -> aux green
                      ["   ",
                       "   ",
                       "o8o"]
      Tree         -> vert_cat
                      [string (with_fore_color attr green) "o8o",
                       string (with_fore_color attr brown) " | ",
                       string (with_fore_color attr brown) " | "]
      House        -> aux white
                      ["   ",
                       " _ ",
                       "/o\\"]
      RedHouse     -> aux red
                      [" _ ",
                       "/_\\",
                       "|o|"]
      Mansion      -> aux yellow
                      ["/_\\",
                       "|o|",
                       "|o|"]
      Castle       -> aux gray
                      ["   ",
                       "   ",
                       "MmM"]
      FlyingCastle -> aux gray
                      ["   ",
                       "MmM",
                       "|X|"]
      TripleCastle -> aux orange
                      ["MmM",
                       "|X|",
                       "|X|"]
      Tombstone    -> aux gray
                      ["   ",
                       " _ ",
                       "| |"]
      Church       -> aux gray
                      ["_|_",
                       " | ",
                       "| |"]
      Cathedral    -> aux orange
                      ["=|=",
                       "/|\\",
                       "| |"]
      Bear {}      -> aux brown
                      ["b.d",
                       "/o\\",
                       "_|_"]

heldText :: InHand -> String
heldText Robot = "Robot"
heldText Crystal = "Crystal"
heldText (Piece p) = case p of
  Bear {}      -> "Bear"
  Rock         -> "Rock"
  BigRock      -> "Big rock"
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
