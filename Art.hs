module Art where

import Graphics.Vty
import Data.Array

import Types

data TexturePack = TexturePack
  { emptyGraphic :: Attr -> Image
  , selectedAttr :: Attr
  , stashAttr    :: Attr
  , defaultAttr  :: Attr
  , pieceGraphic :: Attr -> InHand -> Image
  }

asciiPack :: TexturePack
asciiPack = TexturePack
  { emptyGraphic = emptySquareAscii
  , selectedAttr = with_style def_attr reverse_video
  , stashAttr    = with_back_color def_attr magenta
  , defaultAttr  = def_attr
  , pieceGraphic = pieceGraphicAscii
  }

unicodePack :: TexturePack
unicodePack = TexturePack
  { emptyGraphic = emptySquareUnicode
  , selectedAttr = with_style def_attr reverse_video
  , stashAttr    = with_back_color def_attr magenta
  , defaultAttr  = def_attr
  , pieceGraphic = pieceGraphicUnicode
  }
  

--
-- Drawing Functions
--

gamePicture :: TexturePack -> Coord -> InHand -> Maybe InHand -> Board -> Picture
gamePicture pack c p s b = Picture
  { pic_cursor = NoCursor
  , pic_image  = drawGame pack c p s b
  , pic_background = Background
      { background_char = ' '
      , background_attr = def_attr
      }
  }

drawGame :: TexturePack -> Coord -> InHand -> Maybe InHand -> Board -> Image
drawGame pack c p stash b =
 char (defaultAttr pack) ' '
 <->
 (if isFullBoard b then doneImage else drawCurrent pack p)
 <->
 string (defaultAttr pack) "=================="
 <->
 drawBoard pack c stash b


doneImage :: Image
doneImage = string def_attr "Game Over"

drawCurrent :: TexturePack -> InHand -> Image
drawCurrent pack p =
  (string def_attr "Current: "
   <-> string def_attr (heldText p))
  <|> pieceGraphic pack (defaultAttr pack) p

-- | Draw board with a highlighted piece
drawBoard :: TexturePack -> Coord -> Maybe InHand -> Board -> Image
drawBoard pack cur stash b = vert_cat [draw_row r | r <- [rMin..rMax]]
  where
  ((rMin,cMin),(rMax,cMax)) = bounds b
  draw_row r = horiz_cat [draw_cell r c <|> char def_attr ' ' | c <- [cMin..cMax]]

  draw_cell r c = maybe (emptyGraphic pack a) (pieceGraphic pack a) p
    where
    a | (r,c) == cur        = selectedAttr pack
      | (r,c) == stashCoord = stashAttr    pack
      | otherwise           = defaultAttr  pack
    p | stashCoord == (r,c) = stash
      | otherwise = fmap Piece (b ! (r,c))

emptySquareAscii :: Attr -> Image
emptySquareAscii attr = stringsToImage attr 
  ["   ",
   "   ",
   " . "]

emptySquareUnicode :: Attr -> Image
emptySquareUnicode attr = string attr "⬛ "

stringsToImage :: Attr -> [String] -> Image
stringsToImage attr xs = vert_cat (map (string attr) xs)

brown, gray, orange :: Color
brown  = Color240 107
gray   = Color240 214
orange = Color240 180
                  
pieceGraphicAscii :: Attr -> InHand -> Image
pieceGraphicAscii attr inh =
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
    BearHand -> aux brown
                  ["b.d",
                   "/o\\",
                   "_|_"]
    NinjaHand -> aux brown
                 ["\\ /",
                  "\\o/",
                   "_|_"]
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
      Ninja {} -> aux brown
                 ["\\ /",
                  "\\o/",
                   "_|_"]

heldText :: InHand -> String
heldText Robot = "Robot"
heldText Crystal = "Crystal"
heldText BearHand = "Bear"
heldText NinjaHand = "Ninja"
heldText (Piece p) = case p of
  Bear {}      -> "Bear"
  Ninja {}     -> "Ninja"
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

pieceGraphicUnicode :: Attr -> InHand -> Image
pieceGraphicUnicode attr h = case h of
  Crystal -> string attr "🎆 "
  Robot -> string attr "💣 "
  BearHand -> string attr "🐻 "
  NinjaHand -> string attr "🐝 "
  Piece p -> case p of
    Rock -> string attr "🔸 "
    BigRock -> string attr "🔶 "
    Grass -> string attr "🌱 "
    Bush -> string attr "🌿 "
    Tree -> string attr "🌲 "
    House -> string attr "🏠 "
    RedHouse -> string attr "🏡 "
    Mansion -> string attr "🏤 "
    Castle -> string attr "🏭 "
    FlyingCastle -> string attr "🏯 "
    TripleCastle -> string attr "🏰 "
    Tombstone -> string attr "💀 "
    Church -> string attr "👻 "
    Cathedral -> string attr "👼 "
    Bear _ -> string attr "🐻 "
    Ninja _ -> string attr "🐝 "

-- ⛪
