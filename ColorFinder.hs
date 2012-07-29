import Graphics.Vty
import Control.Exception

main = bracket mkVty shutdown (aux 1)

aux color vty = do
  update vty (pic_for_image (string (with_fore_color def_attr (Color240 color)) (show color)))
  ev <- next_event vty
  case ev of
    EvKey KUp   _ -> aux (color+1) vty
    EvKey KDown _ -> aux (color-1) vty
    EvKey KEsc _ -> return ()
    _ -> aux color vty
