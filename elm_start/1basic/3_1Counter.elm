import Html exposing (Html,text,div,button)
import Html.App as Html
import Html.Events exposing (onClick)

main = Html.beginnerProgram { model = model , view = view , update = update}

type alias Model = Int

model = 0

type Msg = Plus | Minus | Reset

update msg model = 
  case msg of
    Plus -> model + 1
    Minus -> model - 1
    Reset -> 0

view model = div[] [
  button [onClick Plus] [text "+"],
  div [] [text (toString model)],
  button [onClick Minus] [text "-"],
  button [onClick Reset][ text "Reset"]
  ]