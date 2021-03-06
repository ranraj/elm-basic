import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import String

main = Html.beginnerProgram{model = model , view = view , update = update}

type alias Model = {
  content : String,
  error : String
  }

type Msg = Validate | NumberVal String

model: Model
model = Model "" ""

update: Msg -> Model -> Model
update action model =
  case action of
    Validate -> {model | error = parseStringToInt model.content}
    NumberVal number -> {model | content = number }

parseStringToInt: String -> String
parseStringToInt x =   
  case (String.toInt x) of
    Ok value -> toString value
    Err msg -> "Input is not a number"

view model = div [][
  input [type' "text", placeholder "Name", onInput NumberVal][]
  ,button [onClick Validate][text "Validate"]
  , div [] [text model.error]
  ]    