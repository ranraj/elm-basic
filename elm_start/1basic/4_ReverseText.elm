import Html exposing (Html,text,div,input)
import Html.App as Html
import Html.Events exposing (onInput)
import String

main = Html.beginnerProgram { model = model , view = view , update = update}

type alias Model = {content : String}

model = {content = ""}

type Msg = Change String

update: Msg -> Model -> Model
update msg model = 
  case msg of
    Change newContent-> {model | content = newContent }

view: Model -> Html Msg
view model = div[] [  
  input [onInput Change] [],
  div [] [
  text (String.reverse model.content)
  ] 
  ]