module MyCounterAPI exposing (Model,Msg,view,init,update)
import Html exposing (Html,div,button,text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type alias Model = Int

init : Int -> Model 
init count = count

type Msg = Increment | Decrement

update : Msg -> Model -> Model    
update msg model = case msg of
  Increment -> model + 1
  Decrement -> model - 1

view : Model -> Html Msg
view model = div [] [
  button[onClick Increment][text "+"] 
  ,div[] [text (toString model)]
  ,button[onClick Decrement][text "-"] 
  ]