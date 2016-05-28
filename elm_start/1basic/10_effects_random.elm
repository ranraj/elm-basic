import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Random

main = Html.program{ init = init , view = view , update = update, subscriptions = subscriptions}

type alias Model = {
   dice : Int     
}

model = Model 1

type Msg = Roll | NewFace Int

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = 
  case msg of
    Roll -> (model,Random.generate NewFace (Random.int 1 6))
    NewFace newFace -> ( Model newFace,Cmd.none) 

view model = div [][
  div[] [text (toString(model.dice))]
  ,button [onClick Roll][ text "Roll"]
  ]

init : (Model, Cmd Msg)
init =
  (model , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none  