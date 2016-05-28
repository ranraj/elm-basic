import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Random

main = Html.program{ init = init , view = view , update = update, subscriptions = subscriptions}

type alias Model = {
   dice1 : Int
   ,dice2 : Int     
}

model = Model 1 1

type Msg = Roll | NewFace (Int,Int)

update : Msg -> Model -> (Model,Cmd Msg)
update msg model = 
  case msg of
    Roll -> (model,rollUpdate)
    NewFace (newFace1,newFace2) -> ({model | dice1 = newFace1 , dice2 = newFace2} , Cmd.none)     

randomPoint : Random.Generator (Int,Int)
randomPoint =
    Random.pair (Random.int 0 6) (Random.int 0 6)

rollUpdate : Cmd Msg
rollUpdate =
  Random.generate NewFace randomPoint

view model = div [][
  div[] [text (toString(model.dice1))]
  ,div[] [text (toString(model.dice2))]
  ,button [onClick Roll][ text "Roll"]
  ]

init : (Model, Cmd Msg)
init =
  (model , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none  