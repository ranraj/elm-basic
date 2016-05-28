import Html exposing (div,button,text)
import Html.App as Html
import Html.Events exposing (onClick)
import Random
import Svg.Attributes exposing (..)
import Svg exposing (svg,polygon,rect,circle)
import Array

-- Main
main = Html.program{ init = init , view = view , update = update, subscriptions = subscriptions}

-- Type
type alias Model = {
   dice1 : Int
   ,dice2 : Int     
}
type alias DicePosition = {
  dice : Int,
  rectColor: String,
  circleColor:String,
  viewBox : String
}
type Msg = Roll | NewFace (Int,Int)

model = Model 1 1
dice1 = DicePosition 1 "#3399ff" "#000066" "0 0"
dice2 = DicePosition 1 "#00cc66" "#006600" "50 50"

-- Update
update : Msg -> Model -> (Model,Cmd Msg)
update msg model = 
  case msg of
    Roll -> (model,rollUpdate)
    NewFace (newFace1,newFace2) -> ({model | dice1 = newFace1 , dice2 = newFace2} , Cmd.none)     

rollUpdate : Cmd Msg
rollUpdate = Random.generate NewFace randomPoint

randomPoint : Random.Generator (Int,Int)
randomPoint = Random.pair (Random.int 1 6) (Random.int 1 6)

-- View
view model = div [][      
  div[] [
     text (toString(model.dice1))      
    ,svg [ version "1.1", x "0", y "0", viewBox dice1.viewBox ] (drawDice model.dice1 dice1) 
    ,text (toString(model.dice2))
    ,svg [ version "1.1", x "0", y "0", viewBox dice2.viewBox ] (drawDice model.dice2 dice2)
  ]  
  ,button [onClick Roll][ text "Roll"]
  ]

-- Init
init : (Model, Cmd Msg)
init =
  (model , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none  



dicePoint = Array.fromList [
  [(50,50)]
  ,[(20,20),(80,80)]
  ,[(20,20),(50,50),(80,80)]
  ,[(20,20),(80,20),(20,80),(80,80)]
  ,[(20,20),(50,50),(80,20),(20,80),(80,80)]
  ,[(20,20),(50,50),(80,20),(20,80),(80,80),(20,50),(80,50)]
  ]

getCirclesByDiceVal x = Maybe.withDefault [] (Array.get (x-1) dicePoint)  

drawDice diceVal dicePosition = drawRect dicePosition.rectColor :: (drawCircle (getCirclesByDiceVal diceVal))

drawRect fillColor = rect [ fill fillColor, stroke "#4d4d4d", x "0", y "0", width "100", height "100"][]

mapCircle (x,y) = circle [ fill "#4d4d4d", cx (toString x), cy (toString y), r "10" ] []

drawCircle : List (Int,Int) -> List (Svg.Svg a)
drawCircle circlePositions = List.map mapCircle circlePositions

