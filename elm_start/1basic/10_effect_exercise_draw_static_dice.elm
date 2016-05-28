import Html exposing (div,button,text)
import Html.App as Html
import Html.Events exposing (onClick)
import Random
import Svg.Attributes exposing (..)
import Svg exposing (svg,polygon,rect,circle)
import Array

-- Main
main = view

-- Type
type alias DicePosition = {
  dice : Int,
  rectColor: String,
  circleColor:String,
  viewBox : String
}

dice1 = DicePosition 2 "#3399ff" "#000066" "0 0"
dice2 = DicePosition 4 "#00cc66" "#006600" "50 50"

-- View
view = div [][      
  div[] [     
    svg [ version "1.1", x "0", y "0", viewBox dice1.viewBox ] (drawDice dice1)     
    ,svg [ version "1.1", x "0", y "0", viewBox dice2.viewBox ] (drawDice dice2)
  ]   
  ]

dicePoint = Array.fromList [
  [(50,50)]
  ,[(20,20),(80,80)]
  ,[(20,20),(50,50),(80,80)]
  ,[(20,20),(80,20),(20,80),(80,80)]
  ,[(20,20),(50,50),(80,20),(20,80),(80,80)]
  ,[(20,20),(50,50),(80,20),(20,80),(80,80),(20,50),(80,50)]
  ]

getCirclesByDiceVal x = Maybe.withDefault [] (Array.get (x-1) dicePoint)  

drawDice dicePosition = drawRect dicePosition.rectColor :: (drawCircle (getCirclesByDiceVal dicePosition.dice))

drawRect fillColor = rect [ fill fillColor, stroke "#4d4d4d", x "0", y "0", width "100", height "100"][]

mapCircle (x,y) = circle [ fill "#4d4d4d", cx (toString x), cy (toString y), r "10" ] []

drawCircle : List (Int,Int) -> List (Svg.Svg a)
drawCircle circlePositions = List.map mapCircle circlePositions

