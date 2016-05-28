import Html exposing (div,text)
import Html.App as Html
import Svg.Attributes exposing (..)
import Svg exposing (svg,rect,circle)
import Array exposing (..)
main = div [][      
  div[] [
    text ("Rectangle and Circle")      
    ,svg [ version "1.1", x "0", y "0", viewBox "0 0" ] (drawRect ++(circletList circlePoints))
    ]
  ] 

circlePoints = [(50,50),(60,60)]

circletList : List (Int,Int) -> List (Svg.Svg a)
circletList circleData = List.map drawCircles circleData

drawCircles (x,y) = circle [ fill "#000066", stroke "#00cc44", cx (toString x), cy (toString y), r "50" ] []

drawRect = [   
      rect [ fill "#3399ff", stroke "#4d4d4d", x "0", y "0", width "100", height "100"][]          
    ]
        
  