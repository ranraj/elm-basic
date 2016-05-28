import Html exposing (div,text)
import Html.App as Html
import Svg.Attributes exposing (stroke,fill,cx,cy,x,y,r,width,height,viewBox,version)
import Svg exposing (svg,rect,circle)

main = div [][      
  div[] [
    text ("Rectangle and Circle")      
    ,svg [ version "1.1", x "0", y "0", viewBox "0 0" ]
    [   
      rect [ fill "#3399ff", stroke "#4d4d4d", x "0", y "0", width "100", height "100"][]    
      ,circle [ fill "#000066", cx "50", cy "50", r "50" ] []
    ]    
  ]    
  ]
        
  