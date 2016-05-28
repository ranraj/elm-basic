import Html exposing (Html,div,button)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = {  
  time : Time  
  ,status : Bool
}


init : (Model, Cmd Msg)
init =
  ( Model 0 True, Cmd.none)


-- UPDATE

type Msg
  = Tick Time | Pause | Start


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      ({model | time = newTime}, Cmd.none)
    Pause -> ({model | status = False},Cmd.none)  
    Start -> ({model | status = True},Cmd.none)  

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = if(model.status) then Time.every second Tick else Sub.none
  


-- VIEW

view : Model -> Html Msg
view model =
  let
    minAngle =
      turns (Time.inSeconds model.time)      
    secAngle =
      turns (Time.inMinutes model.time)
    hourAngle =
      turns (Time.inHours model.time)
  
    secHandX =
      toString (50 + 40 * cos secAngle)

    secHandY =
      toString (50 + 40 * sin secAngle)

    minHandX =
      toString (50 + 30 * cos minAngle)

    minHandY =
      toString (50 + 30 * sin minAngle)

    hourHandX =
      toString (50 + 30 * cos hourAngle)

    hourHandY =
      toString (50 + 30 * sin hourAngle)  

  in div [] [ 
     button [onClick Start] [text "Start"]
    ,button [onClick Pause] [text "Pause"]
    ,div [] [div [] [text (toString (Time.inMinutes model.time))]
    ,div [] [text (toString (Time.inSeconds model.time))]
    ,div [] [text (toString (Time.inHours model.time))]]
    ,svg [ viewBox "0 0 100 100", width "300px" ]
      [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , line [ x1 "50", y1 "50", x2 secHandX, y2 secHandY, stroke "#023963" ] []
      , line [ x1 "50", y1 "50", x2 minHandX, y2 minHandY, stroke "#023963" ] []
      , line [ x1 "50", y1 "50", x2 hourHandX, y2 hourHandY, stroke "#023963" ] []
      ]
    ]  