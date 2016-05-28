import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task

main = Html.program {init=init,view=view,update = update, subscriptions = subscriptions}

type Msg = MorePlease

type alias Model = {
  topic : String
  ,gifUrl : String  
}

model = Model "" ""

init : (Model,Cmd Msg)
init = (model,Cmd.none)

update : Msg -> Model -> (Model ,Cmd Msg)
update msg model = case msg of
  MorePlease -> (model,Cmd.none)  

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , img [src model.gifUrl] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none    

  