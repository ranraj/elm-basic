import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task
import Http

main = Html.program {init=init "cats",view=view,update = update, subscriptions = subscriptions}

type Msg = MorePlease | TopicUpdate String | FetchSucceed String | FetchFail Http.Error

type alias Model = {
  topic : String
  ,gifUrl : String
  ,error : String
}

init : String -> (Model,Cmd Msg)
init topic = (Model topic "waiting.gif" "", getRandomGif topic)

update : Msg -> Model -> (Model ,Cmd Msg)
update msg model = case msg of
  MorePlease -> (model, getRandomGif model.topic)  
  TopicUpdate newTopic -> ({model | topic = newTopic},Cmd.none)
  FetchSucceed newUrl -> (Model model.topic newUrl "", Cmd.none)  
  FetchFail error -> ( {model | gifUrl = "waiting.gif" , error = toString(error)}, Cmd.none)  

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , img [src model.gifUrl] []
    , div [style [("color","red")]] [text model.error]
    , input [onInput TopicUpdate] [ text model.topic ]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none    

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string  