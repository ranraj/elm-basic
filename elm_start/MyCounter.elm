import MyCounterAPI
import Html.App as App
import Html exposing (Html, button, div, text)
import Dict exposing (Dict)

main = App.beginnerProgram{model = init 0 0,view=view,update=update}

init : Int -> Int-> Model
init a b = {myCounter = MyCounterAPI.init a,yourCounter = MyCounterAPI.init a,map=Dict.empty}

type alias Model = {myCounter: MyCounterAPI.Model,yourCounter: MyCounterAPI.Model,map:Dict String Int}

view : Model -> Html Msg
view model = div[] [
  div[][App.map Counter1 (MyCounterAPI.view model.myCounter)]  
  ,div[][App.map Counter2 (MyCounterAPI.view model.yourCounter)]
  ,div [] [text (toString model.map)] 
  ]

type Msg = Counter1 MyCounterAPI.Msg | Counter2 MyCounterAPI.Msg 

update : Msg -> Model -> Model
update action model = case action of
  Counter1 msg -> if(model.myCounter <= 5) then 
    {model | myCounter = MyCounterAPI.update msg model.myCounter
    ,map = eventCalculate "Counter1-" msg model.map } else model
  Counter2 msg -> if(model.yourCounter >= 0) then 
    {model | yourCounter = MyCounterAPI.update msg model.yourCounter
    ,map = eventCalculate "Counter2-" msg model.map } else model

processMaybe a = case a of 
  Just val -> Just (val + 1)
  Nothing -> Just 1

eventCalculate: String -> MyCounterAPI.Msg -> Dict String Int -> Dict String Int
eventCalculate id key myMap= 
  let      
    keyString = id ++ toString(key)
    newMap = case (Dict.get keyString myMap) of
      Nothing -> Dict.insert keyString 1 myMap
      Just val -> Dict.update keyString processMaybe myMap
  in  
   -- case (Dict.get keyString newMap) of
   --   Just val -> val
   --   Nothing -> 0
   newMap