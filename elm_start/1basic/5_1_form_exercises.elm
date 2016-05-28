import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import String
import Basics
main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , error : (String,String)
  }


model : Model
model =
  Model "" "" "" "" ("","")


-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String 
    | Submit   
    

update : Msg -> Model -> Model
update action model =
  case action of
    Name name ->
      { model | name = name }   

    Password password ->
      { model | password = password}      

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->  
      { model | age = age }

    Submit -> 
      {model | error = (viewValidation model)}
        
-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ,value model.name] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ placeholder "Age", onInput Age ] []                
    , div [ style [("color", fst model.error)] ] [ text (snd model.error) ]
    , button [onClick Submit] [text "click"]
    ]

viewValidation : Model -> (String,String)
viewValidation model =
  let
    (color, message) =      
      if String.length model.password < 8 then
        ("red", "Passwords should not be less then 8!")
        else if model.password /= model.passwordAgain then
          ("red", "Passwords do not match!")          
            else if (validateStringToInt model.age)/=True then
              ("red", "Not a valid number")          
                else
                  ("green", "OK")
  in
    (color,message)

validateStringToInt: String -> Bool
validateStringToInt x =   
  case (String.toInt x) of
    Ok value -> True
    Err msg -> False  