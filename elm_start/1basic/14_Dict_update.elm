import Html exposing (Html,text)
import Dict exposing (Dict)

main = text (toString  view)

view = 
  let
    appleDict = insertIntoDict "Apple" 1 Dict.empty
  in
    insertIntoDict "Apple" 2 appleDict

process : Int -> Maybe Int -> Maybe Int
process x y = case y of 
    Just val -> Just x
    Nothing -> Just 1

insertIntoDict: String -> Int -> Dict String Int ->  Dict String Int
insertIntoDict key input myMap= 
  let      
    newMap = case (Dict.get key myMap) of
      Nothing -> Dict.insert key input myMap
      Just val -> Dict.update key (process input) myMap
  in  
    newMap