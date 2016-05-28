import Html exposing (Html,text)
import Dict exposing (Dict)

main = text (toString view)

view = 
  let
    appleDict = insertIntoDict "Apple" 1 Dict.empty
    appleDict2 = insertIntoDict "Apple" 2 appleDict
  in    
    readFromDict "Apple" appleDict2

process : Int -> Maybe Int -> Maybe Int
process x y = case y of 
    Just val -> Just x
    Nothing -> Just 0

insertIntoDict: String -> Int -> Dict String Int ->  Dict String Int
insertIntoDict key input myMap= 
  let      
    newMap = case (Dict.get key myMap) of
      Nothing -> Dict.insert key input myMap
      Just val -> Dict.update key (process input) myMap
  in  
    newMap

readFromDict : String -> Dict String Int -> Int
readFromDict key map =   
  case (Dict.get key map) of
    Nothing -> 0
    Just v -> v