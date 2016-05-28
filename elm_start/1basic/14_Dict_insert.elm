import Html exposing (Html,text)
import Dict exposing (Dict)

main = text (toString (insertIntoDict "Apple" 1 Dict.empty))

insertIntoDict: String -> Int -> Dict String Int -> Dict String Int
insertIntoDict key input myMap = 
  let      
    newMap =  Dict.insert key input myMap      
  in  
    newMap