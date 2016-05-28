import Basics
import Html exposing (..)

length : List Int -> Int
length l = foldl (\t y -> y + 1) 0 l 

foldl : (Int -> Int -> Int) -> Int -> List Int -> Int
foldl f acc l =
  case l of
    [] -> acc
    (x::xs) -> f x (foldl f acc xs)

map : (Int -> Int ) -> List Int -> List Int
map f l = 
  case l of
    [] -> []
    (x::xs) -> f x :: (map f xs)

filter : (Int -> Bool) -> List Int -> List Int
filter f l =
  case l of
    [] -> []
    (x::xs) -> case (f x == True) of
                True -> x :: filter f xs
                False -> filter f xs

--main = length [1,2,3] |> toString |> text
--main = foldl (+) 0 [1,2,3] |> toString |> text
--main = map (\x -> x * x) [1,2,3] |> toString |> text

main = filter (\x -> x < 5) [1,2,3,7,8] |> toString |> text