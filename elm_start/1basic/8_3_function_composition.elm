import Html exposing (..)

main = text (toString((view 10 20)))

view x y = x |> add y >> time 10 >> div 2 

add x y = x + y
time x y = y * x
div x y = y / x