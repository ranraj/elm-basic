import Html exposing (..)

main = text (view 10 20)

view x y = toString (add x y)

add x y = x + y