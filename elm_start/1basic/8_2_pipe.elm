import Html exposing (..)

main = view

view = div [][
  div[] [text "(a + b)2 = a2 + 2ab + b2"] 
  ,div[][text "LHS = RHS"]
  ,div[][text (calcuate 2 2)]
  ]    

-- (a + b)2 = a2 + 2ab + b2

calcuate x y = (left x y) ++ "=" ++ (right x y)

-- reverse Pipe
left x y = toString <| sqrt <| add y <| x

--right x y = (sqrt x |> add (2 |> multiply  (x |> multiply y))) |> add (sqrt y) |> toString 
right x y =  toString (add (add (sqrt x) (multiply 2 (multiply x y))) (sqrt y))

multiply : Int -> Int -> Int 
multiply x y = x * y

add : Int -> Int -> Int
add x y = x + y

sqrt : Int -> Int 
sqrt x = x ^ 2