import Html

names = [ "Pearl", "Steven", "Garnet", "Amethyst" ]

main =
  let
    sorted = List.sort names
    texts = List.map Html.text sorted
    textToItem text = Html.li [] [ text ]
    items = List.map textToItem texts
  in
    Html.ul [] items