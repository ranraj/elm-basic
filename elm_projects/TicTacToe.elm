import Html exposing (..)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)

main = App.program{init=init,view = view ,update = update,subscriptions= subscriptions}

type Player = PlayerO | PlayerX | Empty
type Status = LastMove String | Default | NotValidMove String
type alias Cell = {
    position : (Int,Int),    
    player : Player    
}

type alias Model = {
  board : List Cell
  ,status : Status 
  ,nextPlayer : Player    
}
model = Model drawBoardList Default PlayerO

type Msg = Played (Int,Int) | FindResult

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =  case msg of
  Played point->  
    let 
      xVal = ((fst point) * 100)
      yVal = ((snd point)*100)    
    in        
  ({
    model | 
        board = updateBoard model.nextPlayer point model.board 
       ,status = LastMove (toString point)
       ,nextPlayer = if model.nextPlayer == PlayerO then PlayerX else PlayerO
    },Cmd.none)

  FindResult -> ({
    model |         
       status = case (findWinnerSequence PlayerO model.board winSequence) of
        True -> LastMove "Player O Wins the Game"
        False -> case (findWinnerSequence PlayerX model.board winSequence) of
          True -> LastMove "Player X Wins the Game"
          False -> LastMove "Tie"            
    },Cmd.none)

notification status = case status of
    Default -> "Welcome.."
    LastMove msg -> "LastMove - " ++ msg
    NotValidMove msg -> "Exception - " ++ msg

view model = div [] [
  div[] [Html.text (notification model.status)]
  ,Svg.svg [ version "1.1", x "0", y "0", viewBox "0 0" ,width "300",height "300"] (drawBoard model.board)  
  ,div [] [ button [onClick FindResult] [Html.text "Result"]]
  ]

drawBoardList = [
   Cell (0,0) Empty
  ,Cell (0,1) Empty
  ,Cell (0,2) Empty
  ,Cell (1,0) Empty
  ,Cell (1,1) Empty
  ,Cell (1,2) Empty
  ,Cell (2,0) Empty
  ,Cell (2,1) Empty
  ,Cell (2,2) Empty 
  ]

winSequence = [
 [(0,0),(0,1),(0,2)]
 ,[(0,0),(1,0),(2,0)]
 ,[(0,0),(1,1),(2,2)]
 ,[(0,1),(1,1),(2,1)]
 ,[(1,0),(1,1),(1,2)]
 ,[(2,0),(2,1),(2,2)]
 ,[(0,2),(1,2),(2,2)]
 ,[(0,2),(1,1),(2,0)]
 ]

whoWin player board l = containsAll (fetchCellsByPlayer player board) l 
fetchCellsByPlayer player board = List.filter (\x -> x.player == player) board |> List.map (\x -> x.position)

findWinnerSequence player board l = case l of
  [] -> False
  (x::xs) -> case (whoWin player board x) of
    True -> True
    False -> findWinnerSequence player board xs

containsAll mainl subl = 
  case subl of
    [] -> True
    (x::xs) -> case (List.member x mainl) of
       True -> containsAll mainl xs
       False -> False


drawBoard board = List.map  drawBoardMap board
drawBoardMap cell = 
    let
      xVal = ((fst cell.position) * 100)
      yVal = ((snd cell.position)*100)
      boardBgColor = "#a3c2c2"                   
      symbolColor = "#4d4d4d"
    in
    case cell.player of
      PlayerO ->        
        g [] [
        rect [onClick (Played (cell.position)), fill boardBgColor, stroke symbolColor, x (toString xVal), y (toString yVal), width "100", height "100"][]                
        ,circle [ fill symbolColor, cx (toString (xVal+50)), cy (toString (yVal+50)), r "50" ] []
        ,circle [ fill boardBgColor, cx (toString (xVal+50)), cy (toString (yVal+50)), r "30" ] []
        ]
      PlayerX -> 
         g [] [
        rect [onClick (Played (cell.position)), fill boardBgColor, stroke symbolColor, x (toString ((fst cell.position) * 100)), y (toString ((snd cell.position)*100)), width "100", height "100"][]        
        ,line [ x1 (toString (xVal + 10)), y1 (toString (yVal + 10)), x2 (toString (xVal + 90)), y2 (toString (yVal + 90)), stroke symbolColor,strokeWidth "20"] []
        ,line [ x1 (toString (xVal + 10)), y1 (toString (yVal + 90)), x2 (toString (xVal + 90)), y2 (toString (yVal + 10)), stroke symbolColor,strokeWidth "20"] []
        ]
      Empty ->
        rect [onClick (Played (cell.position)), fill symbolColor, stroke boardBgColor, x (toString ((fst cell.position) * 100)), y (toString ((snd cell.position)*100)), width "100", height "100"][]   

updateBoard player point board = 
  case (hasOccupied point board) of    
    Empty -> List.map (updateBoardMap player point) board
    _ -> board

updateBoardMap player updatePoint currentCell =     
  if updatePoint == currentCell.position then
    Cell updatePoint player
  else
    currentCell

hasOccupied point board = 
  let 
    cell = List.head (List.filter (\currentCell -> currentCell.position == point) board)
  in     
    case cell of
      Just val -> val.player
      Nothing -> Empty

init : (Model, Cmd Msg)
init = (model , Cmd.none)    

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none  

