import Html exposing (..)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

main = App.program{init=init,view = view ,update = update,subscriptions= subscriptions}

type Player = PlayerO | PlayerX | Empty
type GameBoard = EmptyBoard (List Cell) | FinishedBoard (List Cell) | PlayBoard (List Cell) | WinBoard (List Cell) String | TieBoard (List Cell)
type Status = LastMove String | Default | NotValidMove String | GameResult String
type Response = Board (List Cell)| Error String
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

type Msg = Played (Int,Int)

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =  case msg of
  Played point->  
    let       
      xVal = ((fst point) * 100)
      yVal = ((snd point)*100)
      newModel = case (updateBoard model.nextPlayer point model.board) of
        Board b ->
            case (gameResult b) of
               _ -> {
                model | board = b
                ,status = renderStatus b point
                ,nextPlayer = if model.nextPlayer == PlayerO then PlayerX else PlayerO
            }  
        Error msg -> {
          model  | status = NotValidMove msg}
    in        
      (newModel,Cmd.none)
renderStatus : List Cell -> (Int,Int) -> Status
renderStatus board point =   
  case (gameResult board) of
    TieBoard b -> GameResult "Tie"
    WinBoard b msg -> GameResult msg
    PlayBoard b -> LastMove (toString point)
    _ -> GameResult "UnKnown"

gameResult : List Cell -> GameBoard
gameResult board = case (findWinnerSequence PlayerO board winSequence) of
    True -> WinBoard board "Player O Wins the Game"
    False -> case (findWinnerSequence PlayerX board winSequence) of
      True -> WinBoard board "Player X Wins the Game"
      False -> case (isFinishedBoard board) of
        True -> TieBoard board
        False -> PlayBoard board

notification : Status -> (List (String,String),String)
notification status = case status of
    Default -> ([("color","orange")],"Welcome..")
    LastMove msg -> ([("color","green")],"LastMove - " ++ msg)
    NotValidMove msg -> ([("color","red")],"Exception - " ++ msg)
    GameResult msg -> ([("color","green")],"Result - " ++ msg)

view model = let
  notificationResponse = notification model.status  
  in
    div [] [
     div [ Html.Attributes.style (fst notificationResponse) ] [Html.text (snd notificationResponse)]
     ,Svg.svg [ version "1.1", x "0", y "0", viewBox "0 0" ,width "300",height "300"] (drawBoard model.board)  
    --,div [] [ button [onClick FindResult] [Html.text "Result"]]
     ]
--whoWin : Player -> List a -> List a -> Bool
whoWin player board l = containsAll (fetchCellsByPlayer player board) l 

fetchCellsByPlayer : Player -> List Cell -> List (Int,Int)
fetchCellsByPlayer player board = List.filter (\x -> x.player == player) board |> List.map (\x -> x.position)

isFinishedBoard : List Cell -> Bool
isFinishedBoard board = List.isEmpty (List.filter (\x -> x.player == Empty) board)

findWinnerSequence : Player -> List Cell -> List (List (Int,Int)) -> Bool
findWinnerSequence player board l = case l of
  [] -> False
  (x::xs) -> case (whoWin player board x) of
    True -> True
    False -> findWinnerSequence player board xs

containsAll : List a -> List a -> Bool
containsAll mainl subl = 
  case subl of
    [] -> True
    (x::xs) -> case (List.member x mainl) of
       True -> containsAll mainl xs
       False -> False

drawBoard : List Cell -> List (Svg Msg)
drawBoard board = List.map  drawBoardMap board

drawBoardMap cell = 
    let
      xVal = ((fst cell.position) * 100)
      yVal = ((snd cell.position) * 100)
      boardBgColor = "#a3c2c2"                   
      symbolColor = "#4d4d4d"      
    in
    case cell.player of
      PlayerO ->        
        g [] [
        rect [onClick (Played (cell.position)), fill boardBgColor, stroke symbolColor, x (toString xVal), y (toString yVal), width "100", height "100"][]                
        ,circle [onClick (Played (cell.position)), fill symbolColor, cx (toString (xVal+50)), cy (toString (yVal+50)), r "50" ] []
        ,circle [onClick (Played (cell.position)), fill boardBgColor, cx (toString (xVal+50)), cy (toString (yVal+50)), r "30" ] []
        ]
      PlayerX -> 
         g [] [
        rect [onClick (Played (cell.position)), fill boardBgColor, stroke symbolColor, x (toString ((fst cell.position) * 100)), y (toString ((snd cell.position)*100)), width "100", height "100"][]        
        ,line [onClick (Played (cell.position)), x1 (toString (xVal + 10)), y1 (toString (yVal + 10)), x2 (toString (xVal + 90)), y2 (toString (yVal + 90)), stroke symbolColor,strokeWidth "20"] []
        ,line [onClick (Played (cell.position)), x1 (toString (xVal + 10)), y1 (toString (yVal + 90)), x2 (toString (xVal + 90)), y2 (toString (yVal + 10)), stroke symbolColor,strokeWidth "20"] []
        ]
      Empty ->
        rect [onClick (Played (cell.position)), fill symbolColor, stroke boardBgColor, x (toString ((fst cell.position) * 100)), y (toString ((snd cell.position)*100)), width "100", height "100"][]   

updateBoard : Player -> (Int,Int) -> List Cell -> Response
updateBoard player point board =  
  case (hasOccupied point board) of    
    Empty ->         
        Board (List.map (updateBoardMap player point) board)    
    _ -> Error "Invalid Move"

updateBoardMap : Player -> (Int,Int) -> Cell -> Cell
updateBoardMap player updatePoint currentCell =     
  case (updatePoint == currentCell.position) of
    True -> Cell updatePoint player
    False -> currentCell

hasOccupied : (Int,Int) -> List Cell -> Player
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