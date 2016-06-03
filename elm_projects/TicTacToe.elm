import Html exposing (..)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

main = App.program{init=init,view = view ,update = update,subscriptions= subscriptions}

type Player = PlayerO | PlayerX | Empty
type Board = EmptyBoard Cells | PlayBoard Cells | WinBoard Cells String (List Position )| TieBoard Cells String
type Status = LastMove String | Default | NotValidMove String | GameResult String | Error String
type BoardUpdateResponse = Success (List Cell)| Failure String

type alias Cells = List Cell
type alias Cell = {
    position : Position,    
    player : Player    
}
type alias Position = (Int,Int)

type alias Model = {
  board : Cells
  ,status : Status 
  ,nextPlayer : Player    
}
model = Model defaultCells Default PlayerO

type Msg = Played Position

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =  case msg of
  Played position->  
    case (fetchGameResult model.board) of 
        PlayBoard b->         
            let       
              xVal = ((fst position) * 100)
              yVal = ((snd position) * 100)      
              updatedModel = case (plotPlayerCellOnBoard model.nextPlayer position model.board) of
                Success b ->
                    case (fetchGameResult b) of
                       _ -> {
                        model | board = b
                        ,status = buildStatusFromResult b position
                        ,nextPlayer = if model.nextPlayer == PlayerO then PlayerX else PlayerO
                    }  
                Failure msg -> {
                  model  | status = NotValidMove msg}
            in        
              (updatedModel,Cmd.none)
        TieBoard cells msg ->  ({ model  | status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)         
        WinBoard cells msg sequence->  ({ model  | status = NotValidMove ("Game Over as match result :" ++ msg)},Cmd.none)           
        EmptyBoard cesll -> ({ model  | status = NotValidMove "Play has not completed "},Cmd.none)           

buildStatusFromResult : List Cell -> Position -> Status
buildStatusFromResult cells position =   
  case (fetchGameResult cells) of
    TieBoard b msg -> GameResult msg
    WinBoard b msg sequence-> GameResult msg
    PlayBoard b -> LastMove (toString position)
    _ -> Error "UnKnown"

fetchGameResult : List Cell -> Board
fetchGameResult cells = case (whoWin PlayerO cells winCellSequence) of
    Just sequence -> WinBoard cells ("Player O Wins the Game") sequence
    Nothing -> case (whoWin PlayerX cells winCellSequence) of
      Just sequence -> WinBoard cells ("Player X Wins the Game") sequence
      Nothing -> case (isFinishedBoard cells) of
        True -> TieBoard cells "Game Tie"
        False -> PlayBoard cells

statusNotification : Status -> (List (String,String),String)
statusNotification status = case status of
    Default -> ([("color","orange")],"Welcome..")
    LastMove msg -> ([("color","green")],"LastMove - " ++ msg)
    NotValidMove msg -> ([("color","red")],"Exception - " ++ msg)
    GameResult msg -> ([("color","green")],"Result - " ++ msg)
    Error msg -> ([("color","red")],"Error - " ++ msg)

view model = let
  notificationResponse = statusNotification model.status  
  in
    div [] [
     div [ Html.Attributes.style (fst notificationResponse) ] [Html.text (snd notificationResponse)]
     ,Svg.svg [ version "1.1", x "0", y "0", viewBox "0 0" ,width "600",height "600"] (drawTilesFromCells model.board)  
    --,div [] [ button [onClick FindResult] [Html.text "Result"]]
     ]

filterCellsByPlayer : Player -> List Cell -> List (Int,Int)
filterCellsByPlayer player cells = List.filter (\x -> x.player == player) cells |> List.map (\x -> x.position)

isFinishedBoard : List Cell -> Bool
isFinishedBoard cells = List.isEmpty (List.filter (\x -> x.player == Empty) cells)

whoWin : Player -> List Cell -> List (List Position) -> Maybe (List ( Int, Int ))
whoWin player cells l = case l of
  [] -> Nothing
  (x::xs) -> case (isSubsetOf (filterCellsByPlayer player cells) x) of
    True -> Just x
    False -> whoWin player cells xs

drawTilesFromCells : List Cell -> List (Svg Msg)
drawTilesFromCells cells = List.map cellToTileBuilder cells

cellToTileBuilder cell = 
    let
      size = 100
      strokeWidthSize = 20
      times = 1
      xVal = ((fst cell.position) * size)
      yVal = ((snd cell.position) * size)
      boardBgColor = "#a3c2c2"                   
      symbolColor = "#4d4d4d"      
    in
    case cell.player of
      PlayerO ->        
        g [] [
        rect [onClick (Played (cell.position)), fill boardBgColor, stroke symbolColor, x (toString xVal), y (toString yVal), width (toString size), height (toString size)][]                
        ,circle [onClick (Played (cell.position)), fill symbolColor, cx (toString ((xVal + 50) * times)), cy (toString ((yVal + 50) * times)), r "50" ] []
        ,circle [onClick (Played (cell.position)), fill boardBgColor, cx (toString ((xVal + 50) * times)), cy (toString ((yVal + 50) * times)), r "30" ] []
        ]
      PlayerX -> 
         g [] [
        rect [onClick (Played (cell.position)), fill boardBgColor, stroke symbolColor, x (toString ((fst cell.position) * size)), y (toString ((snd cell.position) * size)), width (toString size), height (toString size)][]        
        ,line [onClick (Played (cell.position)), x1 (toString (xVal + 10)), y1 (toString (yVal + 10)), x2 (toString (xVal + 90)), y2 (toString (yVal + 90)), stroke symbolColor,strokeWidth (toString strokeWidthSize)] []
        ,line [onClick (Played (cell.position)), x1 (toString (xVal + 10)), y1 (toString (yVal + 90)), x2 (toString (xVal + 90)), y2 (toString (yVal + 10)), stroke symbolColor,strokeWidth (toString strokeWidthSize)] []
        ]
      Empty ->
        rect [onClick (Played (cell.position)), fill symbolColor, stroke boardBgColor, x (toString ((fst cell.position) * size)), y (toString ((snd cell.position) * size)), width (toString size), height (toString size)][]   

plotPlayerCellOnBoard : Player -> Position -> List Cell -> BoardUpdateResponse
plotPlayerCellOnBoard player position cells =  
  case (hasOccupied position cells) of    
    Empty ->         
        Success (List.map 
                 (\ cell -> case (position == cell.position) of 
                    True -> Cell position player
                    False -> cell) 
                 cells)    
    _ -> Failure "Invalid Move"


hasOccupied : Position -> List Cell -> Player
hasOccupied position cells = 
  let 
    cell = List.head (List.filter (\currentCell -> currentCell.position == position) cells)
  in     
    case cell of
      Just val -> val.player
      Nothing -> Empty

init : (Model, Cmd Msg)
init = (model , Cmd.none)    

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none  

-- Util --

isSubsetOf : List a -> List a -> Bool
isSubsetOf mainl subl = 
  case subl of
    [] -> True
    (x::xs) -> case (List.member x mainl) of
       True -> isSubsetOf mainl xs
       False -> False

-- Static Data --

defaultCells = [
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

winCellSequence = [
 [(0,0),(0,1),(0,2)]
 ,[(0,0),(1,0),(2,0)]
 ,[(0,0),(1,1),(2,2)]
 ,[(0,1),(1,1),(2,1)]
 ,[(1,0),(1,1),(1,2)]
 ,[(2,0),(2,1),(2,2)]
 ,[(0,2),(1,2),(2,2)]
 ,[(0,2),(1,1),(2,0)]
 ]