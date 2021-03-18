module Sudoku exposing (..)

import Browser
import Dict 
import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Set 
import String 
import Maybe 
import Array 
import Collage exposing (square, outlined, uniform, solid, thin, rendered, group)
import Collage.Layout exposing (impose, horizontal, vertical)
import Collage.Render exposing (svg)
import Collage.Text 
import Color
import Array 
import Json.Decode as Decode
import Browser.Events

type alias Flags = ()
type SudokuCell = Filled Int | Empty | Problem
type alias SudokuBoard =  Array.Array (Array.Array (SudokuCell))
type alias Model = {
  currentGame: SudokuBoard
  ,currentSquare: (Int, Int)
  , message: String 
  }

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

easyGame = [".6.3..8.4",
            "537.9....",
            ".4...63.7",
            ".9..51238",
            ".........",
            "71362..4.",
            "3.64...1.",
            "....6.523",
            "1.2..9.8."]
hardGame = ["...26.7.1",
            "68..7..9.",
            "19...45..",
            "82.1...4.",
            "..46.29..",
            ".5...3.28",
            "..93...74",
            ".4..5..36",
            "7.3.18..."]
mediumGame = ["..7...1..",
              "8...6...5",
              "..91.38..",
              ".3.2.4.8.",
              ".........",
              ".6.7.5.4.",
              "..18.75..",
              "9...3...7",
              "..2...3.."]
gameOverBoard = [".35269781",
            "682571493",
            "197834562",
            "826195347",
            "374682915",
            "951743628",
            "519326874",
            "248957136",
            "763418259"]

type Msg = UpKey | DownKey | LeftKey | RightKey | Hard | Noop | Num String | Delete | Easy | Medium | GameOver

createModel : (List String) -> SudokuBoard
createModel game = 
  let
    cell c =
      case String.toInt (String.fromChar c) of 
        Just x -> Filled x 
        Nothing -> Empty
  in 
    (Array.map (\chars -> Array.map cell (Array.fromList (String.toList chars))) (Array.fromList game))

init : Flags -> (Model, Cmd Msg)
init () = 
  (initModel, Cmd.none)

initHard : Model 
initHard = 
  {currentGame = createModel hardGame, currentSquare = (0,0), message = ""}

initModel : Model 
initModel = 
  {currentGame = createModel easyGame, currentSquare = (0,0), message = ""}

initMedium : Model
initMedium =
  {currentGame = createModel mediumGame, currentSquare = (0,0), message = ""}

gameOverTest : Model 
gameOverTest = 
  {currentGame = createModel gameOverBoard, currentSquare = (0,0), message = ""}

checkRow : Int -> Model -> Bool 
checkRow val model = 
  let 
    arr = model.currentGame
    cell = model.currentSquare
    row = Tuple.first cell 
    col = Tuple.second cell 
  in 
    case (Array.get row arr) of 
      (Just list) -> 
        if (List.member (Filled val) (Array.toList list)) then 
          False 
        else 
          True 
      _ -> False 
  
findCurrentBlock : Model -> (Int, Int)
findCurrentBlock model = 
  let
    cell = model.currentSquare
    row = Tuple.first cell 
    col = Tuple.second cell 
  in 
    if ((row < 3) && (col < 3)) then (0,0)
    else if ((row < 3) && (col < 6)) then (0,3)
    else if (row < 3) then (0,6)
    else if ((row < 6) && (col < 3)) then (3,0)
    else if ((row < 6) && (col < 6)) then (3,3)
    else if (row < 6) then (3, 6)
    else if (col < 3) then (6, 0)
    else if (col < 6) then (6, 3)
    else (6,6)

checkBlock : Int -> Model -> Bool
checkBlock val model = 
  let 
    firstCell = findCurrentBlock model 
    row = Tuple.first firstCell
    col = Tuple.second firstCell
    check list = 
      (List.member (Filled val) (Array.toList (Array.slice col (col+3) list))) 
  in 
    not (List.member True (List.map check (Array.toList (Array.slice row (row+3) model.currentGame)))) 

checkCol : Int -> Model -> Bool 
checkCol val model = 
  let 
    arr = model.currentGame
    cell = model.currentSquare
    row = Tuple.first cell 
    col = Tuple.second cell 
    helper list =
      case (Array.get col list) of 
        (Just x) ->
          if x == (Filled val) then 
            0 
          else 
            1 
        _ -> 1
  in 
    not (List.member 0 (List.map helper (Array.toList arr)))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  let 
    arr = model.currentGame
    square = model.currentSquare
    num1 = Tuple.first square
    num2 = Tuple.second square
  in 
    case msg of 
      UpKey -> 
        if num1 == 0 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1 - 1, num2), message = ""}, Cmd.none)
      DownKey -> 
        if num1 == 8 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1+1, num2), message = ""}, Cmd.none)
      LeftKey -> 
        if num2 == 0 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1, num2-1), message = ""}, Cmd.none)
      RightKey -> 
        if num2 == 8 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1, num2+1), message = ""}, Cmd.none)
      Num val ->
        case ((Array.get num1 arr), (String.toInt val)) of 
          (Just z, Just v) -> 
            if ((checkRow v model) && (checkCol v model) && (checkBlock v model)) then 
              (gameOver {currentGame = (Array.set num1 (Array.set num2 (Filled v) z) arr), currentSquare = square, message = ""}, Cmd.none)
            else 
              ({currentGame = arr, currentSquare = square, message = "This input violates the rules!"}, Cmd.none)
          _ -> (model, Cmd.none)
      Hard -> (initHard, Cmd.none)
      Easy -> (initModel, Cmd.none)
      Medium -> (initMedium, Cmd.none)
      GameOver -> (gameOverTest, Cmd.none)
      Delete -> 
        case (Array.get num1 arr) of 
          (Just z) -> ({currentGame = (Array.set num1 (Array.set num2 Empty z) arr), currentSquare = square, message = ""}, Cmd.none)
          _ -> (model, Cmd.none)
      _ -> (model, Cmd.none)

gameOver : Model -> Model  
gameOver model =
  let 
    arr = model.currentGame 
    check list = 
      (List.member Empty (Array.toList list)) 
    isGameOver = not (List.member True (List.map check (Array.toList arr)))
  in 
    if isGameOver then 
      {currentGame = arr, currentSquare = model.currentSquare, message = "You completed the game!"}
    else 
      model 

createCell : SudokuCell -> Collage.Collage msg 
createCell cell = 
  case cell of 
    Filled x -> 
      Collage.Layout.impose (Collage.rendered (Collage.Text.fromString (String.fromInt x)))
      (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 
    Empty -> (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 
    Problem -> (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 

view : Model -> Html Msg
view model = 
  let 
    m = model.currentGame
    gameCells = (List.map (\cell -> List.map createCell (Array.toList cell)) (Array.toList m))
    board = Collage.Layout.vertical (List.map Collage.Layout.horizontal gameCells)
    square = Html.text (" Current square: (" ++ (String.fromInt (Tuple.first model.currentSquare)) ++ ", " ++ (String.fromInt (Tuple.second model.currentSquare)) ++ ")")
    hard = Html.button [onClick Hard] [Html.text "Hard"]
    header = Html.h1 [] [Html.text "Sudoku"]
    rules1 = Html.h4 [] [Html.text ("To play, navigate the board using the arrow keys, and add numbers 1 through 9 into the open squares.")]
    rules2 = Html.h4 [] [Html.text ("You can only use each number once in each row and column, and within each of the 9 blocks")]
    mess = Html.h4 [] [Html.text model.message]
    easy = Html.button [onClick Easy] [Html.text "Easy"]
    medium = Html.button [onClick Medium] [Html.text "Medium"]
    goTest = Html.button [onClick GameOver] [Html.text "Almost Complete Test"]
  in 
    Html.div [] ([header] ++ [rules1] ++ [rules2] ++ [Collage.Render.svg board] ++ [easy] ++ [medium] ++ [hard] ++ [goTest] ++ [square] ++ [mess])

keyDecoder : Decode.Decoder String 
keyDecoder = 
  Decode.field "key" Decode.string 

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch 
  [ Browser.Events.onKeyDown
      (Decode.map (\key -> if key == "ArrowUp" then UpKey else Noop) keyDecoder)
  , Browser.Events.onKeyDown
      (Decode.map (\key -> if key == "ArrowDown" then DownKey else Noop) keyDecoder) 
  , Browser.Events.onKeyDown
      (Decode.map (\key -> if key == "ArrowLeft" then LeftKey else Noop) keyDecoder)
  , Browser.Events.onKeyDown
      (Decode.map (\key -> if key == "ArrowRight" then RightKey else Noop) keyDecoder)
  , Browser.Events.onKeyDown 
      (Decode.map (\key -> if (List.member key ["1", "2", "3", "4", "5", "6", "7", "8", "9"]) then Num key else Noop) keyDecoder)
  , Browser.Events.onKeyDown 
      (Decode.map (\key -> if key == "Backspace" then Delete else Noop) keyDecoder)
  ]
