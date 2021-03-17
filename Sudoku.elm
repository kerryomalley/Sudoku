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
type SudokuCell = Filled Int | Options (Set.Set Int) | Problem
type alias SudokuBoard =  Array.Array (Array.Array (SudokuCell))
type alias Model = {
  currentGame: SudokuBoard
  ,currentSquare: (Int, Int)
  }

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

exampleGame = [ "9...2....",
         "7.1..4..8",
         ".32.7..4.",
         "...6.78..",
         ".8.....7.",
         "..65.1...",
         ".4..6.58.",
         "5..4..6.9",
         "....1...7"
      ]

type Msg = UpKey | DownKey | LeftKey | RightKey | Reset | Noop | Num String

createModel : (List String) -> SudokuBoard
createModel game = 
  let
    cell c =
      case String.toInt (String.fromChar c) of 
        Just x -> Filled x 
        Nothing -> Options (Set.fromList (List.range 1 9))
  in 
    (Array.map (\chars -> Array.map cell (Array.fromList (String.toList chars))) (Array.fromList game))

init : Flags -> (Model, Cmd Msg)
init () = 
  (initModel, Cmd.none)

initModel : Model 
initModel = 
  {currentGame = createModel exampleGame, currentSquare = (0,0)}

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
          ({currentGame = arr, currentSquare = (num1 - 1, num2)}, Cmd.none)
      DownKey -> 
        if num1 == 8 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1+1, num2)}, Cmd.none)
      LeftKey -> 
        if num2 == 0 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1, num2-1)}, Cmd.none)
      RightKey -> 
        if num2 == 8 then 
          (model, Cmd.none)
        else
          ({currentGame = arr, currentSquare = (num1, num2+1)}, Cmd.none)
      Num val ->
        case ((Array.get num1 arr), (String.toInt val)) of 
          (Just z, Just v) -> ({currentGame = (Array.set num1 (Array.set num2 (Filled v) z) arr), currentSquare = square}, Cmd.none)
          _ -> (model, Cmd.none)
      Reset -> (initModel, Cmd.none)
      _ -> (model, Cmd.none)


createCell : SudokuCell -> Collage.Collage msg 
createCell cell = 
  case cell of 
    Filled x -> 
      Collage.Layout.impose (Collage.rendered (Collage.Text.fromString (String.fromInt x)))
      (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 
    Options _ -> (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 
    Problem -> (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 

view : Model -> Html Msg
view model = 
  let 
    m = model.currentGame
    gameCells = (List.map (\cell -> List.map createCell (Array.toList cell)) (Array.toList m))
    board = Collage.Layout.vertical (List.map Collage.Layout.horizontal gameCells)
    square = Html.text ((String.fromInt (Tuple.first model.currentSquare)) ++ ", " ++ (String.fromInt (Tuple.second model.currentSquare)))
    reset = Html.button [onClick Reset] [Html.text "Reset"]
  in 
    Html.div [] ([Collage.Render.svg board] ++ [square] ++ [reset])

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
  ]
