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

type SudokuCell = Filled Int | Options (Set.Set Int) | Problem
type alias SudokuBoard =  Array.Array (Array.Array (SudokuCell))
type alias Model = {
  currentGame: SudokuBoard
  }

main : Program () Model Msg
main = 
  Browser.sandbox 
    { init = init
    , view = view
    , update = update
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

type Msg = Input String 

createModel : (List String) -> SudokuBoard
createModel game = 
  let
    cell c =
      case String.toInt (String.fromChar c) of 
        Just x -> Filled x 
        Nothing -> Options (Set.fromList (List.range 1 9))
  in 
    (Array.map (\chars -> Array.map cell (Array.fromList (String.toList chars))) (Array.fromList game))


init : Model 
init = 
  {currentGame = createModel exampleGame}

update : Msg -> Model -> Model 
update msg model = 
  case msg of 
    Input x -> 
      let 
        row = (String.toInt (String.slice 0 1 x))
        col = (String.toInt (String.slice 2 3 x))
        input = (String.toInt (String.slice 4 5 x))
        arr = model.currentGame
      in 
        case (row, col, input) of 
          (Just a, Just b, Just c) -> 
            case (Array.get a arr) of 
             Just z -> {currentGame = (Array.set a (Array.set b (Filled c) z) arr)}
             _ -> model 
          _ -> model 

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
    input = Html.textarea [Html.Events.onInput Input] [Html.text "enter 'row,column,input' here"]
  in 
    Html.div [] ([input] ++[Collage.Render.svg board])


