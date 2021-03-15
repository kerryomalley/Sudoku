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

type SudokuCell = Filled Int | Options (Set.Set Int) | Problem
type alias SudokuBoard = List (List (SudokuCell))
type alias Model = {
  currentGame: SudokuBoard
  , newGame: SudokuBoard
  }

main : Program () Model Model
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

type Msg = StartGame 

createModel : (List String) -> SudokuBoard
createModel game = 
  let
    cell c =
      case String.toInt (String.fromChar c) of 
        Just x -> Filled x 
        Nothing -> Options (Set.fromList (List.range 1 9))
  in 
    (List.map (\chars -> List.map cell (String.toList chars)) game)


init : Model 
init = 
  {newGame = createModel exampleGame, currentGame = createModel exampleGame}

update : Model -> Model -> Model 
update model model1 = 
  model 

createCell : SudokuCell -> Collage.Collage msg 
createCell cell = 
  case cell of 
    Filled x -> 
      Collage.Layout.impose (Collage.rendered (Collage.Text.fromString (String.fromInt x)))
      (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 
    Options _ -> (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 
    Problem -> (Collage.outlined (Collage.solid Collage.thin (Collage.uniform Color.black)) (Collage.square 50)) 

view : Model -> Html Model
view model = 
  let 
    m = model.currentGame
    gameCells = (List.map (\cell -> List.map createCell cell) m)
    board = Collage.Layout.vertical (List.map Collage.Layout.horizontal gameCells)
  in 
    Html.div [] [Collage.Render.svg board]

