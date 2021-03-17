module Algorithm exposing (..)

import Browser
import Dict 
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Set 
import String 
import Maybe 
import Array
import Array exposing (fromList, foldr, map, slice, set, append, toList)
import Collage exposing (square, outlined, uniform, solid, thin, rendered, group)
import Collage.Layout exposing (impose, horizontal, vertical)
import Collage.Render exposing (svg)
import Collage.Text 
import Color
-- import Playground exposing (..)
import Sudoku exposing (..)

main : Program () Model Model
main = 
  Browser.sandblock 
    { init = init
    , view = view
    , update = update
    }

type Msg
  = Number Int

type alias Model =
  { row_nums : Array (Array Int)
  , col_nums : Array (Array Int)
  , block_nums : Array (Array Int)
  , total_left : Int
  }

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () =
  ( initModel, Cmd.none )

exampleGame : Array (Array Int)
exampleGame = fromList [fromList [9,0,0,0,2,0,0,0,0], fromList[7,0,1,0,0,4,0,0,8], fromList [0,3,2,0,7,0,0,4,0], fromList [0,0,0,6,0,7,8,0,0], fromList [0,8,0,0,0,0,0,7,0], fromList [0,0,6,5,0,1,0,0,0], fromList [0,4,0,0,6,0,5,8,0], fromList [5,0,0,4,0,0,6,0,9], fromList [0,0,0,0,1,0,0,0,7]]

--- initialize col struct
init_col : Array (Array Int) -> Array (Array Int)
init_col board = 
  Array.map (\ind -> (Array.map (\row ->  (slice ind (ind + 1) row)) board)) [0,1,2,3,4,5,6,7,8]

emptyArray : Array a
emptyArray = fromList []

--- initialize block struct
init_block : Array (Array Int) -> Array (Array Int)
init_block board = 
  let 
    zero = foldr append emptyArray (map (\row -> slice 0 3 row) (slice 0 3 board))
    one = foldr append emptyArray (map (\row -> slice 3 6 row) (slice 0 3 board))
    two = foldr append emptyArray (map (\row -> slice 6 9 row) (slice 0 3 board))
    three = foldr append emptyArray (map (\row -> slice 0 3 row) (slice 3 6 board))
    four = foldr append emptyArray (map (\row -> slice 3 6 row) (slice 3 6 board))
    five = foldr append emptyArray (map (\row -> slice 3 6 row) (slice 3 6 board))
    six = foldr append emptyArray (map (\row -> slice 0 3 row) (slice 6 9 board))
    seven = foldr append emptyArray (map (\row -> slice 3 6 row) (slice 6 9 board))
    eight = foldr append emptyArray (map (\row -> slice 6 9 row) (slice 6 9 board))
  in
    fromList [zero, one, two, three, four, five, six, seven, eight]


--- check if the num entered is legal
legal_entry : Int -> (Int, Int) -> Model -> Bool
legal_entry num (row_ind, col_ind) model =
  let
    block_ind = to_block row_ind col_ind
  in
  not (List.member num (toList (slice row_ind (row_ind + 1) model.row_nums)) ||
  List.member num (toList (slice col_ind (col_ind + 1) model.col_nums)) ||
  List.member num (toList (slice block_ind (block_ind + 1) model.block_nums)))

-- convert row_ind and col_ind to which block
to_block : Int -> Int -> Int
to_block row_ind col_ind = 
    let 
      first = [0,1,2]
      second = [3,4,5]
      third = [6,7,8]
    in
    if (List.member row_ind first) && (List.member col_ind first) then 0
    else if (List.member row_ind first) && (List.member col_ind second) then 1
    else if (List.member row_ind first) && (List.member col_ind third) then 2
    else if (List.member row_ind second) && (List.member col_ind first) then 3
    else if (List.member row_ind second) && (List.member col_ind second) then 4
    else if (List.member row_ind second) && (List.member col_ind third) then 5
    else if (List.member row_ind third) && (List.member col_ind first) then 6
    else if (List.member row_ind third) && (List.member col_ind second) then 7
    else if (List.member row_ind third) && (List.member col_ind third) then 8
    else Debug.log("thats an error")

-- convert row_ind and col_ind to which block
to_ind_block : Int -> Int -> Int
to_ind_block row_ind col_ind =
    if List.member row_ind [0,3,6] && List.member col_ind [0,3,6] then 0
    else if List.member row_ind [1,4,7] && List.member col_ind [0,3,6] then 3
    else if List.member row_ind [2,5,8] && List.member col_ind [0,3,6] then 6
    else if List.member row_ind [0,3,6] && List.member col_ind [1,4,7] then 1
    else if List.member row_ind [1,4,7] && List.member col_ind [1,4,7] then 4
    else if List.member row_ind [2,5,8] && List.member col_ind [1,4,7] then 7
    else if List.member row_ind [0,3,6] && List.member col_ind [2,5,8] then 2
    else if List.member row_ind [1,4,7] && List.member col_ind [2,5,8] then 5
    else if List.member row_ind [2,5,8] && List.member col_ind [2,5,8] then 8
    else Debug.log("thats an error")

-- updates model struct after entering
update_struct : Int -> (Int, Int, Int) -> Model -> Model
update_struct num (row_ind, col_ind) model =
  let
    block_ind = to_block row_ind col_ind
    ind_block = to_ind_block row_ind col_ind
  in
  Model {
    row_nums = set col_ind num (slice row_ind (row_ind + 1) model.row_nums),
    col_nums = set row_ind num (slice col_ind (col_ind + 1) model.col_nums),
    block_nums = set ind_block num (slice block_ind (block_ind + 1) model.block_nums),
    total_left = Model.total_left - 1
  }

initModel : Model
initModel =
  { row_nums = emptyArray, col_nums = emptyArray[], block_nums = emptyArray, total_left = 0}

game_over : model -> Bool
game_over model = 
  model.total_left <= 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Number num -> 
      if legal_entry num (row_ind, col_ind) model then
        (update_struct num (row_ind, col_ind) model, msg)
      else
        Debug.todo
