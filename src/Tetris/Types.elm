module Tetris.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Keyboard
import Time exposing (Time)


type Facing
    = North
    | South
    | East
    | West


type Piece
    = O
    | J
    | L
    | S
    | Z
    | I
    | T


type alias PiecePosition =
    { p1 : Position
    , p2 : Position
    , p3 : Position
    , p4 : Position
    }


type alias Position =
    ( Int, Int )


type alias Board =
    Dict Position PositionProperty


type alias PositionProperty =
    { occupied : Bool
    , piece : Piece
    }


type alias Model =
    { piece : Piece
    , currentTick : Float
    , currentPosition : Position
    , boardHeight : Int
    , boardWidth : Int
    , board : Board
    , piecePosition : PiecePosition
    , nextPiece : Piece
    , facing : Facing
    , linesCounter : Array Int
    , gameOver : Bool
    , lines : Int
    }


type Msg
    = NoOp
    | Tick Time
    | KeyMsg Keyboard.KeyCode
    | NextPiece Piece
