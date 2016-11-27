module Tetris.State exposing (..)

import Array
import Dict
import Tetris.Types exposing (..)
import Random
import Time exposing (..)
import Keyboard


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.gameOver then
        Sub.batch [ every second Tick, Keyboard.presses KeyMsg ]
    else
        Sub.none


init =
    let
        boardHeight =
            20

        boardWidth =
            10

        piecePosition =
            newPiece piece

        piece =
            J

        nextPiece =
            J

        model =
            { piece = piece
            , currentTick = 0
            , currentPosition = ( 5, 1 )
            , boardHeight = boardHeight
            , boardWidth = boardWidth
            , board = Dict.empty
            , piecePosition = piecePosition
            , nextPiece = nextPiece
            , facing = North
            , linesCounter = Array.repeat (boardHeight + 1) 0
            , gameOver = False
            , lines = 0
            }
    in
        model ! [ generatePiece ]


generatePiece : Cmd Msg
generatePiece =
    Random.int 1 7
        |> Random.map randomPiece
        |> Random.generate NextPiece


randomPiece : Int -> Piece
randomPiece int =
    case int of
        1 ->
            O

        2 ->
            J

        3 ->
            L

        4 ->
            S

        5 ->
            Z

        6 ->
            I

        7 ->
            T

        _ ->
            O


newPiece : Piece -> PiecePosition
newPiece piece =
    case piece of
        I ->
            { p1 = ( 5, -2 )
            , p2 = ( 5, -1 )
            , p3 = ( 5, 0 )
            , p4 = ( 5, 1 )
            }

        O ->
            { p1 = ( 5, 1 )
            , p2 = ( 5, 0 )
            , p3 = ( 6, 1 )
            , p4 = ( 6, 0 )
            }

        J ->
            { p1 = ( 5, 1 )
            , p2 = ( 6, 1 )
            , p3 = ( 6, 0 )
            , p4 = ( 6, -1 )
            }

        L ->
            { p1 = ( 5, -1 )
            , p2 = ( 5, 0 )
            , p3 = ( 5, 1 )
            , p4 = ( 6, 1 )
            }

        S ->
            { p1 = ( 4, 1 )
            , p2 = ( 5, 1 )
            , p3 = ( 5, 0 )
            , p4 = ( 6, 0 )
            }

        Z ->
            { p1 = ( 4, 0 )
            , p2 = ( 5, 0 )
            , p3 = ( 5, 1 )
            , p4 = ( 6, 1 )
            }

        T ->
            { p1 = ( 5, -1 )
            , p2 = ( 5, 0 )
            , p3 = ( 5, 1 )
            , p4 = ( 6, 0 )
            }
