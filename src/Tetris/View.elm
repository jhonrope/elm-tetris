module Tetris.View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Tetris.Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h2
                []
                [ text <| "Next piece: " ++ toString model.nextPiece
                ]
            ]
        , div [ style <| border model ] []
        , div []
            [ h2 [] [ text <| "Lines: " ++ toString model.lines ]
            ]
        , showControls
        , tablero model
        , showGameOver model.gameOver
        ]


showControls : Html Msg
showControls =
    div []
        [ h2 [] [ text "Controls:" ]
        , div [] [ text "Move left -> A" ]
        , div [] [ text "Move right -> D" ]
        , div [] [ text "Move down -> S" ]
        , div [] [ text "Rotate -> W" ]
        ]


showGameOver : Bool -> Html Msg
showGameOver isGameOver =
    if isGameOver then
        h1 [] [ text "Game Over!" ]
    else
        div [] []


tablero : Model -> Html Msg
tablero model =
    let
        piece =
            model.piecePosition
                |> piecePositionToList
                |> List.map (\pos -> ( pos, PositionProperty True model.piece ))

        tablero =
            model.board
                |> Dict.toList
                |> List.append piece
                |> List.map drawSquare
    in
        div [] tablero


border : Model -> List ( String, String )
border model =
    let
        blockSize =
            18

        left =
            195

        top =
            100

        width =
            blockSize * model.boardWidth + 5

        height =
            blockSize * (model.boardHeight + 1)
    in
        [ ( "position", "absolute" )
        , ( "left", toString left ++ "px" )
        , ( "top", toString top ++ "px" )
        , ( "height", toString height ++ "px" )
        , ( "width", toString width ++ "px" )
        , ( "border", "1px solid black" )
        ]


squareStyle : Position -> PositionProperty -> List ( String, String )
squareStyle pos posPro =
    let
        ( x, y ) =
            ( Basics.max 1 <| fst pos, Basics.max 1 <| snd pos )

        blockSize =
            18

        left =
            180 + (x * blockSize)

        top =
            100 + (y * blockSize)
    in
        [ ( "position", "absolute" )
        , ( "left", toString left ++ "px" )
        , ( "top", toString top ++ "px" )
        , ( "height", toString blockSize ++ "px" )
        , ( "width", toString blockSize ++ "px" )
        , ( "background-color", pieceColor posPro.piece )
        , ( "border", "1px solid black" )
        ]


pieceColor : Piece -> String
pieceColor piece =
    case piece of
        L ->
            "red"

        O ->
            "blue"

        T ->
            "green"

        J ->
            "orange"

        I ->
            "purple"

        S ->
            "yellow"

        Z ->
            "cyan"


drawSquare : ( Position, PositionProperty ) -> Html Msg
drawSquare ( pos, bool ) =
    div [ style <| squareStyle pos bool ] []


piecePositionToList : PiecePosition -> List Position
piecePositionToList piecePos =
    [ piecePos.p1
    , piecePos.p2
    , piecePos.p3
    , piecePos.p4
    ]
