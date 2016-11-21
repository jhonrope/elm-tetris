module App exposing (..)

import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Dict exposing (..)
import Debug exposing (..)
import Keyboard
import Char


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
    Dict Position Bool


type alias Model =
    { currentPiece : Piece
    , currentTick : Float
    , currentPosition : Position
    , boardHeight : Int
    , boardWidth : Int
    , board : Board
    , piecePosition : PiecePosition
    }


type Msg
    = NoOp
    | Tick Time
    | KeyMsg Keyboard.KeyCode


updatePosition : Int -> Int -> Position -> Position
updatePosition x y pos =
    ( fst pos + x, snd pos + y )


movePieceLeft : Model -> ( Position, Board )
movePieceLeft model =
    let
        ( x, y ) =
            (updatePosition -1 0 model.currentPosition)

        ( newCurrentPos, newBoard ) =
            if
                collisionByBorderLeft model.currentPosition
                    || collisionByOccupied ( x, y ) model.board
            then
                ( model.currentPosition, model.board )
            else
                ( ( x, y )
                , model.board
                    |> Dict.insert ( x, y ) True
                    |> Dict.remove model.currentPosition
                )
    in
        ( newCurrentPos, newBoard )


movePieceRight : Model -> ( Position, Board )
movePieceRight model =
    let
        ( x, y ) =
            (updatePosition 1 0 model.currentPosition)

        ( newCurrentPos, newBoard ) =
            if
                collisionByBorderRight model.boardWidth model.currentPosition
                    || collisionByOccupied ( x, y ) model.board
            then
                ( model.currentPosition, model.board )
            else
                ( ( x, y )
                , model.board
                    |> Dict.insert ( x, y ) True
                    |> Dict.remove model.currentPosition
                )
    in
        ( newCurrentPos, newBoard )


listToPiecePosition : List Position -> PiecePosition
listToPiecePosition list =
    case list of
        p1 :: p2 :: p3 :: p4 :: [] ->
            PiecePosition p1 p2 p3 p4

        _ ->
            newPiece


piecePositionToList : PiecePosition -> List Position
piecePositionToList piecePos =
    [ piecePos.p1
    , piecePos.p2
    , piecePos.p3
    , piecePos.p4
    ]


movePieceDown : Model -> ( Position, Board )
movePieceDown model =
    let
        nextPos =
            (updatePosition 0 1 model.currentPosition)

        {--
        list =
            piecePositionToList model.piecePosition

        newPiecePosition : Dict Position Bool
        newPiecePosition =
            list
                |> List.map (updatePosition 0 1)
                |> List.map (\pos -> ( pos, True ))
                |> Dict.fromList

        collideWithBorder =
            list |> List.any (collisionByBorder model.boardHeight)

        collideWithOther =
            list |> List.any (\pos -> collisionByOccupied pos model.board)

        ( b, o ) =
            log "col" ( collideWithBorder, collideWithOther )
-}
    in
        ( newCurrentPosition model nextPos, newBoard model nextPos )


newCurrentPosition : Model -> Position -> Position
newCurrentPosition model nextPosition =
    if
        collisionByBorder model.boardHeight model.currentPosition
            || collisionByOccupied nextPosition model.board
    then
        ( 5, 1 )
    else
        nextPosition


newBoard : Model -> Position -> Board
newBoard model nextPos =
    if
        collisionByBorder model.boardHeight model.currentPosition
            || collisionByOccupied nextPos model.board
    then
        model.board
            |> Dict.insert ( 5, 1 ) True
    else
        model.board
            |> Dict.insert nextPos True
            |> Dict.remove model.currentPosition


calculate : (Model -> ( Position, Board )) -> Model -> ( Model, Cmd Msg )
calculate fn model =
    let
        ( newCurrentPos, newBoard ) =
            fn model
    in
        { model
            | currentPosition = newCurrentPos
            , board = newBoard
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                _ =
                    log "tick" model.currentPosition

                ( model, cmd ) =
                    calculate movePieceDown model
            in
                { model | currentTick = model.currentTick + 1 } ! [ cmd ]

        KeyMsg code ->
            case Char.fromCode code of
                's' ->
                    calculate movePieceDown model

                'a' ->
                    calculate movePieceLeft model

                'd' ->
                    calculate movePieceRight model

                _ ->
                    model ! []


collisionByBorderLeft : Position -> Bool
collisionByBorderLeft pos =
    fst pos <= 1


collisionByBorderRight : Int -> Position -> Bool
collisionByBorderRight rightBorder pos =
    fst pos >= rightBorder


collisionByBorder : Int -> Position -> Bool
collisionByBorder maxPos pos =
    snd pos == maxPos


collisionByOccupied : Position -> Board -> Bool
collisionByOccupied pos board =
    board |> Dict.member pos


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ every second Tick, Keyboard.presses KeyMsg ]


view : Model -> Html Msg
view model =
    div []
        [ text <| toString model.currentTick
        , text <| toString model.board
        , tablero model
        ]


tablero : Model -> Html Msg
tablero model =
    let
        tablero =
            model.board
                |> Dict.toList
                |> List.map drawSquare
    in
        div [] tablero


squareStyle : Position -> List ( String, String )
squareStyle pos =
    let
        ( x, y ) =
            pos

        blockSize =
            20

        left =
            30 + (x * blockSize)

        top =
            100 + (y * blockSize)
    in
        [ ( "position", "absolute" )
        , ( "left", toString left ++ "px" )
          -- , ( "left", "30px" )
        , ( "top", toString top ++ "px" )
        , ( "height", toString blockSize ++ "px" )
        , ( "width", toString blockSize ++ "px" )
        , ( "background-color", "black" )
        ]


drawSquare : ( Position, Bool ) -> Html Msg
drawSquare ( pos, bool ) =
    div [ style <| squareStyle pos ] []


newPiece : PiecePosition
newPiece =
    { p1 = ( 1, 0 )
    , p2 = ( 2, 0 )
    , p3 = ( 3, 0 )
    , p4 = ( 4, 0 )
    }


init =
    let
        boardHeight =
            20

        boardWidth =
            10

        piecePosition =
            { p1 = ( 1, 0 )
            , p2 = ( 2, 0 )
            , p3 = ( 3, 0 )
            , p4 = ( 4, 0 )
            }

        model =
            { currentPiece = I
            , currentTick = 0
            , currentPosition = ( 5, 1 )
            , boardHeight = boardHeight
            , boardWidth = boardWidth
            , board = Dict.insert ( 5, 1 ) True Dict.empty
            , piecePosition = piecePosition
            }
    in
        model ! []


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
