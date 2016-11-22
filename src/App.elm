module App exposing (..)

import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Dict exposing (..)
import Debug exposing (..)
import Keyboard
import Char
import Random exposing (..)


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
    { piece : Piece
    , currentTick : Float
    , currentPosition : Position
    , boardHeight : Int
    , boardWidth : Int
    , board : Board
    , piecePosition : PiecePosition
    , nextPiece : Piece
    }


type Msg
    = NoOp
    | Tick Time
    | KeyMsg Keyboard.KeyCode
    | NextPiece Piece


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


updatePosition : Int -> Int -> Position -> Position
updatePosition x y pos =
    ( fst pos + x, snd pos + y )


updatePiecePosition : Int -> Int -> PiecePosition -> PiecePosition
updatePiecePosition x y piecePos =
    piecePos
        |> piecePositionToList
        |> List.map (updatePosition x y)
        |> listToPiecePosition


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
            newPiece L


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
    in
        ( newCurrentPosition model nextPos, newBoard model nextPos )


newMovePieceDown : Model -> ( Model, Cmd Msg )
newMovePieceDown model =
    let
        nextPos =
            updatePiecePosition 0 1 model.piecePosition

        ( mdl, cmd ) =
            newBoardPiecePosition model nextPos

        newPos =
            newPiecePosition model nextPos
    in
        { mdl | piecePosition = newPos, piece = model.nextPiece } ! [ cmd ]


newCurrentPosition : Model -> Position -> Position
newCurrentPosition model nextPosition =
    if
        collisionByBorder model.boardHeight model.currentPosition
            || collisionByOccupied nextPosition model.board
    then
        ( 5, 1 )
    else
        nextPosition


newPiecePosition : Model -> PiecePosition -> PiecePosition
newPiecePosition model piecePos =
    let
        list =
            piecePositionToList model.piecePosition

        newPiecePositionList =
            model.piecePosition
                |> piecePositionToList
                |> List.map (updatePosition 0 1)

        collideWithBorder =
            list |> List.any (collisionByBorder model.boardHeight)

        collideWithOther =
            newPiecePositionList |> List.any (\pos -> collisionByOccupied pos model.board)
    in
        if collideWithBorder || collideWithOther then
            newPiece model.piece
        else
            newPiecePositionList |> listToPiecePosition


newBoardPiecePosition : Model -> PiecePosition -> ( Model, Cmd Msg )
newBoardPiecePosition model piecePos =
    let
        list =
            piecePositionToList model.piecePosition

        newPiecePositionList =
            model.piecePosition
                |> piecePositionToList
                |> List.map (updatePosition 0 1)

        collideWithBorder =
            list |> List.any (collisionByBorder model.boardHeight)

        collideWithOther =
            newPiecePositionList |> List.any (\pos -> collisionByOccupied pos model.board)
    in
        if collideWithBorder || collideWithOther then
            { model
                | board =
                    (model.board |> insertPiece model.piecePosition)
                , piecePosition = newPiece model.nextPiece
            }
                ! [ generatePiece ]
        else
            model ! []


piecePositionToDict : PiecePosition -> Board
piecePositionToDict piecePos =
    piecePos
        |> piecePositionToList
        |> List.map (\pos -> ( pos, True ))
        |> Dict.fromList


insertPiece : PiecePosition -> Board -> Board
insertPiece piecePos board =
    let
        positions =
            piecePos |> piecePositionToDict
    in
        board |> Dict.union positions


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


calculatePiecePosition : (Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
calculatePiecePosition fn mdl =
    let
        ( model, cmd ) =
            fn mdl
    in
        model ! [ cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                ( model, cmd ) =
                    calculatePiecePosition newMovePieceDown model
            in
                { model | currentTick = model.currentTick + 1 } ! [ cmd ]

        KeyMsg code ->
            case Char.fromCode code of
                's' ->
                    calculatePiecePosition newMovePieceDown model

                'a' ->
                    calculate movePieceLeft model

                'd' ->
                    calculate movePieceRight model

                _ ->
                    model ! []

        NextPiece piece ->
            { model | nextPiece = piece } ! []


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
        , div [] [ text <| toString model.nextPiece ]
        , tablero model
        ]


tablero : Model -> Html Msg
tablero model =
    let
        piece =
            model.piecePosition
                |> piecePositionToList
                |> List.map (\pos -> ( pos, True ))

        tablero =
            model.board
                |> Dict.toList
                |> List.append piece
                |> List.map drawSquare
    in
        div [] tablero


squareStyle : Position -> List ( String, String )
squareStyle pos =
    let
        ( x, y ) =
            ( Basics.max 1 <| fst pos, Basics.max 1 <| snd pos )

        blockSize =
            20

        left =
            30 + (x * blockSize)

        top =
            100 + (y * blockSize)
    in
        [ ( "position", "absolute" )
        , ( "left", toString left ++ "px" )
        , ( "top", toString top ++ "px" )
        , ( "height", toString blockSize ++ "px" )
        , ( "width", toString blockSize ++ "px" )
        , ( "background-color", "black" )
        ]


drawSquare : ( Position, Bool ) -> Html Msg
drawSquare ( pos, bool ) =
    div [ style <| squareStyle pos ] []


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
            , p2 = ( 6, -1 )
            , p3 = ( 6, 0 )
            , p4 = ( 6, 1 )
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


init =
    let
        boardHeight =
            20

        boardWidth =
            10

        piecePosition =
            newPiece piece

        piece =
            T

        nextPiece =
            I

        model =
            { piece = piece
            , currentTick = 0
            , currentPosition = ( 5, 1 )
            , boardHeight = boardHeight
            , boardWidth = boardWidth
            , board = Dict.empty
            , piecePosition = piecePosition
            , nextPiece = nextPiece
            }
    in
        model ! [ generatePiece ]


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
