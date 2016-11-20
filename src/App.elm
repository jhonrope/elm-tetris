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
                ( model.currentPosition
                , model.board
                )
            else
                ( ( x, y )
                , model.board
                    |> Dict.insert ( x, y ) True
                    |> Dict.remove model.currentPosition
                )
    in
        ( newCurrentPos, newBoard )


calculateNewData : Model -> ( Position, Board )
calculateNewData model =
    let
        ( x, y ) =
            (updatePosition 0 1 model.currentPosition)

        ( newCurrentPos, newBoard ) =
            if
                collisionByBorder model.boardHeight model.currentPosition
                    || collisionByOccupied ( x, y ) model.board
            then
                ( ( 5, 1 )
                , model.board
                    |> Dict.insert ( 5, 1 ) True
                )
            else
                ( ( x, y )
                , model.board
                    |> Dict.insert ( x, y ) True
                    |> Dict.remove model.currentPosition
                )
    in
        ( newCurrentPos, newBoard )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                _ =
                    log "tick" model.currentPosition

                ( newCurrentPos, newBoard ) =
                    calculateNewData model
            in
                { model
                    | currentTick = model.currentTick + 1
                    , currentPosition = newCurrentPos
                    , board = newBoard
                }
                    ! []

        KeyMsg code ->
            case Char.fromCode code of
                's' ->
                    let
                        _ =
                            log "s" model.currentPosition

                        ( newCurrentPos, newBoard ) =
                            calculateNewData model
                    in
                        { model
                            | currentPosition = newCurrentPos
                            , board = newBoard
                        }
                            ! []

                'a' ->
                    let
                        ( newCurrentPos, newBoard ) =
                            movePieceLeft model
                    in
                        { model
                            | currentPosition = newCurrentPos
                            , board = newBoard
                        }
                            ! []

                'w' ->
                    let
                        _ =
                            log "w" model.currentPosition

                        ( newCurrentPos, newBoard ) =
                            calculateNewData model
                    in
                        { model
                            | currentTick = model.currentTick + 1
                            , currentPosition = newCurrentPos
                            , board = newBoard
                        }
                            ! []

                _ ->
                    let
                        _ =
                            log "tecla" code
                    in
                        model ! []


collisionByBorderLeft : Position -> Bool
collisionByBorderLeft pos =
    fst pos <= 1


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


init =
    let
        boardHeight =
            5

        boardWidth =
            10

        model =
            { currentPiece = I
            , currentTick = 0
            , currentPosition = ( 5, 1 )
            , boardHeight = boardHeight
            , boardWidth = boardWidth
            , board = Dict.insert ( 5, 1 ) True Dict.empty
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
