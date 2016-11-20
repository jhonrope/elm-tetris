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
    Int


type alias Board =
    Dict Position Bool


type alias Model =
    { currentPiece : Piece
    , currentTick : Float
    , currentPosition : Position
    , boardHeight : Int
    , board : Board
    }


type Msg
    = NoOp
    | Tick Time
    | KeyMsg Keyboard.KeyCode


calculateNewData : Model -> ( Position, Board )
calculateNewData model =
    let
        ( newCurrentPos, newBoard ) =
            if
                collisionByBorder model.boardHeight (model.currentPosition)
                    || collisionByOccupied (model.currentPosition + 1) model.board
            then
                ( 1
                , model.board
                    |> Dict.insert model.currentPosition True
                    |> Dict.remove (model.currentPosition - 1)
                )
            else
                ( model.currentPosition + 1
                , model.board
                    |> Dict.insert model.currentPosition True
                    |> Dict.remove (model.currentPosition - 1)
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
                    log "pato" model.currentPosition

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
                            log "pato" model.currentPosition

                        ( newCurrentPos, newBoard ) =
                            calculateNewData model
                    in
                        { model
                            | currentPosition = newCurrentPos
                            , board = newBoard
                        }
                            ! []

                _ ->
                    let
                        _ =
                            log "tecla" code
                    in
                        model ! []


collisionByBorder : Int -> Position -> Bool
collisionByBorder maxPos pos =
    pos == maxPos


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
        , table [] [ tablero model ]
        ]


tablero : Model -> Html Msg
tablero model =
    let
        tablero =
            model.board
                |> Dict.toList
                |> List.map drawSquare
    in
        td [] tablero


squareStyle : Position -> List ( String, String )
squareStyle pos =
    let
        blockSize =
            20

        left =
            30 + (pos * blockSize)

        top =
            100 + (pos * blockSize)
    in
        [ ( "position", "absolute" )
          --        , ( "left", toString left ++ "px" )
        , ( "left", "30px" )
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
            10

        model =
            { currentPiece = I
            , currentTick = 0
            , currentPosition = 1
            , boardHeight = boardHeight
            , board = Dict.empty
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
