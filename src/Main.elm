module Main exposing (..)

-- Elm Core

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div [ class "images-image_container" ]
            [ img [ src "http://yumurtaliekmek.com/wp-content/uploads/2014/11/manet-teknede-0711.jpg", class "images-original_image_container-image" ] [] ]
        , div [ class "images-image_container" ]
            []
        , div [ class "controls" ]
            [ div [ class "controls-start" ]
                [ button [] [ text "Start" ] ]
            ]
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
