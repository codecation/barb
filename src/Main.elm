module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


-- Algorithm
-- Start: generate a random DNA sequence.
-- 1. Copy the current DNA sequence and mutate it slightly
-- 2. Use the new DNA to render polygons onto a canvas
-- 3. Compare the canvas to the source image
-- 4. If the new image looks more like the source image than the previous
--    image did, then overwrite the current DNA with the new DNA
-- 5. Repeat
-- MODEL


type alias Model =
    { fittestDNA : String
    , candidateDNA : String
    }

type alias Circle =
    { center : (Int, Int)
    }


init : ( Model, Cmd Msg )
init =
    ( { fittestDNA = "dnA", candidateDNA = "hey" }
    , Cmd.none
    )


randomCircle : Random.Generator Circle
randomCircle =
   Random.map Circle (Random.pair (Random.int -200 200) (Random.int -200 200))



-- UPDATE


type Msg
    = NoOp
    | Start
    | InitialDNA (List Circle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Start ->
            ( model, Random.generate InitialDNA (Random.list 10 randomCircle) )

        InitialDNA _ ->
            ( model, Cmd.none )




-- VIEW


uploadedImageSize : Attribute msg
uploadedImageSize =
    style
        [ ( "width", "300px" )
        , ( "height", "300px" )
        ]


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div [ class "images-image_container" ]
            [ img [ src "http://yumurtaliekmek.com/wp-content/uploads/2014/11/manet-teknede-0711.jpg", class "images-original_image_container-image" ] [] ]
        , div [ class "images-image_container" ]
            [ div [ uploadedImageSize, class "images-image_container-generated_image_canvas" ]
                []
            , div [ class "controls" ]
                [ button [ Html.Events.onClick Start, class "controls-start" ] [ text "Start" ] ]
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
