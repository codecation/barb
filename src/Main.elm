module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Svg
import Svg.Attributes


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
    { fittest : List Circle
    , candidate : List Circle
    }


type alias Circle =
    { center : ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { fittest = [], candidate = [] }
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

        InitialDNA circles ->
            ( { model | fittest = circles }, Cmd.none )


-- VIEW


imageWidth : String
imageWidth =
    "300px"


imageHeight : String
imageHeight =
    "300px"


styleUploadedImageSize : Attribute msg
styleUploadedImageSize =
    style
        [ ( "width", imageWidth )
        , ( "height", imageHeight )
        ]


loaderRect : Html.Html msg
loaderRect =
    svg [ Svg.Attributes.width imageWidth, Svg.Attributes.height imageHeight, Svg.Attributes.class "images-image_container-loader" ]
        [ Svg.rect [ Svg.Attributes.x "0", Svg.Attributes.y "0", Svg.Attributes.width "100%", Svg.Attributes.height "100%" ] [] ]


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div [ class "images-image_container" ]
            [ img [ src "http://yumurtaliekmek.com/wp-content/uploads/2014/11/manet-teknede-0711.jpg", class "images-original_image_container-image" ] [] ]
        , div [ class "images-image_container" ]
            [ div [ styleUploadedImageSize, class "images-image_container-generated_image_canvas" ]
                [drawCandidate model.candidate]
            , loaderRect
            , div [ class "controls" ]
                [ button [ Html.Events.onClick Start, class "controls-start" ] [ text "Start" ] ]
            ]
        ]


drawCandidate : (List Circle) -> String
drawCandidate circles =
    "hey"


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
