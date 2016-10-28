port module Main exposing (..)

import Array exposing (Array)
import Collage
import Color
import Debug
import Element
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


numberOfCircles : Int
numberOfCircles =
    200


minimumRadiusLength : Float
minimumRadiusLength =
    1.0


maximumRadiusLength : Float
maximumRadiusLength =
    80.0


maximumAlpha : Float
maximumAlpha =
    0.85


type alias Model =
    { fittest : Image
    , candidate : Image
    }


type alias Circle =
    { center : ( Float, Float )
    , radius : Float
    , color : Color.Color
    , alpha : Float
    }


type alias Image =
    List Circle


init : ( Model, Cmd Msg )
init =
    ( { fittest = [], candidate = [] }
    , Cmd.none
    )


randomCircle : Random.Generator Circle
randomCircle =
    Random.map4
        Circle
        (Random.pair (Random.float -200 200) (Random.float -200 200))
        (Random.float minimumRadiusLength maximumRadiusLength)
        randomColor
        (Random.float 0 maximumAlpha)


randomColor : Random.Generator Color.Color
randomColor =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)



-- UPDATE


type Msg
    = NoOp
    | Start
    | InitialDNA Image
    | ImageData (Array (Array Int))
    | RequestImageData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Start ->
            ( model, Random.generate InitialDNA (Random.list numberOfCircles randomCircle) )

        InitialDNA image ->
            ( { model | fittest = image }, Cmd.none )

        ImageData rgbValues ->
            let
                test =
                    Debug.log "image arrays" rgbValues
            in
                ( model, Cmd.none )

        RequestImageData ->
            ( model, requestImageDetails "" )



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


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div [ class "images-image_container" ]
            [ img [ src "img/manet.jpg", class "images-original_image_container-image" ] [] ]
        , div [ class "images-image_container images-image_container--clickable" ]
            [ div [ styleUploadedImageSize, Html.Events.onClick Start, class "images-image_container-generated_image_canvas" ]
                [ drawCandidate model.fittest ]
            ]
        , button [ Html.Events.onClick RequestImageData ] [ text "Send" ]
        ]


drawCandidate : Image -> Html Msg
drawCandidate circles =
    Collage.collage 300
        300
        (List.map drawCircle circles)
        |> Element.toHtml


drawCircle : Circle -> Collage.Form
drawCircle circle =
    Collage.circle circle.radius
        |> Collage.filled circle.color
        |> Collage.move circle.center
        |> Collage.alpha circle.alpha



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    imageDetails ImageData


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port requestImageDetails : String -> Cmd msg


port imageDetails : (Array (Array Int) -> msg) -> Sub msg
