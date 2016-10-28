port module Main exposing (..)

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
    , fittestFitness : Float
    , candidate : Image
    , candidateFitness : Float
    , iterations : Int
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
    ( { fittest = [], fittestFitness = 0.0, candidate = [], candidateFitness = 0.0, iterations = 0 }
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
    | GenerateFirstImage Image
    | ReceiveImageData ( List Int, List Int )
    | RequestImageData
    | GenerateNewCandidate
    | UpdateCandidate Image


checkFitness : ( List Int, List Int ) -> Float
checkFitness ( uploadedImage, candidateImage ) =
    let
        pixelCount =
            List.length uploadedImage

        differences =
            List.map2 (-) uploadedImage candidateImage

        squares =
            List.map (\x -> x ^ 2) differences

        sumOfSquares =
            List.foldr (+) 0 squares

        maximumDifference =
            pixelCount * 256 * 256
    in
        1 - (sumOfSquares / toFloat (maximumDifference))


-- mutate : Image -> Image
-- mutate image =



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        Start ->
            ( model, Random.generate GenerateFirstImage (Random.list numberOfCircles randomCircle) )

        GenerateFirstImage image ->
            ( { model | fittest = image, fittestFitness = 0, candidate = image }, Cmd.none )

        ReceiveImageData rgbValues ->
            let
                candidateFitness =
                    checkFitness rgbValues
            in
                if candidateFitness > model.fittestFitness then
                    let
                        newModel =
                          { model | fittest = model.candidate, fittestFitness = candidateFitness, iterations = model.iterations + 1 }
                    in
                        ( newModel, Cmd.none )
                else
                    ( { model | candidateFitness = candidateFitness, iterations = model.iterations + 1 }, Cmd.none )

        RequestImageData ->
            ( model, requestImageDetails "" )

        GenerateNewCandidate ->
            ( model, Random.generate UpdateCandidate (Random.list numberOfCircles randomCircle) )

        UpdateCandidate image ->
            ( { model | candidate = image }, Cmd.none )



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
        , div [ class "images-image_container" ]
            [ div [ styleUploadedImageSize, Html.Events.onClick Start ]
                [ drawCandidate model.fittest ]
            ]
        , div [ class "images-image_container images-image_container--clickable" ]
            [ div [ styleUploadedImageSize, Html.Events.onClick Start, class "images-image_container-generated_image_canvas" ]
                [ drawCandidate model.candidate ]
            ]
        , div [ class "debug_area" ]
            [ button [ Html.Events.onClick RequestImageData ] [ text "RequestImageData" ]
            , button [ Html.Events.onClick GenerateNewCandidate ] [ text "GenerateNewCandidate" ]
            , div [] [ text <| "fittestFitness: " ++ toString model.fittestFitness ]
            , div [] [ text <| "candidateFitness: " ++ toString model.candidateFitness ]
            , div [] [ text <| "iterations: " ++ toString model.iterations ]
            ]
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
    imageDetails ReceiveImageData


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port requestImageDetails : String -> Cmd msg


port imageDetails : (( List Int, List Int ) -> msg) -> Sub msg
