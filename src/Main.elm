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
    = CalculateFitness ( List Int, List Int )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalculateFitness imageDataForBothImages ->
            let
                candidateFitness =
                    checkFitness imageDataForBothImages
            in
                if candidateFitness > model.fittestFitness then
                    ( { model
                        | fittest = model.candidate
                        , fittestFitness = candidateFitness
                        , iterations = model.iterations + 1
                        , candidateFitness = candidateFitness
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | candidateFitness = candidateFitness
                        , iterations = model.iterations + 1
                      }
                    , Cmd.none
                    )

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
            [ div [ styleUploadedImageSize ]
                [ drawCandidate model.fittest ]
            ]
        , div [ class "images-image_container" ]
            [ div [ styleUploadedImageSize, class "images-image_container-generated_image_canvas" ]
                [ drawCandidate model.candidate ]
            ]
        , div [ class "debug_area" ]
            [ button [ Html.Events.onClick GenerateNewCandidate ] [ text "Generate New Candidate" ]
            , button [ Html.Events.onClick RequestImageData ] [ text "Calculate Fitness" ]
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
    imageDetails CalculateFitness


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
