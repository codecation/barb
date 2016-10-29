port module Main exposing (..)

import Collage
import Color
import Element
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.Extra
import Task
import Process
import Exts.Float


-- MODEL


numberOfCircles : Int
numberOfCircles =
    75


minimumRadiusLength : Float
minimumRadiusLength =
    1.0


maximumRadiusLength : Float
maximumRadiusLength =
    80.0


maximumAlpha : Float
maximumAlpha =
    1.0


minimumAlpha : Float
minimumAlpha =
    0


type alias Model =
    { fittest : Image
    , fittestFitness : Float
    , candidate : Image
    , candidateFitness : Float
    , iterations : Int
    , sourceImageRgbData : List Int
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
    ( { fittest = []
      , fittestFitness = 0.0
      , candidate = []
      , candidateFitness = 0.0
      , iterations = 0
      , sourceImageRgbData = []
      }
    , Cmd.none
    )


randomCircle : Random.Generator Circle
randomCircle =
    Random.map4
        Circle
        (Random.pair (Random.float -50 50) (Random.float -50 50))
        (Random.float minimumRadiusLength maximumRadiusLength)
        randomColor
        (Random.float minimumAlpha maximumAlpha)


sometimesReplace : Circle -> Random.Generator Circle
sometimesReplace circle =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant circle )
        , ( 10.0, randomCircle )
        ]


generateListWithRandomlyReplaceCircles : Image -> Random.Generator Image
generateListWithRandomlyReplaceCircles listOfCircles =
    let
        listOfGenerators =
            List.map sometimesReplace listOfCircles
    in
        Random.Extra.together listOfGenerators


randomColor : Random.Generator Color.Color
randomColor =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


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



-- UPDATE


type Msg
    = CalculateFitness ( List Int, List Int )
    | RequestImageData
    | GenerateFirstCandidate
    | MutateCandidate
    | UpdateCandidate Image
    | Sleep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalculateFitness imageDataForBothImages ->
            let
                newCandidateFitness =
                    checkFitness imageDataForBothImages
            in
                if newCandidateFitness > model.fittestFitness then
                    update MutateCandidate
                        { model
                            | fittest = model.candidate
                            , fittestFitness = newCandidateFitness
                            , iterations = model.iterations + 1
                            , candidateFitness = newCandidateFitness
                        }
                else
                    update MutateCandidate
                        { model
                            | candidateFitness = newCandidateFitness
                            , candidate = model.fittest
                            , iterations = model.iterations + 1
                        }

        RequestImageData ->
            ( model, requestImageDetails "" )

        GenerateFirstCandidate ->
            ( model, Random.generate UpdateCandidate (Random.list numberOfCircles randomCircle) )

        MutateCandidate ->
            ( model, Random.generate UpdateCandidate (generateListWithRandomlyReplaceCircles model.candidate) )

        UpdateCandidate image ->
            update Sleep { model | candidate = image }

        Sleep ->
            ( model, Task.perform (always RequestImageData) (always RequestImageData) (Process.sleep 0) )



-- VIEW


imageWidth : String
imageWidth =
    "100px"


imageHeight : String
imageHeight =
    "100px"


styleUploadedImageSize : Attribute msg
styleUploadedImageSize =
    style
        [ ( "width", imageWidth )
        , ( "height", imageHeight )
        ]


displayablePercentage : Float -> String
displayablePercentage number =
    let
        rounded =
            Exts.Float.roundTo 2 (number * 100)
    in
        (toString rounded) ++ "%"


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div [ class "images-image_container" ]
            [ img [ src "img/manet.jpg", class "images-original_image_container-image" ] [] ]
        , div [ class "images-image_container" ]
            [ div
                [ class "images-image_container-peeking_number images-image_container-peeking_number--top" ]
                [ text <| displayablePercentage model.fittestFitness ]
            , div
                [ styleUploadedImageSize ]
                [ drawCandidate model.fittest ]
            ]
        , div
            [ Html.Events.onClick GenerateFirstCandidate
            , class "images-image_container images-image_container--clickable"
            ]
            [ div
                [ class "images-image_container-peeking_number images-image_container-peeking_number--top" ]
                [ text <| displayablePercentage model.candidateFitness ]
            , div
                [ class "images-image_container-generated_image_canvas"
                , styleUploadedImageSize
                ]
                [ drawCandidate model.candidate ]
            , div
                [ class "images-image_container-peeking_number images-image_container-peeking_number--bottom" ]
                [ text <| toString model.iterations ]
            ]
        ]


drawCandidate : Image -> Html Msg
drawCandidate circles =
    Collage.collage 100
        100
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
