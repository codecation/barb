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
import Debug
import Exts.Float


-- MODEL


numberOfCircles : Int
numberOfCircles =
    150


minimumRadiusLength : Float
minimumRadiusLength =
    1.0


maximumRadiusLength : Float
maximumRadiusLength =
    80.0


maximumAlpha : Float
maximumAlpha =
    0.7


minimumAlpha : Float
minimumAlpha =
    0.1


type alias Model =
    { fittest : Image
    , fittestFitness : Float
    , fittestFitnessHistory : List Float
    , candidate : Image
    , candidateFitness : Float
    , candidateFitnessHistory : List Float
    , iterations : Int
    , sourceImageRgbData : List Int
    }


type alias Circle =
    { center : ( Float, Float )
    , radius : Float
    , color :
        Color.Color
        -- TODO - combine color and alpha into rgba val
    , alpha : Float
    }


type alias Image =
    List Circle


init : ( Model, Cmd Msg )
init =
    ( { fittest = []
      , fittestFitness = 0.0
      , fittestFitnessHistory = List.repeat 25 0.0
      , candidate = []
      , candidateFitness = 0.0
      , candidateFitnessHistory = List.repeat 25 0.0
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


maybeMutateCenter : ( Float, Float ) -> Random.Generator ( Float, Float )
maybeMutateCenter ( x, y ) =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant ( x, y ) )
        , ( 10.0
          , (Random.float -20.0 20.0)
                `Random.andThen` \f -> Random.Extra.constant ( (f + x), (f + y) )
          )
        ]
        

randomColor : Random.Generator Color.Color
randomColor =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


maybeMutateColor : Color.Color -> Random.Generator Color.Color
maybeMutateColor color =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant color )
        , ( 10.0, randomColor)
        ]


maybeMutateAlpha : Float -> Random.Generator Float
maybeMutateAlpha alpha =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant alpha )
        , ( 10.0
          , (Random.float -80.0 80.0)
                `Random.andThen` \f -> Random.Extra.constant (clamp minimumAlpha maximumAlpha (f + alpha))
          )
        ]


maybeMutateRadius : Float -> Random.Generator Float
maybeMutateRadius radius =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant radius )
        , ( 10.0
          , (Random.float -40.0 40.0)
                `Random.andThen` \f -> Random.Extra.constant (clamp minimumRadiusLength maximumRadiusLength (f + radius))
          )
        ]


mutateCircle : Circle -> Random.Generator Circle
mutateCircle circle =
    Random.map4
        Circle
        (maybeMutateCenter circle.center)
        (maybeMutateRadius circle.radius)
        (maybeMutateColor circle.color)
        (maybeMutateAlpha circle.alpha)


sometimesMutate : Circle -> Random.Generator Circle
sometimesMutate circle =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant circle )
        , ( 9.0, mutateCircle circle )
        , ( 1.0, randomCircle )
        ]


mutateImage : List Circle -> Random.Generator (List Circle)
mutateImage listOfCircles =
    let
        listOfGenerators =
            List.map sometimesMutate listOfCircles
    in
        Random.Extra.together listOfGenerators


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


shiftList : List Float -> Float -> List Float
shiftList existingList newListItem =
  List.append (List.drop 1 existingList) [newListItem]


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
                            , fittestFitnessHistory =
                                  shiftList model.fittestFitnessHistory (exaggeratePercentage newCandidateFitness)
                            , iterations = model.iterations + 1
                            , candidateFitness = newCandidateFitness
                        }
                else
                    update MutateCandidate
                        { model
                            | candidateFitness = newCandidateFitness
                            , candidateFitnessHistory =
                                  shiftList model.candidateFitnessHistory (exaggeratePercentage newCandidateFitness)
                            , candidate = model.fittest
                            , iterations = model.iterations + 1
                        }

        RequestImageData ->
            ( model, requestImageDetails "" )

        GenerateFirstCandidate ->
            ( model, Random.generate UpdateCandidate (Random.list numberOfCircles randomCircle) )

        MutateCandidate ->
            ( model, Random.generate UpdateCandidate (mutateImage model.candidate) )

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


exaggeratePercentage : Float -> Float
exaggeratePercentage number =
    (((number * 100) - 95) * 20) / 100

graphBar : Float -> Html Msg
graphBar percentage =
    div [ class "graph-bar", style [ ( "height", (displayablePercentage percentage) ) ] ] []

graphList : List Float -> Html Msg
graphList fitnessHistory =
    let
        graphBars =
            List.map (\x -> graphBar x) fitnessHistory
    in
        div [ class "graph" ] graphBars


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div [ class "images-image_container" ]
            [ img [ src "img/manet.jpg", class "images-original_image_container-image" ] [] ]
        , div [ class "images-image_container images-image_container--hoverable" ]
            [ div
                [ class "images-image_container-peeking_number" ]
                [ text <| displayablePercentage model.fittestFitness ]
            , div
                [ styleUploadedImageSize ]
                [ drawCandidate model.fittest ]
            , graphList model.fittestFitnessHistory
            ]
        , div
            [ Html.Events.onClick GenerateFirstCandidate
            , class "images-image_container images-image_container--hoverable"
            ]
            [ div
                [ class "images-image_container-peeking_number" ]
                [ text <| displayablePercentage model.candidateFitness ]
            , div
                [ class "images-image_container-generated_image_canvas"
                , styleUploadedImageSize
                ]
                [ drawCandidate model.candidate ]
            , div
                [ class "images-image_container-overlay_text" ]
                [ text <| toString model.iterations ]
            , graphList model.candidateFitnessHistory
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
