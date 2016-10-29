port module Main exposing (..)

import Collage
import Color exposing (Color)
import Element
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator)
import Random.Color
import Random.Extra
import Task
import Process
import Exts.Float


-- MODEL


numberOfPolygons : Int
numberOfPolygons =
    125


maximumAlpha : Float
maximumAlpha =
    0.9


minimumAlpha : Float
minimumAlpha =
    0.1


maximumVertexDisplacement : Float
maximumVertexDisplacement =
    50


minimumRGBValue : Int
minimumRGBValue =
    10


maximumRGBValue : Int
maximumRGBValue =
    240


type alias Model =
    { fittest : List Polygon
    , fittestFitness : Float
    , fittestFitnessHistory : List Float
    , candidate : List Polygon
    , candidateFitness : Float
    , candidateFitnessHistory : List Float
    , iterations : Int
    , sourceImageRgbData : List Int
    }


type alias Polygon =
    { vertices : List ( Float, Float )
    , color :
        Color
        -- todo: combine color and alpha to rgba
    , alpha : Float
    }


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


randomPolygon : Generator Polygon
randomPolygon =
    Random.map3
        Polygon
        (Random.list
            3
            (Random.pair
                (Random.float -maximumVertexDisplacement maximumVertexDisplacement)
                (Random.float -maximumVertexDisplacement maximumVertexDisplacement)
            )
        )
        Random.Color.rgba
        (Random.float minimumAlpha maximumAlpha)


maybeMutateColor : Color -> Generator Color
maybeMutateColor color =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant color )
        , ( 10.0, Random.Color.rgba )
        ]


maybeMutateAlpha : Float -> Generator Float
maybeMutateAlpha alpha =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant alpha )
        , ( 10.0
          , (Random.float -80.0 80.0)
                `Random.andThen` \f -> Random.Extra.constant (clamp minimumAlpha maximumAlpha (f + alpha))
          )
        ]


mutatePolygon : Polygon -> Generator Polygon
mutatePolygon polygon =
    Random.map3
        Polygon
        (maybeMutateVertices polygon.vertices)
        (maybeMutateColor polygon.color)
        (maybeMutateAlpha polygon.alpha)


sometimesMutate : Polygon -> Generator Polygon
sometimesMutate polygon =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant polygon )
        , ( 9.0, mutatePolygon polygon )
        , ( 1.0, randomPolygon )
        ]


maybeMutateVertices : List ( Float, Float ) -> Generator (List ( Float, Float ))
maybeMutateVertices vertices =
    let
        listOfGenerators =
            List.map sometimesMutateVertex vertices
    in
        Random.Extra.together listOfGenerators


sometimesMutateVertex : ( Float, Float ) -> Generator ( Float, Float )
sometimesMutateVertex ( x, y ) =
    Random.Extra.frequency
        [ ( 90.0, Random.Extra.constant ( x, y ) )
        , ( 10.0, (Random.float -30.0 30.0) `Random.andThen` \i -> Random.Extra.constant ( (i + x), (i + y) ) )
        ]


mutatePolygons : List Polygon -> Generator (List Polygon)
mutatePolygons polygons =
    let
        listOfGenerators =
            List.map sometimesMutate polygons
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
    List.append (List.drop 1 existingList) [ newListItem ]



-- UPDATE


type Msg
    = CalculateFitness ( List Int, List Int )
    | RequestImageData
    | GenerateFirstCandidate
    | MutateCandidate
    | UpdateCandidate (List Polygon)
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
            ( model, Random.generate UpdateCandidate (Random.list numberOfPolygons randomPolygon) )

        MutateCandidate ->
            ( model, Random.generate UpdateCandidate (mutatePolygons model.candidate) )

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
        [ div [ class "images-image_container images-image_container--hoverable" ]
            [ img [ src "img/bluered.jpg", class "images-original_image_container-image" ] [] ]
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


drawCandidate : List Polygon -> Html Msg
drawCandidate image =
    Collage.collage 100
        100
        (List.map drawPolygon image)
        |> Element.toHtml


drawPolygon : Polygon -> Collage.Form
drawPolygon polygon =
    Collage.polygon polygon.vertices
        |> Collage.filled polygon.color
        |> Collage.alpha polygon.alpha



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
