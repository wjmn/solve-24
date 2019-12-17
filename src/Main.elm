module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Logic exposing (..)



---- MODEL ----


type alias Model =
    { numOne : Maybe Int
    , numTwo : Maybe Int
    , numThree : Maybe Int
    , numFour : Maybe Int
    , goal : Maybe Int
    , solutions : Maybe (List String)
    , errors : Maybe String
    }


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


combine : Int -> Int -> Int -> Int -> Int -> ( List Int, Int )
combine n1 n2 n3 n4 goal =
    ( [ n1, n2, n3, n4 ], goal )


validate : Model -> Maybe ( List Int, Int )
validate model =
    Maybe.map5 combine model.numOne model.numTwo model.numThree model.numFour model.goal


init : ( Model, Cmd Msg )
init =
    ( { numOne = Nothing
      , numTwo = Nothing
      , numThree = Nothing
      , numFour = Nothing
      , goal = Just 24
      , solutions = Nothing
      , errors = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | ChangedNumOne String
    | ChangedNumTwo String
    | ChangedNumThree String
    | ChangedNumFour String
    | ChangedGoal String
    | ClickedSubmit
    | ClickedReset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        resetModel =
            case model.solutions of
                Just _ -> 
                    { model | solutions = Just [], errors = Nothing }
                Nothing -> 
                    { model | errors = Nothing }

        changedModel =
            { resetModel | errors = Just "You need to provide a valid numerical value." }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangedNumOne string ->
            case String.toInt string of
                Nothing ->
                    ( { changedModel | numOne = Nothing }, Cmd.none )

                int ->
                    ( { resetModel | numOne = int }, Cmd.none )

        ChangedNumTwo string ->
            case String.toInt string of
                Nothing ->
                    ( { changedModel | numTwo = Nothing }, Cmd.none )

                int ->
                    ( { resetModel | numTwo = int }, Cmd.none )

        ChangedNumThree string ->
            case String.toInt string of
                Nothing ->
                    ( { changedModel | numThree = Nothing }, Cmd.none )

                int ->
                    ( { resetModel | numThree = int }, Cmd.none )

        ChangedNumFour string ->
            case String.toInt string of
                Nothing ->
                    ( { changedModel | numFour = Nothing }, Cmd.none )

                int ->
                    ( { resetModel | numFour = int }, Cmd.none )

        ChangedGoal string ->
            case String.toInt string of
                Nothing ->
                    ( { changedModel | goal = Nothing }, Cmd.none )

                int ->
                    ( { resetModel | goal = int }, Cmd.none )

        ClickedSubmit ->
            case validate model of
                Just ( numbers, goal ) ->
                    let
                        solutions =
                            Logic.solutions goal numbers
                        errors = 
                            case solutions of
                                [] -> Just "No solutions found!"
                                _ -> Nothing
                    in
                    ( { model | solutions = Just solutions, errors = errors }
                    , Cmd.none
                    )

                Nothing ->
                    ( { resetModel | errors = Just "You need to fill in all the values before you can submit." }, Cmd.none )

        ClickedReset ->
            init



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        toValue maybeNum =
            maybeNum
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""

        valueGoal =
            toValue model.goal

        valueOne =
            toValue model.numOne

        valueTwo =
            toValue model.numTwo

        valueThree =
            toValue model.numThree

        valueFour =
            toValue model.numFour
    in
    main_ [ class "hero", classList [ ( "full-height", not (isJust model.solutions) ) ] ]
        [ article [ class "centric" ]
            [ header [ class "card-header" ] [ h1 [ class "title" ] [ text "24 Solver" ] ]
            , section [ class "card-body" ]
                [ section [ class "row" ]
                    [ h1 [ class "label" ] [ text "Your goal " ]
                    , div [ class "right" ] [ input [ class "numerical wide", value valueGoal, onInput ChangedGoal, type_ "number" ] [] ]
                    ]
                , section [ class "row" ]
                    [ h1 [ class "label" ] [ text "Your numbers " ]
                    , div [ class "right" ]
                        [ input [ class "numerical small", value valueOne, onInput ChangedNumOne, type_ "number" ] []
                        , input [ class "numerical small", value valueTwo, onInput ChangedNumTwo, type_ "number" ] []
                        , input [ class "numerical small", value valueThree, onInput ChangedNumThree, type_ "number" ] []
                        , input [ class "numerical small", value valueFour, onInput ChangedNumFour, type_ "number" ] []
                        ]
                    ]
                , section [ class "row" ]
                    [ button [ class "small", onClick ClickedReset ] [ text "Reset" ]
                    , button [ class "big", onClick ClickedSubmit ] [ text "Solve!" ]
                    ]
                , viewErrors model.errors
                , viewSolutions model.solutions
                ]
            ]
        ]


viewErrors : Maybe String -> Html Msg
viewErrors maybeError =
    case maybeError of
        Nothing ->
            div [] []

        Just error ->
            article [ class "error" ] [ p [] [ text error ] ]


viewSolution : String -> Html Msg
viewSolution string =
    article [ class "solution" ] [ text string ]


viewSolutions : Maybe (List String) -> Html Msg
viewSolutions maybeSolutions =
    case maybeSolutions of
        Just solutions ->
            section [ class "solutions" ] (List.map viewSolution solutions)

        Nothing ->
            div [] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
