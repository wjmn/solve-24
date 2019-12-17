module Logic exposing (..)

import Calculation exposing (Calculation)
import List
import Operation exposing (Operation(..))
import Rational exposing (Rational)


distribute : a -> List a -> List (List a)
distribute item list =
    case list of
        [] ->
            [ [ item ] ]

        head :: tail ->
            (item :: list) :: List.map (\t -> head :: t) (distribute item tail)


permute : List a -> List (List a)
permute list =
    case list of
        [] ->
            [ [] ]

        head :: tail ->
            List.concatMap (distribute head) (permute tail)


fromList : List Int -> List Calculation
fromList ints =
    List.map Calculation.fromInt ints


reduce : List Calculation -> List Calculation
reduce calculations =
    case calculations of
        [] ->
            []

        [ final ] ->
            [ final ]

        calc1 :: calc2 :: tail ->
            let
                newCalcs =
                    [ Calculation.apply Add calc1 calc2
                    , Calculation.apply Subtract calc1 calc2
                    , Calculation.apply Multiply calc1 calc2
                    , Calculation.apply Divide calc1 calc2
                    ]
            in
            newCalcs
                |> List.map (\head -> head :: tail)
                |> List.concatMap permute
                |> List.concatMap reduce


solve : Int -> List Int -> List Calculation
solve goal calculations =
    calculations
        |> List.map Calculation.fromInt
        |> permute
        |> List.concatMap reduce
        |> List.filter (\calculation -> Rational.toInt calculation.value == Just goal)


solutions : Int -> List Int -> List String
solutions goal calculations =
    solve goal calculations
        |> List.map (\calculation -> calculation.display)
