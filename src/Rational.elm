module Rational exposing (..)

import Operation exposing (Operation(..))


type Rational
    = Rational Int Int


fromInt : Int -> Rational
fromInt number =
    Rational number 1


toInt : Rational -> Maybe Int
toInt (Rational num denom) =
    if denom /= 0 then
        if modBy denom num == 0 then
            Just (num // denom)

        else
            Nothing

    else
        Nothing


apply : Operation -> Rational -> Rational -> Rational
apply operation (Rational n1 d1) (Rational n2 d2) =
    case operation of
        Add ->
            Rational (n1 * d2 + n2 * d1) (d1 * d2)

        Subtract ->
            Rational (n1 * d2 - n2 * d1) (d1 * d2)

        Multiply ->
            Rational (n1 * n2) (d1 * d2)

        Divide ->
            Rational (n1 * d2) (d1 * n2)
