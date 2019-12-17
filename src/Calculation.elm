module Calculation exposing (..)

import Operation exposing (Operation(..))
import Rational exposing (Rational)


type alias Calculation =
    { display : String
    , value : Rational
    }


fromInt : Int -> Calculation
fromInt number =
    { display = String.fromInt number
    , value = Rational.fromInt number
    }


apply : Operation -> Calculation -> Calculation -> Calculation
apply operation calc1 calc2 =
    let
        newDisplay =
            String.join "" [ "(", calc1.display, Operation.toString operation, calc2.display, ")" ]

        newValue =
            Rational.apply operation calc1.value calc2.value
    in
    { display = newDisplay, value = newValue }
