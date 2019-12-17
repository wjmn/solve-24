module Operation exposing (..)


type Operation
    = Add
    | Subtract
    | Multiply
    | Divide


toString : Operation -> String
toString operation =
    case operation of
        Add ->
            " + "

        Subtract ->
            " - "

        Multiply ->
            " × "

        Divide ->
            " ÷ "
