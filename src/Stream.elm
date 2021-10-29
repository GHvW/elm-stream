module Stream exposing (Stream, cons, empty, filter, fromList, head, map, tail, zip)

import Debug exposing (toString)
import Html exposing (a, text)
import List exposing (range)



-- Stream


type alias Thunk a =
    () -> a


type Stream a
    = Cons ( a, Thunk (Stream a) )
    | Empty



-- private util


force : Thunk (Stream a) -> Stream a
force thunk =
    thunk ()



-- base ops


head : Stream a -> Maybe a
head stream =
    case stream of
        Empty ->
            Nothing

        Cons ( it, _ ) ->
            Just it


tail : Stream a -> Stream a
tail stream =
    case stream of
        Empty ->
            stream

        Cons ( _, next ) ->
            force next


empty : Stream a
empty =
    Empty


cons : a -> Stream a -> Stream a
cons a stream =
    Cons ( a, \() -> stream )



-- ops


filter : (a -> Bool) -> Stream a -> Stream a
filter predicate stream =
    case stream of
        Empty ->
            stream

        Cons ( it, next ) ->
            if predicate it then
                Cons ( it, \() -> filter predicate (force next) )

            else
                filter predicate (force next)


map : (a -> b) -> Stream a -> Stream b
map func stream =
    case stream of
        Empty ->
            Empty

        Cons ( it, next ) ->
            Cons ( func it, \() -> map func (force next) )


take : Int -> Stream a -> Stream a
take n stream =
    if n == 0 then
        Empty

    else
        case stream of
            Empty ->
                Empty

            Cons ( a, next ) ->
                Cons ( a, \() -> take (n - 1) (force next) )


zip : Stream a -> Stream b -> Stream ( a, b )
zip first second =
    case first of
        Empty ->
            Empty

        Cons ( a, nextAs ) ->
            case second of
                Empty ->
                    Empty

                Cons ( b, nextBs ) ->
                    Cons ( ( a, b ), \() -> zip (force nextAs) (force nextBs) )



-- from base data structures


fromList : List a -> Stream a
fromList list =
    case list of
        [] ->
            Empty

        x :: xs ->
            Cons ( x, \() -> fromList xs )


main =
    let
        conses =
            cons 40 (cons 30 (cons 20 (cons 10 empty)))

        stuff =
            conses
                |> filter (\x -> x > 15)
                |> map (\x -> x + 100)
                |> take 2
                |> toString
    in
    text ("hello stream! " ++ stuff)
