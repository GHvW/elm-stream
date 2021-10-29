module Stream exposing (Stream, cons, empty, filter, fromList, head, map, tail, zip)

import Html exposing (a, text)



-- Stream


type alias Thunk a =
    () -> a


type StreamCell a
    = Cons ( a, Stream a )
    | Empty


type alias Stream a =
    Thunk (StreamCell a)



-- private util


force : Stream a -> StreamCell a
force thunk =
    thunk ()



-- base ops


head : Stream a -> Maybe a
head stream =
    case force stream of
        Empty ->
            Nothing

        Cons ( it, _ ) ->
            Just it


tail : Stream a -> Stream a
tail stream =
    case force stream of
        Empty ->
            stream

        Cons ( _, next ) ->
            next


empty : Stream a
empty =
    \() -> Empty


cons : a -> Stream a -> Stream a
cons a stream =
    \() -> Cons ( a, stream )



-- ops


filter : (a -> Bool) -> Stream a -> Stream a
filter predicate stream =
    case force stream of
        Empty ->
            stream

        Cons ( it, next ) ->
            if predicate it then
                \() -> Cons ( it, filter predicate next )

            else
                filter predicate next


map : (a -> b) -> Stream a -> Stream b
map func stream =
    case force stream of
        Empty ->
            \() -> Empty

        Cons ( it, next ) ->
            \() -> Cons ( func it, map func next )


zip : Stream a -> Stream b -> Stream ( a, b )
zip first second =
    case force first of
        Empty ->
            \() -> Empty

        Cons ( a, nextAs ) ->
            case force second of
                Empty ->
                    \() -> Empty

                Cons ( b, nextBs ) ->
                    \() -> Cons ( ( a, b ), zip nextAs nextBs )



-- from base data structures


fromList : List a -> Stream a
fromList list =
    case list of
        [] ->
            \() -> Empty

        x :: xs ->
            \() -> Cons ( x, fromList xs )


main =
    let
        conses =
            cons 30 (cons 20 (cons 10 empty))

        stuff =
            conses
                |> filter (\x -> x > 15)
                |> map (\x -> x + 100)
    in
    text "hello stream!"
