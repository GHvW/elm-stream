module Stream exposing
    ( Stream
    , append
    , concat
    , concatMap
    , cons
    , empty
    , filter
    , foldl
    , foldr
    , fromList
    , head
    , map
    , scan
    , tail
    , take
    , toList
    , zip
    )

import Debug exposing (toString)
import Html exposing (a, b, text)
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


cons : a -> Thunk (Stream a) -> Stream a
cons a thunkedStream =
    Cons ( a, thunkedStream )



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


flip2 : (a -> b -> c) -> (b -> a -> c)
flip2 fn =
    \b -> \a -> fn a b


append : Stream a -> Stream a -> Stream a
append first second =
    case second of
        Empty ->
            first

        _ ->
            -- foldr cons second first
            case first of
                Empty ->
                    second

                Cons ( x, xs ) ->
                    Cons ( x, \() -> append (force xs) second )


concat : Stream (Stream a) -> Stream a
concat stream =
    case stream of
        Empty ->
            Empty

        Cons ( x, xs ) ->
            case x of
                Empty ->
                    concat (force xs)

                Cons ( y, ys ) ->
                    Cons ( y, \() -> concat (Cons ( force ys, xs )) )


map : (a -> b) -> Stream a -> Stream b
map func stream =
    case stream of
        Empty ->
            Empty

        Cons ( it, next ) ->
            Cons ( func it, \() -> map func (force next) )


concatMap : (a -> Stream b) -> Stream a -> Stream b
concatMap func stream =
    concat (map func stream)


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

        Cons ( x, xs ) ->
            case second of
                Empty ->
                    Empty

                Cons ( y, ys ) ->
                    Cons ( ( x, y ), \() -> zip (force xs) (force ys) )


type alias Reducer a b =
    a -> b -> b


foldl : Reducer a b -> b -> Stream a -> b
foldl reducer initialState stream =
    case stream of
        Empty ->
            initialState

        Cons ( x, xs ) ->
            foldl reducer (reducer x initialState) (force xs)


foldr : Reducer a b -> b -> Stream a -> b
foldr reducer initialState stream =
    case stream of
        Empty ->
            initialState

        Cons ( x, xs ) ->
            reducer x (foldr reducer initialState (force xs))


scan : Reducer a b -> b -> Stream a -> Stream b
scan reducer initialState stream =
    case stream of
        Empty ->
            Empty

        Cons ( x, xs ) ->
            let
                nextState =
                    reducer x initialState
            in
            Cons ( nextState, \() -> scan reducer nextState (force xs) )



-- to other data structures


toList : Stream a -> List a
toList stream =
    foldr (::) [] stream



-- from base data structures


fromList : List a -> Stream a
fromList list =
    case list of
        [] ->
            Empty

        x :: xs ->
            Cons ( x, \() -> fromList xs )



-- generate
-- TODO - iterate
-- TODO - unfold
