module StreamTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Stream
    exposing
        ( Stream
        , append
        , concatMap
        , cons
        , empty
        , filter
        , foldl
        , foldr
        , fromList
        , map
        , scan
        , take
        , toList
        , zip
        )
import Test exposing (..)


suite : Test
suite =
    let
        stream =
            cons 50
                (\() ->
                    cons 40
                        (\() ->
                            cons 30
                                (\() ->
                                    cons 20
                                        (\() -> cons 10 (\() -> empty))
                                )
                        )
                )
    in
    describe "The Stream Module"
        [ describe "given a stream of numbers"
            [ test "when converting it to a list" <|
                \_ -> Expect.equal [ 50, 40, 30, 20, 10 ] (toList stream)
            , test "when adding 10 to each number" <|
                \_ -> Expect.equal [ 60, 50, 40, 30, 20 ] (stream |> map ((+) 10) |> toList)
            , test "when filtering out even numbers" <|
                \_ -> Expect.equal [] (stream |> filter (\it -> modBy 2 it /= 0) |> toList)
            , test "when taking just the first 2 numbers" <|
                \_ -> Expect.equal [ 50, 40 ] (stream |> take 2 |> toList)
            , test "when putting it together with another stream" <|
                \_ ->
                    Expect.equal
                        [ 200, 100, 50, 40, 30, 20, 10 ]
                        (stream
                            |> append (cons 200 (\() -> cons 100 (\() -> empty)))
                            |> toList
                        )
            , test "when summing the numbers" <|
                \_ -> Expect.equal 150 (stream |> foldl (+) 0)
            , test "when zipping another stream" <|
                \_ -> Expect.equal [ ( 200, 50 ), ( 100, 40 ) ] (stream |> zip (cons 200 (\() -> cons 100 (\() -> empty))) |> toList)
            , test "when creating a new stream by adding the last state to the next" <|
                \_ -> Expect.equal [ 50, 90, 120, 140, 150 ] (stream |> scan (+) 0 |> toList)
            , test "when creating a stream from each item and then flattening" <|
                \_ ->
                    Expect.equal
                        [ 45, 55, 35, 45, 25, 35, 15, 25, 5, 15 ]
                        (stream
                            |> concatMap (\it -> fromList [ it - 5, it + 5 ])
                            |> toList
                        )
            ]
        , describe "given a stream of streams of numbers"
            [ test "when flattening them to a single stream of numbers" <|
                \_ ->
                    let
                        first =
                            cons 100 (\() -> cons 200 (\() -> empty))

                        second =
                            cons 1000 (\() -> cons 2000 (\() -> empty))

                        third =
                            cons 10000 (\() -> cons 20000 (\() -> empty))

                        streamOfStreams =
                            cons first (\() -> cons second (\() -> cons third (\() -> empty)))
                    in
                    Expect.equal [ 100, 200, 1000, 2000, 10000, 20000 ] (streamOfStreams |> Stream.concat |> toList)
            ]
        , describe "given a list"
            [ test "when converting it to a stream" <|
                \_ ->
                    let
                        it =
                            [ 1, 2, 3, 4, 5 ]
                    in
                    Expect.equal [ 1, 2, 3, 4, 5 ] (fromList it |> toList)
            ]
        ]
