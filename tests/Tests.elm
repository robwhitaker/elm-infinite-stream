module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import InfiniteStream exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Elm Infinite Stream tests"
        [ creatingStreams
        , consumingStreams
        , subStreams
        , combiningAndSplittingStreams
        , mapping
        , specialMapsAndFolds
        , creative
        ]


creatingStreams : Test
creatingStreams =
    describe "Creating streams"
        [ test "Cycle on a non-empty list should cycle the elements" <|
            \_ ->
                Maybe.map (consumeMany 10 >> Tuple.first) (cycle [ 1, 2, 3 ])
                    |> Expect.equal (Just [ 1, 2, 3, 1, 2, 3, 1, 2, 3, 1 ])
        , test "Cycle on a single-element list should behave like repeat" <|
            \_ ->
                Maybe.map (consumeMany 10 >> Tuple.first) (cycle [ 1 ])
                    |> Expect.equal (Just (Tuple.first <| consumeMany 10 <| repeat 1))
        , test "Cycle on an empty list should return Nothing" <|
            \_ ->
                cycle []
                    |> Expect.equal Nothing
        , test "Repeat should infinitely repeat a single value" <|
            \_ ->
                Tuple.first (consumeMany 5 (repeat "wow"))
                    |> Expect.equal [ "wow", "wow", "wow", "wow", "wow" ]
        , test "Iterate should endlessly apply a function to the previous value" <|
            \_ ->
                Tuple.first (consumeMany 5 (iterate ((*) 2) 3))
                    |> Expect.equal [ 3, 6, 12, 24, 48 ]
        ]


consumingStreams : Test
consumingStreams =
    describe "Consuming streams"
        [ test "consume should produce the next value in the stream and the next stream" <|
            \_ ->
                consume (iterate ((+) 1) 0)
                    |> (\( first, newStream ) ->
                            ( first, head newStream )
                                |> Expect.equal ( 0, 1 )
                       )
        , test "consumeMany should produce the next n values and the next stream" <|
            \_ ->
                consumeMany 5 (iterate ((+) 1) 0)
                    |> (\( first, newStream ) ->
                            ( first, head newStream )
                                |> Expect.equal ( [ 0, 1, 2, 3, 4 ], 5 )
                       )
        , fuzz Fuzz.int "Repeating `consume` n times should produce the same results as `consumeMany n`" <|
            \startingInt ->
                let
                    mkStream n =
                        stream n (\() -> mkStream (n + 1))

                    initialStream =
                        mkStream startingInt

                    ( v1, s1 ) =
                        consume initialStream

                    ( v2, s2 ) =
                        consume s1

                    ( v3, s3 ) =
                        consume s2

                    ( v4, s4 ) =
                        consume s3

                    ( v5, s5 ) =
                        consume s4
                in
                    [ v1, v2, v3, v4, v5 ]
                        |> Expect.equal (Tuple.first <| consumeMany 5 initialStream)
        , test "consumeWhile should consume elements until its predicate becomes false, then return the list of elements and the next stream" <|
            \_ ->
                iterate ((+) 1) 0
                    |> consumeWhile (\n -> n < 10)
                    |> (\( first, newStream ) ->
                            ( first, head newStream )
                                |> Expect.equal ( List.range 0 9, 10 )
                       )
        ]


subStreams : Test
subStreams =
    describe "Sub-streams"
        [ test "head returns the first element of a stream" <|
            \_ ->
                head (repeat 1)
                    |> Expect.equal 1
        , test "tail returns the tail of a stream" <|
            \_ ->
                tail (iterate ((+) 1) 0)
                    |> head
                    |> Expect.equal 1
        , test "filter removes all elements from the stream that don't match the predicate" <|
            \_ ->
                let
                    even n =
                        n % 2 == 0
                in
                    filter even (iterate ((+) 1) 0)
                        |> take 5
                        |> Expect.equal [ 0, 2, 4, 6, 8 ]
        , test "take produces the next n elements of the stream" <|
            \_ ->
                take 5 (iterate ((+) 1) 0)
                    |> Expect.equal [ 0, 1, 2, 3, 4 ]
        , test "takeWhile produces the next elements of the stream as long as they match a predicate" <|
            \_ ->
                iterate ((+) 1) 0
                    |> takeWhile (\n -> n < 10)
                    |> Expect.equal (List.range 0 9)
        , test "drop drops the next n elements from the stream and produces the next stream" <|
            \_ ->
                iterate ((+) 1) 0
                    |> drop 5
                    |> head
                    |> Expect.equal 5
        , test "dropWhile drops elements of the stream as long as they match a predicate and returns the next stream" <|
            \_ ->
                iterate ((+) 1) 0
                    |> dropWhile (\n -> n < 10)
                    |> head
                    |> Expect.equal 10
        ]


combiningAndSplittingStreams : Test
combiningAndSplittingStreams =
    describe "Combining and splitting streams"
        [ test "(<:>) puts an element at the beginning of the stream" <|
            \_ ->
                100
                    <:> repeat 1
                    |> head
                    |> Expect.equal 100
        , test "intersperse adds an element between every element of the stream" <|
            \_ ->
                repeat "wow"
                    |> intersperse "!"
                    |> take 6
                    |> Expect.equal [ "wow", "!", "wow", "!", "wow", "!" ]
        , test "partition splits a stream into two streams (pred was true, pred was false) based on a predicate" <|
            \_ ->
                let
                    even n =
                        n % 2 == 0

                    ( evenStream, oddStream ) =
                        partition even (iterate ((+) 1) 0)

                    partitionedCorrectly =
                        (take 5 evenStream == [ 0, 2, 4, 6, 8 ])
                            && (take 5 oddStream == [ 1, 3, 5, 7, 9 ])
                in
                    Expect.equal True partitionedCorrectly
        ]


mapping : Test
mapping =
    describe "Mapping"
        [ test "map applies a function to every element of a stream" <|
            \_ ->
                repeat 5
                    |> map ((*) 3)
                    |> take 3
                    |> Expect.equal [ 15, 15, 15 ]
        , test "andMap applys a stream of functions to a stream of values" <|
            \_ ->
                map (\x y z -> x + y + z) (repeat 1)
                    |> andMap (iterate ((+) 1) 0)
                    |> andMap (iterate ((*) 2) 1)
                    |> take 5
                    |> Expect.equal [ 2, 4, 7, 12, 21 ]
        , withFuzzyCycles "map2 should be equivalent to `map f s1 |> andMap s2`" <|
            \fuzzyCycles ->
                let
                    ( s1, rest1 ) =
                        consume fuzzyCycles

                    ( s2, rest2 ) =
                        consume rest1

                    f a b =
                        a + b
                in
                    take 5 (map2 f s1 s2)
                        |> Expect.equal (take 5 (map f s1 |> andMap s2))
        , withFuzzyCycles "map3 should be equivalent to `map f s1 |> andMap s2 |> andMap s3`" <|
            \fuzzyCycles ->
                let
                    ( s1, rest1 ) =
                        consume fuzzyCycles

                    ( s2, rest2 ) =
                        consume rest1

                    ( s3, rest3 ) =
                        consume rest2

                    f a b c =
                        a + b + c
                in
                    take 5 (map3 f s1 s2 s3)
                        |> Expect.equal (take 5 (map f s1 |> andMap s2 |> andMap s3))
        , withFuzzyCycles "map4 should be equivalent to `map f s1 |> andMap s2 |> andMap s3 |> andMap s4`" <|
            \fuzzyCycles ->
                let
                    ( s1, rest1 ) =
                        consume fuzzyCycles

                    ( s2, rest2 ) =
                        consume rest1

                    ( s3, rest3 ) =
                        consume rest2

                    ( s4, rest4 ) =
                        consume rest3

                    f a b c d =
                        a + b + c + d
                in
                    take 5 (map4 f s1 s2 s3 s4)
                        |> Expect.equal (take 5 (map f s1 |> andMap s2 |> andMap s3 |> andMap s4))
        , withFuzzyCycles "map5 should be equivalent to `map f s1 |> andMap s2 |> andMap s3 |> andMap s4 |> andMap s5`" <|
            \fuzzyCycles ->
                let
                    ( s1, rest1 ) =
                        consume fuzzyCycles

                    ( s2, rest2 ) =
                        consume rest1

                    ( s3, rest3 ) =
                        consume rest2

                    ( s4, rest4 ) =
                        consume rest3

                    ( s5, rest5 ) =
                        consume rest4

                    f a b c d e =
                        a + b + c + d + e
                in
                    take 5 (map5 f s1 s2 s3 s4 s5)
                        |> Expect.equal (take 5 (map f s1 |> andMap s2 |> andMap s3 |> andMap s4 |> andMap s5))
        ]


specialMapsAndFolds : Test
specialMapsAndFolds =
    describe "Special maps and folds"
        [ test "filterMap should filter any elements that return Nothing from the stream" <|
            \_ ->
                let
                    lists =
                        cycle [ [], [ 1, 2, 3 ], [], [], [ 24 ], [ 7, 8, 9 ] ]
                in
                    Maybe.map (filterMap List.head) lists
                        |> Maybe.map (take 5)
                        |> Expect.equal (Just [ 1, 24, 7, 1, 24 ])
        , test "indexedMap should map a function over an element's index and the element" <|
            \_ ->
                take 3 (indexedMap (,) (repeat "index"))
                    |> Expect.equal [ ( 0, "index" ), ( 1, "index" ), ( 2, "index" ) ]
        , test "scanl should reduce a stream from the left, building up a new stream from the intermediate values" <|
            \_ ->
                take 5 (scanl (*) 1 (repeat 2))
                    |> Expect.equal [ 1, 2, 4, 8, 16 ]
        ]


creative : Test
creative =
    describe "Creative tests"
        [ test "Combining stuff" <|
            \_ ->
                let
                    s1 =
                        cycle (List.range 1 9)
                            |> Maybe.withDefault (repeat 1)
                            |> filter (\val -> val /= 5)
                            |> map ((*) 10)

                    s2 =
                        repeat 5
                            |> intersperse 0

                    s3 =
                        iterate ((+) 1) 1
                            |> scanl (*) 100
                            |> map (\val -> min 10000 val)

                    run =
                        map3 (\a b c -> a + b + c) s1 s2 s3
                            |> indexedMap (,)
                in
                    run
                        |> take 5
                        |> Expect.equal
                            [ ( 0, 115 )
                            , ( 1, 120 )
                            , ( 2, 235 )
                            , ( 3, 640 )
                            , ( 4, 2465 )
                            ]
        ]



---- HELPERS ----


withFuzzyCycles : String -> (Stream (Stream number) -> Expectation) -> Test
withFuzzyCycles label test =
    fuzz (Fuzz.list (Fuzz.list Fuzz.int)) label <|
        \fuzzyLists ->
            let
                fuzzyLists_ =
                    -- absolutely do not allow empty lists to sneak through
                    -- or the program _will_ hang
                    case cycle (List.filter (not << List.isEmpty) fuzzyLists) of
                        Nothing ->
                            repeat [ 1, 3, 5 ]

                        Just list ->
                            list

                fuzzyCycles =
                    filterMap cycle fuzzyLists_
            in
                test fuzzyCycles
