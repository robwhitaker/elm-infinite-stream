module InfiniteStream
    exposing
        ( (<:>)
        , Stream
        , andMap
        , consume
        , consumeMany
        , consumeWhile
        , cycle
        , drop
        , dropWhile
        , filter
        , filterMap
        , head
        , indexedMap
        , intersperse
        , iterate
        , map
        , map2
        , map3
        , map4
        , map5
        , partition
        , repeat
        , scanl
        , stream
        , tail
        , take
        , takeWhile
        )

{-| A library for creating lazy, infinite streams. Good for random generators,
number sequences, and anything else that needs to be generated indefinitely.

**Warning:** While using this library, keep in mind that streams are _infinite_,
so if a function never matches a terminating condition, that function will run forever and
cause your program to hang. All functions where this is possible have been marked
with a warning.

@docs Stream


## Creating Streams

@docs stream, cycle, repeat, iterate


## Consuming Streams

@docs consume, consumeMany, consumeWhile


## Sub-streams

@docs head, tail, filter, take, takeWhile, drop, dropWhile


## Combining and Splitting Streams

@docs (<:>), intersperse, partition


## Mapping

@docs map, map2, map3, map4, map5, andMap


## Special Maps and Folds

@docs filterMap, indexedMap, scanl

-}


{-| A lazy, infinite stream type.
-}
type Stream a
    = Cons a (() -> Stream a)


{-| Construct a stream by providing an initial value and a lazy, recursive function
to produce additional values.

To generate an infinite stream of incrementing Ints:

    intStream : Int -> Stream Int
    intStream n =
        stream n (\() -> intStream (n+1))

-}
stream : a -> (() -> Stream a) -> Stream a
stream =
    Cons


{-| Add an element to the front of the stream.

    take 3 (1 <:> repeat 5) == [ 1, 5, 5 ]

-}
(<:>) : a -> Stream a -> Stream a
(<:>) val s =
    stream val (\() -> s)


infixr 5 <:>


{-| Construct a stream by cycling a list endlessly. Returns Nothing if the list is empty.

    Maybe.map (consumeMany 10) (cycle [ 1, 2, 3 ]) == Just ([ 1, 2, 3, 1, 2, 3, 1, 2, 3, 1 ], restOfStream)

-}
cycle : List a -> Maybe (Stream a)
cycle list =
    let
        go x xs =
            let
                newHead =
                    Maybe.withDefault x (List.head xs)
            in
                stream x (\() -> go newHead (List.drop 1 xs ++ [ x ]))

        head =
            List.head list

        tail =
            Maybe.withDefault [] (List.tail list)
    in
        case head of
            Nothing ->
                Nothing

            Just h ->
                Just (go h tail)


{-| Construct a stream by repeating a value endlessly.

    consumeMany 5 (repeat "wow") == ([ "wow", "wow", "wow", "wow", "wow" ], restOfStream)

-}
repeat : a -> Stream a
repeat =
    iterate identity


{-| Construct a stream by repeatedly applying a function to an initial value.

    consumeMany 3 (iterate ((*) 2) 3) == ([ 3, 6, 12 ], restOfStream)

-}
iterate : (a -> a) -> a -> Stream a
iterate f val =
    stream val (\() -> iterate f (f val))


{-| Consume an element of a stream, producing a tuple with the next value and a new stream.
-}
consume : Stream a -> ( a, Stream a )
consume (Cons value next) =
    ( value, next () )


{-| Consume multiple elements of a stream, producing a tuple with a list of values and a new stream.
-}
consumeMany : Int -> Stream a -> ( List a, Stream a )
consumeMany numToConsume stream =
    let
        go n ( acc, currentStream ) =
            if n > 0 then
                let
                    ( value, newStream ) =
                        consume currentStream
                in
                    go (n - 1) ( value :: acc, newStream )

            else
                ( List.reverse acc, currentStream )
    in
        go numToConsume ( [], stream )


{-| Consume elements of a stream while a predicate remains true.

**Warning:** If your predicate never becomes false, this function will never terminate
and your program will hang! (e.g. `consumeWhile ((==) 1) (repeat 1)` will loop forever)

-}
consumeWhile : (a -> Bool) -> Stream a -> ( List a, Stream a )
consumeWhile pred stream =
    let
        go p ( acc, currentStream ) =
            if p (head currentStream) then
                let
                    ( value, newStream ) =
                        consume currentStream
                in
                    go p ( value :: acc, newStream )

            else
                ( List.reverse acc, currentStream )
    in
        go pred ( [], stream )


{-| Take some number of elements from the stream without consuming them.
-}
take : Int -> Stream a -> List a
take n =
    consumeMany n >> Tuple.first


{-| Drop some number of elements from the stream.
-}
drop : Int -> Stream a -> Stream a
drop n =
    consumeMany n >> Tuple.second


{-| Take elements from the stream without consuming them while a predicate remains true.

**Warning:** If your predicate never becomes false, this function will never terminate
and your program will hang! (e.g. `takeWhile ((==) 1) (repeat 1)` will loop forever)

-}
takeWhile : (a -> Bool) -> Stream a -> List a
takeWhile pred =
    consumeWhile pred >> Tuple.first


{-| Drop elements from the stream while a predicate remains true.

**Warning:** If your predicate never becomes false, this function will never terminate
and your program will hang! (e.g. `dropWhile ((==) 1) (repeat 1)` will loop forever)

-}
dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile pred =
    consumeWhile pred >> Tuple.second


{-| Get the value of the current head of the stream.
-}
head : Stream a -> a
head (Cons val _) =
    val


{-| Get the tail of the stream.
-}
tail : Stream a -> Stream a
tail =
    drop 1


{-| Map a function over an infinite stream.
-}
map : (a -> b) -> Stream a -> Stream b
map f (Cons val rest) =
    stream (f val) (\() -> map f (rest ()))


{-| Map a function over two infinite streams.
-}
map2 : (a -> b -> c) -> Stream a -> Stream b -> Stream c
map2 f s1 s2 =
    s1
        |> map f
        |> andMap s2


{-| -}
map3 : (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
map3 f s1 s2 s3 =
    s1
        |> map f
        |> andMap s2
        |> andMap s3


{-| -}
map4 : (a -> b -> c -> d -> e) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e
map4 f s1 s2 s3 s4 =
    s1
        |> map f
        |> andMap s2
        |> andMap s3
        |> andMap s4


{-| You sure have a lot of streams, huh?
-}
map5 : (a -> b -> c -> d -> e -> f) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> Stream f
map5 f s1 s2 s3 s4 s5 =
    s1
        |> map f
        |> andMap s2
        |> andMap s3
        |> andMap s4
        |> andMap s5


{-| Map a stream of functions over a stream of values. Can be used when `mapN` functions aren't enough.

    cycle [ 1, 2, 3 ]
        |> map (\a b c -> a + b + c)
        |> andMap (repeat 4)
        |> andMap (iterate ((*) 2) 5)

is the same as

    map3 (\a b c -> a + b + c)
        (cycle [ 1, 2, 3 ])
        (repeat 4)
        (iterate ((*) 2) 5)

-}
andMap : Stream a -> Stream (a -> b) -> Stream b
andMap (Cons v restV) (Cons f restFn) =
    stream (f v) (\() -> andMap (restV ()) (restFn ()))


{-| Filter an infinite stream based on a predicate.

**Warning:** If your predicate never becomes true, this function will never terminate
and your program will hang! (e.g. `filter ((==) 1) (repeat 0)` will loop forever)

-}
filter : (a -> Bool) -> Stream a -> Stream a
filter pred (Cons val rest) =
    if pred val then
        stream val (\() -> filter pred (rest ()))

    else
        filter pred (rest ())


{-| Places the given value between all elements of the stream.

    take 6 (intersperse "! " (repeat "Wow")) == ["Wow", "! ", "Wow", "! ", "Wow", "! "]

-}
intersperse : a -> Stream a -> Stream a
intersperse v (Cons val rest) =
    stream val
        (\() ->
            stream v (\() -> intersperse v (rest ()))
        )


{-| Partition a stream based on a predicate. The first stream contains all values that satisfy the predicate,
and the second stream contains all the values that do not.

    even n = n % 2 == 0
    (evenStream, oddStream) = partition even (iterate ((+) 1) 0)

    take 5 evenStream == [ 0, 2, 4, 6, 8 ]
    take 5 oddStream == [ 1, 3, 5, 7, 9 ]

**Warning:** If your predicate does not become both true and false at some point, this function will never terminate
and your program will hang! (e.g. `partition ((==) 1) (repeat 0)` and
`partition ((==) 0) (repeat 0)` will both loop forever)

-}
partition : (a -> Bool) -> Stream a -> ( Stream a, Stream a )
partition pred stream =
    (,)
        (filter pred stream)
        (filter (not << pred) stream)


{-| Map a function that may or may not succeed over the stream, only keeping successes.

    take 3 (filterMap List.head (cycle [[ 1, 2 ], [], [ 10, 9 ]])) == [ 1, 10, 1 ]

**Warning:** If your mapping function never returns a `Just` value, this function will never terminate
and your program will hang! (e.g. `filterMap List.head (repeat [])` will loop forever)

-}
filterMap : (a -> Maybe b) -> Stream a -> Stream b
filterMap f (Cons val rest) =
    case f val of
        Nothing ->
            filterMap f (rest ())

        Just v ->
            stream v (\() -> filterMap f (rest ()))


{-| Same as `map` but the function takes the index of the element as its first parameter.

    take 3 (indexedMap (,) (repeat "index")) == [ (0, "index"), (1, "index"), (2, "index") ]

-}
indexedMap : (Int -> a -> b) -> Stream a -> Stream b
indexedMap f =
    map2 f (iterate ((+) 1) 0)


{-| Reduce a stream from the left, building up the intermediate results as a new stream.

    take 5 (scanl (*) 1 (repeat 2)) == [ 1, 2, 4, 8, 16 ]

-}
scanl : (a -> b -> b) -> b -> Stream a -> Stream b
scanl f acc (Cons val rest) =
    stream acc (\() -> scanl f (f val acc) (rest ()))
