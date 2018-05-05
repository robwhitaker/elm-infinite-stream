module OverflowTests exposing (main)

import Html exposing (Html)
import InfiniteStream exposing (..)
import Task


type alias Model =
    { maxStacks : Int }


type Msg
    = Start


stacksToRun : Int -> Int
stacksToRun n =
    max 500000 (n * 5)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                runTest ( s, f ) =
                    let
                        a =
                            Debug.log (s ++ ">>> Output") (f <| stacksToRun model.maxStacks)
                    in
                        ()

                running =
                    List.map runTest tests
            in
                (,)
                    model
                    Cmd.none


view : Model -> Html Msg
view model =
    let
        stacks =
            stacksToRun model.maxStacks

        nTimes =
            toString <| toFloat (round (toFloat stacks * 10 / toFloat model.maxStacks)) / 10
    in
        Html.div [] <|
            [ Html.h1 [] [ Html.text "Overflow Tests" ]
            ]
                ++ List.map (\msg -> Html.p [] [ Html.text msg ])
                    [ "Max stacks is: " ++ toString model.maxStacks ++ ". All tests ran for " ++ nTimes ++ "X that: " ++ toString stacks ++ "."
                    , "View the console for output."
                    ]


main : Program Int Model Msg
main =
    Html.programWithFlags
        { init =
            \maxStacks ->
                (,)
                    { maxStacks = maxStacks }
                    (Task.succeed Start |> Task.perform identity)
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



{- Some functions have been excluded here as they are either implicitly
   tested or don't do any deep recursion. These include:

   stream
   consume
   consumeMany
   head
   tail
   (<:>)
-}


tests : List ( String, Int -> String )
tests =
    [ ( "cycle"
      , \n ->
            let
                s =
                    cycle [ 1, 2, 3, 4, 5 ]

                run =
                    Maybe.map (consumeMany n) s
            in
                toString run
      )
    , ( "repeat"
      , \n ->
            let
                s =
                    repeat 1

                run =
                    consumeMany n s
            in
                toString run
      )
    , ( "iterate"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    consumeMany n s
            in
                toString run
      )
    , ( "consumeWhile"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    consumeWhile (\num -> num < n) s
            in
                toString run
      )
    , ( "filter"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    consumeMany n <|
                        filter (\num -> num % 2 == 0) s
            in
                toString run
      )
    , ( "take"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    take n s
            in
                toString run
      )
    , ( "drop"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    drop n s
            in
                toString run
      )
    , ( "takeWhile"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    takeWhile (\num -> num < n) s
            in
                toString run
      )
    , ( "dropWhile"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    dropWhile (\num -> num < n) s
            in
                toString run
      )
    , ( "intersperse"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    consumeMany n <|
                        intersperse 10 s
            in
                toString run
      )
    , ( "partition"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    partition (\num -> num % 2 == 0) s
                        |> (\( evens, odds ) ->
                                ( consumeMany n evens, consumeMany n odds )
                           )
            in
                toString run
      )
    , ( "map"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                run =
                    map ((*) 10) s
                        |> consumeMany n
            in
                toString run
      )
    , ( "andMap"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                s2 =
                    repeat 17

                run =
                    map (*) s
                        |> andMap s2
                        |> consumeMany n
            in
                toString run
      )
    , ( "map2"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                s2 =
                    repeat 17

                run =
                    map2 (*) s s2
                        |> consumeMany n
            in
                toString run
      )
    , ( "map3"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                s2 =
                    repeat 17

                s3 =
                    repeat 22

                fn =
                    \a b c -> a * b * c

                run =
                    map3 fn s s2 s3
                        |> consumeMany n
            in
                toString run
      )
    , ( "map4"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                s2 =
                    repeat 17

                s3 =
                    repeat 22

                s4 =
                    repeat 5

                fn =
                    \a b c d -> a * b * c + d

                run =
                    map4 fn s s2 s3 s4
                        |> consumeMany n
            in
                toString run
      )
    , ( "map5"
      , \n ->
            let
                s =
                    iterate ((+) 1) 0

                s2 =
                    repeat 17

                s3 =
                    repeat 22

                s4 =
                    repeat 5

                s5 =
                    repeat 6

                fn =
                    \a b c d e -> a * b * c + d / e

                run =
                    map5 fn s s2 s3 s4 s5
                        |> consumeMany n
            in
                toString run
      )
    , ( "filterMap"
      , \n ->
            let
                s =
                    cycle [ [], [ 1, 2, 3 ], [ 5, 43 ], [], [] ]

                run =
                    Maybe.map
                        (consumeMany n << filterMap List.head)
                        s
            in
                toString run
      )
    , ( "indexedMap"
      , \n ->
            let
                s =
                    repeat "wow!"

                run =
                    indexedMap (,) s
                        |> consumeMany n
            in
                toString run
      )
    , ( "scanl"
      , \n ->
            let
                s =
                    repeat 1

                run =
                    scanl (+) 0 s
                        |> consumeMany n
            in
                toString run
      )
    , ( "let's mix it up"
      , \n ->
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
                        |> consumeMany n
            in
                toString run
      )
    ]
