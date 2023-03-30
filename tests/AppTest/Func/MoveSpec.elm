module AppTest.Func.MoveSpec exposing (spec)

import Expect
import Func exposing (move)
import Test exposing (Test, describe, test)


spec : Test
spec =
    let
        testdata : List String
        testdata =
            [ "a", "b", "c", "d" ]

        lastIndex =
            List.length testdata - 1

        testFunc : Int -> Int -> List String -> (a -> Expect.Expectation)
        testFunc from to expected =
            \_ ->
                Expect.equal (move from to testdata) expected
    in
    describe "move"
        [ describe "to right"
            [ test "middle from edge" <| testFunc 0 2 [ "b", "c", "a", "d" ]
            , test "edge from middle" <| testFunc 1 lastIndex [ "a", "c", "d", "b" ]
            , test "edge from edge" <| testFunc 0 lastIndex [ "b", "c", "d", "a" ]
            , test "middle from another middle" <| testFunc 1 2 [ "a", "c", "b", "d" ]
            ]
        , describe "to left"
            [ test "middle from edge" <| testFunc lastIndex 1 [ "a", "d", "b", "c" ]
            , test "edge from middle" <| testFunc 1 0 [ "b", "a", "c", "d" ]
            , test "edge from edge" <| testFunc lastIndex 0 [ "d", "a", "b", "c" ]
            , test "middle from another middle" <| testFunc 2 1 [ "a", "c", "b", "d" ]
            ]
        , test "to same position" <| testFunc 1 1 testdata
        ]
