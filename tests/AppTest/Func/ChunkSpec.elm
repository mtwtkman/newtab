module AppTest.Func.ChunkSpec exposing (spec)

import Expect
import Func exposing (chunk)
import Test exposing (Test, describe, test)


spec : Test
spec =
    let
        filled : List Int
        filled =
            [ 1, 2, 3, 4, 5 ]

        empty : List Int
        empty = []

        testFunc : Int -> List Int -> List (List Int) -> (a -> Expect.Expectation)
        testFunc size data expected =
            \_ ->
                Expect.equal (chunk data size) expected
    in
    describe "chunk"
        [ test "divides by odd size" <| testFunc 3 filled [ [ 1, 2, 3 ], [ 4, 5 ] ]
        , test "divides by just size" <| testFunc (List.length filled) filled [filled]
        , test "retuns empty against empty list" <| testFunc 3 empty []
        ]
