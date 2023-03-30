module AppTest.Func.InjectSpec exposing (spec)

import Expect
import Func exposing (inject)
import Test exposing (Test, describe, test)


spec : Test
spec =
    let
        filled : List Int
        filled =
            [ 1, 2, 3, 4, 5 ]

        value : Int
        value =
            10

        testFunc : List Int -> Int -> List Int -> (a -> Expect.Expectation)
        testFunc xs pos expected =
            \_ ->
                Expect.equal (inject xs pos value) expected
    in
    describe "inject adds the value to"
        [ test "middle position" <| testFunc filled 2 [ 1, 2, value, 3, 4, 5 ]
        , test "last of list" <| testFunc filled (List.length filled) (filled ++ [ value ])
        , test "head of list" <| testFunc filled 0 (value :: filled)
        , test "last of list but the position which is over against original list" <| testFunc filled (List.length filled + 1) (filled ++ [ value ])
        ]
