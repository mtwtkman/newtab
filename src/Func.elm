module Func exposing (chunk, flip, inject, move)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


move : Int -> Int -> List a -> List a
move from to xs =
    if from == to then
        xs

    else if from < to then
        let
            former =
                List.take from xs

            target =
                List.drop from xs |> List.take 1

            middle =
                List.drop (from + 1) xs |> List.take (to - from)

            latter =
                List.drop (to + 1) xs
        in
        former ++ middle ++ target ++ latter

    else
        let
            former =
                List.take to xs

            target =
                List.drop from xs |> List.take 1

            middle =
                List.drop to xs |> List.take (from - to)

            latter =
                List.drop (from + 1) xs
        in
        former ++ target ++ middle ++ latter


inject : List a -> Int -> a -> List a
inject xs pos x =
    List.take pos xs ++ x :: List.drop pos xs


chunk : List a -> Int -> List (List a)
chunk xs size =
    if List.isEmpty xs then
        []

    else
        List.take size xs :: chunk (List.drop size xs) size
