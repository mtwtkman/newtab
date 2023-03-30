module Func exposing (chunk, flip, inject)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


inject : List a -> Int -> a -> List a
inject xs pos x =
    List.take pos xs ++ x :: List.drop pos xs


chunk : List a -> Int -> List (List a)
chunk xs size =
    if List.isEmpty xs then
        []

    else
        List.take size xs :: chunk (List.drop size xs) size
