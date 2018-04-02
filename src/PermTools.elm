module PermTools exposing (permutations, assignments)

import Dict exposing (Dict, empty, insert)
import Set exposing (Set, foldl)
import List exposing (concatMap, map)

-- Borrowed from https://github.com/elm-community/list-extra
select : List a -> List ( a, List a )
select xs =case xs of
    [] -> []
    x :: xs -> ( x, xs ) :: map (\( y, ys ) -> ( y, x :: ys )) (select xs)

-- Borrowed from https://github.com/elm-community/list-extra
permutations : List a -> List (List a)
permutations xs_ = case xs_ of
    [] -> [ [] ]
    xs ->
        let
            f ( y, ys ) = map ((::) y) (permutations ys)
        in
            concatMap f (select xs)

-- Gets every truth value assignment
assignments : Set comparable -> List (Dict comparable Bool)
assignments = foldl
    (\k -> \list -> concatMap
        (\assign -> [insert k True assign, insert k False assign]) list)
    [empty]
