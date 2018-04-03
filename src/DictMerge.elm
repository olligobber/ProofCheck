module DictMerge exposing (mergeErr, mergeDef)

import Dict exposing (Dict, empty, insert, merge)
import Maybe exposing (Maybe, map)

type alias MD comparable a = Maybe (Dict comparable a)

-- Merges two dictionaries, returning Nothing on any clash
mergeErr:(a->a->Bool)->Dict comparable a->Dict comparable a->MD comparable a
mergeErr f a b = merge
    (\k -> \v -> \d -> map (insert k v) d)
    (\k -> \v1 -> \v2 -> \d -> case f v1 v2 of
        True -> map (insert k v1) d
        False -> Nothing)
    (\k -> \v -> \d -> map (insert k v) d)
    a
    b
    (Just empty)

-- Do merge error, unless one has already errored
mergeDef:(a->a->Bool)->MD comparable a->MD comparable a->MD comparable a
mergeDef f a b = case (a,b) of
    (Just a1, Just b1) -> mergeErr f a1 b1
    _ -> Nothing
