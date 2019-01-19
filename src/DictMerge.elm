module DictMerge exposing (mergeErr, mergeDef)

import Dict exposing (Dict, empty, insert, merge)
import Maybe exposing (Maybe, map)

type alias MD comparable a = Maybe (Dict comparable a)

-- Merges two dictionaries, returning Nothing on any clash
mergeErr : Dict comparable a -> Dict comparable a -> MD comparable a
mergeErr a b = merge
    (\k v d -> map (insert k v) d)
    (\k v1 v2 d -> case v1 == v2 of
        True -> map (insert k v1) d
        False -> Nothing)
    (\k v d -> map (insert k v) d)
    a
    b
    (Just empty)

-- Do merge error, unless one has already errored
mergeDef : MD comparable a -> MD comparable a -> MD comparable a
mergeDef a b = case (a,b) of
    (Just a1, Just b1) -> mergeErr a1 b1
    _ -> Nothing
