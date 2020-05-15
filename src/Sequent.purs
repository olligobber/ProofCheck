module Sequent
    ( Sequent
    , render
    , verify
    ) where

import Prelude
    ( class Ord, class Functor
    , (<>), (<$>), (<*>), ($), (>>=)
    , map, flip)
import Data.Foldable
    (class Foldable, foldMap, foldlDefault, foldrDefault, foldl, all)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.String.Common (joinWith)
import Data.Set as S
import Data.Array as A

import WFF (WFF)
import WFF as WFF

data Sequent a = Sequent
    { ante :: Array (WFF a)
    , conse :: WFF a
    }

instance functorSequent :: Functor Sequent where
    map f (Sequent s) = Sequent
        { ante : map f <$> s.ante, conse : f <$> s.conse }

instance foldableSequent :: Foldable Sequent where
    foldMap f (Sequent s) = foldMap (foldMap f) s.ante <> foldMap f s.conse
    foldl f x y = foldlDefault f x y
    foldr f x y = foldrDefault f x y

instance traversableSequent :: Traversable Sequent where
    sequence (Sequent s) = Sequent <$> (
        {ante : _, conse : _}
        <$> sequence (sequence <$> s.ante)
        <*> sequence s.conse)
    traverse = traverseDefault

render :: Sequent String -> String
render (Sequent s) =
    joinWith "," (WFF.render <$> s.ante) <> " ⊢ " <> WFF.render s.conse

verify :: forall p. Ord p => Sequent p -> Boolean
verify (Sequent s) = all (flip sat s.conse) satAnte where
    variables = S.fromFoldable $ Sequent s
    -- assignments = powerset of variables
    assignments = foldl
        (\list k -> list >>= \a -> [a, S.insert k a])
        [S.empty]
        variables
    sat a w = WFF.eval $ flip S.member a <$> w
    satAnte = A.filter (\a -> all (sat a) s.ante) assignments
