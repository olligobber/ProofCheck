module Sequent
    ( Sequent(..)
    , render
    , verify
    , match
    ) where

import Prelude
    ( class Eq, class Ord, class Functor
    , (<>), (<$>), (<*>), ($), (>>=), (==)
    , map, flip, bind, pure, otherwise)
import Data.Array as A
import Data.Foldable
    ( class Foldable
    , foldMap, foldlDefault, foldrDefault, foldl, all, fold)
import Data.Set as S
import Data.String.Common (joinWith)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple (Tuple(..))
import Data.Map (Map)

import WFF (WFF)
import WFF as WFF
import Mapping (Mapping(..))

data Sequent a = Sequent
    { ante :: Array (WFF a)
    , conse :: WFF a
    }

derive instance eqSequent :: Eq a => Eq (Sequent a)
derive instance ordSequent :: Ord a => Ord (Sequent a)

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

-- Match a sequent to one after substitutions were applied
match1 :: forall a b. Ord a => Eq b =>
    Sequent a -> Sequent b -> Mapping a (WFF b)
match1 (Sequent small) (Sequent big) =
    fold (A.zipWith WFF.match small.ante big.ante)
    <> WFF.match small.conse big.conse

permute :: forall a. Eq a => Array a -> Array (Array a)
permute [] = [[]]
permute l = do
    h <- l
    t <- permute $ A.delete h l
    pure $ A.cons h t

-- Match a sequent to one after substitutions were applied and the antecedents
-- permuted, returning all possible substitutions and inverse permutations
match :: forall a b i. Ord a => Eq b => Eq i => Array i -> Sequent a ->
    Sequent b -> Array {permutation :: Array i, substitution :: Map a (WFF b)}
match indices (Sequent small) (Sequent big)
    | A.length small.ante == A.length big.ante = do
        let indexedAnte = A.zip indices big.ante
        Tuple permutation bigantes <- A.unzip <$> permute indexedAnte
        case match1 (Sequent small) $ Sequent $ big { ante = bigantes } of
            Mapping substitution -> [{permutation, substitution}]
            None -> []
    | otherwise = []
