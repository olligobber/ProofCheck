module Proof
    ( Deduction(..)
    , Proof(..)
    , empty
    , isEmpty
    , conclusion
    , addDeduction
    , renderReason
    , getAssumptions
    ) where

import Prelude
    ( (<>), (<$>), ($), (>>>), (==), (/=), (+), (-)
    , show, bind, map, pure, mempty, not, flip, discard, unit
    , class Eq, class Ord, class Monoid
    )
import Data.String.Common (joinWith)
import Data.Set (Set)
import Data.Set as Set
import Data.Either (Either(..))
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Array as A
import Data.Traversable (traverse)
import Data.Foldable (length)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))

import WFF (WFF, validateBindings)
import Sequent (Sequent(..))
import Deduce (class Deduce, matchDeduction, isAssumption, renderRule)
import Typing (class Typing, getTyping, validate)

data Deduction r w = Deduction
    { assumptions :: Set Int
    , deduction :: w
    , rule :: r
    , reasons :: Array Int
    }

data Proof r w t = Proof
    { lines :: Array (Deduction r w)
    , assumptions :: Map Int w
    , types :: t
    }

renderReason :: forall r w. Deduce r w => Deduction r w -> String
renderReason (Deduction d) =
    renderRule d.rule
    <> " "
    <> joinWith "," (show <$> d.reasons)

empty :: forall r w t. Monoid t => Proof r w t
empty = Proof
    { lines : []
    , assumptions : M.empty
    , types : mempty
    }

isEmpty :: forall r w t. Proof r w t -> Boolean
isEmpty (Proof p) = length p.lines == 0

conclusion :: forall r t. Proof r (WFF String String String) t -> Maybe (Sequent String String String)
conclusion (Proof p) = case A.last p.lines of
    Just (Deduction d) -> case
        traverse (flip M.lookup p.assumptions) $ A.fromFoldable d.assumptions
        of
            Just ante -> Just $ Sequent { ante, conse : d.deduction }
            Nothing -> Nothing
    Nothing -> Nothing

pack :: forall r w. Deduce r w => Deduction r w ->
    { formula :: w
    , isAssumption :: Boolean
    , assumptions :: Set Int
    }
pack (Deduction d) =
    { formula : d.deduction
    , isAssumption : isAssumption d.rule
    , assumptions : d.assumptions
    }

addDeduction :: forall r w t. Ord w => Deduce r w => Typing w t => 
    Deduction r w -> Proof r w t -> Either String (Proof r w t)
addDeduction (Deduction d) (Proof p) = do
    case validateBindings d.deduction of
        Just e -> Left e
        Nothing -> Right unit
    antes <- E.note "Invalid line number in reason"
        $ traverse ((_ - 1) >>> A.index p.lines >>> map pack) d.reasons
    assumptions <- matchDeduction antes p.assumptions d.deduction d.rule
    let newTypes = p.types <> getTyping d.deduction
    case assumptions of
        _ | not (validate newTypes) -> Left "Invalid types"
        Just x | x == d.assumptions  -> Right $ Proof $
            p { lines = p.lines <> [Deduction d], types = newTypes }
        Just _ -> Left "Incorrect assumptions"
        _ | Set.size d.assumptions /= 1 -> Left "Wrong number of assumptions"
        _ | d.assumptions `Set.subset` M.keys p.assumptions ->
            Left "Assumption number already in use"
        _ -> Right $ Proof $
            { lines : p.lines <> [Deduction d]
            , assumptions : p.assumptions `M.union` M.fromFoldable
                (Set.map (\x -> Tuple x $ d.deduction) d.assumptions)
            , types : newTypes
            }

getNextUnused :: Set Int -> Int
getNextUnused s = case Set.findMin $ plus `Set.difference` s of
    Nothing -> 1
    Just m -> m
    where
        plus = Set.insert 1 $ Set.map (_ + 1) s

getAssumptions :: forall r t x y z. Eq z => Deduce r (WFF x y z) => 
    Deduction r (WFF x y z) -> Proof r (WFF x y z) t -> Either String (Set Int)
getAssumptions (Deduction d) (Proof p) = do
    case validateBindings d.deduction of
        Just e -> Left e
        Nothing -> Right unit
    antes <- E.note "Invalid line number in reason"
        $ traverse ((_ - 1) >>> A.index p.lines >>> map pack) d.reasons
    assumptions <- matchDeduction antes p.assumptions d.deduction d.rule
    case assumptions of
        Just s -> pure s
        Nothing -> pure $ Set.singleton $ getNextUnused $ M.keys p.assumptions
