module Proof
    ( Deduction(..)
    , Proof(..)
    , empty
    , addSequent
    , addSymbol
    , addDeduction
    , renderReason
    , getAssumptions
    ) where

import Prelude
    ( (<>), (<$>), ($), (>>>), (==), (/=), (+)
    , show, bind, map, pure
    )
import Data.String.Common (joinWith)
import Data.Set (Set)
import Data.Set as Set
import Data.Either (Either(..))
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Array as A
import Data.Traversable (traverse)

import WFF (WFF)
import Sequent (Sequent)
import Symbol (Symbol, SymbolMap)
import Symbol as Sym
import Deduction

data Deduction = Deduction
    { assumptions :: Set Int
    , deduction :: WFF String
    , rule :: DeductionRule
    , reasons :: Set Int
    }

data Proof = Proof
    { sequents :: Array (Sequent String)
    , symbols :: Array Symbol
    , symbolMap :: SymbolMap
    , lines :: Array Deduction
    , assumptions :: Set Int
    }

renderReason :: Deduction -> String
renderReason (Deduction d) =
    renderRule d.rule
    <> " "
    <> joinWith "," (show <$> Set.toUnfoldable d.reasons)

empty :: Proof
empty = Proof
    { sequents : []
    , symbols : []
    , symbolMap : Sym.defaultMap
    , lines : []
    , assumptions : Set.empty
    }

addSequent :: Sequent String -> Proof -> Proof
addSequent s (Proof p) = Proof $ p { sequents = p.sequents <> [s] }

addSymbol :: Symbol -> Proof -> Either String Proof
addSymbol s (Proof p) = do
    newMap <- Sym.updateMap p.symbolMap s
    pure $ Proof $ p
        { symbols = p.symbols <> [s]
        , symbolMap = newMap
        }

pack :: Deduction ->
    {formula :: WFF String, isAssumption :: Boolean, assumptions :: Set Int}
pack (Deduction d) =
    { formula : d.deduction
    , isAssumption : isAssumption d.rule
    , assumptions : d.assumptions
    }

addDeduction :: Deduction -> Proof -> Either String Proof
addDeduction (Deduction d) (Proof p) = do
    antes <- E.note "Invalid line number"
        $ traverse (A.index p.lines >>> map pack)
        $ Set.toUnfoldable d.reasons
    assumptions <- matchDeduction antes d.deduction d.rule
    case assumptions of
        Just x | x == d.assumptions -> Right $ Proof $
            p { lines = p.lines <> [Deduction d] }
        Just _ -> Left "Incorrect assumptions"
        _ | Set.size d.assumptions /= 1 -> Left "Incorrect assumptions"
        _ | d.assumptions `Set.subset` p.assumptions ->
            Left "Assumption number already in use"
        _ -> Right $ Proof $ p
            { lines = p.lines <> [Deduction d]
            , assumptions = p.assumptions `Set.union` d.assumptions
            }

getNextUnused :: Set Int -> Int
getNextUnused s = case Set.findMin $ plus `Set.difference` s of
    Nothing -> 1
    Just m -> m
    where
        plus = Set.insert 1 $ Set.map (_ + 1) s

getAssumptions :: Deduction -> Proof -> Either String (Set Int)
getAssumptions (Deduction d) (Proof p) = do
    antes <- E.note "Invalid line number"
        $ traverse (A.index p.lines >>> map pack)
        $ Set.toUnfoldable d.reasons
    assumptions <- matchDeduction antes d.deduction d.rule
    case assumptions of
        Just s -> pure s
        Nothing -> pure $ Set.singleton $ getNextUnused p.assumptions
