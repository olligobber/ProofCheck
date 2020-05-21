module Proof
    ( Deduction
    , Proof
    , empty
    , addSequent
    , addSymbol
    , addDeduction
    , renderReason
    , addAll
    ) where

import Prelude
    ( class Ord
    , (<>), (<$>), ($), (>>>), (==), (/=)
    , show, otherwise, bind, map, flip
    )
import Data.String.Common (joinWith)
import Data.Map as M
import Data.Set (Set)
import Data.Set as Set
import Data.Either (Either(..))
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Array as A
import Data.Traversable (traverse)
import Data.Foldable (foldM, foldl)

import WFF (WFF)
import Sequent (Sequent)
import Symbol (Symbol(..), SymbolMap)
import Symbol as Sym
import Deduction

data Deduction x = Deduction
    { assumptions :: Set Int
    , deduction :: WFF x
    , rule :: DeductionRule x
    , reasons :: Set Int
    }

data Proof x = Proof
    { sequents :: Array (Sequent x)
    , symbols :: Array Symbol
    , symbolMap :: SymbolMap
    , lines :: Array (Deduction x)
    , assumptions :: Set Int
    }

renderReason :: Deduction String -> String
renderReason (Deduction d) =
    renderRule d.rule
    <> " "
    <> joinWith "," (show <$> Set.toUnfoldable d.reasons)

empty :: forall x. Proof x
empty = Proof
    { sequents : []
    , symbols : []
    , symbolMap : Sym.defaultMap
    , lines : []
    , assumptions : Set.empty
    }

addSequent :: forall x. Sequent x -> Proof x -> Proof x
addSequent s (Proof p) = Proof $ p { sequents = p.sequents <> [s] }

addSymbol :: forall x. Symbol -> Proof x -> Either String (Proof x)
addSymbol (UnarySymbol s) (Proof p)
    | M.member s.operator.symbol p.symbolMap =
        Left $ "Symbol " <> s.operator.symbol <> " is already defined"
    | otherwise = Right $ Proof $ p
        { symbols = p.symbols <> [UnarySymbol s]
        , symbolMap = M.insert s.operator.symbol (Left s.operator) p.symbolMap
        }
addSymbol (BinarySymbol s) (Proof p)
    | M.member s.operator.symbol p.symbolMap =
        Left $ "Symbol " <> s.operator.symbol <> " is already defined"
    | otherwise = Right $ Proof $ p
        { symbols = p.symbols <> [BinarySymbol s]
        , symbolMap = M.insert s.operator.symbol (Right s.operator) p.symbolMap
        }

pack :: forall x. Deduction x ->
    {formula :: WFF x, isAssumption :: Boolean, assumptions :: Set Int}
pack (Deduction d) =
    { formula : d.deduction
    , isAssumption : isAssumption d.rule
    , assumptions : d.assumptions
    }

addDeduction :: forall x. Ord x =>
    Deduction x -> Proof x -> Either String (Proof x)
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

addAll :: forall x. Ord x => Array Symbol -> Array (Sequent x) ->
    Array (Deduction x) -> Either String (Proof x)
addAll symbols sequents deductions = do
    addedSymbols <- foldM (flip addSymbol) empty symbols
    let addedSequents = foldl (flip addSequent) addedSymbols sequents
    foldM (flip addDeduction) addedSequents deductions
