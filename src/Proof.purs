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

import Prelude ((<>), otherwise)
import Data.String.Common (joinWith)
import Data.Map as M
import Data.Either (Either(..))

import WFF (WFF)
import Sequent (Sequent)
import Sequent as Seq
import Symbol (Symbol)
import Symbol as Sym

data Deduction x = Deduction
    { assumptions :: Array Int
    , deduction :: WFF x
    , rule :: DeductionRule
    , reasons :: Array Int
    }

data Proof x = Proof
    { sequents :: Array (Sequent x)
    , symbols :: Array Symbol
    , symbolMap :: SymbolMap
    , lines :: Array (Deduction x)
    , assumptions :: Int
    }

renderReason :: Deduction x -> String
renderReason (Deduction d) =
    renderRule d.rule <> " " <> joinWith "," (show <$> d.reasons)

empty :: Proof x
empty = Proof
    { sequents : []
    , symbols : []
    , symbolMap : defaultMap
    , lines : []
    , assumptions : 0
    }

addSequent :: Sequent x -> Proof x -> Proof x
addSequent s (Proof p) = Proof $ p { sequents = p.sequents <> [s] }

addSymbol :: Symbol -> Proof x -> Either String (Proof x)
addSymbol (UnarySymbol s) (Proof p)
    | M.member s.operator.symbol p.symbolMap =
        Left $ "Symbol " <> s.operator.symbol <> " is already defined"
    | otherwise = Proof $ p
        { symbols = p.symbols <> [UnarySymbol s]
        , symbolMap = M.insert s.operator.symbol $ Left s.operator
        }
addSymbol (BinarySymbol s) (Proof p)
    | M.member s.operator.symbol p.symbolMap =
        Left $ "Symbol " <> s.operator.symbol <> " is already defined"
    | otherwise = Proof $ p
        { symbols = p.symbols <> [BinarySymbol s]
        , symbolMap = M.insert s.operator.symbol $ Right s.operator
        }
