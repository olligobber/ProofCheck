module Deduction
    ( DeductionRule(..)
    , renderRule
    , toSequent
    ) where

import Prelude ((<>), otherwise)
import Data.String.Common (joinWith)
import Data.Map as M
import Data.Either (Either(..))

import WFF (WFF(..), (-->), (/\), (\/), neg)
import Sequent (Sequent)
import Sequent as Seq
import Symbol (Symbol(..))
import Symbol as Sym

data DeductionRule
    = Assumption
    | ModusPonens
    | ModusTollens
    | DoubleNegation
    | ConditionalProof
    | AndIntroduction
    | AndElimination
    | OrIntroduction
    | OrElimination
    | RAA
    | Definition Symbol
    | Introduction (Sequent Int)

renderRule :: DeductionRule -> String
renderRule Assumption = "A"
renderRule ModusPonens = "MP"
renderRule ModusTollens = "MT"
renderRule DoubleNegation = "DN"
renderRule ConditionalProof = "CP"
renderRule AndIntroduction = "&I"
renderRule AndElimination = "&E"
renderRule OrIntroduction = "|I"
renderRule OrElimination = "|E"
renderRule RAA = "RAA"
renderRule (Definition (UnarySymbol s)) = "Def (" <> s.operator.symbol <> ")"
renderRule (Definition (BinarySymbol s)) = "Def (" <> s.operator.symbol <> ")"
renderRule (Introduction s) = "SI (" <> Seq.render s <> ")"

toSequent :: DeductionRule -> Array (Sequent Int)
toSequent Assumption = [ {ante : [], conse : Prop 1} ]
toSequent ModusPonens = [ {ante : [Prop 1, Prop 1 ==> Prop 2], conse : Prop 2} ]
toSequent ModusTollens =
    [ {ante : [neg $ Prop 2, Prop 1 ==> Prop 2], conse : neg $ Prop 1} ]
toSequent DoubleNegation =
    [ {ante : [Prop 1], conse : neg $ neg $ Prop 1}
    , {ante : [neg $ neg $ Prop 1], conse : Prop 1}
    ]
toSequent ConditionalProof =
    [ {ante : [Prop 1, Prop 2], conse : Prop 1 ==> Prop 2} ]
toSequent AndIntroduction =
    [ {ante : [Prop 1, Prop 2], conse : Prop 1 /\ Prop 2} ]
toSequent AndElimination =
    [ {ante : [Prop 1 /\ Prop 2], conse : Prop 1}
    , {ante : [Prop 1 /\ Prop 2], conse : Prop 2}
    ]
toSequent OrIntroduction =
    [ {ante : [Prop 1], conse : Prop 1 \/ Prop 2}
    , {ente : [Prop 2], conse : Prop 1 \/ Prop 2}
    ]
toSequent OrElimination =
    [
        { ante : [Prop 1, Prop 2, Prop 1 \/ Prop 2, Prop 3, Prop 3]
        , conse : Prop 3
        }
    ]
toSequent RAA =
    [ {ante : [Prop 1, Prop 2 /\ neg (Prop 2)], conse : neg $ Prop 1} ]
toSequent (Definition s)
