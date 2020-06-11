module Deduction
    ( DeductionRule(..)
    , isAssumption
    , renderRule
    , matchDeduction
    ) where

import Prelude
    ( (<>), (<$>), ($)
    , map, bind, pure, discard, not
    , class Eq, class Ord
    )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as A
import Control.MonadZero (guard)

import WFF (WFF(..), (==>), (/\), (\/), neg)
import Sequent (Sequent(..))
import Sequent as Seq
import Symbol (CustomSymbol)
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
    | Definition CustomSymbol Int
    | Introduction (Sequent String) Int

derive instance eqDeductionRule :: Eq DeductionRule
derive instance ordDeductionRule :: Ord DeductionRule

isAssumption :: DeductionRule -> Boolean
isAssumption Assumption = true
isAssumption _ = false

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
renderRule (Definition s _) = "Def (" <> Sym.getDisplay (Sym.Custom s) <> ")"
renderRule (Introduction s _) = "SI (" <> Seq.render s <> ")"

toSequents :: DeductionRule -> Array (Sequent (Either Int String))
toSequents Assumption = [ Left <$> Sequent {ante : [], conse : Prop 1} ]
toSequents ModusPonens =
    [ Left <$> Sequent {ante : [Prop 1, Prop 1 ==> Prop 2], conse : Prop 2} ]
toSequents ModusTollens = map Left <$>
    [ Sequent {ante : [neg $ Prop 2, Prop 1 ==> Prop 2], conse : neg $ Prop 1} ]
toSequents DoubleNegation =
    [ Left <$> Sequent {ante : [Prop 1], conse : neg $ neg $ Prop 1}
    , Left <$> Sequent {ante : [neg $ neg $ Prop 1], conse : Prop 1}
    ]
toSequents ConditionalProof =
    [ Left <$> Sequent {ante : [Prop 1, Prop 2], conse : Prop 1 ==> Prop 2} ]
toSequents AndIntroduction =
    [ Left <$> Sequent {ante : [Prop 1, Prop 2], conse : Prop 1 /\ Prop 2} ]
toSequents AndElimination =
    [ Left <$> Sequent {ante : [Prop 1 /\ Prop 2], conse : Prop 1}
    , Left <$> Sequent {ante : [Prop 1 /\ Prop 2], conse : Prop 2}
    ]
toSequents OrIntroduction =
    [ Left <$> Sequent {ante : [Prop 1], conse : Prop 1 \/ Prop 2}
    , Left <$> Sequent {ante : [Prop 2], conse : Prop 1 \/ Prop 2}
    ]
toSequents OrElimination =
    [ Left <$> Sequent
        { ante : [Prop 1, Prop 2, Prop 1 \/ Prop 2, Prop 3, Prop 3]
        , conse : Prop 3
        }
    ]
toSequents RAA = map Left <$>
    [ Sequent {ante : [Prop 1, Prop 2 /\ neg (Prop 2)], conse : neg $ Prop 1} ]
toSequents (Definition s _) = map Left <$> Sym.toSequents s
toSequents (Introduction s _) = [ Right <$> s ]

{-
    Check a deduction was correctly applied, given
        - the list of referenced formulas, whether they were assumptions, and
            what assumptions they rely on
        - the conclusion
        - the deduction rule
    returns either an error or the assumptions the conclusion relies on,
    using Nothing as a flag that this is a new assumption
-}
matchDeduction :: Array
    {formula :: WFF String, isAssumption :: Boolean, assumptions :: Set Int} ->
    WFF String -> DeductionRule -> Either String (Maybe (Set Int))
matchDeduction a conse d@Assumption =
    case A.head $ do
        s <- toSequents d
        Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
    of
        Nothing -> Left "Invalid use of deduction rule"
        Just _ -> Right Nothing
matchDeduction a conse d@ConditionalProof =
    case A.head $ do
        s <- toSequents d
        perm <- _.permutation <$> Seq.match a s
            (Sequent {ante : _.formula <$> a, conse})
        assumption <- A.fromFoldable $ A.index perm 0
        conclusion <- A.fromFoldable $ A.index perm 1
        guard $ assumption.isAssumption
        guard $ assumption.assumptions `Set.subset` conclusion.assumptions
        pure $ conclusion.assumptions `Set.difference` assumption.assumptions
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a conse d@OrElimination =
    case A.head $ do
        s <- toSequents d
        perm <- _.permutation <$> Seq.match a s
            (Sequent {ante : _.formula <$> a, conse})
        firstA <- A.fromFoldable $ A.index perm 0
        secondA <- A.fromFoldable $ A.index perm 1
        orA <- A.fromFoldable $ A.index perm 2
        firstC <- A.fromFoldable $ A.index perm 3
        secondC <- A.fromFoldable $ A.index perm 4
        guard $ firstA.isAssumption
        guard $ secondA.isAssumption
        guard $ firstA.assumptions `Set.subset` firstC.assumptions
        guard $ secondA.assumptions `Set.subset` secondC.assumptions
        guard $ not $ firstA.assumptions `Set.subset` secondC.assumptions
        guard $ not $ firstA.assumptions `Set.subset` orA.assumptions
        guard $ not $ secondA.assumptions `Set.subset` firstC.assumptions
        guard $ not $ secondA.assumptions `Set.subset` orA.assumptions
        pure $
            Set.unions (_.assumptions <$> [orA, firstC, secondC])
            `Set.difference` Set.unions (_.assumptions <$> [firstA, secondA])
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a conse d@RAA =
    case A.head $ do
        s <- toSequents d
        perm <- _.permutation <$> Seq.match a s
            (Sequent {ante : _.formula <$> a, conse})
        assumption <- A.fromFoldable $ A.index perm 0
        contradiction <- A.fromFoldable $ A.index perm 1
        guard $ assumption.isAssumption
        guard $ assumption.assumptions `Set.subset` contradiction.assumptions
        pure $
            contradiction.assumptions `Set.difference` assumption.assumptions
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a conse d =
    case do
        s <- toSequents d
        Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
    of
        [] -> Left "Invalid use of deduction rule"
        _ -> Right $ Just $ Set.unions $_.assumptions <$> a
