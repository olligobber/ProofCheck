module Deduction
    ( DeductionRule(..)
    , isAssumption
    , renderRule
    , matchDeduction
    ) where

import Prelude
    ( (<>), (<$>), ($)
    , bind, pure, discard, not
    , class Eq, class Ord
    )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as A
import Control.MonadZero (guard)

import WFF (WFF, (==>), (/\), (\/), neg, prop)
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
    | Introduction (Sequent String String String) Int

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
renderRule AndIntroduction = "∧I"
renderRule AndElimination = "∧E"
renderRule OrIntroduction = "∨I"
renderRule OrElimination = "∨E"
renderRule RAA = "RAA"
renderRule (Definition s _) =
    "Def (" <> Sym.getDisplay (Sym.getOperator $ Sym.Custom s) <> ")"
renderRule (Introduction s _) = "SI (" <> Seq.render s <> ")"

toSequents :: DeductionRule -> Array (Sequent String String String)
toSequents Assumption = [ Sequent {ante : [], conse : prop "A"} ]
toSequents ModusPonens =
    [ Sequent {ante : [prop "A", prop "A" ==> prop "B"], conse : prop "B"}]
toSequents ModusTollens =
    [ Sequent
        { ante : [neg $ prop "B", prop "A" ==> prop "B"]
        , conse : neg $ prop "A"
        }
    ]
toSequents DoubleNegation =
    [ Sequent {ante : [prop "A"], conse : neg $ neg $ prop "A"}
    , Sequent {ante : [neg $ neg $ prop "A"], conse : prop "A"}
    ]
toSequents ConditionalProof =
    [ Sequent {ante : [prop "A", prop "B"], conse : prop "A" ==> prop "B"} ]
toSequents AndIntroduction =
    [ Sequent {ante : [prop "A", prop "B"], conse : prop "A" /\ prop "B"} ]
toSequents AndElimination =
    [ Sequent {ante : [prop "A" /\ prop "B"], conse : prop "A"}
    , Sequent {ante : [prop "A" /\ prop "B"], conse : prop "B"}
    ]
toSequents OrIntroduction =
    [ Sequent {ante : [prop "A"], conse : prop "A" \/ prop "B"}
    , Sequent {ante : [prop "B"], conse : prop "A" \/ prop "B"}
    ]
toSequents OrElimination =
    [ Sequent
        { ante : [prop "A", prop "B", prop "A" \/ prop "B", prop "C", prop "C"]
        , conse : prop "C"
        }
    ]
toSequents RAA =
    [ Sequent
        { ante : [prop "A", prop "B" /\ neg (prop "B")]
        , conse : neg $ prop "A"
        }
    ]
toSequents (Definition s _) = Sym.toSequents s
toSequents (Introduction s _) = [ s ]

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
    { formula :: WFF String String String
    , isAssumption :: Boolean
    , assumptions :: Set Int
    } ->
    WFF String String String -> DeductionRule -> Either String (Maybe (Set Int))
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
        perm <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
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
        perm <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
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
        perm <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
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
