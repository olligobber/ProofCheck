module Deduction
    ( DeductionRule(..)
    , isAssumption
    , renderRule
    , matchDeduction
    ) where

import Prelude
    ( (<>), (<$>), ($), (<<<)
    , bind, pure, discard, flip, not, map
    , class Eq, class Ord
    )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as A
import Control.MonadZero (guard)
import Data.Map (Map)
import Data.Map as M
import Data.Foldable (foldMap, fold)

import WFF
    ( WFF, Variable(Free)
    , (==>), (/\), (\/)
    , neg, prop, foralv, exists, pred, freeVars
    )
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
    | UniversalIntroduction
    | UniversalElimination
    | ExistentialIntroduction
    | ExistentialElimination
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
renderRule UniversalIntroduction = "∀I"
renderRule UniversalElimination = "∀E"
renderRule ExistentialIntroduction = "∃I"
renderRule ExistentialElimination = "∃E"
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
toSequents UniversalIntroduction =
    [ Sequent
        { ante : [ pred "F" [Free "x"] ]
        , conse : foralv "x" $ pred "F" [Free "x"]
        }
    ]
toSequents UniversalElimination =
    [ Sequent
        { ante : [foralv "x" $ pred "F" [Free "x"] ]
        , conse : pred "F" [Free "x"]
        }
    ]
toSequents ExistentialIntroduction =
    [ Sequent
        { ante : [ pred "F" [Free "x"] ]
        , conse : exists "x" $ pred "F" [Free "x"]
        }
    ]
toSequents ExistentialElimination =
    [ Sequent
        { ante :
            [ exists "x" $ pred "F" [Free "x"]
            , pred "F" [Free "x"]
            , prop "P"
            ]
        , conse : prop "P"
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
        - the set of free variables in the assumptions
        - the conclusion
        - the deduction rule
    returns either an error or the assumptions the conclusion relies on,
    using Nothing as a flag that this is a new assumption
-}
matchDeduction :: Array
    { formula :: WFF String String String
    , isAssumption :: Boolean
    , assumptions :: Set Int
    } -> Map Int (WFF String String String) -> WFF String String String ->
    DeductionRule -> Either String (Maybe (Set Int))
matchDeduction a _ conse d@Assumption =
    case A.head $ do
        s <- toSequents d
        Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
    of
        Nothing -> Left "Invalid use of deduction rule"
        Just _ -> Right Nothing
matchDeduction a _ conse d@ConditionalProof =
    case A.head $ do
        s <- toSequents d
        {perm, sub} <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
        assumption <- A.fromFoldable $ A.index perm 0
        conclusion <- A.fromFoldable $ A.index perm 1
        guard $ assumption.isAssumption
        guard $ assumption.assumptions `Set.subset` conclusion.assumptions
        pure $ conclusion.assumptions `Set.difference` assumption.assumptions
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a _ conse d@OrElimination =
    case A.head $ do
        s <- toSequents d
        {perm, sub} <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
        firstA <- A.fromFoldable $ A.index perm 0
        secondA <- A.fromFoldable $ A.index perm 1
        orA <- A.fromFoldable $ A.index perm 2
        firstC <- A.fromFoldable $ A.index perm 3
        secondC <- A.fromFoldable $ A.index perm 4
        guard $ firstA.isAssumption
        guard $ secondA.isAssumption
        guard $ firstA.assumptions `Set.subset` firstC.assumptions
        guard $ secondA.assumptions `Set.subset` secondC.assumptions
        pure $
            Set.unions
                [ orA.assumptions
                , firstC.assumptions `Set.difference` firstA.assumptions
                , secondC.assumptions `Set.difference` secondA.assumptions
                ]
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a m conse d@UniversalIntroduction =
    case A.head $ do
        s <- toSequents d
        {perm, sub} <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
        let assumptions = foldMap _.assumptions a
        let assFreeVars = fold $
                Set.mapMaybe (map freeVars <<< flip M.lookup m) assumptions
        case M.lookup "x" sub.freeMatch of
            Nothing -> []
            Just (Left _) -> pure assumptions
            Just (Right x) -> do
                guard $ not $ x `Set.member` assFreeVars
                pure assumptions
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a m conse d@ExistentialElimination =
    case A.head $ do
        s <- toSequents d
        {perm, sub} <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
        existential <- A.fromFoldable $ A.index perm 0
        assumption <- A.fromFoldable $ A.index perm 1
        conclusion <- A.fromFoldable $ A.index perm 2
        guard $ assumption.isAssumption
        guard $ assumption.assumptions `Set.subset` conclusion.assumptions
        let assumptions = Set.union existential.assumptions $
                conclusion.assumptions `Set.difference` assumption.assumptions
        let assFreeVars = fold $
                Set.mapMaybe (map freeVars <<< flip M.lookup m) assumptions
        case M.lookup "x" sub.freeMatch of
            Nothing -> []
            Just (Left _) -> pure assumptions
            Just (Right x) -> do
                guard $ not $ x `Set.member` assFreeVars
                guard $ not $ x `Set.member` freeVars conse
                pure assumptions
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a _ conse d@RAA =
    case A.head $ do
        s <- toSequents d
        {perm, sub} <- Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
        assumption <- A.fromFoldable $ A.index perm 0
        contradiction <- A.fromFoldable $ A.index perm 1
        guard $ assumption.isAssumption
        guard $ assumption.assumptions `Set.subset` contradiction.assumptions
        pure $
            contradiction.assumptions `Set.difference` assumption.assumptions
    of
        Nothing -> Left "Invalid use of deduction rule"
        x -> Right x
matchDeduction a _ conse d@(Definition _ _) =
    case do
        s <- toSequents d
        Seq.matchLift a s $ Sequent {ante : _.formula <$> a, conse}
    of
        [] -> Left "Invalid use of deduction rule"
        _ -> Right $ Just $ Set.unions $ _.assumptions <$> a
matchDeduction a _ conse d =
    case do
        s <- toSequents d
        Seq.match a s $ Sequent {ante : _.formula <$> a, conse}
    of
        [] -> Left "Invalid use of deduction rule"
        _ -> Right $ Just $ Set.unions $ _.assumptions <$> a
