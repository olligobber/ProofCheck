module Proof exposing
    ( DeductionRule(..)
    , Deduction
    , Proof
    , empty
    , addSequent
    , addSymbol
    , addDeduction
    , showReason
    )

import WFF exposing (WFF)
import Sequent exposing (..)
import CustomSymbol exposing (Symbol, toSequent1, toSequent2)
import List exposing (map, foldl, filterMap, concatMap, all, filter, sort)
import List.Extra exposing ((!!), unique, isSubsequenceOf, notMember)
import Result exposing (map)
import Tuple exposing (first)
import String exposing (join)

type DeductionRule
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
    | Definition Int
    | Introduction Int

type alias Deduction =
    { assumptions : List Int
    , deduction : WFF
    , rule : DeductionRule
    , reasons : List Int
    }

type alias Proof =
    { sequents : List Sequent
    , symbols : List Symbol
    , lines : List Deduction
    , assumptions : Int
    }

niceList : List a -> String
niceList x = join "," (List.map toString x)

showRule : Proof -> DeductionRule -> String
showRule proof rule = case rule of
    Assumption -> "A"
    ModusPonens -> "MP"
    ModusTollens -> "MT"
    DoubleNegation -> "DN"
    ConditionalProof -> "CP"
    AndIntroduction -> "&I"
    AndElimination -> "&E"
    OrIntroduction -> "|I"
    OrElimination -> "|E"
    RAA -> "RAA"
    Definition x -> case proof.symbols !! x of
        Nothing -> "Internal Error with Symbol Definition"
        Just symbol -> "Def (" ++ symbol.name ++ ")"
    Introduction x -> case proof.sequents !! x of
        Nothing -> "SI"
        Just s -> "SI (" ++ show s ++ ")"

showReason : Proof -> Deduction -> String
showReason proof ded =
    showRule proof ded.rule ++
    " " ++
    niceList (List.map ((+) 1) ded.reasons)

-- Empty proof
empty : Proof
empty =
    { sequents = []
    , symbols = []
    , lines = []
    , assumptions = 0
    }

-- Add a sequent to a proof
addSequent : Sequent -> Proof -> Proof
addSequent sequent proof =
    { proof
    | sequents = proof.sequents ++ [sequent]
    }

-- Add a symbol to a proof
addSymbol : Symbol -> Proof -> Proof
addSymbol symbol proof =
    { proof
    | symbols = proof.symbols ++ [symbol]
    }

-- Turns a list of maybes into maybe a list, only if all were Just
getAll : List (Maybe a) -> Maybe (List a)
getAll = foldl
    (\mb -> \mlist -> case (mb, mlist) of
        (Just val, Just list) -> Just <| list ++ [val]
        _ -> Nothing)
    (Just [])

-- Turn a deduction in a proof into the appropriate sequent
makeSequent : Proof -> Deduction -> Result String Sequent
makeSequent proof deduction =
    case getAll <| List.map (\i -> proof.lines !! i) deduction.reasons of
        Nothing -> Err "Line referenced was not found in the proof"
        Just reasons -> Ok
            { ante = List.map .deduction reasons
            , conse = deduction.deduction
            }

-- Returns a list of sequents for a rule
ruleSequent : Proof -> DeductionRule -> List Sequent
ruleSequent proof rule = case rule of
    Assumption          -> [ass]
    ModusPonens         -> [mp]
    ModusTollens        -> [mt]
    DoubleNegation      -> [dn1, dn2]
    ConditionalProof    -> [cp]
    AndIntroduction     -> [ai]
    AndElimination      -> [ae1, ae2]
    OrIntroduction      -> [oi1, oi2]
    OrElimination       -> [oe]
    RAA                 -> [raa]
    Definition i         -> case proof.symbols !! i of
        Just symb -> [toSequent1 symb, toSequent2 symb]
        Nothing -> []
    Introduction i      -> filterMap ((!!) proof.sequents) [i]

-- Applies a permutation to a list
applyPerm : List a -> List Int -> Maybe (List a)
applyPerm list indices = List.map ((!!) list) indices
    |> getAll

-- Matches a deduction to its sequent, returning a list of possible
-- permutations of premises
matchDeduction : Proof -> Deduction -> Result String (List (List Deduction))
matchDeduction proof deduction = makeSequent proof deduction
    |> Result.map
        (\ded -> concatMap (flip match ded) (ruleSequent proof deduction.rule))
    |> Result.map (List.map first)
    |> Result.map (List.filterMap (applyPerm deduction.reasons))
    |> Result.map (List.filterMap (applyPerm (proof.lines)))

-- Add a deduction to a proof
addDeduction : Proof -> Deduction -> Result String Proof
addDeduction proof new = case (new.rule, matchDeduction proof new) of
    (_, Err err) -> Err err
    (Assumption, _) -> Ok
        { proof
        | lines = proof.lines ++ [
            { new
            | assumptions = [proof.assumptions + 1]
            , reasons = []
            } ]
        , assumptions = proof.assumptions + 1
        }
    (ConditionalProof, Ok list) ->
        case filter
            (\deds -> case deds of
                [ass, res] -> all identity
                    [ ass.rule == Assumption
                    , isSubsequenceOf ass.assumptions res.assumptions
                    ]
                _ -> False)
            list
        of
            [] -> Err "Incorrect use of Conditional Proof"
            [ass,res]::_ -> Ok
                { proof
                | lines = proof.lines ++ [
                    { new
                    | assumptions =
                        filter
                            (flip notMember ass.assumptions)
                            res.assumptions
                    } ]
                }
            _ -> Err "Internal error when adding deduction: Conditional Proof"
    (RAA, Ok list) ->
        case filter
            (\deds -> case deds of
                [ass, res] -> all identity
                    [ ass.rule == Assumption
                    , isSubsequenceOf ass.assumptions res.assumptions
                    ]
                _ -> False)
            list
        of
            [] -> Err "Incorrect use of Reductio Ad Absurdium"
            [ass,res]::_ -> Ok
                { proof
                | lines = proof.lines ++ [
                    { new
                    | assumptions =
                        filter
                            (flip notMember ass.assumptions)
                            res.assumptions
                    } ]
                }
            _ -> Err "Internal error when adding deduction: RAA"
    (OrElimination, Ok list) ->
        case filter
            (\deds -> case deds of
                [aorb, assa, assb, resa, resb] -> all identity
                    [ assa.rule == Assumption
                    , assb.rule == Assumption
                    , isSubsequenceOf assa.assumptions resa.assumptions
                    , isSubsequenceOf assb.assumptions resb.assumptions
                    , not <| isSubsequenceOf assa.assumptions resb.assumptions
                    , not <| isSubsequenceOf assa.assumptions aorb.assumptions
                    , not <| isSubsequenceOf assb.assumptions resa.assumptions
                    , not <| isSubsequenceOf assb.assumptions aorb.assumptions
                    ]
                _ -> False)
            list
        of
            [] -> Err "Incorrect use of Or Elimination"
            [aorb, assa, assb, resa, resb]::_ -> Ok
                { proof
                | lines = proof.lines ++ [
                    { new
                    | assumptions = aorb.assumptions
                        |> (++) resa.assumptions
                        |> (++) resb.assumptions
                        |> sort
                        |> unique
                        |> filter (flip notMember assa.assumptions)
                        |> filter (flip notMember assb.assumptions)
                    } ]
                }
            _ -> Err "Internal error when adding deduction: Or Elimination"
    (_, Ok list) ->
        case list of
            [] -> Err "Error in deduction"
            antes::_ -> Ok
                { proof
                | lines = proof.lines ++ [
                    { new
                    | assumptions = concatMap .assumptions antes
                        |> sort
                        |> unique
                    } ]
                }
