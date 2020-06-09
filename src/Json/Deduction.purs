module Json.Deduction
    ( toJson
    , fromJson
    ) where

import Prelude (($), (<>), bind, pure)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Tuple (Tuple(..))
import Data.Int (toNumber, fromNumber)
import Data.Array as A
import Data.Either (Either(..))
import Data.Either as E

import Symbol (Symbol)
import Sequent (Sequent)
import Deduction (DeductionRule(..))
import Deduction as D

toJson :: DeductionRule -> Json
toJson (Introduction _ i) = AC.fromObject $ O.fromFoldable
    [ Tuple "rule" $ AC.fromString "SI"
    , Tuple "number" $ AC.fromNumber $ toNumber i
    ]
toJson (Definition _ i) = AC.fromObject $ O.fromFoldable
    [ Tuple "rule" $ AC.fromString "Def"
    , Tuple "number" $ AC.fromNumber $ toNumber i
    ]
toJson d = AC.fromString $ D.renderRule d

fromObject :: Array Symbol -> Array (Sequent String) -> O.Object Json ->
    Either String DeductionRule
fromObject syms seqs o = do
    ruleJson <- E.note "Deduction rule is missing name" $ O.lookup "rule" o
    rule <- AC.caseJsonString (Left "Deduction rule name is not a string")
        Right ruleJson
    numJson <- E.note "Deduction rule is missing reference number" $
        O.lookup "number" o
    number <- AC.caseJsonNumber (Left "Deduction rule number is not a number")
        Right numJson
    i <- E.note "Deduction rule number is not an integer" $ fromNumber number
    case rule of
        "SI" -> do
            seq <- E.note "Deduction rule index out of range" $ A.index seqs i
            pure $ Introduction seq i
        "Def" -> do
            sym <- E.note "Deduction rule index out of range" $ A.index syms i
            pure $ Definition sym i
        _ -> Left $ "Invalid indexed deduction rule: " <> rule

fromString :: String -> Either String DeductionRule
fromString "A" = Right Assumption
fromString "MP" = Right ModusPonens
fromString "MT" = Right ModusTollens
fromString "DN" = Right DoubleNegation
fromString "CP" = Right ConditionalProof
fromString "&I" = Right AndIntroduction
fromString "&E" = Right AndElimination
fromString "|I" = Right OrIntroduction
fromString "|E" = Right OrElimination
fromString "RAA" = Right RAA
fromString r = Left $ "Invalid non-indexed deduction rule: " <> r

badDedType :: forall x. x -> Either String DeductionRule
badDedType _ = Left "Deduction rule is not an object or string"

fromJson :: Array Symbol -> Array (Sequent String) -> Json ->
    Either String DeductionRule
fromJson syms seqs = AC.caseJson
    badDedType
    badDedType
    badDedType
    fromString
    badDedType
    (fromObject syms seqs)
