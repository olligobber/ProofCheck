module Json.VerOne.Deduction
    ( fromJson
    ) where

import Prelude (($), (<>), bind, pure)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Int (fromNumber)
import Data.Array as A
import Data.Either (Either(..))
import Data.Either as E

import Symbol (CustomSymbol)
import Sequent (Sequent)
import Lemmon (LemmonRule(..))

fromObject :: Array CustomSymbol -> Array (Sequent String String String) ->
    O.Object Json -> Either String LemmonRule
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

fromString :: String -> Either String LemmonRule
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

badDedType :: forall x. x -> Either String LemmonRule
badDedType _ = Left "Deduction rule is not an object or string"

fromJson :: Array CustomSymbol -> Array (Sequent String String String) ->
    Json -> Either String LemmonRule
fromJson syms seqs = AC.caseJson
    badDedType
    badDedType
    badDedType
    fromString
    badDedType
    (fromObject syms seqs)
