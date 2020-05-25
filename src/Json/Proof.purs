module Json.Proof
    ( toJson
    , fromJson
    ) where

import Prelude (($), (<<<), (<$>), (>=>), pure, bind, flip)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Int (toNumber, fromNumber)
import Data.Set as S
import Data.Traversable (traverse)
import Data.Foldable (foldM)

import Json.WFF as JW
import Json.Deduction as JD
import Json.Symbol as JSym
import Json.Sequent as JSeq
import Proof as P
import Proof (Deduction(..), Proof(..))

fromDeduction :: Deduction -> Json
fromDeduction (Deduction d) = AC.fromObject $ O.fromFoldable
    [ Tuple "assumptions" $ AC.fromArray $
        AC.fromNumber <<< toNumber <$> S.toUnfoldable d.assumptions
    , Tuple "formula" $ JW.toJson d.deduction
    , Tuple "rule" $ JD.toJson d.rule
    , Tuple "references" $ AC.fromArray $
        AC.fromNumber <<< toNumber <$> S.toUnfoldable d.reasons
    ]

toDeduction :: Proof -> Json -> Either String Deduction
toDeduction (Proof p) j = do
    o <- E.note "Deduction is not an object" $ AC.toObject j
    formJson <- E.note "Deduction is missing formula" $ O.lookup "formula" o
    deduction <- JW.fromJson p.symbolMap formJson
    ruleJson <- E.note "Deduction is missing rule" $ O.lookup "rule" o
    rule <- JD.fromJson p.symbols p.sequents ruleJson
    refJson <- E.note "Deduction is missing references" $
        O.lookup "references" o
    refArr <- E.note "Deduction references are not a list" $
        AC.toArray refJson
    reasons <- E.note "Deduction references are not integers" $
        S.fromFoldable <$>
        traverse (AC.toNumber >=> fromNumber) refArr
    case O.lookup "assumptions" o of
        Just assJson -> do
            assArr <- E.note "Deduction assumptions are not a list" $
                AC.toArray assJson
            assumptions <- E.note "Deduction assumptions are not integers" $
                S.fromFoldable <$>
                traverse (AC.toNumber >=> fromNumber) refArr
            pure $ Deduction { assumptions, deduction, rule, reasons }
        Nothing -> do
            let d = Deduction
                    { assumptions : S.empty, deduction, rule, reasons }
            assumptions <- P.getAssumptions d $ Proof p
            pure $ Deduction { assumptions, deduction, rule, reasons }

toJson :: Proof -> Json
toJson (Proof p) = AC.fromObject $ O.fromFoldable
    [ Tuple "symbols" $ AC.fromArray $ JSym.toJson <$> p.symbols
    , Tuple "sequents" $ AC.fromArray $ JSeq.toJson <$> p.sequents
    , Tuple "lines" $ AC.fromArray $ fromDeduction <$> p.lines
    ]

fromJson :: Json -> Either String Proof
fromJson j = do
    o <- E.note "Proof is not an object" $ AC.toObject j
    symJson <- E.note "Proof is missing symbols" $ O.lookup "symbols" o
    {symbols, symbolMap} <- JSym.allFromJson symJson
    seqJson <- E.note "Proof is missing sequents" $ O.lookup "sequents" o
    seqArr <- E.note "Sequents are not in a list" $ AC.toArray seqJson
    sequents <- traverse (JSeq.fromJson symbolMap) seqArr
    linesJson <- E.note "Proof is missing lines" $ O.lookup "lines" o
    linesArr <- E.note "Lines are not in a list" $ AC.toArray linesJson
    let startP = Proof
            { symbols
            , symbolMap
            , sequents
            , assumptions : S.empty
            , lines : []
            }
    foldM
        (\p -> toDeduction p >=> flip P.addDeduction p)
        startP
        linesArr
