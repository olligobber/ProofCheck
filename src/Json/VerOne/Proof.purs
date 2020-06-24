module Json.VerOne.Proof
    ( fromJson
    ) where

import Prelude (($), (<<<), (<$>), (>=>), (+), pure, bind, flip, map)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Either (Either)
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.Int (fromNumber)
import Data.Set as S
import Data.Traversable (traverse)
import Data.Foldable (foldM)
import Data.Array (sort)

import Json.VerOne.WFF as JW
import Json.VerOne.Deduction as JD
import Proof as P
import Proof (Deduction(..), Proof(..))
import Symbol (CustomSymbol, SymbolMap)
import Sequent (Sequent)

toDeduction :: SymbolMap -> Array CustomSymbol ->
    Array (Sequent String String String) -> Proof -> Json ->
    Either String Deduction
toDeduction symbolMap syms seqs (Proof p) j = do
    o <- E.note "Deduction is not an object" $ AC.toObject j
    formJson <- E.note "Deduction is missing formula" $ O.lookup "formula" o
    deduction <- JW.fromJson symbolMap formJson
    ruleJson <- E.note "Deduction is missing rule" $ O.lookup "rule" o
    rule <- JD.fromJson syms seqs ruleJson
    refJson <- E.note "Deduction is missing references" $
        O.lookup "references" o
    refArr <- E.note "Deduction references are not a list" $
        AC.toArray refJson
    reasons <- E.note "Deduction references are not integers" $
        map (_ + 1) <<< sort <$>
        traverse (AC.toNumber >=> fromNumber) refArr
    case O.lookup "assumptions" o of
        Just assJson -> do
            assArr <- E.note "Deduction assumptions are not a list" $
                AC.toArray assJson
            assumptions <- E.note "Deduction assumptions are not integers" $
                S.fromFoldable <$>
                traverse (AC.toNumber >=> fromNumber) assArr
            pure $ Deduction { assumptions, deduction, rule, reasons }
        Nothing -> do
            let d = Deduction
                    { assumptions : S.empty, deduction, rule, reasons }
            assumptions <- P.getAssumptions d $ Proof p
            pure $ Deduction { assumptions, deduction, rule, reasons }

fromJson :: SymbolMap -> Array CustomSymbol ->
    Array (Sequent String String String) -> Json -> Either String Proof
fromJson symbolMap syms seqs j = do
    linesArr <- E.note "Proof is not a list" $ AC.toArray j
    let startP = Proof
            { assumptions : S.empty
            , lines : []
            }
    foldM
        (\p -> toDeduction symbolMap syms seqs p >=> flip P.addDeduction p)
        startP
        linesArr
