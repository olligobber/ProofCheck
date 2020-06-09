module Json
    ( toJson
    , fromJson
    ) where

import Prelude (($), (<$>), bind, pure)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.Either as E
import Data.Traversable (traverse)

import Json.Proof as JP
import Json.Symbol as JSym
import Json.Sequent as JSeq
import Symbol (Symbol, SymbolMap)
import Sequent (Sequent)
import Proof (Proof)

toJson :: Array Symbol -> Array (Sequent String) -> Proof -> Json
toJson syms seqs p = AC.fromObject $ O.fromFoldable
    [ Tuple "symbols" $ AC.fromArray $ JSym.toJson <$> syms
    , Tuple "sequents" $ AC.fromArray $ JSeq.toJson <$> seqs
    , Tuple "lines" $ JP.toJson p
    ]

fromJson :: Json -> Either String
    { symbolMap :: SymbolMap
    , symbols :: Array Symbol
    , sequents :: Array (Sequent String)
    , proof :: Proof
    }
fromJson j = do
    o <- E.note "Json is not an object" $ AC.toObject j
    symJson <- E.note "Json is missing symbols" $ O.lookup "symbols" o
    {symbols, symbolMap} <- JSym.allFromJson symJson
    seqJson <- E.note "Json is missing sequents" $ O.lookup "sequents" o
    seqArr <- E.note "Sequents are not in a list" $ AC.toArray seqJson
    sequents <- traverse (JSeq.fromJson symbolMap) seqArr
    linesJson <- E.note "Json is missing lines" $ O.lookup "lines" o
    proof <- JP.fromJson symbolMap symbols sequents linesJson
    pure $ { symbolMap, symbols, sequents, proof }
