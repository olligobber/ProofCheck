module Json.VerOne
    ( fromObject
    ) where

import Prelude (($), (<>), (<$>), bind, pure, discard, not)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object (Object)
import Foreign.Object as O
import Data.Either (Either(..))
import Data.Either as E
import Data.Traversable (traverse)
import Control.Applicative (when)
import Data.Foldable (all)

import Json.VerOne.Proof as JP
import Json.VerOne.Symbol as JSym
import Json.VerOne.Sequent as JSeq
import Symbol (Symbol, SymbolMap)
import Symbol as Sym
import Sequent (Sequent)
import Sequent as Seq
import Proof (Proof)

fromObject :: Object Json -> Either String
    { symbolMap :: SymbolMap
    , symbols :: Array Symbol
    , sequents :: Array (Sequent String String String)
    , proof :: Proof
    }
fromObject o = do
    symJson <- E.note "Json is missing symbols" $ O.lookup "symbols" o
    {symbols, symbolMap} <- JSym.allFromJson symJson
    let csymbols = Sym.oldDefaultSymbols <> (Sym.Custom <$> symbols)
    seqJson <- E.note "Json is missing sequents" $ O.lookup "sequents" o
    seqArr <- E.note "Sequents are not in a list" $ AC.toArray seqJson
    sequents <- traverse (JSeq.fromJson symbolMap) seqArr
    when (not $ all Seq.verifyTypes sequents) $
        Left "Sequent is not well typed"
    linesJson <- E.note "Json is missing lines" $ O.lookup "lines" o
    proof <- JP.fromJson symbolMap symbols sequents linesJson
    pure $ { symbolMap, symbols : csymbols, sequents, proof }
