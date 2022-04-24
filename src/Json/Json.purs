module Json
    ( toJson
    , fromJson
    ) where

import Prelude (($), (<$>), (>>=), (<>), bind, pure, discard, not, unit)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Either as E
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..))
import Control.Applicative (when)
import Data.Foldable (all, foldMap)
import Data.Array as A

import Json.Proof as JP
import Json.Symbol as JSym
import Json.Sequent as JSeq
import Sequent as Seq
import Proof as P
import WFF as W
import Lemmon (LemmonRule)
import Symbol (Symbol, SymbolMap)

type WFF = W.WFF String String String
type Sequent = Seq.Sequent String String String
type Proof = P.Proof LemmonRule WFF String

toJson :: Array Symbol -> Array Sequent -> Proof -> Json
toJson syms seqs p = AC.fromObject $ O.fromFoldable
    [ Tuple "version" $ AC.fromNumber 2.0
    , Tuple "symbols" $ AC.fromArray $ JSym.toJson <$> syms
    , Tuple "sequents" $ AC.fromArray $ JSeq.toJson <$> seqs
    , Tuple "lines" $ JP.toJson p
    ]

fromObject :: O.Object Json -> Either String
    { symbolMap :: SymbolMap
    , symbols :: Array Symbol
    , sequents :: Array Sequent
    , proof :: Proof
    }
fromObject o = do
    symJson <- E.note "Json is missing symbols" $ O.lookup "symbols" o
    {symbols, symbolMap} <- JSym.allFromJson symJson
    seqJson <- E.note "Json is missing sequents" $ O.lookup "sequents" o
    seqArr <- E.note "Sequents are not in a list" $ AC.toArray seqJson
    sequents <- traverse (JSeq.fromJson symbolMap) seqArr
    when (not $ all Seq.verifyTypes sequents) $
        Left "Sequent is not well typed"
    case (foldMap Seq.verifyBindings sequents) >>= A.head of
        Just e -> Left $ "Error in sequent: " <> e
        Nothing -> Right unit
    linesJson <- E.note "Json is missing lines" $ O.lookup "lines" o
    proof <- JP.fromJson symbolMap symbols sequents linesJson
    pure $ { symbolMap, symbols, sequents, proof }

fromJson :: Json -> Either String
    { symbolMap :: SymbolMap
    , symbols :: Array Symbol
    , sequents :: Array Sequent
    , proof :: Proof
    }
fromJson j = do
    o <- E.note "Json is not an object" $ AC.toObject j
    case O.lookup "version" o of
        Nothing -> Left "Version 1 is no longer supported"
        Just verJson -> case AC.toNumber verJson of
            Nothing -> Left "Save version is not a number"
            Just 2.0 -> fromObject o
            Just 1.0 -> Left "Version 1 is no longer supported"
            _ -> Left "Unrecognised save version"
