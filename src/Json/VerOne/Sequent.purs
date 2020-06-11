module Json.VerOne.Sequent
    ( fromJson
    ) where

import Prelude (($), bind, pure)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Foreign.Object as O
import Data.Either (Either(..))
import Data.Either as E
import Data.Traversable (traverse)

import Json.VerOne.WFF as JW
import Sequent (Sequent(..))
import Symbol (SymbolMap)

fromObject :: SymbolMap -> O.Object Json -> Either String (Sequent String)
fromObject m o = do
    anteJson <- E.note "Sequent is missing ante" $ O.lookup "ante" o
    ante <- AC.caseJsonArray (Left "Sequent ante is not a list")
        (traverse $ JW.fromJson m) anteJson
    conseJson <- E.note "Sequent is missing conse" $ O.lookup "conse" o
    conse <- JW.fromJson m conseJson
    pure $ Sequent { ante, conse }

fromJson :: SymbolMap -> Json -> Either String (Sequent String)
fromJson m = AC.caseJsonObject (Left "Sequent is not an object") $ fromObject m
