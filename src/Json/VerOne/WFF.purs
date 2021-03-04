module Json.VerOne.WFF
    ( fromJson
    ) where

import Prelude (($))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Data.Either (Either(..))

import WFF (WFF)
import Parser (parse)
import Symbol (SymbolMap)

fromJson :: SymbolMap -> Json -> Either String (WFF String String String)
fromJson m = AC.caseJsonString (Left "Formula is not a string") $ parse m
