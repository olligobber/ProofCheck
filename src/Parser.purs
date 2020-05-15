module Parser (parse) where

import Prelude (($), (||), (<$>))
import Data.Char.Unicode as U
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as PS
import Data.String.CodeUnits (fromCharArray)
import Data.Array as A

symbol :: Parser String String
symbol = fromCharArray <$> A.many
    (PS.satisfy $ U.isPunctuation || U.isSymbol)

parse = \a -> a
