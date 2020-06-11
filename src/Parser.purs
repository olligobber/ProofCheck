module Parser (parseSymbol, parse) where

import Prelude
    (($), (||), (&&), (<$>), (<>), (<<<), (>>>), (<*), bind, pure, show)
import Control.Alt ((<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array as A
import Data.Char.Unicode as U
import Data.Either (Either(..))
import Data.Either as E
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT
import Text.Parsing.Parser.Pos as PP
import Data.String as S

import WFF (UnaryOp, BinaryOp, WFF)
import WFF as WFF
import Symbol (SymbolMap)

symbol :: Parser String String
symbol = fromCharArray <$> A.some
    ( PS.satisfy
        ( (U.isPunctuation || U.isSymbol) &&
        (_ `A.notElem` [',','(',')']) )
        <?> "Symbol or Punctuation"
    )

definedSymbol :: SymbolMap -> Parser String (Either UnaryOp BinaryOp)
definedSymbol m = do
    p <- P.position
    s <- symbol
    case M.lookup s m of
        Just o -> pure o
        Nothing -> P.failWithPosition ("Unrecognised symbol: " <> s) p

proposition :: Parser String (WFF String)
proposition = WFF.Prop <<< fromCharArray <$> A.some PT.letter

safeExpression :: Lazy (Parser String (WFF String)) =>
    SymbolMap -> Parser String (WFF String)
safeExpression m = proposition
    <|> PC.between (PS.char '(') (PS.char ')') (defer \_ -> expression m)

unaryExpression :: Lazy (Parser String (WFF String)) =>
    SymbolMap -> Parser String (WFF String)
unaryExpression m = do
    p <- P.position
    o <- definedSymbol m
    contents <- safeExpression m
    case o of
        Left operator -> pure $ WFF.Unary { operator, contents }
        Right _ -> P.failWithPosition "Expected Unary Symbol" p

tailBinaryExpression :: Lazy (Parser String (WFF String)) => SymbolMap ->
    Parser String { operator :: BinaryOp, right :: WFF String }
tailBinaryExpression m = do
    p <- P.position
    o <- definedSymbol m
    right <- safeExpression m
    case o of
        Right operator -> pure { operator, right }
        Left _ -> P.failWithPosition "Expected Binary Symbol" p

maybeBinaryExpression :: Lazy (Parser String (WFF String)) =>
    SymbolMap -> Parser String (WFF String)
maybeBinaryExpression m = do
    left <- safeExpression m
    rest <- PC.option Nothing $ Just <$> tailBinaryExpression m
    case rest of
        Nothing -> pure left
        Just {right, operator} -> pure $ WFF.Binary $ {left, operator, right}

expression :: Lazy (Parser String (WFF String)) =>
    SymbolMap -> Parser String (WFF String)
expression m = maybeBinaryExpression m <|> unaryExpression m

showError :: String -> P.ParseError -> String
showError s (P.ParseError e (PP.Position p)) =
    "Parsing Error: " <>
    e <>
    " at position " <>
    show p.column <>
    " when parsing \"" <>
    s <>
    "\""

removeSpaces :: String -> String
removeSpaces = S.replaceAll (S.Pattern " ") (S.Replacement "")

parseSymbol :: String -> Either String String
parseSymbol s = E.either (showError s >>> Left) Right $ P.runParser s $
    symbol <* PS.eof

parse :: SymbolMap -> String -> Either String (WFF String)
parse m s = E.either (showError (removeSpaces s) >>> Left) Right $
    P.runParser (removeSpaces s) $
    expression m <* PS.eof
