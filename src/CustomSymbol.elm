module CustomSymbol exposing
    ( Symbol
    , makeUnary
    , makeBinary
    , toSequent1
    , toSequent2
    , makeMap
    )

import WFF exposing
    (WFF(..), fromUn, fromBin, eval, variables, neg, and, or, implies)
import Sequent exposing (Sequent)
import Set exposing (diff, singleton, fromList, isEmpty)
import Parser exposing (Unaries, Binaries, SymbolMaps)

type alias Symbol =
    { name : String
    , wff : WFF
    , definition : WFF
    }

-- Construct a unary symbol, nothing if extra variables found
makeUnary : String -> String -> WFF -> Result String Symbol
makeUnary prop symbol def =
    if isEmpty <| diff (variables def) (singleton prop) then
        Ok
            { name = symbol
            , wff = Unary
                { fn = fromUn
                    (\b -> eval (always b) def)
                , symbol = symbol
                , contents = Prop prop
                }
            , definition = def
            }
    else
        Err "Extra propositions found in symbol definition"

-- Construct a binary symbol, nothing if extra variables found
makeBinary : String -> String -> String -> WFF -> Result String Symbol
makeBinary propa propb symbol def =
    if isEmpty <| diff (variables def) (fromList [propa, propb]) then
        Ok
            { name = symbol
            , wff = Binary
                { fn = fromBin
                    (\a -> \b -> eval
                        (\c -> case c == propa of
                            True -> a
                            False -> b)
                        def)
                , symbol = symbol
                , first = Prop propa
                , second = Prop propb
                }
            , definition = def
            }
    else
        Err "Extra propositions found in symbol definition"

-- Turns a symbol definition into the first sequent
toSequent1 : Symbol -> Sequent
toSequent1 symbol =
    { ante = [symbol.wff]
    , conse = symbol.definition
    }

-- Turns a symbol definition into the second sequent
toSequent2 : Symbol -> Sequent
toSequent2 symbol =
    { ante = [symbol.definition]
    , conse = symbol.wff
    }

-- Turns a list of sequents into maps for parsing
makeMap : List Symbol -> SymbolMaps
makeMap list = case list of
    [] ->
        ( \s -> case s of
            "~" -> Just neg
            _ -> Nothing
        , \s -> case s of
            "|" -> Just or
            "&" -> Just and
            "->" -> Just implies
            _ -> Nothing
        )
    symbol::rest -> case (symbol.wff, makeMap rest) of
        (Prop _,_) -> (always Nothing, always Nothing) -- Should not occur
        (Unary v, (uns, bins)) ->
            ( \s -> if s == v.symbol
                then Just (\x -> Unary { v | contents = x } )
                else uns s
            , bins )
        (Binary v, (uns, bins)) ->
            ( uns
            , \s -> if s == v.symbol
                then Just (\x -> \y -> Binary { v | first = x, second = y} )
                else bins s
            )
