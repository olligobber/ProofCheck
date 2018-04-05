module Parser exposing (..)

import Char exposing (isUpper, isLower)
import WFF exposing (WFF(Prop))
import Result exposing (andThen, map, map2)
import String exposing (any, all, fromChar, filter, toList)
import List exposing (take, drop, head)

-- Proposition characters
isProp : Char -> Bool
isProp char = isLower char || isUpper char

-- Symbol characters
isSymbol : Char -> Bool
isSymbol char = any ((==) char) "`~!@#$%^&*_+-=[]{}|\\:;\"',<.>/?"

-- Whitespace
isNotSpace : Char -> Bool
isNotSpace char = all ((/=) char) " \t\n\r"

-- Parse Tree
type ParseTree
    = Base String
    | Unary String ParseTree
    | Binary ParseTree String ParseTree

-- Action Tokens
type Token
    = Letter (Char -> Bool)
    | Symbol Sym
    | Action Int (List ParseTree -> Result String ParseTree)

-- Valid Symbols
type Sym
    = A
    | B
    | C
    | P
    | Q
    | S
    | T

type alias State = (List Char, List Token, List ParseTree)

-- Production rules
prod1 : List Token
prod1 =
    [ Symbol B
    , Symbol C
    , Action 2 (\vals -> case vals of
        [Base _, tree] -> Ok tree
        [Unary symb treeb, treea] -> Ok <| Binary treea symb treeb
        _ -> Err "Parse Error: Action token 1 failed")
    ]

prod2 : List Token
prod2 =
    [ Symbol S
    , Symbol B
    , Action 2 (\vals -> case vals of
        [tree, Base symb] -> Ok <| Unary symb tree
        _ -> Err "Parse Error: Action token 2 failed")
    ]

prod3 : List Token
prod3 =
    [ Symbol P
    , Action 1 (\vals -> case vals of
        [tree] -> Ok tree
        _ -> Err "Parse Error: Action token 3 failed")
    ]

prod4 : List Token
prod4 =
    [ Letter ((==) '(')
    , Symbol A
    , Letter ((==) ')')
    , Action 3 (\vals -> case vals of
        [_, tree, _] -> Ok tree
        _ -> Err "Parse Error: Action token 4 failed")
    ]

prod5 : List Token
prod5 =
    [ Action 0 (always (Ok <| Base ""))
    ]

prod6 : List Token
prod6 =
    [ Letter isProp
    , Symbol Q
    , Action 2 (\vals -> case vals of
        [Base string, Base char] -> Ok (Base <| char ++ string)
        _ -> Err "Parse Error: Action token 6 failed")
    ]

prod7 : List Token
prod7 =
    [ Letter isSymbol
    , Symbol T
    , Action 2 (\vals -> case vals of
        [Base string, Base char] -> Ok (Base <| char ++ string)
        _ -> Err "Parse Error: Action token 7 failed")
    ]

-- Categorises a character
toCategory : Maybe Char -> Int
toCategory next = case next of
    Nothing -> 3
    Just char -> case char of
        '(' -> 1
        ')' -> 4
        _ -> if isProp char then
                0
            else if isSymbol char then
                2
            else
                -1

-- Produce error given invalid character
unexpectedError : Maybe Char -> Result String a
unexpectedError c = case c of
    Just char -> Err <| "Parse Error: Unexpected character: " ++ fromChar char
    Nothing -> Err <| "Parse Error: Unexpected end of input"

-- Given the next character of the input and a symbol, choose a production rule
chooseProd : Maybe Char -> Sym -> Result String (List Token)
chooseProd next symb = case (symb, toCategory next) of
    (A, 0) -> Ok prod1
    (A, 1) -> Ok prod1
    (A, 2) -> Ok prod2
    (B, 0) -> Ok prod3
    (B, 1) -> Ok prod4
    (C, 2) -> Ok prod2
    (C, 3) -> Ok prod5
    (C, 4) -> Ok prod5
    (P, 0) -> Ok prod6
    (Q, 0) -> Ok prod6
    (Q, 2) -> Ok prod5
    (Q, 3) -> Ok prod5
    (Q, 4) -> Ok prod5
    (S, 2) -> Ok prod7
    (T, 0) -> Ok prod5
    (T, 1) -> Ok prod5
    (T, 2) -> Ok prod7
    _ -> unexpectedError next

-- A step in parsing a string
parseStep : State -> Result (Result String ParseTree) State
parseStep state = case state of
    ([], [], [tree]) -> Err <| Ok tree
    (char::string, (Letter cond)::tokens, trees) -> if cond char then
            Ok (string, tokens, (Base <| fromChar char)::trees)
        else
            Err <| unexpectedError (Just char)
    (string, (Action num function)::tokens, trees) ->
        case function <| take num trees of
            Err err -> Err <| Err err
            Ok newtree -> Ok (string, tokens, newtree::(drop num trees))
    (string, (Symbol symb)::tokens, trees) ->
        case chooseProd (head string) symb of
            Ok newtokens -> Ok (string, newtokens++tokens, trees)
            Err err -> Err <| Err err
    ([], _, _) -> Err <| unexpectedError Nothing
    _ -> Err <| Err "Parse Error: Invalid state"

-- repeatedly parseStep until done
parseFull : State -> Result (Result String ParseTree) State
parseFull state =
    parseStep state
        |> andThen parseFull

-- make a parse tree from a string
parseTree : String -> Result String ParseTree
parseTree string = case parseFull
    ( string
        |> filter isNotSpace
        |> toList
    , [Symbol A]
    , []) of
        Ok _ -> Err "Parse Error: Parsing finished early"
        Err done -> done

type alias Unaries = String -> Maybe (WFF -> WFF)
type alias Binaries = String -> Maybe (WFF -> WFF -> WFF)

convert : Unaries -> Binaries -> ParseTree -> Result String WFF
convert unaries binaries tree = case tree of
    Base string -> Ok <| Prop string
    Unary symb tree -> case unaries symb of
        Nothing -> Err <| "Parse Error: Unrecognised unary operator: " ++
            symb
        Just f -> map f <| convert unaries binaries tree
    Binary atree symb btree -> case binaries symb of
        Nothing -> Err <| "Parse Error: Unrecognised binary operator: " ++
            symb
        Just f -> map2 f
            (convert unaries binaries atree)
            (convert unaries binaries btree)

-- parse string to WFF
parse : Unaries -> Binaries -> String -> Result String WFF
parse unaries binaries string = parseTree string
    |> andThen (convert unaries binaries)
