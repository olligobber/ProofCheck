module Parser exposing (parse)

import Char exposing (isUpper, isLower)
import WFF exposing (WFF(Prop))

-- Proposition characters
isProp : Char -> Bool
isProp char = Char.isLower char || Char.isUpper char

-- Symbol characters
isSymbol : Char -> Bool
isSymbol char = String.any ((==) char) "`~!@#$%^&*_+-=[]{}|\\:;\"',<.>/?"

-- Whitespace
isNotSpace : Char -> Bool
isNotSpace char = String.all ((/=) char) " \t\n\r"

-- Parse Tree
type ParseTree
    = Base String
    | Unary String ParseTree
    | Binary ParseTree String ParseTree

-- Action Tokens
type Token
    = Letter (Char -> Bool)
    | Symbol Char
    | Action Int (List ParseTree -> Result String ParseTree)

type alias State = (List Char, List Token, List ParseTree)

-- Production rules
prod0 : List Token
prod0 = [Action 0 (\_ -> Ok <| Base "")]

prod1 : List Token
prod1 =
    [ Symbol 'P'
    , Action 1 (\vals -> case vals of
        [tree] -> Ok tree
        _ -> Err "Parse Error: Action token 1 failed")
    ]

prod2 : List Token
prod2 =
    [ Letter ((==) '(')
    , Symbol 'B'
    , Action 2 (\vals -> case vals of
        [tree,_] -> Ok tree
        _ -> Err "Parse Error: Action token 2 failed")
    ]

prod3 : List Token
prod3 =
    [ Symbol 'S'
    , Symbol 'A'
    , Letter ((==) ')')
    , Action 3 (\vals -> case vals of
        [_,tree,Base symb] -> Ok <| Unary symb tree
        _ -> Err "Parse Error: Action token 3 failed")
    ]

prod4 : List Token
prod4 =
    [ Symbol 'A'
    , Symbol 'C'
    , Action 2 (\vals -> case vals of
        [rest,atree] -> case rest of
            Unary symb btree -> Ok <| Binary atree symb btree
            Base _ -> Ok atree
            Binary _ _ _ -> Err "Parse Error: Action token 4 failed on binary"
        _ -> Err "Parse Error: Action token 4 failed")
    ]

prod5 : List Token
prod5 =
    [ Letter ((==) ')')
    , Action 1 (\_ -> Ok <| Base "")
    ]

prod6 : List Token
prod6 =
    [ Letter isProp
    , Symbol 'Q'
    , Action 2 (\vals -> case vals of
        [Base string, Base char] -> Ok (Base <| char ++ string)
        _ -> Err "Parse Error: Action token 6 failed")
    ]

prod7 : List Token
prod7 =
    [ Letter isSymbol
    , Symbol 'T'
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
    Just char -> Err <| "Parse Error: Unexpected character: "++
        String.fromChar char
    Nothing -> Err <| "Parse Error: Unexpected end of input"

-- Given the next character of the input and a symbol, choose a production rule
chooseProd : Maybe Char -> Char -> Result String (List Token)
chooseProd next symb = case (symb, toCategory next) of
    ('A', 0) -> Ok prod1
    ('A', 1) -> Ok prod2
    ('A', _) -> unexpectedError next
    ('B', 0) -> Ok prod4
    ('B', 1) -> Ok prod4
    ('B', 2) -> Ok prod3
    ('B', _) -> unexpectedError next
    ('C', 2) -> Ok prod3
    ('C', 4) -> Ok prod5
    ('C', _) -> unexpectedError next
    ('P', 0) -> Ok prod6
    ('P', _) -> unexpectedError next
    ('Q', 0) -> Ok prod6
    ('Q', 2) -> Ok prod0
    ('Q', 3) -> Ok prod0
    ('Q', 4) -> Ok prod0
    ('Q', _) -> unexpectedError next
    ('S', 2) -> Ok prod7
    ('S', _) -> unexpectedError next
    ('T', 0) -> Ok prod0
    ('T', 1) -> Ok prod0
    ('T', 2) -> Ok prod7
    ('T', _) -> unexpectedError next
    _ -> Err <| "Parse Error: Unexpected symbol on stack: " ++
        String.fromChar symb

-- A step in parsing a string
parseStep : State -> Result (Result String ParseTree) State
parseStep state = case state of
    ([], [], [tree]) -> Err <| Ok tree
    (char::string, (Letter cond)::tokens, trees) -> if cond char then
            Ok (string, tokens, (Base <| String.fromChar char)::trees)
        else
            Err <| unexpectedError (Just char)
    (string, (Action num function)::tokens, trees) ->
        case function <| List.take num trees of
            Err err -> Err <| Err err
            Ok newtree -> Ok (string, tokens, newtree::(List.drop num trees))
    (string, (Symbol symb)::tokens, trees) ->
        case chooseProd (List.head string) symb of
            Ok newtokens -> Ok (string, newtokens++tokens, trees)
            Err err -> Err <| Err err
    ([], _, _) -> Err <| unexpectedError Nothing
    _ -> Err <| Err "Parse Error: Invalid state"

-- repeatedly parseStep until done
parseFull : State -> Result (Result String ParseTree) State
parseFull state =
    parseStep state
        |> Result.andThen parseFull

-- make a parse tree from a string
parseTree : String -> Result String ParseTree
parseTree string =  case String.filter isNotSpace string of
    "" -> Err "Parse Error: Cannot parse empty string"
    strings -> case parseFull
        ( "("++strings++")"
            |> String.toList
        , [Symbol 'A']
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
        Just f -> Result.map f <| convert unaries binaries tree
    Binary atree symb btree -> case binaries symb of
        Nothing -> Err <| "Parse Error: Unrecognised binary operator: " ++
            symb
        Just f -> Result.map2 f
            (convert unaries binaries atree)
            (convert unaries binaries btree)

-- parse string to WFF
parse : Unaries -> Binaries -> String -> Result String WFF
parse unaries binaries string = parseTree string
    |> Result.andThen (convert unaries binaries)
