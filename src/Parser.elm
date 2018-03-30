module Parser exposing (parse)

import Char exposing (isUpper, isLower)
import WFF exposing (WFF(..))

-- Get list index
infixl 9 !!
(!!) : List a -> Int -> Maybe a
(!!) xs n = List.head (List.drop n xs)

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
    | Action Int (List ParseTree -> ParseTree)

type alias State = (List Char, List Token, List ParseTree)

-- Production rule 4 is more complicated
prod4 : List ParseTree -> ParseTree
prod4 vals = case vals of
    [rest,atree] -> case rest of
        Unary symb btree -> Binary atree symb btree
        Base _ -> atree
        Binary _ _ _ -> Base "" -- Shouldn't happen
    _ -> Base "" -- Shouldn't happen

-- Production rules
prodRules : List (List Token)
prodRules =
    [   [ Action 0 (\_ -> Base "") ]
    ,   [ Symbol 'P'
        , Action 1 (\vals -> case vals of
            [tree] -> tree
            _ -> Base "")
        ]
    ,   [ Letter ((==) '(')
        , Symbol 'B'
        , Action 2 (\vals -> case vals of
            [tree,_] -> tree
            _ -> Base "")
        ]
    ,   [ Symbol 'S'
        , Symbol 'A'
        , Letter ((==) ')')
        , Action 3 (\vals -> case vals of
            [_,tree,Base symb] -> Unary symb tree
            _ -> Base "")
        ]
    ,   [ Symbol 'A'
        , Symbol 'C'
        , Action 2 prod4
        ]
    ,   [ Letter ((==) ')')
        , Action 1 (\_ -> Base "")
        ]
    ,   [ Letter isProp
        , Symbol 'Q'
        , Action 2 (\vals -> case vals of
            [Base string, Base char] -> Base <| char ++ string
            _ -> Base "")
        ]
    ,   [ Letter isSymbol
        , Symbol 'T'
        , Action 2 (\vals -> case vals of
            [Base string, Base char] -> Base <| char ++ string
            _ -> Base "")
        ]
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

-- Given the next character of the input and a symbol, choose a production rule
chooseProd : Maybe Char -> Char -> Maybe Int
chooseProd next symb = case (symb, toCategory next) of
    ('A', 0) -> Just 1
    ('A', 1) -> Just 2
    ('B', 0) -> Just 4
    ('B', 1) -> Just 4
    ('B', 2) -> Just 3
    ('C', 2) -> Just 3
    ('C', 4) -> Just 5
    ('P', 0) -> Just 6
    ('Q', 0) -> Just 6
    ('Q', 2) -> Just 0
    ('Q', 3) -> Just 0
    ('Q', 4) -> Just 0
    ('S', 2) -> Just 7
    ('T', 0) -> Just 0
    ('T', 1) -> Just 0
    ('T', 2) -> Just 7
    _ -> Nothing

-- A step in parsing a string
parseStep : State -> Result (Maybe ParseTree) State
parseStep state = case state of
    ([], [], [tree]) -> Err <| Just tree
    (char::string, (Letter cond)::tokens, trees) -> if cond char then
            Ok (string, tokens, (Base <| String.fromChar char)::trees)
        else
            Err Nothing
    (string, (Action num function)::tokens, trees) ->
        Ok (string, tokens,
            (function <| List.take num trees)::(List.drop num trees))
    (string, (Symbol symb)::tokens, trees) ->
        case chooseProd (List.head string) symb of
            Nothing -> Err Nothing
            Just index -> case prodRules!!index of
                Nothing -> Err Nothing
                Just newtokens -> Ok (string, newtokens++tokens, trees)
    _ -> Err Nothing

-- repeatedly parseStep until done
parseFull : State -> Result (Maybe ParseTree) State
parseFull state =
    parseStep state
        |> Result.andThen parseFull

-- make a parse tree from a string
parseTree : String -> Maybe ParseTree
parseTree string = case parseFull
    ( "("++string++")"
        |> String.filter isNotSpace
        |> String.toList
    , [Symbol 'A']
    , []) of
            Ok _ -> Nothing
            Err done -> done

type alias Unaries = String -> Maybe (WFF -> WFF)
type alias Binaries = String -> Maybe (WFF -> WFF -> WFF)

convert : Unaries -> Binaries -> ParseTree -> Maybe WFF
convert unaries binaries tree = case tree of
    Base string -> Just <| Prop string
    Unary symb tree -> case unaries symb of
        Nothing -> Nothing
        Just f -> Maybe.map f <| convert unaries binaries tree
    Binary atree symb btree -> case binaries symb of
        Nothing -> Nothing
        Just f -> Maybe.map2 f
            (convert unaries binaries atree)
            (convert unaries binaries btree)

-- parse string to WFF
parse : Unaries -> Binaries -> String -> Maybe WFF
parse unaries binaries string = parseTree string
    |> Maybe.andThen (convert unaries binaries)
