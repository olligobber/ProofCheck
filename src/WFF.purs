module WFF
    ( NullaryOp(..)
    , UnaryOp(..)
    , BinaryOp(..)
    , Quantifier(..)
    , Variable(..)
    , WFF(..)
    , render
    , renderQ
    , renderNullaryOp
    , renderUnaryOp
    , renderBinaryOp
    , traversePredicates
    , traverseFree
    , traverseBound
    , prop
    , pred
    , botOp
    , negOp
    , andOp
    , impliesOp
    , orOp
    , bot
    , neg
    , and
    , or
    , foralv
    , exists
    , (/\)
    , (\/)
    , (==>)
    , implies
    , match
    , freeVars
    , ValidFree(..)
    , Match
    , Matches(..)
    , Typing
    , isWellTyped
    , getTyping
    , validateBindings
    ) where

import Prelude
    ( class Eq, class Ord, class Show, class Semigroup, class Applicative
    , class Monoid
    , Unit
    , show, identity, otherwise, unit, bind, pure, not, const, mempty, discard
    , (<>), ($), (<$>), (<<<), (==), (&&), (+), (<), (<*>), (-), (>), (>>>)
    )
import Data.String (joinWith)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (all, foldMap, length, fold)
import Data.Map (Map)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (traverse, sequence)

import Util (mapTraversal, getAllIndex)

-- Operators used to join formulas

newtype NullaryOp = NullaryOp String

derive instance eqNullaryOp :: Eq NullaryOp
derive instance ordNullaryOp :: Ord NullaryOp

instance showNullaryOp :: Show NullaryOp where
    show (NullaryOp s) = "(NullarOp " <> show s <> ")"

renderNullaryOp :: NullaryOp -> String
renderNullaryOp (NullaryOp s) = s

newtype UnaryOp = UnaryOp String

derive instance eqUnaryOp :: Eq UnaryOp
derive instance ordUnaryOp :: Ord UnaryOp

instance showUnaryOp :: Show UnaryOp where
    show (UnaryOp s) = "(UnaryOp " <> show s <> ")"

renderUnaryOp :: UnaryOp -> String
renderUnaryOp (UnaryOp s) = s

newtype BinaryOp = BinaryOp String

derive instance eqBinaryOp :: Eq BinaryOp
derive instance ordBinaryOp :: Ord BinaryOp

instance showBinaryOp :: Show BinaryOp where
    show (BinaryOp s) = "(BinaryOp " <> show s <> ")"

renderBinaryOp :: BinaryOp -> String
renderBinaryOp (BinaryOp s) = s

-- Quantifiers to bind variables in formulas

data Quantifier = Forall | Exists

derive instance eqQuantifier :: Eq Quantifier
derive instance ordQuantifier :: Ord Quantifier

instance showQuantifier :: Show Quantifier where
    show Forall = "Forall"
    show Exists = "Exists"

renderQ :: Quantifier -> String
renderQ Forall = "∀"
renderQ Exists = "∃"

-- Variables in formulas

data Variable free bound
    = Free free
    | Bound bound Int

derive instance eqVariable :: (Eq free, Eq bound) => Eq (Variable free bound)
derive instance ordVariable :: (Ord free, Ord bound) =>
    Ord (Variable free bound)

instance showVariable :: (Show free, Show bound) =>
    Show (Variable free bound) where
        show (Free x) = "(Free " <> show x <> ")"
        show (Bound x i) = "(Bound " <> show x <> " " <> show i <> ")"

renderVariable :: Variable String String -> String
renderVariable (Free x) = x
renderVariable (Bound x _) = x

-- Determine if a variable is bound to a quantifier within b levels
boundBelow :: forall free bound. Int -> Variable free bound -> Boolean
boundBelow _ (Free _) = true
boundBelow b (Bound _ i) = i < b

-- Determine if a variable is bound at a given level with a given name
isBound :: forall free bound. Eq bound =>
    bound -> Int -> Variable free bound -> Boolean
isBound v l (Bound w k) = v == w && l == k
isBound _ _ (Free _) = false

-- WFF with types for propositions and predicates, and free and bound variables
data WFF pred free bound
    = Pred { predicate :: pred, variables :: Array (Variable free bound) }
    | Nullary { operator :: NullaryOp }
    | Unary { operator :: UnaryOp, contents :: WFF pred free bound }
    | Binary
        { operator :: BinaryOp
        , left :: WFF pred free bound
        , right :: WFF pred free bound
        }
    | Quant
        { operator :: Quantifier
        , variable :: bound
        , contents :: WFF pred free bound
        }

-- Construct a predicate given its name and an array of variables
pred :: forall pred free bound.
    pred -> Array (Variable free bound) -> WFF pred free bound
pred f x = Pred { predicate : f, variables : x}

-- Construct a proposition given its name
prop :: forall pred free bound. pred -> WFF pred free bound
prop x = pred x []

derive instance eqWFF :: (Eq pred, Eq free, Eq bound) =>
    Eq (WFF pred free bound)
derive instance ordWFF :: (Ord pred, Ord free, Ord bound) =>
    Ord (WFF pred free bound)

instance showWFF :: (Show pred, Show free, Show bound) =>
    Show (WFF pred free bound) where
        show (Pred p) = "(Pred " <> show p <> ")"
        show (Nullary n) = "(Nullary" <> show n <> ")"
        show (Unary u) = "(Unary " <> show u <> ")"
        show (Binary b) = "(Binary " <> show b <> ")"
        show (Quant q) = "(Quant " <> show q <> ")"

traversePredicates :: forall a b f free bound. Applicative f =>
    (a -> f b) -> WFF a free bound -> f (WFF b free bound)
traversePredicates f (Pred p) =
    Pred <<< { predicate : _, variables : p.variables } <$> f p.predicate
traversePredicates _ (Nullary n) = pure $ Nullary n
traversePredicates f (Unary u) =
    Unary <<< { operator : u.operator, contents : _ }
    <$> traversePredicates f u.contents
traversePredicates f (Binary b) =
    (\left right -> Binary { operator : b.operator, left, right })
    <$> traversePredicates f b.left <*> traversePredicates f b.right
traversePredicates f (Quant q) =
    Quant <<< { operator : q.operator, variable : q.variable, contents : _ }
    <$> traversePredicates f q.contents

traverseFree :: forall a b f pred bound. Applicative f =>
    (a -> f b) -> WFF pred a bound -> f (WFF pred b bound)
traverseFree f (Pred p) =
    Pred <<< { predicate : p.predicate, variables : _ }
    <$> traverse (\x -> case x of
        Free y -> Free <$> f y
        Bound y i -> pure $ Bound y i
        ) p.variables
traverseFree _ (Nullary n) = pure $ Nullary n
traverseFree f (Unary u) =
    Unary <<< { operator : u.operator, contents : _ }
    <$> traverseFree f u.contents
traverseFree f (Binary b) =
    (\left right -> Binary { operator : b.operator, left, right })
    <$> traverseFree f b.left <*> traverseFree f b.right
traverseFree f (Quant q) =
    Quant <<< { operator : q.operator, variable : q.variable, contents : _ }
    <$> traverseFree f q.contents

traverseBound :: forall a b f pred free. Applicative f =>
    (a -> f b) -> WFF pred free a -> f (WFF pred free b)
traverseBound f (Pred p) =
    Pred <<< { predicate : p.predicate, variables : _ }
    <$> traverse (\x -> case x of
        Free y -> pure $ Free y
        Bound y i -> Bound <$> f y <*> pure i
        ) p.variables
traverseBound _ (Nullary n) = pure $ Nullary n
traverseBound f (Unary u) =
    Unary <<< { operator : u.operator, contents : _ }
    <$> traverseBound f u.contents
traverseBound f (Binary b) =
    (\left right -> Binary { operator : b.operator, left, right })
    <$> traverseBound f b.left <*> traverseBound f b.right
traverseBound f (Quant q) =
    (\variable contents -> Quant {operator : q.operator, variable, contents})
    <$> f q.variable <*> traverseBound f q.contents

freeVars :: forall pred free bound. Ord free => WFF pred free bound -> Set free
freeVars (Pred p) = foldMap getFree p.variables where
    getFree (Free x) = S.singleton x
    getFree (Bound _ _) = S.empty
freeVars (Nullary _) = S.empty
freeVars (Unary u) = freeVars u.contents
freeVars (Binary b) = freeVars b.left <> freeVars b.right
freeVars (Quant q) = freeVars q.contents

-- Render a WFF to show to the user
render :: WFF String String String -> String
render (Pred p) = case p.variables of
    [] -> p.predicate
    _ ->
        p.predicate <>
        "(" <>
        joinWith "," (renderVariable <$> p.variables) <>
        ")"
render (Nullary n) = renderNullaryOp n.operator
render (Unary u) = renderUnaryOp u.operator <> safeRender u.contents
render (Binary b) =
    safeRender b.left <> renderBinaryOp b.operator <> safeRender b.right
render (Quant q) =
    "(" <> renderQ q.operator <> q.variable <> ")" <> safeRender q.contents

-- Renders a WFF to be contained in another WFF
safeRender :: WFF String String String -> String
safeRender (Pred p) = render $ Pred p
safeRender (Quant q) = render $ Quant q
safeRender w = "(" <> render w <> ")"

-- Binds all instances of a variable to a quantifier
bindv :: forall pred var. Eq var =>
    var -> WFF pred var var -> WFF pred var var
bindv = bindAt 0

-- Bind all instances of a variable e to a quantifier l levels up
bindAt :: forall pred var. Eq var =>
    Int -> var -> WFF pred var var -> WFF pred var var
bindAt l e (Pred p) = Pred $ p
    { variables = (\v -> if v == Free e then Bound e l else v) <$> p.variables
    }
bindAt _ _ (Nullary n) = Nullary n
bindAt l e (Unary u) = Unary $ u { contents = bindAt l e u.contents }
bindAt l e (Binary b) =
    Binary $ b { left = bindAt l e b.left, right = bindAt l e b.right }
bindAt l e (Quant q) = Quant $ q { contents = bindAt (l+1) e q.contents }

-- Check all bound variables are in scope
closed :: forall pred free bound. WFF pred free bound -> Boolean
closed = closedAt 0

-- Check all bound variables are in scope given the number of wrapping quantifiers
closedAt :: forall pred free bound. Int -> WFF pred free bound -> Boolean
closedAt i (Pred p) = all (boundBelow i) p.variables
closedAt _ (Nullary n) = true
closedAt i (Unary u) = closedAt i u.contents
closedAt i (Binary b) = closedAt i b.left && closedAt i b.right
closedAt i (Quant q) = closedAt (i+1) q.contents

-- Builtin operators

botOp :: NullaryOp
botOp = NullaryOp "⊥"

negOp :: UnaryOp
negOp = UnaryOp "~"

andOp :: BinaryOp
andOp = BinaryOp "∧"

orOp :: BinaryOp
orOp = BinaryOp "∨"

impliesOp :: BinaryOp
impliesOp = BinaryOp "→"

bot :: forall pred free bound. WFF pred free bound
bot = Nullary { operator : botOp }

neg :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound
neg contents = Unary { operator : negOp, contents }

and :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound -> WFF pred free bound
and left right = Binary { operator : andOp, left, right }

or :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound -> WFF pred free bound
or left right = Binary { operator : orOp, left, right }

implies :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound -> WFF pred free bound
implies left right = Binary { operator : impliesOp, left, right }

-- Add a forall quantifier to a formula given the variable being bound
foralv :: forall pred var. Eq var =>
    var -> WFF pred var var -> WFF pred var var
foralv variable contents = Quant
    { operator : Forall
    , variable
    , contents : bindv variable contents
    }

-- Add an exists quantifier to a formula given the variable being bound
exists :: forall pred var. Eq var =>
    var -> WFF pred var var -> WFF pred var var
exists variable contents = Quant
    { operator : Exists
    , variable
    , contents : bindv variable contents
    }

-- Common operators for common formula connectives

infix 5 and as /\
infix 5 or as \/
infix 5 implies as ==>

-- Matching of WFFs

-- A type representing a value that is either known,
-- or a set of impossible values is known
data ValidFree v = Known v | Impossible (Set v)

derive instance eqValidFree :: Eq v => Eq (ValidFree v)

instance showValidFree :: Show v => Show (ValidFree v) where
    show (Known v) = "(Known " <> show v <> ")"
    show (Impossible s) = "(Impossible " <> show s <> ")"

-- Combine two possible values if they are compatible
combineFree :: forall y. Ord y =>
    ValidFree y -> ValidFree y -> Maybe (ValidFree y)
combineFree (Impossible s) (Impossible t) = Just $ Impossible $ s <> t
combineFree (Impossible s) (Known v) | not $ v `S.member` s = Just $ Known v
combineFree (Known v) (Impossible s) | not $ v `S.member` s = Just $ Known v
combineFree (Known v) (Known w) | v == w = Just $ Known v
combineFree _ _ = Nothing

{- A mapping between contents of formulas
Each input predicate is mapped to some formula containing
    - Output predicates
    - Free variables representing the input predicate's arguments
        or output free variables
    - Bound variables whose names don't matter so we use Unit
Each input free variable is mapped to either a known output or an unknown with
    disallowed values
-}
type Match inPred inFree outPred outFree =
    { predMatch :: Map inPred (WFF outPred (Either Int outFree) Unit)
    , freeMatch :: Map inFree (ValidFree outFree)
    }

-- An array of all possible mappings arising from matching two formulas
newtype Matches inPred inFree outPred outFree =
    Matches (Array (Match inPred inFree outPred outFree))

instance showMatches ::
    (Show inPred, Show inFree, Show outPred, Show outFree) =>
    Show (Matches inPred inFree outPred outFree) where
        show (Matches m) = "(Matches " <> show m <> ")"

-- Combine two mappings if they are compatible
joinMatch :: forall inPred inFree outPred outFree.
    Ord inPred => Ord inFree => Eq outPred => Ord outFree =>
    Match inPred inFree outPred outFree -> Match inPred inFree outPred outFree
    -> Maybe (Match inPred inFree outPred outFree)
joinMatch m n =
    { predMatch : _ , freeMatch : _ } <$> sequence newPred <*> sequence newFree
    where
        newPred = M.unionWith
            (\a b -> if a == b then a else Nothing)
            (Just <$> m.predMatch)
            (Just <$> n.predMatch)
        newFree = M.unionWith
            (\as bs -> do
                a <- as
                b <- bs
                combineFree a b
            )
            (Just <$> m.freeMatch)
            (Just <$> n.freeMatch)

instance semigroupMatches ::
    (Ord inPred, Ord inFree, Eq outPred, Ord outFree) =>
    Semigroup (Matches inPred inFree outPred outFree)
    where
        append (Matches ms) (Matches ns) = Matches $ do
            m <- ms
            n <- ns
            A.fromFoldable $ joinMatch m n

instance monoidMatches :: (Ord a, Ord b, Eq x, Ord y) =>
    Monoid (Matches a b x y)
    where
        mempty = Matches [ { predMatch : M.empty, freeMatch : M.empty } ]

-- Use a function that takes in the variables binding level relative to the
-- formula and its name to convert bound variables to free variables
boundToFree :: forall pred free bound.
    (Int -> bound -> Maybe free) -> WFF pred free bound -> WFF pred free bound
boundToFree = boundToFreeLevel 0

boundToFreeLevel :: forall pred free bound. Int ->
    (Int -> bound -> Maybe free) -> WFF pred free bound -> WFF pred free bound
boundToFreeLevel l f (Pred p) =
    Pred $ p { variables = mapBound <$> p.variables }
        where
            mapBound (Free x) = Free x
            mapBound (Bound x i) = case f (i-l) x of
                Nothing -> Bound x i
                Just y -> Free y
boundToFreeLevel _ _ (Nullary n) = Nullary n
boundToFreeLevel l f (Unary u) =
    Unary $ u { contents = boundToFreeLevel l f u.contents }
boundToFreeLevel l f (Binary b) = Binary $ b
    { left = boundToFreeLevel l f b.left
    , right = boundToFreeLevel l f b.right
    }
boundToFreeLevel l f (Quant q) =
    Quant $ q { contents = boundToFreeLevel (l+1) f q.contents }

-- What each input to a predicate maps to
type InVarMap outFree = Array (Variable (ValidFree outFree) Unit)

-- Match a WFF to one after substitutions were applied,
-- given a list of all free variables,
-- returning the relevant substitutions
match :: forall inPred inFree inBound outPred outFree outBound.
    Ord inPred => Ord inFree => Eq outPred => Ord outFree =>
    WFF inPred inFree inBound -> WFF outPred outFree outBound ->
    Matches inPred inFree outPred outFree
match (Pred p) w = Matches $ do
    -- Get a map target for each input to p
    mapping <- traverse getMapped p.variables
    -- Get a representation of the formula where variables may be replaced with
    -- inputs to the predicate
    mappedW <- mapVars mapping w
    -- Verify the resulting formula is closed
    if not $ closed mappedW then [] else pure unit
    -- Return the resulting match
    pure
        { predMatch : M.singleton p.predicate mappedW
        , freeMatch : fold $ A.zipWith matchVar p.variables mapping
        }
    where
        -- Use the mapping of inputs to the predicate to get all representations
        -- of the formula in terms of either themselves or inputs
        mapVars :: InVarMap outFree -> WFF outPred outFree outBound ->
            Array (WFF outPred (Either Int outFree) Unit)
        mapVars m = mapTraversal traverseFree (replaceFree m)
            >>> boundToFree (replaceBound m)
            >>> mapTraversal traverseBound (const unit)
            >>> traverseFree identity

        -- Set of all free variables in the output
        vars :: Set outFree
        vars = freeVars w

        -- Get all things a variable could map to
        getMapped :: Variable inFree inBound ->
            Array (Variable (ValidFree outFree) Unit)
        getMapped (Free x) = [Free $ Impossible vars] <>
            ((Free <<< Known) <$> S.toUnfoldable vars)
        getMapped (Bound _ i) = [Bound unit i]

        -- Create the mapping corresponding to a pair of input and output variables
        matchVar :: Variable inFree inBound -> Variable (ValidFree outFree) Unit
            -> Map inFree (ValidFree outFree)
        matchVar (Free x) (Free y) = M.singleton x y
        matchVar _ _ = M.empty

        -- Get all representations of a free variable as either itself or
        -- input variables that map to it
        replaceFree :: InVarMap outFree -> outFree -> Array (Either Int outFree)
        replaceFree m x =
            [ Right x ] <> (Left <$> getAllIndex (Free $ Known x) m)

        -- Turn a bound variable into the input variables that map to it if
        -- any exist
        replaceBound :: InVarMap outFree -> Int -> outBound ->
            Maybe (Array (Either Int outFree))
        replaceBound m i _ = case getAllIndex (Bound unit i) m of
            [] -> Nothing
            a -> Just $ Left <$> a
match (Nullary n) (Nullary m)
    | n.operator == m.operator = mempty
match (Unary u) (Unary v)
    | u.operator == v.operator = match u.contents v.contents
match (Binary b) (Binary c)
    | b.operator == c.operator = match b.left c.left <> match b.right c.right
match (Quant q) (Quant r)
    | q.operator == r.operator = match q.contents r.contents
match _ _ = Matches []

-- TODO comment code below here

data Type
    = FreeVar
    | BoundVar
    | Predicate Int

derive instance eqType :: Eq Type

instance showType :: Show Type where
    show FreeVar = "FreeVar"
    show BoundVar = "BoundVar"
    show (Predicate x) = "(Predicate " <> show x <> ")"

newtype Typing x = Typing (Maybe (Map x Type))

instance semigroupTyping :: Ord x => Semigroup (Typing x) where
    append (Typing (Just m)) (Typing (Just n))
        | all identity $ M.intersectionWith (==) m n
            = Typing $ Just $ M.union m n
    append _ _ = Typing Nothing

instance monoidTyping :: Ord x => Monoid (Typing x) where
    mempty = Typing $ Just $ M.empty

isWellTyped :: forall x. Typing x -> Boolean
isWellTyped (Typing (Just _)) = true
isWellTyped (Typing Nothing) = false

getVarTyping :: forall x. Variable x x -> Map x Type
getVarTyping (Free a) = M.singleton a FreeVar
getVarTyping (Bound a _) = M.singleton a BoundVar

getTyping :: forall x. Ord x => WFF x x x -> Typing x
getTyping (Pred p) = foldMap (Typing <<< Just) $
    [ M.singleton p.predicate $ Predicate $ length p.variables ]
    <> (getVarTyping <$> p.variables)
getTyping (Nullary _) = mempty
getTyping (Unary u) = getTyping u.contents
getTyping (Binary b) = getTyping b.left <> getTyping b.right
getTyping (Quant q) =
    (Typing $ Just $ M.singleton q.variable BoundVar) <> getTyping q.contents

-- Given a bound variable, check it is not bound elsewhere and how many times
-- it is used
validBoundVariable :: forall pred free bound. Eq bound =>
    Int -> bound -> WFF pred free bound -> Maybe Int
validBoundVariable l v (Pred p) =
    Just $ length $ A.filter (isBound v l) p.variables
validBoundVariable _ _ (Nullary _) = Just 0
validBoundVariable l v (Unary u) = validBoundVariable l v u.contents
validBoundVariable l v (Binary b) =
    (+) <$> validBoundVariable l v b.left <*> validBoundVariable l v b.right
validBoundVariable l v (Quant q)
    | q.variable == v = Nothing
    | otherwise = validBoundVariable (l+1) v q.contents

-- Verifies all bindings are correct, returning an error if there is any
validateBindings :: forall pred free bound. Eq bound =>
    WFF pred free bound -> Maybe String
validateBindings (Pred _) = Nothing
validateBindings (Nullary _) = Nothing
validateBindings (Unary u) = validateBindings u.contents
validateBindings (Binary b) =
    validateBindings b.left <> validateBindings b.right
validateBindings (Quant q) = case validBoundVariable 0 q.variable q.contents of
    Just n | n > 0 -> validateBindings q.contents
    Just _ -> Just "Quantifier does not bind any variables"
    Nothing -> Just "Nested quantifiers use same variable name"
