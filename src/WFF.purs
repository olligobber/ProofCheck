module WFF
    ( UnaryOp(..)
    , BinaryOp(..)
    , Quantifier(..)
    , Variable(..)
    , WFF(..)
    , render
    , renderUnaryOp
    , renderBinaryOp
    , traversePredicates
    , traverseFree
    , traverseBound
    , prop
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
    , Match
    , Matches(..)
    ) where

import Prelude
    ( class Eq, class Ord, class Show, class Semigroup, class Applicative
    , class Monoid
    , Unit
    , show, identity, otherwise, unit, bind, pure, not
    , (<>), ($), (<$>), (<<<), (==), (&&), (+), (<), (<*>)
    )
import Data.String (joinWith)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (all, foldMap)
import Data.Map (Map)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (traverse)

newtype UnaryOp = UnaryOp String

derive instance eqUnaryOp :: Eq UnaryOp
derive instance ordUnaryOp :: Ord UnaryOp

instance showUnaryOp :: Show UnaryOp where
    show (UnaryOp s) = "UnaryOp " <> show s

renderUnaryOp :: UnaryOp -> String
renderUnaryOp (UnaryOp s) = s

newtype BinaryOp = BinaryOp String

derive instance eqBinaryOp :: Eq BinaryOp
derive instance ordBinaryOp :: Ord BinaryOp

instance showBinaryOp :: Show BinaryOp where
    show (BinaryOp s) = "BinaryOp " <> show s

renderBinaryOp :: BinaryOp -> String
renderBinaryOp (BinaryOp s) = s

data Quantifier = Forall | Exists

derive instance eqQuantifier :: Eq Quantifier
derive instance ordQuantifier :: Ord Quantifier

instance showQuantifier :: Show Quantifier where
    show Forall = "Forall"
    show Exists = "Exists"

renderQ :: Quantifier -> String
renderQ Forall = "∀"
renderQ Exists = "∃"

data Variable free bound
    = Free free
    | Bound bound Int

derive instance eqVariable :: (Eq free, Eq bound) => Eq (Variable free bound)
derive instance ordVariable :: (Ord free, Ord bound) =>
    Ord (Variable free bound)

instance showVariable :: (Show free, Show bound) =>
    Show (Variable free bound) where
        show (Free x) = "Free " <> show x
        show (Bound x i) = "Bound " <> show x <> " " <> show i

renderVariable :: Variable String String -> String
renderVariable (Free x) = x
renderVariable (Bound x _) = x

boundBelow :: forall free bound. Int -> Variable free bound -> Boolean
boundBelow _ (Free _) = true
boundBelow b (Bound _ i) = i < b

-- WFF with a type for propositions, predicates, and variables
data WFF pred free bound
    = Pred { predicate :: pred, variables :: Array (Variable free bound) }
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

prop :: forall pred free bound. pred -> WFF pred free bound
prop x = Pred { predicate : x, variables : []}

derive instance eqWFF :: (Eq pred, Eq free, Eq bound) =>
    Eq (WFF pred free bound)
derive instance ordWFF :: (Ord pred, Ord free, Ord bound) =>
    Ord (WFF pred free bound)

instance showWFF :: (Show pred, Show free, Show bound) =>
    Show (WFF pred free bound) where
        show (Pred p) = "Pred " <> show p
        show (Unary u) = "Unary " <> show u
        show (Binary b) = "Binary " <> show b
        show (Quant q) = "Quant " <> show q

traversePredicates :: forall a b f free bound. Applicative f =>
    (a -> f b) -> WFF a free bound -> f (WFF b free bound)
traversePredicates f (Pred p) =
    Pred <<< { predicate : _, variables : p.variables } <$> f p.predicate
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
freeVars (Unary u) = freeVars u.contents
freeVars (Binary b) = freeVars b.left <> freeVars b.right
freeVars (Quant q) = freeVars q.contents

mapVars :: forall pred a b x.
    (Variable a b -> Variable x Unit) -> WFF pred a b -> WFF pred x Unit
mapVars f (Pred p) = Pred $ p { variables = f <$> p.variables }
mapVars f (Unary u) = Unary $ u { contents = mapVars f u.contents }
mapVars f (Binary b) = Binary $ b
    { left = mapVars f b.left
    , right = mapVars f b.right
    }
mapVars f (Quant q) = Quant $ q
    { variable = unit
    , contents = mapVars f q.contents
    }

-- Render a WFF to show to the user
render :: WFF String String String -> String
render (Pred p) = case p.variables of
    [] -> p.predicate
    _ ->
        p.predicate <>
        "(" <>
        joinWith "," (renderVariable <$> p.variables) <>
        ")"
render (Unary u) = renderUnaryOp u.operator <> safeRender u.contents
render (Binary b) =
    safeRender b.left <> renderBinaryOp b.operator <> safeRender b.right
render (Quant q) =
    "(" <> renderQ q.operator <> q.variable <> ")" <> quantRender q.contents

-- Renders a WFF to be contained in another WFF
safeRender :: WFF String String String -> String
safeRender (Pred p) = render $ Pred p
safeRender w = "(" <> render w <> ")"

-- Renders a WFF to be contained in a quantified WFF
quantRender :: WFF String String String -> String
quantRender (Quant q) = render $ Quant q
quantRender w = safeRender w

-- Binds all instances of a variable
bindv :: forall pred var. Eq var =>
    var -> WFF pred var var -> WFF pred var var
bindv = bindAt 0

bindAt :: forall pred var. Eq var =>
    Int -> var -> WFF pred var var -> WFF pred var var
bindAt l e (Pred p) = Pred $ p
    { variables = (\v -> if v == Free e then Bound e l else v) <$> p.variables
    }
bindAt l e (Unary u) = Unary $ u { contents = bindAt l e u.contents }
bindAt l e (Binary b) =
    Binary $ b { left = bindAt l e b.left, right = bindAt l e b.right }
bindAt l e (Quant q) = Quant $ q { contents = bindAt (l+1) e q.contents }

-- Check all bound variables are in scope
closed :: forall pred free bound. WFF pred free bound -> Boolean
closed = closedAt 0

closedAt :: forall pred free bound. Int -> WFF pred free bound -> Boolean
closedAt i (Pred p) = all (boundBelow i) p.variables
closedAt i (Unary u) = closedAt i u.contents
closedAt i (Binary b) = closedAt i b.left && closedAt i b.right
closedAt i (Quant q) = closedAt (i+1) q.contents

-- Builtin operators
neg :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound
neg contents = Unary { operator : UnaryOp "~", contents }

and :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound -> WFF pred free bound
and left right = Binary { operator : BinaryOp "∧", left, right }

or :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound -> WFF pred free bound
or left right = Binary { operator : BinaryOp "∨", left, right }

implies :: forall pred free bound.
    WFF pred free bound -> WFF pred free bound -> WFF pred free bound
implies left right = Binary { operator : BinaryOp "⇒", left, right }

foralv :: forall pred var. Eq var =>
    var -> WFF pred var var -> WFF pred var var
foralv variable contents = Quant
    { operator : Forall
    , variable
    , contents : bindv variable contents
    }

exists :: forall pred var. Eq var =>
    var -> WFF pred var var -> WFF pred var var
exists variable contents = Quant
    { operator : Exists
    , variable
    , contents : bindv variable contents
    }

infix 5 and as /\
infix 5 or as \/
infix 5 implies as ==>

type Match a b x y =
    { predMatch :: Map a (WFF x (Either Int y) Unit)
    , freeMatch :: Map b (Either (Set y) y)
    }

compatibleFree :: forall y. Ord y =>
    Either (Set y) y -> Either (Set y) y -> Boolean
compatibleFree (Left _) (Left _) = true
compatibleFree (Left s) (Right v) = not $ v `S.member` s
compatibleFree (Right v) (Left s) = not $ v `S.member` s
compatibleFree (Right v) (Right w) = v == w

combineFree :: forall y. Ord y =>
    Either (Set y) y -> Either (Set y) y -> Either (Set y) y
combineFree (Left s) (Left t) = Left $ s <> t
combineFree (Right v) _ = Right v
combineFree _ (Right v) = Right v

data Matches a b x y = Matches (Array (Match a b x y))

joinMatch :: forall a b x y. Ord a => Ord b => Eq x => Ord y =>
    Match a b x y -> Match a b x y -> Maybe (Match a b x y)
joinMatch m n
    | (all identity $ M.intersectionWith (==) m.predMatch n.predMatch)
        && (all identity $
            M.intersectionWith compatibleFree m.freeMatch n.freeMatch)
                = Just
                    { predMatch : M.union m.predMatch n.predMatch
                    , freeMatch :
                        M.unionWith combineFree m.freeMatch n.freeMatch
                    }
    | otherwise = Nothing

instance semigroupMatches :: (Ord a, Ord b, Eq x, Ord y) =>
    Semigroup (Matches a b x y) where
        append (Matches ms) (Matches ns) = Matches $ do
            m <- ms
            n <- ns
            A.fromFoldable $ joinMatch m n

instance monoidMatching :: (Ord a, Ord b, Eq x, Ord y) =>
    Monoid (Matches a b x y) where
        mempty = Matches [ { predMatch : M.empty, freeMatch : M.empty } ]

-- Match a WFF to one after substitutions were applied,
-- given a list of all free variables,
-- returning the relevant substitutions
match :: forall a b c x y z. Ord a => Ord b => Eq x => Ord y =>
    WFF a b c -> WFF x y z -> Matches a b x y
match (Pred p) w = Matches $ do
    mapping <- traverse getMapped p.variables
    let mappedW = mapVars (replace mapping) w
    if closed mappedW then
        pure
            { predMatch : M.singleton p.predicate mappedW
            , freeMatch :
                foldMap identity $ A.zipWith matchVar p.variables mapping
            }
    else
        []
    where
        vars :: Set y
        vars = freeVars w
        getMapped :: Variable b c -> Array (Variable (Either (Set y) y) Unit)
        getMapped (Free x) =
            [Free $ Left vars] <> ((Free <<< Right) <$> S.toUnfoldable vars)
        getMapped (Bound _ i) = [Bound unit i]
        matchVar :: Variable b c -> Variable (Either (Set y) y) Unit ->
            Map b (Either (Set y) y)
        matchVar (Free x) (Free y) = M.singleton x y
        matchVar _ _ = M.empty
        replace :: Array (Variable (Either (Set y) y) Unit) ->
            Variable y z -> Variable (Either Int y) Unit
        replace m (Free x) = case A.elemIndex (Free $ Right x) m of
            Just i -> Free $ Left i
            Nothing -> Free $ Right x
        replace m (Bound _ i) = case A.elemIndex (Bound unit i) m of
            Just j -> Free $ Left j
            Nothing -> Bound unit i
match (Unary u) (Unary v)
    | u.operator == v.operator = match u.contents v.contents
match (Binary b) (Binary c)
    | b.operator == c.operator = match b.left c.left <> match b.right c.right
match (Quant q) (Quant r)
    | q.operator == r.operator = match q.contents r.contents
match _ _ = Matches []
