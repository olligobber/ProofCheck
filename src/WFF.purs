module WFF (
    UnaryOp,
    BinaryOp,
    WFF(..)
) where

import Prelude
    ( class Functor, class Apply, class Applicative, class Bind, class Monad
    , (<>), ($), (<$>), (<*>), (>>=), (<<<))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Traversable (class Traversable, sequence, traverseDefault)

type UnaryOp =
    { onT :: Boolean
    , onF :: Boolean
    , symbol :: String
    }

type BinaryOp =
    { onTT :: Boolean
    , onTF :: Boolean
    , onFT :: Boolean
    , onFF :: Boolean
    , symbol :: String
    }

data WFF a
    = Prop a
    | Unary { operator :: UnaryOp, contents :: WFF a }
    | Binary { operator :: BinaryOp, left :: WFF a, right :: WFF a }

instance functorWFF :: Functor WFF where
    map f (Prop p) = Prop $ f p
    map f (Unary u) = Unary $ u { contents = f <$> u.contents }
    map f (Binary b) =
        Binary $ b { left = f <$> b.left, right = f <$> b.right}

instance applyWFF :: Apply WFF where
    apply (Prop f) w = f <$> w
    apply (Unary u) w = Unary $ u { contents = u.contents <*> w }
    apply (Binary b) w =
        Binary $ b { left = b.left <*> w, right = b.right <*> w }

instance applicativeWFF :: Applicative WFF where
    pure = Prop

instance bindWFF :: Bind WFF where
    bind (Prop p) f = f p
    bind (Unary u) f = Unary $ u { contents = u.contents >>= f }
    bind (Binary b) f =
        Binary $ b { left = b.left >>= f, right = b.right >>= f}

instance monadWFF :: Monad WFF

instance foldableWFF :: Foldable WFF where
    foldMap f (Prop p) = f p
    foldMap f (Unary u) = foldMap f u.contents
    foldMap f (Binary b) = foldMap f b.left <> foldMap f b.right
    foldl = foldlDefault
    foldr = foldrDefault

instance traversableWFF :: Traversable WFF where
    sequence (Prop p) = Prop <$> p
    sequence (Unary u) = Unary <<< u { contents = _ } <$> sequence u.contents
    sequence (Binary b) = Binary <$> (b { left = _, right = _}
        <$> sequence b.left
        <*> sequence b.right)
    traverse = traverseDefault

-- Render a WFF to show to the user
render :: WFF String -> String
render (Prop p) = p
render (Unary u) = u.operator.symbol <> safeRender u.contents
render (Binary b) = safeRender b.left <> b.operator.symbol <> safeRender b.right

-- Renders a WFF to be contained in another WFF
safeRender :: WFF String -> String
safeRender (Prop p) = p
safeRender w = "(" <> render w <> ")"

-- Evaluate a WFF
eval :: WFF Boolean -> Boolean
eval (Prop p) = p
eval (Unary u) =
    if eval u.contents then
        u.operator.onT
    else
        u.operator.onF
eval (Binary b) =
    if eval b.left then
        if eval b.right then
            b.operator.onTT
        else
            b.operator.onTF
    else
        if eval b.right then
            b.operator.onFT
        else
            b.operator.onFF

negOp :: UnaryOp
negOp =
    { symbol : "~"
    , onT : false
    , onF : true
    }

andOp :: BinaryOp
andOp =
    { symbol : "&"
    , onTT : true
    , onTF : false
    , onFT : false
    , onFF : false
    }

orOp :: BinaryOp
orOp =
    { symbol : "|"
    , onTT : true
    , onTF : true
    , onFT : true
    , onFF : false
    }

impliesOp :: BinaryOp
impliesOp =
    { symbol : "->"
    , onTT : true
    , onTF : false
    , onFT : true
    , onFF : true
    }
