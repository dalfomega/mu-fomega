module Syntax where

import qualified Numeric.Natural as Nat

data Expr
    = NatLit Nat.Natural
    | FreeVar Int Expr
    | App Expr Expr
    | Lam (Expr → Expr) (Maybe Expr)
    | Builtin Builtins
    | TypeLit TypeLiteral
    | Forall (Expr → Expr) (Maybe Expr)
    deriving (Eq, Show)

data Builtins
    = BNaturalSubtract
    | BNaturalPlus
    | BNaturalTimes
    deriving (Eq, Show)

data TypeLiteral = TLNatural | TLType | TLKind
    deriving (Eq, Show)
