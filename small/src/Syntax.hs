module Syntax where

import qualified Numeric.Natural as Nat

newtype DBI = DBI Nat.Natural
    deriving stock (Eq, Show, Ord)

newtype InternedName = InternedName Int
    deriving (Eq, Show, Ord)

data SynExpr
    = SNatLit Nat.Natural -- 123
    | SVar InternedName DBI -- x@1
    | SApp SynExpr SynExpr -- f x
    | SLam InternedName SynExpr SynExpr -- λ(x : t) → y
    | SForall InternedName SynExpr SynExpr -- ∀(x : t) → y
    | SLet InternedName SynExpr SynExpr -- let x = y in z
    | SBuiltin Builtins -- Natural/subtract, +, *, Natural, Type, Kind
    | STypeAnn SynExpr SynExpr -- x : t
    deriving stock (Eq, Show, Ord)

-- data ValExpr -- This is created after typechecking. Each expression should already be well-typed. For now, we omit the type annotations.
--     = NatLit Nat.Natural
--     | FreeVar Int
--     | App ValExpr ValExpr
--     | Lam (ValExpr -> ValExpr)   -- The type of a Lam must be a Forall.
--     | Forall (ValExpr -> ValExpr)    -- The type of a Forall must be a TypeLiteral.
--     | Builtin Builtins   -- Each builtin has a fixed known type, no need to have the type annotation here.

data BuiltinFunctions = BNaturalSubtract
    deriving stock (Eq, Show, Ord)

data BuiltinOperators
    = BNaturalPlus
    | BNaturalTimes
    deriving stock (Eq, Show, Ord)

data Builtins
    = BFunction BuiltinFunctions
    | BOperator BuiltinOperators
    | BTypeLit TypeLiteral
    deriving stock (Eq, Show, Ord)

data TypeLiteral = TLNatural | TLType | TLKind
    deriving stock (Eq, Show, Ord)
