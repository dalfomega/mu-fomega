module MuFomega.Syntax.Lazy (
    ExprLazy (..),
) where

import Control.DeepSeq (NFData (rnf))
import Data.Text (Text)
import MuFomega.Syntax.Common (BinOp, Builtin, Var)

data ExprLazy
    = ENatural Integer
    | EBuiltin Builtin
    | EVar Var
    | EAnnot ExprLazy ExprLazy
    | ELam Text ExprLazy ExprLazy
    | EForall Text ExprLazy ExprLazy
    | ELet Text ExprLazy ExprLazy
    | EApp ExprLazy ExprLazy
    | EBinOp BinOp ExprLazy ExprLazy
    deriving (Eq, Show)

instance NFData ExprLazy where
    rnf expr =
        case expr of
            ENatural n -> rnf n
            EBuiltin b -> rnf b
            EVar v -> rnf v
            EAnnot body tipe -> rnf body `seq` rnf tipe
            ELam name tipe body -> rnf name `seq` rnf tipe `seq` rnf body
            EForall name tipe body -> rnf name `seq` rnf tipe `seq` rnf body
            ELet name value body -> rnf name `seq` rnf value `seq` rnf body
            EApp fn arg -> rnf fn `seq` rnf arg
            EBinOp op lhs rhs -> rnf op `seq` rnf lhs `seq` rnf rhs
