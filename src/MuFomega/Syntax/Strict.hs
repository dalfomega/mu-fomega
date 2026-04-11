module MuFomega.Syntax.Strict
  ( ExprStrict (..)
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Text (Text)
import MuFomega.Syntax.Common (BinOp, Builtin, Var)

data ExprStrict
  = SENatural !Integer
  | SEBuiltin !Builtin
  | SEVar !Var
  | SEAnnot !ExprStrict !ExprStrict
  | SELam !Text !ExprStrict !ExprStrict
  | SEForall !Text !ExprStrict !ExprStrict
  | SELet !Text !ExprStrict !ExprStrict
  | SEApp !ExprStrict !ExprStrict
  | SEBinOp !BinOp !ExprStrict !ExprStrict
  deriving (Eq, Show)

instance NFData ExprStrict where
  rnf expr =
    case expr of
      SENatural n -> rnf n
      SEBuiltin b -> rnf b
      SEVar v -> rnf v
      SEAnnot body tipe -> rnf body `seq` rnf tipe
      SELam name tipe body -> rnf name `seq` rnf tipe `seq` rnf body
      SEForall name tipe body -> rnf name `seq` rnf tipe `seq` rnf body
      SELet name value body -> rnf name `seq` rnf value `seq` rnf body
      SEApp fn arg -> rnf fn `seq` rnf arg
      SEBinOp op lhs rhs -> rnf op `seq` rnf lhs `seq` rnf rhs
