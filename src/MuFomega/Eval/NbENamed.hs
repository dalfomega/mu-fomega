module MuFomega.Eval.NbENamed
  ( normalizeLazy
  , normalizeStrict
  ) where

import qualified MuFomega.Eval.NbECommon as Common
import MuFomega.Syntax.Lazy (ExprLazy)
import MuFomega.Syntax.Strict (ExprStrict)

normalizeLazy :: ExprLazy -> ExprLazy
normalizeLazy = Common.normalizeLazy

normalizeStrict :: ExprStrict -> ExprStrict
normalizeStrict = Common.normalizeStrict
