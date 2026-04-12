{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Normalize
  ( alphaNormalizeLazy
  , betaNormalizeLazy
  , normalizeLazy
  , alphaNormalizeStrict
  , betaNormalizeStrict
  , normalizeStrict
  ) where

import Data.Text (Text)
import MuFomega.Substitute (substituteLazy, substituteStrict)
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (NaturalFold, NaturalSubtract), Var (Var))
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EAnnot
      , EApp
      , EBinOp
      , EBuiltin
      , EForall
      , ELam
      , ELet
      , ENatural
      , EVar
      )
  )
import MuFomega.Syntax.Strict
  ( ExprStrict
      ( SEAnnot
      , SEApp
      , SEBinOp
      , SEBuiltin
      , SEForall
      , SELam
      , SELet
      , SENatural
      , SEVar
      )
  )

alphaNormalizeLazy :: ExprLazy -> ExprLazy
alphaNormalizeLazy = go []
  where
    go scope expr =
      case expr of
        ENatural n -> ENatural n
        EBuiltin b -> EBuiltin b
        EVar v -> EVar (renameVar scope v)
        EAnnot body tipe -> EAnnot (go scope body) (go scope tipe)
        ELam name tipe body -> ELam "_" (go scope tipe) (go (name : scope) body)
        EForall name tipe body -> EForall "_" (go scope tipe) (go (name : scope) body)
        ELet name value body -> ELet "_" (go scope value) (go (name : scope) body)
        EApp fn arg -> EApp (go scope fn) (go scope arg)
        EBinOp op lhs rhs -> EBinOp op (go scope lhs) (go scope rhs)

alphaNormalizeStrict :: ExprStrict -> ExprStrict
alphaNormalizeStrict = go []
  where
    go scope expr =
      case expr of
        SENatural n -> SENatural n
        SEBuiltin b -> SEBuiltin b
        SEVar v -> SEVar (renameVar scope v)
        SEAnnot body tipe -> SEAnnot (go scope body) (go scope tipe)
        SELam name tipe body -> SELam "_" (go scope tipe) (go (name : scope) body)
        SEForall name tipe body -> SEForall "_" (go scope tipe) (go (name : scope) body)
        SELet name value body -> SELet "_" (go scope value) (go (name : scope) body)
        SEApp fn arg -> SEApp (go scope fn) (go scope arg)
        SEBinOp op lhs rhs -> SEBinOp op (go scope lhs) (go scope rhs)

renameVar :: [Text] -> Var -> Var
renameVar scope (Var name idx) =
  case resolveBoundDepth scope name idx 0 0 of
    Just depth -> Var "_" depth
    Nothing
      | name == "_" -> Var "_" (idx + fromIntegral (length scope))
      | otherwise -> Var name idx

resolveBoundDepth :: [Text] -> Text -> Word -> Word -> Word -> Maybe Word
resolveBoundDepth scope target needle depth seen =
  case scope of
    [] -> Nothing
    binder : rest
      | binder == target && seen == needle -> Just depth
      | binder == target -> resolveBoundDepth rest target needle (depth + 1) (seen + 1)
      | otherwise -> resolveBoundDepth rest target needle (depth + 1) seen

betaNormalizeLazy :: ExprLazy -> ExprLazy
betaNormalizeLazy = go
  where
    go expr =
      case expr of
        ENatural n -> ENatural n
        EBuiltin b -> EBuiltin b
        EVar v -> EVar v
        EAnnot body _ -> go body
        ELam name tipe body -> ELam name (go tipe) (go body)
        EForall name tipe body -> EForall name (go tipe) (go body)
        ELet name value body -> go (substituteLazy name 0 (go value) body)
        EApp fn arg ->
          let fn' = go fn
              arg' = go arg
           in case fn' of
                ELam name _ body -> go (substituteLazy name 0 arg' body)
                _ -> evalBuiltinAppLazy go fn' arg'
        EBinOp op lhs rhs ->
          let lhs' = go lhs
              rhs' = go rhs
           in reduceBinOpLazy op lhs' rhs'

betaNormalizeStrict :: ExprStrict -> ExprStrict
betaNormalizeStrict = go
  where
    go expr =
      case expr of
        SENatural n -> SENatural n
        SEBuiltin b -> SEBuiltin b
        SEVar v -> SEVar v
        SEAnnot body _ -> go body
        SELam name tipe body -> SELam name (go tipe) (go body)
        SEForall name tipe body -> SEForall name (go tipe) (go body)
        SELet name value body -> go (substituteStrict name 0 (go value) body)
        SEApp fn arg ->
          let fn' = go fn
              arg' = go arg
           in case fn' of
                SELam name _ body -> go (substituteStrict name 0 arg' body)
                _ -> evalBuiltinAppStrict go fn' arg'
        SEBinOp op lhs rhs ->
          let lhs' = go lhs
              rhs' = go rhs
           in reduceBinOpStrict op lhs' rhs'

normalizeLazy :: ExprLazy -> ExprLazy
normalizeLazy = alphaNormalizeLazy . betaNormalizeLazy

normalizeStrict :: ExprStrict -> ExprStrict
normalizeStrict = alphaNormalizeStrict . betaNormalizeStrict

reduceBinOpLazy :: BinOp -> ExprLazy -> ExprLazy -> ExprLazy
reduceBinOpLazy op lhs rhs =
  case (op, lhs, rhs) of
    (Plus, ENatural 0, _) -> rhs
    (Plus, _, ENatural 0) -> lhs
    (Plus, ENatural l, ENatural r) -> ENatural (l + r)
    (Times, ENatural l, ENatural r) -> ENatural (l * r)
    _ -> EBinOp op lhs rhs

reduceBinOpStrict :: BinOp -> ExprStrict -> ExprStrict -> ExprStrict
reduceBinOpStrict op lhs rhs =
  case (op, lhs, rhs) of
    (Plus, SENatural 0, _) -> rhs
    (Plus, _, SENatural 0) -> lhs
    (Plus, SENatural l, SENatural r) -> SENatural (l + r)
    (Times, SENatural l, SENatural r) -> SENatural (l * r)
    _ -> SEBinOp op lhs rhs

evalBuiltinAppLazy :: (ExprLazy -> ExprLazy) -> ExprLazy -> ExprLazy -> ExprLazy
evalBuiltinAppLazy normalize fn arg =
  case collectAppsLazy (EApp fn arg) of
    (EBuiltin NaturalSubtract, a : b : rest) ->
      case (a, b) of
        (ENatural x, ENatural y) ->
          normalize (applyRestLazy (ENatural (max 0 (y - x))) rest)
        _ -> applyRestLazy (EApp (EApp (EBuiltin NaturalSubtract) a) b) rest
    (EBuiltin NaturalFold, n : tipe : succFn : zero : rest) ->
      case n of
        ENatural k
          | k >= 0 ->
              let folded = foldNaturalLazy normalize k succFn zero
               in normalize (applyRestLazy folded rest)
        _ -> applyRestLazy (EApp (EApp (EApp (EApp (EBuiltin NaturalFold) n) tipe) succFn) zero) rest
    _ -> EApp fn arg

evalBuiltinAppStrict :: (ExprStrict -> ExprStrict) -> ExprStrict -> ExprStrict -> ExprStrict
evalBuiltinAppStrict normalize fn arg =
  case collectAppsStrict (SEApp fn arg) of
    (SEBuiltin NaturalSubtract, a : b : rest) ->
      case (a, b) of
        (SENatural x, SENatural y) ->
          normalize (applyRestStrict (SENatural (max 0 (y - x))) rest)
        _ -> applyRestStrict (SEApp (SEApp (SEBuiltin NaturalSubtract) a) b) rest
    (SEBuiltin NaturalFold, n : tipe : succFn : zero : rest) ->
      case n of
        SENatural k
          | k >= 0 ->
              let folded = foldNaturalStrict normalize k succFn zero
               in normalize (applyRestStrict folded rest)
        _ -> applyRestStrict (SEApp (SEApp (SEApp (SEApp (SEBuiltin NaturalFold) n) tipe) succFn) zero) rest
    _ -> SEApp fn arg

foldNaturalLazy :: (ExprLazy -> ExprLazy) -> Integer -> ExprLazy -> ExprLazy -> ExprLazy
foldNaturalLazy normalize k succFn zero =
  apply k (normalize zero)
  where
    apply n acc
      | n <= 0 = acc
      | otherwise = apply (n - 1) (normalize (EApp succFn acc))

foldNaturalStrict :: (ExprStrict -> ExprStrict) -> Integer -> ExprStrict -> ExprStrict -> ExprStrict
foldNaturalStrict normalize k succFn zero =
  apply k (normalize zero)
  where
    apply n acc
      | n <= 0 = acc
      | otherwise = apply (n - 1) (normalize (SEApp succFn acc))

collectAppsLazy :: ExprLazy -> (ExprLazy, [ExprLazy])
collectAppsLazy = go []
  where
    go args expr =
      case expr of
        EApp fn arg -> go (arg : args) fn
        root -> (root, args)

collectAppsStrict :: ExprStrict -> (ExprStrict, [ExprStrict])
collectAppsStrict = go []
  where
    go args expr =
      case expr of
        SEApp fn arg -> go (arg : args) fn
        root -> (root, args)

applyRestLazy :: ExprLazy -> [ExprLazy] -> ExprLazy
applyRestLazy = foldl EApp

applyRestStrict :: ExprStrict -> [ExprStrict] -> ExprStrict
applyRestStrict = foldl SEApp
