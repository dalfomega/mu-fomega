module MuFomega.Traversal
  ( mapExprLazy
  , foldExprLazy
  , mapExprWithBindersLazy
  , mapExprStrict
  , foldExprStrict
  , mapExprWithBindersStrict
  ) where

import Data.Text (Text)
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

mapExprLazy :: (ExprLazy -> ExprLazy) -> ExprLazy -> ExprLazy
mapExprLazy f = go
  where
    go expr =
      f $ case expr of
        ENatural n -> ENatural n
        EBuiltin b -> EBuiltin b
        EVar v -> EVar v
        EAnnot body tipe -> EAnnot (go body) (go tipe)
        ELam name tipe body -> ELam name (go tipe) (go body)
        EForall name tipe body -> EForall name (go tipe) (go body)
        ELet name value body -> ELet name (go value) (go body)
        EApp fn arg -> EApp (go fn) (go arg)
        EBinOp op lhs rhs -> EBinOp op (go lhs) (go rhs)

foldExprLazy :: (a -> ExprLazy -> a) -> a -> ExprLazy -> a
foldExprLazy step = go
  where
    go acc expr =
      case expr of
        ENatural _ -> step acc expr
        EBuiltin _ -> step acc expr
        EVar _ -> step acc expr
        EAnnot body tipe ->
          let acc1 = step acc expr
              acc2 = go acc1 body
           in go acc2 tipe
        ELam _ tipe body ->
          let acc1 = step acc expr
              acc2 = go acc1 tipe
           in go acc2 body
        EForall _ tipe body ->
          let acc1 = step acc expr
              acc2 = go acc1 tipe
           in go acc2 body
        ELet _ value body ->
          let acc1 = step acc expr
              acc2 = go acc1 value
           in go acc2 body
        EApp fn arg ->
          let acc1 = step acc expr
              acc2 = go acc1 fn
           in go acc2 arg
        EBinOp _ lhs rhs ->
          let acc1 = step acc expr
              acc2 = go acc1 lhs
           in go acc2 rhs

mapExprWithBindersLazy ::
  (ctx -> ExprLazy -> ExprLazy) ->
  (ctx -> Text -> ctx) ->
  ctx ->
  ExprLazy ->
  ExprLazy
mapExprWithBindersLazy f enter = go
  where
    go ctx expr =
      ctx `seq` f ctx $ case expr of
        ENatural n -> ENatural n
        EBuiltin b -> EBuiltin b
        EVar v -> EVar v
        EAnnot body tipe -> EAnnot (go ctx body) (go ctx tipe)
        ELam name tipe body -> ELam name (go ctx tipe) (go (enter ctx name) body)
        EForall name tipe body -> EForall name (go ctx tipe) (go (enter ctx name) body)
        ELet name value body -> ELet name (go ctx value) (go (enter ctx name) body)
        EApp fn arg -> EApp (go ctx fn) (go ctx arg)
        EBinOp op lhs rhs -> EBinOp op (go ctx lhs) (go ctx rhs)

mapExprStrict :: (ExprStrict -> ExprStrict) -> ExprStrict -> ExprStrict
mapExprStrict f = go
  where
    go expr =
      f $ case expr of
        SENatural n -> SENatural n
        SEBuiltin b -> SEBuiltin b
        SEVar v -> SEVar v
        SEAnnot body tipe -> SEAnnot (go body) (go tipe)
        SELam name tipe body -> SELam name (go tipe) (go body)
        SEForall name tipe body -> SEForall name (go tipe) (go body)
        SELet name value body -> SELet name (go value) (go body)
        SEApp fn arg -> SEApp (go fn) (go arg)
        SEBinOp op lhs rhs -> SEBinOp op (go lhs) (go rhs)

foldExprStrict :: (a -> ExprStrict -> a) -> a -> ExprStrict -> a
foldExprStrict step = go
  where
    go acc expr =
      case expr of
        SENatural _ -> step acc expr
        SEBuiltin _ -> step acc expr
        SEVar _ -> step acc expr
        SEAnnot body tipe ->
          let acc1 = step acc expr
              acc2 = go acc1 body
           in go acc2 tipe
        SELam _ tipe body ->
          let acc1 = step acc expr
              acc2 = go acc1 tipe
           in go acc2 body
        SEForall _ tipe body ->
          let acc1 = step acc expr
              acc2 = go acc1 tipe
           in go acc2 body
        SELet _ value body ->
          let acc1 = step acc expr
              acc2 = go acc1 value
           in go acc2 body
        SEApp fn arg ->
          let acc1 = step acc expr
              acc2 = go acc1 fn
           in go acc2 arg
        SEBinOp _ lhs rhs ->
          let acc1 = step acc expr
              acc2 = go acc1 lhs
           in go acc2 rhs

mapExprWithBindersStrict ::
  (ctx -> ExprStrict -> ExprStrict) ->
  (ctx -> Text -> ctx) ->
  ctx ->
  ExprStrict ->
  ExprStrict
mapExprWithBindersStrict f enter = go
  where
    go ctx expr =
      ctx `seq` f ctx $ case expr of
        SENatural n -> SENatural n
        SEBuiltin b -> SEBuiltin b
        SEVar v -> SEVar v
        SEAnnot body tipe -> SEAnnot (go ctx body) (go ctx tipe)
        SELam name tipe body -> SELam name (go ctx tipe) (go (enter ctx name) body)
        SEForall name tipe body -> SEForall name (go ctx tipe) (go (enter ctx name) body)
        SELet name value body -> SELet name (go ctx value) (go (enter ctx name) body)
        SEApp fn arg -> SEApp (go ctx fn) (go ctx arg)
        SEBinOp op lhs rhs -> SEBinOp op (go ctx lhs) (go ctx rhs)
