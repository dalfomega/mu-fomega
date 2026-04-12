{-# LANGUAGE OverloadedStrings #-}

module MuFomega.TypeCheck
  ( TypeError (..)
  , inferTypeLazy
  , checkTypeLazy
  , inferTypeStrict
  , checkTypeStrict
  ) where

import Control.Monad (unless)
import Data.Text (Text)
import MuFomega.Normalize (normalizeLazy)
import MuFomega.Substitute (substituteLazy)
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
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
import MuFomega.Syntax.Strict (ExprStrict)
import MuFomega.Syntax.Convert (toLazy, toStrict)

data TypeError
  = TypeMismatch ExprLazy ExprLazy
  | NotAFunction ExprLazy
  | UnboundVariable Var
  | InvalidTypeExpression ExprLazy
  | KindHasNoType
  | TopLevelFreeVariable Var
  deriving (Eq, Show)

data Sort = SortType | SortKind

data Binding = Binding
  { bindingName :: Text
  , bindingType :: ExprLazy
  , bindingValue :: Maybe ExprLazy
  }

type Context = [Binding]

inferTypeLazy :: ExprLazy -> Either TypeError ExprLazy
inferTypeLazy expr =
  case inferTypeWithContext [] expr of
    Left (UnboundVariable v@(Var name _))
      | appearsAsBinderName expr name -> Left (UnboundVariable v)
      | otherwise -> Left (TopLevelFreeVariable v)
    other -> other

checkTypeLazy :: ExprLazy -> ExprLazy -> Either TypeError ()
checkTypeLazy expr expected = checkTypeWithContext [] expr expected

inferTypeStrict :: ExprStrict -> Either TypeError ExprStrict
inferTypeStrict expr = toStrict <$> inferTypeLazy (toLazy expr)

checkTypeStrict :: ExprStrict -> ExprStrict -> Either TypeError ()
checkTypeStrict expr expected = checkTypeLazy (toLazy expr) (toLazy expected)

inferTypeWithContext :: Context -> ExprLazy -> Either TypeError ExprLazy
inferTypeWithContext ctx expr =
  case expr of
    ENatural _ -> Right (EBuiltin Natural)
    EBuiltin Natural -> Right (EBuiltin Type)
    EBuiltin NaturalSubtract ->
      Right (EForall "_" (EBuiltin Natural) (EForall "_" (EBuiltin Natural) (EBuiltin Natural)))
    EBuiltin NaturalFold ->
      Right
        ( EForall
            "_"
            (EBuiltin Natural)
            ( EForall
                "r"
                (EBuiltin Type)
                ( EForall
                    "_"
                    (EForall "_" (EVar (Var "r" 0)) (EVar (Var "r" 0)))
                    (EForall "_" (EVar (Var "r" 0)) (EVar (Var "r" 0)))
                )
            )
        )
    EBuiltin Type -> Right (EBuiltin Kind)
    EBuiltin Kind -> Left KindHasNoType
    EVar var ->
      case lookupVarBinding ctx var of
        Just binding -> Right (bindingType binding)
        Nothing -> Left (UnboundVariable var)
    EAnnot body tipe -> do
      ensureTypeExpression ctx tipe
      checkTypeWithContext ctx body tipe
      Right tipe
    ELam name inputType body -> do
      ensureTypeExpression ctx inputType
      bodyType <- inferTypeWithContext (Binding name inputType Nothing : ctx) body
      Right (EForall name inputType bodyType)
    EForall name inputType bodyType -> do
      ensureTypeExpression ctx inputType
      ensureTypeExpression (Binding name inputType Nothing : ctx) bodyType
      bodySort <- inferSort (Binding name inputType Nothing : ctx) bodyType
      Right (sortToExpr bodySort)
    ELet name value body -> do
      valueType <- inferTypeWithContext ctx value
      inferTypeWithContext (Binding name valueType (Just value) : ctx) body
    EApp fn arg -> do
      fnType <- inferTypeWithContext ctx fn
      case normalizeLazy (expandTypeAliases ctx fnType) of
        EForall name inputType outputType -> do
          checkTypeWithContext ctx arg inputType
          Right (substituteLazy name 0 arg outputType)
        other -> Left (NotAFunction other)
    EBinOp op lhs rhs -> do
      checkTypeWithContext ctx lhs (EBuiltin Natural)
      checkTypeWithContext ctx rhs (EBuiltin Natural)
      case op of
        Plus -> Right (EBuiltin Natural)
        Times -> Right (EBuiltin Natural)

checkTypeWithContext :: Context -> ExprLazy -> ExprLazy -> Either TypeError ()
checkTypeWithContext ctx expr expected = do
  actual <- inferTypeWithContext ctx expr
  unless (typesEqual ctx actual expected) (Left (TypeMismatch expected actual))

typesEqual :: Context -> ExprLazy -> ExprLazy -> Bool
typesEqual ctx a b =
  normalizeLazy (expandTypeAliases ctx a) == normalizeLazy (expandTypeAliases ctx b)

ensureTypeExpression :: Context -> ExprLazy -> Either TypeError ()
ensureTypeExpression ctx expr =
  case expr of
    EBuiltin Kind -> Right ()
    _ -> do
      sort <- inferSort ctx expr
      case sort of
        SortType -> Right ()
        SortKind -> Right ()

inferSort :: Context -> ExprLazy -> Either TypeError Sort
inferSort _ (EBuiltin Kind) = Right SortKind
inferSort ctx expr = do
  tipe <- inferTypeWithContext ctx expr
  case normalizeLazy (expandTypeAliases ctx tipe) of
    EBuiltin Type -> Right SortType
    EBuiltin Kind -> Right SortKind
    other -> Left (InvalidTypeExpression other)

sortToExpr :: Sort -> ExprLazy
sortToExpr sort =
  case sort of
    SortType -> EBuiltin Type
    SortKind -> EBuiltin Kind

lookupVarBinding :: Context -> Var -> Maybe Binding
lookupVarBinding ctx (Var name idx) = go idx ctx
  where
    go _ [] = Nothing
    go n (binding : rest)
      | bindingName binding == name =
          if n == 0
            then Just binding
            else go (n - 1) rest
      | otherwise = go n rest

expandTypeAliases :: Context -> ExprLazy -> ExprLazy
expandTypeAliases ctx expr =
  case expr of
    ENatural n -> ENatural n
    EBuiltin b -> EBuiltin b
    EVar var ->
      case lookupVarBinding ctx var of
        Just binding ->
          case bindingValue binding of
            Just value -> expandTypeAliases ctx value
            Nothing -> EVar var
        Nothing -> EVar var
    EAnnot body tipe -> EAnnot (expandTypeAliases ctx body) (expandTypeAliases ctx tipe)
    ELam name tipe body -> ELam name (expandTypeAliases ctx tipe) (expandTypeAliases ctx body)
    EForall name tipe body -> EForall name (expandTypeAliases ctx tipe) (expandTypeAliases ctx body)
    ELet name value body -> ELet name (expandTypeAliases ctx value) (expandTypeAliases ctx body)
    EApp fn arg -> EApp (expandTypeAliases ctx fn) (expandTypeAliases ctx arg)
    EBinOp op lhs rhs -> EBinOp op (expandTypeAliases ctx lhs) (expandTypeAliases ctx rhs)

appearsAsBinderName :: ExprLazy -> Text -> Bool
appearsAsBinderName expr target =
  case expr of
    ENatural _ -> False
    EBuiltin _ -> False
    EVar _ -> False
    EAnnot body tipe -> appearsAsBinderName body target || appearsAsBinderName tipe target
    ELam name tipe body -> name == target || appearsAsBinderName tipe target || appearsAsBinderName body target
    EForall name tipe body -> name == target || appearsAsBinderName tipe target || appearsAsBinderName body target
    ELet name value body -> name == target || appearsAsBinderName value target || appearsAsBinderName body target
    EApp fn arg -> appearsAsBinderName fn target || appearsAsBinderName arg target
    EBinOp _ lhs rhs -> appearsAsBinderName lhs target || appearsAsBinderName rhs target
