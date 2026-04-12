{-# LANGUAGE OverloadedStrings #-}

module MuFomega.TypeCheckSpec
  ( spec
  ) where

import qualified Data.Text as Text
import MuFomega.Parser.Megaparsec (parseExpr)
import MuFomega.Syntax.Common (Builtin (Kind, Natural, Type), Var (Var))
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EApp
      , EBuiltin
      , EForall
      , ELet
      , ENatural
      , EVar
      )
  )
import MuFomega.Syntax.Strict
  ( ExprStrict
      ( SEApp
      , SEBuiltin
      , SEForall
      , SELam
      , SELet
      , SENatural
      , SEVar
      )
  )
import MuFomega.TypeCheck
  ( TypeError
      ( KindHasNoType
      , NotAFunction
      , TopLevelFreeVariable
      , TypeMismatch
      , UnboundVariable
      )
  , checkTypeLazy
  , checkTypeStrict
  , inferTypeLazy
  , inferTypeStrict
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "type checker positive cases" $ do
    it "infers id and type_of_id declarations from README" $ do
      inferFromLet "id" readmeProgram
        `shouldBe` Right (EForall "a" (EBuiltin Type) (EForall "x" (EVar (Var "a" 0)) (EVar (Var "a" 0))))
      inferFromLet "type_of_id" readmeProgram
        `shouldBe` Right (EBuiltin Type)

    it "accepts Type : Kind" $
      inferTypeLazy (EBuiltin Type) `shouldBe` Right (EBuiltin Kind)

    it "type-checks the full README sample" $
      inferTypeLazyParsed readmeProgram `shouldBe` Right (EBuiltin Natural)

  describe "type checker negative cases" $ do
    it "rejects non-function application" $
      inferTypeLazy (EApp (ENatural 1) (ENatural 2))
        `shouldBe` Left (NotAFunction (EBuiltin Natural))

    it "rejects wrong operand types for arithmetic" $
      inferTypeLazy (EApp (EApp (EBuiltin Natural) (ENatural 1)) (ENatural 2))
        `shouldBe` Left (NotAFunction (EBuiltin Type))

    it "rejects invalid annotation" $
      checkTypeLazy (ENatural 1) (EBuiltin Type)
        `shouldBe` Left (TypeMismatch (EBuiltin Type) (EBuiltin Natural))

    it "rejects bare top-level free variables" $
      inferTypeLazy (EVar (Var "x" 0)) `shouldBe` Left (TopLevelFreeVariable (Var "x" 0))

    it "rejects invalid index scope" $
      inferTypeLazy (EForall "x" (EBuiltin Type) (EVar (Var "x" 1)))
        `shouldBe` Left (UnboundVariable (Var "x" 1))

    it "rejects Kind as a term type" $
      inferTypeLazy (EBuiltin Kind) `shouldBe` Left KindHasNoType

    it "rejects free variables in top-level accepted programs" $
      inferTypeLazy (EForall "x" (EBuiltin Type) (EVar (Var "y" 0)))
        `shouldBe` Left (TopLevelFreeVariable (Var "y" 0))

  describe "lazy/strict parity" $ do
    it "matches inferred type across representations" $ do
      inferTypeStrict strictReadmeExpr
        `shouldBe` Right (SEBuiltin Natural)

    it "matches check behavior across representations" $ do
      checkTypeStrict strictIdExpr strictIdType `shouldBe` Right ()

inferTypeLazyParsed :: Text.Text -> Either TypeError ExprLazy
inferTypeLazyParsed source = do
  expr <- case parseExpr source of
    Left bundle -> Left (errorFromParse bundle)
    Right parsed -> Right parsed
  inferTypeLazy expr

inferFromLet :: Text.Text -> Text.Text -> Either TypeError ExprLazy
inferFromLet name source = do
  expr <- case parseExpr source of
    Left bundle -> Left (errorFromParse bundle)
    Right parsed -> Right parsed
  inferFromLets name expr

inferFromLets :: Text.Text -> ExprLazy -> Either TypeError ExprLazy
inferFromLets target expr =
  case expr of
    ELet name value body
      | name == target -> inferTypeLazy value
      | otherwise -> inferFromLets target body
    _ -> Left (UnboundVariable (Var target 0))

errorFromParse :: Show e => e -> TypeError
errorFromParse _ = UnboundVariable (Var "<parse-error>" 0)

strictReadmeExpr :: ExprStrict
strictReadmeExpr =
  SELet
    "id"
    strictIdExpr
    (SEApp (SEApp (SEVar (Var "id" 0)) (SEBuiltin Natural)) (SENatural 3))

strictIdExpr :: ExprStrict
strictIdExpr =
  SELam
    "a"
    (SEBuiltin Type)
    (SELam "x" (SEVar (Var "a" 0)) (SEVar (Var "x" 0)))

strictIdType :: ExprStrict
strictIdType = SEForall "a" (SEBuiltin Type) (SEForall "x" (SEVar (Var "a" 0)) (SEVar (Var "a" 0)))

readmeProgram :: Text.Text
readmeProgram =
  Text.unlines
    [ "let f = λ(x : Natural) → λ(x : Natural) → (123 + x) * x@1"
    , "let id = λ(a : Type) → λ(x : a) → x"
    , "let type_of_id = ∀(a : Type) → a → a"
    , "let _ = id : type_of_id"
    , "let _ = type_of_id : Type"
    , "let _ = Type : Kind"
    , "let Void = ∀(r : Type) → r"
    , "let Unit = ∀(r : Type) → r → r"
    , "let Pair = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (a → b → r) → r"
    , "in f (Natural/subtract 2 10) (id Natural 20)"
    ]
