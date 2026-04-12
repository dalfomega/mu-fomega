{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.CommonGrammarSpec (
    GrammarBackend (..),
    ParseFailureCategory (..),
    grammarSpec,
) where

import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
import MuFomega.Syntax.Lazy (
    ExprLazy (
        EAnnot,
        EApp,
        EBinOp,
        EBuiltin,
        EForall,
        ELam,
        ELet,
        ENatural,
        EVar
    ),
 )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

data ParseFailureCategory
    = UnexpectedEndOfInput
    | UnexpectedToken
    deriving (Eq, Show)

data GrammarBackend = GrammarBackend
    { backendName :: String
    , backendParseExpr :: Text -> Either String ExprLazy
    , backendParseExprWithCategory :: Text -> Either ParseFailureCategory ExprLazy
    }

grammarSpec :: GrammarBackend -> Spec
grammarSpec backend =
    describe (backendName backend <> " grammar validation") $ do
        describe "lexer + atoms" $ do
            it "parses natural literals including 0" $ do
                shouldParse backend "0" (ENatural 0)
                shouldParse backend "123" (ENatural 123)

            it "rejects leading-zero natural literal" $
                shouldFail backend "01"

            it "parses variables with implicit and explicit indices" $ do
                shouldParse backend "x" (EVar (Var "x" 0))
                shouldParse backend "x@2" (EVar (Var "x" 2))
                shouldParse backend "x @ 3" (EVar (Var "x" 3))

            it "rejects malformed variable indices" $ do
                shouldFail backend "x@"
                shouldFail backend "x@-1"
                shouldFail backend "x@-2"
                shouldFail backend "x@01"

            it "parses builtins and disambiguates longer labels" $ do
                shouldParse backend "Natural" (EBuiltin Natural)
                shouldParse backend "Natural/fold" (EBuiltin NaturalFold)
                shouldParse backend "Natural/subtract" (EBuiltin NaturalSubtract)
                shouldParse backend "Type" (EBuiltin Type)
                shouldParse backend "Naturalx" (EVar (Var "Naturalx" 0))
                shouldParse backend "forallx" (EVar (Var "forallx" 0))

            it "rejects reserved labels in binders" $ do
                shouldFail backend "\\(Natural : Type) -> Natural"
                shouldFail backend "forall (let : Type) -> let"
                shouldFail backend "forall (in : Type) -> in"

            it "supports spaces tabs and comments" $
                shouldParse
                    backend
                    "-- a comment\n\t  x@1  -- trailing\n"
                    (EVar (Var "x" 1))

            it "handles line comments ending at EOF" $
                backendParseExprWithCategory backend "-- no newline"
                    `shouldBe` Left UnexpectedEndOfInput

        describe "expression grammar" $ do
            it "parses lambda forms with both lambda/arrow spellings" $ do
                shouldParse backend "\\(x : Natural) -> x" (ELam "x" (EBuiltin Natural) (EVar (Var "x" 0)))
                shouldParse backend "λ(x : Natural) → x" (ELam "x" (EBuiltin Natural) (EVar (Var "x" 0)))

            it "parses forall forms with both keyword/symbol spellings" $ do
                shouldParse backend "forall (a : Type) -> a" (EForall "a" (EBuiltin Type) (EVar (Var "a" 0)))
                shouldParse backend "∀(a : Type) → a" (EForall "a" (EBuiltin Type) (EVar (Var "a" 0)))

            it "parses arrow shorthand as anonymous forall" $
                shouldParse
                    backend
                    "Natural -> Type -> Kind"
                    (EForall "_" (EBuiltin Natural) (EForall "_" (EBuiltin Type) (EBuiltin Kind)))

            it "respects precedence: application > * > + > annotation > arrow" $
                shouldParse
                    backend
                    "f x * y + z : Natural -> Type"
                    ( EAnnot
                        (EBinOp Plus (EBinOp Times (EApp (EVar (Var "f" 0)) (EVar (Var "x" 0))) (EVar (Var "y" 0))) (EVar (Var "z" 0)))
                        (EForall "_" (EBuiltin Natural) (EBuiltin Type))
                    )

            it "parses let chains without repeated in" $
                shouldParse
                    backend
                    "let x = 1 let y = x in y"
                    (ELet "x" (ENatural 1) (ELet "y" (EVar (Var "x" 0)) (EVar (Var "y" 0))))

            it "rejects invalid constructs" $ do
                shouldFail backend "()"
                shouldFail backend "λ(x) → x"
                shouldFail backend "let x ="

        describe "README full program" $ do
            it "parses sample program and preserves key AST shape" $ do
                let parsed = backendParseExpr backend readmeProgram
                parsed `shouldSatisfy` isRight
                case parsed of
                    Left _ -> pure ()
                    Right expr -> do
                        topLevelLetBinders expr
                            `shouldBe` ["f", "id", "type_of_id", "_", "_", "_", "Void", "Unit", "Pair"]
                        finalBodyTag expr `shouldBe` "app"

        describe "corpus regression" $ do
            it "accepts all valid corpus samples" $
                map (isRight . backendParseExpr backend) validCorpus `shouldSatisfy` and

            it "rejects all invalid corpus samples" $
                map (isLeft . backendParseExpr backend) invalidCorpus `shouldSatisfy` and

        describe "failure categorization" $ do
            it "classifies unexpected EOF" $
                backendParseExprWithCategory backend "let x = 1 in"
                    `shouldBe` Left UnexpectedEndOfInput

            it "classifies unexpected token" $
                backendParseExprWithCategory backend "x@-2"
                    `shouldBe` Left UnexpectedToken

            it "classifies reserved-binder failure as token" $
                backendParseExprWithCategory backend "\\(Natural : Type) -> Natural"
                    `shouldBe` Left UnexpectedToken

shouldParse :: GrammarBackend -> Text -> ExprLazy -> IO ()
shouldParse backend input expected =
    backendParseExpr backend input `shouldBe` Right expected

shouldFail :: GrammarBackend -> Text -> IO ()
shouldFail backend input =
    backendParseExpr backend input `shouldSatisfy` isLeft

isRight :: Either a b -> Bool
isRight value =
    case value of
        Right _ -> True
        Left _ -> False

topLevelLetBinders :: ExprLazy -> [Text]
topLevelLetBinders expr =
    case expr of
        ELet name _ body -> name : topLevelLetBinders body
        _ -> []

finalBodyTag :: ExprLazy -> Text
finalBodyTag expr =
    case unwindLets expr of
        EApp _ _ -> "app"
        ELam _ _ _ -> "lam"
        EForall _ _ _ -> "forall"
        EAnnot _ _ -> "annot"
        EBinOp _ _ _ -> "binop"
        EVar _ -> "var"
        ENatural _ -> "nat"
        EBuiltin _ -> "builtin"
        ELet{} -> "let"

unwindLets :: ExprLazy -> ExprLazy
unwindLets expr =
    case expr of
        ELet _ _ body -> unwindLets body
        other -> other

validCorpus :: [Text]
validCorpus =
    [ "x"
    , "x@1"
    , "x@2"
    , "x @ 3"
    , "Natural"
    , "Natural/fold"
    , "Natural/subtract 2 10"
    , "f x y"
    , "a * b + c"
    , "f x * y + z : Natural -> Type"
    , "(a + b) * c"
    , "\\(x : Natural) -> x"
    , "λ(x : Natural) → x"
    , "forall (a : Type) -> a"
    , "∀(a : Type) → a"
    , "Natural -> Type -> Type"
    , "let x = 1 let y = x in y"
    , "-- c\n\tlet x = 1 in x"
    , Text.unlines
        [ "let f = λ(x : Natural) → λ(x : Natural) → (123 + x) * x@1"
        , "let id = λ(a : Type) → λ(x : a) → x"
        , "in f 1 (id Natural 2)"
        ]
    ]

invalidCorpus :: [Text]
invalidCorpus =
    [ ""
    , "()"
    , "01"
    , "x@"
    , "x@-1"
    , "x@-2"
    , "x@01"
    , "λ(x) → x"
    , "let x ="
    , "let x = 1 in"
    , "forall (in : Type) -> in"
    , "forall (let : Type) -> let"
    , "forall (Natural : Type) -> Natural"
    ]

readmeProgram :: Text
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
