module ParserSpec (spec) where

import Control.Monad (forM_)
import Parser
import Syntax
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- Positive test fixtures: (description, input, expected AST)
positiveFixtures :: [(String, String, SynExpr)]
positiveFixtures =
    [ ("Natural literal", "123", SNatLit 123)
    , ("Natural literal zero", "0", SNatLit 0)
    , ("Builtin Natural", "Natural", builtinNatural)
    , ("Builtin Type", "Type", builtinType)
    , ("Builtin Kind", "Kind", builtinKind)
    , ("Builtin Natural/subtract", "Natural/subtract", builtinSubtract)
    , ("Variable", "x", SVar in0 dbi0)
    , ("Variable with index", "x@1", SVar in0 dbi1)
    , ("Application of two variables", "f x", SApp (SVar in0 dbi0) (SVar in1 dbi0))
    , ("Application of three variables", "f x y", SApp (SApp (SVar in0 dbi0) (SVar in1 dbi0)) (SVar in2 dbi0))
    , ("Lambda", "\\(x : Natural) -> x", SLam in0 builtinNatural (SVar in0 dbi0))
    , ("Lambda with different names", "\\(a : Type) -> \\(b : a) -> b", SLam in0 builtinType (SLam in1 (SVar in0 dbi0) (SVar in1 dbi0)))
    , ("Forall", "forall (x : Natural) -> x", SForall in0 builtinNatural (SVar in0 dbi0))
    , ("Forall with Unicode ∀", "∀(x : Natural) -> x", SForall in0 builtinNatural (SVar in0 dbi0))
    , ("Let single binding", "let x = 123 in x", SLet in0 (SNatLit 123) (SVar in0 dbi0))
    , ("Let multiple bindings", "let x = 1 let y = 2 in x + y", SLet in0 (SNatLit 1) (SLet in1 (SNatLit 2) (SApp (SApp builtinPlus (SVar in0 dbi0)) (SVar in1 dbi0))))
    , ("Plus expression", "1 + 2", SApp (SApp builtinPlus (SNatLit 1)) (SNatLit 2))
    , ("Times expression", "3 * 4", SApp (SApp builtinTimes (SNatLit 3)) (SNatLit 4))
    , ("Operator precedence: times over plus", "1 + 2 * 3", SApp (SApp builtinPlus (SNatLit 1)) (SApp (SApp builtinTimes (SNatLit 2)) (SNatLit 3)))
    , ("Operator associativity: left for plus", "1 + 2 + 3", SApp (SApp builtinPlus (SApp (SApp builtinPlus (SNatLit 1)) (SNatLit 2))) (SNatLit 3))
    , ("Type annotation", "x : Natural", STypeAnn (SVar in0 dbi0) builtinNatural)
    , ("Parenthesized expression", "(123)", SNatLit 123)
    , ("Parenthesized application", "(f x)", SApp (SVar in0 dbi0) (SVar in1 dbi0))
    , ("Complex expression: lambda with application", "\\(f : Natural -> Natural) -> f 42", SLam in0 (SForall in1 builtinNatural builtinNatural) (SApp (SVar in0 dbi0) (SNatLit 42)))
    , ("Arrow type shorthand", "Natural -> Type", SForall in0 builtinNatural builtinType)
    , ("Arrow in annotation", "id : forall (a : Type) -> a -> a", STypeAnn (SVar in0 dbi0) (SForall in1 builtinType (SForall in2 (SVar in1 dbi0) (SVar in1 dbi0))))
    , ("Expression with line comment", "x -- this is a comment\n + y", SApp (SApp builtinPlus (SVar in0 dbi0)) (SVar in1 dbi0))
    , ("Identifier with --", "x--", SVar in0 dbi0)
    , ("Let with identifier containing --", "let x-- = 123 in x--", SLet in0 (SNatLit 123) (SVar in0 dbi0))
    , ("Forall with identifier containing --", "forall (let-- : Natural) -> let--", SForall in0 builtinNatural (SVar in0 dbi0))
    ]
  where
    in0 = InternedName 0
    in1 = InternedName 1
    in2 = InternedName 2
    dbi0 = DBI 0
    dbi1 = DBI 1
    funSubtract = BFunction BNaturalSubtract
    builtinNatural = SBuiltin $ BTypeLit TLNatural
    builtinType = SBuiltin $ BTypeLit TLType
    builtinKind = SBuiltin $ BTypeLit TLKind
    builtinPlus = SBuiltin $ BOperator BNaturalPlus
    builtinTimes = SBuiltin $ BOperator BNaturalTimes
    builtinSubtract = SBuiltin funSubtract

-- Negative test fixtures: (description, input) - these should fail to parse
negativeFixtures :: [(String, String)]
negativeFixtures =
    [ ("Empty input", "")
    , ("Only whitespace", "   \n\t  ")
    , ("Invalid character", ";")
    , ("Multiple invalid characters", "$$")
    , ("Incomplete lambda: missing body", "\\(x : Natural)")
    , ("Incomplete lambda: missing arrow", "\\(x : Natural) x")
    , ("Incomplete lambda: missing type", "\\(x) -> x")
    , ("Incomplete lambda: missing param name", "\\(: Natural) -> x")
    , ("Incomplete lambda: missing colon", "\\(x Natural) -> x")
    , ("Incomplete lambda: missing paren", "\\x : Natural) -> x")
    , ("Incomplete forall: missing body", "forall (x : Natural)")
    , ("Incomplete forall: missing arrow", "forall (x : Natural) x")
    , ("Incomplete forall: missing type", "forall (x) -> x")
    , ("Incomplete forall: missing param name", "forall (: Natural) -> x")
    , ("Incomplete forall: missing colon", "forall (x Natural) -> x")
    , ("Incomplete forall: missing paren", "forall x : Natural) -> x")
    , ("Incomplete let: missing in", "let x = 123")
    , ("Incomplete let: missing equals", "let x 123 in x")
    , ("Incomplete let: missing value", "let x = in x")
    , ("Incomplete let: missing name", "let = 123 in x")
    , ("Incomplete annotation: missing type", "x :")
    , ("Incomplete parentheses: missing closing", "(123")
    , ("Incomplete parentheses: missing opening", "123)")
    , ("Wrong arrow", "λ(x : Natural) ⇒ x")
    , ("Missing whitespace after colon in annotation", "x:Natural")
    , ("Invalid operator", "1 & 2")
    , ("Builtin as variable name", "let Natural = 1 in Natural")
    , ("Reserved identifier in lambda", "\\(Natural : Type) -> Natural")
    , ("De Bruijn index on non-variable", "123@1")
    , ("Negative De Bruijn index", "x@-1")
    , ("Operator without operands", "+")
    , ("Only operator", "* 1")
    , ("Builtin + alone", "+")
    , ("Builtin * alone", "*")
    , ("Nested incomplete", "\\(x : (Natural) -> x")
    ]

spec :: Spec
spec = describe "Parser" $ do
    describe "Positive tests" $
        forM_ positiveFixtures $ \(desc, input, expected) ->
            it desc $ parseExprFromString input `shouldBe` Right expected
    describe "Negative tests" $
        forM_ negativeFixtures $ \(desc, input) ->
            it desc $ parseExprFromString input `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
