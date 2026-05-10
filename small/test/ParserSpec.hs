module ParserSpec (spec) where

import Control.Monad (forM_)
import Numeric.Natural (Natural)
import Parser
import qualified Syntax as S
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- Helper functions for concise test fixtures
in0 :: S.InternedName
in0 = S.InternedName 0

in1 :: S.InternedName
in1 = S.InternedName 1

in2 :: S.InternedName
in2 = S.InternedName 2

dbi0 :: S.DBI
dbi0 = S.DBI 0

dbi1 :: S.DBI
dbi1 = S.DBI 1

natLit :: Natural -> S.SynExpr
natLit = S.SNatLit

var :: S.InternedName -> S.DBI -> S.SynExpr
var = S.SVar

builtin :: S.Builtins -> S.SynExpr
builtin = S.SBuiltin

tlNatural :: S.Builtins
tlNatural = S.BTypeLit S.TLNatural

tlType :: S.Builtins
tlType = S.BTypeLit S.TLType

tlKind :: S.Builtins
tlKind = S.BTypeLit S.TLKind

opPlus :: S.Builtins
opPlus = S.BOperator S.BNaturalPlus

opTimes :: S.Builtins
opTimes = S.BOperator S.BNaturalTimes

funSubtract :: S.Builtins
funSubtract = S.BFunction S.BNaturalSubtract

app :: S.SynExpr -> S.SynExpr -> S.SynExpr
app = S.SApp

lam :: S.InternedName -> S.SynExpr -> S.SynExpr -> S.SynExpr
lam = S.SLam

forallExpr :: S.InternedName -> S.SynExpr -> S.SynExpr -> S.SynExpr
forallExpr = S.SForall

letExpr :: S.InternedName -> S.SynExpr -> S.SynExpr -> S.SynExpr
letExpr = S.SLet

ann :: S.SynExpr -> S.SynExpr -> S.SynExpr
ann = S.STypeAnn

-- Positive test fixtures: (description, input, expected AST)
positiveFixtures :: [(String, String, S.SynExpr)]
positiveFixtures =
    [ ("Natural literal", "123", natLit 123)
    , ("Natural literal zero", "0", natLit 0)
    , ("Builtin Natural", "Natural", builtin tlNatural)
    , ("Builtin Type", "Type", builtin tlType)
    , ("Builtin Kind", "Kind", builtin tlKind)
    , ("Builtin Natural/subtract", "Natural/subtract", builtin funSubtract)
    , ("Builtin +", "+", builtin opPlus)
    , ("Builtin *", "*", builtin opTimes)
    , ("Variable", "x", var in0 dbi0)
    , ("Variable with index", "x@1", var in0 dbi1)
    , ("Application of two variables", "f x", app (var in0 dbi0) (var in1 dbi0))
    , ("Application of three variables", "f x y", app (app (var in0 dbi0) (var in1 dbi0)) (var in2 dbi0))
    , ("Lambda", "\\(x : Natural) -> x", lam in0 (builtin tlNatural) (var in0 dbi0))
    , ("Lambda with different names", "\\(a : Type) -> \\(b : a) -> b", lam in0 (builtin tlType) (lam in1 (var in0 dbi0) (var in1 dbi0)))
    , ("Forall", "forall (x : Natural) -> x", forallExpr in0 (builtin tlNatural) (var in0 dbi0))
    , ("Forall with Unicode ∀", "∀(x : Natural) -> x", forallExpr in0 (builtin tlNatural) (var in0 dbi0))
    , ("Let single binding", "let x = 123 in x", letExpr in0 (natLit 123) (var in0 dbi0))
    , ("Let multiple bindings", "let x = 1 let y = 2 in x + y", letExpr in0 (natLit 1) (letExpr in1 (natLit 2) (app (app (builtin opPlus) (var in0 dbi0)) (var in1 dbi0))))
    , ("Plus expression", "1 + 2", app (app (builtin opPlus) (natLit 1)) (natLit 2))
    , ("Times expression", "3 * 4", app (app (builtin opTimes) (natLit 3)) (natLit 4))
    , ("Operator precedence: times over plus", "1 + 2 * 3", app (app (builtin opPlus) (natLit 1)) (app (app (builtin opTimes) (natLit 2)) (natLit 3)))
    , ("Operator associativity: left for plus", "1 + 2 + 3", app (app (builtin opPlus) (app (app (builtin opPlus) (natLit 1)) (natLit 2))) (natLit 3))
    , ("Type annotation", "x : Natural", ann (var in0 dbi0) (builtin tlNatural))
    , ("Parenthesized expression", "(123)", natLit 123)
    , ("Parenthesized application", "(f x)", app (var in0 dbi0) (var in1 dbi0))
    , ("Complex expression: lambda with application", "\\(f : Natural -> Natural) -> f 42", lam in0 (forallExpr in1 (builtin tlNatural) (builtin tlNatural)) (app (var in0 dbi0) (natLit 42)))
    , ("Arrow type shorthand", "Natural -> Type", forallExpr in0 (builtin tlNatural) (builtin tlType))
    , ("Arrow in annotation", "id : forall (a : Type) -> a -> a", ann (var in0 dbi0) (forallExpr in1 (builtin tlType) (forallExpr in2 (var in1 dbi0) (var in1 dbi0))))
    ]

-- Negative test fixtures: (description, input) - these should fail to parse
negativeFixtures :: [(String, String)]
negativeFixtures =
    [ ("Empty input", "")
    , ("Only whitespace", "   \n\t  ")
    , ("Invalid character", "#")
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
    , ("Incomplete annotation: missing colon", "x Natural")
    , ("Incomplete parentheses: missing closing", "(123")
    , ("Incomplete parentheses: missing opening", "123)")
    , ("Wrong keyword for lambda", "lambda (x : Natural) -> x")
    , ("Wrong arrow", "λ(x : Natural) ⇒ x")
    , ("Missing whitespace after colon in annotation", "x:Natural")
    , ("Extra token at end", "123 x")
    , ("Invalid operator", "1 & 2")
    , ("Builtin as variable name", "let Natural = 1 in Natural")
    , ("Reserved identifier in lambda", "\\(Natural : Type) -> Natural")
    , ("De Bruijn index on non-variable", "123@1")
    , ("Negative De Bruijn index", "x@-1")
    , ("Invalid literal", "123abc")
    , ("Application without space", "fx")  -- depending on lexer, might be identifier
    , ("Operator without operands", "+")
    , ("Only operator", "* 1")
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