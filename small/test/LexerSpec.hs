module LexerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad (forM_)
import qualified Syntax as S
import Lexer

tok :: Int -> Int -> Int -> BareToken -> SToken
tok a b c t = SToken (AlexPn a b c) t

-- | List of test fixtures: (description, input, expected output)
fixtures :: [(String, String, [SToken])]
fixtures =
  [ ("Keywords: lexes 'let'", "let", [tok 0 1 1 TokLet])
  , ("Keywords: lexes 'in'", "in", [tok 0 1 1 TokIn])
  , ("Keywords: lexes 'forall'", "forall", [tok 0 1 1 TokForall])
  , ("Keywords: lexes Unicode ∀", "∀", [tok 0 1 1 TokForall])
  , ("Keywords: lexes '\\'", "\\", [tok 0 1 1 TokLambda])
  , ("Keywords: lexes Unicode λ", "λ", [tok 0 1 1 TokLambda])
  , ("Operators and Punctuation: lexes '->'", "->", [tok 0 1 1 TokArrow])
  , ("Operators and Punctuation: lexes Unicode →", "→", [tok 0 1 1 TokArrow])
  , ("Operators and Punctuation: lexes '+'", "+", [tok 0 1 1 (TokBuiltin (S.BOperator S.BNaturalPlus))])
  , ("Operators and Punctuation: lexes '*'", "*", [tok 0 1 1 (TokBuiltin (S.BOperator S.BNaturalTimes))])
  , ("Operators and Punctuation: lexes '('", "(", [tok 0 1 1 TokLParen])
  , ("Operators and Punctuation: lexes ')'", ")", [tok 0 1 1 TokRParen])
  , ("Operators and Punctuation: lexes ':'", ":", [tok 0 1 1 TokColon])
  , ("Operators and Punctuation: lexes '='", "=", [tok 0 1 1 TokEquals])
  , ("Operators and Punctuation: lexes '@'", "@", [tok 0 1 1 TokAt])
  , ("Builtins and Type Literals: lexes 'Natural/subtract'", "Natural/subtract", [tok 0 1 1 (TokBuiltin (S.BFunction S.BNaturalSubtract))])
  , ("Builtins and Type Literals: lexes 'Natural'", "Natural", [tok 0 1 1 (TokBuiltin (S.BTypeLit S.TLNatural))])
  , ("Builtins and Type Literals: lexes 'Type'", "Type", [tok 0 1 1 (TokBuiltin (S.BTypeLit S.TLType))])
  , ("Builtins and Type Literals: lexes 'Kind'", "Kind", [tok 0 1 1 (TokBuiltin (S.BTypeLit S.TLKind))])
  , ("Natural Literals: lexes '0'", "0", [tok 0 1 1 (TokNatLit 0)])
  , ("Natural Literals: lexes '123'", "123", [tok 0 1 1 (TokNatLit 123)])
  , ("Natural Literals: rejects leading zero '0123' as two tokens", "0123", [tok 0 1 1 (TokNatLit 0), tok 1 1 2 (TokNatLit 123)])
  , ("Identifiers: lexes 'x'", "x", [tok 0 1 1 (TokIdentifier "x")])
  , ("Identifiers: lexes 'leta'", "leta", [tok 0 1 1 (TokIdentifier "leta")])
  , ("Identifiers: lexes 'x_y-1'", "x_y-1", [tok 0 1 1 (TokIdentifier "x_y-1")])
  , ("Whitespace and Comments: gives TokWS tokens", "  \t\n  let  ", [tok 0 1 1 TokWS, tok 6 2 3 TokLet, tok 9 2 6 TokWS])
  , ("Whitespace and Comments: skips line comments", "let -- this is a comment\nin", [tok 0 1 1 TokLet, tok 3 1 4 TokWS, tok 25 2 1 TokIn])
  , ("Error Handling: lexes invalid character as TokError", "#", [tok 0 1 1 (TokError "#")])
  , ("Error Handling: lexes multiple invalid characters", "$$", [tok 0 1 1 (TokError "$"), tok 1 1 2 (TokError "$")])
  , ("Error Handling: continues lexing after error", "#let", [tok 0 1 1 (TokError "#"), tok 1 1 2 TokLet])
  , ("Combined Examples: lexes 'let x = 1 in x'", "let x = 1 in x", [tok 0 1 1 TokLet, tok 3 1 4 TokWS, tok 4 1 5 (TokIdentifier "x"), tok 5 1 6 TokWS, tok 6 1 7 TokEquals, tok 7 1 8 TokWS, tok 8 1 9 (TokNatLit 1), tok 9 1 10 TokWS, tok 10 1 11 TokIn, tok 12 1 13 TokWS, tok 13 1 14 (TokIdentifier "x")])
  , ("Combined Examples: lexes '\\(x : Natural) → x'", "\\(x : Natural) → x", [tok 0 1 1 TokLambda, tok 1 1 2 TokLParen, tok 2 1 3 (TokIdentifier "x"), tok 3 1 4 TokWS, tok 4 1 5 TokColon, tok 5 1 6 TokWS, tok 6 1 7 (TokBuiltin (S.BTypeLit S.TLNatural)), tok 13 1 14 TokRParen, tok 14 1 15 TokWS, tok 15 1 16 TokArrow, tok 16 1 17 TokWS, tok 17 1 18 (TokIdentifier "x")])
  , ("Combined Examples: incorrect  missing whitespace after ':' ", "let x:Natural = 1 in x", [tok 0 1 1 TokLet, tok 3 1 4 TokWS, tok 4 1 5 (TokIdentifier "x"), tok 5 1 6 TokColon, tok 6 1 7 (TokBuiltin (S.BTypeLit S.TLNatural)), tok 13 1 14 TokWS, tok 14 1 15 TokEquals, tok 15 1 16 TokWS, tok 16 1 17 (TokNatLit 1), tok 17 1 18 TokWS, tok 18 1 19 TokIn, tok 20 1 21 TokWS, tok 21 1 22 (TokIdentifier "x")])
  ]

-- | Test suite for the lexer, generated from fixtures.
spec :: Spec
spec = describe "Lexer" $ forM_ fixtures $ \(desc, input, expected) ->
  it desc $ scanTokens input `shouldBe` expected
