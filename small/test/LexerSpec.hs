module LexerSpec (spec) where

import Control.Monad (forM_)
import Lexer
import LexerDefs
import qualified Syntax as S
import Test.Hspec (Spec, describe, it, shouldBe)

-- Helper functions to make test fixtures more readable.
tokN :: Int -> Int -> Int -> TokenNullary -> SToken
tokN a b c t = tok a b c (TokNullary t)

tok :: Int -> Int -> Int -> BareToken -> SToken
tok a b c t = SToken (AlexPn a b c) t

-- | List of test fixtures: (description, input, expected output)
fixtures :: [(String, String, [SToken])]
fixtures =
    [ ("Keywords: lexes 'let' as identifier if no space follows", "let", [tok 0 1 1 (TokIdentifier "let")])
    , ("Keywords: lexes 'let' as keyword if a space follows", "let ", [tokN 0 1 1 TokLet])
    , ("Keywords: lexes 'in' as identifier if no space follows", "in", [tok 0 1 1 (TokIdentifier "in")])
    , ("Keywords: lexes 'in' as keyword if a space follows", "in ", [tokN 0 1 1 TokIn])
    , ("Keywords: lexes 'forall'", "forall", [tokN 0 1 1 TokForall])
    , ("Keywords: lexes Unicode ∀", "∀", [tokN 0 1 1 TokForall])
    , ("Keywords: lexes '\\'", "\\", [tokN 0 1 1 TokLambda])
    , ("Keywords: lexes Unicode λ", "λ", [tokN 0 1 1 TokLambda])
    , ("Operators and Punctuation: lexes '->'", "->", [tokN 0 1 1 TokArrow])
    , ("Operators and Punctuation: lexes Unicode →", "→", [tokN 0 1 1 TokArrow])
    , ("Operators and Punctuation: lexes '+'", "+", [tok 0 1 1 (TokBuiltin (S.BOperator S.BNaturalPlus))])
    , ("Operators and Punctuation: lexes '*'", "*", [tok 0 1 1 (TokBuiltin (S.BOperator S.BNaturalTimes))])
    , ("Operators and Punctuation: lexes '('", "(", [tokN 0 1 1 TokLParen])
    , ("Operators and Punctuation: lexes ')'", ")", [tokN 0 1 1 TokRParen])
    , ("Operators and Punctuation: lexes ':' as error if no space follows", ":", [tok 0 1 1 (TokError ":")])
    , ("Operators and Punctuation: lexes ':'", ":\n", [tokN 0 1 1 TokColon])
    , ("Operators and Punctuation: lexes '='", "=", [tokN 0 1 1 TokEquals])
    , ("Operators and Punctuation: lexes '@'", "@", [tokN 0 1 1 TokAt])
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
    , ("Whitespace and Comments: gives no tokens", "  \t\n  let -- comment \n", [tokN 6 2 3 TokLet])
    , ("Whitespace and Comments: skips line comments but considers them whitespace", "let -- this is a comment\nin --another comment", [tokN 0 1 1 TokLet, tokN 25 2 1 TokIn])
    , ("Error Handling: lexes invalid character as TokError", "#", [tok 0 1 1 (TokError "#")])
    , ("Error Handling: lexes multiple invalid characters", "$$", [tok 0 1 1 (TokError "$"), tok 1 1 2 (TokError "$")])
    , ("Error Handling: continues lexing after error", "#let # x", [tok 0 1 1 (TokError "#"), tokN 1 1 2 TokLet, tok 5 1 6 (TokError "#"), tok 7 1 8 (TokIdentifier "x")])
    , ("Combined Examples: lexes 'let x = 1 in x'", "let x = 1 in x", [tokN 0 1 1 TokLet, tok 4 1 5 (TokIdentifier "x"), tokN 6 1 7 TokEquals, tok 8 1 9 (TokNatLit 1), tokN 10 1 11 TokIn, tok 13 1 14 (TokIdentifier "x")])
    , ("Combined Examples: lexes '\\(x : Natural) → x'", "\\(x : Natural) → x", [tokN 0 1 1 TokLambda, tokN 1 1 2 TokLParen, tok 2 1 3 (TokIdentifier "x"), tokN 4 1 5 TokColon, tok 6 1 7 (TokBuiltin (S.BTypeLit S.TLNatural)), tokN 13 1 14 TokRParen, tokN 15 1 16 TokArrow, tok 17 1 18 (TokIdentifier "x")])
    , ("Combined Examples: incorrect  missing whitespace after ':' ", "let x:Natural = 1 in x", [tokN 0 1 1 TokLet, tok 4 1 5 (TokIdentifier "x"), tok 5 1 6 (TokError ":"), tok 6 1 7 (TokBuiltin (S.BTypeLit S.TLNatural)), tokN 14 1 15 TokEquals, tok 16 1 17 (TokNatLit 1), tokN 18 1 19 TokIn, tok 21 1 22 (TokIdentifier "x")])
    , ("Comment in the middle of text must start by whitespace", "let let--=1in let--", [tokN 0 1 1 TokLet, tok 4 1 5 (TokIdentifier "let--"), tokN 9 1 10 TokEquals, tok 10 1 11 (TokNatLit 1), tokN 11 1 12 TokIn, tok 14 1 15 (TokIdentifier "let--")])
    ]

-- | Test suite for the lexer, generated from fixtures.
spec :: Spec
spec = describe "Lexer" $ forM_ fixtures $ \(desc, input, expected) ->
    it desc $ scanTokens input `shouldBe` expected
