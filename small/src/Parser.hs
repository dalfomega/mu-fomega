{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Monad (void)
import Data.Bifunctor (bimap, second)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Lexer (AlexPosn (..), SToken (..), scanTokens)
import LexerDefs (BareToken (..), TokenNullary (..))
import Numeric.Natural (Natural)
import qualified Syntax as S
import Text.Megaparsec (ParseErrorBundle, Parsec, SourcePos (..), Stream (..), mkPos, parse, satisfy, token)

-- import qualified Text.Megaparsec as MP

newtype TokenStream = TokenStream {tokens :: Vector SToken}
    deriving stock (Eq, Show, Ord)

instance Stream TokenStream where
    type Token TokenStream = SToken
    type Tokens TokenStream = TokenStream

    -- Adapt the type signatures of V.uncons, V.splitAt, etc., to the required type signatures of a Stream instance.
    take1_ s = second TokenStream <$> V.uncons s.tokens
    takeN_ n s =
        if n == 0
            then Nothing
            else Just $ bimap TokenStream TokenStream $ V.splitAt n s.tokens
    takeWhile_ p s = bimap TokenStream TokenStream $ V.span p s.tokens

    tokensToChunk Proxy = TokenStream . V.fromList
    chunkToTokens Proxy = V.toList . (.tokens)
    chunkLength Proxy = V.length . (.tokens)
    chunkEmpty Proxy = V.null . (.tokens)

alexPosnToSourcePos :: AlexPosn -> String -> SourcePos
alexPosnToSourcePos (AlexPn _ line col) fileName = SourcePos fileName (mkPos line) (mkPos col)

type SParser = Parsec Void TokenStream

-- All nullary tokens: TokLambda, TokArrow and so on.
tok0 :: TokenNullary -> SParser ()
tok0 t = void $ satisfy (\case (SToken _ (TokNullary t')) -> t == t'; _ -> False)

tokBuiltin :: SParser S.Builtins
tokBuiltin = token (\case SToken _ (TokBuiltin b) -> Just b; _ -> Nothing) Set.empty

tokIdentifier :: SParser String
tokIdentifier = token (\case SToken _ (TokIdentifier s) -> Just s; _ -> Nothing) Set.empty

tokNatLit :: SParser Natural
tokNatLit = token (\case SToken _ (TokNatLit n) -> Just n; _ -> Nothing) Set.empty

tokError :: SParser String
tokError = token (\case SToken _ (TokError s) -> Just s; _ -> Nothing) Set.empty

-- Implement the µFω grammar.

parseExprFromString :: String -> Either (ParseErrorBundle TokenStream Void) S.SynExpr
parseExprFromString input = parse parseExpr "" $ TokenStream $ V.fromList $ scanTokens input

parseExpr :: SParser S.SynExpr
parseExpr = undefined

{-
Top production:
-}

parseNatLit :: SParser S.SynExpr
parseNatLit = S.SNatLit <$> tokNatLit
    