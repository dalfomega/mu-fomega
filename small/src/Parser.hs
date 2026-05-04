{-# LANGUAGE TypeFamilies #-}

module Parser where

import Data.Bifunctor (bimap, second)
import Data.Proxy (Proxy (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lexer (AlexPosn (..), BareToken (..), SToken (..), scanTokens)

-- import Text.Megaparsec.Stream (Stream)
import Text.Megaparsec (ParseErrorBundle, Parsec, Stream (..), parse)

newtype TokenStream = TokenStream {tokens :: Vector SToken}
    deriving stock (Eq, Show, Ord)

instance Stream TokenStream where
    type Token TokenStream = SToken
    type Tokens TokenStream = TokenStream

    take1_ s = second TokenStream <$> V.uncons s.tokens
    takeN_ n s = if n == 0 then Nothing else Just $ bimap TokenStream TokenStream $ V.splitAt n s.tokens
    takeWhile_ p s = bimap TokenStream TokenStream $ V.span p s.tokens

    tokensToChunk Proxy = TokenStream . V.fromList
    chunkToTokens Proxy = V.toList . (.tokens)
    chunkLength Proxy = V.length . (.tokens)
    chunkEmpty Proxy = V.null . (.tokens)
