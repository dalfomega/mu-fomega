module LexerDefs (TokenNullary(..), BareToken(..)) where

import qualified Numeric.Natural as Nat
import qualified Syntax as S
-- | Tokens that carry no extra data.
data TokenNullary
  = TokLet          
  | TokIn           
  | TokForall       
  | TokLambda       
  | TokArrow        
  | TokLParen       
  | TokRParen       
  | TokColon        
  | TokEquals       
  | TokAt
  deriving (Eq, Show, Ord)

-- | Tokens without position information.
data BareToken
  = TokNullary TokenNullary
  | TokBuiltin S.Builtins     
  | TokNatLit Nat.Natural
  | TokIdentifier String
  | TokError String -- Lexer error: usually, invalid character. Presently, also `:` without a following space or comment.
  deriving (Eq, Show, Ord)
