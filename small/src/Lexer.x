{
module Lexer (
  Token(..)
  , AlexPosn(..)
  , scanTokens
  ) where
import Numeric.Natural (Natural)
import qualified Syntax as S
}

%wrapper "posn"
-- later can switch to posn-strict-text or monadUserState-strict-text or -bytestring etc.


-- Whitespace.
$white = [\ \t\n\r\v\f]
@whitespace = $white+

-- Comments.
@comment = "--" [^\n]* \n

-- Natural numbers are either 0 or must begin with a nonzero digit. "01" is not a valid natural number.
$digit       = [0-9]
$nonzero     = [1-9]
@natural_literal         = 0 | $nonzero $digit*

-- For now we don't allow any unicode in identifiers.
-- Identifiers must begin with a letter or underscore. Then they may contain letters, digits, underscores, hyphens, or slashes.
$ident_first  = [_a-zA-Z]
$ident_rest   = [a-zA-Z0-9\-\/\_]

@ident = $ident_first $ident_rest*

------------------------------------------------------------------------
-- Rules for lexing.
------------------------------------------------------------------------


tokens :-

-- For now we skip all whitespace and all comments.

@whitespace ;

@comment ;
 
-- Keywords and symbolic keywords
  "let"                         { \p _ -> TokLet p }
  "in"                          { \p _ -> TokIn p }
  "forall"                      { \p _ -> TokForall p }
  "∀"                           { \p _ -> TokForall p }
  "\\"                          { \p _ -> TokLambda p }
  
  -- Special punctuation.
  "λ"                           { \p _ -> TokLambda p }
  "->"                          { \p _ -> TokArrow p }
  "→"                           { \p _ -> TokArrow p }
  "("                           { \p _ -> TokLParen p }
  ")"                           { \p _ -> TokRParen p }
  ":"                           { \p _ -> TokColon p }
  "="                           { \p _ -> TokEquals p }
  "@"                           { \p _ -> TokAt p }

  -- Infix operators.
  "+"                           { \p _ -> TokBuiltin p (S.BOperator S.BNaturalPlus) }
  "*"                           { \p _ -> TokBuiltin p (S.BOperator S.BNaturalTimes) }
  
  -- Builtins / type literals
  -- Order matters: longer/more specific before shorter, when one is a prefix of the other.
  "Natural/subtract"            { \p _ -> TokBuiltin p (S.BFunction S.BNaturalSubtract) }
  "Natural"                     { \p _ -> TokBuiltin p (S.BTypeLit S.TLNatural) }
  "Type"                        { \p _ -> TokBuiltin p (S.BTypeLit S.TLType) }
  "Kind"                        { \p _ -> TokBuiltin p (S.BTypeLit S.TLKind) }

  -- Numeric literal
  @natural_literal                         { \p s -> TokNatLit p (read s) }

  -- Identifier / label: this rule must be last, or else it will match keywords and builtins.
  @ident                          { \p s -> TokIdentifier p s }

{

data Token
  = TokLet         AlexPosn
  | TokIn          AlexPosn
  | TokForall      AlexPosn
  | TokLambda      AlexPosn
  | TokArrow       AlexPosn
  | TokBuiltin     AlexPosn S.Builtins
  | TokLParen      AlexPosn
  | TokRParen      AlexPosn
  | TokColon       AlexPosn
  | TokEquals      AlexPosn
  | TokAt          AlexPosn
  | TokNatLit      AlexPosn Natural
  | TokIdentifier  AlexPosn String
  deriving (Eq, Show)

-- | Entry point used by the parser.
scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}