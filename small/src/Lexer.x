{
module Lexer (
  SToken(..)
  , BareToken (..)
  , AlexPosn(..)
  , scanTokens
  ) where
import qualified Numeric.Natural as Nat
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
  "let"                         { wrapNullary TokLet }
  "in"                          { wrapNullary TokIn }
  "forall"                      { wrapNullary TokForall }
  "∀"                           { wrapNullary TokForall }
  "\\"                          { wrapNullary TokLambda }
  
  -- Special punctuation.
  "λ"                           { wrapNullary TokLambda }
  "->"                          { wrapNullary TokArrow }
  "→"                           { wrapNullary TokArrow }
  "("                           { wrapNullary TokLParen }
  ")"                           { wrapNullary TokRParen }
  ":"                           { wrapNullary TokColon }
  "="                           { wrapNullary TokEquals }
  "@"                           { wrapNullary TokAt }

  -- Infix operators.
  "+"                           { wrapNullary $ TokBuiltin (S.BOperator S.BNaturalPlus) }
  "*"                           { wrapNullary $ TokBuiltin (S.BOperator S.BNaturalTimes) }
  
  -- Builtins / type literals
  -- Order matters: longer/more specific before shorter, when one is a prefix of the other.
  "Natural/subtract"            {wrapNullary $ TokBuiltin (S.BFunction S.BNaturalSubtract) }
  "Natural"                     {wrapNullary $ TokBuiltin (S.BTypeLit S.TLNatural) }
  "Type"                        {wrapNullary $ TokBuiltin (S.BTypeLit S.TLType) }
  "Kind"                        {wrapNullary $ TokBuiltin (S.BTypeLit S.TLKind) }

  -- Numeric literal
  @natural_literal              { wrapUnary (\s -> TokNatLit (read s)) }

  -- Identifier / label: this rule must be last, or else it will match keywords and builtins.
  @ident                        { wrapUnary TokIdentifier }

{

data BareToken
  = TokLet          
  | TokIn           
  | TokForall       
  | TokLambda       
  | TokArrow        
  | TokBuiltin S.Builtins
  | TokLParen       
  | TokRParen       
  | TokColon        
  | TokEquals       
  | TokAt           
  | TokNatLit Nat.Natural
  | TokIdentifier String
  deriving (Eq, Show)

data SToken = SToken { posn :: AlexPosn, token :: BareToken }

wrapNullary :: BareToken -> AlexPosn -> String -> SToken
wrapNullary t p _ = SToken p t

wrapUnary :: (String -> BareToken) -> AlexPosn -> String -> SToken
wrapUnary f p s = SToken p (f s)

-- | Entry point used by the parser.
scanTokens :: String -> [SToken]
scanTokens = alexScanTokens

}
