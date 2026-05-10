{
module Lexer (
  SToken(..)
  , BareToken (..)
  , AlexPosn(..)
  , TokenNullary (..)
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
@comment = "--" [^\n\r]* (\r\n|\n|\r)?

@nonempty_skip = @whitespace @comment? | @comment

-- Natural numbers are either 0 or must begin with a nonzero digit. "01" is not a valid natural number.
$digit       = [0-9]
$nonzero     = [1-9]
@natural_literal         = 0 | $nonzero $digit*

-- For now we don't allow any unicode in identifiers.
-- Identifiers must begin with a letter or underscore. Then they may contain letters, digits, underscores, hyphens, or slashes.
$ident_first  = [_a-zA-Z]
$ident_rest   = [a-zA-Z0-9\-\/\_]

@ident = $ident_first $ident_rest*

-- Colon / keywords with separator constraints are expressed with
-- right-context directly in token rules.

------------------------------------------------------------------------
-- Rules for lexing.
------------------------------------------------------------------------


tokens :-

  -- For now we skip all comments.

  @comment ;
 
-- Keywords and symbolic keywords
  "let" / @nonempty_skip        { wrapNullary TokLet }
  "in" / @nonempty_skip         { wrapNullary TokIn }
  "forall"                      { wrapNullary TokForall }
  "∀"                           { wrapNullary TokForall }
  "\"                           { wrapNullary TokLambda }
  
  -- Special punctuation.
  "λ"                           { wrapNullary TokLambda }
  "->"                          { wrapNullary TokArrow }
  "→"                           { wrapNullary TokArrow }
  "("                           { wrapNullary TokLParen }
  ")"                           { wrapNullary TokRParen }
  ":" / @nonempty_skip          { wrapNullary TokColon }
  "="                           { wrapNullary TokEquals }
  "@"                           { wrapNullary TokAt }

  -- Infix operators.
  "+"                           { wrapBuiltin $ S.BOperator S.BNaturalPlus }
  "*"                           { wrapBuiltin $ S.BOperator S.BNaturalTimes }
  
  -- Builtins / type literals
  -- Order matters: longer/more specific before shorter, when one is a prefix of the other.
  "Natural/subtract"            {wrapBuiltin $ S.BFunction S.BNaturalSubtract }
  "Natural"                     {wrapBuiltin $ S.BTypeLit S.TLNatural }
  "Type"                        {wrapBuiltin $ S.BTypeLit S.TLType }
  "Kind"                        {wrapBuiltin $ S.BTypeLit S.TLKind }

  -- Numeric literal
  @natural_literal              { wrapUnary (\s -> TokNatLit (read s)) }

  -- Identifier / label: this rule must be last, or else it will match keywords and builtins.
  @ident                        { wrapUnary TokIdentifier }

  -- Nonempty whitespace is ignored unless it is part of other tokens.
  -- This rule is at the end to avoid interference with rules that require nonempty whitespace.
  @whitespace                   ;

  -- Catch-all rule, emits error token. Must be the last rule.
  .                             { wrapUnary TokError }

{
-- | Entry point used by the parser.
scanTokens :: String -> [SToken]
scanTokens = alexScanTokens

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

-- | Fully formed tokens with position information. These are the input tokens for the parser.
data SToken = SToken { posn :: AlexPosn, token :: BareToken }
  deriving (Eq, Show, Ord)

-- Helper functions to make the lexer rules more readable.

wrapNullary :: TokenNullary -> AlexPosn -> String -> SToken
wrapNullary t p _ = SToken p (TokNullary t)

wrapBuiltin :: S.Builtins -> AlexPosn -> String -> SToken
wrapBuiltin b p _ = SToken p (TokBuiltin b)

wrapUnary :: (String -> BareToken) -> AlexPosn -> String -> SToken
wrapUnary f p s = SToken p (f s)

}
