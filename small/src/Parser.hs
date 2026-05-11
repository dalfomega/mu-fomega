{-# LANGUAGE TypeFamilies #-}

module Parser (
    parseExprFromString,
) where

import Control.Monad (void)
import qualified Control.Monad.State as State
import Data.Bifunctor (bimap, second)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Lexer (AlexPosn (..), SToken (..), scanTokens)
import LexerDefs (BareToken (..), TokenNullary (..))
import Numeric.Natural (Natural)
import qualified Syntax as S
import Text.Megaparsec (ParseErrorBundle, Stream (..), mkPos, satisfy, token)
import qualified Text.Megaparsec as Mega

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

alexPosnToSourcePos :: AlexPosn -> String -> Mega.SourcePos
alexPosnToSourcePos (AlexPn _ line col) fileName = Mega.SourcePos fileName (mkPos line) (mkPos col)

type InternState = (Map.Map String S.InternedName, Int) -- Map of interned names to IDs, and the next available ID.
type SParser = Mega.ParsecT Void TokenStream (State.StateT InternState Identity)

-- type SParser = Mega.Parsec Void TokenStream

internNextName :: String -> SParser S.InternedName
internNextName name = do
    (nameMap, nextId) <- State.get
    case Map.lookup name nameMap of
        Just internedName -> pure internedName
        Nothing -> do
            let internedName = S.InternedName nextId
            State.put (Map.insert name internedName nameMap, nextId + 1)
            pure internedName

-- All nullary tokens: TokLambda, TokArrow and so on.
tok0 :: TokenNullary -> SParser ()
tok0 t = void $ satisfy (\case (SToken _ (TokNullary t')) -> t == t'; _ -> False)

tokBuiltin :: SParser S.Builtins
tokBuiltin = token (\case SToken _ (TokBuiltin b) -> Just b; _ -> Nothing) Set.empty

tokBuiltinNonOperator :: SParser S.Builtins
tokBuiltinNonOperator = token (\case SToken _ (TokBuiltin b) | not (isOperator b) -> Just b; _ -> Nothing) Set.empty
  where
    isOperator (S.BOperator _) = True
    isOperator _ = False

tokOperator :: S.BuiltinOperators -> SParser ()
tokOperator op = do
    b <- Mega.lookAhead tokBuiltin
    case b of
        S.BOperator op' | op == op' -> tokBuiltin >> pure ()
        _ -> fail $ "Expected operator: " ++ show op

tokIdentifier :: SParser String
tokIdentifier = token (\case SToken _ (TokIdentifier s) -> Just s; _ -> Nothing) Set.empty

tokNatLit :: SParser Natural
tokNatLit = token (\case SToken _ (TokNatLit n) -> Just n; _ -> Nothing) Set.empty

tokError :: SParser String
tokError = token (\case SToken _ (TokError s) -> Just s; _ -> Nothing) Set.empty

-- Implement the µFω grammar.

topLevelParser :: SParser S.SynExpr
topLevelParser = parseExpr <* Mega.eof

parseExprFromString :: String -> (Either (ParseErrorBundle TokenStream Void) S.SynExpr, Map.Map String S.InternedName)
parseExprFromString input =
    let tokens = TokenStream $ V.fromList $ scanTokens input
        initialState = (Map.empty, 0)
        parseRun :: State.StateT InternState Identity (Either (ParseErrorBundle TokenStream Void) S.SynExpr)
        parseRun = Mega.runParserT topLevelParser "" tokens
        stateRun :: Identity (Either (ParseErrorBundle TokenStream Void) S.SynExpr, InternState)
        stateRun = State.runStateT parseRun initialState
        (result, (nameMap, _)) = runIdentity stateRun
     in (result, nameMap)

-- parse parseExpr "" $ TokenStream $ V.fromList $ scanTokens input

{-
Top production:

expression = lambda | let | forall | operator-or-annotation
-}

-- Baseline: parser with Mega.choice and no frills.

parseExpr :: SParser S.SynExpr
parseExpr =
    Mega.choice
        [ parseOperatorOrAnnotation
        , parseLambda
        , parseLet
        , parseForall
        ]

-- lambda whsp "(" whsp nonreserved-label whsp ":" whsp1 expression whsp ")" whsp arrow whsp expression
parseLambda :: SParser S.SynExpr
parseLambda = do
    tok0 TokLambda
    tok0 TokLParen
    nameStr <- tokIdentifier
    name <- internNextName nameStr
    tok0 TokColon
    ty <- parseExpr
    tok0 TokRParen
    tok0 TokArrow
    body <- parseExpr
    pure $ S.SLam name ty body

-- 1*let-binding in whsp1 expression
parseLet :: SParser S.SynExpr
parseLet = do
    bindings <- Mega.some parseLetBinding
    tok0 TokIn
    body <- parseExpr
    pure $ foldr (\(name, val) b -> S.SLet name val b) body bindings

-- let-binding = let whsp nonreserved-label whsp "=" whsp1 expression
parseLetBinding :: SParser (S.InternedName, S.SynExpr)
parseLetBinding = do
    tok0 TokLet
    nameStr <- tokIdentifier
    name <- internNextName nameStr
    tok0 TokEquals
    body <- parseExpr
    pure (name, body)

-- forall whsp "(" whsp nonreserved-label whsp ":" whsp1 expression whsp ")" whsp arrow whsp expression
parseForall :: SParser S.SynExpr
parseForall = do
    tok0 TokForall
    tok0 TokLParen
    nameStr <- tokIdentifier
    name <- internNextName nameStr
    tok0 TokColon
    ty <- parseExpr
    tok0 TokRParen
    tok0 TokArrow
    body <- parseExpr
    pure $ S.SForall name ty body

-- operator-expression [ whsp arrow whsp expression | whsp ":" whsp1 expression ]
parseOperatorOrAnnotation :: SParser S.SynExpr
parseOperatorOrAnnotation = do
    expr <- parseLowestPrecedenceOperatorExpression
    Mega.choice
        [ do
            tok0 TokArrow
            body <- parseExpr
            underscore <- internNextName "_"
            pure $ S.SForall underscore expr body
        , do
            tok0 TokColon
            ty <- parseExpr
            pure $ S.STypeAnn expr ty
        , pure expr
        ]

-- operator-expression = plus-expression
parseLowestPrecedenceOperatorExpression :: SParser S.SynExpr
parseLowestPrecedenceOperatorExpression = parsePlusExpression

-- plus-expression = times-expression *(whsp plus whsp times-expression)
parsePlusExpression :: SParser S.SynExpr
parsePlusExpression = do
    expr <- parseTimesExpression
    rhss <- Mega.many $ do
        tokOperator S.BNaturalPlus
        parseTimesExpression
    pure $ foldl (\acc rhs -> S.SApp (S.SApp (S.SBuiltin (S.BOperator S.BNaturalPlus)) acc) rhs) expr rhss

-- times-expression = application-expression *(whsp times whsp application-expression)
parseTimesExpression :: SParser S.SynExpr
parseTimesExpression = do
    expr <- parseApplicationExpression
    rhss <- Mega.many $ do
        tokOperator S.BNaturalTimes
        parseApplicationExpression
    pure $ foldl (\acc rhs -> S.SApp (S.SApp (S.SBuiltin (S.BOperator S.BNaturalTimes)) acc) rhs) expr rhss

-- application-expression = primitive-expression *(whsp1 primitive-expression)
parseApplicationExpression :: SParser S.SynExpr
parseApplicationExpression = do
    expr <- parsePrimitiveExpression
    rhss <- Mega.many parsePrimitiveExpression
    pure $ foldl S.SApp expr rhss

-- primitive-expression = natural-literal | builtin | variable | "(" whsp expression whsp ")"
parsePrimitiveExpression :: SParser S.SynExpr
parsePrimitiveExpression =
    Mega.choice
        [ S.SNatLit <$> tokNatLit
        , S.SBuiltin <$> tokBuiltinNonOperator
        , do
            nameStr <- tokIdentifier
            name <- internNextName nameStr
            dbi <- Mega.option (S.DBI 0) (tok0 TokAt *> (S.DBI <$> tokNatLit))
            pure $ S.SVar name dbi
        , do
            tok0 TokLParen
            expr <- parseExpr
            tok0 TokRParen
            pure expr
        ]

-- exprLookup :: Map TokenNullary (SParser S.SynExpr)
-- exprLookup = Map.fromList
--     [ (TokLambda, parseLambda)
--     , (TokLet, parseLet)
--     , (TokForall, parseForall)
--     , (TokNatLit, parseNatLit)
--     , (TokLParen, parse)
--     ]

-- -- Dispatch on the next lookahead token.
-- parseExpr :: SParser S.SynExpr
-- parseExpr = do
--     nextToken <- lookAhead anySingle
--     case nextToken of
--         SToken _ (TokNullary t) -> exprLookup Map.! t
--         _ -> parseOperatorOrAnnotation
