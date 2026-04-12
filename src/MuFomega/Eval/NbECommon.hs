module MuFomega.Eval.NbECommon (
    normalizeLazy,
    normalizeStrict,
) where

import Data.Text (Text)
import MuFomega.Normalize (alphaNormalizeLazy)
import MuFomega.Syntax.Common (
    BinOp (Plus, Times),
    Builtin (NaturalFold, NaturalSubtract),
    Var (Var),
 )
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy (
    ExprLazy (
        EAnnot,
        EApp,
        EBinOp,
        EBuiltin,
        EForall,
        ELam,
        ELet,
        ENatural,
        EVar
    ),
 )
import MuFomega.Syntax.Strict (ExprStrict)

data VarRef
    = Bound !Text !Int
    | Free !Text !Word

data Value
    = VNatural !Integer
    | VVar !VarRef
    | VLambda !Text !Value !Closure
    | VForall !Text !Value !Closure
    | VBuiltin !Builtin [Value]
    | VApp !Value !Value
    | VBinOp !BinOp !Value !Value

newtype Closure = Closure
    { runClosure :: Value -> Value
    }

type ValueEnv = [(Text, [Value])]

type QuoteEnv = [(Text, Int)]

normalizeLazy :: ExprLazy -> ExprLazy
normalizeLazy expr =
    alphaNormalizeLazy (quoteValue [] (evalExpr [] expr))

normalizeStrict :: ExprStrict -> ExprStrict
normalizeStrict = toStrict . normalizeLazy . toLazy

evalExpr :: ValueEnv -> ExprLazy -> Value
evalExpr env expr =
    case expr of
        ENatural n -> VNatural n
        EBuiltin b -> VBuiltin b []
        EVar var -> evalVar env var
        EAnnot body _ -> evalExpr env body
        ELam name tipe body ->
            VLambda name (evalExpr env tipe) (mkClosure env name body)
        EForall name tipe body ->
            VForall name (evalExpr env tipe) (mkClosure env name body)
        ELet name value body ->
            let value' = evalExpr env value
             in evalExpr (pushValue env name value') body
        EApp fn arg ->
            let fn' = evalExpr env fn
                arg' = evalExpr env arg
             in applyValue fn' arg'
        EBinOp op lhs rhs ->
            let lhs' = evalExpr env lhs
                rhs' = evalExpr env rhs
             in reduceBinOp op lhs' rhs'

evalVar :: ValueEnv -> Var -> Value
evalVar env (Var name idx) =
    let stack = lookupValues env name
        stackLen = length stack
        index = fromIntegral idx :: Int
     in if index < stackLen
            then stack !! index
            else VVar (Free name (idx - fromIntegral stackLen))

mkClosure :: ValueEnv -> Text -> ExprLazy -> Closure
mkClosure env name body =
    Closure (\arg -> evalExpr (pushValue env name arg) body)

applyValue :: Value -> Value -> Value
applyValue fn arg =
    case fn of
        VLambda _ _ closure -> runClosure closure arg
        VBuiltin builtin args -> reduceBuiltin builtin (args ++ [arg])
        _ -> VApp fn arg

reduceBuiltin :: Builtin -> [Value] -> Value
reduceBuiltin builtin args =
    case (builtin, args) of
        (NaturalSubtract, VNatural x : VNatural y : rest) ->
            applyRestValue (VNatural (max 0 (y - x))) rest
        (NaturalFold, VNatural k : _ : succFn : zero : rest)
            | k >= 0 -> applyRestValue (foldNatural k succFn zero) rest
        _ -> VBuiltin builtin args

foldNatural :: Integer -> Value -> Value -> Value
foldNatural n succFn zero =
    go n zero
  where
    go k acc
        | k <= 0 = acc
        | otherwise = go (k - 1) (applyValue succFn acc)

reduceBinOp :: BinOp -> Value -> Value -> Value
reduceBinOp op lhs rhs =
    case (op, lhs, rhs) of
        (Plus, VNatural 0, _) -> rhs
        (Plus, _, VNatural 0) -> lhs
        (Plus, VNatural l, VNatural r) -> VNatural (l + r)
        (Times, VNatural l, VNatural r) -> VNatural (l * r)
        _ -> VBinOp op lhs rhs

applyRestValue :: Value -> [Value] -> Value
applyRestValue = foldl applyValue

quoteValue :: QuoteEnv -> Value -> ExprLazy
quoteValue env value =
    case value of
        VNatural n -> ENatural n
        VVar varRef -> EVar (quoteVar env varRef)
        VLambda name tipe closure ->
            let level = depthFor env name
                reflected = VVar (Bound name level)
                body = runClosure closure reflected
             in ELam name (quoteValue env tipe) (quoteValue (pushDepth env name) body)
        VForall name tipe closure ->
            let level = depthFor env name
                reflected = VVar (Bound name level)
                body = runClosure closure reflected
             in EForall name (quoteValue env tipe) (quoteValue (pushDepth env name) body)
        VBuiltin builtin args ->
            foldl EApp (EBuiltin builtin) (map (quoteValue env) args)
        VApp fn arg -> EApp (quoteValue env fn) (quoteValue env arg)
        VBinOp op lhs rhs -> EBinOp op (quoteValue env lhs) (quoteValue env rhs)

quoteVar :: QuoteEnv -> VarRef -> Var
quoteVar env varRef =
    case varRef of
        Bound name level ->
            let depth = depthFor env name
                idx = fromIntegral (depth - level - 1)
             in Var name idx
        Free name base ->
            let depth = fromIntegral (depthFor env name)
             in Var name (base + depth)

lookupValues :: ValueEnv -> Text -> [Value]
lookupValues env name =
    case env of
        [] -> []
        (currentName, values) : rest
            | currentName == name -> values
            | otherwise -> lookupValues rest name

pushValue :: ValueEnv -> Text -> Value -> ValueEnv
pushValue env name value =
    case env of
        [] -> [(name, [value])]
        (currentName, values) : rest
            | currentName == name -> (currentName, value : values) : rest
            | otherwise -> (currentName, values) : pushValue rest name value

depthFor :: QuoteEnv -> Text -> Int
depthFor env name =
    case env of
        [] -> 0
        (currentName, depth) : rest
            | currentName == name -> depth
            | otherwise -> depthFor rest name

pushDepth :: QuoteEnv -> Text -> QuoteEnv
pushDepth env name =
    case env of
        [] -> [(name, 1)]
        (currentName, depth) : rest
            | currentName == name -> (currentName, depth + 1) : rest
            | otherwise -> (currentName, depth) : pushDepth rest name
