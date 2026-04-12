module MuFomega.Eval.NbEParamHOAS
  ( normalizeLazy
  , normalizeStrict
  ) where

import Data.Text (Text)
import MuFomega.Normalize (alphaNormalizeLazy)
import MuFomega.Syntax.Common
  ( BinOp (Plus, Times)
  , Builtin (NaturalFold, NaturalSubtract)
  , Var (Var)
  )
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EAnnot
      , EApp
      , EBinOp
      , EBuiltin
      , EForall
      , ELam
      , ELet
      , ENatural
      , EVar
      )
  )
import MuFomega.Syntax.Strict (ExprStrict)

data ExprPH a
  = PVar !a
  | PFree !Text !Word
  | PNatural !Integer
  | PBuiltin !Builtin
  | PAnnot !(ExprPH a) !(ExprPH a)
  | PLam !Text !(ExprPH a) (a -> ExprPH a)
  | PForall !Text !(ExprPH a) (a -> ExprPH a)
  | PLet !Text !(ExprPH a) (a -> ExprPH a)
  | PApp !(ExprPH a) !(ExprPH a)
  | PBinOp !BinOp !(ExprPH a) !(ExprPH a)

data Neutral
  = NBound !Text !Int
  | NFree !Text !Word

data Value
  = VNatural !Integer
  | VNeutral !Neutral
  | VLambda !Text !Value !(Value -> Value)
  | VForall !Text !Value !(Value -> Value)
  | VBuiltin !Builtin [Value]
  | VApp !Value !Value
  | VBinOp !BinOp !Value !Value

type BoundEnv a = [(Text, [a])]

type QuoteEnv = [(Text, Int)]

normalizeLazy :: ExprLazy -> ExprLazy
normalizeLazy expr =
  let term :: ExprPH Value
      term = fromExpr [] expr
   in alphaNormalizeLazy (quoteValue [] (eval term))

normalizeStrict :: ExprStrict -> ExprStrict
normalizeStrict = toStrict . normalizeLazy . toLazy

fromExpr :: BoundEnv a -> ExprLazy -> ExprPH a
fromExpr env expr =
  case expr of
    ENatural n -> PNatural n
    EBuiltin b -> PBuiltin b
    EVar (Var name idx) ->
      let stack = lookupBound env name
          stackLen = length stack
          index = fromIntegral idx :: Int
       in if index < stackLen
            then PVar (stack !! index)
            else PFree name (idx - fromIntegral stackLen)
    EAnnot body tipe -> PAnnot (fromExpr env body) (fromExpr env tipe)
    ELam name tipe body ->
      PLam name (fromExpr env tipe) (\x -> fromExpr (pushBound env name x) body)
    EForall name tipe body ->
      PForall name (fromExpr env tipe) (\x -> fromExpr (pushBound env name x) body)
    ELet name value body ->
      PLet name (fromExpr env value) (\x -> fromExpr (pushBound env name x) body)
    EApp fn arg -> PApp (fromExpr env fn) (fromExpr env arg)
    EBinOp op lhs rhs -> PBinOp op (fromExpr env lhs) (fromExpr env rhs)

eval :: ExprPH Value -> Value
eval expr =
  case expr of
    PVar value -> value
    PFree name base -> VNeutral (NFree name base)
    PNatural n -> VNatural n
    PBuiltin builtin -> VBuiltin builtin []
    PAnnot body _ -> eval body
    PLam name tipe body ->
      VLambda name (eval tipe) (\arg -> eval (body arg))
    PForall name tipe body ->
      VForall name (eval tipe) (\arg -> eval (body arg))
    PLet _ value body ->
      eval (body (eval value))
    PApp fn arg ->
      applyValue (eval fn) (eval arg)
    PBinOp op lhs rhs ->
      reduceBinOp op (eval lhs) (eval rhs)

applyValue :: Value -> Value -> Value
applyValue fn arg =
  case fn of
    VLambda _ _ closure -> closure arg
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
    VNeutral neutral -> EVar (quoteNeutral env neutral)
    VLambda name tipe closure ->
      let level = depthFor env name
          reflected = VNeutral (NBound name level)
          body = closure reflected
       in ELam name (quoteValue env tipe) (quoteValue (pushDepth env name) body)
    VForall name tipe closure ->
      let level = depthFor env name
          reflected = VNeutral (NBound name level)
          body = closure reflected
       in EForall name (quoteValue env tipe) (quoteValue (pushDepth env name) body)
    VBuiltin builtin args ->
      foldl EApp (EBuiltin builtin) (map (quoteValue env) args)
    VApp fn arg -> EApp (quoteValue env fn) (quoteValue env arg)
    VBinOp op lhs rhs -> EBinOp op (quoteValue env lhs) (quoteValue env rhs)

quoteNeutral :: QuoteEnv -> Neutral -> Var
quoteNeutral env neutral =
  case neutral of
    NBound name level ->
      let depth = depthFor env name
          idx
            | depth <= level = 0
            | otherwise = fromIntegral (depth - level - 1)
       in Var name idx
    NFree name base ->
      let depth = fromIntegral (depthFor env name)
       in Var name (base + depth)

lookupBound :: BoundEnv a -> Text -> [a]
lookupBound env name =
  case env of
    [] -> []
    (currentName, values) : rest
      | currentName == name -> values
      | otherwise -> lookupBound rest name

pushBound :: BoundEnv a -> Text -> a -> BoundEnv a
pushBound env name value =
  case env of
    [] -> [(name, [value])]
    (currentName, values) : rest
      | currentName == name -> (currentName, value : values) : rest
      | otherwise -> (currentName, values) : pushBound rest name value

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
