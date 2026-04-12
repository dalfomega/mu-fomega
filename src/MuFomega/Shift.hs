module MuFomega.Shift
  ( shiftLazy
  , shiftStrict
  ) where

import Data.Text (Text)
import MuFomega.Syntax.Common (Var (Var))
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
import MuFomega.Syntax.Strict
  ( ExprStrict
      ( SEAnnot
      , SEApp
      , SEBinOp
      , SEBuiltin
      , SEForall
      , SELam
      , SELet
      , SENatural
      , SEVar
      )
  )

shiftLazy :: Integer -> Text -> Word -> ExprLazy -> ExprLazy
shiftLazy d symbol cutoff = go cutoff
  where
    go m expr =
      m `seq` case expr of
        ENatural n -> ENatural n
        EBuiltin b -> EBuiltin b
        EVar (Var name idx)
          | name == symbol && idx >= m -> EVar (Var name (applyDelta d idx))
          | otherwise -> EVar (Var name idx)
        EAnnot body tipe -> EAnnot (go m body) (go m tipe)
        ELam name tipe body ->
          ELam name (go m tipe) (go (nextCutoff m name) body)
        EForall name tipe body ->
          let mType = m
           in EForall name (go mType tipe) (go (nextCutoff m name) body)
        ELet name value body ->
          ELet name (go m value) (go (nextCutoff m name) body)
        EApp fn arg -> EApp (go m fn) (go m arg)
        EBinOp op lhs rhs -> EBinOp op (go m lhs) (go m rhs)

    nextCutoff m binderName
      | binderName == symbol = m + 1
      | otherwise = m

shiftStrict :: Integer -> Text -> Word -> ExprStrict -> ExprStrict
shiftStrict d symbol cutoff = go cutoff
  where
    go m expr =
      m `seq` case expr of
        SENatural n -> SENatural n
        SEBuiltin b -> SEBuiltin b
        SEVar (Var name idx)
          | name == symbol && idx >= m -> SEVar (Var name (applyDelta d idx))
          | otherwise -> SEVar (Var name idx)
        SEAnnot body tipe -> SEAnnot (go m body) (go m tipe)
        SELam name tipe body ->
          SELam name (go m tipe) (go (nextCutoff m name) body)
        SEForall name tipe body ->
          let mType = m
           in SEForall name (go mType tipe) (go (nextCutoff m name) body)
        SELet name value body ->
          SELet name (go m value) (go (nextCutoff m name) body)
        SEApp fn arg -> SEApp (go m fn) (go m arg)
        SEBinOp op lhs rhs -> SEBinOp op (go m lhs) (go m rhs)

    nextCutoff m binderName
      | binderName == symbol = m + 1
      | otherwise = m

applyDelta :: Integer -> Word -> Word
applyDelta d idx
  | d >= 0 = idx + fromInteger d
  | otherwise =
      let k = fromInteger (abs d)
       in if idx < k
            then error "shift would produce a negative de Bruijn index"
            else idx - k
