module MuFomega.Substitute (
    substituteLazy,
    substituteStrict,
) where

import Data.Text (Text)
import MuFomega.Shift (shiftLazy, shiftStrict)
import MuFomega.Syntax.Common (Var (Var))
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
import MuFomega.Syntax.Strict (
    ExprStrict (
        SEAnnot,
        SEApp,
        SEBinOp,
        SEBuiltin,
        SEForall,
        SELam,
        SELet,
        SENatural,
        SEVar
    ),
 )

substituteLazy :: Text -> Word -> ExprLazy -> ExprLazy -> ExprLazy
substituteLazy symbol needle replacement = go needle replacement
  where
    go n repl expr =
        n `seq`
            repl `seq`
                case expr of
                    ENatural k -> ENatural k
                    EBuiltin b -> EBuiltin b
                    EVar (Var name idx)
                        | name == symbol && idx == n ->
                            repl
                        | name == symbol && idx > n ->
                            EVar (Var name (idx - 1))
                        | otherwise -> EVar (Var name idx)
                    EAnnot body tipe -> EAnnot (go n repl body) (go n repl tipe)
                    ELam name tipe body ->
                        let repl' = shiftLazy 1 name 0 repl
                         in repl' `seq`
                                ELam
                                    name
                                    (go n repl tipe)
                                    (go (nextNeedle n name) repl' body)
                    EForall name tipe body ->
                        let repl' = shiftLazy 1 name 0 repl
                         in repl' `seq`
                                EForall
                                    name
                                    (go n repl tipe)
                                    (go (nextNeedle n name) repl' body)
                    ELet name value body ->
                        let repl' = shiftLazy 1 name 0 repl
                         in repl' `seq`
                                ELet
                                    name
                                    (go n repl value)
                                    (go (nextNeedle n name) repl' body)
                    EApp fn arg -> EApp (go n repl fn) (go n repl arg)
                    EBinOp op lhs rhs -> EBinOp op (go n repl lhs) (go n repl rhs)

    nextNeedle n binderName
        | binderName == symbol = n + 1
        | otherwise = n

substituteStrict :: Text -> Word -> ExprStrict -> ExprStrict -> ExprStrict
substituteStrict symbol needle replacement = go needle replacement
  where
    go n repl expr =
        n `seq`
            repl `seq`
                case expr of
                    SENatural k -> SENatural k
                    SEBuiltin b -> SEBuiltin b
                    SEVar (Var name idx)
                        | name == symbol && idx == n ->
                            repl
                        | name == symbol && idx > n ->
                            SEVar (Var name (idx - 1))
                        | otherwise -> SEVar (Var name idx)
                    SEAnnot body tipe -> SEAnnot (go n repl body) (go n repl tipe)
                    SELam name tipe body ->
                        let repl' = shiftStrict 1 name 0 repl
                         in repl' `seq`
                                SELam
                                    name
                                    (go n repl tipe)
                                    (go (nextNeedle n name) repl' body)
                    SEForall name tipe body ->
                        let repl' = shiftStrict 1 name 0 repl
                         in repl' `seq`
                                SEForall
                                    name
                                    (go n repl tipe)
                                    (go (nextNeedle n name) repl' body)
                    SELet name value body ->
                        let repl' = shiftStrict 1 name 0 repl
                         in repl' `seq`
                                SELet
                                    name
                                    (go n repl value)
                                    (go (nextNeedle n name) repl' body)
                    SEApp fn arg -> SEApp (go n repl fn) (go n repl arg)
                    SEBinOp op lhs rhs -> SEBinOp op (go n repl lhs) (go n repl rhs)

    nextNeedle n binderName
        | binderName == symbol = n + 1
        | otherwise = n
