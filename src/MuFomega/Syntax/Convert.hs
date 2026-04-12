module MuFomega.Syntax.Convert (
    toStrict,
    toLazy,
) where

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

toStrict :: ExprLazy -> ExprStrict
toStrict expr =
    case expr of
        ENatural n -> SENatural n
        EBuiltin b -> SEBuiltin b
        EVar v -> SEVar v
        EAnnot body tipe -> SEAnnot (toStrict body) (toStrict tipe)
        ELam name tipe body -> SELam name (toStrict tipe) (toStrict body)
        EForall name tipe body -> SEForall name (toStrict tipe) (toStrict body)
        ELet name value body -> SELet name (toStrict value) (toStrict body)
        EApp f x -> SEApp (toStrict f) (toStrict x)
        EBinOp op l r -> SEBinOp op (toStrict l) (toStrict r)

toLazy :: ExprStrict -> ExprLazy
toLazy expr =
    case expr of
        SENatural n -> ENatural n
        SEBuiltin b -> EBuiltin b
        SEVar v -> EVar v
        SEAnnot body tipe -> EAnnot (toLazy body) (toLazy tipe)
        SELam name tipe body -> ELam name (toLazy tipe) (toLazy body)
        SEForall name tipe body -> EForall name (toLazy tipe) (toLazy body)
        SELet name value body -> ELet name (toLazy value) (toLazy body)
        SEApp f x -> EApp (toLazy f) (toLazy x)
        SEBinOp op l r -> EBinOp op (toLazy l) (toLazy r)
