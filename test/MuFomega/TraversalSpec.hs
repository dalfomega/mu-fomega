{-# LANGUAGE OverloadedStrings #-}

module MuFomega.TraversalSpec (
    spec,
) where

import Control.DeepSeq (force)
import Data.Text (Text)
import MuFomega.Gen (AnyExprLazy (..), AnyExprStrict (..))
import MuFomega.Syntax.Common (BinOp (Plus), Builtin (Natural), Var (Var))
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
import MuFomega.Traversal (
    foldExprLazy,
    foldExprStrict,
    mapExprLazy,
    mapExprStrict,
    mapExprWithBindersLazy,
    mapExprWithBindersStrict,
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "identity traversal" $ do
        it "preserves lazy AST" $ property $ \(AnyExprLazy expr) ->
            mapExprLazy id expr == expr

        it "preserves strict AST" $ property $ \(AnyExprStrict expr) ->
            mapExprStrict id expr == expr

    describe "fold traversal order" $ do
        it "visits lazy nodes pre-order" $ do
            foldExprLazy (\acc e -> acc <> [lazyTag e]) [] sampleLazy
                `shouldBe` [ "let"
                           , "nat"
                           , "lam"
                           , "builtin"
                           , "app"
                           , "forall"
                           , "builtin"
                           , "var"
                           , "binop"
                           , "var"
                           , "annot"
                           , "nat"
                           , "builtin"
                           ]

        it "visits strict nodes pre-order" $ do
            foldExprStrict (\acc e -> acc <> [strictTag e]) [] sampleStrict
                `shouldBe` [ "let"
                           , "nat"
                           , "lam"
                           , "builtin"
                           , "app"
                           , "forall"
                           , "builtin"
                           , "var"
                           , "binop"
                           , "var"
                           , "annot"
                           , "nat"
                           , "builtin"
                           ]

    describe "binder-aware traversal" $ do
        it "tracks depth for lazy lambda/forall/let bodies only" $ do
            collectLazyDepths binderSampleLazy
                `shouldBe` [ ("x", 0)
                           , ("x", 1)
                           , ("x", 2)
                           , ("x", 3)
                           , ("x", 4)
                           ]

        it "tracks depth for strict lambda/forall/let bodies only" $ do
            collectStrictDepths binderSampleStrict
                `shouldBe` [ ("x", 0)
                           , ("x", 1)
                           , ("x", 2)
                           , ("x", 3)
                           , ("x", 4)
                           ]

        it "covers binder-aware lazy traversal across all constructors" $ do
            mapExprWithBindersLazy (\_ e -> e) (\d _ -> d + (1 :: Int)) 0 sampleLazy
                `shouldBe` sampleLazy

        it "covers binder-aware strict traversal across all constructors" $ do
            force (mapExprWithBindersStrict (\_ e -> e) (\d _ -> d + (1 :: Int)) 0 sampleStrict)
                `shouldBe` force sampleStrict

        it "preserves arbitrary lazy AST with binder-aware identity" $ property $ \(AnyExprLazy expr) ->
            force (mapExprWithBindersLazy (\_ e -> e) (\d _ -> d + (1 :: Int)) 0 expr) == force expr

        it "preserves arbitrary strict AST with binder-aware identity" $ property $ \(AnyExprStrict expr) ->
            force (mapExprWithBindersStrict (\_ e -> e) (\d _ -> d + (1 :: Int)) 0 expr) == force expr

        it "forces binder names in lazy enter callback" $ do
            let enter depth name = name `seq` (depth + (1 :: Int))
            force (mapExprWithBindersLazy (\_ e -> e) enter 0 binderSampleLazy)
                `shouldBe` force binderSampleLazy

        it "forces binder names in strict enter callback" $ do
            let enter depth name = name `seq` (depth + (1 :: Int))
            force (mapExprWithBindersStrict (\_ e -> e) enter 0 binderSampleStrict)
                `shouldBe` force binderSampleStrict

collectLazyDepths :: ExprLazy -> [(Text, Word)]
collectLazyDepths expr =
    foldExprLazy
        step
        []
        (mapExprWithBindersLazy tagVar bumpDepth (0 :: Word) expr)
  where
    bumpDepth :: Word -> Text -> Word
    bumpDepth depth _ = depth + 1

    tagVar :: Word -> ExprLazy -> ExprLazy
    tagVar depth node =
        case node of
            EVar (Var name _) -> EVar (Var name (fromIntegral depth))
            _ -> node

    step acc node =
        case node of
            EVar (Var name depth) -> acc <> [(name, depth)]
            _ -> acc

collectStrictDepths :: ExprStrict -> [(Text, Word)]
collectStrictDepths expr =
    foldExprStrict
        step
        []
        (mapExprWithBindersStrict tagVar bumpDepth (0 :: Word) expr)
  where
    bumpDepth :: Word -> Text -> Word
    bumpDepth depth _ = depth + 1

    tagVar :: Word -> ExprStrict -> ExprStrict
    tagVar depth node =
        case node of
            SEVar (Var name _) -> SEVar (Var name (fromIntegral depth))
            _ -> node

    step acc node =
        case node of
            SEVar (Var name depth) -> acc <> [(name, depth)]
            _ -> acc

lazyTag :: ExprLazy -> String
lazyTag expr =
    case expr of
        ENatural _ -> "nat"
        EBuiltin _ -> "builtin"
        EVar _ -> "var"
        EAnnot _ _ -> "annot"
        ELam _ _ _ -> "lam"
        EForall _ _ _ -> "forall"
        ELet _ _ _ -> "let"
        EApp _ _ -> "app"
        EBinOp _ _ _ -> "binop"

strictTag :: ExprStrict -> String
strictTag expr =
    case expr of
        SENatural _ -> "nat"
        SEBuiltin _ -> "builtin"
        SEVar _ -> "var"
        SEAnnot _ _ -> "annot"
        SELam _ _ _ -> "lam"
        SEForall _ _ _ -> "forall"
        SELet _ _ _ -> "let"
        SEApp _ _ -> "app"
        SEBinOp _ _ _ -> "binop"

sampleLazy :: ExprLazy
sampleLazy =
    ELet
        "x"
        (ENatural 1)
        ( ELam
            "x"
            (EBuiltin Natural)
            ( EApp
                ( EForall
                    "f"
                    (EBuiltin Natural)
                    (EVar (Var "x" 0))
                )
                ( EBinOp
                    Plus
                    (EVar (Var "f" 0))
                    (EAnnot (ENatural 2) (EBuiltin Natural))
                )
            )
        )

sampleStrict :: ExprStrict
sampleStrict =
    SELet
        "x"
        (SENatural 1)
        ( SELam
            "x"
            (SEBuiltin Natural)
            ( SEApp
                ( SEForall
                    "f"
                    (SEBuiltin Natural)
                    (SEVar (Var "x" 0))
                )
                ( SEBinOp
                    Plus
                    (SEVar (Var "f" 0))
                    (SEAnnot (SENatural 2) (SEBuiltin Natural))
                )
            )
        )

binderSampleLazy :: ExprLazy
binderSampleLazy =
    ELet
        "x"
        (EVar (Var "x" 0))
        ( ELam
            "x"
            (EVar (Var "x" 0))
            ( EForall
                "x"
                (EVar (Var "x" 0))
                ( ELet
                    "x"
                    (EVar (Var "x" 0))
                    (EVar (Var "x" 0))
                )
            )
        )

binderSampleStrict :: ExprStrict
binderSampleStrict =
    SELet
        "x"
        (SEVar (Var "x" 0))
        ( SELam
            "x"
            (SEVar (Var "x" 0))
            ( SEForall
                "x"
                (SEVar (Var "x" 0))
                ( SELet
                    "x"
                    (SEVar (Var "x" 0))
                    (SEVar (Var "x" 0))
                )
            )
        )
