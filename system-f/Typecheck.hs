{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Typecheck where

import Ast (Expr (..), Name (..), Type (..))
import qualified Context as Ctx
import Control.Monad.RWS (gets, lift, put)
import Errors (TypeError (..))
import InferenceTree (InferenceTree (..))
import Util (RSF)

type TmVar = Name
type TyVar = Name

data Entry
    = -- | variable binding: x: A
      EnVarBnd TmVar Type
    | -- | (universal) type variable binding: α
      EnTVarBnd TyVar
    | -- | existential type variable binding: ^α
      EnETVarBnd TyVar
    | -- | solved existential type variable binding: ^α = τ
      EnSETVarBnd TyVar Type
    | -- | scope marker: $α
      EnMark TyVar
    deriving (Show)

type Context = Ctx.Context Entry

newtype BiDirectional = BiDirectional {_counter :: Int}

freshTyVar :: RSF () BiDirectional TyVar
freshTyVar = do
    var <- gets _counter
    put $ BiDirectional (var + 1)
    return $ Name ("α" <> show var)

substType :: TyVar -> Type -> Type -> Type
substType var replacement ty@(TVar name)
    | name == var = replacement
    | otherwise = ty
substType var replacement ty@(TETVar name)
    | name == var = replacement
    | otherwise = ty
substType _ _ ty@TInt = ty
substType _ _ ty@TBool = ty
substType var replacement (TArrow t1 t2) =
    let
        t1' = substType var replacement t1
        t2' = substType var replacement t2
     in
        TArrow t1' t2'
substType var replacement ty@(TForall boundVar body)
    | boundVar == var = ty -- variable shadowing
    | otherwise = TForall boundVar (substType var replacement body)

applyCtxType :: Context -> Type -> RSF () BiDirectional Type
applyCtxType ctx ty = do
    newType <- applyCtxTypeOnce ctx ty
    if newType /= ty
        then return newType
        else applyCtxType ctx ty

applyCtxTypeOnce :: Context -> Type -> RSF () BiDirectional Type
applyCtxTypeOnce ctx ty@(TETVar a) = do
    case Ctx.find (\case (EnSETVarBnd name _) | name == a -> True; _ -> False) ctx of
        Just (EnSETVarBnd _ replacement) -> applyCtxTypeOnce ctx replacement
        _ -> return ty
applyCtxTypeOnce ctx (TArrow t1 t2) = do
    t1' <- applyCtxTypeOnce ctx t1
    t2' <- applyCtxTypeOnce ctx t2
    return $ TArrow t1' t2'
applyCtxTypeOnce ctx (TForall var body) = do
    body' <- applyCtxTypeOnce ctx body
    return $ TForall var body'
applyCtxTypeOnce _ctx ty = return ty

infer :: Context -> Expr -> RSF () BiDirectional (Type, Context, InferenceTree)
infer ctx expr = do
    let input = show ctx <> " ⊢ " <> show expr
    case expr of
        (EVar x) -> inferVar ctx x input
        (EAnn expr ty) -> _
        (EInt n) -> inferLitInt ctx n input
        (EBool b) -> inferLitBool ctx b input
        (EAbs x paramTy body) -> inferAbs ctx x paramTy body input
        (EApp func arg) -> inferApplication ctx func arg input
        (ETAbs a body) -> _
        (ETApp func tyArg) -> _
        (ELet x e1 e2) -> inferLet ctx x e1 e2 input
        (EIf e1 e2 e3) -> inferIf ctx e1 e2 e3 input
        (EBinOp op e1 e2) -> _

inferVar :: Context -> Name -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferVar ctx x input = do
    case Ctx.find (\case (EnVarBnd name _) | name == x -> True; _ -> False) ctx of
        Just (EnVarBnd _ ty) ->
            let output = input <> " ⇒  " <> show ty <> " ⊣ " <> show ctx
             in return (ty, ctx, InferenceTree "InfVar" input output [])
        _ -> lift $ Left (UnboundVariable x Nothing)

inferLitInt :: Context -> Int -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferLitInt ctx _n input =
    let output = input <> " ⇒  Int ⊣ " <> show ctx
     in return (TInt, ctx, InferenceTree "InfLitInt" input output [])

inferLitBool :: Context -> Bool -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferLitBool ctx _b input =
    let output = input <> " ⇒  Bool ⊣ " <> show ctx
     in return (TBool, ctx, InferenceTree "InfLitBool" input output [])

inferAbs :: Context -> Name -> Type -> Expr -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferAbs ctx x paramTy body input = do
    b <- freshTyVar
    let newCtx = Ctx.pushs [EnVarBnd x paramTy, EnETVarBnd b] ctx

    (ctx1, tree) <- check newCtx body (TETVar b)
    let (left, _, right) = Ctx.break3 (\case (EnVarBnd name _) | name == x -> True; _ -> False) ctx1
    let finalCtxEntries = filter (\case (EnSETVarBnd _ _) -> True; _ -> False) left ++ right
    let finalCtx = Ctx.Context finalCtxEntries
    let resultTy = TArrow paramTy (TETVar b)
    let output = show input <> " ⇒  " <> show resultTy <> " ⊣ " <> show finalCtx
    return (resultTy, finalCtx, InferenceTree "InfLam" input output [tree])

inferApplication :: Context -> Expr -> Expr -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferApplication ctx func arg input = do
    (funcTy, ctx1, tree1) <- infer ctx func
    funcTyApplied <- applyCtxType ctx1 funcTy
    (resultTy, ctx2, tree2) <- inferApp ctx1 funcTyApplied arg
    let output = show input <> " ⇒  " <> show resultTy <> " ⊣ " <> show ctx2
    return (resultTy, ctx2, InferenceTree "InfApp" input output [tree1, tree2])

inferLet :: Context -> Name -> Expr -> Expr -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferLet ctx x e1 e2 input = do
    (ty1, ctx1, tree1) <- infer ctx e1
    let newCtx = Ctx.push (EnVarBnd x ty1) ctx1
    (ty2, ctx2, tree2) <- infer newCtx e2
    let (left, _, right) = Ctx.break3 (\case (EnVarBnd name _) | name == x -> True; _ -> False) ctx2
    let finalCtxEntries = filter (\case (EnSETVarBnd _ _) -> True; _ -> False) left ++ right
    let finalCtx = Ctx.Context finalCtxEntries
    let output = show input <> " ⇒  " <> show ty2 <> " ⊣ " <> show finalCtx
    return (ty2, finalCtx, InferenceTree "InfLet" input output [tree1, tree2])

inferIf :: Context -> Expr -> Expr -> Expr -> String -> RSF () BiDirectional (Type, Context, InferenceTree)
inferIf ctx e1 e2 e3 input = do
    (ctx1, tree1) <- check ctx e1 TBool
    (ty2, ctx2, tree2) <- infer ctx1 e2
    (ty3, ctx3, tree3) <- infer ctx2 e3
    -- both branches need to have the same type
    (unifiedCtx, treeUnify) <- subtype ctx3 ty2 ty3
    let output = show input <> " ⇒  " <> show ty2 <> " ⊣ " <> show unifiedCtx
    return (ty2, unifiedCtx, InferenceTree "InfIf" input output [tree1, tree2, tree3, treeUnify])

inferApp :: Context -> Type -> Expr -> RSF () BiDirectional (Type, Context, InferenceTree)
inferApp = _

check :: Context -> Expr -> Type -> RSF () BiDirectional (Context, InferenceTree)
check ctx expr ty = do
    let input = show ctx <> " ⊢ " <> show expr <> " ⇐  " <> show ty
    case (expr, ty) of
        (EInt _, TInt) -> return (ctx, InferenceTree "CheckLitInt" input (show ctx) [])
        (EBool _, TBool) -> return (ctx, InferenceTree "CheckLitBool" input (show ctx) [])
        (EAbs x _paramTy body, TArrow expectedParam resultTy) -> do
            -- assuming param type matches
            let newCtx = Ctx.push (EnVarBnd x expectedParam) ctx
            (ctx1, tree) <- check newCtx body resultTy
            let (_, _, right) = Ctx.break3 (\case (EnVarBnd name _) | name == x -> True; _ -> False) ctx1
            let finalCtx = Ctx.Context right
            return (finalCtx, InferenceTree "CheckLam" input (show finalCtx) [tree])
        (_, TForall a tyBody) -> do
            let newCtx = Ctx.push (EnTVarBnd a) ctx
            (ctx1, tree) <- check newCtx expr tyBody
            let (_, _, right) = Ctx.break3 (\case (EnTVarBnd name) | name == a -> True; _ -> False) ctx1
            let finalCtx = Ctx.Context right
            return (finalCtx, InferenceTree "CheckAll" input (show finalCtx) [tree])
        _ -> do
            (inferredTy, ctx1, tree1) <- infer ctx expr
            inferredApplied <- applyCtxType ctx1 inferredTy
            tyApplied <- applyCtxType ctx1 ty
            (ctx2, tree2) <- subtype ctx1 inferredApplied tyApplied
            return (ctx2, InferenceTree "CheckSub" input (show ctx2) [tree1, tree2])
