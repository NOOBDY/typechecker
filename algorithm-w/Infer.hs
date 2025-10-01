{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Infer where

import Ast (Expr (..), Lit (..), Name, Type (..))
import Errors (InferenceError (..), Result)

import Control.Monad (foldM)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set

data Scheme = Scheme
    { _vars :: [Name]
    , _ty :: Type
    }
    deriving (Show, Eq, Ord)

type TyVar = Name -- type
type TmVar = Name -- term
type Env = Map TmVar Scheme
type Subst = Map TyVar Type

data InferenceTree
    = InferenceTree
    { _rule :: String
    , _input :: String
    , _output :: String
    , _children :: [InferenceTree]
    }
    deriving (Show)

newtype TypeInference = TypeInference
    { counter :: Int
    }
    deriving (Show, Eq, Ord)

freshTyVar :: StateT TypeInference Result TyVar
freshTyVar = do
    s <- get
    let counter' = counter s
    put s{counter = counter' + 1}
    return $ "t" <> BS.pack (show counter')

applySubst :: Subst -> Type -> Type
applySubst subst (TVar name) =
    case Map.lookup name subst of
        Just v -> v
        Nothing -> TVar name
applySubst subst (TArrow t1 t2) =
    TArrow (applySubst subst t1) (applySubst subst t2)
applySubst subst (TTuple types) =
    TTuple (map (applySubst subst) types)
applySubst _ TInt = TInt
applySubst _ TBool = TBool

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
    Map.foldlWithKey
        (\acc k v -> Map.insert k (applySubst s1 v) acc)
        s1
        s2

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme subst (Scheme vars ty) =
    let
        filteredSubst = foldl' (\acc x -> Map.delete x acc) subst vars
     in
        Scheme vars (applySubst filteredSubst ty)

applySubstEnv :: Subst -> Env -> Env
applySubstEnv subst env =
    Map.map (\v -> applySubstScheme subst v) env

unify :: Type -> Type -> Result (Subst, InferenceTree)
unify t1 t2 =
    case (t1, t2) of
        (TInt, TInt) -> unifyBase
        (TBool, TBool) -> unifyBase
        (TVar v, ty) -> unifyVar v ty
        (ty, TVar v) -> unifyVar v ty
        (TArrow a1 a2, TArrow b1 b2) -> unifyArrow (a1, a2) (b1, b2)
        (TTuple ts1, TTuple ts2) -> unifyTuple ts1 ts2
        (t1', t2') -> Left $ UnificationFailure t1' t2'
  where
    input = show t1 <> " ~ " <> show t2

    unifyBase = return (Map.empty, InferenceTree "Unify-Base" input "{}" [])

    --      α ∉ ftv(τ)
    -- --------------------- (U-VarL)
    --  unify(α,τ) = [α↦ τ]
    --
    -- U-VarR is unify(τ,α)
    unifyVar v ty
        | TVar v == ty = return (Map.empty, InferenceTree "Unify-Var-Same" input "{}" [])
        | occursCheck v ty = Left (OccursCheck v ty)
        | otherwise =
            let
                subst = Map.fromList [(v, ty)]
                output = "{" <> show ty <> "/" <> show v <> "}"
             in
                return (subst, InferenceTree "Unify-Var" input output [])

    --  S₁ = unify(τ₁,τ₃)   S₂ = unify(S₁(τ₂),S₁(τ₄))
    -- ----------------------------------------------- (U-Arrow)
    --       unify(τ₁ → τ₂ , τ₃ → τ₄) = S₂ ◯ S₁
    unifyArrow (a1, a2) (b1, b2) = do
        (s1, tree1) <- unify a1 b1
        let a2Subst = applySubst s1 a2
        let b2Subst = applySubst s1 b2
        (s2, tree2) <- unify a2Subst b2Subst
        let finalSubst = s2 `composeSubst` s1
        let output = prettySubst finalSubst
        return (finalSubst, InferenceTree "Unify-Arrow" input output [tree1, tree2])

    --  S₁ = unify(τ₁,τ₃)   S₂ = unify(S₁(τ₂),S₁(τ₄))
    -- ----------------------------------------------- (U-Tuple)
    --       unify((τ₁,τ₂) , (τ₃,τ₄)) = S₂ ◯ S₁
    unifyTuple ts1 ts2
        | length ts1 /= length ts2 = Left $ TupleLengthMismatch (length ts1) (length ts2)
        | otherwise = do
            (subst, trees) <-
                foldM
                    ( \(subst', trees') (t1', t2') -> do
                        let t1Subst = applySubst subst' t1'
                        let t2Subst = applySubst subst' t2'
                        (s, tree) <- unify t1Subst t2Subst
                        return (s `composeSubst` subst', tree : trees')
                    )
                    (Map.empty, [])
                    (zip ts1 ts2)
            let output = prettySubst subst
            return (subst, InferenceTree "Unify-Tuple" input output trees)

infer :: Env -> Expr -> StateT TypeInference Result (Subst, Type, InferenceTree)
infer env expr@(ELit (LInt _)) = inferLitInt env expr
infer env expr@(ELit (LBool _)) = inferLitBool env expr
infer env expr@(EVar name) = inferVar env expr name
infer env expr@(EAbs param body) = inferAbs env expr param body
infer env expr@(EApp func arg) = inferApp env expr func arg
infer env expr@(ELet var value body) = inferLet env expr var value body
infer env expr@(ETuple exprs) = inferTuple env expr exprs

-- variable lookup
--
--  x : σ ∈ Γ       τ = inst(σ)
-- ----------------------------- (T-Var)
--           Γ ⊢ x : τ
inferVar :: Env -> Expr -> Name -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferVar env expr name = do
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"
    case Map.lookup name env of
        Just scheme -> do
            instantiated <- instantiate scheme
            let output = show scheme
            return (Map.empty, instantiated, InferenceTree "T-Var" input output [])
        Nothing -> lift $ Left $ UnboundVariable name

-- lambda abstraction
--
--  Γ,x : α ⊢ e : τ     α fresh
-- ----------------------------- (T-Lam)
--       Γ ⊢ λx. e : α → τ
inferAbs :: Env -> Expr -> Name -> Expr -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferAbs env expr param body = do
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"

    paramType <- TVar <$> freshTyVar
    let paramScheme = Scheme [] paramType
    let newEnv = Map.insert param paramScheme env
    (s1, bodyType, tree1) <- infer newEnv body
    let paramTypeSubst = applySubst s1 paramType
    let resultType = TArrow paramTypeSubst bodyType
    let output = show resultType
    return (s1, resultType, InferenceTree "T-Abs" input output [tree1])

-- function application
--
--  Γ ⊢ e₁ : τ₁     Γ ⊢ e₂ : τ₂     α fresh     S = unify(τ₁,τ₂ → α)
-- ------------------------------------------------------------------ (T-App)
--                         Γ ⊢ e₁ e₂ : S(α)
inferApp :: Env -> Expr -> Expr -> Expr -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferApp env expr func arg = do
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"

    resultType <- TVar <$> freshTyVar

    (s1, funcType, tree1) <- infer env func
    let envSubst = applySubstEnv s1 env
    (s2, argType, tree2) <- infer envSubst arg

    let funcTypeSubst = applySubst s2 funcType
    let expectedFuncType = TArrow argType resultType

    (s3, tree3) <- lift $ unify funcTypeSubst expectedFuncType

    let finalSubst = s3 `composeSubst` s2 `composeSubst` s1
    let finalType = applySubst s3 resultType

    let output = show finalType
    return (finalSubst, finalType, InferenceTree "T-App" input output [tree1, tree2, tree3])

-- let polymorphism
--
--  Γ ⊢ e₁ : τ₁     σ = gen(Γ,τ₁)     Γ,x : σ ⊢ e₂ : τ₂
-- ----------------------------------------------------- (T-Let)
--              Γ ⊢ let x = e₁ in e₂ : τ₂
inferLet :: Env -> Expr -> Name -> Expr -> Expr -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferLet env expr var value body = do
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"

    (s1, valueType, tree1) <- infer env value
    let envSubst = applySubstEnv s1 env
    let generalizedType = generalize envSubst valueType

    let newEnv = Map.insert var generalizedType env

    (s2, bodyType, tree2) <- infer newEnv body

    let finalSubst = s2 `composeSubst` s1
    let output = show bodyType
    return (finalSubst, bodyType, InferenceTree "T-Let" input output [tree1, tree2])

--       Γ ⊢ e₁ : τ₁ ... eₙ : τₙ
-- ----------------------------------- (T-Tuple)
--  Γ ⊢ (e₁, ..., eₙ) : (τ₁, ..., τₙ)
inferTuple :: Env -> Expr -> [Expr] -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferTuple env expr exprs = do
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"

    (subst, types, trees, _) <-
        foldM
            ( \(subst', types', trees', env') expr' -> do
                (s, ty, tree) <- infer env' expr'
                let subst'' = s `composeSubst` subst'
                let env'' = applySubstEnv s env'
                return (subst'', ty : types', tree : trees', env'')
            )
            (Map.empty, [], [], env)
            exprs

    let resultType = TTuple types
    let output = show resultType
    return (subst, resultType, InferenceTree "T-Tuple" input output trees)

inferLitInt :: Env -> Expr -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferLitInt env expr =
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"
     in return (Map.empty, TInt, InferenceTree "T-Int" input "Int" [])

inferLitBool :: Env -> Expr -> StateT TypeInference Result (Subst, Type, InferenceTree)
inferLitBool env expr =
    let input = prettyEnv env <> " ⊢ " <> show expr <> " ⇒"
     in return (Map.empty, TBool, InferenceTree "T-Bool" input "Bool" [])

generalize :: Env -> Type -> Scheme
generalize env ty =
    let
        typeVars = freeTypeVars ty
        envVars = freeTypeVarsEnv env
        freeVars = Set.toAscList $ typeVars \\ envVars
     in
        Scheme freeVars ty

freeTypeVars :: Type -> Set TyVar
freeTypeVars (TVar name) = Set.fromList [name]
freeTypeVars (TArrow t1 t2) = Set.union (freeTypeVars t1) (freeTypeVars t2)
freeTypeVars (TTuple types) = Set.unions $ map freeTypeVars types
freeTypeVars (TInt) = Set.empty
freeTypeVars (TBool) = Set.empty

freeTypeVarsEnv :: Env -> Set TyVar
freeTypeVarsEnv env =
    Set.unions $ map freeTypeVarsScheme (Map.elems env)

freeTypeVarsScheme :: Scheme -> Set TyVar
freeTypeVarsScheme (Scheme vars ty) =
    foldl' (\acc var -> Set.delete var acc) (freeTypeVars ty) vars

instantiate :: Scheme -> StateT TypeInference Result Type
instantiate (Scheme vars ty) = do
    subst <-
        foldM
            ( \acc var -> do
                fresh <- TVar <$> freshTyVar
                return $ Map.insert var fresh acc
            )
            Map.empty
            vars
    return $ applySubst subst ty

occursCheck :: Name -> Type -> Bool
occursCheck v (TVar name) = name == v
occursCheck v (TArrow t1 t2) = occursCheck v t1 || occursCheck v t2
occursCheck v (TTuple types) = any (occursCheck v) types
occursCheck _ TInt = False
occursCheck _ TBool = False

prettyEnv :: Env -> String
prettyEnv subst
    | Map.null subst = "{}"
    | otherwise =
        let entries = map (\(k, v) -> show k <> ": " <> show v) $ Map.toList subst
         in "{" <> intercalate ", " entries <> "}"

prettySubst :: Subst -> String
prettySubst subst
    | Map.null subst = "{}"
    | otherwise =
        let entries = map (\(k, v) -> show v <> "/" <> show k) $ Map.toList subst
         in "{" <> intercalate ", " entries <> "}"

runInference :: Expr -> Result InferenceTree
runInference expr = do
    (_, _, tree) <- evalStateT (infer Map.empty expr) (TypeInference 0)
    return tree

inferTypeOnly :: Expr -> Result Type
inferTypeOnly expr = do
    (_, ty, _) <- evalStateT (infer Map.empty expr) (TypeInference 0)
    return ty
