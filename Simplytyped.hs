module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- ///////////////////////////////////////////
-- NO USAR ESTO, USAR LAS MONADAS DEL EVAL.HS
-- //////////////////////////////////////////


-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs t u)  = Lam (conversion' (t:b) u)--hay que sacarle el tipado de aca
conversion' b (LIC s) = IC s
conversion' b (LBinOp f e1 e2) = BinOp f (conversion' b e1) (conversion' b e2)
conversion' b (LBoolOp f e1 e2) = BoolOp f (conversion' b e1) (conversion' b e2)
conversion' b (LUnOp f e d) = UnOp f (conversion' b e1) (conversion' b e2)
conversion' b (LComplement e) = Complement e

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam u)               = Lam (sub (i+1) t u)
sub i t (IC s)                = IC s
sub i t (BinOp f e1 e2)       = BinOp f (sub i t e1) (sub i t e2)
sub i t (BoolOp f e1 e2)      = BoolOp f (sub i t e1) (sub i t e2)
sub i t (UnOP f e d)          = UnOp f (sub i t e) d
sub i t (Complement e)        = Complement (sub i t e)


-- evaluador de términos --voy a tener que reescribir el evaluador con lo que hice en el Eval2
eval :: NameEnv Value Type -> Term -> Value--ver el tema de los tipos
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t)               = VLam t
eval e (Lam u :@: v)         = eval e (sub 0 v u)--voy a reemplazar la variable dentro del termino lambda u por v
eval e (u :@: v)             = error "El primer termino no es una funcion"
eval e (IC s)                = readImageRGB VU s--leer la Imagen (ver el tema de los tipos)
eval e (BinOp f t1 t2)       = blend (eval e t1) (eval e t1) f
eval e (BoolOp f t1 t2)      = blend (eval e t1) (eval e t1) f
eval e (UnOp f t1 d)         = undefined--ver esto
eval e (Complement t)        = undefined --ver esto

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type--modificar esto
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt ->
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1)
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
----------------------------------
