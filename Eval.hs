--module Eval1 (eval) where
{-# LANGUAGE FlexibleContexts #-}

import Prelude as P
import Graphics.Image as I
import Common
import Data.List
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


--Imports del SimplyTyped
{-
import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter-}

-- Para calmar al GHC


data ErrorM a = EM String | JustE a--chequear esto
instance Monad ErrorM where
  return a = JustE a
  JustE a >>= f = f a
  EM s >>= f = EM s

instance Functor ErrorM where
    fmap = liftM

instance Applicative ErrorM where
    pure   = return
    (<*>)  = ap



fmap2 (PixelRGB r1 g1 b1)  (PixelRGB r2 g2 b2) f = (PixelRGB (f r1 r2) (f g1 g2) (f b1 b2))

--blend::(MArray arr RGB Double, Array arr1 RGB Double, Array arr1 RGB Double) =>Image arr RGB Double -> Image arr RGB Double -> ((Int, Int) -> Pixel RGB Double -> Pixel RGB Double)-> Image arr RGB Double
blend::(MArray arr RGB t,Array arr1 RGB t,Array arr1 RGB e) => Image arr1 RGB t -> Image arr RGB t -> (t -> t -> e) -> Image arr1 RGB e --ATENCION: Estoy usando FlexibleContexts, sin setear eso en ghci se rompe
blend im1 im2 f = I.imap (\(i,j) p1 -> fmap2 p1 (index im2 (i,j)) f) im1

edit im d f = I.map (fmap f d) im



--normal::Double -> Double -> Double
normal a b = b

--multiply::Double -> Double -> Double
multiply a b = a*b

--screen::Double -> Double -> Double
screen a b = a + b - (a*b)

--overlay::Double -> Double -> Double
overlay a b = hardlight b a

--darken::Double -> Double -> Double
darken a b = min a b

--lighten::Double -> Double -> Double
ligthen a b = max a b

--colordodge::Double -> Double -> Double
colordodge a b = if b==1 then 1 else min 1 (a/(1-b))

--colorburn::Double -> Double -> Double
colorburn a b = if b==0 then 0 else 1 - min 1 ((1-a)/b)

hardlight::Double -> Double -> Double
hardlight a b = if (b <= 0.5) then multiply a 2*b else screen a 2*b-1

--softlight::Double -> Double -> Double
softlight a b = if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x

--difference::Double -> Double -> Double
difference a b = abs (a-b)

--exclusion::Double -> Double -> Double
exclusion a b = a + b -2*a*b

opposite im = I.map (fmap opp) im
              where opp x= 1-x






--eval :: LamTerm -> ErrorM
eval t = evalTerm (conversion t)


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
conversion' b (LUnOp f e d) = UnOp f (conversion' b e) d
conversion' b (LComplement e) = Complement (conversion' b e)

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
sub i t (UnOp f e d)          = UnOp f (sub i t e) d
sub i t (Complement e)        = Complement (sub i t e)

--evalTerm :: (MonadState m, MonadError m) => Term -> m ()
evalTerm (Bound _)  = EM "variable ligada inesperada en eval"
evalTerm (Free n)     = EM "variable libre"--fst $ fromJust $ lookup n e --para mi aca hay que poner un error
evalTerm (Lam t)      = EM "funcion sin termino para reemplazar en la variable"
evalTerm (Lam u :@: v) = evalTerm (sub 0 v u)
evalTerm (IC dim) = do im <- readImageRGB VU dim--Va a usar un >>= diferente? por que se esta metiendo con la monada IO y la de imagenes creo
                       return im
evalTerm (BinOp f e1 e2) = do e1' <- evalTerm e1
                              e2' <- evalTerm e2
                              if ((dims e1') == (dims e2')) then return (blend e1' e2' f) else EM "Las imagenes tienen dimensiones diferentes"
evalTerm (BoolOp f e1 e2) =undefined
evalTerm (UnOp f e1 d) = do e1' <- evalTerm e1
                            return (edit e1' d f)
evalTerm (Complement e)= do e' <- evalTerm e
                            return (opposite e')--es aplicar la funcion de doubles opposite a cada pixel de e'

--Ver por el tema de las monadas si lo tengo que escribir asi (por el hecho de que arrastra el mensaje de error)
{-evalTerm (BinOp f e1 e2) = do case (evalTerm e1) of--Esto lo voy a cambiar entre los evaluadores disponibles
                              EM s -> EM s
                              JustE x -> case (evalTerm e2) of
                                         EM s -> EM s
                                         JustE y -> if ((dims x) == (dims y)) then return (blend x y f) else EM "Las imagenes tienen dimensiones diferentes"
evalTerm (UnOp f e1 d) = do e1' <- evalTerm e1
                           case e1' of
                           EM s -> EM s
                           JustE x -> return (edit d x f)-}
