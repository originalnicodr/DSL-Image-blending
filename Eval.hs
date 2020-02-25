--module Eval1 (eval) where
{-# LANGUAGE FlexibleContexts #-}

import Prelude as P
import Graphics.Image as I
import Common
import Data.List
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad.IO.Class


--Imports del SimplyTyped
{-
import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter-}

-- Para calmar al GHC

import Control.Monad.Trans.Class
data ErrorM a = EM String | JustE a--chequear esto
newtype ErrorMT m a = ErrorMT {runErrorMT :: m (ErrorM a) }

instance Monad ErrorM where
  return a = JustE a
  JustE a >>= f = f a
  EM s >>= f = EM s

instance Functor ErrorM where
    fmap = liftM

instance Applicative ErrorM where
    pure   = return
    (<*>)  = ap

instance Monad m => Monad (ErrorMT m) where --la intencion de esta monada es para poder usarla como ErrorMT (IO (Image a r d))
  return = ErrorMT . return . JustE

  x >>= f = ErrorMT $ do e <- runErrorMT x
                         case e of
                           EM s -> return (EM s)
                           JustE x -> runErrorMT $ f x

instance MonadTrans ErrorMT where
  lift = ErrorMT . (liftM JustE)



instance Monad m => Functor (ErrorMT m) where
    fmap = liftM

instance Monad m => Applicative (ErrorMT m) where
    pure   = return
    (<*>)  = ap



--raise:: String ->ErrorMT m a
raise s= ErrorMT (return (EM s))




fmap2 (PixelRGB r1 g1 b1)  (PixelRGB r2 g2 b2) f = (PixelRGB (f r1 r2) (f g1 g2) (f b1 b2))

--blend::(MArray arr RGB Double, Array arr1 RGB Double, Array arr1 RGB Double) =>Image arr RGB Double -> Image arr RGB Double -> ((Int, Int) -> Pixel RGB Double -> Pixel RGB Double)-> Image arr RGB Double
blend::(MArray arr RGB t,Array arr1 RGB t,Array arr1 RGB e) => Image arr1 RGB t -> Image arr RGB t -> (t -> t -> e) -> Image arr1 RGB e --ATENCION: Estoy usando FlexibleContexts, sin setear eso en ghci se rompe
blend im1 im2 f = I.imap (\(i,j) p1 -> fmap2 p1 (index im2 (i,j)) f) im1


edit im d f = I.map (fmap (f d)) im

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
conversion' b (LUnOp f e d) = UnOp f (conversion' b e) d
conversion' b (LComplement e) = Complement (conversion' b e)




--exatracti (IO (Image a r d)) = (Image a r d)


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
sub i t (UnOp f e d)          = UnOp f (sub i t e) d
sub i t (Complement e)        = Complement (sub i t e)


--IO (Image arr RGB Double) -> IO (ErrorM (Image arr RGB Double))
--liftIO (readimage)
--evalTerm :: (Array arr1 RGB Double) => Term -> ErrorMT IO (Image arr RGB Double)
evalTerm (Bound _)  = raise "variable ligada inesperada en eval" --el return que usa deberia ser del IO
evalTerm (Free n)     = raise "variable libre"--fst $ fromJust $ lookup n e --para mi aca hay que poner un error
evalTerm (Lam t)      = raise "funcion sin termino para reemplazar en la variable"
evalTerm (Lam u :@: v) = evalTerm (sub 0 v u)
evalTerm (IC dim) = ErrorMT (do x <-readImageRGB VU dim--Va a usar un >>= diferente? por que se esta metiendo con la monada IO y la de imagenes creo
                                return (JustE x))
evalTerm (BinOp f e1 e2) = ErrorMT (do e1' <- runErrorMT (evalTerm e1)
                                       e2' <- runErrorMT (evalTerm e2)
                                       if ((dims e1') == (dims e2')) then return (JustE (blend e1' e2' f)) else return(EM "Las imagenes tienen dimensiones diferentes"))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm (UnOp f e1 d) = ErrorMT (do e1' <- runErrorMT (evalTerm e1)
                                     return (JustE (edit e1' d f)))
evalTerm (Complement e)= ErrorMT (do e' <- runErrorMT (evalTerm e)
                                     return (JustE (opposite (runErrorMT e'))))--es aplicar la funcion de doubles opposite a cada pixel de e'






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
