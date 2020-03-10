{-# LANGUAGE FlexibleContexts #-}
module Eval where
import Graphics.Image as I
import Common
import Data.List
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Parser
-- Para calmar al GHC
import Control.Monad.Trans.Class

-- ///////////////////////////////////////////////
-- // Declaracion de monadas
-- ///////////////////////////////////////////////

data ErrorM a = EM String | JustE a
newtype ErrorMT m a = ErrorMT {runErrorMT :: m (ErrorM a) }--Tipo de la monada que utiliza transformadores de monadas

instance Monad ErrorM where--Monada con mensaje de error
  return a = JustE a
  JustE a >>= f = f a
  EM s >>= f = EM s

instance Functor ErrorM where
    fmap = liftM

instance Applicative ErrorM where
    pure   = return
    (<*>)  = ap

instance Monad m => Monad (ErrorMT m) where --para usarla como ErrorMT (IO (Image a r d))
  return = ErrorMT . return . JustE
  x >>= f = ErrorMT $ do e <- runErrorMT x
                         case e of
                           EM s -> return (EM s)
                           JustE x -> runErrorMT $ f x

instance MonadTrans ErrorMT where
  lift m = ErrorMT (liftM JustE m)



instance Monad m => Functor (ErrorMT m) where
    fmap = liftM

instance Monad m => Applicative (ErrorMT m) where
    pure   = return
    (<*>)  = ap

raise:: Monad m => String ->ErrorMT m a
raise s= ErrorMT (return (EM s))


-- Conversion a términos localmente sin nombres aplicando beta-reducciones cuando sea necesario
conversion :: LamTerm -> Term
conversion x= beta_red (conversion' [] x) 0

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)         = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)        = conversion' b t :@: conversion' b u
conversion' b (Abs t u)        = Lam (conversion' (t:b) u)--hay que sacarle el tipado de aca
conversion' b (LIC s)          = IC s
conversion' b (LBinOp f e1 e2) = BinOp f (conversion' b e1) (conversion' b e2) --(convfb f)
conversion' b (LUnOp f e d)    = UnOp f (conversion' b e) d --(convfu f)
conversion' b (LComplement e)  = Complement (conversion' b e)

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

beta_red :: Term ->Int-> Term
beta_red ((Lam t) :@: t') j = sub j t' t
beta_red (Lam t) j          = beta_red (Lam t) (j+1)
beta_red (u :@: t) j        = let res= beta_red u j
                                in case res of
                                  Lam u' -> beta_red (res :@: (beta_red t j)) j
                                  x -> x :@: (beta_red t j)
beta_red (BinOp f e1 e2) j  = (BinOp f (beta_red e1 j) (beta_red e2 j))
beta_red (UnOp f e d) j     = UnOp f (beta_red e j) d
beta_red (Complement e) j   = Complement (beta_red e j)
beta_red x j                = x


-- Conversion de tipos Op en sus respectivas funciones de imagenes
convfb:: Op -> Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
convfb Normal      = normalop
convfb Add         = addop
convfb Diff        = diffop
convfb Darken      = darkenop
convfb Lighten     = lightenop
convfb Multiply    = multiplyop
convfb Screen      = screenop
convfb Overlay     = overlayop
convfb HardLight   = hardlightop
convfb SoftLight   = softlightop
convfb ColorDodge  = colordodgeop
convfb ColorBurn   = colorburnop
convfb Hue         = hueop
convfb Luminosity  = luminosityop
convfb BlendColor  = blendcolorop
convfb BlendSat    = blendsatop
convfb Exclusion   = exclusionop

-- Conversion de tipos Op en sus respectivas funciones
convfu:: UOp -> Image VU RGBA Double -> Double -> Image VU RGBA Double
convfu Temp       = tempop
convfu Sat        = satop
convfu Exposure   = exposureop
convfu Contrast   = contrastop
convfu Shadows    = shadowsop
convfu Highlights = highlightsop
convfu Opacity    = opacityop

-- ///////////////////////////////////////////////
-- // Evaluadores
-- ///////////////////////////////////////////////
---------------------------------------------------------------------------------
evalTerm1 :: Term -> ErrorMT IO (Image VU RGBA Double)
evalTerm1 (Bound _)       = raise "Variable ligada inesperada en eval"
evalTerm1 (Free n)        = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")
evalTerm1 (Lam t)         = raise "Funcion sin termino para reemplazar en la variable"
evalTerm1 (Lam u :@: v)   = evalTerm1 (sub 0 v u)
evalTerm1 (u :@: v)       = raise "Termino trabado: no se puede realizar la aplicacion"--Las aplicaciones ya se resolvieron en el beta redex, si se encuentra una aplicacion se mandara un mensaje
evalTerm1 (IC dim)        = ErrorMT (do x <- readImageRGBA VU dim
                                        return (JustE x))
evalTerm1 (BinOp f e1 e2) = (evalTerm1 e1) >>= (\x -> (evalTerm1 e2) >>= (\y -> if ((dims x) == (dims y)) then return ((convfb f) x y ) else ErrorMT (return (EM "Las imagenes tienen dimensiones diferentes"))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm1 (UnOp f e1 d)   = (evalTerm1 e1) >>= (\x -> return ((convfu f) x d))
evalTerm1 (Complement e)  = (evalTerm1 e) >>= (\x -> return (oppositeop x))

--Funciones que recortan dos imagenes para que tengan el mismo tamaño
----------------------------------------------------------------------------------
checkx1 (a,b)= let (x1,y1)=dims a
              in let (x2,y2)=dims b
                  in if (x1>x2) then (downsample (\v -> (v<=(div (x1-x2) 2)) || (x2+(div (x1-x2) 2)<v)) (\v -> False) a, b)
                                else if(x1<x2) then (a,downsample (\v -> (v<=(div (x2-x1) 2)) || (x1+(div (x2-x1) 2)<v)) (\v -> False)  b)
                                               else (a,b)
checky1 (a,b)= let (x1,y1)=dims a
              in let (x2,y2)=dims b
                  in if (y1>y2) then (downsample (\v -> False) (\v -> (v<=(div (y1-y2) 2)) || (y2+(div (y1-y2) 2)<v)) a, b)
                                else if(y1<y2) then (a, downsample (\v -> False) (\v -> (v<=(div (y2-y1) 2)) || (y1+(div (y2-y1) 2)<v))  b)
                                               else (a,b)

cut x y = checky1 (checkx1 (x,y))
----------------------------------------------------------------------------------

--Evaluador que recorta la imagen mas grande sobre la mas chica
evalTerm2::Term->ErrorMT IO (Image VU RGBA Double)
evalTerm2 (Bound _)       = raise "Variable ligada inesperada en eval"
evalTerm2 (Free n)        = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")
evalTerm2 (Lam t)         = raise "Funcion sin termino para reemplazar en la variable"
evalTerm2 (Lam u :@: v)   = evalTerm2 (sub 0 v u)
evalTerm2 (u :@: v)       = raise "Termino trabado: no se puede realizar la aplicacion"--Las aplicaciones ya se resolvieron en el beta redex, si se encuentra una aplicacion se mandara un mensaje
evalTerm2 (IC dim)        = ErrorMT (do x <- readImageRGBA VU dim
                                        return (JustE x))
evalTerm2 (BinOp f e1 e2) = (evalTerm2 e1) >>= (\x -> (evalTerm2 e2) >>= (\y -> if ((dims x) == (dims y)) then return ((convfb f) x y) else let (x',y')=cut x y
                                                                                                                                                    in return ((convfb f) x' y')))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm2 (UnOp f e1 d)   = (evalTerm2 e1) >>= (\x -> return ((convfu f) x d))
evalTerm2 (Complement e)  = (evalTerm2 e) >>= (\x -> return (oppositeop x))


--Funciones que agrega pixeles invisibles a dos imagenes para que tengan el mismo tamaño
----------------------------------------------------------------------------------
checkx2 (a,b)= let (x1,y1)=dims a
                in let (x2,y2)=dims b
                    in if(x1<x2) then (I.imap (\(i,j) (PixelRGBA r1 g1 b1 a1) -> if (i<(div (x2-x1) 2)+1) || (i>(div (x2-x1) 2)+x1-1) then (PixelRGBA r1 g1 b1 0) else (PixelRGBA r1 g1 b1 a1)) (upsample (\ k -> if (k == 1) then (div (x2-x1) 2, 0) else if (k==x1-1)then (0,div (x2-x1) 2) else (0, 0)) (const (0, 0)) a), b)
                             else if(x1>x2) then (a, I.imap (\(i,j) (PixelRGBA r1 g1 b1 a1) -> if (i<(div (x1-x2) 2)+1) || (i>(div (x1-x2) 2)+x2-1) then (PixelRGBA r1 g1 b1 0) else (PixelRGBA r1 g1 b1 a1)) (upsample (\ k -> if (k == 1) then (div (x1-x2) 2, 0) else if (k==x2-1)then (0,div (x1-x2) 2) else (0, 0)) (const (0, 0)) b))
                                              else (a,b)

checky2 (a,b)= let (x1,y1)=dims a
                in let (x2,y2)=dims b
                    in if(y1<y2) then (I.imap (\(i,j) (PixelRGBA r1 g1 b1 a1) -> if (j<(div (y2-y1) 2)+1) || (j>(div (y2-y1) 2)+y1-1) then (PixelRGBA r1 g1 b1 0) else (PixelRGBA r1 g1 b1 a1)) (upsample (const (0, 0)) (\ k -> if (k == 1) then (div (y2-y1) 2, 0) else if (k==y1-1)then (0,div (y2-y1) 2) else (0, 0)) a), b)
                            else if(y1>y2) then (a, I.imap (\(i,j) (PixelRGBA r1 g1 b1 a1) -> if (j<(div (y1-y2) 2)+1) || (j>(div (y1-y2) 2)+y2-1) then (PixelRGBA r1 g1 b1 0) else (PixelRGBA r1 g1 b1 a1)) (upsample (const (0, 0)) (\ k -> if (k == 1) then (div (y1-y2) 2, 0) else if (k==y2-1)then (0,div (y1-y2) 2) else (0, 0)) b))
                                           else (a,b)

adjust x y= checkx2 (checky2 (x,y))
----------------------------------------------------------------------------------
--Evaluador que centra la imagen mas chica sobre la mas grande
evalTerm3::Term->ErrorMT IO (Image VU RGBA Double)
evalTerm3 (Bound _)       = raise "Variable ligada inesperada en eval"
evalTerm3 (Free n)        = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")
evalTerm3 (Lam t)         = raise "Funcion sin termino para reemplazar en la variable"
evalTerm3 (Lam u :@: v)   = evalTerm3 (sub 0 v u)
evalTerm3 (u :@: v)       = raise "Termino trabado: no se puede realizar la aplicacion"--Las aplicaciones ya se resolvieron en el beta redex, si se encuentra una aplicacion se mandara un mensaje
evalTerm3 (IC dim)        = ErrorMT (do x <- readImageRGBA VU dim
                                        return (JustE x))
evalTerm3 (BinOp f e1 e2) = (evalTerm3 e1) >>= (\x -> (evalTerm3 e2) >>= (\y -> if ((dims x) == (dims y)) then return ((convfb f) x y) else let (x',y')=adjust x y
                                                                                                                                                    in return ((convfb f) x' y')))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm3 (UnOp f e1 d)   = (evalTerm3 e1) >>= (\x -> return ((convfu f) x d))
evalTerm3 (Complement e)  = (evalTerm3 e) >>= (\x -> return (oppositeop x))
