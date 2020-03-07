--module Eval1 (eval) where
{-# LANGUAGE FlexibleContexts #-}--ViewPatterns
import Prelude as P
import Graphics.Image as I
import Common
import Data.List
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
--import Control.Monad.IO.Class

import Parser
import Parsing
--Necesario para usar el programa compilado
import Control.Applicative hiding(many)--puede que no haga falta
import Options
import System.Environment (getArgs, getProgName)

-- Para calmar al GHC

import Control.Monad.Trans.Class
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

instance Monad m => Monad (ErrorMT m) where --la intencion de esta monada es para poder usarla como ErrorMT (IO (Image a r d))
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


-- im2: foreground im1:background
fmap2::Pixel RGBA Double -> Pixel RGBA Double  -> (Double->Double->Double)->Pixel RGBA Double
fmap2 (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) f = (PixelRGBA ((f (r1*a1*(1-a2)) (r2*a2))/(a2+a1*(1-a2))) ((f (g1*a1*(1-a2)) (g2*a2))/(a2+a1*(1-a2))) ((f (b1*a1*(1-a2)) (b2*a2))/(a2+a1*(1-a2)))  (a2+a1*(1-a2)))

--blend:: (MArray arr cs1 e1, Array arr1 cs e, Array arr1 cs' e') =>   Image arr1 cs' e'   -> Image arr cs1 e1   -> (Pixel cs' e' -> Pixel cs1 e1 -> Pixel cs e)   -> Image arr1 cs e
blend im1 im2 f = I.imap (\(i,j) p1 -> f p1 (index im2 (i,j))) im1

edit :: (Array arr cs e, Array arr cs' e') =>   (t -> Pixel cs' e' -> Pixel cs e)   -> Image arr cs' e' -> t -> Image arr cs e
edit f im d= (I.map (f d) im)

opposite::Pixel RGBA Double -> Pixel RGBA Double
opposite= (\(PixelRGBA r g b a) -> (PixelRGBA (1-r) (1-g) (1-b) a))
--opposite im = (I.map ((\(PixelRGBA r g b a) -> (PixelRGBA (1-r) (1-g) (1-b) a))) im)
--En estas funciones de blend y edit voy a estar usandolas con return para meterlas en el bind


--Funciones de debugging
--------------------------------------------------------------------------------------------------------
eval1' t= let a= runErrorMT(evalTerm1 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> displayImage x--writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)

eval2' t= let a= runErrorMT(evalTerm2 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)

eval3' t= let a= runErrorMT(evalTerm3 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)

--eval1' "Darken <cluster.jpg> <pizza.png>"
--eval "Highlights <cluster.jpg> 1"
--eval2 "Normal </home/nico/Desktop/F.png> <izza.png>"

evaldev t= let a= runErrorMT(evalTerm1 t)
           in a >>= (\i -> case i of
                            JustE x -> writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)

-- Ejecuta un programa a partir de su archivo fuente
--run :: [Char] -> IO ()
run ifile = let a = readFile ifile
            in a>>= eval1'
--------------------------------------------------------------------------------------------------------
--evaldev (BinOp Darken (IC "cluster.jpg") (IC "cluster.jpg"))
--ErrorMT IO (Image arr RGBA Double)

-- conversion a términos localmente sin nombres aplicando betareducciones cuando sea necesario
conversion :: LamTerm -> Term
conversion x= beta_red (conversion' [] x) 0

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs t u)  = Lam (conversion' (t:b) u)--hay que sacarle el tipado de aca
conversion' b (LIC s) = IC s
conversion' b (LBinOp f e1 e2) = BinOp f (conversion' b e1) (conversion' b e2) --(convfb f)
conversion' b (LUnOp f e d) = UnOp f (conversion' b e) d --(convfu f)
conversion' b (LComplement e) = Complement (conversion' b e)

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
beta_red (Lam t) j = beta_red (Lam t) (j+1)
beta_red (u :@: t) j = let res= beta_red u j
                        in case res of
                          Lam u' -> beta_red (res :@: (beta_red t j)) j
                          x -> x :@: (beta_red t j)
beta_red (BinOp f e1 e2) j = (BinOp f (beta_red e1 j) (beta_red e2 j))
beta_red (UnOp f e d) j = UnOp f (beta_red e j) d
beta_red (Complement e) j = Complement (beta_red e j)
beta_red x j = x

-- Conversion de tipos Op en sus respectivas funciones
convfb :: Op -> (Pixel RGBA Double->Pixel RGBA Double->Pixel RGBA Double)
convfb Normal      = blendpixel normal
convfb Add         = blendpixel add
convfb Diff        = blendpixel difference
convfb Darken      = blendpixel darken
convfb Lighten     = blendpixel lighten
convfb Multiply    = blendpixel multiply
convfb Screen      = (\x y ->opposite(blendpixel multiply (opposite x) (opposite y))) --LComplement (LBinOp Multiply (LComplement x) (LComplement y))))
convfb Overlay     = blendpixel overlay
convfb HardLight   = (\(PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) -> blendpixel overlay (PixelRGBA r2 g2 b2 a1) (PixelRGBA r1 g1 b1 a2))--necesito acceso a los alphas
convfb SoftLight   = blendpixel softlight
convfb ColorDodge  = blendpixel colordodge
convfb ColorBurn   = (\x y ->opposite(blendpixel colordodge (opposite x) (opposite y)))--LComplement (LBinOp ColorDodge (LComplement x) (LComplement y))
convfb Hue         = hue
convfb Luminosity  = luminosity
convfb BlendColor  = blendcolor
convfb BlendSat    = blendsat
convfb Exclusion   = blendpixel exclusion

-- Conversion de tipos Op en sus respectivas funciones
convfu :: UOp -> (Double->Pixel RGBA Double->Pixel RGBA Double)
convfu Temp  = temp
convfu Sat   = saturation
convfu Vib   = vib
convfu Exposure = exposure
convfu Contrast = contrast
convfu Shadows = shadows
convfu Highlights = highlights
convfu Whites = whites
convfu Blacks = blacks
convfu Opacity = opacity

-----------------------
--- eval
-----------------------

evalTerm1 :: Term -> ErrorMT IO (Image VU RGBA Double)
evalTerm1 (Bound _)  = raise "Variable ligada inesperada en eval"
evalTerm1 (Free n)     = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")
evalTerm1 (Lam t)      = raise "Funcion sin termino para reemplazar en la variable"
evalTerm1 (Lam u :@: v) = evalTerm1 (sub 0 v u)
evalTerm1 (u :@: v) = raise "Termino trabado: no se puede realizar la aplicacion"--Las aplicaciones ya se resolvieron en el beta redex, si se encuentra una aplicacion se mandara un mensaje
evalTerm1 (IC dim) = ErrorMT (do x <- readImageRGBA VU dim--podria usar el isValid de la biblioteca de Path para revisar que la direccion sea valida y mandar un error
                                 return (JustE x))
evalTerm1 (BinOp f e1 e2) = (evalTerm1 e1) >>= (\x -> (evalTerm1 e2) >>= (\y -> if ((dims x) == (dims y)) then return (blend x y (convfb f)) else ErrorMT (return (EM "Las imagenes tienen dimensiones diferentes"))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm1 (UnOp f e1 d) = (evalTerm1 e1) >>= (\x -> return (edit (convfu f) x d))
evalTerm1 (Complement e)= (evalTerm1 e) >>= (\x -> return (I.map opposite x))

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

cut x y=checky1 (checkx1 (x,y))
----------------------------------------------------------------------------------

--Evaluador que recorta la imagen mas grande sobre la mas chica
evalTerm2::Term->ErrorMT IO (Image VU RGBA Double)
evalTerm2 (Bound _)  = raise "Variable ligada inesperada en eval"
evalTerm2 (Free n)     = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")
evalTerm2 (Lam t)      = raise "Funcion sin termino para reemplazar en la variable"
evalTerm2 (Lam u :@: v) = evalTerm2 (sub 0 v u)
evalTerm2 (u :@: v) = raise "Termino trabado: no se puede realizar la aplicacion"--Las aplicaciones ya se resolvieron en el beta redex, si se encuentra una aplicacion se mandara un mensaje
evalTerm2 (IC dim) = ErrorMT (do x <- readImageRGBA VU dim--podria usar el isValid de la biblioteca de Path para revisar que la direccion sea valida y mandar un error
                                 return (JustE x))
evalTerm2 (BinOp f e1 e2) = (evalTerm2 e1) >>= (\x -> (evalTerm2 e2) >>= (\y -> if ((dims x) == (dims y)) then return (blend x y (convfb f)) else let (x',y')=cut x y
                                                                                                                                                    in return (blend x' y' (convfb f))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm2 (UnOp f e1 d) = (evalTerm2 e1) >>= (\x -> return (edit (convfu f) x d))
evalTerm2 (Complement e)= (evalTerm2 e) >>= (\x -> return (I.map opposite x))


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

evalTerm3::Term->ErrorMT IO (Image VU RGBA Double)
evalTerm3 (Bound _)  = raise "Variable ligada inesperada en eval"
evalTerm3 (Free n)     = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")
evalTerm3 (Lam t)      = raise "Funcion sin termino para reemplazar en la variable"
evalTerm3 (Lam u :@: v) = evalTerm3 (sub 0 v u)
evalTerm3 (u :@: v) = raise "Termino trabado: no se puede realizar la aplicacion"--Las aplicaciones ya se resolvieron en el beta redex, si se encuentra una aplicacion se mandara un mensaje
evalTerm3 (IC dim) = ErrorMT (do x <- readImageRGBA VU dim--podria usar el isValid de la biblioteca de Path para revisar que la direccion sea valida y mandar un error
                                 return (JustE x))
evalTerm3 (BinOp f e1 e2) = (evalTerm3 e1) >>= (\x -> (evalTerm3 e2) >>= (\y -> if ((dims x) == (dims y)) then return (blend x y (convfb f)) else let (x',y')=adjust x y
                                                                                                                                                    in return (blend x' y' (convfb f))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm3 (UnOp f e1 d) = (evalTerm3 e1) >>= (\x -> return (edit (convfu f) x d))
evalTerm3 (Complement e)= (evalTerm3 e) >>= (\x -> return (I.map opposite x))




data FileOptions = FileOptions
   { fApp :: String-- App (termino leido en el archivo) string a parsear
     , fDir :: String-- Direccion (ademas del nombre y la extension) en donde se guardara el archivos
     , fMode :: Int --Modo de evaluacion que utiliza para --exact/resizeup/resizedown
     --podria separarse fileDir en nombre y etenxion como argumentos aparte
   }

instance Options FileOptions where
   defineOptions = pure FileOptions
       <*> simpleOption "exec" []
           "Si vas a realizar una aplicacion de un termino sobre lo leido en el archivo"
       <*> simpleOption "d" "output.png"
           "Direccion (ademas del nombre y la extension) en donde se guardara el archivos"
       <*> simpleOption "m" 1
           "Modo de evaluacion: 1 - Las imagenes que se usaran en funciones con dos argumentos necesitaran tener las mismas dimensiones\n                                 2 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el menor tamaño de ambas\n                                 3 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el mayor tamaño de ambas"


data InterpetOptions = InterpetOptions
   { iApp :: String-- App (termino leido en el archivo) string a parsear
     , iDir :: String-- Direccion (ademas del nombre y la extension) en donde se guardara el archivos
     , iMode :: Int --Modo de evaluacion que utiliza para --exact/resizeup/resizedown
     --podria separarse fileDir en nombre y etenxion como argumentos aparte
   }

instance Options InterpetOptions where
   defineOptions = pure InterpetOptions
       <*> simpleOption "exec" []
           "Si vas a realizar una aplicacion de lo leido en un archivo sobre el termino"
       <*> simpleOption "d" "output.png"
           "Direccion (ademas del nombre y la extension) en donde se guardara el archivos"
       <*> simpleOption "m" 1
           "Modo de evaluacion: 1 - Las imagenes que se usaran en funciones con dos argumentos necesitaran tener las mismas dimensiones\n                                 2 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el menor tamaño de ambas\n                                 3 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el mayor tamaño de ambas"

--Funciones de evaluacion para el main
--------------------------------------------------------------------------------------
eval1 t s= let a= runErrorMT(evalTerm1 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage s x
                            EM s -> print s)

eval2 t s= let a= runErrorMT(evalTerm2 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage s x
                            EM s -> print s)

eval3 t s= let a= runErrorMT(evalTerm3 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage s x
                            EM s -> print s)
--------------------------------------------------------------------------------------

--Funcion main principal
main :: IO ()
main = do prog <- getProgName
          args <- getArgs
          if ((head args)=="i" || (head args)=="interpret") then maini (head (tail args))
                                                            else if ((head args)=="f" || (head args)=="file") then mainf (head (tail args))
                                                                                                              else print "Error de argumentos"
--Funcion utilizada para agregar argumentos con aplicaciones al termino parseado
addappi :: String -> String -> IO [Char]
addappi s xs=  case (parse (sepBy (many (sat (\v->v/=','))) (char ',')) xs) of
                [([""],"")] -> return s--addappi' s []
                [(x,y)]-> addappi' s x

addappi'::String->[String]->IO String
addappi' s []   = return s
addappi' s (x:xs) = let a= readFile x
                    in a>>= (\v ->addappi' ("App ("++s++") "++v) xs )

--Main para interprete
maini :: String->IO ()
maini x= runCommand $ \opts args -> do
                              let a=addappi x (fApp opts)
                                in a>>= (\v ->case (fMode opts) of--(\v-> print v)
                                              1 -> eval1 v (fDir opts)
                                              2 -> eval2 v (fDir opts)
                                              3 -> eval3 v (fDir opts))

--Funcion utilizada para agregar argumentos con aplicaciones al termino parseado
addappf s xs= addappf' s (fst(head(parse (sepBy (many (sat (\v->v/=','))) (char ',')) xs)))

addappf'::String->[String]->String
addappf' s (x:xs) = addappf' ("App ("++s++") "++x) xs
addappf' s []   = s

----Main para lectura de archivo
mainf :: String->IO ()
mainf x= runCommand $ \opts args  -> do
             let a = readFile x
               in case fMode opts of
                   1 -> a>>= (\v -> eval1 (addappf v (fApp opts)) (fDir opts)) --(\v -> print (addapp v (fApp opts)))--
                   2 -> a>>= (\v -> eval2 (addappf v (fApp opts)) (fDir opts))
                   3 -> a>>= (\v -> eval3 (addappf v (fApp opts)) (fDir opts))

--runhaskell Eval.hs i "Darken I cluster.jpg I pizza.png"
--runhaskell Eval.hs i "Darken I cluster.jpg I pizza.png" --d='/home/nico/Desktop/output.png'
--runhaskell Eval.hs i "Abs x Screen x I pizza.png" --d='/home/nico/Desktop/output.png' --exec='test.f'
--runhaskell Eval.hs f test2.f  --d='/home/nico/Desktop/output.png' --exec='I cluster.jpg'

--runhaskell Eval.hs i "Normal </home/nico/Desktop/F.png> <pizza.png>" --m='2' --d='preview'
--runhaskell Eval.hs i "Contrast (Sat (Temp </home/nico/Desktop/vikshot.jpg> 3000) 0.5) 0.3"
--runhaskell Eval.hs i "Contrast (Sat </home/nico/Desktop/vikshot.jpg> 0.5) 0.3" --d='output2.png'

--eval1 "App (App (Abs x (Abs y Normal x y))<pizza.png>) <cluster.jpg>"
--runhaskell Eval.hs i "Abs x (Abs y Normal x y)" --exec='test.f,test2.f'


--runhaskell Eval.hs f testm.f  --d='/home/nico/Desktop/output.png' --exec='<pizza.png>, <cluster.jpg>'

--runhaskell Eval.hs i "Blacks <cluster.jpg> 0.5" --d='/home/nico/Desktop/output.png'

--runhaskell Eval.hs i "Normal <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/normal.png'
--runhaskell Eval.hs i "Add <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/add.png'
--runhaskell Eval.hs i "Diff <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/diff.png'
--runhaskell Eval.hs i "Darken <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/darken.png'
--runhaskell Eval.hs i "Lighten <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/lighten.png'
--runhaskell Eval.hs i "Multiply <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/multiply.png'
--runhaskell Eval.hs i "Screen <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/screen.png'
--runhaskell Eval.hs i "Overlay <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/overlay.png'
--runhaskell Eval.hs i "HardLight <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/hardlight.png'
--runhaskell Eval.hs i "SoftLight <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/softlight.png'
--runhaskell Eval.hs i "ColorDodge <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/colordodge.png'
--runhaskell Eval.hs i "ColorBurn <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/colorburn.png'
--runhaskell Eval.hs i "Hue <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/hue.png'
--runhaskell Eval.hs i "Luminosity <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/luminosity.png'
--runhaskell Eval.hs i "BlendColor <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/blendcolor.png'
--runhaskell Eval.hs i "BlendSat <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/blendsat.png'
--runhaskell Eval.hs i "Exclusion <cluster.jpg> <pizza.png>" --d='/home/nico/Desktop/exclusion.png'
---------------------------------------------------------------------
--Ver que pasa con estos casos
-- *Parser> parsear "(Contrast (Sat (Temp </home/nico/Desktop/vikshot.jpg> 3000) 0.5) 0.3)"
--[]
-- *Parser> parsear "(Contrast (Sat (Temp </home/nico/Desktop/vikshot.jpg> 3000) 3000) 3000)"
--[(LUnOp Contrast (LBinOp BlendSat (LUnOp Temp (LIC "/home/nico/Desktop/vikshot.jpg") 3000.0) (LVar "3000")) 3000.0,"")]

--Si se quiere implementar catching de errores: http://hackage.haskell.org/package/enclosed-exceptions-1.0.3/docs/Control-Exception-Enclosed.html
