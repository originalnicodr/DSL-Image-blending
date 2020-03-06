--module Eval1 (eval) where
{-# LANGUAGE FlexibleContexts,ViewPatterns #-}
import Prelude as P
import Graphics.Image as I
import Common
import Data.List
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad.IO.Class

import Parser
import Parsing
--Necesario para usar el programa compilado
import Control.Applicative hiding(many)
import Options --este lo tienen que instalar con cabal
import System.Environment (getArgs, getProgName)
{-
data Paramters = P {
              output :: String,
              eval :: String
          } deriving(Show)
p = P { output= "/home/nico/Desktop/output.png", eval= "Exact"}-}

--Imports del SimplyTyped
{-
import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter-}

-- Para calmar al GHC

--time runhaskell Eval.hs i "Overlay <centaurus.jpg> Multiply <cluster.jpg> ColorBurn <mouse.png> <pizza.png>" --d='/home/nico/Desktop/output.png'

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
  lift m = ErrorMT (liftM JustE m)



instance Monad m => Functor (ErrorMT m) where
    fmap = liftM

instance Monad m => Applicative (ErrorMT m) where
    pure   = return
    (<*>)  = ap

--raise:: String ->ErrorMT m a
raise s= ErrorMT (return (EM s))


--Para parametros opcionales

{-
(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y


multiProduct req1 opt1 opt2 opt3 = req1 * (opt1 // 10) * (opt2 // 20) * (opt3 // 30)
-}


-- im2: foreground im1:background
fmap2 (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) f = (PixelRGBA ((f (r1*a1*(1-a2)) (r2*a2))/(a2+a1*(1-a2))) ((f (g1*a1*(1-a2)) (g2*a2))/(a2+a1*(1-a2))) ((f (b1*a1*(1-a2)) (b2*a2))/(a2+a1*(1-a2)))  (a2+a1*(1-a2)))

--fmap2 (PixelRGB r1 g1 b1)  (PixelRGB r2 g2 b2) f = (PixelRGB (f r1 r2) (f g1 g2) (f b1 b2))

{-fmap2 (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) f = let newa= (a1+(1-a2))--alpha resultante
                                                           in let m= a1*(1-a2)
                                                              in (PixelRGBA ((f (r1*m) r2)/m) ((f (g1*m) g2)/m) ((f (b1*m) b2)/m)  newa)
-}

--blend::(MArray arr RGB Double, Array arr1 RGB Double, Array arr1 RGB Double) =>Image arr RGB Double -> Image arr RGB Double -> ((Int, Int) -> Pixel RGB Double -> Pixel RGB Double)-> Image arr RGB Double
--blend::(MArray arr RGBA t,Array arr1 RGBA t,Array arr1 RGBA e) => Image arr1 RGBA t -> Image arr RGBA t -> (t -> t -> e) -> Image arr1 RGBA e --ATENCION: Estoy usando FlexibleContexts, sin setear eso en ghci se rompe
blend im1 im2 f = I.imap (\(i,j) p1 -> f p1 (index im2 (i,j))) im1
--blend im1 im2 f = I.imap (\(i,j) p1 -> fmap2 p1 (index im2 (i,j)) f) im1


edit f im d= (I.map (f d) im)

opposite= (\(PixelRGBA r g b a) -> (PixelRGBA (1-r) (1-g) (1-b) a))
--Esta era para toda la imagen
--opposite im = (I.map ((\(PixelRGBA r g b a) -> (PixelRGBA (1-r) (1-g) (1-b) a))) im)


--En estas funciones de blend, edit y opposite voy a estar usandolas con return para meterlas en el bind




--eval :: LamTerm -> ErrorM
--eval t = evalTerm (conversion (fst (head t)))
--eval (parsear "\\ x (Multiply x x)")

--La salida se rompe cuando interactuas jpg con png
eval' t= let a= runErrorMT(evalTerm' (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> displayImage x--writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)





eval2' t= let a= runErrorMT(evalTerm'2 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)

eval3' t= let a= runErrorMT(evalTerm'3 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)



--eval' "Darken <cluster.jpg> <pizza.png>"
--eval' "Darken <cluster.jpg> </home/nico/Desktop/test   _'0'02835/pizza.png>"
--eval "App (Abs x (Darken x I centaurus.jpg)) I cluster.jpg "
--Abs x App x (Abs x (Darken x I centaurus.jpg)))
--Abs x Abs y (Darken x y)
--eval "Highlights cluster.jpg 1"
--eval2 "Normal I /home/nico/Desktop/F.png I pizza.png"

evaldev t= let a= runErrorMT(evalTerm' t)
           in a >>= (\i -> case i of
                            JustE x -> writeImage "/home/nico/Desktop/output.png" x
                            EM s -> print s)

-- Ejecuta un programa a partir de su archivo fuente
--run :: [Char] -> IO ()
run ifile = let a = readFile ifile
            in a>>= eval'

--evaldev (BinOp Darken (IC "cluster.jpg") (IC "cluster.jpg"))
--ErrorMT IO (Image arr RGBA Double)

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion x= beta_red (conversion' [] x) 0

--Con el Term original
{-
conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs t u)  = Lam (conversion' (t:b) u)--hay que sacarle el tipado de aca
conversion' b (LIC s) = IC s
conversion' b (LBinOp f e1 e2) = BinOp f (conversion' b e1) (conversion' b e2) --(convfb f)
conversion' b (LUnOp f e d) = UnOp f (conversion' b e) d --(convfu f)
conversion' b (LComplement e) = Complement (conversion' b e)
-}

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs t u)  = Lam (conversion' (t:b) u)--hay que sacarle el tipado de aca
conversion' b (LIC s) = IC s
conversion' b (LBinOp f e1 e2) = BinOp f (conversion' b e1) (conversion' b e2) --(convfb f)
conversion' b (LUnOp f e d) = UnOp f (conversion' b e) d --(convfu f)
conversion' b (LComplement e) = Complement (conversion' b e)

convfb :: Op -> (Pixel RGBA Double->Pixel RGBA Double->Pixel RGBA Double)
convfb Normal      = blendpixel normald
convfb Add         = blendpixel addd
convfb Diff        = blendpixel differenced
convfb Darken      = blendpixel darkend
convfb Lighten     = blendpixel lightend
convfb Multiply    = blendpixel multiplyd
convfb Screen      = (\x y ->opposite(blendpixel multiplyd (opposite x) (opposite y))) --LComplement (LBinOp Multiply (LComplement x) (LComplement y))))
convfb Overlay     = blendpixel overlayd
convfb HardLight   = (\(PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) -> blendpixel overlayd (PixelRGBA r2 g2 b2 a1) (PixelRGBA r1 g1 b1 a2))--necesito acceso a los alphas
convfb SoftLight   = blendpixel softlightd
convfb ColorDodge  = blendpixel colordodged
convfb ColorBurn   = (\x y ->opposite(blendpixel colordodged (opposite x) (opposite y)))--LComplement (LBinOp ColorDodge (LComplement x) (LComplement y))
convfb Hue         = hue
convfb Luminosity  = luminosity
convfb BlendColor  = blendcolor
convfb BlendSat    = blendsat
convfb Exclusion   = blendpixel exclusiond

--convfu :: UOp -> (Double->Double->Double)
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


--IO (Image arr RGBA Double) -> IO (ErrorM (Image arr RGBA Double))
--liftIO (readimage)
--evalTerm :: Term-> ErrorMT IO ()
{-evalTerm t= let x = evalTerm' t
            in x >>= (\i -> (let y= (writeImage "output.jpg" i)--Lo malo de usar bind para guardar la imagen es que no tira mensaje de error
                             in x))
-}

--evalTerm' :: (Array arr1 RGBA Double) => Term -> ErrorMT IO (Image arr RGBA Double)
evalTerm' (Bound _)  = raise "Variable ligada inesperada en eval" --el return que usa deberia ser del IO
evalTerm' (Free n)     = raise ("Se identifico a "++(show n)++" como una variable libre, revise el termino ingresado")--fst $ fromJust $ lookup n e --para mi aca hay que poner un error
evalTerm' (Lam t)      = raise "Funcion sin termino para reemplazar en la variable"
evalTerm' (Lam u :@: v) = evalTerm' (sub 0 v u)
evalTerm' (u :@: v) = raise "Termino trabado: no se puede realizar la aplicacion"--ver que esto no me cague
evalTerm' (IC dim) = ErrorMT (do x <- readImageRGBA VU dim--podria usar el isValid de la biblioteca de Path para revisar que la direccion sea valida y mandar un error
                                 return (JustE x))
evalTerm' (BinOp f e1 e2) = (evalTerm' e1) >>= (\x -> (evalTerm' e2) >>= (\y -> if ((dims x) == (dims y)) then return (blend x y (convfb f)) else ErrorMT (return (EM "Las imagenes tienen dimensiones diferentes"))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm' (UnOp f e1 d) = (evalTerm' e1) >>= (\x -> return (edit (convfu f) x d))---es una funcion por que le falta pasar la imagen al final
evalTerm' (Complement e)= (evalTerm' e) >>= (\x -> return (I.map opposite x))--es aplicar la funcion de doubles opposite a cada pixel de e'

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

--Funciones que recortan dos imagenes para que tengan el mismo tamaño
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

--Otro evaluador que recorta la imagen mas grande sobre la mas chica

evalTerm'2::Term->ErrorMT IO (Image VU RGBA Double)
evalTerm'2 (Bound n)  = raise " es una variable ligada inesperada en eval" --el return que usa deberia ser del IO
evalTerm'2 (Free n)     = raise "es una variable libre"--fst $ fromJust $ lookup n e --para mi aca hay que poner un error
evalTerm'2 (Lam t)      = raise "funcion sin termino para reemplazar en la variable"
evalTerm'2 (Lam u :@: v) = evalTerm'2 (sub 0 v u)
evalTerm'2 (IC dim) = ErrorMT (do x <- readImageRGBA VU dim--podria usar el isValid de la biblioteca de Path para revisar que la direccion sea valida y mandar un error
                                  return (JustE x))
evalTerm'2 (BinOp f e1 e2) = (evalTerm'2 e1) >>= (\x -> (evalTerm'2 e2) >>= (\y -> if ((dims x) == (dims y)) then return (blend x y (convfb f)) else let (x',y')=cut x y
                                                                                                                                                      in return (blend x' y' (convfb f))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm'2 (UnOp f e1 d) = (evalTerm'2 e1) >>= (\x -> return (edit (convfu f) x d))---es una funcion por que le falta pasar la imagen al final
evalTerm'2 (Complement e)= (evalTerm'2 e) >>= (\x -> return (I.map opposite x))--es aplicar la funcion de doubles opposite a cada pixel de e'


--Funciones que agrega pixeles invisibles a dos imagenes para que tengan el mismo tamaño
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

evalTerm'3::Term->ErrorMT IO (Image VU RGBA Double)
evalTerm'3 (Bound n)  = raise " es una variable ligada inesperada en eval" --el return que usa deberia ser del IO
evalTerm'3 (Free n)     = raise "es una variable libre"--fst $ fromJust $ lookup n e --para mi aca hay que poner un error
evalTerm'3 (Lam t)      = raise "funcion sin termino para reemplazar en la variable"
evalTerm'3 (Lam u :@: v) = evalTerm'2 (sub 0 v u)
evalTerm'3 (IC dim) = ErrorMT (do x <- readImageRGBA VU dim--podria usar el isValid de la biblioteca de Path para revisar que la direccion sea valida y mandar un error
                                  return (JustE x))
evalTerm'3 (BinOp f e1 e2) = (evalTerm'2 e1) >>= (\x -> (evalTerm'2 e2) >>= (\y -> if ((dims x) == (dims y)) then return (blend x y (convfb f)) else let (x',y')=adjust x y
                                                                                                                                                      in return (blend x' y' (convfb f))))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
evalTerm'3 (UnOp f e1 d) = (evalTerm'2 e1) >>= (\x -> return (edit (convfu f) x d))---es una funcion por que le falta pasar la imagen al final
evalTerm'3 (Complement e)= (evalTerm'2 e) >>= (\x -> return (I.map opposite x))--es aplicar la funcion de doubles opposite a cada pixel de e'

{-evalTerm (BinOp f e1 e2) = ErrorMT (do e1' <- runErrorMT (evalTerm e1)
                                       e2' <- runErrorMT (evalTerm e2)
                                       if ((dims e1') == (dims e2')) then return (JustE (blend e1' e2' f)) else return(EM "Las imagenes tienen dimensiones diferentes"))--Como necesito los bind de IO para sacar las imagenes los return tienen que estar escritos asi
-}






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
                           --

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

eval1 t s= let a= runErrorMT(evalTerm' (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> if s=="preview" then displayImage x else writeImage s x
                            EM s -> print s)

eval2 t s= let a= runErrorMT(evalTerm'2 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> if s=="preview" then displayImage x else writeImage s x
                            EM s -> print s)

eval3 t s= let a= runErrorMT(evalTerm'3 (conversion (fst (head (parsear t)))))
            in a >>= (\i -> case i of
                            JustE x -> if s=="preview" then displayImage x else writeImage s x
                            EM s -> print s)


main :: IO ()
main = do prog <- getProgName
          args <- getArgs
          if ((head args)=="i" || (head args)=="interpret") then main2 (head (tail args))
                                                            else if ((head args)=="f" || (head args)=="file") then main3 (head (tail args))
                                                                                                              else print "Error de argumentos"

addapp2 s xs=  case (parse (sepBy (many (sat (\v->v/=','))) (char ',')) xs) of
                [(x,y)]-> addapp2' s x
                [] -> addapp2' s []
addapp2'::String->[String]->IO String
addapp2' s []   = return s
addapp2' s (x:xs) = let a= readFile x
                    in a>>= (\v ->addapp2' ("App ("++s++") "++v) xs )


main2 :: String->IO ()
main2 x= runCommand $ \opts args -> do
                              let a=addapp2 x (fApp opts)
                                in a>>= (\v ->case (fMode opts) of--(\v-> print v)
                                              1 -> eval1 v (fDir opts)
                                              2 -> eval2 v (fDir opts)
                                              3 -> eval3 v (fDir opts))

{-
main2 :: String->IO ()
main2 x= runCommand $ \opts args -> do
           if ((fApp opts)==" ") then case (fMode opts) of
                                   1 -> eval1 x (fDir opts)
                                   2 -> eval2 x (fDir opts)
                                   3 -> eval3 x (fDir opts)
                             else let a = readFile (fApp opts)
                                   in a>>= (\v ->case (fMode opts) of
                                                   1 -> eval1 ("App ("++x++") "++v) (fDir opts)
                                                   2 -> eval2 ("App ("++x++") "++v) (fDir opts)
                                                   3 -> eval3 ("App ("++x++") "++v) (fDir opts))
-}

addapp s xs= addapp' s (fst(head(parse (sepBy (many (sat (\v->v/=','))) (char ',')) xs)))
addapp'::String->[String]->String
addapp' s (x:xs) = addapp' ("App ("++s++") "++x) xs
addapp' s []   = s

main3 :: String->IO ()
main3 x= runCommand $ \opts args  -> do
             let a = readFile x
               in case fMode opts of
                   1 -> a>>= (\v -> eval1 (addapp v (fApp opts)) (fDir opts)) --(\v -> print (addapp v (fApp opts)))--
                   2 -> a>>= (\v -> eval2 (addapp v (fApp opts)) (fDir opts))
                   3 -> a>>= (\v -> eval3 (addapp v (fApp opts)) (fDir opts))

--runhaskell Eval.hs i "Darken I cluster.jpg I pizza.png"
--runhaskell Eval.hs i "Darken I cluster.jpg I pizza.png" --d='/home/nico/Desktop/output.png'
--runhaskell Eval.hs i "Abs x Screen x I pizza.png" --d='/home/nico/Desktop/output.png' --exec='test.f'
--runhaskell Eval.hs f test2.f  --d='/home/nico/Desktop/output.png' --exec='I cluster.jpg'

--runhaskell Eval.hs i "Normal </home/nico/Desktop/F.png> <pizza.png>" --m='2' --d='preview'
--runhaskell Eval.hs i "Contrast (Sat (Temp </home/nico/Desktop/vikshot.jpg> 3000) 0.5) 0.3"
--runhaskell Eval.hs i "Contrast (Sat </home/nico/Desktop/vikshot.jpg> 0.5) 0.3" --d='output2.png'

--eval1 "App (App (Abs x (Abs y Normal x y))<pizza.png>) <cluster.jpg>"


--runhaskell Eval.hs f testm.f  --d='/home/nico/Desktop/output.png' --exec='<pizza.png>, <cluster.jpg>'



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
