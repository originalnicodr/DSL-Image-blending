{-# LANGUAGE FlexibleContexts #-}
import Graphics.Image as I
import Parser
import Eval
import Parsing

import Options
import System.Environment (getArgs, getProgName)

data FileOptions = FileOptions
   { fApp :: String-- App (termino leido en el archivo) string a parsear
     , fDir :: String-- Direccion (ademas del nombre y la extension) en donde se guardara el archivos
     , fMode :: Int --Modo de evaluacion que utiliza
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
   { iApp :: String-- App (termino parseado) archivos a parsear
     , iDir :: String-- Direccion (ademas del nombre y la extension) en donde se guardara el archivos
     , iMode :: Int --Modo de evaluacion que utiliza
   }

instance Options InterpetOptions where
   defineOptions = pure InterpetOptions
       <*> simpleOption "exec" []
           "Si vas a realizar una aplicacion de lo leido en un archivo sobre el termino"
       <*> simpleOption "d" "output.png"
           "Direccion (ademas del nombre y la extension) en donde se guardara el archivos"
       <*> simpleOption "m" 1
           "Modo de evaluacion: 1 - Las imagenes que se usaran en funciones con dos argumentos necesitaran tener las mismas dimensiones\n                                 2 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el menor tamaño de ambas\n                                 3 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el mayor tamaño de ambas"


--Funcion de evaluacion que toma como argumento el termino a parsear, la direccion en la que se guardara la imagen y una funcion de evaluacion de termino (evalTerm1, evalTerm2, evalTerm3)
eval t s feval= case (parsear t) of
                []-> print "Error de parseo, revise el termino ingresado"
                t'-> let a=runErrorMT(feval (conversion (fst (head t'))))
                      in a >>= (\i -> case i of
                                      JustE x -> writeImage s x
                                      EM e -> print e)

--Funcion utilizada para agregar argumentos con aplicaciones al termino parseado
addappi :: String -> String -> IO [Char]
addappi s xs=  case (parse (sepBy (many (sat (\v->v/=','))) (char ',')) xs) of
                [([""],"")] -> return s
                [(x,y)]-> addappi' s x

addappi'::String->[String]->IO String
addappi' s []   = return s
addappi' s (x:xs) = let a= readFile x
                    in a>>= (\v ->addappi' ("App ("++s++") "++v) xs )

--Funcion utilizada para agregar argumentos con aplicaciones al termino parseado
addappf s xs= addappf' s (fst(head(parse (sepBy (many (sat (\v->v/=','))) (char ',')) xs)))

addappf'::String->[String]->String
addappf' s (x:xs) = addappf' ("App ("++s++") "++x) xs
addappf' s []   = s

--Funcion main principal
main :: IO ()
main = do prog <- getProgName
          args <- getArgs
          if ((head args)=="i" || (head args)=="interpret") then maini (head (tail args))
                                                            else if ((head args)=="f" || (head args)=="file") then mainf (head (tail args))
                                                                                                              else print "Error de argumentos"

--Main para interprete
maini :: String->IO ()
maini x= runCommand $ \opts args -> do
                              let a=addappi x (fApp opts)
                                in a>>= (\v ->case (fMode opts) of
                                              1 -> eval v (fDir opts) evalTerm1
                                              2 -> eval v (fDir opts) evalTerm2
                                              3 -> eval v (fDir opts) evalTerm3)

----Main para lectura de archivo
mainf :: String->IO ()
mainf x= runCommand $ \opts args  -> do
             let a = readFile x
               in case fMode opts of
                   1 -> a>>= (\v -> eval (addappf v (fApp opts)) (fDir opts) evalTerm1)
                   2 -> a>>= (\v -> eval (addappf v (fApp opts)) (fDir opts) evalTerm2)
                   3 -> a>>= (\v -> eval (addappf v (fApp opts)) (fDir opts) evalTerm3)
