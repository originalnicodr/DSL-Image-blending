import Control.Applicative
import Options
import System.Environment (getArgs, getProgName)
--

data FileOptions = FileOptions
    { fileApp :: String-- App (termino leido en el archivo) string a parsear
      , fileDir :: String-- Direccion (ademas del nombre y la extension) en donde se guardara el archivos
      , fileMode :: Int --Modo de evaluacion que utiliza para --exact/resizeup/resizedown
      --podria separarse fileDir en nombre y etenxion como argumentos aparte
    }

instance Options FileOptions where
    defineOptions = pure FileOptions
        <*> simpleOption "exec" " "
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
        <*> simpleOption "exec" " "
            "Si vas a realizar una aplicacion de lo leido en un archivo sobre el termino"
        <*> simpleOption "d" "output.png"
            "Direccion (ademas del nombre y la extension) en donde se guardara el archivos"
        <*> simpleOption "m" 1
            "Modo de evaluacion: 1 - Las imagenes que se usaran en funciones con dos argumentos necesitaran tener las mismas dimensiones\n                                 2 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el menor tamaño de ambas\n                                 3 - Las imagenes aplicadas en funciones binarias daran una imagen resultante con el mayor tamaño de ambas"


main :: IO ()
main = do prog <- getProgName
          args <- getArgs
          if ((head args)=="i" || (head args)=="interpret") then main2 (tail args)
                            else if ((head args)=="f" || (head args)=="file") then main3 (tail args)
                                                                          else print "Error de argumentos"

main2 :: [String]->IO ()
main2 (x:xs)= let a = readFile fileApp
              in a>>= (\v -> runCommand $ \opts args -> do
                        if (fileApp==" ") then case fileMode of
                                                1 -> print "eval1 x fileDir"
                                                2 -> print "eval2 x fileDir"
                                                3 -> print "eval3 x fileDir"
                        else let x'="App ("++v++") "++x
                              in  case fileMode of
                                    1 -> print "eval1 x' fileDir"
                                    2 -> print "eval2 x' fileDir"
                                    3 -> print "eval3 x' fileDir")

{-
main3 :: [String]->IO ()
main3 x:xs= runCommand $ \opts args  -> do
            let a = readFile x
             in case iMode of
                  1 -> a>>= eval1 ((\v -> eval1 (if (iApp==" ") then x else "App ("++v++") "++iApp) iDir
                  2 -> a>>= eval2 (if (iApp==" ") then x else "App ("++v++") "++iApp) iDir
                  3 -> a>>= eval3 (if (iApp==" ") then x else "App ("++v++") "++iApp) iDir
-}
