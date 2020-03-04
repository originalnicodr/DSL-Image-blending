module Parser where
--Dependencias
{-
Graphics algo
z lib de case c
path

-}


--import Text.ParserCombinators.Parsec
--import Text.Parsec.Token
--import Text.Parsec.Language (emptyDef)
import Common
import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
--import Path
import System.FilePath.Posix
import System.Environment (getArgs)
--import Parsing.lhs
import Options

-----------------------
-- Funcion para facilitar el testing del parser.
{-totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof --Verifica que no queda nada en el parser
                  return t-}

-- Analizador de Tokens - Esto lo deberia sacar?
{-lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","if","then",
                                                     "else", "while", "skip"]
                                  , reservedOpNames = ["+","-","=","-","*","/","!","(",")","{","}"]
                                  })
-}
{-
data Config = Config
    { cIn  :: Maybe String
    , cOut :: Maybe String
    } deriving Show

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

--sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

--main :: IO ()
main = greet =<< execParser opts
 where
   opts = info (sample <**> helper)
     ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()-}






floatParser :: Parser String --usar con many1
floatParser = do x<-sat (\x -> (x=='-') || (isDigit x) || (x=='.')) --podria mejorarse para que solo exista un punto
                 xs<-many (sat (\x -> (isDigit x) || (x=='.')))
                 return (x:xs)



directory :: Parser Char
directory =  sat (\x -> (isAlphaNum x) || (x=='.') || (isPathSeparator x)) --No acepta direcciones con espacios, podria poner algo para limitarlo tipo <>

directory2 :: Parser Char
directory2 = sat (\x -> (x/='>'))

bopParser = (do (string "Normal"<|> string "N ")
                return (\x y-> LBinOp Normal x y))
            <|>(do (string "Add"<|> string "A ")
                   return (\x y-> LBinOp Add x y))
                <|>(do (string "Diff"<|> string "Difference ")
                       return (\x y-> LBinOp Diff x y))
                    <|>(do (string "ColorDodge"<|> string "CD ")
                           return (\x y-> LBinOp ColorDodge x y))
                        <|>(do (string "ColorBurn"<|> string "CB ")
                               return (\x y-> LComplement (LBinOp ColorDodge (LComplement x) (LComplement y))))--return (\x y-> LComplement ColorBurn x y))
                            <|>(do (string "Darken"<|> string "D ")
                                   return (\x y-> LBinOp Darken x y))
                                <|>(do (string "Lighten"<|> string "L ")
                                       return (\x y-> LBinOp Lighten x y))
                                    <|>(do (string "Multiply"<|> string "M ")
                                           return (\x y->LBinOp Multiply x y))--Screen (LComplement x) (LComplement y)))--return (\x y-> LComplement Multiply x y))
                                        <|>(do (string "Screen"<|> string "S ")
                                               return (\x y-> LComplement (LBinOp Multiply (LComplement x) (LComplement y))))
                                            <|>(do (string "Overlay"<|> string "O ")
                                                   return (\x y-> LBinOp Overlay x y))
                                                <|>(do (string"HardLight"<|> string "HL ")
                                                       return (\x y-> LBinOp HardLight x y))--return (\x y-> LBinOp HardLight x y))
                                                    <|>(do (string "SoftLight"<|> string "SL ")
                                                           return (\x y-> LBinOp SoftLight x y))
                                                        <|>(do (string "Hue"<|> string "H ")
                                                               return (\x y-> LBinOp Hue x y))
                                                            <|>(do (string "Luminosity"<|>string "Lum ")
                                                                   return (\x y-> LBinOp Luminosity x y))
                                                                <|>(do (string "Exclusion"<|> string "E ")
                                                                       return (\x y-> LBinOp Exclusion x y))
                                                                    <|>(do (string "BlendColor"<|> string "C " <|> string "BC ")
                                                                           return (\x y-> LBinOp BlendColor x y))
                                                                        <|>(do (string "BlendSaturation"<|> string "Saturation"<|> string "Sat " <|> string "BC ")
                                                                               return (\x y-> LBinOp BlendSat x y))

uopParser :: Parser UOp
uopParser =(do (string "Temp"<|> string "Temperature"<|> string "T ")
               return Temp)
                <|>(do (string "Sat"<|> string "Saturation"<|> string "S ")
                       return Sat)
                    <|>(do (string "Vib"<|> string "Vibrance"<|> string "V ")
                           return Vib)
                        <|>(do (string "Exposure"<|> string "Exp"<|> string "E ")
                               return Exposure)
                            <|>(do (string "Contrast"<|> string "Cont"<|> string "C ")
                                   return Contrast)
                                <|>(do string "Shadows"
                                       return Shadows)
                                    <|>(do string "Highlights"
                                           return Highlights)
                                        <|>(do string "Whites"
                                               return Whites)
                                            <|>(do string "Blacks"
                                                   return Blacks)
                                                <|>(do (string "Opacity"<|> string "O ")
                                                       return Opacity)


--Lenguaje de escritura
{-LamTerm  ::=
            |  '\' String LamTerm --no se si se puede hacer
            |  App LamTerm LamTerm --por las dudas le dejo el app
            |  I String --imagen A partir de aca esta lo que agregue
            |  Op LamTerm LamTerm
            |  BOp LamTerm LamTerm
            |  UOp LamTerm Float
            |  Complement LamTerm
            | String --variable
            | ‘(‘ Exp ‘)’-}

--I \direccion

--parserLT:: Parser LamTerm --Voy a tener que cambiar lo que recibe para que sea mas lindo de escribir
parserLT =        (do symbol "Abs" --hay que ver si me deja ver este caracter
                      space
                      v <- many1 (sat isAlphaNum) --los nombres de variables spueden ser alfanumericos
                      space
                      e <- parserLT
                      return (Abs v e)) --deberia tener parentesis el e?
                      <|>(do symbol "App" -- No pasar por la Aplicacion creo que seria muy complicado
                             e1 <- parserLT
                             space
                             e2 <- parserLT
                             space
                             return (App e1 e2))
                             <|>(do symbol "<"
                                    d <- many1 directory2 --chequear que creo que no le va a caer bien al evaluador
                                    symbol ">"
                                    space
                                    return (LIC d))
                                    <|>(do f <- bopParser
                                           space
                                           e1 <- parserLT
                                           space
                                           e2 <- parserLT
                                           space
                                           return (f e1 e2))
                                           <|> (do f <- uopParser
                                                   space
                                                   e1 <- parserLT
                                                   space
                                                   d <- floatParser
                                                   space
                                                   return (LUnOp f e1 (read d::Double)))
                                                   <|> (do symbol "Complement"
                                                           e <- parserLT
                                                           space
                                                           return (LComplement e))
                                                           <|> (do v <- many1 alphanum --los nombres de variables spueden ser alfanumericos (si llego aca significa que no va a guardar nada que no sea una variable?)
                                                                   space
                                                                   return (LVar v))
                                                                   <|>(do symbol "("
                                                                          space
                                                                          e <- parserLT
                                                                          space
                                                                          symbol ")"
                                                                          space
                                                                          return e)
test = (do symbol "<"
           d <- many1 directory2 --chequear que creo que no le va a caer bien al evaluador
           symbol ">"
           space
           return (LIC d))
-- /cluster.jpg

------------------------------------
-- Función de parseo
------------------------------------
--parseComm :: SourceName -> String -> Either ParseError LamTerm
parsear = parse parserLT

{-main :: IO ()
main = do arg:_ <- getArgs
          run arg-}
