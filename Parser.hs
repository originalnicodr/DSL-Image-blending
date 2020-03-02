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

bopParser = (do string "Normal"
                return Normal)
            <|>(do string "Add"
                   return Add)
                <|>(do string "Diff"
                       return Diff)
                    <|>(do string "ColorDodge"
                           return ColorDodge)
                        <|>(do string "ColorBurn"
                               return ColorBurn)
                            <|>(do string "Darken"
                                   return Darken)
                                <|>(do string "Lighten"
                                       return Lighten)
                                    <|>(do string "Multiply"
                                           return Multiply)
                                        <|>(do string "Screen"
                                               return Screen)
                                            <|>(do string "Overlay"
                                                   return Overlay)
                                                <|>(do string"HardLight"
                                                       return HardLight)
                                                    <|>(do string "SoftLight"
                                                           return SoftLight)
                                                        <|>(do string "Hue"
                                                               return Hue)
                                                            <|>(do string "Luminosity"
                                                                   return Luminosity)
                                                                <|>(do string "Exclusion"
                                                                       return Exclusion)
                                                                    <|>(do string "Color "--hace falta este espacio?
                                                                           return BlendColor)
                                                                        <|>(do string "BlendSaturation"
                                                                               return BlendSat)

uopParser :: Parser UOp
uopParser =(do string "Temp"
               return Temp)
                <|>(do string "Sat"
                       return Sat)
                    <|>(do string "Vib"
                           return Vib)
                        <|>(do string "Exposure"
                               return Exposure)
                            <|>(do string "Contrast"
                                   return Contrast)
                                <|>(do string "Shadows"
                                       return Shadows)
                                    <|>(do string "Highlights"
                                           return Highlights)
                                        <|>(do string "Whites"
                                               return Whites)
                                            <|>(do string "Blacks"
                                                   return Blacks)
                                                <|>(do string "Opacity"
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
                                           return (LBinOp f e1 e2))
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
