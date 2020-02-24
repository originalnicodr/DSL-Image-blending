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




floatParser :: Parser Char --usar con many1
floatParser = sat (\x -> (isDigit x) || (x=='.')) --podria mejorarse para que solo exista un punto



directory :: Parser Char
directory =  sat (\x -> (isAlphaNum x) || (x=='.') || (isPathSeparator x)) --No acepta direcciones con espacios, podria poner algo para limitarlo tipo <>

bopParser :: Parser Op
bopParser = (do string "Normal"
                return Normal)
            <|>(do string "Add"
                   return Add)
                <|>(do string "Sub"
                       return Sub)
                    <|>(do string "Diff"
                           return Diff)
                        <|>(do string "Div"
                               return Div)
                            <|>(do string "Mult"
                                   return Mult)
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
                                                                        <|> failure

boolopParser :: Parser BOp
boolopParser =(do string "And"
                  return And)
                <|>(do string "Or"
                       return Or)
                    <|>(do string "Xor"
                           return Xor)
                        <|> failure

uopParser :: Parser UOp
uopParser =(do string "Temp"
               return Temp)
                <|>(do string "Sat"
                       return Sat)
                    <|>(do string "Multi"
                           return Multi)
                        <|> (do string "Power"
                                return Power)
                        <|> failure


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
parserLT =        (do char '\\' --hay que ver si me deja ver este caracter
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
                             <|>(do symbol "I"
                                    d <- many1 directory --chequear que creo que no le va a caer bien al evaluador
                                    space
                                    return (LIC (show d)))
                                    <|>(do f <- bopParser
                                           space
                                           e1 <- parserLT
                                           space
                                           e2 <- parserLT
                                           space
                                           return (LBinOp f e1 e2))
                                           <|> (do f <- boolopParser
                                                   space
                                                   e1 <- parserLT
                                                   space
                                                   e2 <- parserLT
                                                   space
                                                   return (LBoolOp f e1 e2))
                                                   <|> (do f <- uopParser
                                                           space
                                                           e1 <- parserLT
                                                           space
                                                           d <- many1 floatParser
                                                           return (LUnOp f e1 (read d::Float)))
                                                           <|> (do symbol "Complement"
                                                                   e <- parserLT
                                                                   space
                                                                   return (LComplement e))
                                                                   <|> (do v <- many1 alphanum --los nombres de variables spueden ser alfanumericos (si llego aca significa que no va a guardar nada que no sea una variable?)
                                                                           return (LVar v))
                                                                           <|>(do char '('
                                                                                  space
                                                                                  e <- parserLT
                                                                                  space
                                                                                  char ')'
                                                                                  space
                                                                                  return e)
test = do d <- item
          return (show d)
-- /cluster.jpg

------------------------------------
-- Función de parseo
------------------------------------
--parseComm :: SourceName -> String -> Either ParseError LamTerm
parsear = parse parserLT

{-main :: IO ()
main = do arg:_ <- getArgs
          run arg-}

-- Ejecuta un programa a partir de su archivo fuente
--run :: [Char] -> IO ()
run ifile = do s <- readFile ifile
               print (parsear s)
