module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Common

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof --Verifica que no queda nada en el parser
                  return t

-- Analizador de Tokens - Esto lo deberia sacar?
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","if","then",
                                                     "else", "while", "skip"]
                                  , reservedOpNames = ["+","-","=","-","*","/","!","(",")","{","}"]
                                  })



{-LamTerm  ::=  ‘\’ String --variable
            |  Abs String LamTerm
            |  App LamTerm LamTerm
            |  I String --imagen A partir de aca esta lo que agregue
            |  BinOp Op LamTerm LamTerm
            |  BoolOp BOp LamTerm LamTerm
            |  UnOp UOp LamTerm Float
            |  Complement LamTerm
            | ‘(‘ Exp ‘)’-}


--parserLT:: Parser LamTerm --Voy a tener que cambiar lo que recibe para que sea mas lindo de escribir
parserLT = (do char '('
              e <- parserLT
              char ')'
              return e)
            <|>(do char '\' --hay que ver si me deja ver este caracter 
                   v <- many1 alphanum --los nombres de variables spueden ser alfanumericos
                   e <- parserLT
                   return (Abs v e)) --deberia tener parentesis el e?
            <|>(do symbol "App" -- No pasar por la Aplicacion creo que seria muy complicado
                   e1 <- parserLT
                   e2 <- parserLT
                   return (App e1 e2))
            <|>(do symbol "I"
                   d <- many directory --chequear esto
                   return (LIC d))
            <|>(do symbol "BinOp"
                   f <- bopParser
                   e1 <- parserLT
                   e2 <- parserLT
                   return (LBinOp f e1 e2))
            <|> (do symbol "BoolOp"
                    f <- boolopParser
                    e1 <- parserLT
                    e2 <- parserLT
                    return (LBoolOp f e1 e2))
            <|> (do symbol "UnOp"
                    f <- uopParser
                    e1 <- parserLT
                    d <- floatparser
                    return (LUnOp f e1 d))
            <|> (do symbol "Complement"
                    e <- parserLT
                    return (LComplement e))
            <|> (do v <- many1 alphanum --los nombres de variables spueden ser alfanumericos (si llego aca significa que no va a guardar nada que no sea una variable?)
                    return (LVar v))
            <|> failure
            

directory :: Parser Char
directory =  sat isAlphaNum || (=='.') || (=='\') --Las cosas que pueden componer una direccion (podria hacerse mas fino)

bopParser :: Parser Op
bopParser = (do String "Normal"
                return Normal)
            <|>(do String "Add"
                   return Add)
                <|>(do String "Sub"
                       return Sub)
                    <|>(do String "Diff"
                           return Diff)
                        <|>(do String "Div"
                               return Div)
                            <|>(do String "Mult"
                                   return Mult)
                                <|>(do String "Darken"
                                       return Darken)
                                    <|>(do String "Lighten"
                                           return Lighten)
                                        <|>(do String "Multiply"
                                               return Multiply)
                                            <|>(do String "Screen"
                                                   return Screen)
                                                <|>(do String "Overlay"
                                                       return Overlay)
                                                    <|>(do String"HardLight"
                                                           return HardLight)
                                                        <|>(do String "SoftLight"
                                                               return SoftLight)
                                                            <|>(do String "Hue"
                                                                   return Hue)
                                                                <|>(do String "Luminosity"
                                                                       return Luminosity)
                                                                    <|>(do String "Exclusion"
                                                                           return Exclusion)
                                                                        <|> failure

boolopParser :: Parser BOp
boolopParser =(do String "And"
                  return And)
                <|>(do String "Or"
                       return Or)
                    <|>(do String "Xor"
                           return Xor)
                        <|> failure

uopParser :: Parser UOp
uopParser =(do String "Temp"
               return Temp)
                <|>(do String "Sat"
                       return Sat)
                    <|>(do String "Mult"
                           return Mult)
                        <|> (do String "Power"
                                return Power)
                        <|> failure

floatParser :: Parser Float --usar con many1
floatParser = sat isDigit || (=='.') --podria mejorarse para que solo exista un punto

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)