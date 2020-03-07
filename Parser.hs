module Parser where

import Common
import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import System.FilePath.Posix
import System.Environment (getArgs)
import Options
--Parser de numeros flotantes (no realiza verificacion necesaria, solo que los caracteres sean acordes a un flotante)
floatParser :: Parser String
floatParser = do x<-sat (\x -> (x=='-') || (isDigit x) || (x=='.')) --podria mejorarse para que solo exista un punto y un signo negativo
                 xs<-many (sat (\x -> (isDigit x) || (x=='.')))
                 return (x:xs)

--Parser utilizado para parsear de directorios (estos estaran entre los caracteres <>)
directory2 :: Parser Char
directory2 = sat (\x -> (x/='>'))

--Parser de operaciones de tipo Op
bopParser :: Parser (LamTerm -> LamTerm -> LamTerm)
bopParser = (do (string "Normal"<|> string "N ")
                return (\x y-> LBinOp Normal x y))
            <|>(do (string "Add"<|> string "A ")
                   return (\x y-> LBinOp Add x y))
                <|>(do (string "Diff"<|> string "Difference ")
                       return (\x y-> LBinOp Diff x y))
                    <|>(do (string "ColorDodge"<|> string "CD ")
                           return (\x y-> LBinOp ColorDodge x y))
                        <|>(do (string "ColorBurn"<|> string "CB ")
                               return (\x y-> LBinOp ColorBurn x y))--LComplement (LBinOp ColorDodge (LComplement x) (LComplement y))))--return (\x y-> LComplement ColorBurn x y))
                            <|>(do (string "Darken"<|> string "D ")
                                   return (\x y-> LBinOp Darken x y))
                                <|>(do (string "Lighten"<|> string "L ")
                                       return (\x y-> LBinOp Lighten x y))
                                    <|>(do (string "Multiply"<|> string "M ")
                                           return (\x y->LBinOp Multiply x y))--Screen (LComplement x) (LComplement y)))--return (\x y-> LComplement Multiply x y))
                                        <|>(do (string "Screen"<|> string "S ")
                                               return (\x y-> LBinOp Screen x y))--LComplement (LBinOp Multiply (LComplement x) (LComplement y))))
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
                                                                        <|>(do (string "BlendSaturation"<|> string "BC ")
                                                                               return (\x y-> LBinOp BlendSat x y))

--Parser de operaciones de tipo UOp
uopParser :: Parser UOp
uopParser =(do (string "Temp "<|> string "Temperature"<|> string "T ")
               return Temp)
                <|>(do (string "Sat "<|> string "Saturation"<|> string "S ")
                       return Sat)
                    <|>(do (string "Vib "<|> string "Vibrance"<|> string "V ")
                           return Vib)
                        <|>(do (string "Exposure"<|> string "Exp ")
                               return Exposure)
                            <|>(do (string "Contrast"<|> string "Cont ")
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

--Parser de Terminos
parserLT:: Parser LamTerm
parserLT =        (do symbol "Abs"
                      space
                      v <- many1 (sat isAlphaNum) --los nombres de variables spueden ser alfanumericos
                      space
                      e <- parserLT
                      return (Abs v e))
                      <|>(do symbol "App"
                             e1 <- parserLT
                             space
                             e2 <- parserLT
                             space
                             return (App e1 e2))
                             <|>(do symbol "<"
                                    d <- many1 directory2
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
                                                           <|> (do v <- many1 alphanum --los nombres de variables pueden ser alfanumericos (Muchas veces el escribir mal un termino resultara en parsear algo como variable cuando no lo es)
                                                                   space
                                                                   return (LVar v))
                                                                   <|>(do symbol "("
                                                                          space
                                                                          e <- parserLT
                                                                          space
                                                                          symbol ")"
                                                                          space
                                                                          return e)
------------------------------------
-- Función de parseo
------------------------------------
--parseComm :: SourceName -> String -> Either ParseError LamTerm
parsear = parse parserLT
