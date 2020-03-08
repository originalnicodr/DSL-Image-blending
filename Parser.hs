module Parser where
import Common
import Parsing
import Data.Char
import Control.Applicative hiding (many)

--Parser de numeros flotantes (no realiza verificacion necesaria, solo que los caracteres sean acordes a un flotante)
floatParser :: Parser String
floatParser = do x<-sat (\x -> (x=='-') || (isDigit x) || (x=='.')) --podria mejorarse para que solo exista un punto y un signo negativo
                 xs<-many (sat (\x -> (isDigit x) || (x=='.')))
                 return (x:xs)

--Parser utilizado para parsear de directorios (estos estaran entre los caracteres <>)
directory :: Parser Char
directory = sat (\x -> (x/='>'))

--Parser de operaciones de tipo Op
bopParser :: Parser Op
bopParser = (do (string "Normal"<|> string "N ")
                return Normal)
            <|>(do (string "Add"<|> string "A ")
                   return Add)
                <|>(do (string "Diff"<|> string "Difference ")
                       return Diff)
                    <|>(do (string "ColorDodge"<|> string "CD "<|> string "Color Dodge")
                           return ColorDodge)
                        <|>(do (string "ColorBurn"<|> string "CB "<|> string "Color Burn")
                               return ColorBurn)
                            <|>(do (string "Darken"<|> string "D ")
                                   return Darken)
                                <|>(do (string "Lighten"<|> string "L ")
                                       return Lighten)
                                    <|>(do (string "Multiply"<|> string "M ")
                                           return Multiply)
                                        <|>(do (string "Screen"<|> string "S ")
                                               return Screen)
                                            <|>(do (string "Overlay"<|> string "O ")
                                                   return Overlay)
                                                <|>(do (string"HardLight"<|> string "HL "<|> string "Hard Light")
                                                       return HardLight)
                                                    <|>(do (string "SoftLight"<|> string "SL "<|> string "Soft Light")
                                                           return SoftLight)
                                                        <|>(do (string "Hue"<|> string "H ")
                                                               return Hue)
                                                            <|>(do (string "Luminosity"<|>string "Lum ")
                                                                   return Luminosity)
                                                                <|>(do (string "Exclusion"<|> string "E ")
                                                                       return Exclusion)
                                                                    <|>(do (string "BlendColor"<|> string "C " <|> string "BC "<|> string "Blend Color")
                                                                           return BlendColor)
                                                                        <|>(do (string "BlendSaturation"<|> string "BC "<|> string "Blend Saturation"<|>string "BlendSat")
                                                                               return BlendSat)

--Parser de operaciones de tipo UOp
uopParser :: Parser UOp
uopParser =(do (string "Temp "<|> string "Temperature"<|> string "T ")
               return Temp)
                <|>(do (string "Sat "<|> string "Saturation"<|> string "S ")
                       return Sat)
                    <|>(do (string "Exposure"<|> string "Exp ")
                           return Exposure)
                        <|>(do (string "Contrast"<|> string "Cont ")
                               return Contrast)
                            <|>(do string "Shadows"
                                   return Shadows)
                                <|>(do string "Highlights"
                                       return Highlights)
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
                                    d <- many1 directory
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
parsear :: String -> [(LamTerm, String)]
parsear = parse parserLT
