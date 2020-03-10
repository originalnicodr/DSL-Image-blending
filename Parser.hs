module Parser where
import Common
import Parsing
import Data.Char
import Control.Applicative hiding (many)

--Parser de numeros flotantes (no realiza verificacion necesaria, solo que los caracteres sean acordes a un flotante)
floatParser :: Parser String
floatParser = do x<-sat (\x -> (x=='-') || (isDigit x) || (x=='.')) --podria mejorarse para que solo exista un punto y un signo negativo
                 xs<-many (sat (\x -> (isDigit x) || (x=='.')))
                 space
                 return (x:xs)

--Parser utilizado para parsear de directorios (estos estaran entre los caracteres <>)
directory :: Parser Char
directory = sat (\x -> (x/='>'))

--Parser de operaciones de tipo Op
bopParser :: Parser Op
bopParser = (do (symbol "Normal"<|> symbol "N ")
                return Normal)
            <|>(do (symbol "Add"<|> symbol "A ")
                   return Add)
                <|>(do (symbol "Diff"<|> symbol "Difference ")
                       return Diff)
                    <|>(do (symbol "ColorDodge"<|> symbol "CD "<|> symbol "Color Dodge")
                           return ColorDodge)
                        <|>(do (symbol "ColorBurn"<|> symbol "CB "<|> symbol "Color Burn")
                               return ColorBurn)
                            <|>(do (symbol "Darken"<|> symbol "D ")
                                   return Darken)
                                <|>(do (symbol "Lighten"<|> symbol "L ")
                                       return Lighten)
                                    <|>(do (symbol "Multiply"<|> symbol "M ")
                                           return Multiply)
                                        <|>(do (symbol "Screen"<|> symbol "S ")
                                               return Screen)
                                            <|>(do (symbol "Overlay"<|> symbol "O ")
                                                   return Overlay)
                                                <|>(do (symbol"HardLight"<|> symbol "HL "<|> symbol "Hard Light")
                                                       return HardLight)
                                                    <|>(do (symbol "SoftLight"<|> symbol "SL "<|> symbol "Soft Light")
                                                           return SoftLight)
                                                        <|>(do (symbol "Hue"<|> symbol "H ")
                                                               return Hue)
                                                            <|>(do (symbol "Luminosity"<|>symbol "Lum ")
                                                                   return Luminosity)
                                                                <|>(do (symbol "Exclusion"<|> symbol "E ")
                                                                       return Exclusion)
                                                                    <|>(do (symbol "BlendColor"<|> symbol "BC "<|> symbol "Blend Color"<|> symbol "Color ")
                                                                           return BlendColor)
                                                                        <|>(do (symbol "BlendSaturation"<|> symbol "BS "<|> symbol "Blend Saturation"<|>symbol "BlendSat")
                                                                               return BlendSat)

--Parser de operaciones de tipo UOp
uopParser :: Parser UOp
uopParser =(do (symbol "Temp "<|> symbol "Temperature"<|> symbol "T ")
               return Temp)
                <|>(do (symbol "Sat "<|> symbol "Saturation")
                       return Sat)
                    <|>(do (symbol "Exposure"<|> symbol "Exp ")
                           return Exposure)
                        <|>(do (symbol "Contrast"<|> symbol "Cont "<|> symbol "C ")
                               return Contrast)
                            <|>(do symbol "Shadows"
                                   return Shadows)
                                <|>(do symbol "Highlights"
                                       return Highlights)
                                    <|>(do (symbol "Opacity")
                                           return Opacity)

--Parser de Terminos
parserLT:: Parser LamTerm
parserLT =        (do symbol "Abs"
                      v <- many1 (sat isAlphaNum) --los nombres de variables pueden ser alfanumericos
                      e <- parserLT
                      return (Abs v e))
                      <|>(do symbol "App"
                             e1 <- parserLT
                             e2 <- parserLT
                             return (App e1 e2))
                             <|>(do symbol "<"
                                    d <- many1 directory
                                    symbol ">"
                                    return (LIC d))
                                    <|>(do f <- bopParser
                                           e1 <- parserLT
                                           e2 <- parserLT
                                           return (LBinOp f e1 e2))
                                           <|> (do f <- uopParser
                                                   e <- parserLT
                                                   d <- floatParser
                                                   return (LUnOp f e (read d::Double)))
                                                   <|> (do symbol "Complement" <|>symbol "Comp"
                                                           e <- parserLT
                                                           return (LComplement e))
                                                           <|> (do v <- many1 alphanum --los nombres de variables pueden ser alfanumericos (Muchas veces el escribir mal un termino resultara en parsear algo como variable cuando no lo es)
                                                                   return (LVar v))
                                                                   <|>(do symbol "("
                                                                          e <- parserLT
                                                                          symbol ")"
                                                                          return e)
------------------------------------
-- Función de parseo
------------------------------------
parsear :: String -> [(LamTerm, String)]
parsear = parse parserLT
