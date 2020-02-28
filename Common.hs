module Common where
import Graphics.Image as I

-- Comandos interactivos o de archivos
data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
            | Eval i                 --  Evaluar el término
  deriving (Show)

instance Functor Stmt where
  fmap f (Def s i) = Def s (f i)
  fmap f (Eval i)  = Eval (f i)
-- Tipos de los nombres

data Name
   =  Global  String
  deriving (Show, Eq)
-- Entornos

type NameEnv v t = [(Name, (v, t))]
-- Tipo de los tipos
data Type = Base
          | Fun Type Type
          deriving (Show, Eq)


--Editar Lenguaje para sacar BoolOp y para aceptar funciones en lugar de operaciones

-- Términos con nombres (modificado con mi lenguaje)
{-data LamTerm  =  LVar String
              |  Abs String LamTerm --Tengo que sacar el Type de aca, va no me gustaria tenerlo en el lenguaje
              |  App LamTerm LamTerm
              |  LIC String --imagen A partir de aca esta lo que agregue
              |  LBinOp Op LamTerm LamTerm
              |  LUnOp UOp LamTerm Float
              |  LComplement LamTerm
              deriving Show-}

data LamTerm  =  LVar String
              |  Abs String LamTerm --Tengo que sacar el Type de aca, va no me gustaria tenerlo en el lenguaje
              |  App LamTerm LamTerm
              |  LIC String --imagen A partir de aca esta lo que agregue
              |  LBinOp Op LamTerm LamTerm
              |  LUnOp UOp LamTerm Double
              |  LComplement LamTerm
              deriving Show

-- Modos de blending
data Op = Normal
        | Add
        | Diff
        | Darken
        | Lighten
        | Multiply
        | Screen
        | Overlay
        | HardLight
        | SoftLight
        | ColorDodge
        | ColorBurn
        | Hue
        | Luminosity
        | BlendColor
        | BlendSat
        | Exclusion
        deriving Show

--Funciones de tipo Image->Float->Image
data UOp = Temp --entre 1000 y 40000
         | Sat --entre 0 y 1
         | Vib
         | Exposure --entre -1 y 1
         | Contrast-- entre -1 y 1
         | Shadows
         | Highlights
         | Whites
         | Blacks
         | Opacity--entre -1 y 1

         deriving Show


-- Términos localmente sin nombres (aca te encuentra las variables)
{-data Term  = Bound Int
           | Free Name
           | Term :@: Term
           | Lam Term --No se pone la variable por que no es necesario, ya se guardo con las otras variables al realizar la conversion
           | IC String --imagen A partir de aca esta lo que agregue (sigue igual que antes)
           | BinOp (Double->Double->Double) Term Term
           | UnOp (Double->Double->Double) Term Double
           | Complement Term
           --deriving Show
-}

data Term  = Bound Int
           | Free Name
           | Term :@: Term
           | Lam Term --No se pone la variable por que no es necesario, ya se guardo con las otras variables al realizar la conversion
           | IC String --imagen A partir de aca esta lo que agregue (sigue igual que antes)
           | BinOp Op Term Term
           | UnOp UOp Term Double
           | Complement Term
           deriving Show



-- Valores
data Value = VLam Type Term
           | VUnit

-- Contextos del tipado
type Context = [Type]

--Hago una funcion map que no me mapea el canal alpha
rgbamap f (PixelRGBA r1 g1 b1 a1) = (PixelRGBA (f r1) (f g1) (f b1) a1)

--Para dos argumentos, mapeo y combino
blendpixel f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2)= (PixelRGBA ((f r1 r2)*a2+r1*a1*(1-a2)) ((f g1 g2)*a2+g1*a1*(1-a2)) ((f b1 b2)*a2+b1*a1*(1-a2)) (a2+a1*(1-a2)))

alphablend c1 cr a1 a2=(cr*a2+c1*a1*(1-a2))-- /(a2+a1*(1-a2))


--Funciones de blending que se aplican en cada canal uniformemente

normald a b = b

addd a b= a+b

--multiply::Double -> Double -> Double
multiplyd a b = a*b

--screen::Double -> Double -> Double
screend a b = a + b - (a*b)

--overlay::Double -> Double -> Double
overlayd a b = hardlightd b a

--darken::Double -> Double -> Double
darkend a b = min a b

--lighten::Double -> Double -> Double
lightend a b = max a b

--colordodge::Double -> Double -> Double
colordodged a b = if b==1 then 1 else min 1 (a/(1-b))

--colorburn::Double -> Double -> Double
colorburnd a b = if b==0 then 0 else 1 - min 1 ((1-a)/b)

hardlightd::Double -> Double -> Double
hardlightd a b = if (b <= 0.5) then multiplyd a 2*b else screend a 2*b-1

--softlight::Double -> Double -> Double
softlightd a b = if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x

--difference::Double -> Double -> Double
differenced a b = abs (a-b)

--exclusion::Double -> Double -> Double
exclusiond a b = a + b -2*a*b




--Voy a necesitar usar todo el Pixel
--Se puede escribir mas lindo si se tuviera funciones para acceder a las componentes
hue a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                              in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                  in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h2 s1 i1 a1)
                                                                      in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha


blendsat a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                              in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                  in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h1 s2 i1 a1)
                                                                      in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha





blendcolor a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                                    in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                        in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h2 s2 i1 a1)
                                                                            in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha


luminosity a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                                    in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                        in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h1 s2 i2 a1)
                                                                            in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha


--Funcion auxiliar
clamp x = clamp2 x 0 1
clamp2 x a b=if x<a then a else (if x>b then b else x)
lerp a b t= t*a + (1-t)*b
getLuminance (PixelRGBA r g b a) = r*0.212656+g*0.715158+b*0.072186
curve x = x^3*(x*(x*6-15)+10)

--temp d a= undefined

saturation d a= let PixelHSIA h1 s1 i1 a1=(toPixelHSIA a)--d de 0 a 1
                  in toPixelRGBA (PixelHSIA h1 (clamp s1*d) i1 a1)

vib d c@(PixelRGBA r g b a)= let (x,y)=(min (min r g) b,max (max r g) b)--No me convence
                             in let z= y-x
                                in rgbamap (\v -> lerp (getLuminance c) v (1+d*(1-z))) c


contrast d a= let factor=(1.0157 * (d + 1)) / (1 * (1.0157 - d)) --d entre -1 y 1
               in rgbamap (\v -> factor * (v  - 0.5) + 0.5) a



{-
weight_s  = curve( max( 1.0f - pLuma * 2.0f, 0.0f ));--sombras
weight_h  = curve( max(( pLuma - 0.5f ) * 2.0f, 0.0f ));--luces
weight_m  = saturate( 1.0f - weight_s - weight_h );--medio

color.xyz        = exposure( color.xyz, exposure_m * weight_m );
color.xyz        = con( color.xyz, contrast_m   * weight_m );
color.xyz        = bri( color.xyz, brightness_m * weight_m );--1 neutro
-}

shadows d a=undefined
highlights d c@(PixelRGBA r g b a)=rgbamap (\y -> if y>0.7 then y+d else y) c

bri d a=rgbamap (\y -> clamp(d*y)) a
--hace lo mismo (creo)
{-
highlights d a= let PixelHSIA h1 s1 i1 a1=(toPixelHSIA a)--d de 0 a 1
                  in toPixelRGBA (PixelHSIA h1 s1 (clamp i1*d) a1)
-}

blacks::Double->Pixel RGBA Double -> Pixel RGBA Double
blacks d c@(PixelRGBA r g b a)=let pLuma = getLuminance c--max (max r g) b--valor de luminosidad
                                in let weight= (curve (max (1-pLuma*2) 0))*d
                                    in (contrast weight (exposure weight c))--bri weight (contrast weight (exposure weight c))

--blacks d a=undefined
whites d c@(PixelRGBA r g b a)= if(r>0.9 && g>0.9 && b>0.9) then (contrast d (exposure d c)) else c


exposure d a=rgbamap (\y -> clamp(d+y)) a--d de -1 a 1
--buscar otra funcion
{-exposure d c = let x= if d<0 then d/3 else d
                in  rgbamap (\y ->clamp(y * (x*(1-y))+1)) c
-}

opacity d (PixelRGBA r g b a) = (PixelRGBA r g b (clamp (a+d)))--modifica la opasidad uniformemente (y la satura)

kelvinToRGBA::Double->Pixel RGBA Double
kelvinToRGBA k =let kelvin=(clamp2 k 1000 40000)/100
                in if (kelvin>66) then (PixelRGBA (clamp (1.29293618606274509804 * (kelvin - 60)**(-0.1332047592))) (clamp (1.12989086089529411765 * (kelvin - 60)**(-0.0755148492))) 1 1)
                                  else if (kelvin==66) then (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) 1 1)
                                                         else if (kelvin<19) then (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) 0 1)
                                                                               else (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) (clamp (0.54320678911019607843 * log( kelvin - 10) - 1.19625408914)) 1)

temp d c@(PixelRGBA r g b a) = let (PixelRGBA kr kg kb ka) = kelvinToRGBA d--entre 1000 y 40000
                                in let (PixelHSIA h s i la)=toPixelHSIA c
                                    in let (PixelRGBA br bg bb ba)=(PixelRGBA (r*kr) (g*kg) (b*kb) a)--blended
                                        in let (PixelHSIA hbr hbg hbb hba)=toPixelHSIA (PixelRGBA br bg bb ba)
                                            in (lerp (PixelRGBA br bg bb ba) (toPixelRGBA (PixelHSIA hbr hbg i hba)) 1)--se puede cambiar ese ultimo 1 por un



--Funciones de blending viejas (andan, pero esta desprolijo)

normal (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 r2 a1 a2) (alphablend g1 g2 a1 a2) (alphablend b1 b2 a1 a2) (a2+a1*(1-a2)))--chequear si el fmap me afecta el alpha

add (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (r1+r2) a1 a2) (alphablend g1 (g1+g2) a1 a2) (alphablend b1 (b1+b2) a1 a2) (a2+a1*(1-a2)))


multiply (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (r1*r2) a1 a2) (alphablend g1 (g1*g2) a1 a2) (alphablend b1 (b1*b2) a1 a2) (a2+a1*(1-a2)))


screen (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (r1 + r2 - (r1*r2)) a1 a2) (alphablend g1 (g1 + g2 - (g1*g2)) a1 a2) (alphablend b1 (b1 + b2 - (b1*b2)) a1 a2) (a2+a1*(1-a2)))


overlay a b = hardlight b a --testear esto con dos imagenes que tengan alpha


darken (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (min r1 r2) a1 a2) (alphablend g1 (min g1 g2) a1 a2) (alphablend b1 (min b1 b2) a1 a2) (a2+a1*(1-a2)))


lighten (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (max r1 r2) a1 a2) (alphablend g1 (max g1 g2) a1 a2) (alphablend b1 (max b1 b2) a1 a2) (a2+a1*(1-a2)))


colordodge (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) = (PixelRGBA (alphablend r1 (f r1 r2) a1 a2) (alphablend g1 (f g1 g2) a1 a2) (alphablend b1 (f b1 b2) a1 a2) (a2+a1*(1-a2)))
                                                              where f a b= if b==1 then 1 else min 1 (a/(1-b))


colorburn (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) = (PixelRGBA (alphablend r1 (f r1 r2) a1 a2) (alphablend g1 (f g1 g2) a1 a2) (alphablend b1 (f b1 b2) a1 a2) (a2+a1*(1-a2)))
                                                              where f a b= if b==0 then 0 else 1 - min 1 ((1-a)/b)


hardlight (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) = (PixelRGBA (alphablend r1 (f r1 r2) a1 a2) (alphablend g1 (f g1 g2) a1 a2) (alphablend b1 (f b1 b2) a1 a2) (a2+a1*(1-a2)))
                                                              where f a b= if (b <= 0.5) then a*2*b else a + 2*b-1 - (a*(2*b-1))


softlight (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) = (PixelRGBA (alphablend r1 (f r1 r2) a1 a2) (alphablend g1 (f g1 g2) a1 a2) (alphablend b1 (f b1 b2) a1 a2) (a2+a1*(1-a2)))
                                                              where f a b= if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                                                                           where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x


difference (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (abs (r1-r2)) a1 a2) (alphablend g1 (abs (g1-g2)) a1 a2) (alphablend b1 (abs (b1-b2)) a1 a2) (a2+a1*(1-a2)))


exclusion (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (r1 + r2 -2*r1*r2) a1 a2) (alphablend g1 (g1 + g2 -2*g1*g2) a1 a2) (alphablend b1 (b1 + b2 -2*b1*b2) a1 a2) (a2+a1*(1-a2)))
