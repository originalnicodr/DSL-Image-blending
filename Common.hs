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
        | Exclusion
        deriving Show

--Funciones de tipo Image->Float->Image
data UOp = Temp
         | Sat
         | Vib
         | Multi--sacar
         | Power--sacar
         | Contrast
         | Shadows
         | Lights
         | Whites
         | Blacks

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

--normal (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 r2 a1 a2) (alphablend g1 g2 a1 a2) (alphablend b1 b2 a1 a2) (a2+a1*(1-a2)))--chequear si el fmap me afecta el alpha

normal a b= blendpixel normald a b

add (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2)= (PixelRGBA (alphablend r1 (r1+r2) a1 a2) (alphablend g1 (g1+g2) a1 a2) (alphablend b1 (b1+b2) a1 a2) (a2+a1*(1-a2)))

hue a b = b
luminosity a b= b


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




temp a d= a


saturation a d= a


multi a d= a


power a d= a
--pixel -> double -> pixel
{-exposure c d = let x= if d<0 then d/3 else d
                                  in  saturate(c * (x*(1-c))+1)--Esto esta mal, hay que hacer un fmap que solo afecte a los colores
-}

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

hued= undefined
luminosityd= undefined

--opposited im = I.map (fmap opp) im
--              where opp x= 1-x
