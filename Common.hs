module Common where

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
         | Multi
         | Power
         deriving Show


-- Términos localmente sin nombres (aca te encuentra las variables)
data Term  = Bound Int
           | Free Name
           | Term :@: Term
           | Lam Term --No se pone la variable por que no es necesario, ya se guardo con las otras variables al realizar la conversion
           | IC String --imagen A partir de aca esta lo que agregue (sigue igual que antes)
           | BinOp (Double->Double->Double) Term Term
           | UnOp (Double->Double->Double) Term Double
           | Complement Term
           --deriving Show

-- Valores
data Value = VLam Type Term
           | VUnit

-- Contextos del tipado
type Context = [Type]





--normal::Double -> Double -> Double
normal a b = b

add a b= (a + b)/2

hue a b = b
luminosity a b= b

--multiply::Double -> Double -> Double
multiply a b = a*b

--screen::Double -> Double -> Double
screen a b = a + b - (a*b)

--overlay::Double -> Double -> Double
overlay a b = hardlight b a

--darken::Double -> Double -> Double
darken a b = min a b

--lighten::Double -> Double -> Double
lighten a b = max a b

--colordodge::Double -> Double -> Double
colordodge a b = if b==1 then 1 else min 1 (a/(1-b))

--colorburn::Double -> Double -> Double
colorburn a b = if b==0 then 0 else 1 - min 1 ((1-a)/b)

hardlight::Double -> Double -> Double
hardlight a b = if (b <= 0.5) then multiply a 2*b else screen a 2*b-1

--softlight::Double -> Double -> Double
softlight a b = if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x

--difference::Double -> Double -> Double
difference a b = abs (a-b)

--exclusion::Double -> Double -> Double
exclusion a b = a + b -2*a*b



temp::Double -> Double -> Double
temp a d= a

saturation::Double -> Double -> Double
saturation a d= a

multi::Double -> Double -> Double
multi a d= a

power::Double -> Double -> Double
power a d= a
