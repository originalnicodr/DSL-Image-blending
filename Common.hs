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

-- Términos con nombres (modificado con mi lenguaje)
data LamTerm  =  LVar String
              |  Abs String LamTerm --Tengo que sacar el Type de aca, va no me gustaria tenerlo en el lenguaje
              |  App LamTerm LamTerm
              |  LIC String --imagen A partir de aca esta lo que agregue
              |  LBinOp Op LamTerm LamTerm
              |  LBoolOp BOp LamTerm LamTerm
              |  LUnOp UOp LamTerm Float
              |  LComplement LamTerm
deriving (Show, Eq)

-- Modos de blending
data Op = Normal
        | Add
        | Sub
        | Diff
        | Div
        | Mult
        | Darken
        | Lighten
        | Multiply
        | Screen
        | Overlay
        | HardLight
        | SoftLight
        | Hue
        | Luminosity
        | Exclusion
deriving Show

--Funciones booleanas
data BOp = And
    | Or
    | Xor
deriving Show

--Funciones de tipo Image->Float->Image
data UOp = Temp
    | Sat
    | Mult
    | Power
deriving Show


-- Términos localmente sin nombres (aca te encuentra las variables)
data Term  = Bound Int
           | Free Name 
           | Term :@: Term
           | Lam Type Term --Tengo que sacar el Type de aca, va no me gustaria tenerlo en el lenguaje
           | IC String --imagen A partir de aca esta lo que agregue (sigue igual que antes)
           | BinOp Op Term Term
           | BoolOp BOp Term Term
           | UnOp UOp Term Float
           | Complement Term
deriving (Show, Eq)

-- Valores
data Value = VLam Type Term 
           | VUnit 

-- Contextos del tipado
type Context = [Type]