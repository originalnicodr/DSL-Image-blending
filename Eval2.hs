module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

import Graphics.Image as I

-- Estados
type Env = [(Variable,Image arr RGB Float)]--probablemente este masl escrito lo del arr

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado, con manejo de errores
newtype StateErrorCost a = StateErrorCost { runStateErrorCost :: Env -> Maybe (a, Env, (Image arr RGB Float)) }

-- Para calmar al GHC
instance Functor StateErrorCost where
    fmap = liftM

instance Applicative StateErrorCost where
    pure   = return
    (<*>)  = ap

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
   -- Busca el valor de una variable
   lookfor :: Variable -> m (Image arr RGB Float)
   -- Cambia el valor de una variable
   update :: Variable -> Int -> m ()

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
   -- Lanza un error
   throw :: m a

instance Monad StateError where
  return x = StateError (\s-> Just (x,s))
  StateError g >>= f = StateError (\s-> case (g s) of
                                          Nothing -> Nothing
                                          Just (a,s') -> runStateError (f a) s')

instance MonadError StateError where
  throw = StateError (\_ -> Nothing)

instance MonadState StateError where
  lookfor v = StateError (\s -> case lookfor' v s of
                                  Nothing -> Nothing
                                  Just n -> Just (n, s))
              where lookfor' v [] = Nothing
                    lookfor' v ((u, j):ss) | v == u = Just j
                                           | v /= u = lookfor' v ss
  update v i = StateError (\s -> Just ((), update' v i s))
               where update' v i [] = [(v, i)]
                     update' v i ((u, _):ss) | v == u = (v, i):ss
                     update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

eval :: Exp -> Maybe Env
eval p = case (runStateError (evalExp p) initState) of
           Nothing -> Nothing
           Just (a, s) -> Just s

evalExp :: (MonadState m, MonadError m) => Exp -> m ()
evalExp (IC dim) = do im <- readImageRGB VU dim
                   return im
evalExp (BinOp f e1 e2) = evalBinOp f e1 e2
evalExp (BoolOp f e1 e2) = evalBoolOp f e1 e2
evalExp (UnOp f e1 d) = evalUnOp f e1 d
evalExp (Complement e)= do e' <- evalExp e
                           return (opposite e')--es aplicar la funcion de doubles opposite a cada pixel de e'
evalExp ()
evalExp (Var v)= lookfor v

data Exp = Imagen
        | BinOp Op Exp Exp
        | BoolOp BOp Exp Exp
        | UnOp UOp Exp Float
        | Complement Exp
        | F Variable Exp
        | Exp :@: Exp
        | Var Variable --necesario ponerlo aca para la función
 deriving Show