import Data.List
import System.IO
import Data.Function

type Var = String


data Exp
    = C Float
    | V Var
    | Lam Var Exp
    | Ap Exp Exp
    | IF Exp Exp Exp
    | Op Primop Exp Exp
    | Let Var Exp Exp
    deriving (Show, Eq, Ord)

data Primop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)


desugar :: Exp -> Exp
desugar (Ap fun arg) = Ap (desugar fun) (desugar arg)
desugar (Lam x body) = Lam x (desugar body)
desugar (Let x e body) = Ap (Lam x (desugar body)) (desugar e)
desugar (IF guard a b) = foldl Ap (V "$IF") args
   where args =map desugar [guard, a, b]
desugar (Op primop a b) = foldl Ap (V n) args
   where
       args = map desugar [a, b]
       n = case primop of
           Add -> "$ADD"
           Sub -> "$SUB"
           Mul -> "$Mul"
           Eql -> "$EQL"
desugar e = e


data Closure = Closure Var Exp Env
  deriving Show

data Val = Number Float | Cl Closure
  deriving Show

type Env =[(Var, Val)]

ev :: Exp -> Env -> Val
ev e env = evCore (desugar e) env

evCore ::Exp -> Env -> Val
evCore (C n) env = Number n
evCore (V x) env = case lookup x env of
    Nothing -> error ("Unbound variable")
    Just v -> v
evCore (Lam x body) env = Cl (Closure x body env) 
evCore 