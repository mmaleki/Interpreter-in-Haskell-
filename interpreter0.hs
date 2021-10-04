import Data.Function
import Data.List

type Var = String
type Val = Float

data Exp
    = C Float
    | V Var
    | B Bool
    | Op Primop Exp Exp
    | Ap Exp Exp
    deriving (Show, Eq, Ord)

data Primop = Add | Sub | Mul | Div
  deriving (Eq, Ord, Show)


data Statement 
    = Var := Exp           --assignment 
    | While Exp Statement  --loop
    | Seq [Statement]      --sequence of statements
  deriving (Eq, Ord, Show)

type Env = [(Var, Val)]

{-desugar :: Exp -> Exp
desugar (Ap fun arg) = Ap (desugar fun) (desugar arg)
desugar (Op primop a b) = foldl Ap (V n) args
   where
       args = map desugar [a, b]
       n = case primop of
           Add -> "$ADD"
           Sub -> "$SUB"
           Mul -> "$Mul"
           Div -> "$EQL"
desugar e = e


ev :: Exp -> Env -> Val
ev e env = evCore (desugar e) env

evCore :: Exp -> Env -> Val
evCore (C n) env = n
evCore (V x) env = case lookup x env of
    Nothing -> error (x ++ "is unbound variable" )
    Just v -> v
-}


ev :: Exp -> Env -> Val
ev (C n) env = n
ev (V x) env = case lookup x env of
    Nothing -> error (x ++ "is unbound variable" )
    Just v -> v
ev (Op binary a b) env = case binary of
    Add -> (ev a env) + (ev b env)
    Sub -> (ev a env) - (ev b env)
    Mul -> (ev a env) * (ev b env)
    Div -> (ev a env) / (ev b env)


execut :: Statement -> Env -> Env
execut (x := e) env = (x, ev e env):env 
execut (While e s) env | ev e env /= 0 = execut (Seq [s, While e s]) env 
                       | otherwise     = env

execut (Seq []) env = env 
execut (Seq (s:ss)) env = execut (Seq ss) (execut s env) 
 

type Prog = Statement


run :: Prog -> Env -> Env
run p r = nubBy ((==) `on` fst) (execut p r)

-- Computing n-th Fibonaci number

fib :: Prog

fib = Seq [ "x" := C 0, "y" := C 1, While (V "n") (Seq ["z" := Op Add (V "x") (V "y"), "x" := V "y", "y" := V "z", "n" := Op Sub (V "n") (C 1)])]

--lookup "x" $ run fib [("n", 7)]

sm :: Prog

sm = Seq ["s" := C 0,
          "temp" := C 1,
           While (V "n") (Seq ["s" := Op Add (V "temp") (V "s"),
                               "temp" := Op Add (V "temp") (C 1),
                               "n" := Op Sub (V "n") (C 1)
                            ]
                        )
        ]

--lookup "s" $ run sum [("n",5)]
