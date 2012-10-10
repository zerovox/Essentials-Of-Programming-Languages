----Chapter2.hs
---Page 33
--Unary Representation
zero                = []
isZero []           = True
isZero _            = False
successor xs        = True : xs
predecessor (x:xs)  = xs

--Interal Number Representation
zero'           = 0
isZero' 0       = True
isZero' _       = False
successor' x    = x + 1
predecessor' x  = x - 1

---Page 38
data Env                   = Empty | ExtendedEnv Identifier Value Env
    deriving (Read, Show, Eq, Ord)
type Identifier         = String
data Value              = Ident Identifier | IntVal Integer | BoolVal Bool | CharVal Char | List [Value] | Function [Value] Value
    deriving (Read, Show, Eq, Ord)

emptyEnv                = Empty
extendEnv               = ExtendedEnv
applyEnv Empty ident    = error ("No binding for " ++ ident ++ " found")
applyEnv (ExtendedEnv a b env) ident
                        | a == ident = b
                        | otherwise = applyEnv env ident

---Page 40
type Env'               = Identifier -> Value
--By representing these two as lambdas, it's easy that they have a return type of Env', that is, they each specify a function that given an Identifier either returns a value, or throws an error.
emptyEnv'               = \x -> error ("No binding for " ++ x ++ " found")
extendEnv' i v env      = \x -> if x==i then v else (env x)
applyEnv' env ident     = env ident