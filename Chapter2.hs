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
data Env = Empty | ExtendedEnv Identifier Value Env
    deriving (Read, Show, Eq, Ord)
type Identifier = String
data Value = Ident Identifier | IntVal Integer | BoolVal Bool | CharVal Char | List [Value] | Function [Value] Value
    deriving (Read, Show, Eq, Ord)

emptyEnv                = Empty
extendEnv               = ExtendedEnv
applyEnv Empty ident    = error ("No binding for " ++ ident ++ " found")
applyEnv (ExtendedEnv a b env) ident
                        | a == ident = b
                        | otherwise = applyEnv env ident