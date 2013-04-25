module Environment (emptyEnv, extendEnv, applyEnv) where
	---2.1
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

	---2.2.2
	data Env                = Empty | ExtendedEnv Identifier Value Env
	    deriving (Read, Show, Eq, Ord)
	type Identifier         = String
	data Value              = Ident Identifier | IntVal Integer | BoolVal Bool | CharVal Char | List [Value] | Function [Value] Value
	    deriving (Read, Show, Eq, Ord)

	emptyEnv'                = Empty
	extendEnv'               = ExtendedEnv
	applyEnv' Empty ident    = error ("No binding for " ++ ident ++ " found")
	applyEnv' (ExtendedEnv a b env) ident
	                        | a == ident = b
	                        | otherwise = applyEnv' env ident

	---2.2.3
	emptyEnv :: (Eq a, Show a) => a -> b
	emptyEnv               = \x -> error ("No binding for " ++ show x ++ " found")
	extendEnv :: (Eq a, Show a) => a -> b -> (a -> b) -> (a -> b)
	extendEnv i v env      = \x -> if x==i then v else (env x)
	applyEnv :: (Eq a, Show a) => (a -> b) -> a -> b
	applyEnv env ident     = env ident

	---2.3 - 2.5
	--We already know about datatypes and parsing lambda expressions, see my Lambda project for more :)
