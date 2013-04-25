module ExplicitRefs (Program (..), Exp (..), Proc (..), Value (..), evalProgram) where
	import Environment
	---4.2
	data Program a 	= Program (Exp a)
		deriving (Show)
	data Exp a		= Const (Value a)			-- From LET
					| Diff (Exp a) (Exp a)
					| IsZero (Exp a)
					| If (Exp a) (Exp a) (Exp a)
					| Var a
					| Let a (Exp a) (Exp a)
					| Proc (Proc a)				-- From PROC
					| App (Exp a) (Exp a)
					| LetRec a (Exp a) (Exp a)	-- From LETREC
					| NewRef 					-- From EXPLICIT-REFS
					| DeRef (Exp a)
					| SetRef (Exp a) (Exp a)
		deriving (Show)

	data Proc a		= P a (Exp a)
		deriving (Show)

	data Value a	= Int Int
					| Bool Bool
					| Procedure (Proc a)
					| Ref Ref
					| Null
		deriving (Show)

	type Ref 			= Int
		
	numVal (Int i) 				= i
	numVal _ 					= error "Not a number"
	boolVal (Bool b) 			= b
	boolVal _ 					= error "Not a boolean"
	procVal (Procedure a)		= a
	procVal _ 					= error "Not a procedure"
	refVal (Ref i)				= i
	refVal _ 					= error "Not a reference"

	-- Our Data Store
	type Store a		= [Value a]
	emptyStore 			= []
	newRef [] 			= (0, [Null])
	newRef (x:xs) 		= (1+n, x:ys) 
		where (n, ys) 	= newRef xs
	deRef store addr 	= store !! addr
	setRef [] _ _		= error "Invalid Addr"
	setRef (x:xs) 0 val	= (val:xs)
	setRef (x:xs) a val = (x:setRef xs (a-1) val)

	eval :: (Show a, Eq a) => Exp a -> (a -> Value a) -> Store a -> (Value a, Store a)
	eval (Const v) _ store					= (v, store)
	eval (Diff a b) env store				= (Int ((numVal v1) - (numVal v2)), store'')
		where 
			(v1, store') 	= eval a env store
			(v2, store'') 	= eval b env store'
	eval (IsZero a) env store				= (Bool ((numVal v) == 0), store')
		where (v, store') 	= eval a env store
	eval (If tst thn els) env store			= if (boolVal v) then eval thn env store' else eval els env store'
		where (v, store') 	= eval tst env store
	eval (Var v) env store					= (applyEnv env v, store)
	eval (Let a exp1 exp2) env store		= eval exp2 (extendEnv a val env) store'
		where (val, store') = eval exp1 env store
	eval (App a b) env store				= eval body (extendEnv var val env) store''
		where 
			(P var body) 	= procVal p
			(p, store'') 	= eval a env store'
			(val, store') 	= eval b env store
	eval (Proc a) env store					= (Procedure a, store)
	eval (LetRec a exp1 exp2) env store		= eval exp2 env' store'
		where 
			env' 			= (extendEnv a v env)
			(v, store') 	= (eval exp1 env' store)
	eval (NewRef) _ store 					= (Ref v, store')
		where (v, store') 	= newRef store
	eval (DeRef exp) env store 				= (deRef store' addr, store')
		where ((Ref addr), store') = eval exp env store
	eval (SetRef exp1 exp2) env store		= (v, store'')
		where 
			(v, store') = eval exp1 env store
			addr = refVal v
			store'' = setRef store' addr v
	-- We deviate from the specification here, instead of returning 23, we return the newly assigned value.
	-- This allows statments such similar to a[++x] (or without syntactic sugar a[(x = x+1)])

	evalProgram :: (Show a, Eq a) => Program a -> Value a
	evalProgram (Program a)	 = value
		where (value, store) = eval a emptyEnv emptyStore