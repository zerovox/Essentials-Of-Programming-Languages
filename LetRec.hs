module LetRec (Program (..), Exp (..), Proc (..), Value (..), evalProgram) where
	import Chapter2
	---3.2 - 3.4
	data Program a 	= Program (Exp a)
	data Exp a		= Const (Value a)			-- From LET
					| Diff (Exp a) (Exp a)
					| IsZero (Exp a)
					| If (Exp a) (Exp a) (Exp a)
					| Var a
					| Let a (Exp a) (Exp a)
					| Proc (Proc a)				-- From PROC
					| App (Exp a) (Exp a)
					| LetRec a (Exp a) (Exp a)	-- From LETREC

	data Proc a		= P a (Exp a)

	data Value a	= Int Int
					| Bool Bool
					| Procedure (Proc a)
		
	numVal (Int i) 				= i
	numVal _ 					= error "Not a number"
	boolVal (Bool b) 			= b
	boolVal _ 					= error "Not a boolean"
	procVal (Procedure a)		= a
	procVal _ 					= error "Not a procedure"

	eval :: (Show a, Eq a) => Exp a -> (a -> Value a) -> Value a
	eval (Const v) _ 						= v
	eval (Diff a b) env 					= Int ((numVal $ eval a env) - (numVal $ eval b env))
	eval (IsZero a) env 					= Bool ((numVal $ eval a env) == 0)
	eval (If tst thn els) env 				= if (boolVal $ eval tst env) then eval thn env else eval els env
	eval (Var v) env						= applyEnv env v
	eval (Let a exp1 exp2) env				= eval exp2 (extendEnv a (eval exp1 env) env)
	eval (App a b) env 						= eval body (extendEnv var (eval b env) env)
		where (P var body) = procVal $ eval a env
	eval (Proc a) env 						= Procedure a
	eval (LetRec a exp1 exp2) env 			= eval exp2 env'
		where env' = (extendEnv a (eval exp1 env') env)

	evalProgram :: (Show a, Eq a) => Program a -> Value a
	evalProgram (Program a)		= eval a emptyEnv