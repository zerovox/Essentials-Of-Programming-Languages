module RecurisveData () where

	----Chapter 1
	---Page 2:
	inS :: Int -> Bool
	inS n   | n == 0 = True
	        | n-3 < 0 = False
	        | otherwise = inS (n-3)

	---Page 13
	listLength :: [a] -> Int
	listLength [] = 0
	listLength (x:xs) = 1 + listLength xs

	---Page 15:
	nthElement ::  [a] -> Int -> a
	nthElement [] _ = error "empty list"
	nthElement (x:xs) a     | a == 0 = x
	                        | otherwise = nthElement xs (a-1)

	---Page 17:
	removeFirst :: Eq a => a -> [a] -> [a]
	removeFirst _ []        = []
	removeFirst a (x:xs)    | a == x = xs
	                        | otherwise = x : removeFirst a xs

	---Page 19:
	type Identifier = String
	data LcExpr = Ident Identifier | Lambda Identifier LcExpr | App LcExpr LcExpr
	--TODO: is this correct?
	occursFree :: Identifier -> LcExpr -> Bool
	occursFree a (Ident b)      = a == b
	occursFree a (Lambda y e)   = (a /= y) && (occursFree a e)
	occursFree a (App e1 e2)   = (occursFree a e1) || (occursFree a e2)

	---Page 21:
	--The standard substitue for haskell lists looks like:
	subst' :: Eq a =>  a -> a -> [a] -> [a]
	subst' _ _ []  = []
	subst' a b (x:xs)   | a == x = b : (subst' a b xs)
	                    | otherwise = x : (subst' a b xs)
	--For sublists, we must define a type, then we can 'follow the grammar' by taking advantage of type pattern matching.
	type SList a = [SExp a]
	data SExp a = S a | Sublist (SList a)
	subst :: Eq a => a -> a -> SList a -> SList a
	subst _ _ []                = []
	subst a b ((Sublist x):xs)  = Sublist (subst a b x) : (subst a b xs)
	subst a b ((S x):xs)        | x == a    = S b : (subst a b xs)
	                            | otherwise = S x : (subst a b xs)

	---Page 23:
	numberElementsFrom :: Int -> [a] -> [(Int, a)]
	numberElementsFrom _ [] = []
	numberElementsFrom n (x : xs) = (n, x) : (numberElementsFrom (n+1) xs)

	numberElements :: [a] -> [(Int, a)]
	numberElements = numberElementsFrom 0

	---Page 24:
	listSum :: [Int] -> Int
	listSum [] = 0
	listSum (x:xs) = x + (listSum xs)

	--which is equivalent to
	listSum' = foldl (+) 0
	--or
	listSum'' = foldl1 (+)
	--or simply
	listSum''' = sum

	---Page 25:
	--Since haskell doesn't have vectors/arrays, I'll just pretend that a list is a vector and solve it in the same way as the book.
	partialVectorSum :: [Int] -> Int -> Int
	partialVectorSum xs 0 = nthElement xs 0
	partialVectorSum xs n = (nthElement xs n) + (partialVectorSum xs (n-1))

	vectorSum xs = partialVectorSum xs ((listLength xs) -1)

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