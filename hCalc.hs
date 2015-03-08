
import Data.Char
import Data.Maybe

isNumeric :: String -> Bool
isNumeric "" = True
isNumeric (x:xs)
	| (ord x >= 48) && (ord x <= 57) = isNumeric xs
	| otherwise 					 = False

isOperator :: String -> Bool
isOperator [s] = s == '+' || s == '-' || s == '*' || s == '/'
isOperator _   = False

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

toList :: String -> [String]
toList "" = []
toList (x:xs)
	| isNumeric [x] = let (a, b) = getNumber $ x:xs
					  in a : toList b
	| isOperator [x] || x == '(' || x == ')' = [x] : toList xs where

		getNumber :: String -> (String, String)
		getNumber "" = ("", "")
		getNumber (x:xs)
			| isDigit x = let (a, b) = getNumber xs
						  in (x:a, b)
			| otherwise = ("", x:xs)

convert :: [String] -> [String]
convert s = convert' s [] where

	convert' :: [String] -> [String] -> [String]
	convert' [] stack
		| elem "(" stack = "error" : ["closing bracket not found"] -- Fehlerfall
		| otherwise		 = stack
	convert' (x:xs) []
		| isNumeric x  = x : convert' xs []
		| isOperator x || x == "(" = convert' xs [x]
		| x == ")" = "error" : ["invalid brackets"] -- Fehlerfall
	convert' (x:xs) (y:ys)
		| isNumeric x = x : (convert' xs $ y:ys)
		| x == "(" 	  = convert' xs $ x:y:ys
		| x == ")"	  = case getTerm $ y:ys of
			Nothing 	  -> "error" : ["invalid brackets"] -- Fehlerfall
			Just (x1, x2) -> x1 ++ convert' xs x2
		| (isOperator x) && (x == "*" || x == "/") && (y == "+" || y == "-" || y == "(" || y == ")") = convert' xs $ x:y:ys
		| isOperator x = case y of
			"(" -> convert' xs $ x:y:ys
			_   -> y : (convert' xs $ x:ys) 
		where

			getTerm :: [String] -> Maybe ([String], [String])
			getTerm [] = Nothing -- Fehlerfall
			getTerm (t:ts)
				| isOperator t = case getTerm ts of
					Nothing 	-> Nothing
					Just (a, b) -> Just (t:a, b)
				| t == "("     = Just ([], ts)

calculate :: [String] -> String
calculate s = calculate' s [] where

	calculate' :: [String] -> [String] -> String
	calculate' [] stack
		| length stack == 1 = head stack
		| otherwise			= "error while calculating"
	calculate' (x:xs) stack@(y:ys:yss)
		| isNumeric x = calculate' xs $ x : stack
		| (isOperator x) && (isNumeric y) && (isNumeric ys) = case x of
			"+" -> calculate' xs $ (compute y ys (+)) : yss
			"-" -> calculate' xs $ (compute y ys (-)) : yss
			"*" -> calculate' xs $ (compute y ys (*)) : yss
			"/" -> calculate' xs $ (compute y ys (/)) : yss
	calculate' (x:xs) stack
		| isNumeric x = calculate' xs $ x : stack 

compute :: String -> String -> (Double -> Double -> Double) -> String
compute s1 s2 f = show $ f (read s1) (read s2) :: String

test :: String -> Double
test s = let result = (convert . toList . removeSpaces) s
		 in case (elem "error" result) of
		 	True  -> -1
		 	False -> read $ calculate result