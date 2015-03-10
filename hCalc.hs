{-
Copyright (C) 2015 Tim Schonscheck

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

import Data.Char
import Data.Maybe

-- checks, whether a string represents a number
isNumeric :: String -> Bool
isNumeric "" = True
isNumeric (x:xs)
	| isDigit x || x == '.' = isNumeric xs
	| otherwise 			= False

-- checks, whether a string represents a operator
isOperator :: String -> Bool
isOperator [s] = s == '+' || s == '-' || s == '*' || s == '/'
isOperator _   = False

-- removes all spaces in a string
removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

-- converts a string into a list of strings, splitted by numbers and operators
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

-- transforms a algebraic term from infix to postfix
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

			-- pops all elements of a stack until a bracket "(" is found
			getTerm :: [String] -> Maybe ([String], [String])
			getTerm [] = Nothing -- Fehlerfall
			getTerm (t:ts)
				| isOperator t = case getTerm ts of
					Nothing 	-> Nothing
					Just (a, b) -> Just (t:a, b)
				| t == "("     = Just ([], ts)

-- evaluates a postfix term with help of a stack
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
			"-" -> calculate' xs $ (compute ys y (-)) : yss
			"*" -> calculate' xs $ (compute y ys (*)) : yss
			"/" -> calculate' xs $ (compute ys y (/)) : yss
		| otherwise	= "error while calculating"
	calculate' (x:xs) stack
		| isNumeric x = calculate' xs $ x : stack 

-- applies a mathematic function on two strings
compute :: String -> String -> (Double -> Double -> Double) -> String
compute s1 s2 f = show $ f (read s1) (read s2)

-- just a test function because IO is not handled yet
-- e.g.: test "(1+2)*3" ~> "9"
execute :: String -> String
execute s = let result = (convert . toList . removeSpaces) s
		 	in case (elem "error" result) of
		 		True  -> last result
		 		False -> calculate result

main :: IO ()
main = putStrLn "Enter a term:" >> getLine >>= \term -> putStrLn $ execute term