module LispVal where 

import Text.ParserCombinators.Parsec

data LispVal = Atom String
			 | List [LispVal]
			 | Vector (Int, [LispVal])
			 | DottedList [LispVal] LispVal
			 | Number Integer
			 | String String
			 | Bool Bool

instance Show LispVal where
	show = showVal 

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List xs)         = "(" ++ unwordsList xs ++ ")"
showVal (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ showVal xs ++ ")"





unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

