module LispVal where 

import Text.ParserCombinators.Parsec
import Control.Monad.Error

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


data LispError = NumberArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSepcialForm String LispVal
               | NotFunction String String
               | UnboundVal String String
               | Default String 

showError :: LispError -> String
showError (UnboundVal message varname)      = message ++ ": " ++ varname
showError (BadSepcialForm message form)     = message ++ ": " ++ show form
showError (NotFunction message func)        = message ++ ": " ++ show func
showError (NumberArgs expected found)       = "Expected " ++ show expected ++
                                              " args; found values " ++ unwordsList found
showError (TypeMismatch expected found)     = "Invalid type: expected " ++ expected ++ 
                                              ", found " ++ show found
showError (Parser parseErr)         		= "Parse error at " ++ show parseErr


instance Show LispError where
	show = showError

type ThrowsError = Either LispError

instance Error LispError where
	noMsg  = Default "An error has occurred" 
	strMsg = Default

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = catchError action (return . show)


