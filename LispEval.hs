module LispEval where

import LispVal
import LispParser
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)



eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
--eval (List (Atom func : args))  = return $ apply func $ mapM eval args
eval (List (Atom func : args))  = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                  ($ args) 
                  (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",          numericBinop (+)),
              ("-",          numericBinop (-)),
              ("*",          numericBinop (*)),
              ("/",          numericBinop div),
              ("mod",        numericBinop mod),
              ("quotient",   numericBinop quot),
              ("remainder",  numericBinop rem),
              ("=",          numBoolBinOp (==)),
              ("<",          numBoolBinOp (<)),
              (">",          numBoolBinOp (>)),
              (">=",         numBoolBinOp (>=)),
              ("<=",         numBoolBinOp (<=)),
              ("&&",         boolBoolBinOp (&&)),
              ("||",         boolBoolBinOp (||)),
              ("string=?",   strBoolBinOp (==)),
              ("string?",    strBoolBinOp (>)),
              ("string<=?",  strBoolBinOp (<=)),
              ("string>=?",  strBoolBinOp (>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumberArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
						if null parsed
							then throwError $ TypeMismatch "Number " $ String n
							else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNumber  = throwError $ TypeMismatch "Number " notNumber

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr badArg     = throwError $ TypeMismatch "String" badArg

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool badArg   = throwError $ TypeMismatch "Boolean" badArg

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2 
                             then throwError $ NumberArgs 2 args 
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


numBoolBinOp  = boolBinOp unpackNum
strBoolBinOp  = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool









