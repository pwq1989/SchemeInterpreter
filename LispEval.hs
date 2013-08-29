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

eval (List [Atom "if", precidate, consequent, alternative]) = 
    do result <- eval precidate
       case result of
           Bool False -> eval alternative
           otherwise  -> eval consequent


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


car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [x]                   = throwError $ TypeMismatch "Pair" x
car badArgsList           = throwError $ NumberArgs 1 badArgsList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [x]                   = throwError $ TypeMismatch "Pair" x
cdr badArgsList           = throwError $ NumberArgs 1 badArgsList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]         = return $ List [x]
cons [x, List xs]         = return $ List (x:xs)
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y]               = return $ DottedList [x] y
cons badArgList           = throwError $ NumberArgs 2 badArgList


-- TODO: figure out equality between numerical types
-- TODO: make eq, eqv, equal truly schemish instead of as in tutorial
-- TODO: update with new LispVal constructors
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(List arg1), (List arg2)] = return . Bool $ (length arg1 == length arg2) 
                                 && (all eqvPair $ zip arg1 arg2) 
                                     where eqvPair (x1, x2) = case eqv [x1, x2] of 
                                            Left err         -> False
                                            Right (Bool val) -> val 

eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(Vector (length1, arg1)), (Vector (length2, arg2))] = if length1 == length2
                                                 then eqv [List arg1, List arg2]
                                                 else return $ Bool False   
eqv [(Number arg1), (Number arg2)]         = return . Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return . Bool $ arg1 == arg2
eqv [(Bool arg1), (Bool arg2)]             = return . Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return . Bool $ arg1 == arg2
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumberArgs 2 badArgList
