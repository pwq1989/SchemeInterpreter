module LispParser where

import LispVal
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of 
    Left  err -> String $ "No match: " ++ show err
    Right val -> val



parseString :: Parser LispVal
parseString = do 
	char '"'
	x <- many (noneOf "\"") 
	char '"' 
	return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
	first <- letter <|> symbol 
	rest <- many (letter <|> digit <|> symbol) 
	let atom = first : rest 
	return $ case atom of 
		"#t" -> Bool True 
		"#f" -> Bool False 
		otherwise ->Atom atom


parseNumber :: Parser LispVal
parseNumber = do 
	num <- many digit 
	return $ (Number . read) num
              




spaces1 :: Parser ()
spaces1 = skipMany1 space

parseList :: Parser LispVal
parseList = do 
	char '(' 
	x <- fmap List $ sepBy parseExpr spaces1 
	char ')' 
	return x


parseDottedList :: Parser LispVal
parseDottedList = do 
	char '(' 
	x <- endBy parseExpr spaces1
	char '.' 
	spaces1 
	xs <- parseExpr 
	char ')'
	return $ DottedList x xs

parseQuote :: Parser LispVal
parseQuote = do 
	char '\'' 
	x <- parseExpr 
	return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do 
	char '`' 
	x <- parseExpr 
	return $ List [Atom "quasiquote", x]


parseUnquote :: Parser LispVal
parseUnquote = do 
	char ',' 
	x <- parseExpr 
	return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do 
	string "#(" 
	x <- sepBy parseExpr spaces1 
	char ')' 
	return $ Vector (length x, x)



            
parseExpr :: Parser LispVal
parseExpr = try parseAtom
        <|> try parseString
        <|> try parseNumber
        <|> try parseQuote
        <|> try parseQuasiquote
        <|> try parseUnquote
        <|> try parseList
        <|> try parseDottedList


