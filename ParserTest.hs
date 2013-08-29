module Main where

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple = letter

parens :: Parser ()
parens = do 
		char '(' 
		parens 
		char ')'
		parens 
		


parserTest :: Show a => Parser a -> String -> IO ()
parserTest p input 
			= case parse p "" input of 
				Left err -> do
						str <- putStr $ "Parse error at " ++ show err
						print str
				Right x  -> print x


main :: IO ()
main = parserTest parens "()()"



