module Main where

import Control.Monad
import System.Environment
import LispVal
import LispParser
import LispEval
import Debug.Trace

main :: IO ()
--main = getArgs >>= print. eval . readExpr . (!! 0)
main = do
	--args <- getArgs
	--evaled <- return $ liftM show $ readExpr(args !! 0) >>= eval
	--putStrLn $ extractValue $ trapError evaled

	--print $ show  (args !! 0)
	--print $ liftM show $ readExpr (args !! 0)

	args <- getArgs
	case length args of
		0 -> runRepl
		1 -> evalAndPrint $ args !! 0
		otherwise -> putStrLn "too many arguments"