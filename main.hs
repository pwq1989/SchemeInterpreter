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
	args <- getArgs
	evaled <- return $ liftM show $ readExpr(args !! 0) >>= eval
	putStrLn $ extractValue $ trapError evaled