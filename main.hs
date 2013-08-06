module Main where

import Control.Monad
import System.Environment
import LispParser
import LispEval

main :: IO ()
--main = getArgs >>= print. eval . readExpr . (!! 0)
main = do
	args <- getArgs
	print $ (eval . readExpr) (args !! 0)