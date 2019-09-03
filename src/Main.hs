module Main where 

import Repl 

import System.Environment (getArgs)

main :: IO () 
main = do 
  args <- getArgs 
  text <- concat <$> mapM readFile args 

  repl (lines text)
