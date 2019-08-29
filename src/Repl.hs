module Repl where 

import Parser
import Eval

import Data.List (genericLength)
import Data.Maybe (fromMaybe, maybe)
import Text.Read (readMaybe)

import System.Console.Haskeline

prompt = "Prec> "

repl :: IO () 
repl = runInputT defaultSettings loop

loop :: InputT IO () 
loop = do 
  minput <- getInputLine prompt 
  case minput of 
    Nothing -> return () 
    Just "quit" -> return () 
    Just input -> processInput input 

processInput :: String -> InputT IO () 
processInput input = do 
  let program = parseString input 
  case program of 
    Left error -> outputStrLn $ show error 
    Right ast -> promptNumbers ast 

promptNumbers :: Program -> InputT IO () 
promptNumbers ast = 
  case findType ast of 
    Nothing -> outputStrLn "Type error detected"
    Just (numIn, _) -> do 
      let numText = maybe "(any #)" show numIn
      minput <- getInputLine $ "Enter list of " ++ numText ++ " numbers: "
      case minput of 
        Nothing -> loop 
        Just input -> do 
          case (readMaybe input :: Maybe [Integer]) of 
            Just xs | genericLength xs == (fromMaybe (genericLength xs) numIn) -> 
              outputStrLn . show $ eval xs ast  
            _ -> promptNumbers ast 

          loop 