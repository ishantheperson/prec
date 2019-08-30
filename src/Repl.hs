{-# LANGUAGE LambdaCase #-}
module Repl where 

import Eval
import Parser
import Programs 

import Data.Char (isSpace)
import Data.List (genericLength)
import Data.Maybe (fromMaybe, maybe)
import Text.Read (readMaybe)

import System.Console.Haskeline

prompt = "Prec> "

repl :: IO () 
repl = runInputT settings loop
  where settings = defaultSettings { historyFile = Just ".precHistory" }

loop :: InputT IO () 
loop = do 
  minput <- getInputLine prompt 
  case minput of 
    Nothing -> return () 
    Just "quit" -> return () 
    Just input | isComment input -> loop 
               | otherwise -> processInput input 

processInput :: String -> InputT IO () 
processInput input = do 
  let program = parseString input 
  case program of 
    Left error -> do outputStrLn $ show error 
                     loop 
    Right ast -> promptNumbers ast 

promptNumbers :: Program -> InputT IO () 
promptNumbers ast = 
  case findType ast of 
    Nothing -> outputStrLn "Type error detected"

    Just (numIn, numOut) -> do 
      let numText = maybe "(any #)" show numIn
      outputStrLn $ "f: N^" ++ numText ++ " -> N^" ++ show numOut 
      outputStrLn $ "f = " ++ show ast 

      minput <- getInputLine $ "Enter list of " ++ numText ++ " numbers: "
      case minput of 
        Nothing -> loop 

        Just input | isComment input -> promptNumbers ast 
                   | otherwise -> do 
          case (readMaybe input :: Maybe [Integer]) of 
            Just xs | genericLength xs == (fromMaybe (genericLength xs) numIn) -> 
              outputStrLn . show $ eval xs ast  
                    | otherwise -> do outputStrLn "Wrong number of args"
                                      promptNumbers ast 
            _ -> do outputStrLn "Numbers should be in Haskell list format"
                    outputStrLn $ "e.g. " ++ show [1..fromMaybe 3 numIn]
                    promptNumbers ast 

          loop 

isComment = \case 
  [] -> True 
  '#':_ -> True 
  x:xs | isSpace x -> isComment xs 
       | otherwise -> False 
