{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
module Repl where 

import Eval
import Parser
import Programs 

import Data.Char (isSpace)
import Data.List (genericLength)
import Data.Maybe (fromMaybe, maybe, fromJust)
import qualified Data.Map as Map

import Text.Read (readMaybe)

import Data.Foldable (for_)
import Control.Monad.State

import System.Console.Haskeline

prompt = "Prec> "

repl :: [String] -> IO () 
repl input = runInputT settings $ evalEvalT do 
               for_ input \line -> 
                 when (not $ isComment line) 
                  case parseString line of 
                    Left err -> lift $ outputStrLn $ show err 

                    Right (Just name, p) -> do 
                      context <- get 
                      case findType context p of 
                        Just _ -> addProgram name p 
                        _ -> lift $ outputStrLn "Type error"
                        
                    Right _ -> return () 

               loop
  where settings = defaultSettings { historyFile = Just ".precHistory" }

loop :: EvalT (InputT IO) ()
loop = do 
  minput <- lift $ getInputLine prompt 
  case minput of 
    Nothing -> return () 
    Just "quit" -> return () 
    Just ":ls" -> do 
      context <- get 
      for_ (Map.toList context) \(name, p) -> do 
        -- If it's in the map, it had to typecheck initially 
        let t = fromJust $ findType context p 
        lift $ outputStrLn name
        printFunction t p 

        lift $ outputStrLn ""

      loop 

    Just input | isComment input -> loop 
               | otherwise -> processInput input 

processInput :: String -> EvalT (InputT IO) () 
processInput input = do 
  context <- get 
  case parseString input of 
    Left error -> do lift $ outputStrLn $ show error 
                     loop 
                     
    Right (Just name, program) -> 
      case findType context program of 
        Nothing -> do lift $ outputStrLn "Type error detected, or unknown function name"
                      loop 

        Just t -> do printFunction t program 
                     addProgram name program 
                     loop 
                                     
    Right (Nothing, program) -> promptNumbers program  

promptNumbers :: Program -> EvalT (InputT IO) () 
promptNumbers ast = do 
  context <- get 
  case findType context ast of 
    Nothing -> lift $ outputStrLn "Type error detected, or unknown function name"

    Just t@(numIn, _) -> do 
      printFunction t ast 

      minput <- lift $ getInputLine $ "Enter list of " ++ maybe "(any #)" show numIn ++ " numbers: "
      case minput of 
        Nothing -> loop 

        Just input | isComment input -> promptNumbers ast 
                   | otherwise -> do 
          case (readMaybe input :: Maybe [Integer]) of 
            Just xs -> 
              if genericLength xs == (fromMaybe (genericLength xs) numIn) 
                then lift . outputStrLn . show $ eval context xs ast  
                else do lift $ outputStrLn "Wrong number of args"
                        promptNumbers ast 

            Nothing -> do lift $ outputStrLn "Numbers should be in Haskell list format"
                          lift $ outputStrLn $ "e.g. " ++ show [1..fromMaybe 3 numIn]
                          promptNumbers ast 

          loop 

printFunction (numIn, numOut) ast = do 
  let numText = maybe "(any #)" show numIn
  lift $ outputStrLn $ "N^" ++ numText ++ " -> N^" ++ show numOut 
  lift $ outputStrLn $ show ast 

isComment = \case 
  [] -> True 
  '#':_ -> True 
  x:xs | isSpace x -> isComment xs 
       | otherwise -> False 
