{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables, FlexibleContexts #-}
module Repl where 

import Eval
import Parser
import Programs 

import Data.Char (isSpace)
import Data.List (genericLength, isPrefixOf)
import Data.Maybe (fromMaybe, maybe, fromJust, mapMaybe)
import qualified Data.Map as Map

import Text.Read (readMaybe)

import Data.Foldable (for_)
import Control.Monad.State
import Control.Arrow ((>>>))

import System.Console.Haskeline
import System.Console.Haskeline.Completion 

prompt = "Prec> "


repl :: [String] -> IO () 
repl input = evalEvalT $ runInputT settings do 
               for_ input \line -> 
                 when (not $ isComment line)
                  case parseString line of 
                    Left err -> outputStrLn $ show err 
                    Right (Just name, p) -> do 
                      context <- lift get 
                      if isWellTyped context p 
                        then lift $ addProgram name p 
                        else outputStrLn "Type error"

                    -- Ignore floating definitions 
                    Right _ -> return () 
               loop
  where settings :: Settings (EvalT IO)
        settings = setComplete completer (defaultSettings { historyFile = Just ".precHistory" })
                   
        completer = completeWord Nothing " \t" findCompletion


-- To implement TAB completion, we need to switch the order
-- of the monad transformer stack. Then we can use the example 
-- from Reddit to add it
findCompletion :: String -> (EvalT IO) [Completion]
findCompletion s = gets (Map.keys >>> 
  mapMaybe (\k -> if s `isPrefixOf` k 
                    then Just (simpleCompletion k) 
                    else Nothing))

loop :: InputT (EvalT IO) ()
loop = do 
  minput <- getInputLine prompt 
  case minput of 
    Nothing -> return () 
    Just "quit" -> return () 
    Just ":ls" -> do 
      context <- lift get 
      for_ (Map.toList context) \(name, p) -> do 
        -- If it's in the map, it had to typecheck initially 
        let t = fromJust $ findType context p 
        outputStrLn name
        printFunction t p 

        outputStrLn ""

      loop 

    Just input | isComment input -> loop 
               | otherwise -> processInput input 

processInput :: String -> InputT (EvalT IO) () 
processInput input = do 
  context <- lift get 
  case parseString input of 
    Left error -> do outputStrLn $ show error 
                     loop 
                     
    Right (Just name, program) -> 
      case findType context program of 
        Nothing -> do outputStrLn "Type error detected, or unknown function name"
                      loop 

        Just t -> do printFunction t program 
                     lift $ addProgram name program 
                     loop 
                                     
    Right (Nothing, program) -> promptNumbers program  

promptNumbers :: Program -> InputT (EvalT IO) () 
promptNumbers ast = do 
  context <- lift get 
  case findType context ast of 
    Nothing -> do outputStrLn "Type error detected, or unknown function name"
                  loop 

    Just t@(FunctionType (numIn, _)) -> do 
      printFunction t ast 

      minput <- getInputLine $ "Enter list of " ++ maybe "(any #)" show numIn ++ " numbers: "
      case minput of 
        Nothing -> loop 

        Just input | isComment input -> promptNumbers ast 
                   | otherwise -> do 
          case (readMaybe input :: Maybe [Integer]) of 
            Just xs -> 
              if genericLength xs == (fromMaybe (genericLength xs) numIn) 
                then outputStrLn . show $ eval context xs ast  
                else do outputStrLn "Wrong number of args"
                        promptNumbers ast 

            Nothing -> do outputStrLn "Numbers should be in Haskell list format"
                          outputStrLn $ "e.g. " ++ show [1..fromMaybe 3 numIn]
                          promptNumbers ast 

          loop 

printFunction t ast = do 
  outputStrLn $ show t 
  outputStrLn $ show ast 

isComment = \case 
  [] -> True 
  '#':_ -> True 
  x:xs | isSpace x -> isComment xs 
       | otherwise -> False 
