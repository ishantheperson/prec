{-# LANGUAGE LambdaCase, NPlusKPatterns #-}
module Eval where 

import Parser 

import Data.List (genericLength, genericIndex)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

-- | A function's type is either 
--   (Just a, b) --> N^a -> N^b
--   (Nothing, b) -> anything -> N^b  
type FunctionType = (Maybe Integer, Integer) 

eval :: [Integer] -> Program -> [Integer]
eval input = \case 
  Nat i -> [i] 
  Succ -> [head input + 1]
  Proj index _ -> [input `genericIndex` (index - 1)]
  Compose f g -> eval (eval input g) f 
  Tuple fs -> concatMap (eval input) fs 
  Prec h g -> case input of 
                0:xs -> eval xs g 
                (x+1):xs -> eval (x:(eval (x:xs) (Prec h g)) ++ xs) h 

typesEqual :: FunctionType -> FunctionType -> Bool 
typesEqual (Nothing, a) (_, b) = a == b
typesEqual (_, a) (Nothing, b) = a == b
typesEqual (Just x, a) (Just y, b) = x == y && a == b

findType :: Program -> Maybe FunctionType 
findType = \case 
  Nat _ -> return (Nothing, 1)
  Succ -> return (Just 1, 1) 
  Proj index arity -> return (Just arity, 1)

  Compose f g -> do (fIn, fOut) <- findType f 
                    (gIn, gOut) <- findType g 

                    -- If fIn is Nothing, then we are fine 
                    guard (gOut == fromMaybe gOut fIn) 

                    return (gIn, fOut) 

  Prec h g -> do (gIn, 1) <- findType g 
                 (hIn, 1) <- findType h 

                 case (gIn, hIn) of 
                   (Just gIn', Just hIn') -> do 
                     guard (hIn' == gIn' + 2)
                     return (Just $ gIn' + 1, 1)

                   (Nothing, Just hIn') -> 
                     return (Just $ hIn' - 1, 1)

                   (Just gIn', Nothing) -> 
                     return (Just $ gIn' + 1, 1)

                   (Nothing, Nothing) ->
                     return (Nothing, 1) 

  Tuple ~(f:fs) -> do fType@(fIn, _) <- findType f 
                      -- Make sure all functions have the same types
                      fsTypes <- traverse findType fs 
                      guard (and $ map (typesEqual fType) fsTypes)
                      --guard (foldl (typesEqual fType) True fsTypes) 

                      return (fIn, genericLength (f:fs))

