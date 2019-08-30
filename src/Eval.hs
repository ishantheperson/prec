{-# LANGUAGE LambdaCase, NPlusKPatterns #-}
module Eval where 

import Programs 

import Data.List (genericIndex)

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
