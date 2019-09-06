{-# LANGUAGE LambdaCase, NPlusKPatterns #-}
module Eval where 

import Programs 

-- import Data.Vector
import Data.Maybe (fromJust)
import Data.List (genericIndex)
import qualified Data.Map.Strict as Map 

import Control.Monad.Trans.State.Strict 

eval :: EvalState -> [Integer] -> Program -> [Integer]
eval context = 
  let go input = \case 
        Identifier v -> go input (fromJust $ Map.lookup v context)
        Nat i -> [i]
        Succ -> map succ input
        Proj index _ -> [input `genericIndex` (index - 1)]
        Compose f g -> go (go input g) f 
        Tuple fs -> concatMap (go input) fs 
        Prec h g -> case input of 
                      0:xs -> go xs g 
                      ~((x+1):xs) -> go (x:(go (x:xs) (Prec h g)) ++ xs) h 
                      -- Empty list is not possible since 
                      -- Prec[h, g] has type N^n -> N for n >= 1
  in go 

type EvalT = StateT EvalState

evalEvalT :: Monad m => EvalT m a -> m a 
evalEvalT = flip evalStateT Map.empty 

addProgram name program = modify $ Map.insert name program
