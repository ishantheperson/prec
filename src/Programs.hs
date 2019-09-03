{-# LANGUAGE LambdaCase #-}
module Programs where 

import Data.Maybe (fromMaybe)
import Data.List (intercalate, genericLength)
import qualified Data.Map as Map

import Control.Monad (guard)

-- | A function's type is either 
--   (Just a, b) --> N^a -> N^b
--   (Nothing, b) -> anything -> N^b  
type FunctionType = (Maybe Integer, Integer) 
type EvalState = Map.Map String Program 

data Program = 
             -- | Represents natural number constants 
               Nat Integer 
             | Succ 
             -- | Projection Index Arity
             | Proj Integer Integer 
             | Compose Program Program 
             | Prec Program Program 
             -- | Technically this is composition
             --   but its easier to notate and easier
             --   to understand this way
             | Tuple [Program] 
             -- | Previously bound expressions
             | Identifier String 

typesEqual :: FunctionType -> FunctionType -> Bool 
typesEqual (Nothing, a) (_, b) = a == b
typesEqual (_, a) (Nothing, b) = a == b
typesEqual (Just x, a) (Just y, b) = x == y && a == b

-- | Checks if a given Program is well-formed,
--   and returns its type if it is. Otherwise
--   returns Nothing
findType :: EvalState -> Program -> Maybe FunctionType 
findType context = 
  let go = \case 
        Nat _ -> return (Nothing, 1)
        Succ -> return (Just 1, 1) 
        Proj _ arity -> return (Just arity, 1)
      
        Compose f g -> do (fIn, fOut) <- go f 
                          (gIn, gOut) <- go g 
      
                          -- If fIn is Nothing, then we are fine 
                          guard (gOut == fromMaybe gOut fIn) 
      
                          return (gIn, fOut) 
      
        Prec h g -> do (gIn, 1) <- go g 
                       (hIn, 1) <- go h 
      
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
      
        Tuple ~(f:fs) -> do fType@(fIn, _) <- go f 
                            -- Make sure all functions have the same types
                            fsTypes <- traverse go fs 
                            guard (and $ map (typesEqual fType) fsTypes)
      
                            return (fIn, genericLength (f:fs))
    
        Identifier v -> go =<< Map.lookup v context
  in go 

  

instance Show Program where 
  show = \case 
    Nat i -> show i 
    Succ -> "S"
    Proj index arity -> "Proj(" ++ show index ++ ", " ++ show arity ++ ")"
    Compose f g -> "(" ++ show f ++ " o " ++ show g ++ ")"
    Prec h g -> "Prec[" ++ show h ++ ", " ++ show g ++ "]"
    Tuple xs -> "(" ++ intercalate ", " (map show xs) ++ ")"
    Identifier v -> v 
