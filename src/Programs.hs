{-# LANGUAGE LambdaCase #-}
module Programs where 

import Data.Maybe (fromMaybe)
import Data.List (intercalate, genericLength)

import Control.Monad (guard)

-- | A function's type is either 
--   (Just a, b) --> N^a -> N^b
--   (Nothing, b) -> anything -> N^b  
type FunctionType = (Maybe Integer, Integer) 

data Program = 
             -- | Represents natural number constants 
               Nat Integer 
             | Succ 
             -- | Projection Index Arity
             | Proj Integer Integer 
             | Compose Program Program 
             | Prec Program Program 
             | Tuple [Program] 

typesEqual :: FunctionType -> FunctionType -> Bool 
typesEqual (Nothing, a) (_, b) = a == b
typesEqual (_, a) (Nothing, b) = a == b
typesEqual (Just x, a) (Just y, b) = x == y && a == b

-- | Checks if a given Program is well-formed,
--   and returns its type if it is. Otherwise
--   returns Nothing 
findType :: Program -> Maybe FunctionType 
findType = \case 
  Nat _ -> return (Nothing, 1)
  Succ -> return (Just 1, 1) 
  Proj _ arity -> return (Just arity, 1)

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

                      return (fIn, genericLength (f:fs))

instance Show Program where 
  show = \case 
    Nat i -> show i 
    Succ -> "S"
    Proj index arity -> "Proj(" ++ show index ++ ", " ++ show arity ++ ")"
    Compose f g -> "(" ++ show f ++ " o " ++ show g ++ ")"
    Prec h g -> "Prec[" ++ show h ++ ", " ++ show g ++ "]"
    Tuple xs -> "(" ++ intercalate ", " (map show xs) ++ ")"
