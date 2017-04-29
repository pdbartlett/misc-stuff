-- Empty module to serve as the default current module.
module Hugs where

import Control.Concurrent
import Data.List
import IO
import System.Environment

main = do
  threadDelay 3000
  putStrLine "Was it worth the wait?"

main3 = do
  prog <- getProgName
  args <- getArgs
  putStrLn $ unwords (prog : args)

main2 = do
  name <- getLine
  if name == "Paul"
    then putStrLn "Programmer"
    else if name == "Sophie"
      then putStrLn "Student"
      else putStrLn "Not sure"

main1 = do
  list <- getList
  putStrLn ("list is " ++ show list)
  putStrLn ("sum is " ++ show (foldr (+) 0 list))
  putStrLn ("product is " ++ show (foldr (*) 1 list))
  showFactorials list

getList = do
  line <- getLine
  if read line == 0
    then return []
    else do
      rest <- getList
      return ((read line :: Int) : rest)

showFactorials [] = return ()
showFactorials (x:xs) = do
  putStrLn (show x ++ "! = " ++ show (foldr (*) 1 [2..x]))
  showFactorials xs

x = 5
y = (6, "Hello")
z = x * fst y

square x = x * x
signum x | x > 0  = 1
         | x == 0 = 0
         | x < 0  = -1
         
fib 1 = 1
fib 2 = 1
fib n = fib (n - 2) + fib (n - 1)

my_mult 0 _ = 0
my_mult _ 0 = 0
my_mult a b = a + my_mult a (b - 1)

my_map :: (a -> b) -> [a] -> [b]
my_map _ [] = []
my_map f (x:xs) = f x : my_map f xs

data Triple a b c = Triple a b c

tripleFst (Triple x _ _) = x
tripleSnd (Triple _ x _) = x
tripleThd (Triple _ _ x) = x

data List a = Nil
            | Cons a (List a)

listHead (Cons x xs) = x
listTail (Cons x xs) = xs

listFoldr f n Nil = n
listFoldr f n (Cons x xs) = listFoldr f (f n x) xs

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)

elements (Leaf v) = [v]
elements (Branch lhs v rhs) = (elements lhs) ++ (v : elements rhs)
