module Fib where

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x
