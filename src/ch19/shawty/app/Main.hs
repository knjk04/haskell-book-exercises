module Main where

{-# LANGUAGE OverloadedStrings #-}
-- OverloadedStrings makes Strings polymorphic over the Num type class
-- Strings are usually a concrete type, not polymorphic
-- OverloadedStrings allows Strings to be used as Text and ByteString values

import Lib

main :: IO ()
main = someFunc
