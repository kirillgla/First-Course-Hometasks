module Main where

import Parser

main :: IO ()
main = putStrLn $ either show show $ parseLambda "(λx.x * 2*) 21"
