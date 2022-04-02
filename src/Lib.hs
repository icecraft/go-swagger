module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


orString :: String -> String -> String 
orString a b = case len a of 
                0 -> b 
                _ -> a 


