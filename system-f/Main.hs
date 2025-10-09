{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ast (Name (..), Type (..))

main :: IO ()
main = do
    print (TForall (Name "a") TInt)
    putStrLn $ show (TArrow (TArrow TInt TInt) TBool)
