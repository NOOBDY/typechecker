module Main where

import qualified Lexer as L

import Control.Monad (forM_, unless)
import qualified Data.ByteString.Lazy.Char8 as BS
import Infer (inferTypeOnly)
import Parser (parseExpr)

main :: IO ()
main = do
    content <- BS.readFile "algorithm-w/example/basic.fun"

    forM_
        (BS.lines content)
        ( \line -> do
            let tokens = L.alexScanTokens line
            unless
                (null tokens)
                ( do
                    putStrLn $ show line
                    let parseResult = parseExpr tokens
                    case inferTypeOnly parseResult of
                        Left err -> do
                            putStrLn $ show parseResult
                            putStrLn $ show err
                        Right inferResult -> putStrLn $ show inferResult

                    putStrLn ""
                )
        )
