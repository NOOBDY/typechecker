module Main where

import qualified Lexer as L

import Control.Monad (forM_, unless)
import qualified Data.ByteString.Lazy.Char8 as BS
import Infer (runInference)
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
                    -- putStrLn $ show tokens
                    let parseResult = parseExpr tokens
                    -- putStrLn $ show parseResult
                    inferResult <- eitherToIO $ runInference parseResult
                    putStrLn $ show inferResult
                )
        )
  where
    eitherToIO :: (Show a) => Either a b -> IO b
    eitherToIO (Left err) = error $ show err
    eitherToIO (Right res) = return res
