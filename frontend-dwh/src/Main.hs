
module Main (main, runReplPipe, runFile) where

import Graph
import Parser (program)
import Semantic (checkProgram)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

runFile :: FilePath -> IO ()
runFile path = do
    content <- TIO.readFile path
    case parse program path content of
        Left err -> putStrLn $ "Parser Error: \n" ++ errorBundlePretty err
        Right ast -> do
            visualizeAST ast
            putStrLn $ "Success AST"

runReplPipe :: String -> IO ()
runReplPipe input = do
    let txt = T.pack input
    case parse program "REPL" txt of
        Left parseErr -> putStrLn $ "Parse Error:\n" ++ errorBundlePretty parseErr
        Right ast -> case checkProgram ast of
            Left semErr    -> putStrLn $ "Semantic Error: " ++ show semErr
            Right typedAst -> putStrLn $ "Success AST:\n" ++ show typedAst

main :: IO ()
main = putStrLn "Debugger.."