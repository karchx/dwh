
module Main (main, runReplPipe) where

import Parser (program)
import Semantic (checkProgram)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T

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