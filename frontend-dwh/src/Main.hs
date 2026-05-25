
module Main (main, runDebugger) where

import Eval (eval)
import Lexer (program)
import Semantic (checkProgram)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T

runDebugger :: String -> IO()
runDebugger input = do
    let txtInput = T.pack input
    case parse program "" txtInput of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> do
            case checkProgram ast of
                Left err -> do
                    putStrLn "============ Semantic Error ============"
                    print err
                Right _ -> do
                    putStrLn "============== AST PARSER =============="
                    print ast

main :: IO ()
main = putStrLn "Debugger.."