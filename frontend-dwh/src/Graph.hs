module Graph (analyzeGraph) where

import Syntax
import Data.Text (Text)
import qualified Data.Text as T

analyzeGraph :: Program a -> IO ()
analyzeGraph stmts = mapM_ extractConnector stmts
    where
        extractConnector (SExpr _ (EConnect _ e1 e2)) =
            putStrLn $ "Check for a connection between " ++ identifyJob e1 ++ " and " ++ identifyJob e2
        extractConnector (SFun _ _ _ body) = mapM_ extractConnector body
        extractConnector _ = return ()

        identifyJob (EVar _ name) = T.unpack name
        identifyJob (EApp _ (EVar _ name) _) = T.unpack name
        identifyJob _ = "undefined (?_?)"