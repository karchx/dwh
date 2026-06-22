{-# LANGUAGE OverloadedStrings #-}
module Graph (visualizeAST) where

import Syntax
import Data.Graph
import Data.Tree (drawForest)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S

extractEdges :: Program a -> [(String, String)]
extractEdges stmts = concatMap extract stmts
    where
        extract (SExpr _ (EConnect _ e1 e2)) = [(identifyJob e1, identifyJob e2)]
        extract (SFun _ _ _ body) = concatMap extract body
        extract (SReturn _ e) = [(identifyJob e, "return")]
        extract (SAssign _ _ e) = [(identifyJob e, "assign")]
        extract _ = []

        identifyJob (EVar _ name) = T.unpack name
        identifyJob (ELit _ name) = show name
        identifyJob (EApp _ (EVar _ name) _) = T.unpack name
        identifyJob _ = "undefined (?_?)"

buildGraphData :: [(String, String)] -> [(String, String, [String])]
buildGraphData edgs =
    let
        allNodes = S.toList $ S.fromList (map fst edgs ++ map snd edgs)

        adjacencyMap = M.fromListWith (++) [(u, [v]) | (u, v) <- edgs]
    in
        [ (node, node, M.findWithDefault [] node adjacencyMap) | node <- allNodes ]

visualizeAST :: Program a -> IO ()
visualizeAST prog = do
    let edgs = extractEdges prog
        graphData = buildGraphData edgs

    let (graph, nodeFromVertex, _) = graphFromEdges graphData
    
    let graphForest = fmap (fmap (\v -> let (label, _, _) = nodeFromVertex v in label)) (components graph)
    putStr (drawForest graphForest)