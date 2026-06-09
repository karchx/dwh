{-# LANGUAGE OverloadedStrings #-}

module Main (stmts, main, getVarNames, countSExprs) where
import Data.Text (Text)
import Syntax

stmt2 = [
    SVar 1 "x" (EBool 1 True), 
    SVar 1 "y" (EString 1 "karchx")
    ]

stmt3 = [
    SVar 1 "name" (EString 1 "kar"), 
    SVar 1 "lastname" (EString 1 "chx"),
    SVar 1 "x" (ELit 1 123.33)
    ]

stmts = [
    SVar 1 "x" (EBool 1 True), 
    SVar 1 "y" (EString 1 "karchx"), 
    SExpr 1 (ELit 1 2.5),
    SExpr 1 (ELit 1 12.5),
    SExpr 1 (EBool 1 True),
    SFun 1 "fun1" ["a", "b"] stmt2,
    SFun 1 "fun2" ["name", "lastname", "x"] stmt3
    ]

getVarNames :: [Stmt a] -> [Text]
getVarNames ss = [name | SVar _ name _ <- ss]

countSExprs :: [Stmt a] -> Int
countSExprs ss = sum $ [1 | SExpr _ _ <- ss]

getFunctionArities :: [Stmt a] -> [(Text, Int)]
getFunctionArities ss = [(name, length prms) | SFun _ name prms _ <- ss]

main = undefined