{-|
Module    : Database.Orville.Internal.Expr.Expr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE CPP #-}

module Database.Orville.Internal.Expr.Expr where

#if ! MIN_VERSION_base(4,11,0)
import            Data.Semigroup
#endif

import            Data.String


data RawExpr =
    RawExprString String
  | RawExprAppend RawExpr RawExpr
  | RawExprConcat [RawExpr]

rawSql :: String -> RawExpr
rawSql = RawExprString

rawExprToSql :: RawExpr -> String
rawExprToSql =
    go ""
  where
    go rest (RawExprString s) = s ++ rest
    go rest (RawExprAppend r1 r2) = go (go rest r2) r1
    go rest (RawExprConcat exprs) = foldr (flip go) rest exprs

instance Monoid RawExpr where
  mempty = RawExprString ""
  mappend = RawExprAppend
  mconcat = RawExprConcat

instance Semigroup RawExpr where
  (<>) = mappend

instance IsString RawExpr where
  fromString = rawSql

newtype Expr a = Expr (Either RawExpr a)

class GenerateSql expr where
  generateSql :: expr -> RawExpr

instance GenerateSql RawExpr where
  generateSql = id

instance GenerateSql a => GenerateSql (Expr a) where
  generateSql (Expr (Right a)) = generateSql a
  generateSql (Expr (Left raw)) = raw

rawSqlExpr :: String -> Expr a
rawSqlExpr = Expr . Left . rawSql

expr :: a -> Expr a
expr = Expr . Right
