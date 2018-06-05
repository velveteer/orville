{-|
Module    : Database.Orville.Internal.Expr.NameExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Database.Orville.Internal.Expr.NameExpr where

#if ! MIN_VERSION_base(4,11,0)
import                Data.Monoid
#endif

import                Data.String

import                Database.Orville.Internal.Expr.Expr

type NameExpr = Expr NameForm

newtype NameForm = NameForm String
  deriving (Eq, Ord, IsString)

instance GenerateSql NameForm where
  generateSql (NameForm name) = "\"" <> rawSql name <> "\""

unescapedName :: NameForm -> String
unescapedName (NameForm s) = s
