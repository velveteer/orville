{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.ColumnName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.ColumnName
  ( ColumnName,
    columnName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnName
  = ColumnName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

columnName :: String -> ColumnName
columnName = ColumnName . identifier
