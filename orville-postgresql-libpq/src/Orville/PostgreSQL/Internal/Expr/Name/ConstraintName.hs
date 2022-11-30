{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.ConstraintName
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.ConstraintName
  ( ConstraintName,
    constraintName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ConstraintName
  = ConstraintName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

constraintName :: String -> ConstraintName
constraintName = ConstraintName . identifier
