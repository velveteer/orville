{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.CursorName
Copyright : Flipstone Technology Partners 2022
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.CursorName
  ( CursorName,
    cursorName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype CursorName
  = CursorName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

cursorName :: String -> CursorName
cursorName =
  CursorName . identifier
