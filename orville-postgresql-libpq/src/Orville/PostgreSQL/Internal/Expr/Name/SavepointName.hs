{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.SavepointName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.SavepointName
  ( SavepointName,
    savepointName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SavepointName
  = SavepointName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

savepointName :: String -> SavepointName
savepointName = SavepointName . identifier
