{-# LANGUAGE GADTs #-}

{- |

Module    : Orville.PostgreSQL.Internal.Delete
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Delete (Delete, deleteToDeleteExpr, executeDelete, executeDeleteReturnEntity, deleteToTableReturning, deleteToTable) where

import Data.Maybe (listToMaybe)

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller)
import Orville.PostgreSQL.Internal.TableDefinition (HasKey, ReturningOption (WithReturning, WithoutReturning), TableDefinition, mkDeleteExpr, tableMarshaller)

{- | Represents an @DELETE@ statement that can be executed against a database. An 'Delete' has a
  'SqlMarshaller' bound to it that, when the delete returns data from the database, will be used to
  decode the database result set when it is executed.
-}
data Delete readEntity where
  Delete :: SqlMarshaller writeEntity readEntity -> Expr.DeleteExpr -> Delete readEntity

{- |
  Extracts the query that will be run when the delete is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.
-}
deleteToDeleteExpr :: Delete readEntity -> Expr.DeleteExpr
deleteToDeleteExpr (Delete _ expr) = expr

{- |
  Executes the database query for the 'Delete' and returns '()'.
-}
executeDelete :: MonadOrville.MonadOrville m => Delete readEntity -> m ()
executeDelete (Delete _ expr) =
  Execute.executeVoid expr

{- | Executes the database query for the 'Delete' and uses its 'SqlMarshaller' to decode the row (that
  was just deleted) as returned via a RETURNING clause.
-}
executeDeleteReturnEntity :: MonadOrville.MonadOrville m => Delete readEntity -> m (Maybe readEntity)
executeDeleteReturnEntity (Delete marshaller expr) =
  fmap listToMaybe $ Execute.executeAndDecode expr marshaller

{- |
  Builds an 'Delete' that will delete all of the writable columns described in the
  'TableDefinition' without returning the data as seen by the database.
-}
deleteToTable ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  Delete readEntity
deleteToTable =
  deleteTable WithoutReturning

{- |
  Builds an 'Delete' that will delete all of the writable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.
-}
deleteToTableReturning ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  Delete readEntity
deleteToTableReturning =
  deleteTable WithReturning

-- an internal helper function for creating an delete with a given `ReturningOption`
deleteTable ::
  ReturningOption ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  Delete readEntity
deleteTable returningOption tableDef key =
  rawDeleteExpr (tableMarshaller tableDef) (mkDeleteExpr returningOption tableDef key)

{- |
  Builds an 'Delete' that will execute the specified query and use the given 'SqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.DeleteExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'SqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Select'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.DeleteExpr'.
-}
rawDeleteExpr :: SqlMarshaller writeEntity readEntity -> Expr.DeleteExpr -> Delete readEntity
rawDeleteExpr = Delete
