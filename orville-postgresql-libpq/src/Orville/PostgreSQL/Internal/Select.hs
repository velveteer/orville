{-# LANGUAGE GADTs #-}

module Orville.PostgreSQL.Internal.Select
  ( Select,
    executeSelect,
    selectToQueryExpr,
    selectTable,
    selectMarshalledColumns,
    selectSelectList,
    selectQueryExpr,
  )
where

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import Orville.PostgreSQL.Internal.SqlMarshaller (ReadOnlyColumnOption (IncludeReadOnlyColumns), SqlMarshaller, marshallerColumnNames)
import Orville.PostgreSQL.Internal.TableDefinition (TableDefinition, tableMarshaller, tableName)

{- |
  Represents a @SELECT@ statement that can be executed against a database. A
  'Select' has a 'SqlMarshaller' bound to it that will be used to decode the
  database result set when it is executed.
-}
data Select readEntity where
  Select :: SqlMarshaller writeEntity readEntity -> Expr.QueryExpr -> Select readEntity

{- |
  Extracts the query that will be run when the select is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.
-}
selectToQueryExpr :: Select readEntity -> Expr.QueryExpr
selectToQueryExpr (Select _ query) = query

{- |
  Excutes the database query for the 'Select' and uses its 'SqlMarshaller' to
  decode the result set.
-}
executeSelect :: MonadOrville.MonadOrville m => Select row -> m [row]
executeSelect (Select marshaller query) =
  Execute.executeAndDecode query marshaller

{- |
  Builds a 'Select' that will select all the columns described in the
  'TableDefinition'. This is the safest way to build a 'Select', because table
  name and columns are all read from the 'TableDefinition'. If the table is
  being managed with Orville auto migrations, this will match the schema in the
  database.
-}
selectTable ::
  TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  Select readEntity
selectTable tableDef =
  selectMarshalledColumns (tableMarshaller tableDef) (tableName tableDef)

{- |
  Builds a 'Select' that will select the columns described by the marshaller
  from the specified table. It is up to the caller to ensure that the columns
  in the marshaller make sense for the table.

  This function is useful for query a subset of table columns using a custom
  marshaller.
-}
selectMarshalledColumns ::
  SqlMarshaller writeEntity readEntity ->
  Expr.QualifiedTableName ->
  SelectOptions.SelectOptions ->
  Select readEntity
selectMarshalledColumns marshaller =
  selectSelectList
    (Expr.selectColumns (marshallerColumnNames IncludeReadOnlyColumns marshaller))
    marshaller

{- |
  Builds a 'Select' that will use the specified 'Expr.SelectList' when building
  the @SELECT@ statement to execute. It it up to the caller to make sure that
  the 'Expr.SelectList' expression makes sens for the table being queried, and
  that the names of the columns in the result set match those expected by the
  given 'SqlMarshaller', which will be used to decode it.

  This function is useful for building more advanced queries that need to
  select things other than simple columns from the table, such as using
  aggregate functions. The 'Expr.SelectList' can be built however the caller
  desires. If Orville does not support building the 'Expr.SelectList' you need
  using any of the expression building functions, you can resort to
  @RawSql.fromRawSql@ as an escape hatch to build the 'Expr.SelectList' here.
-}
selectSelectList ::
  Expr.SelectList ->
  SqlMarshaller writeEntity readEntity ->
  Expr.QualifiedTableName ->
  SelectOptions.SelectOptions ->
  Select readEntity
selectSelectList selectList marshaller qualifiedTableName selectOptions =
  selectQueryExpr marshaller $
    Expr.queryExpr
      (SelectOptions.selectDistinct selectOptions)
      selectList
      ( Just $
          Expr.tableExpr
            qualifiedTableName
            (SelectOptions.selectWhereClause selectOptions)
            (SelectOptions.selectOrderByClause selectOptions)
            (SelectOptions.selectGroupByClause selectOptions)
            (SelectOptions.selectLimitExpr selectOptions)
            (SelectOptions.selectOffsetExpr selectOptions)
      )

{- |
  Builds a 'Select' that will execute the specified query and use the given
  'SqlMarshaller' to decode it. It is up to the caller to ensure that the given
  'Expr.QueryExpr' makes sense and produces a result set that the
  'SqlMarshaller' can decode.

  This is the lowest level of escape hatch available for 'Select'. The caller
  can build any query that Orville supports using the expression building
  functions, or use @RawSql.fromRawSql@ to build a raw 'Expr.QueryExpr'.
-}
selectQueryExpr ::
  SqlMarshaller writeEntity readEntity ->
  Expr.QueryExpr ->
  Select readEntity
selectQueryExpr = Select
