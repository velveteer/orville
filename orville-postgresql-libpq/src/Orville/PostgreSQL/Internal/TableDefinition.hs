{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Orville.PostgreSQL.Internal.TableDefinition
  ( TableDefinition,
    HasKey,
    NoKey,
    mkTableDefinition,
    mkTableDefinitionWithoutKey,
    dropColumns,
    columnsToDrop,
    tableIdentifier,
    tableName,
    setTableSchema,
    tableConstraints,
    addTableConstraints,
    tablePrimaryKey,
    tableMarshaller,
    mkInsertExpr,
    mkCreateTableExpr,
    mkTableColumnDefinitions,
    mkTablePrimaryKeyExpr,
    mkInsertColumnList,
    mkInsertSource,
    mkUpdateExpr,
    mkDeleteExpr,
    ReturningOption (WithoutReturning, WithReturning),
  )
where

import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Orville.PostgreSQL.Internal.ConstraintDefinition (ConstraintDefinition, ConstraintMigrationKey, constraintMigrationKey, constraintSqlExpr)
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import Orville.PostgreSQL.Internal.FieldDefinition (fieldColumnDefinition, fieldColumnName, fieldValueToSqlValue)
import Orville.PostgreSQL.Internal.PrimaryKey (PrimaryKey, mkPrimaryKeyExpr, primaryKeyEqualsExpr)
import Orville.PostgreSQL.Internal.SqlMarshaller (FieldFold, ReadOnlyColumnOption (ExcludeReadOnlyColumns, IncludeReadOnlyColumns), SqlMarshaller, collectFromField, foldMarshallerFields, marshallerColumnNames)
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import Orville.PostgreSQL.Internal.TableIdentifier (TableIdentifier, setTableIdSchema, tableIdQualifiedName, unqualifiedNameToTableId)

{- |
  Contains the definition of a SQL table for Orville to use for generating
  queries and marshalling Haskell values to and from the database.

  * 'key' is the Haskell type used to store values of the primary
    key for the table.

  * 'writeEntity' is the Haskell type for values that Orville will write
    to the database for you (i.e. both inserts and updates)

  * 'readEntity' is the Haskell type for values that Orville will decode
    from the result set when entities are queried from this table.
-}
data TableDefinition key writeEntity readEntity = TableDefinition
  { _tableIdentifier :: TableIdentifier
  , _tablePrimaryKey :: TablePrimaryKey key
  , _tableMarshaller :: SqlMarshaller writeEntity readEntity
  , _tableColumnsToDrop :: Set.Set String
  , _tableConstraints :: Map.Map ConstraintMigrationKey ConstraintDefinition
  }

data HasKey key
data NoKey

data TablePrimaryKey tag where
  TableHasKey :: PrimaryKey key -> TablePrimaryKey (HasKey key)
  TableHasNoKey :: TablePrimaryKey NoKey

{- |
  Constructs a new 'TableDefinition' with the basic fields required for
  operation. For convenience, this function accepts a 'PrimaryKey' even though
  this is not required for all Orville operations to work. If you need to
  create a table without any primary key, see 'mkTableDefinitionWithoutKey'.
-}
mkTableDefinition ::
  -- | The name of the table
  String ->
  -- | Definition of the table's primary key
  PrimaryKey key ->
  -- | A 'SqlMarshaller' to marshall table entities to and from the database
  SqlMarshaller writeEntity readEntity ->
  TableDefinition (HasKey key) writeEntity readEntity
mkTableDefinition name primaryKey marshaller =
  (mkTableDefinitionWithoutKey name marshaller)
    { _tablePrimaryKey = TableHasKey primaryKey
    }

{- |
  Constructs a new 'TableDefinition' with the minimal fields required for
  operation. Note: tables created via this function will not have a primary
  key. Certain Orville functions required a primary key. Attempting to call
  functions requiring a primary key will fail to compile when using a table
  that has no key.
-}
mkTableDefinitionWithoutKey ::
  -- | The name of the table
  String ->
  -- | A 'SqlMarshaller' to marshall table entities to and from the database
  SqlMarshaller writeEntity readEntity ->
  TableDefinition NoKey writeEntity readEntity
mkTableDefinitionWithoutKey name marshaller =
  TableDefinition
    { _tableIdentifier = unqualifiedNameToTableId name
    , _tablePrimaryKey = TableHasNoKey
    , _tableMarshaller = marshaller
    , _tableColumnsToDrop = Set.empty
    , _tableConstraints = Map.empty
    }

{- |
  Annotates a 'TableDefinition' with a direction to drop columns if they are
  found in the database. Orville does not drop columns during auto migration
  unless they are explicitly requested to be dropped via 'dropColumns'.

  If you remove a reference to a column from the table's 'SqlMarshaller'
  without adding the column's name to 'dropColumns', Orville will operate as if
  the column does not exist without actually dropping the column. This is often
  useful if you're not sure you want to lose the data in the column, or if you
  have zero down-time deployments, which requires the column not be referenced
  by deployed code before it can be dropped.
-}
dropColumns ::
  -- | Columns that should be dropped from the table
  [String] ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
dropColumns columns tableDef =
  tableDef
    { _tableColumnsToDrop = _tableColumnsToDrop tableDef <> Set.fromList columns
    }

{- |
  Returns the set of columns that have be marked be dropped by 'dropColumns'
-}
columnsToDrop :: TableDefinition key writeEntity readEntity -> Set.Set String
columnsToDrop =
  _tableColumnsToDrop

{- |
  Returns the table's 'TableIdentifier'
-}
tableIdentifier :: TableDefinition key writeEntity readEntity -> TableIdentifier
tableIdentifier =
  _tableIdentifier

{- |
  Returns the table's name as an expression that can be used to build SQL
  statements. If the table has a schema name set, the name will be qualified
  with it.
-}
tableName :: TableDefinition key writeEntity readEntity -> Expr.QualifiedTableName
tableName =
  tableIdQualifiedName . _tableIdentifier

{- |
  Set's the table's schema to the name in the given string, which will be
  treated as a SQL identifier. If a table has a schema name set, it will be
  included as a qualified on the table name for all queries involving the
  table.
-}
setTableSchema ::
  String ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
setTableSchema schemaName tableDef =
  tableDef
    { _tableIdentifier = setTableIdSchema schemaName (_tableIdentifier tableDef)
    }

{- |
  Retrieves all the table constraints that have been added to the table via
  'addTableConstraints'.
-}
tableConstraints ::
  TableDefinition key writeEntity readEntity ->
  Map.Map ConstraintMigrationKey ConstraintDefinition
tableConstraints =
  _tableConstraints

{- |
  Adds the given table constraints to the table definition.

  Note: If multiple constraints are added with the same
  'ConstraintMigrationKey', only the last one that is added will be part of the
  'TableDefinition'. Any previously added constraint with the same key is
  replaced by the new one.
-}
addTableConstraints ::
  [ConstraintDefinition] ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
addTableConstraints constraintDefs tableDef =
  let addConstraint constraint constraintMap =
        Map.insert (constraintMigrationKey constraint) constraint constraintMap
   in tableDef
        { _tableConstraints = foldr addConstraint (_tableConstraints tableDef) constraintDefs
        }

{- |
  Returns the primary key for the table, as defined at construction via 'mkTableDefinition'.
-}
tablePrimaryKey :: TableDefinition (HasKey key) writeEntity readEntity -> PrimaryKey key
tablePrimaryKey def =
  case _tablePrimaryKey def of
    TableHasKey primaryKey -> primaryKey

{- |
  Returns the marshaller for the table, as defined at construction via 'mkTableDefinition'.
-}
tableMarshaller :: TableDefinition key writeEntity readEntity -> SqlMarshaller writeEntity readEntity
tableMarshaller = _tableMarshaller

{- |
  Builds a 'Expr.CreateTableExpr' that will create a SQL table matching the
  given 'TableDefinition' when it is executed.
-}
mkCreateTableExpr ::
  TableDefinition key writeEntity readEntity ->
  Expr.CreateTableExpr
mkCreateTableExpr tableDef =
  Expr.createTableExpr
    (tableName tableDef)
    (mkTableColumnDefinitions tableDef)
    (mkTablePrimaryKeyExpr tableDef)
    (map constraintSqlExpr . Map.elems . _tableConstraints $ tableDef)

{- |
  Builds the 'Expr.ColumnDefinitions' for all the fields described by the
  table definition's 'SqlMarshaller'.
-}
mkTableColumnDefinitions ::
  TableDefinition key writeEntity readEntity ->
  [Expr.ColumnDefinition]
mkTableColumnDefinitions tableDef =
  foldMarshallerFields
    (tableMarshaller tableDef)
    []
    (collectFromField IncludeReadOnlyColumns fieldColumnDefinition)

{- |
  Builds the 'Expr.PrimaryKeyExpr' for this table, or none of this table has no
  primary key.
-}
mkTablePrimaryKeyExpr ::
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.PrimaryKeyExpr
mkTablePrimaryKeyExpr tableDef =
  case _tablePrimaryKey tableDef of
    TableHasKey primaryKey ->
      Just $ mkPrimaryKeyExpr primaryKey
    TableHasNoKey ->
      Nothing

{- |
  Specifies whether or not a @RETURNING@ clause should be included when a
  query expression is built. This type is found as a parameter on a number
  of the query building functions related to 'TableDefinition'.
-}
data ReturningOption
  = WithoutReturning
  | WithReturning

mkReturningClause ::
  ReturningOption ->
  TableDefinition key writeEntity readEntty ->
  Maybe Expr.ReturningExpr
mkReturningClause returningOption tableDef =
  case returningOption of
    WithoutReturning ->
      Nothing
    WithReturning ->
      Just
        . Expr.returningExpr
        . marshallerColumnNames IncludeReadOnlyColumns
        . tableMarshaller
        $ tableDef

{- |
  Builds an 'Expr.InsertExpr' that will insert the given entities into the SQL
  table when it is executed. A @RETURNING@ clause with either be included to
  return the insert rows or not, depending on the 'ReturnOption' given.
-}
mkInsertExpr ::
  ReturningOption ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Expr.InsertExpr
mkInsertExpr returningOption tableDef entities =
  let insertColumnList =
        mkInsertColumnList . tableMarshaller $ tableDef

      insertSource =
        mkInsertSource (tableMarshaller tableDef) entities
   in Expr.insertExpr
        (tableName tableDef)
        (Just insertColumnList)
        insertSource
        (mkReturningClause returningOption tableDef)

{- |
  Builds an 'Expr.InsertColumnList' that specifies the columns for an
  insert statement in the order that they appear in the given 'SqlMarshaller'.

  In normal circumstances you will want to build the complete insert statement
  via 'mkInsertExpr', but this is exported in case you are a composing SQL
  yourself and need the column list of an insert as a fragment.
-}
mkInsertColumnList ::
  SqlMarshaller writeEntity readEntity ->
  Expr.InsertColumnList
mkInsertColumnList =
  Expr.insertColumnList . marshallerColumnNames ExcludeReadOnlyColumns

{- |
  Builds an 'Expr.InsertSource' that will insert the given entities with their
  values specified in the order that the fields appear in the given
  'SqlMarshaller' (which matches the order of column names produced by
  'mkInsertColumnColumnsList').

  In normal circumstances you will want to build the complete insert statement
  via 'mkInsertExpr', but this is exported in case you are a composing SQL
  yourself and need the column list of an insert as a fragment.
-}
mkInsertSource ::
  SqlMarshaller writeEntity readEntity ->
  NonEmpty writeEntity ->
  Expr.InsertSource
mkInsertSource marshaller entities =
  let encodeRow =
        foldMarshallerFields marshaller (const []) collectSqlValue
   in Expr.insertSqlValues $ map encodeRow (toList entities)

{- |
  An internal helper function that collects the 'SqlValue' encoded value for a
  field from a Haskell entity, adding it a list of 'SqlValue's that is being
  built.
-}
collectSqlValue :: FieldFold entity (entity -> [SqlValue])
collectSqlValue fieldDef maybeAccessor encodeRest entity =
  case maybeAccessor of
    Just accessor -> fieldValueToSqlValue fieldDef (accessor entity) : (encodeRest entity)
    Nothing -> (encodeRest entity)

{- |
  Builds an 'Expr.UpdateExpr' that will update the entity at the given 'key'
  with the values from the 'writeEntity' when executed.
  SQL table when it is executed.
-}
mkUpdateExpr ::
  ReturningOption ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Expr.UpdateExpr
mkUpdateExpr returningOption tableDef key writeEntity =
  let setClauses =
        foldMarshallerFields
          (tableMarshaller tableDef)
          []
          (collectSetClauses writeEntity)

      isEntityKey =
        primaryKeyEqualsExpr
          (tablePrimaryKey tableDef)
          key
   in Expr.updateExpr
        (tableName tableDef)
        (Expr.setClauseList setClauses)
        (Just (Expr.whereClause isEntityKey))
        (mkReturningClause returningOption tableDef)

{- |
  Builds an 'Expr.DeleteExpr' that will delete the entity with the given 'key'.
-}
mkDeleteExpr ::
  ReturningOption ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  Expr.DeleteExpr
mkDeleteExpr returningOption tableDef key =
  let isEntityKey =
        primaryKeyEqualsExpr
          (tablePrimaryKey tableDef)
          key
   in Expr.deleteExpr
        (tableName tableDef)
        (Just (Expr.whereClause isEntityKey))
        (mkReturningClause returningOption tableDef)

{- |
  An internal helper function that collects the 'Expr.SetClause's to
  update all the fields contained in a 'SqlMarshaller'
-}
collectSetClauses :: entity -> FieldFold entity [Expr.SetClause]
collectSetClauses entity fieldDef maybeAccessor clauses =
  case maybeAccessor of
    Nothing ->
      clauses
    Just accessor ->
      let newClause =
            Expr.setColumn
              (fieldColumnName fieldDef)
              (fieldValueToSqlValue fieldDef (accessor entity))
       in newClause : clauses
