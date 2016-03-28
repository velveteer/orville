{-# LANGUAGE ExistentialQuantification #-}
module Database.Orville.Internal.MigrateTable
  ( createTable
  , dropTable
  , migrateTable
  , MigrateTableException(..)
  ) where

import            Control.Monad
import qualified  Control.Exception as Exc
import qualified  Data.List as List
import            Data.Maybe
import            Data.Typeable
import            Database.HDBC

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types


createTable :: IConnection conn => conn -> TableDefinition entity -> IO ()
createTable conn tableDef = do
  let ddl = mkCreateTableDDL tableDef
  putStrLn ddl
  void $ run conn ddl []


dropTable :: IConnection conn => conn -> String -> IO ()
dropTable conn name = do
  let ddl = "DROP TABLE \"" ++ name ++ "\""
  putStrLn ddl
  void $ run conn ddl []


migrateTable :: IConnection conn => conn -> TableDefinition entity -> IO ()
migrateTable conn tableDef = do
  columns <- describeTable conn (tableName tableDef)

  case mkMigrateTableDDL columns tableDef of
    Nothing -> return ()
    Just ddl -> do
      putStrLn ddl
      stmt <- prepare conn ddl

      executeRaw stmt
        `Exc.catch` (Exc.throw . MTE tableDef)

mkMigrateTableDDL :: [(String, SqlColDesc)] -> TableDefinition entity -> Maybe String
mkMigrateTableDDL columns tableDef =
    if null stmts then
      Nothing
    else
      Just $ "ALTER TABLE \"" ++ tableName tableDef ++ "\" " ++ cols

  where fields = tableFields tableDef
        fieldColumn fieldDef = lookup (fieldName fieldDef) columns
        colStmt = mkMigrateColumnDDL <$> id <*> fieldColumn
        dropStmt = mkDropColumnDDL <$> id <*> flip lookup columns
        stmts = List.concatMap colStmt fields ++
                List.concatMap dropStmt (tableSafeToDelete tableDef)
        cols = List.intercalate ", " $ stmts


mkMigrateColumnTypeDDL :: FieldDefinition -> SqlColDesc -> Maybe String
mkMigrateColumnTypeDDL fieldDef colDesc =
  let fieldDesc = sqlFieldDesc fieldDef
  in if colType fieldDesc /= colType colDesc ||
        colSize fieldDesc /= colSize colDesc then
      Just $ "ALTER COLUMN " ++ fieldName fieldDef ++ " SET DATA TYPE " ++
             mkTypeDDL (fieldType fieldDef)
     else
      Nothing


mkMigrateColumnNullDDL :: FieldDefinition -> SqlColDesc -> Maybe String
mkMigrateColumnNullDDL fieldDef colDesc =
  let fieldDesc = sqlFieldDesc fieldDef
      fieldNull = fromMaybe True (colNullable fieldDesc)
      colNull = fromMaybe True (colNullable colDesc)
  in if fieldNull && not colNull then
      Just $ "ALTER COLUMN " ++ fieldName fieldDef ++ " DROP NOT NULL"
     else if not fieldNull && colNull then
      Just $ "ALTER COLUMN " ++ fieldName fieldDef ++ " SET NOT NULL"
     else
      Nothing


mkMigrateColumnDDL :: FieldDefinition -> Maybe SqlColDesc -> [String]
mkMigrateColumnDDL fieldDef Nothing = ["ADD COLUMN " ++ mkFieldDDL fieldDef]
mkMigrateColumnDDL fieldDef (Just desc) = catMaybes [
    mkMigrateColumnTypeDDL fieldDef desc
  , mkMigrateColumnNullDDL fieldDef desc
  ]


mkDropColumnDDL :: ColumnName -> Maybe SqlColDesc -> [String]
mkDropColumnDDL _ Nothing = []
mkDropColumnDDL name (Just _) = ["DROP COLUMN " ++ name]

mkFlagDDL :: ColumnFlag -> String
mkFlagDDL PrimaryKey = "PRIMARY KEY"
mkFlagDDL Unique = "UNIQUE"
mkFlagDDL Null = "NULL"
mkFlagDDL (Default def) = "DEFAULT " ++ toColumnDefaultSql def
mkFlagDDL (InsertDefault def) = "DEFAULT " ++ toColumnDefaultSql def
mkFlagDDL (References table field) =
  "REFERENCES \"" ++ tableName table ++ "\" (" ++ fieldName field ++ ")"

mkTypeDDL :: ColumnType -> String
mkTypeDDL AutomaticId = "SERIAL"
mkTypeDDL ForeignId = "INTEGER"
mkTypeDDL Integer = "INTEGER"
mkTypeDDL BigInteger = "BIGINT"
mkTypeDDL Double = "DOUBLE PRECISION"
mkTypeDDL Boolean = "BOOLEAN"
mkTypeDDL (Text len) = "CHAR(" ++ show len ++ ")"
mkTypeDDL (VarText len) = "VARCHAR(" ++ show len ++ ")"
mkTypeDDL (Date) = "DATE"
mkTypeDDL (Timestamp) = "TIMESTAMP with time zone"
mkTypeDDL TextSearchVector = "TSVECTOR"

mkFieldDDL :: FieldDefinition -> String
mkFieldDDL (name, columnType, flags) =
        name ++ " " ++ sqlType ++ " " ++ flagSql
  where sqlType = mkTypeDDL columnType
        flagSql = List.intercalate " " (notNull : map mkFlagDDL flags)
        notNull = if any isNullFlag flags then
                    ""
                  else
                    "NOT NULL"

mkCreateTableDDL :: TableDefinition entity -> String
mkCreateTableDDL tableDef =
    "CREATE TABLE \"" ++ tableName tableDef ++ "\" (" ++ fields ++ ")"
  where fields = List.intercalate ", " $ map mkFieldDDL (tableFields tableDef)

columnTypeSqlId :: ColumnType -> SqlTypeId
columnTypeSqlId AutomaticId = SqlBigIntT
columnTypeSqlId ForeignId = SqlBigIntT
columnTypeSqlId Integer = SqlBigIntT
columnTypeSqlId Boolean = SqlBitT
columnTypeSqlId BigInteger = SqlBigIntT
columnTypeSqlId Double = SqlFloatT
columnTypeSqlId (VarText _) = SqlVarCharT
columnTypeSqlId (Text _) = SqlCharT
columnTypeSqlId Date = SqlDateT
columnTypeSqlId Timestamp = SqlTimestampWithZoneT
columnTypeSqlId TextSearchVector = SqlUnknownT "3614"

columnTypeSqlSize :: ColumnType -> Maybe Int
columnTypeSqlSize AutomaticId = Just 4
columnTypeSqlSize ForeignId = Just 4
columnTypeSqlSize Integer = Just 4
columnTypeSqlSize BigInteger = Just 8
columnTypeSqlSize Double = Just 8
columnTypeSqlSize Boolean = Just 1
columnTypeSqlSize (VarText n) = Just n
columnTypeSqlSize (Text n) = Just n
columnTypeSqlSize Date = Just 4
columnTypeSqlSize Timestamp = Just 8
columnTypeSqlSize TextSearchVector = Nothing

sqlFieldDesc :: FieldDefinition -> SqlColDesc
sqlFieldDesc (_, columnType, flags) = SqlColDesc {
    colType = columnTypeSqlId columnType
  , colSize = columnTypeSqlSize columnType
  , colNullable = Just (any isNullFlag flags)
  , colOctetLength = Nothing
  , colDecDigits = Nothing
  }

data MigrateTableException = forall entity. MTE (TableDefinition entity) Exc.SomeException
  deriving Typeable

instance Show MigrateTableException where
  show = formatMigrationException

instance Exc.Exception MigrateTableException

formatMigrationException :: MigrateTableException -> String
formatMigrationException (MTE tableDef exception) = message
  where message = "There was an error migrating table " ++ name ++ ".\n\
                  \The error is:\n\
                  \\n\
                  \ " ++ Exc.displayException exception ++ "\\n\
                  \\n\
                  \\n\
                  \Here are the developer comments regarding the table:\n\
                  \\n\
                  \ " ++ comments ++ "\
                  \\n"

        name = tableName tableDef
        comments = formatTableComments " " tableDef

formatTableComments :: String -> TableDefinition entity -> String
formatTableComments indent tableDef =
    List.intercalate ("\n" ++ indent) commentLines
  where
    commentLines = map formatTableComment comments
    comments = runComments (tableComments tableDef)

formatTableComment :: TableComment -> String
formatTableComment c = List.intercalate " - " [
                         tcWhat c
                       , show (tcWhen c)
                       , tcWho c
                       ]

