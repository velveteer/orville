{- |
Module    : Orville.PostgreSQL.SqlType
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.SqlType
  ( SqlType
      ( SqlType,
        sqlTypeExpr,
        sqlTypeReferenceExpr,
        sqlTypeOid,
        sqlTypeMaximumLength,
        sqlTypeToSql,
        sqlTypeFromSql
      ),
    -- numeric types
    integer,
    serial,
    bigInteger,
    bigSerial,
    smallInteger,
    double,
    -- textual-ish types
    boolean,
    unboundedText,
    fixedText,
    boundedText,
    textSearchVector,
    -- date types
    date,
    timestamp,
    timestampWithoutZone,
    -- postgresql types
    oid,
    -- type conversions
    foreignRefType,
    convertSqlType,
    maybeConvertSqlType,
  )
where

import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Foreign.C.Types as CTypes

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  SqlType defines the mapping of a Haskell type (`a`) to a SQL column type in the
  database. This includes both how to convert the type to and from the raw values
  read from the database as well as the schema information required to create
  and migrate columns using the type.
-}
data SqlType a = SqlType
  { -- | The sql data type expression to use when creating/migrating columns of
    -- this type
    sqlTypeExpr :: Expr.DataType
  , -- | The sql data type experession to use when creating/migrating columns
    -- with foreign keys to this type. This is used foreignRefType to build a
    -- new SqlType when making foreign key fields
    sqlTypeReferenceExpr :: Maybe Expr.DataType
  , -- | The Oid for the type in postgresql. This will be used during
    -- migrations to determine whether the column type needs to be altered.
    sqlTypeOid :: LibPQ.Oid
  , -- | The maximum length for lengths that take a type parameter (such as
    -- @char@ and @varchar@).  This will be used during migration to determine
    -- whether the column type needs to be altered.
    sqlTypeMaximumLength :: Maybe Int32
  , -- | A function for converting Haskell values of this type into values to
    -- be stored in the database.
    sqlTypeToSql :: a -> SqlValue
  , -- | A function for converting values of this are stored in the database
    -- into Haskell values. This function should return 'Nothing' to indicate
    -- an error if the conversion is impossible. Otherwise it should return
    -- 'Just' the corresponding 'a' value.
    sqlTypeFromSql :: SqlValue -> Maybe a
  }

{- |
  'integer' defines a 32-bit integer type. This corresponds to the "INTEGER" type in SQL.
-}
integer :: SqlType Int32
integer =
  SqlType
    { sqlTypeExpr = Expr.int
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 23
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromInt32
    , sqlTypeFromSql = SqlValue.toInt32
    }

{- |
  'serial' defines a 32-bit auto-incrementing column type. This corresponds to
  the "SERIAL" type in PostgreSQL.
-}
serial :: SqlType Int32
serial =
  SqlType
    { sqlTypeExpr = Expr.serial
    , sqlTypeReferenceExpr = Just Expr.int
    , sqlTypeOid = LibPQ.Oid 23
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromInt32
    , sqlTypeFromSql = SqlValue.toInt32
    }

{- |
  'bigInteger' defines a 64-bit integer type. This corresponds to the "BIGINT"
  type in SQL.
-}
bigInteger :: SqlType Int64
bigInteger =
  SqlType
    { sqlTypeExpr = Expr.bigInt
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 20
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromInt64
    , sqlTypeFromSql = SqlValue.toInt64
    }

{- |
  'bigSerial' defines a 64-bit auto-incrementing column type. This corresponds to
  the "BIGSERIAL" type in PostgresSQL.
-}
bigSerial :: SqlType Int64
bigSerial =
  SqlType
    { sqlTypeExpr = Expr.bigSerial
    , sqlTypeReferenceExpr = Just Expr.bigInt
    , sqlTypeOid = LibPQ.Oid 20
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromInt64
    , sqlTypeFromSql = SqlValue.toInt64
    }

{- |
  'smallInteger' defines a 16-bit integer type. This corresponds to the "SMALLINT" type in SQL.
-}
smallInteger :: SqlType Int16
smallInteger =
  SqlType
    { sqlTypeExpr = Expr.smallint
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 21
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromInt16
    , sqlTypeFromSql = SqlValue.toInt16
    }

{- |
  'double' defines a floating point numeric type. This corresponds to the "DOUBLE
  PRECISION" type in SQL.
-}
double :: SqlType Double
double =
  SqlType
    { sqlTypeExpr = Expr.doublePrecision
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 701
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromDouble
    , sqlTypeFromSql = SqlValue.toDouble
    }

{- |
  'boolean' defines a True/False boolean type. This corresponds to the "BOOLEAN"
  type in SQL.
-}
boolean :: SqlType Bool
boolean =
  SqlType
    { sqlTypeExpr = Expr.boolean
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 16
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromBool
    , sqlTypeFromSql = SqlValue.toBool
    }

{- |
  'unboundedText' defines a unbounded length text field type. This corresponds to a
  "TEXT" type in PostgreSQL.
-}
unboundedText :: SqlType Text
unboundedText =
  SqlType
    { sqlTypeExpr = Expr.text
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 25
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromText
    , sqlTypeFromSql = SqlValue.toText
    }

{- |
  'fixedText' defines a fixed length text field type. This corresponds to a
  "CHAR(len)" type in PostgreSQL.
-}
fixedText :: Int32 -> SqlType Text
fixedText len =
  SqlType
    { sqlTypeExpr = Expr.char len
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 1042
    , sqlTypeMaximumLength = Just len
    , sqlTypeToSql = SqlValue.fromText
    , sqlTypeFromSql = SqlValue.toText
    }

{- |
  'boundedText' defines a variable length text field type. This corresponds to a
  "VARCHAR(len)" type in PostgreSQL.
-}
boundedText :: Int32 -> SqlType Text
boundedText len =
  SqlType
    { sqlTypeExpr = Expr.varchar len
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 1043
    , sqlTypeMaximumLength = Just len
    , sqlTypeToSql = SqlValue.fromText
    , sqlTypeFromSql = SqlValue.toText
    }

{- |
  'textSearchVector' defines a type for indexed text searching. It corresponds to the
  "TSVECTOR" type in PostgreSQL.
-}
textSearchVector :: SqlType Text
textSearchVector =
  SqlType
    { sqlTypeExpr = Expr.tsvector
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 3614
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromText
    , sqlTypeFromSql = SqlValue.toText
    }

{- |
  'date' defines a type representing a calendar date (without time zone). It corresponds
  to the "DATE" type in SQL.
-}
date :: SqlType Time.Day
date =
  SqlType
    { sqlTypeExpr = Expr.date
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 1082
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromDay
    , sqlTypeFromSql = SqlValue.toDay
    }

{- |
  'timestamp' defines a type representing a particular point in time without time zone information,
  but can be constructed with a time zone offset.
  It corresponds to the "TIMESTAMP with time zone" type in SQL.

  Note: This is NOT a typo. The "TIMESTAMP with time zone" type in SQL does not include
  any actual time zone information. For an excellent explanation of the complexities
  involving this type, please see Chris Clark's blog post about it:
  http://blog.untrod.com/2016/08/actually-understanding-timezones-in-postgresql.html
-}
timestamp :: SqlType Time.UTCTime
timestamp =
  SqlType
    { sqlTypeExpr = Expr.timestampWithZone
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 1184
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromUTCTime
    , sqlTypeFromSql = SqlValue.toUTCTime
    }

{- |
  'timestampWithoutZone' defines a type representing a particular point in time (without time zone).
  It corresponds to the "TIMESTAMP without time zone" type in SQL.

  http://blog.untrod.com/2016/08/actually-understanding-timezones-in-postgresql.html
-}
timestampWithoutZone :: SqlType Time.LocalTime
timestampWithoutZone =
  SqlType
    { sqlTypeExpr = Expr.timestampWithoutZone
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 1114
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = SqlValue.fromLocalTime
    , sqlTypeFromSql = SqlValue.toLocalTime
    }

{- |
  'oid' corresponds to the type used in PostgreSQL for identifying system
  objects
-}
oid :: SqlType LibPQ.Oid
oid =
  SqlType
    { sqlTypeExpr = Expr.oid
    , sqlTypeReferenceExpr = Nothing
    , sqlTypeOid = LibPQ.Oid 26
    , sqlTypeMaximumLength = Nothing
    , sqlTypeToSql = \(LibPQ.Oid (CTypes.CUInt word)) -> SqlValue.fromWord32 word
    , sqlTypeFromSql = fmap (LibPQ.Oid . CTypes.CUInt) . SqlValue.toWord32
    }

{- |
  'foreignRefType' creates a 'SqlType' suitable for columns will be foreign
  keys referencing a column of the given 'SqlType'. For most types the
  underlying sql type with be identical, but for special types (such as
  autoincrementing primary keys), the type construted by 'foreignRefType' with
  have regular underlying sql type. Each 'SqlType' definition must specify any
  special handling required when creating foreign reference types by setting
  the 'sqlTypeReferenceExpr' field to an appropriate value.
-}
foreignRefType :: SqlType a -> SqlType a
foreignRefType sqlType =
  case sqlTypeReferenceExpr sqlType of
    Nothing -> sqlType
    Just refExpr -> sqlType {sqlTypeExpr = refExpr, sqlTypeReferenceExpr = Nothing}

{- |
  'maybeConvertSqlType' changes the Haskell type used by a 'SqlType' which changing
  the column type that will be used in the database schema. The functions given
  will be used to convert the now Haskell type to and from the original type when
  reading and writing values from the database. When reading an 'a' value from
  the database, the conversion function should produce 'Nothing' if the value
  cannot be successfully converted to a 'b'
-}
maybeConvertSqlType :: (b -> a) -> (a -> Maybe b) -> SqlType a -> SqlType b
maybeConvertSqlType bToA aToB sqlType =
  sqlType
    { sqlTypeToSql = sqlTypeToSql sqlType . bToA
    , sqlTypeFromSql = \sql -> do
        a <- sqlTypeFromSql sqlType sql
        aToB a
    }

{- |
  'convertSqlType' changes the Haskell type used by a 'SqlType' in the same manner
  as 'maybeConvertSqlType' in cases where an 'a' can always be converted to a 'b'.
-}
convertSqlType :: (b -> a) -> (a -> b) -> SqlType a -> SqlType b
convertSqlType bToA aToB =
  maybeConvertSqlType bToA (Just . aToB)
