{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.PgCatalog.PgSequence
  ( PgSequence (..),
    pgSequenceTable,
    sequencePgClassOidField,
  )
where

import Data.Int (Int64)
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField, oidTypeField)

{- |
  The Haskell representation of data read from the @pg_catalog.pg_sequence@
  table. Rows in this table sequences in PostgreSQL.
-}
data PgSequence = PgSequence
  { -- | The PostgreSQL @oid@ of the @pg_class@ for this sequence
    pgSequenceClassOid :: LibPQ.Oid
  , -- | The PostgreSQL @oid@ of the data type  of the sequence. References
    -- @pg_type.oid@
    pgSequenceTypeOid :: LibPQ.Oid
  , -- | The start value of the sequence
    pgSequenceStart :: Int64
  , -- | The increment value of the sequence
    pgSequenceIncrement :: Int64
  , -- | The max value of the sequence
    pgSequenceMax :: Int64
  , -- | The max value of the sequence
    pgSequenceMin :: Int64
  , -- | The cache size of the sequence
    pgSequenceCache :: Int64
  , -- | Wether the sequence cycles
    pgSequenceCycle :: Bool
  }

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_sequence@ table
-}
pgSequenceTable :: Orville.TableDefinition (Orville.HasKey LibPQ.Oid) PgSequence PgSequence
pgSequenceTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinition
      "pg_sequence"
      (Orville.primaryKey oidField)
      pgSequenceMarshaller

pgSequenceMarshaller :: Orville.SqlMarshaller PgSequence PgSequence
pgSequenceMarshaller =
  PgSequence
    <$> Orville.marshallField pgSequenceClassOid sequencePgClassOidField
    <*> Orville.marshallField pgSequenceTypeOid sequenceTypeOidField
    <*> Orville.marshallField pgSequenceStart sequenceStartField
    <*> Orville.marshallField pgSequenceIncrement sequenceIncrementField
    <*> Orville.marshallField pgSequenceMax sequenceMaxField
    <*> Orville.marshallField pgSequenceMin sequenceMinField
    <*> Orville.marshallField pgSequenceCache sequenceCacheField
    <*> Orville.marshallField pgSequenceCycle sequenceCycleField

{- |
  The @seqrelid@ column of the @pg_cataglog.pg_sequence@ table
-}
sequencePgClassOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
sequencePgClassOidField =
  oidTypeField "seqrelid"

{- |
  The @seqtypid@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceTypeOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
sequenceTypeOidField =
  oidTypeField "seqtypid"

{- |
  The @seqstart@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceStartField :: Orville.FieldDefinition Orville.NotNull Int64
sequenceStartField =
  Orville.bigIntegerField "seqstart"

{- |
  The @seqincrement@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceIncrementField :: Orville.FieldDefinition Orville.NotNull Int64
sequenceIncrementField =
  Orville.bigIntegerField "seqincrement"

{- |
  The @seqmax@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceMaxField :: Orville.FieldDefinition Orville.NotNull Int64
sequenceMaxField =
  Orville.bigIntegerField "seqmax"

{- |
  The @seqmin@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceMinField :: Orville.FieldDefinition Orville.NotNull Int64
sequenceMinField =
  Orville.bigIntegerField "seqmin"

{- |
  The @seqcache@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceCacheField :: Orville.FieldDefinition Orville.NotNull Int64
sequenceCacheField =
  Orville.bigIntegerField "seqcache"

{- |
  The @seqcycle@ column of the @pg_catalog.pg_sequence@ table
-}
sequenceCycleField :: Orville.FieldDefinition Orville.NotNull Bool
sequenceCycleField =
  Orville.booleanField "seqcycle"
