module Test.PgCatalog
  ( pgCatalogTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.Set as Set
import qualified Data.String as String
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

pgCatalogTests :: Pool.Pool Conn.Connection -> Property.Group
pgCatalogTests pool =
  Property.group
    "PgCatalog"
    [ prop_queryPgClass pool
    , prop_queryPgAttribute pool
    , prop_queryPgConstraint pool
    , prop_describeDatabaseRelations pool
    ]

prop_queryPgClass :: Property.NamedDBProperty
prop_queryPgClass =
  Property.singletonNamedDBProperty "Can query the pg_class table to find out about a table" $ \pool -> do
    result <- MIO.liftIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField pgClass)

    fmap PgCatalog.pgClassRelationName result === Just pgClass
    fmap PgCatalog.pgClassRelationKind result === Just PgCatalog.OrdinaryTable

prop_queryPgAttribute :: Property.NamedDBProperty
prop_queryPgAttribute =
  Property.singletonNamedDBProperty "Can query the pg_attribute table to find out about a column" $ \pool -> do
    maybePgClassRecord <- MIO.liftIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField pgClass)

    pgClassOid <-
      case maybePgClassRecord of
        Nothing -> do
          HH.annotate "Expected to find pg_class table, but did not"
          HH.failure
        Just pgClassRecord ->
          pure $ PgCatalog.pgClassOid pgClassRecord

    maybePgAttr <- MIO.liftIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgAttributeTable
        ( Orville.where_ $
            Orville.whereAnd
              ( Orville.fieldEquals PgCatalog.attributeRelationOidField pgClassOid
                  :| [Orville.fieldEquals PgCatalog.attributeNameField relname]
              )
        )

    fmap PgCatalog.pgAttributeName maybePgAttr === Just relname
    fmap PgCatalog.pgAttributeLength maybePgAttr === Just 64

prop_queryPgConstraint :: Property.NamedDBProperty
prop_queryPgConstraint =
  Property.singletonNamedDBProperty "Can query the pg_constraint table to find out about a constraint" $ \pool -> do
    maybePgClassRecord <- MIO.liftIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField fooRelation)

    pgClassOid <-
      case maybePgClassRecord of
        Nothing -> do
          HH.annotate "Expected to find pg_class table, but did not"
          HH.failure
        Just pgClassRecord ->
          pure $ PgCatalog.pgClassOid pgClassRecord

    constraints <- MIO.liftIO . Orville.runOrville pool $ do
      Orville.findEntitiesBy
        PgCatalog.pgConstraintTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.constraintRelationOidField pgClassOid)

    map PgCatalog.pgConstraintType constraints === [PgCatalog.PrimaryKeyConstraint]
    map PgCatalog.pgConstraintKey constraints === [Just [1]]

prop_describeDatabaseRelations :: Property.NamedDBProperty
prop_describeDatabaseRelations =
  Property.singletonNamedDBProperty "Can describe relations from different schemas at once" $ \pool -> do
    let relationsToDescribe =
          [ (pgCatalog, pgNamespace)
          , (pgCatalog, pgClass)
          , (informationSchema, tables)
          ]

    desc <- MIO.liftIO . Orville.runOrville pool $ do
      PgCatalog.describeDatabaseRelations relationsToDescribe

    Map.keysSet (PgCatalog.databaseRelations desc) === Set.fromList relationsToDescribe

    pgNamespaceDescription <-
      case Map.lookup (pgCatalog, pgNamespace) (PgCatalog.databaseRelations desc) of
        Nothing -> do
          HH.annotate "Expected to find pg_namespace table, but did not"
          HH.failure
        Just description ->
          pure description

    Map.keysSet (PgCatalog.relationAttributes pgNamespaceDescription)
      === Set.fromList
        [ String.fromString "cmax"
        , String.fromString "cmin"
        , String.fromString "ctid"
        , String.fromString "nspacl"
        , String.fromString "nspname"
        , String.fromString "nspowner"
        , String.fromString "oid"
        , String.fromString "tableoid"
        , String.fromString "xmax"
        , String.fromString "xmin"
        ]

pgCatalog :: PgCatalog.NamespaceName
pgCatalog =
  String.fromString "pg_catalog"

informationSchema :: PgCatalog.NamespaceName
informationSchema =
  String.fromString "information_schema"

pgClass :: PgCatalog.RelationName
pgClass =
  String.fromString "pg_class"

pgNamespace :: PgCatalog.RelationName
pgNamespace =
  String.fromString "pg_namespace"

tables :: PgCatalog.RelationName
tables =
  String.fromString "tables"

relname :: PgCatalog.AttributeName
relname =
  String.fromString "relname"

fooRelation :: PgCatalog.RelationName
fooRelation =
  String.fromString
    . Orville.tableIdUnqualifiedNameString
    . Orville.tableIdentifier
    $ Foo.table
