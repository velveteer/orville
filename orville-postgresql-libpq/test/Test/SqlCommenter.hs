module Test.SqlCommenter (sqlCommenterTests) where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlCommenter as SqlCommenter

import Test.Expr.TestSchema
  ( assertEqualFooBarRows, -- barColumn,
    dropAndRecreateTestTable,
    findAllFooBars,
    fooBarTable, -- fooColumn,
    insertFooBarSource,
    mkFooBar,
  )
import qualified Test.Property as Property

sqlCommenterTests :: Pool.Pool Conn.Connection -> Property.Group
sqlCommenterTests pool =
  Property.group
    "SqlCommenter"
    [ prop_sqlcommenterEscaped
    , prop_sqlCommenterInsertExpr pool
    ]

prop_sqlcommenterEscaped :: Property.NamedProperty
prop_sqlcommenterEscaped =
  Property.singletonNamedProperty "SqlCommenter is escaped and put at end of raw sql" $ do
    let rawSql :: RawSql.RawSql
        rawSql =
          SqlCommenter.addComment staticSqlCommenter $
            RawSql.fromString "SELECT * "
              <> RawSql.fromString "FROM foo "
              <> RawSql.fromString "WHERE id = 1"

        expectedBytes =
          B8.pack "SELECT * FROM foo WHERE id = 1/*key='value',keyForEscapedValue='queryParam%3Dfoo%20bar%2Fbaz-fizz',keyWith%27InIt='valueWith%27InIt',orm='orville'*/"

        (actualBytes, actualParams) =
          runIdentity $
            RawSql.toBytesAndParams RawSql.exampleEscaping rawSql

    actualBytes HH.=== expectedBytes
    actualParams HH.=== []

prop_sqlCommenterInsertExpr :: Property.NamedDBProperty
prop_sqlCommenterInsertExpr =
  Property.singletonNamedDBProperty "sqlcommenter support does not impact ability of insertExpr inserting values" $ \pool -> do
    let fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

    rows <-
      MIO.liftIO $
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          let insertExpr = Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing
          RawSql.executeVoid connection $
            SqlCommenter.addComment staticSqlCommenter insertExpr

          result <- RawSql.execute connection findAllFooBars

          ExecResult.readRows result

    assertEqualFooBarRows rows fooBars

staticSqlCommenter :: SqlCommenter.SqlCommenter
staticSqlCommenter =
  fmap T.pack
    . Map.mapKeys T.pack
    $ Map.fromList
      [ ("orm", "orville")
      , ("key", "value")
      , ("keyForEscapedValue", "queryParam=foo bar/baz-fizz")
      , ("keyWith'InIt", "valueWith'InIt")
      ]
