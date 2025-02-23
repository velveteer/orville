module Test.Cursor
  ( cursorTests,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.Cursor as Cursor
import qualified Orville.PostgreSQL.Internal.Select as Select

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

cursorTests :: Pool.Pool Conn.Connection -> Property.Group
cursorTests pool =
  Property.group "Cursor" $
    [ prop_withCursorFetch pool
    , prop_withCursorMove pool
    ]

prop_withCursorFetch :: Property.NamedDBProperty
prop_withCursorFetch =
  Property.namedDBProperty "withCursor - fetch" $ \pool -> do
    foos <- HH.forAll (Foo.generateNonEmpty (Range.linear 1 20))
    -- A fetch count of 0 has the special meaning of "current row" in
    -- PostgreSQL so we avoid generating 0 for the fetch count in this test
    numToFetch <- HH.forAll (Gen.integral (Range.linear 1 (length foos)))

    let expectedRows =
          take numToFetch
            . List.sortBy (Ord.comparing Foo.fooId)
            . NEL.toList
            $ foos

    actualRows <-
      Foo.withTable pool $ do
        Orville.withTransaction $ do
          _ <- Orville.insertEntities Foo.table foos
          Cursor.withCursor Nothing Nothing selectAllFoosOrderedById $ \cursor ->
            Cursor.fetch (Just $ Cursor.forwardCount numToFetch) cursor

    expectedRows === actualRows

prop_withCursorMove :: Property.NamedDBProperty
prop_withCursorMove =
  Property.namedDBProperty "withCursor - move" $ \pool -> do
    foos <- HH.forAll (Foo.generateNonEmpty (Range.linear 1 20))
    -- A fetch count of 0 has the special meaning of "current row" in
    -- PostgreSQL so we avoid generating any scenario that would give us a
    -- fetch count of 0 in this test
    numToSkip <- HH.forAll (Gen.integral (Range.linear 0 (length foos - 1)))
    numToFetch <- HH.forAll (Gen.integral (Range.linear 1 (length foos - numToSkip)))

    let expectedRows =
          take numToFetch
            . drop numToSkip
            . List.sortBy (Ord.comparing Foo.fooId)
            . NEL.toList
            $ foos

    actualRows <-
      Foo.withTable pool $ do
        Orville.withTransaction $ do
          _ <- Orville.insertEntities Foo.table foos
          Cursor.withCursor Nothing Nothing selectAllFoosOrderedById $ \cursor -> do
            Cursor.move (Just $ Cursor.forwardCount numToSkip) cursor
            Cursor.fetch (Just $ Cursor.forwardCount numToFetch) cursor

    expectedRows === actualRows

selectAllFoosOrderedById :: Select.Select Foo.Foo
selectAllFoosOrderedById =
  Select.selectTable
    Foo.table
    (Orville.orderBy $ Orville.orderByField Foo.fooIdField Orville.ascendingOrder)
