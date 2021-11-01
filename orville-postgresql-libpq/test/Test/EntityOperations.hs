module Test.EntityOperations
  ( entityOperationsTests,
  )
where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Connection

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

entityOperationsTests :: Pool.Pool Connection.Connection -> Property.Group
entityOperationsTests pool =
  Property.group "EntityOperations" $
    [
      ( String.fromString "insertEntity/findEntitiesBy forms a round trip"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate

          retrievedFoos <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.findEntitiesBy Foo.table mempty

          retrievedFoos === [originalFoo]
      )
    ,
      ( String.fromString "insertEntities/findFirstEntityBy only return 1"
      , HH.property $ do
          originalFoos <- HH.forAll $ Foo.generateList (Range.linear 0 10)

          HH.cover 1 (String.fromString "empty list") (null originalFoos)
          HH.cover 20 (String.fromString "non-empty list") (not (null originalFoos))

          mbRetrievedFoo <-
            Foo.withTable pool $ do
              mapM_ (Orville.insertEntities Foo.table) (NEL.nonEmpty originalFoos)
              Orville.findFirstEntityBy Foo.table mempty

          let expectedLength =
                case originalFoos of
                  [] -> 0
                  _ -> 1

          -- Once we add order by to 'SelectOptions' we can order by something here
          -- and assert which item is returned.
          length (Maybe.maybeToList mbRetrievedFoo) === expectedLength
      )
    ,
      ( String.fromString "insertEntity/findEntity forms a round trip"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate

          mbRetrievedFoo <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.findEntity Foo.table (Foo.fooId originalFoo)

          mbRetrievedFoo === Just originalFoo
      )
    ,
      ( String.fromString "insertAndReturnEntity returns the inserted entity"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate

          retrievedFoo <-
            Foo.withTable pool $ do
              Orville.insertAndReturnEntity Foo.table originalFoo

          retrievedFoo === originalFoo
      )
    ,
      ( String.fromString "updateEntity updates row at the given key"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate
          newFoo <- HH.forAll Foo.generate

          returnedFoo <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.updateEntity Foo.table (Foo.fooId originalFoo) newFoo
              Orville.findEntitiesBy Foo.table mempty

          returnedFoo === [newFoo]
      )
    ,
      ( String.fromString "updateAndReturnEntity returns the updated entity"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate
          newFoo <- HH.forAll Foo.generate

          mbReturnedFoo <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.updateAndReturnEntity Foo.table (Foo.fooId originalFoo) newFoo

          mbReturnedFoo === Just newFoo
      )
    ,
      ( String.fromString "updateEntity updates no rows when key does not match"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate
          newFoo <- HH.forAll Foo.generate

          let mismatchFooId =
                1 + Foo.fooId originalFoo

          retrievedFoos <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.updateEntity Foo.table mismatchFooId newFoo
              Orville.findEntitiesBy Foo.table mempty

          retrievedFoos === [originalFoo]
      )
    ,
      ( String.fromString "updateAndReturnEntity returns Nothing when key does not match"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate
          newFoo <- HH.forAll Foo.generate

          let mismatchFooId =
                1 + Foo.fooId originalFoo

          mbReturnedFoo <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.updateAndReturnEntity Foo.table mismatchFooId newFoo

          mbReturnedFoo === Nothing
      )
    ,
      ( String.fromString "deleteEntity deletes row at the given key"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate
          let withDifferentKey = Gen.filter $ (Foo.fooId originalFoo /=) . Foo.fooId
          anotherFoo <- HH.forAll . withDifferentKey $ Foo.generate

          retrievedFoos <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.insertEntity Foo.table anotherFoo
              Orville.deleteEntity Foo.table (Foo.fooId originalFoo)
              Orville.findEntitiesBy Foo.table mempty

          retrievedFoos === [anotherFoo]
      )
    ,
      ( String.fromString "deleteAndReturnEntity returns the deleted row"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate
          mbReturnedFoo <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.deleteAndReturnEntity Foo.table (Foo.fooId originalFoo)

          mbReturnedFoo === Just originalFoo
      )
    ,
      ( String.fromString "deleteEntity deletes no rows when key doesn't match"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate

          let mismatchFooId =
                1 + Foo.fooId originalFoo

          retrievedFoos <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.deleteEntity Foo.table mismatchFooId
              Orville.findEntitiesBy Foo.table mempty

          retrievedFoos === [originalFoo]
      )
    ,
      ( String.fromString "deleteAndReturnEntity returns Nothing when key doesn't match"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate

          let mismatchFooId =
                1 + Foo.fooId originalFoo

          mbReturnedFoo <-
            Foo.withTable pool $ do
              Orville.insertEntity Foo.table originalFoo
              Orville.deleteAndReturnEntity Foo.table mismatchFooId

          mbReturnedFoo === Nothing
      )
    ]
