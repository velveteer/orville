{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orville.PostgreSQL.Plan.Operation
  ( Operation (..),
    AssertionFailed,
    mkAssertionFailed,
    WherePlanner (..),
    byField,
    byFieldTuple,
    findOne,
    findOneWhere,
    findAll,
    findAllWhere,
    findSelect,
    askParam,
    assertRight,
    SelectOperation (..),
    selectOperation,
  )
where

import Control.Exception (Exception)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Foldable as Fold
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.Select (Select)
import qualified Orville.PostgreSQL.Internal.Select as Select
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDefinition
import qualified Orville.PostgreSQL.Plan.Explanation as Exp
import Orville.PostgreSQL.Plan.Many (Many)
import qualified Orville.PostgreSQL.Plan.Many as Many

{- |
  'Operation' provides a stucture for building primitive operations that can be
  incorporated into a 'Database.Orville.PostgreSQL.Plan.Plan'. An 'Operation'
  provides base case implementations of the various plan execution functions.
  You only need to care about this type if you want to create new custom
  operations to include in a 'Database.Orville.PostgreSQL.Plan.Plan' beyond
  those already provided in the 'Database.Orville.PostgreSQL.Plan.Plan'
  api.

  You can build your own custom 'Operation' values either directly, or using
  the function and types in this module, such as 'WherePlanner' (via 'findAll',
  etc), or 'SelectOperation' (via 'selectOperation').
-}
data Operation param result = Operation
  { -- | 'executeOperationOne' will be called when an plan is
    -- executed with a single input parameter
    executeOperationOne ::
      forall m.
      (MonadOrville.MonadOrville m) =>
      param ->
      m (Either AssertionFailed result)
  , -- | 'executeOperationMany' will be called when an plan is
    -- executed with multiple input parameters (via 'planMany').
    executeOperationMany ::
      forall m.
      (MonadOrville.MonadOrville m) =>
      NonEmpty param ->
      m (Either AssertionFailed (Many param result))
  , -- | 'explainOperationOne' will be called when producing an explanation
    -- of what the plan will do when given one input parameter. Plans that do
    -- not perform any interesting IO interactions should generally return an
    -- empty explanation.
    explainOperationOne :: Exp.Explanation
  , -- | 'explainOperationMany' will be called when producing an explanation
    -- of what the plan will do when given multiple input parameters (via
    -- 'planMany'). Plans that do not perform any interesting IO interactions
    -- should generally return an empty explanation.
    explainOperationMany :: Exp.Explanation
  }

{- |
  'AssertionFailed' may be returned from the execute functions of an
  'Operation' to indicate that some expected invariant has failed. For example,
  following a foreign key that is enforced by the database only to find that no
  record exists. When an 'Operation' returns an 'AssertionFailed' value during
  plan execution the error is thrown as an exception using the
  'Control.Monad.Catch.MonadThrow' instance for whatever monad the plan is
  executing in.
-}
newtype AssertionFailed
  = AssertionFailed String
  deriving (Show)

{- |
  'mkAssertionFailed' builds an 'AssertionFailed' error from an error message.
-}
mkAssertionFailed :: String -> AssertionFailed
mkAssertionFailed =
  AssertionFailed

instance Exception AssertionFailed

{- |
  'askParam' simply returns the paremeter given from the plan.
-}
askParam :: Operation param param
askParam =
  Operation
    { executeOperationOne = pure . Right
    , executeOperationMany = \params -> pure . Right $ Many.fromKeys (Fold.toList params) Right
    , explainOperationOne = Exp.noExplanation
    , explainOperationMany = Exp.noExplanation
    }

{- |
  'assertRight' returns the value on the 'Right' side of an 'Either'. If
  the 'Either' is a 'Left', it raises 'AssertionFailed' with the message
  from the left side of the either.
-}
assertRight :: Operation (Either String a) a
assertRight =
  Operation
    { executeOperationOne = \eitherA ->
        pure $
          case eitherA of
            Left err ->
              Left (AssertionFailed $ "Assertion failed: " <> err)
            Right b ->
              Right b
    , executeOperationMany = \eitherAs ->
        pure $
          case sequence eitherAs of
            Left err ->
              Left (AssertionFailed $ "Assertion failed: " <> err)
            Right _ ->
              let errorOnLeft :: forall a b. Either a b -> Either Many.NotAKey b
                  errorOnLeft eitherA =
                    case eitherA of
                      Left _ ->
                        -- We proved all the values above didn't have any lefts in
                        -- then, so if we get a left here it must not be one of the
                        -- keys from the Many.
                        Left Many.NotAKey
                      Right a ->
                        Right a
               in Right $ Many.fromKeys (Fold.toList eitherAs) errorOnLeft
    , explainOperationOne = Exp.noExplanation
    , explainOperationMany = Exp.noExplanation
    }

{- |
  The functions below ('findOne', 'findAll', etc) accept a 'WherePlanner'
  to determine how to build the where conditions for executing a 'Select'
  statement as part of a the plan operation.

  For simple queries you can use the functions such as 'byField' that are
  provided here to build a 'WherePlanner', but you may also build your own
  custom 'WherePlanner' for more advanced use cases.

  If you need to execute a custom query that cannot be build by providing
  custom where clause via 'WherePlanner', you may want to use more
  direct 'selectOperation' function.
-}
data WherePlanner param = WherePlanner
  { -- | The 'paramMarshaller' function provided here will be used to decode
    -- the parameter field from the result set so that the row can be properly
    -- associated with the input parameter that matched it.
    paramMarshaller :: forall entity. (entity -> param) -> SqlMarshaller.SqlMarshaller entity param
  , -- | 'executeOneWhereCondition' must build a where condition that will
    -- match only those rows that match the input paramater.
    executeOneWhereCondition :: param -> SelectOptions.WhereCondition
  , -- | 'executeManyWhereCondition' must build a where condition that will
    -- match only those rows that match any (not all!) of the input parameters.
    executeManyWhereCondition :: NonEmpty param -> SelectOptions.WhereCondition
  , -- | 'explainOneWhereCondition' must build a where condition that is
    -- suitable to be used as an example of 'executeManyWhereCondition' would
    -- return when given a parameter.  This where condition will be used for
    -- when producing explanations of plans. For example, this could fill in
    -- either an example or dummy value.
    explainOneWhereCondition :: SelectOptions.WhereCondition
  , -- | 'explainManyWhereCondition' must build a where condition that is
    -- suitable to be used as an example of 'executeOneWhereCondition' would
    -- return when given a list of parameters.  This where condition will be
    -- used for when producing explanations of plans. For example, this could
    -- fill in either an example or dummy value.
    explainManyWhereCondition :: SelectOptions.WhereCondition
  }

{- |
  Builds a 'WherePlanner' that will match on a single
  'FieldDefinition.FieldDefinition'.  The resulting 'WherePlanner' can be used
  with functions such as 'findOne' and 'findAll' to construct an 'Operation'.
-}
byField ::
  FieldDefinition.FieldDefinition nullability fieldValue ->
  WherePlanner fieldValue
byField fieldDef =
  let stringyField =
        stringifyField fieldDef
   in WherePlanner
        { paramMarshaller = flip SqlMarshaller.marshallField fieldDef
        , executeOneWhereCondition = \fieldValue -> SelectOptions.fieldEquals fieldDef fieldValue
        , executeManyWhereCondition = \fieldValues -> SelectOptions.fieldIn fieldDef fieldValues
        , explainOneWhereCondition = SelectOptions.fieldEquals stringyField $ T.pack "EXAMPLE VALUE"
        , explainManyWhereCondition = SelectOptions.fieldIn stringyField $ fmap T.pack ("EXAMPLE VALUE 1" :| ["EXAMPLE VALUE 2"])
        }

{- |
  Builds a 'WherePlanner' that will match on a 2-tuple of
  'FieldDefinition.FieldDefinition's.  The resulting 'WherePlanner' can be used
  with functions such as 'findOne' and 'findAll' to construct an 'Operation'.
-}
byFieldTuple ::
  forall nullabilityA fieldValueA nullabilityB fieldValueB.
  FieldDefinition.FieldDefinition nullabilityA fieldValueA ->
  FieldDefinition.FieldDefinition nullabilityB fieldValueB ->
  WherePlanner (fieldValueA, fieldValueB)
byFieldTuple fieldDefA fieldDefB =
  let stringyFieldA =
        stringifyField fieldDefA

      stringyFieldB =
        stringifyField fieldDefB

      marshaller ::
        (a -> (fieldValueA, fieldValueB)) ->
        SqlMarshaller.SqlMarshaller a (fieldValueA, fieldValueB)
      marshaller accessor =
        (,)
          <$> SqlMarshaller.marshallField (fst . accessor) fieldDefA
          <*> SqlMarshaller.marshallField (snd . accessor) fieldDefB

      packAll =
        fmap (\(a, b) -> (T.pack a, T.pack b))
   in WherePlanner
        { paramMarshaller = marshaller
        , executeOneWhereCondition = \fieldValue -> SelectOptions.fieldTupleIn fieldDefA fieldDefB (fieldValue :| [])
        , executeManyWhereCondition = \fieldValues -> SelectOptions.fieldTupleIn fieldDefA fieldDefB fieldValues
        , explainOneWhereCondition =
            SelectOptions.fieldTupleIn
              stringyFieldA
              stringyFieldB
              (packAll $ ("EXAMPLE VALUE A", "EXAMPLE VALUE B") :| [])
        , explainManyWhereCondition =
            SelectOptions.fieldTupleIn
              stringyFieldA
              stringyFieldB
              (packAll $ (("EXAMPLE VALUE A 1", "EXAMPLE VALUE B 1") :| [("EXAMPLE VALUE A 2", "EXAMPLE VALUE B 2")]))
        }

{- |
  'findOne' builds a planning primitive that finds (at most) one row from the
  given table where the column value for the provided 'Core.FieldDefinition' matches
  the plan's input parameter. When executed on multiple parameters it fetches
  all rows where the field matches the inputs and arbitrarily picks at most one
  of those rows to use as the result for each input.
-}
findOne ::
  Ord param =>
  TableDefinition.TableDefinition key writeEntity readEntity ->
  WherePlanner param ->
  Operation param (Maybe readEntity)
findOne tableDef wherePlanner =
  findOneWithOpts tableDef wherePlanner mempty

{- |
  'findOneWhere' is similar to 'findOne' but allows a 'WhereCondition' to be
  specified that is added to the database query to restrict which rows are
  returned.
-}
findOneWhere ::
  Ord param =>
  TableDefinition.TableDefinition key writeEntity readEntity ->
  WherePlanner param ->
  SelectOptions.WhereCondition ->
  Operation param (Maybe readEntity)
findOneWhere tableDef wherePlanner cond =
  findOneWithOpts tableDef wherePlanner (SelectOptions.where_ cond)

{- |
  'findOneWithOpts' is a internal helper used by 'findOne' and 'findOneWhere'
-}
findOneWithOpts ::
  Ord param =>
  TableDefinition.TableDefinition key writeEntity readEntity ->
  WherePlanner param ->
  SelectOptions.SelectOptions ->
  Operation param (Maybe readEntity)
findOneWithOpts tableDef wherePlanner opts =
  selectOperation selectOp
  where
    selectOp =
      SelectOperation
        { selectOne = \param ->
            select (opts <> SelectOptions.where_ (executeOneWhereCondition wherePlanner param) <> SelectOptions.limit 1)
        , selectMany = \params ->
            select (opts <> SelectOptions.where_ (executeManyWhereCondition wherePlanner params))
        , explainSelectOne =
            select (opts <> SelectOptions.where_ (explainOneWhereCondition wherePlanner))
        , explainSelectMany =
            select (opts <> SelectOptions.where_ (explainManyWhereCondition wherePlanner))
        , categorizeRow = fst
        , produceResult = fmap snd . Maybe.listToMaybe
        }

    select =
      Select.selectMarshalledColumns
        marshaller
        (TableDefinition.tableName tableDef)

    marshaller =
      SqlMarshaller.mapSqlMarshaller
        ( \m ->
            (,)
              <$> paramMarshaller wherePlanner fst
              <*> SqlMarshaller.marshallNested snd m
        )
        (TableDefinition.tableMarshaller tableDef)

{- |
  'findAll' builds a planning primitive that finds all the rows from the
  given table where the column value for the provided field matches the
  plan's input parameter. Where executed on multiple parameters all rows
  are fetch in a single query and then associated with their respective
  inputs after being fetched.
-}
findAll ::
  Ord param =>
  TableDefinition.TableDefinition key writeEntity readEntity ->
  WherePlanner param ->
  Operation param [readEntity]
findAll tableDef wherePlanner =
  findAllWithOpts tableDef wherePlanner mempty

{- |
  'findAllWhere' is similar to 'findAll' but allows a 'WhereCondition' to be
  specified that is added to the database query to restrict which rows are
  returned.
-}
findAllWhere ::
  Ord param =>
  TableDefinition.TableDefinition key writeEntity readEntity ->
  WherePlanner param ->
  SelectOptions.WhereCondition ->
  Operation param [readEntity]
findAllWhere tableDef wherePlanner cond =
  findAllWithOpts tableDef wherePlanner (SelectOptions.where_ cond)

{- |
  'findAllWithOpts' is an internal helper used by 'findAll' and 'findAllWhere'
-}
findAllWithOpts ::
  Ord param =>
  TableDefinition.TableDefinition key writeEntity readEntity ->
  WherePlanner param ->
  SelectOptions.SelectOptions ->
  Operation param [readEntity]
findAllWithOpts tableDef wherePlanner opts =
  selectOperation selectOp
  where
    selectOp =
      SelectOperation
        { selectOne = \param ->
            select (opts <> SelectOptions.where_ (executeOneWhereCondition wherePlanner param))
        , selectMany = \params ->
            select (opts <> SelectOptions.where_ (executeManyWhereCondition wherePlanner params))
        , explainSelectOne =
            select (opts <> SelectOptions.where_ (explainOneWhereCondition wherePlanner))
        , explainSelectMany =
            select (opts <> SelectOptions.where_ (explainManyWhereCondition wherePlanner))
        , categorizeRow = fst
        , produceResult = map snd
        }

    select =
      Select.selectMarshalledColumns
        marshaller
        (TableDefinition.tableName tableDef)

    marshaller =
      SqlMarshaller.mapSqlMarshaller
        ( \m ->
            (,)
              <$> paramMarshaller wherePlanner fst
              <*> SqlMarshaller.marshallNested snd m
        )
        (TableDefinition.tableMarshaller tableDef)

{- |
  'stringifyField' arbitrarily re-labels the 'SqlType' of a field definition
  as text. It is an internal helper function that is used for constructing
  'WhereCondition' clauses used to generate sql when explaining how a plan
  will be executed. Relabeling the type as 'T.Text' allows us to use text
  values as example inputs in the queries when for explaining plans.
-}
stringifyField ::
  FieldDefinition.FieldDefinition nullability a ->
  FieldDefinition.FieldDefinition nullability T.Text
stringifyField =
  FieldDefinition.convertField (const SqlType.unboundedText)

{- |
  'SelectOperation' is a helper type for building 'Operation' primitives that
  run 'Select' queries. Specifying the fields of 'SelectOperation' and then
  using the 'selectOperation' function to build an 'Operation' is more
  convenient that building functions to execute the queries thate are required
  by the 'Operation' type.

  Note: If you only need to build a custom where clause based on the
  'Operation' parameter, you may want to use a custom 'WherePlanner' with one
  of the existing 'findOne' or 'findAll' functions.

  If you cannot respresent your custom operation using 'SelectOperation' then
  you need build the 'Operation' value directly yourself.
-}
data SelectOperation param row result = SelectOperation
  { -- | 'selectOne' will be called to build the 'Select' query that should
    -- be run when there is a single input parameter while executing a plan.
    -- Note that the "One-ness" here refers to the single input parameter
    -- rather than result. See 'produceResult' below for more information
    -- about returning one values vs. many from a 'SelectOperation'.
    selectOne :: param -> Select row
  , -- | 'selectMany' will be called to build the 'Select' query that should
    -- be run when there are multiple parameters while executing a plan.
    -- Note that the "Many-ness" here refers to the multiple input parameters
    -- rather than result. See 'produceResult' below for more information
    -- about returning one values vs. many from a 'SelectOperation'.
    selectMany :: NonEmpty param -> Select row
  , -- | 'explainSelectOne' should show a representative query of what will
    -- be returned when 'selectOne' is used. No input parameter is available
    -- here to build the query, however, because this value is used to
    -- explain a plan without actually running it.
    explainSelectOne :: Select row
  , -- | 'explainSelectMany' should show a representative query of what will
    -- be returned when 'selectMany is used. No input parameters are available
    -- here to build the query, however, because this value is used to
    -- explain a plan without actually running it.
    explainSelectMany :: Select row
  , -- | 'categorizeRow' will be used when a plan is executed with multiple
    -- parameters to determine which input parameter the row should be
    -- associated with.
    categorizeRow :: row -> param
  , -- | 'produceResult' will be used convert the 'row' type returned by the
    -- 'Select' queries for the operation input the 'result' type that is
    -- present as the output of the operation. The input rows will be all the
    -- inputs associated with a single parameter. The 'result' type
    -- constructed here need not be a single value. For instance, 'findAll'
    -- uses the list type as the 'result' type and 'findOne' uses 'Maybe'.
    produceResult :: [row] -> result
  }

{- |
  'selectOperation' builds a primitive planning 'Operation' using the functions
  given by a 'SelectOperation'. If you are implementing a custom operation that
  runs a select statement, it is probably easier to use this function rather
  than building the 'Operation' functions directly.
-}
selectOperation ::
  Ord param =>
  SelectOperation param row result ->
  Operation param result
selectOperation selectOp =
  Operation
    { executeOperationOne = executeSelectOne selectOp
    , executeOperationMany = executeSelectMany selectOp
    , explainOperationOne = explainSelect $ explainSelectOne selectOp
    , explainOperationMany = explainSelect $ explainSelectMany selectOp
    }

explainSelect :: Select row -> Exp.Explanation
explainSelect =
  Exp.explainStep . BS8.unpack . RawSql.toExampleBytes . Select.selectToQueryExpr

{- |
  'runSelectOne' is an internal helper function that executes a
  'SelectOperation' on a single input parameter.
-}
executeSelectOne ::
  MonadOrville.MonadOrville m =>
  SelectOperation param row result ->
  param ->
  m (Either AssertionFailed result)
executeSelectOne selectOp param =
  Right . produceResult selectOp
    <$> (Select.executeSelect . selectOne selectOp $ param)

{- |
  'executeSelectMany' is an internal helper function that executes a
  'SelectOperation' on multiple input parameters.
-}
executeSelectMany ::
  forall param row result m.
  (Ord param, MonadOrville.MonadOrville m) =>
  SelectOperation param row result ->
  NonEmpty param ->
  m (Either AssertionFailed (Many param result))
executeSelectMany selectOp params = do
  rows <- Select.executeSelect . selectMany selectOp $ params

  let -- Seed add initial map with an empty list for every input parameter
      -- to guarantee that each param is a key in the map even if no rows
      -- where returned from the select query for that param.
      emptyRowsMap :: Map.Map param [a]
      emptyRowsMap =
        Map.fromList
          . map (\param -> (param, []))
          . Fold.toList
          $ params

      insertRow results row =
        Map.alter
          (\mbRows -> Just (row : Maybe.fromMaybe [] mbRows))
          (categorizeRow selectOp row)
          results

      rowMap =
        produceResult selectOp <$> Fold.foldl' insertRow emptyRowsMap rows

      manyRows =
        Many.fromKeys (Fold.toList params) $ \param ->
          case Map.lookup param rowMap of
            Nothing ->
              -- Because we seeded the map above with all the input parameters we
              -- can be sure that if we don't find a value in the map here it is
              -- because the function parameter is not one of the original inputs
              -- rather than just an input for which no rows were returned by the
              -- select query.
              Left Many.NotAKey
            Just row ->
              Right row

  pure . Right $ manyRows

{- |
  'findSelect' builds a plan 'Operation' where the select that is run does not
  use the input parameters for the plan in any way. If the
  'executeOperationMany' function of the resulting 'Operation' will run the
  query once and use the entire result set as the result each of the input
  parameters in turn.
-}
findSelect :: forall param row. Select.Select row -> Operation param [row]
findSelect select =
  let executeOne :: MonadOrville.MonadOrville m => param -> m (Either a [row])
      executeOne _ =
        Right <$> Select.executeSelect select

      executeMany :: MonadOrville.MonadOrville m => NonEmpty param -> m (Either a (Many param [row]))
      executeMany params = do
        rows <- Select.executeSelect select
        pure . Right $ Many.fromKeys (Fold.toList params) (const (Right rows))

      selectToSqlString :: Select readEntity -> String
      selectToSqlString =
        BS8.unpack
          . RawSql.toExampleBytes
          . Select.selectToQueryExpr
   in Operation
        { executeOperationOne = executeOne
        , executeOperationMany = executeMany
        , explainOperationOne = Exp.explainStep (selectToSqlString select)
        , explainOperationMany = Exp.explainStep (selectToSqlString select)
        }
