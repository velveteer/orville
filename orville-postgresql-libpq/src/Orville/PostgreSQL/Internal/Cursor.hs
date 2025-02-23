{-# LANGUAGE GADTs #-}

module Orville.PostgreSQL.Internal.Cursor
  ( withCursor,
    declareCursor,
    closeCursor,
    newCursorName,
    fetch,
    move,
    Expr.CursorDirection,
    Expr.next,
    Expr.prior,
    Expr.first,
    Expr.last,
    Expr.absolute,
    Expr.relative,
    Expr.count,
    Expr.fetchAll,
    Expr.forward,
    Expr.forwardCount,
    Expr.forwardAll,
    Expr.backward,
    Expr.backwardCount,
    Expr.backwardAll,
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Word as Word
import qualified System.Random as Random
import qualified Text.Printf as Printf

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.QueryType as QueryType
import Orville.PostgreSQL.Internal.Select (Select, useSelect)
import Orville.PostgreSQL.Internal.SqlMarshaller (AnnotatedSqlMarshaller)

{- |
  A 'Cursor' allows you to fetch rows incrementally from PostgreSQL. Using
  a cursor will allow you to execute a query that returns a very large
  result set without the entire result set being loaded in memory in your
  application and instead pulling rows as you're able to process them.

  See 'withCursor', 'fetch' and 'move' for details on creating and using
  'Cursor' values.
-}
data Cursor readEntity where
  Cursor ::
    AnnotatedSqlMarshaller writeEntity readEntity ->
    Expr.CursorName ->
    Cursor readEntity

{- |
  Declares a @CURSOR@ in PostgreSQL that is available for the duration of the
  action passed to 'withCursor' and closes the cursor when that action
  completes (or raises an exception).

  See @https://www.postgresql.org/docs/current/sql-declare.html@ for details
  about the 'Expr.ScrollExpr' and 'Expr.HoldExpr' parameters and how cursor
  behave in general.

  We recommend you use this instead of 'declareCursor' and 'closeCursor'
  unless you need to control the cursor resource acquisition and release
  yourself and can do so safely.
-}
withCursor ::
  MonadOrville.MonadOrville m =>
  Maybe Expr.ScrollExpr ->
  Maybe Expr.HoldExpr ->
  Select readEntity ->
  (Cursor readEntity -> m a) ->
  m a
withCursor scrollExpr holdExpr select useCursor =
  MonadOrville.liftBracket
    bracket
    (declareCursor scrollExpr holdExpr select)
    closeCursor
    useCursor

{- |
  Declares a @CURSOR@ in PostgreSQL and returns it for you to use. The cursor
  must be closed via 'closeCursor' (or another means) when you are done using
  it. Generally you should use 'withCursor' instead of 'declareCursor' to
  ensure that the cursor gets closed properly.

  See @https://www.postgresql.org/docs/current/sql-declare.html@ for details
  about the 'Expr.ScrollExpr' and 'Expr.HoldExpr' parameters and how cursor
  behave in general.
-}
declareCursor ::
  MonadOrville.MonadOrville m =>
  Maybe Expr.ScrollExpr ->
  Maybe Expr.HoldExpr ->
  Select readEntity ->
  m (Cursor readEntity)
declareCursor scrollExpr holdExpr =
  useSelect $ \queryExpr marshaller -> do
    cursorName <- newCursorName

    let declareExpr =
          Expr.declare cursorName scrollExpr holdExpr queryExpr

    _ <- Execute.executeVoid QueryType.CursorQuery declareExpr
    pure (Cursor marshaller cursorName)

{- |
  Closes a @CURSOR@ in PostgreSQL that was previously declared.
  This should be used to close any cursors you open via 'declareCursor',
  thought we recommend you use 'withCursor' instead to ensure that any
  opened cursor are closed in the event of an exception.
-}
closeCursor ::
  MonadOrville.MonadOrville m =>
  Cursor readEntity ->
  m ()
closeCursor (Cursor _ cursorName) =
  Execute.executeVoid QueryType.CursorQuery
    . Expr.close
    . Right
    $ cursorName

{- |
  Fetch rows from a cursor according to the 'Expr.CursorDirection' given. See
  @https://www.postgresql.org/docs/current/sql-fetch.html@ for details about
  effect of fetch and the meanings of cursor directions to PostgreSQL.
-}
fetch ::
  MonadOrville.MonadOrville m =>
  Maybe Expr.CursorDirection ->
  Cursor readEntity ->
  m [readEntity]
fetch direction (Cursor marshaller cursorName) =
  Execute.executeAndDecode
    QueryType.CursorQuery
    (Expr.fetch direction cursorName)
    marshaller

{- |
  Moves a cursor according to the 'Expr.CursorDirection' given. See
  @https://www.postgresql.org/docs/current/sql-fetch.html@ for details about
  effect of move and the meanings of cursor directions to PostgreSQL.
-}
move ::
  MonadOrville.MonadOrville m =>
  Maybe Expr.CursorDirection ->
  Cursor readEntity ->
  m ()
move direction (Cursor _ cursorName) =
  Execute.executeVoid
    QueryType.CursorQuery
    (Expr.move direction cursorName)

{- |
  INTERNAL - Generates a unique (or very nearly guaranteed to be) cursor name.
  Cursor names only need to be unique among the currently open cursors on the
  current connection, so using POSIX time plus a 32 bit random tag should be
  more than sufficient to ensure conflicts are not seen in practice.
-}
newCursorName :: MonadIO m => m Expr.CursorName
newCursorName =
  liftIO $ do
    now <- POSIXTime.getPOSIXTime
    randomWord <- Random.randomIO

    let nowAsInteger =
          fromEnum . Time.nominalDiffTimeToSeconds $ now

    pure . Expr.cursorName $
      Printf.printf
        "orville_cursor_%x_%08x"
        nowAsInteger
        (randomWord :: Word.Word32)
