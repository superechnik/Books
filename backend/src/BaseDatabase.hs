module BaseDatabase (
    sqliteConnString,
    withDbRun
)
where

import System.Environment ( lookupEnv )    
import Data.Text ( Text, pack )
import Control.Monad.Trans.Resource ( ResourceT )
import Control.Monad.Trans.Control ()
import Control.Monad.Logger ( NoLoggingT )
import Database.Persist.Class ()
import Database.Persist.Sqlite as DbSql ( runSqlite, SqlPersistT )
import Data.Maybe (fromMaybe)

sqliteConnString :: IO Data.Text.Text
sqliteConnString = do
  maybeDbConnString <- lookupEnv "bookCollection_conn_string"
  return $ Data.Text.pack $ fromMaybe "bookCollection_default.db" maybeDbConnString

withDbRun :: SqlPersistT (NoLoggingT (ResourceT IO)) b -> IO b
withDbRun command = do
  connString <- sqliteConnString
  runSqlite connString command