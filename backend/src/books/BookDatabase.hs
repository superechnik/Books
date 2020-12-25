{-# LANGUAGE OverloadedStrings #-} -- Language Extensions

module BookDatabase (
    dbMigration
  , getBooks
  , getBookById
  , insertBook
  , updateBookById
  , deleteBookById
) where

import BaseDatabase
import BookModel
import BookView

--read evn vars

import Data.Int ( Int64 )
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( unpack )
import Data.ByteString.Lazy()
import Data.Maybe ( fromMaybe )
import Data.Aeson ()

-- Needed for interfacing with SQLite
import Database.Persist
import Database.Persist.Class ()
import Database.Persist.Sqlite as DbSql


-- This will create our web books table if it does not already exist
-- Persistent will assist with update our table schema should our model change

dbMigration :: IO ()
dbMigration = withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Book)

-- Helper function to convert the URL ID string to the needed 64 bit integer primary key

getBookIdKey :: Maybe Data.ByteString.ByteString -> Key Book
getBookIdKey maybeIdBS = toSqlKey bookIdInt64
  where
    -- If we receive `Nothing` for the ID, we will return an invalid ID of `-1`
    bookIdBS = fromMaybe ("-1" :: Data.ByteString.ByteString) maybeIdBS
    -- Convert the string the needed 64 bit integer
    bookIdInt64 = read (Data.ByteString.Char8.unpack bookIdBS) :: Int64

-- Retrieves multiple book rows from our table starting at `start` and up to the `limit`

getBooks :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Book]
getBooks maybeLimitTo maybeOffsetBy = do
  -- If the limit and offset are `Nothing`, we will use the defaults 10 for the limit and 0 for the offset
  let limitToBS  = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  -- Converts the strings to integers
  let limitToInt  = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  -- The actual database call 
  -- we can take out these limits for these gets
  withDbRun $ DbSql.selectList ([] :: [Filter Book]) [LimitTo limitToInt, OffsetBy offsetByInt]

getBookById :: Maybe Data.ByteString.ByteString -> IO (Key Book, Maybe Book)
getBookById maybeIdBS = do
  -- Get the book primary key
  let bookIdKey = getBookIdKey maybeIdBS
  -- Retrieve the book from the database
  maybeBook <- withDbRun $ DbSql.get bookIdKey
  -- Return both the primary key and maybe the book (if it actually exists in the database)
  return (bookIdKey, maybeBook)

insertBook :: Book -> IO (Key Book)
-- Create a new book row in the database
insertBook book = withDbRun $ DbSql.insert book

updateBookById :: Maybe Data.ByteString.ByteString -> BookJson -> IO (Key Book, Maybe Book)
updateBookById maybeIdBS bookJson = do
  let bookIdKey = getBookIdKey maybeIdBS
  -- Look up the book in the database
  (bookKeyId, maybeBook) <- getBookById maybeIdBS
  case maybeBook of
    -- If the book mark does not exist, return `Nothing`
    Nothing -> return (bookKeyId, Nothing)
    -- If the book mark does exist
    Just book -> do
      -- Create an updated book record
      let bookUpdated = Book {
          -- The Json maybe not have the title so use the book's current title
          bookTitle = fromMaybe (bookTitle book) (bookJsonTitle bookJson)
          -- The Json maybe not have the URL so use the book's current URL
        , bookAuthor = fromMaybe (bookAuthor book) (bookJsonAuthor bookJson)
      }
      -- Update the book's title and URL in the database
      withDbRun $ DbSql.update bookKeyId [
            BookTitle =. bookTitle bookUpdated
          , BookAuthor =. bookAuthor bookUpdated
        ]
      return (bookKeyId, Just bookUpdated)

deleteBookById :: Maybe Data.ByteString.ByteString -> IO (Key Book, Maybe Book)
deleteBookById maybeIdBS = do
  let bookIdKey = getBookIdKey maybeIdBS
  -- Look up the book in the database
  (bookKeyId, maybeBook) <- getBookById maybeIdBS
  case maybeBook of
    -- No book?
    Nothing -> return (bookKeyId, Nothing)
    -- Book?
    Just book -> do
      -- Delete the book from the database
      withDbRun $ DbSql.delete bookKeyId
      return (bookKeyId, Just book)