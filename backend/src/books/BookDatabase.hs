{-# LANGUAGE OverloadedStrings #-}

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
import Database.Persist
import Database.Persist.Class ()
import Database.Persist.Sqlite as DbSql

--seed db
dbMigration :: IO ()
dbMigration = withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Book)

getBookIdKey :: Maybe Data.ByteString.ByteString -> Key Book
getBookIdKey maybeIdBS = toSqlKey bookIdInt64
  where
    -- If we receive `Nothing` for the ID, we will return an invalid ID of `-1`
    bookIdBS = fromMaybe ("-1" :: Data.ByteString.ByteString) maybeIdBS
    bookIdInt64 = read (Data.ByteString.Char8.unpack bookIdBS) :: Int64

getBooks :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Book]
getBooks maybeLimitTo maybeOffsetBy = do
  let limitToBS  = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  let limitToInt  = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  withDbRun $ DbSql.selectList ([] :: [Filter Book]) [LimitTo limitToInt, OffsetBy offsetByInt]

getBookById :: Maybe Data.ByteString.ByteString -> IO (Key Book, Maybe Book)
getBookById maybeIdBS = do
  let bookIdKey = getBookIdKey maybeIdBS
  maybeBook <- withDbRun $ DbSql.get bookIdKey
  return (bookIdKey, maybeBook)

insertBook :: Book -> IO (Key Book)
insertBook book = withDbRun $ DbSql.insert book

updateBookById :: Maybe Data.ByteString.ByteString -> BookJson -> IO (Key Book, Maybe Book)
updateBookById maybeIdBS bookJson = do
  let bookIdKey = getBookIdKey maybeIdBS
  (bookKeyId, maybeBook) <- getBookById maybeIdBS
  case maybeBook of
    Nothing -> return (bookKeyId, Nothing)
    Just book -> do
      let bookUpdated = Book {
          bookTitle = fromMaybe (bookTitle book) (bookJsonTitle bookJson)
        , bookAuthor = fromMaybe (bookAuthor book) (bookJsonAuthor bookJson)
      }
      withDbRun $ DbSql.update bookKeyId [
            BookTitle =. bookTitle bookUpdated
          , BookAuthor =. bookAuthor bookUpdated
        ]
      return (bookKeyId, Just bookUpdated)

deleteBookById :: Maybe Data.ByteString.ByteString -> IO (Key Book, Maybe Book)
deleteBookById maybeIdBS = do
  let bookIdKey = getBookIdKey maybeIdBS
  (bookKeyId, maybeBook) <- getBookById maybeIdBS
  case maybeBook of
    Nothing -> return (bookKeyId, Nothing)
    Just book -> do
      withDbRun $ DbSql.delete bookKeyId
      return (bookKeyId, Just book)