{-# LANGUAGE OverloadedStrings #-}

module BookController (
  booksRouter
) where

import BookDatabase
    ( getBooks,
      getBookById,
      insertBook,
      updateBookById,
      deleteBookById )
import BookModel ( Book )
import BookView
import Snap
import Data.Int ()
import Data.Text()
import Data.ByteString ( ByteString )
import Data.ByteString.Char8()
import Data.ByteString.Lazy
import Data.Maybe ( fromMaybe )
import Data.Aeson ( decode, encode )
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Class ()


booksRouter :: Snap ()
booksRouter =  route [
                  (    "", method GET    booksRouteIndex) 
                , (    "", method POST   booksRouteCreate)
                , ("/:id", method GET    booksRouteShow)  
                , ("/:id", method PUT    booksRouteUpdate)
                , ("/:id", method DELETE booksRouteDelete)
              ]

booksRouteIndex :: Snap ()
booksRouteIndex = do
  -- this limit is to get a range if needed
  maybeLimitTo  <- getParam "limit"
  maybeOffsetBy <- getParam "start"
  -- Get a list or array of books from the database
  books <- liftIO $ getBooks maybeLimitTo maybeOffsetBy
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
  writeLBS $ encode $ Prelude.map entityIdToJSON books

booksRouteShow :: Snap ()
booksRouteShow = do
  set404AndContentType
  maybeBookId <- getParam "id"
  (bookIdKey, maybeBook) <- liftIO $ getBookById maybeBookId
  respondWithMaybeBook 200 bookIdKey maybeBook

booksRouteCreate :: Snap ()
booksRouteCreate = do
  body <- readRequestBody 50000
  let book = bookJsonToBook $ parseBodyToBookJson body
  bookIdKey <- liftIO $ insertBook book
  modifyResponse $ setHeader "Content-Type" "application/json"
  respondWithBook 201 bookIdKey book

booksRouteUpdate :: Snap ()
booksRouteUpdate = do
  set404AndContentType
  maybeBookId <- getParam "id"
  body <- readRequestBody 50000
  let bookJson = parseBodyToBookJson body
  (bookIdKey, maybeBook) <- liftIO $ updateBookById maybeBookId bookJson
  respondWithMaybeBook 200 bookIdKey maybeBook

booksRouteDelete :: Snap ()
booksRouteDelete = do
  set404AndContentType
  maybeBookId <- getParam "id"
  (bookIdKey, maybeBook) <- liftIO $ deleteBookById maybeBookId
  respondWithMaybeBook 200 bookIdKey maybeBook

set404AndContentType :: Snap ()
set404AndContentType = do
  modifyResponse $ setResponseCode 404
  modifyResponse $ setHeader "Content-Type" "application/json"

parseBodyToBookJson :: Data.ByteString.Lazy.ByteString -> BookJson
parseBodyToBookJson body = fromMaybe (BookJson (Just "") (Just "")) (decode body :: Maybe BookJson)

respondWithMaybeBook :: Int -> Key Book -> Maybe Book -> Snap()
respondWithMaybeBook code bookIdKey maybeBook = case maybeBook of
    Nothing -> writeBS ("{\"error\": \"Not found.\"}" :: Data.ByteString.ByteString)
    Just book -> respondWithBook code bookIdKey book

respondWithBook :: Int -> Key Book -> Book -> Snap()
respondWithBook code bookIdKey book = do
  modifyResponse $ setResponseCode code
  writeLBS $ bookAsJsonLbs bookIdKey book