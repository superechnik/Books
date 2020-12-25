{-# LANGUAGE OverloadedStrings #-}

module BookController (
  booksRouter -- We only need to export the `mainRouter` function
             -- This is used in Main.hs
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
  -- Get the limit and start paramters (?limit=:limit&start=:start) if sent
  maybeLimitTo  <- getParam "limit"
  maybeOffsetBy <- getParam "start"
  -- Get a list or array of books from the database
  books <- liftIO $ getBooks maybeLimitTo maybeOffsetBy
  -- Set the content type to Json
  -- We will be responding with Json
  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Write out the Json response
  writeLBS $ encode $ Prelude.map entityIdToJSON books

booksRouteShow :: Snap ()
booksRouteShow = do
  -- We will start off assuming the book could not be found
  -- This sets the HTTP status code to 404 (not found)
  set404AndContentType
  -- Get the ID parameter
  maybeBookId <- getParam "id"
  -- Get the book primary key and record
  (bookIdKey, maybeBook) <- liftIO $ getBookById maybeBookId
  -- Respond with 200 if the book with ID actually exists
  -- This will write out our Json response
  respondWithMaybeBook 200 bookIdKey maybeBook

booksRouteCreate :: Snap ()
booksRouteCreate = do
  -- Read in the request HTTP body
  body <- readRequestBody 50000
  -- Parse the Json request body into a `Book` model (record)
  let book = bookJsonToBook $ parseBodyToBookJson body
  -- Insert the book into the database
  bookIdKey <- liftIO $ insertBook book
  -- Set the content type to Json
  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Let the client know that we created a new record (201)
  -- Respond with the newly created book in Json format
  respondWithBook 201 bookIdKey book

booksRouteUpdate :: Snap ()
booksRouteUpdate = do
  set404AndContentType
  maybeBookId <- getParam "id"
  body <- readRequestBody 50000
  -- Parse the request body into `BookJson`
  let bookJson = parseBodyToBookJson body
  -- Update the book if it exists
  (bookIdKey, maybeBook) <- liftIO $ updateBookById maybeBookId bookJson
  -- If the book exists, tell the client OK (200)
  -- Respond with the book Json or an error message in Json
  respondWithMaybeBook 200 bookIdKey maybeBook

booksRouteDelete :: Snap ()
booksRouteDelete = do
  set404AndContentType
  maybeBookId <- getParam "id"
  -- Delete the book in the database if it exists
  (bookIdKey, maybeBook) <- liftIO $ deleteBookById maybeBookId
  -- If the book exists, resond with 200 and the book in Json form
  -- Otherwise respond with 404 (not found) and an error message in Json format
  respondWithMaybeBook 200 bookIdKey maybeBook

set404AndContentType :: Snap ()
set404AndContentType = do
  -- Set the HTTP status code to 404 (not found)
  modifyResponse $ setResponseCode 404
  -- Set the content type as Json
  -- This will let the client know what kind of data is being returned
  -- in the HTTP response body
  modifyResponse $ setHeader "Content-Type" "application/json"

parseBodyToBookJson :: Data.ByteString.Lazy.ByteString -> BookJson
-- Parse a raw HTTP body into a `BookJson` record
parseBodyToBookJson body = fromMaybe (BookJson (Just "") (Just "")) (decode body :: Maybe BookJson)

respondWithMaybeBook :: Int -> Key Book -> Maybe Book -> Snap()
respondWithMaybeBook code bookIdKey maybeBook = case maybeBook of
    -- Book not found?
    Nothing -> writeBS ("{\"error\": \"Not found.\"}" :: Data.ByteString.ByteString)
    -- Book found?
    -- The code is the HTTP status code
    Just book -> respondWithBook code bookIdKey book

respondWithBook :: Int -> Key Book -> Book -> Snap()
respondWithBook code bookIdKey book = do
  -- Set the HTTP status code
  modifyResponse $ setResponseCode code
  -- Write out the book in Json format into the response body
  writeLBS $ bookAsJsonLbs bookIdKey book