{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

-- Export our view (`BookJson`) and two helper functions for
-- turning some Json into a `Book` record or
-- turning a `Book` record into a Json string

module BookView (
    BookJson(..)
  , bookJsonToBook
  , bookAsJsonLbs-- LBS stands for Lazy Byte String
) where

-- Our custom Model module

import BookModel

-- Build dependencies

import GHC.Generics ( Generic )
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Database.Persist

-- Our "view" or `BookJson` record

data BookJson = BookJson {
    bookJsonTitle :: Maybe String
  , bookJsonAuthor :: Maybe String
} deriving (Show, Generic)

-- Here we defined how to parse a Json string "{\"title\": \"...\", \"url\": \"...\"}"
-- into a `BookJson` record

instance FromJSON BookJson where
  parseJSON (Object v) =
    BookJson <$> v .:?  "title" -- .:? is syntax for parsing a Json string field into Maybe String
                 <*> v .:?  "author"   -- The Json string may not have "{\"url\": \"...\"}"
                                    -- If that is the case, `BookJsonURL` will be `Nothing`

-- Here we define how to take a `BookJson` record
-- and turn it into Json {"title": "...", "url": "..."}
-- For example:
-- > let x = BookJson {BookJsonTitle = Just "one", BookJsonUrl = Just "two"}
-- > toJson x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJson x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON BookJson where
    toJSON (BookJson title author) = object ["title" .= title, "author" .= author]

bookJsonToBook :: BookJson -> Book
bookJsonToBook bookJson = Book titleJsonToTitle urlJsonToUrl
  where
    -- If the Json didn't have a title, just set the title to an empty string
    titleJsonToTitle = fromMaybe "" $ bookJsonTitle bookJson
    -- If the Json didn't have a URL, just set the title to an empty string
    urlJsonToUrl = fromMaybe "" $ bookJsonAuthor bookJson

bookAsJsonLbs :: Key Book -> Book -> Data.ByteString.Lazy.ByteString
-- Convert a Book primary key and `Book` record to a Json lazy byte string
-- "{\"id\": 1, \"title\": \"...\", \"url\": \"...\"}"
bookAsJsonLbs k b = encode . entityIdToJSON $ Entity k b