{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

module BookView (
    BookJson(..)
  , bookJsonToBook
  , bookAsJsonLbs
) where

import BookModel

import GHC.Generics ( Generic )
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Database.Persist

data BookJson = BookJson {
    bookJsonTitle :: Maybe String
  , bookJsonAuthor :: Maybe String
} deriving (Show, Generic)

instance FromJSON BookJson where
  parseJSON (Object v) =
    BookJson <$> v .:?  "title" -- .:? is syntax for parsing a Json string field into Maybe String
                 <*> v .:?  "author"   -- The Json string may not have "{\"url\": \"...\"}"
                                    -- If that is the case, `BookJsonURL` will be `Nothing`

instance ToJSON BookJson where
    toJSON (BookJson title author) = object ["title" .= title, "author" .= author]

bookJsonToBook :: BookJson -> Book
bookJsonToBook bookJson = Book titleJsonToTitle urlJsonToUrl
  where
    titleJsonToTitle = fromMaybe "" $ bookJsonTitle bookJson
    urlJsonToUrl = fromMaybe "" $ bookJsonAuthor bookJson

bookAsJsonLbs :: Key Book -> Book -> Data.ByteString.Lazy.ByteString
bookAsJsonLbs k b = encode . entityIdToJSON $ Entity k b