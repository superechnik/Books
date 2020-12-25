{-# LANGUAGE
    OverloadedStrings
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , GADTs
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving
  , DerivingStrategies
  , UndecidableInstances #-}

-- Export our `Book` record (model),
-- the entity (book) definition,
-- and the entity fields' setters and getters

module BookModel (
    Book(..)
  , entityDefs
  , EntityField(..)
) where

-- Needed for encoding and decoding to/from Json

import GHC.Generics
import Data.Aeson
import Data.Default.Class ()

-- Needed for generating our book entity

import Database.Persist
import Database.Persist.Class ()
import Database.Persist.TH

-- Generates our `BookEntity` instance and `Book` record

share [mkPersist sqlSettings, mkSave "entityDefs"][persistLowerCase|
  Book
    -- Two fields
    title String
    author  String
    deriving Show Generic
|]

-- Defines the ToJson interface for our `Book` record
-- This will take a `Book` record and convert it to Json
-- For example:
-- > let x = Book {bookTitle = "one", bookUrl = "two"}
-- > toJson x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJson x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON Book where
  toJSON (Book title author) = object ["title" .= title, "author" .= author]