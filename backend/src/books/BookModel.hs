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

module BookModel (
    Book(..)
  , entityDefs
  , EntityField(..)
) where

import GHC.Generics
import Data.Aeson
import Data.Default.Class ()
import Database.Persist
import Database.Persist.Class ()
import Database.Persist.TH

share [mkPersist sqlSettings, mkSave "entityDefs"][persistLowerCase|
  Book
    -- Two fields for intial proof of concept
    title String
    author  String
    deriving Show Generic
|]

instance ToJSON Book where
  toJSON (Book title author) = object ["title" .= title, "author" .= author]