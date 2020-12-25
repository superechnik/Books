module Main where

import BookDatabase ( dbMigration )
import Router ( mainRouter )
import Snap ( quickHttpServe )

main :: IO ()
main = do
  -- Create or modify the bookmark database table
  dbMigration
  -- Begin serving all HTTP requests
  quickHttpServe mainRouter