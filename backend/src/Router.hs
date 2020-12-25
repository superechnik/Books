{-# LANGUAGE OverloadedStrings #-}

module Router(
    mainRouter 
) where

import Snap
import BookController (booksRouter)
import Data.ByteString.Char8()

mainRouter :: Snap ()
mainRouter =  route [
                  (    "", writeBS "aaaaaaaaaaaaaaargh default") -- Base / route
                , ("books", booksRouter) -- /books route
              ]