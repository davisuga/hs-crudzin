{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Model.Recipe where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Database.MongoDB
  ( Action,
    Document,
    Select (select),
    Val (val),
    Value,
    at,
    delete,
    find,
    insert,
    rest,
    upsert,
    (=:),
  )
import Domain.Recipe (Recipe (..))
import GHC.Records (HasField (getField))
import Model.Connection (performDBAction)

toDoc :: Recipe -> Document
toDoc recipe =
  [ "name" =: getField @"name" recipe,
    "preparation" =: getField @"preparation" recipe,
    "ingredients" =: getField @"ingredients" recipe
  ]

fromDoc :: Document -> Recipe
fromDoc doc =
  Recipe
    ("name" `at` doc)
    ("preparation" `at` doc)
    ("ingredients" `at` doc)

fromDocs :: [Document] -> [Recipe]
fromDocs = map fromDoc

fromFirstDoc :: [Document] -> Recipe
fromFirstDoc = fromDoc . head

deleteRecipe :: (MonadIO m) => String -> Action m ()
deleteRecipe recipeName =
  delete (select ["name" =: val recipeName] "recipes")

createRecipe :: Recipe -> Action IO Value
createRecipe recipe =
  performDBAction $ insert "recipes" (toDoc recipe)

getRecipes :: Action IO [Recipe]
getRecipes =
  performDBAction $ find (select [] "recipes") >>= rest <&> fromDocs

getRecipe :: String -> Action IO Recipe
getRecipe recipeName =
  performDBAction $ find (select ["name" =: recipeName] "recipes") >>= rest <&> fromFirstDoc

editRecipe :: Recipe -> Action IO ()
editRecipe recipe =
  performDBAction $ upsert (select ["name" =: getField @"name" recipe] "recipes") (toDoc recipe)
