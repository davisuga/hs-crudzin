{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Connection where

import Control.Monad.IO.Class (liftIO)
import Data.Functor
import Database.MongoDB

pipe = connect $ host "127.0.0.1"

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs

runAction command = do
  myPipe <- pipe
  results <- access myPipe master "recipeDB" command
  close myPipe
  return results

(|>) = flip ($)

performDBAction :: Action IO a -> Action IO a
performDBAction = runAction <&> liftIO