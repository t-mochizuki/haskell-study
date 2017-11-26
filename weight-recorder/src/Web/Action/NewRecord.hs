{-# LANGUAGE OverloadedStrings #-}

module Web.Action.NewRecord (newRecordAction) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Time.Clock as TM
import qualified Data.Time.LocalTime as TM
import qualified Entity.User as User
import Model.WeightRecord
    ( NewWRecord (NewWRecord)
    , insertNewWRecord
    )
import Web.Core (WRAction, runSqlite, wrconUser)
import Web.Spock (getContext, param, redirect)
import Web.View.Main (mainView)

newRecordAction :: WRAction a
newRecordAction = do
  mWeight <- param "weight"
  case mWeight of
    Nothing -> mainView (Just "体重の取得に失敗しました")
    Just weight -> do
      Just user <- wrconUser <$> getContext
      now <- liftIO utcTime
      let record = NewWRecord (User.id user) now weight
      n <- runSqlite $ insertNewWRecord record
      when (n <= 0) $ mainView (Just "記録に失敗しました")
      redirect "/"
  where
    utcTime = TM.utcToLocalTime TM.utc <$> TM.getCurrentTime
