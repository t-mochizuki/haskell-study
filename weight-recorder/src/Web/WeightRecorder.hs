{-# LANGUAGE OverloadedStrings #-}

module Web.WeightRecorder (runWeightRecorder, weightRecorderMiddleware, WRConfig(..)) where

import Data.Pool (Pool, createPool)
import Database.HDBC (IConnection (disconnect))
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import qualified Network.Wai as WAI
import Web.Action.Login (loginAction)
import Web.Action.NewRecord (newRecordAction)
import Web.Action.Register (registerAction)
import Web.Core
    ( WRAction
    , WRApp
    , WRConfig (..)
    , WRContext (wrconUser)
    , WRSession (wrsesUser)
    , WRState (WRState, wrstMainTemplate, wrstStartTemplate)
    , emptyContext
    , emptySession
    )

import Web.Spock
    ( get
    , getContext
    , post
    , prehook
    , readSession
    , root
    , runSpock
    , spock
    )

import Web.Spock.Config
    ( PoolOrConn (PCPool)
    , defaultSpockCfg
    )

import Web.View.Main (loadMainTemplate, mainView)
import Web.View.Start (loadStartTemplate, startView)

authHook :: WRAction WRContext
authHook = do
  ctx <- getContext
  mUser <- fmap wrsesUser readSession
  case mUser of
    Nothing -> startView Nothing
    Just user ->
      return $
      ctx
      { wrconUser = Just user
      }

spockApp :: WRApp () ()
spockApp =
  prehook (return emptyContext) $
  do prehook authHook $
       do get root $ mainView Nothing
          post "new_record" newRecordAction
     post "register" registerAction
     post "login" loginAction

weightRecorderMiddleware :: WRConfig -> IO WAI.Middleware
weightRecorderMiddleware cfg = do
  starttpl <- loadStartTemplate cfg
  maintpl <- loadMainTemplate cfg
  let state =
        WRState
        { wrstStartTemplate = starttpl
        , wrstMainTemplate = maintpl
        }
  pool <- sqlitePool $ wrcDBPath cfg
  spCfg <- defaultSpockCfg emptySession (PCPool pool) state
  spock spCfg spockApp

sqlitePool :: FilePath -> IO (Pool Connection)
sqlitePool dbpath = createPool (connectSqlite3 dbpath) disconnect 1 60 5

runWeightRecorder :: WRConfig -> IO ()
runWeightRecorder cfg = runSpock (wrcPort cfg) (weightRecorderMiddleware cfg)
