module Web.Core
    ( WRState(WRState, wrstMainTemplate, wrstStartTemplate)
    , WRContext(WRContext, wrconUser), emptyContext, WRConnection
    , WRSession(WRSession, wrsesUser), emptySession, WRApp, WRAction
    , runSqlite, WRConfig(WRConfig, wrcDBPath, wrcTplRoots, wrcPort))
  where

import Control.Monad.IO.Class (liftIO)
import Database.HDBC.Sqlite3 (Connection)
import qualified Entity.User as User
import Text.Mustache (Template)
import Web.Spock (SpockActionCtx, SpockCtxM, runQuery)

type WRApp ctx = SpockCtxM ctx WRConnection WRSession WRState
type WRAction = SpockActionCtx WRContext WRConnection WRSession WRState

data WRConfig = WRConfig
  { wrcDBPath :: !FilePath
  , wrcTplRoots :: ![FilePath]
  , wrcPort :: !Int
  }

data WRState = WRState
  { wrstStartTemplate :: !Template
  , wrstMainTemplate :: !Template
  }

newtype WRContext = WRContext
  { wrconUser :: Maybe User.User }

emptyContext :: WRContext
emptyContext = WRContext Nothing

type WRConnection = Connection

newtype WRSession = WRSession
  { wrsesUser :: Maybe User.User }

emptySession :: WRSession
emptySession = WRSession Nothing

runSqlite :: (Connection -> IO m) -> WRAction m
runSqlite f = runQuery $ liftIO . f
