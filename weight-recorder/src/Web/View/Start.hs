{-# LANGUAGE OverloadedStrings #-}

module Web.View.Start (startView, loadStartTemplate) where

import qualified Data.Text as TXT
import Text.Mustache
  ( Template
  , automaticCompile
  , object
  , substitute
  , (~>)
  )
import Web.Core
  ( WRAction
  , WRConfig (wrcTplRoots)
  , WRState (wrstStartTemplate)
  )
import Web.Spock (getState, html)

loadStartTemplate :: WRConfig -> IO Template
loadStartTemplate cfg = do
  compiled <- automaticCompile (wrcTplRoots cfg) "start.mustache"
  case compiled of
    Left err -> error (show err)
    Right template -> return template

startView :: Maybe TXT.Text -> WRAction a
startView mMsg = do
    tpl <- wrstStartTemplate <$> getState
    html $ substitute tpl $ object (toPairs mMsg)
  where
    toPairs (Just msg) = ["message" ~> msg]
    toPairs Nothing = []
