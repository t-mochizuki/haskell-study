module Data.Hjq.Parser (JqFilter, JqFilter (..)) where

import qualified Data.Text as T

data JqFilter
  = JqField T.Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Show, Read, Eq)

-- parseJqFilter :: T.Text -> Either T.Text JqFilter
-- parseJqFilter s = undefined
