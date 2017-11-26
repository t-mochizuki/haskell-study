module Data.Hjq.Query (JqQuery, JqQuery (..)) where

import Data.Hjq.Parser (JqFilter)
import qualified Data.Text as T

data JqQuery
  = JqQueryObject [(T.Text, JqQuery)]
  | JqQueryArray [JqQuery]
  | JqQueryFilter JqFilter
  deriving (Show, Read, Eq)
