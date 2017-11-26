module Hjq (hjq, someFunc, parseJqFilter, parseJqQuery) where

import Control.Applicative
import Data.Hjq.Parser (JqFilter (..))
import Data.Hjq.Query (JqQuery (..))
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)

import Data.Aeson hiding (Result)
import Data.Aeson.Lens (key, nth)
-- import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Lens ((^?))
import Data.ByteString.Lazy hiding (putStrLn)
import Control.Error.Util (note)
import Control.Monad (join)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseJqFilter :: T.Text -> Either T.Text JqFilter
parseJqFilter s = showParseResult
  $ parse (jqFilterParser <* endOfInput) s `feed` (T.pack "")

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
  where
    jqFilter :: Parser JqFilter
    jqFilter
      = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

    jqField :: Parser JqFilter
    jqField = JqField <$> (word <* skipSpace) <*> jqFilter

    jqIndex :: Parser JqFilter
    jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

showParseResult :: Show a => Result a -> Either T.Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . T.pack $ show r

word :: Parser T.Text
word = fmap T.pack $ many1 (letter <|> schar '-' <|> schar '_' <|> digit)



parseJqQuery :: T.Text -> Either T.Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` (T.pack "")

jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryFilter <|> queryObject
  where
    queryArray :: Parser JqQuery
    queryArray = JqQueryArray <$> (schar '[' *> jqQueryParser `sepBy` (schar ',') <* schar ']')

    queryObject :: Parser JqQuery
    queryObject = JqQueryObject <$> (schar '{' *> (qObj `sepBy` schar ',') <* schar '}')

    qObj :: Parser (T.Text, JqQuery)
    qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

    queryFilter :: Parser JqQuery
    queryFilter = JqQueryFilter <$> jqFilterParser

unsafeParseFilter :: T.Text -> JqFilter
unsafeParseFilter t = case parseJqFilter t of
  Right f -> f
  Left s -> error $ "PARSE FAILURE IN A TEST :" ++ T.unpack s

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _)
  = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key fieldName))
applyFilter (JqIndex index n) array@(Array _)
  = join $ noteOutOfRangeError index (fmap (applyFilter n) (array ^? nth index))
applyFilter JqNil v = Right v
applyFilter f o = Left $ T.pack "unexpected pattern :" -- <> tshow f <> T.pack " : " <> tshow o

noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ T.pack "field name not found " -- <> s

noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ T.pack "out of range : " -- <> tshow s

tshow :: Show a => a -> T.Text
tshow = T.pack . show

executeQuery :: JqQuery -> Value -> Either T.Text Value
executeQuery (JqQueryObject o) v
  = fmap (Object . H.fromList) . sequence . fmap sequence $ fmap (fmap $ flip executeQuery v) o
executeQuery (JqQueryArray l) v
  = fmap (Array . V.fromList) . sequence $ fmap (flip executeQuery v) l
executeQuery (JqQueryFilter f) v = applyFilter f v

hjq :: ByteString -> T.Text -> Either T.Text ByteString
hjq jsonString queryString = do
  value <- note (T.pack "Invalid json format.") $ decode jsonString
  query <- parseJqQuery queryString
  executeQuery query value >>= return . encodePretty
