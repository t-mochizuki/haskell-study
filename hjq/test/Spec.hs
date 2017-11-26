import Test.HUnit
import Hjq (parseJqFilter, parseJqQuery)
import Data.Hjq.Parser (JqFilter (..))
import Data.Hjq.Query (JqQuery (..))
import qualified Data.Text as T

main :: IO ()
main = do
  runTestTT $ TestList
    [ jqFilterParserTest
    , jqFilterParserSpacesTest
    , jqQueryParserTest
    , jqQueryParserSpacesTest
    ]
  return ()

jqFilterParserTest :: Test
jqFilterParserTest = TestList
  [ "jqFilterParser test 1" ~: parseJqFilter (T.pack ".") ~?= Right JqNil
  , "jqFilterParser test 2" ~: parseJqFilter (T.pack ".[0]") ~?= Right (JqIndex 0 JqNil)
  , "jqFilterParser test 3" ~: parseJqFilter (T.pack ".fieldName") ~?= Right (JqField (T.pack "fieldName") JqNil)
  , "jqFilterParser test 4" ~: parseJqFilter (T.pack ".[0].fieldName") ~?= Right (JqIndex 0 (JqField (T.pack "fieldName")JqNil))
  , "jqFilterParser test 5" ~: parseJqFilter (T.pack ".fieldName[0]") ~?= Right (JqField (T.pack "fieldName") (JqIndex 0 JqNil))
  ]

jqFilterParserSpacesTest :: Test
jqFilterParserSpacesTest = TestList
  [ "jqFilterParser spaces test 1" ~: parseJqFilter (T.pack " . ") ~?= Right JqNil
  , "jqFilterParser spaces test 2" ~: parseJqFilter (T.pack " . [ 0 ] ") ~?= Right (JqIndex 0 JqNil)
  , "jqFilterParser spaces test 3" ~: parseJqFilter (T.pack " . fieldName ") ~?= Right (JqField (T.pack "fieldName") JqNil)
  , "jqFilterParser spaces test 4" ~: parseJqFilter (T.pack " . [0] . fieldName ") ~?= Right (JqIndex 0 (JqField (T.pack "fieldName")JqNil))
  , "jqFilterParser spaces test 5" ~: parseJqFilter (T.pack " . fieldName [ 0 ] ") ~?= Right (JqField (T.pack "fieldName") (JqIndex 0 JqNil))
  ]

jqQueryParserTest :: Test
jqQueryParserTest = TestList
  [ "jqQueryParser test 1" ~:
      parseJqQuery (T.pack "[]") ~?=
        Right (JqQueryArray [])
  , "jqQueryParser test 2" ~:
      parseJqQuery (T.pack "[.hoge,.piyo]")~?=
        Right (JqQueryArray [JqQueryFilter (JqField (T.pack "hoge") JqNil), JqQueryFilter (JqField (T.pack "piyo") JqNil)])
  , "jqQueryParser test 3" ~:
      parseJqQuery (T.pack "{\"hoge\":[],\"piyo\":[]}") ~?=
        Right (JqQueryObject [(T.pack "hoge", JqQueryArray []), (T.pack "piyo", JqQueryArray [])])
  ]

jqQueryParserSpacesTest :: Test
jqQueryParserSpacesTest = TestList
  [ "jqQueryParser spaces test 1" ~:
      parseJqQuery (T.pack " [ ] ") ~?=
        Right (JqQueryArray [])
  , "jqQueryParser spaces test 2" ~:
      parseJqQuery (T.pack " [ . hoge , . piyo ] ")~?=
        Right (JqQueryArray [JqQueryFilter (JqField (T.pack "hoge") JqNil), JqQueryFilter (JqField (T.pack "piyo") JqNil)])
  , "jqQueryParser spaces test 3" ~:
      parseJqQuery (T.pack " { \"hoge\" : [ ] , \"piyo\" : [ ] } ") ~?=
        Right (JqQueryObject [(T.pack "hoge", JqQueryArray []), (T.pack "piyo", JqQueryArray [])])
  ]
