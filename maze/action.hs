import Control.Applicative ((<$>))
import Data.Char (toUpper)
import System.Environment (getArgs)

-- hello = putStrLn "hello" >> putStrLn "world"


hello = do
        putStrLn "hello"
        putStrLn "world"


greeting = do
           name <- map toUpper <$> getLine
           putStrLn $ "Hello, " ++ name ++ "!"


getAbsolute = do
              c <- getLine
              return $ read c + 273


printArgs = do
            as <- getArgs
            print as
