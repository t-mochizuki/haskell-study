import Data.Bool
import Data.Char
import System.IO
import System.Environment

import Field

point0 = 100

move :: Char -> Field -> Field
move c = case c of
              'h' -> left
              'j' -> down
              'k' -> up
              'l' -> right
              'H' -> leftf
              'J' -> downf
              'K' -> upf
              'L' -> rightf
              _ -> \f -> f

display p f = do
              putStrLn ""
              print p
              putField f

noBuffering act = do
                  bi <- hGetBuffering stdin
                  hSetBuffering stdin NoBuffering
                  act
                  hSetBuffering stdin bi

loop s act = do
             ms' <- act s
             case ms' of
                  Just s' -> loop s' act
                  Nothing -> return ()

step c p f = do
             display p' f'
             case (goal f', p' <= 0) of
                  (_, True) -> do
                               putStrLn "YOU LOSE!"
                               return Nothing
                  (True, False) -> do
                                   putStrLn "YOU WIN!"
                                   return Nothing
                  (False, False) -> return $ Just (p', f')
             where
             p' = bool p (p - 10) (isUpper c)
             f' = move c f

main :: IO ()
main = do
       n : _ <- getArgs :: IO [String]
       let f0 = field $ read n
       display point0 f0
       noBuffering . loop (point0, f0) $ \(p, f) -> do
                                                    c <- getChar
                                                    case c of
                                                         'q' -> return Nothing
                                                         _ -> step c p f
