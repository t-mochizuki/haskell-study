module Field where

import Data.Bool
import System.Random

type Field = ([([Bool], [Bool])], [([Bool], [Bool])])

width = 40
height = 20

toField :: [[Bool]] -> Field
toField ls = ([], map (\l -> ([], l)) ls)

sample :: [[Bool]]
sample = [
  [False, False, False, True, True],
  [True, False, False, False, True],
  [True, True, False, False, True],
  [True, True, True, False, False] ]

showL :: [Bool] -> [Char]
showL = map (bool ' ' '*') . reverse

showR :: [Bool] -> [Char]
showR = map (bool ' ' '*')

showLine :: ([Bool], [Bool]) -> [Char]
showLine (l, r) = showL l ++ showR r

showField :: Field -> String
showField (t, (l, _ : r) : b) = unlines $
                                map showLine (reverse t) ++
                                [showL l ++ "A" ++ showR r] ++
                                map showLine b ++
                                [replicate (width - 2) ' ' ++ "GOAL"]

putField :: Field -> IO ()
putField = putStr . showField

downf :: Field -> Field
downf (as, [h]) = (as, [h])
downf (as, h : bs) = (h : as, bs)

upf :: Field -> Field
upf ([], hbs) = ([], hbs)
upf (a : as, hbs) = (as, a: hbs)

mapTuple :: (t1 -> t) -> (t1, t1) -> (t, t)
mapTuple f (x, y) = (f x, f y)

rightf :: Field -> Field
rightf = mapTuple . map $ \lhr -> case lhr of
                                       (ls, [h]) -> lhr
                                       (ls, h : rs) -> (h : ls, rs)

leftf :: Field -> Field
leftf = mapTuple . map $ \lhr -> case lhr of
                                      ([], hrs) -> lhr
                                      (l : ls, hrs) -> (ls, l : hrs)

check move field = case move field of
   (_, (_, True : _) : _) -> field
   f' -> f'

[up, down, left, right] = map check [upf, downf, leftf, rightf]

divide _ [] = []
divide n xs = take n xs : divide n (drop n xs)

field :: Int -> Field
field = toField . take height . divide width
        . (\bs -> replicate 4 False ++ bs)
        . randoms . mkStdGen

goal :: Field -> Bool
goal (_, [(_, [_])]) = True
goal _ = False
