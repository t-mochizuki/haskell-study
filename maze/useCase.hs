import Data.Char

answer c = case toLower c of
                'y' -> Just True
                'n' -> Just False
                _ -> Nothing
