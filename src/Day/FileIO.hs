module Day.FileIO (reader) where

reader :: String -> IO [String]
reader = fmap lines . readFile
