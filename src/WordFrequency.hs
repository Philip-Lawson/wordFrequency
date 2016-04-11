{-# LANGUAGE OverloadedStrings #-}
module WordFrequency where

import qualified Data.Text.IO as In
import qualified Data.Text as T
import Data.Char
import Text.Read
import Data.List
import System.Environment

type IgnoreList = [T.Text]
type WordList = [T.Text]

run :: IO ()
run = do
    (file:ignoreFile:num:_) <- getArgs
    text <- In.readFile file
    ignoreText <- In.readFile ignoreFile
    case readMaybe num :: Maybe Int of
      Nothing -> putStrLn "Please enter a valid number as the third argument"
      Just n -> mapM_ print topWords
        where topWords = wordFrequency n (parseIgnoreList ignoreText) text

parseIgnoreList :: T.Text -> IgnoreList
parseIgnoreList = mappend alphaList . map T.toCaseFold . T.splitOn ","
  where alphaList = map T.singleton ['a'..'z']

wordFrequency :: Int -> IgnoreList -> T.Text -> [(T.Text, Int)]
wordFrequency n ignoreList = take n . topFrequency . filterText ignoreList . normaliseText

filterText :: IgnoreList -> WordList -> WordList
filterText ignoreList = filter (`notElem` ignoreList)

normaliseText :: T.Text -> WordList
normaliseText = map T.toCaseFold . T.words . T.map normalise
  where normalise c = if isAlphaNum c then c else ' '

topFrequency :: Ord a => [a] -> [(a, Int)]
topFrequency = sortBy reverseOrder . frequencies
  where reverseOrder a b = compare (snd b) (snd a)

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map occurences . group . sort
  where occurences a = (head a, length a)
