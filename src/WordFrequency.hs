{-# LANGUAGE OverloadedStrings #-}
module WordFrequency where

import qualified Data.Text.IO as In
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Ord
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
wordFrequency n ignoreList = take n . topFrequencies . filterText ignoreList . normaliseText

filterText :: IgnoreList -> WordList -> WordList
filterText ignoreList = filter (`notElem` ignoreList)

normaliseText :: T.Text -> WordList
normaliseText = map T.toCaseFold . T.words . T.map normalise
  where normalise c = if isAlphaNum c then c else ' '

topFrequencies :: Ord a => [a] -> [(a, Int)]
topFrequencies = sortBy reverseOrder . frequencies
    where reverseOrder a b = compare (snd b) (snd a)

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = M.toList . foldr (\a -> M.insertWith (+) a 1) M.empty 

oneLiner :: Ord a => [a] -> [(a, Int)]
oneLiner = sortBy (flip $ comparing snd) . M.toList . foldr (\a -> M.insertWith (+) a 1) M.empty
