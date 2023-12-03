{-# LANGUAGE NoOverloadedStrings #-}
module MyLib (someFunc) where
import System.IO
import System.Directory
import Control.Monad

import Text.Read.Lex (Number)
import qualified Text.Regex as R

import Data.Char (isAlpha, isDigit)
import Data.List
import Data.Text(pack, unpack, replace)

someFunc :: IO ()
someFunc = print . calibrateCorrect <$> words =<< readFile "./src/day1.txt"
-- someFunc = print . calibrateCorrect <$> words =<< readFile "./src/day1.txt" --

calibrateStr :: [String] -> [Int]
calibrateStr = map (parseNum . filter (not . isAlpha))

str2List :: [a] -> [[a]]
str2List = map (:[])

parseNum :: String -> Int
parseNum s = read (head (str2List s) ++ last (str2List s)) :: Int

{--
Part 2: Checking if substring is number
--}
numWords2 :: [(String, String)]
numWords2 = [("zero", "0"), 
            ("one", "1"), 
            ("two", "2"),
            ("three", "3"),
            ("four", "4"),
            ("five", "5"),
            ("six", "6"),
            ("seven", "7"),
            ("eight", "8"),
            ("nine", "9"),
            ("ten", "10")]

replaceAllIn :: String -> [(String, String)] -> String
replaceAllIn = foldl (\acc (k, v) -> R.subRegex (R.mkRegex k) acc v)

numWords :: [String]
numWords = ["zero", 
            "one", 
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine",
            "ten"]

replaceText :: Eq a => [a] -> [a] -> [a] -> [a]
replaceText a b s@(x: xs) = if a `isPrefixOf` s
                            then b ++ replaceText a b (drop (length a) s)
                            else x:replaceText a b xs
replaceText _ _ [] = []

calibrateCorrect :: [String] -> [Int]
calibrateCorrect = map (parseNum . filter (not . isAlpha) . replaceCheck . reverse)
--calibrateCorrect = map (parseNum . filter (not . isAlpha) . flip replaceAllIn (sortOn fst (numWords2)) . dropUntil)

replaceCheck = foldr (\c acc -> replaceAllIn (acc ++ [c]) (sortOn fst numWords2)) ""

dropUntil :: [Char] -> [Char]
dropUntil s =   if any isDigit s
                then drop (nth-1) s
                else s
                where
                    nth = (getIndex . head) $ filter (isDigit . fst) (zip s [1..(length s)])

getIndex (_, x) = x

str2num :: String -> String
str2num s = case s of
    "zero" -> "0"
    "one" -> "1"
    "two" -> "2"
    "three" -> "3"
    "four" -> "4"
    "five" -> "5"
    "six" -> "6"
    "seven" -> "7"
    "eight" -> "8"
    "nine" -> "9"
    "ten" -> "10"