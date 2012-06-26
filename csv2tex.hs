module Main where	

import Text.ParserCombinators.Parsec 
import Control.Monad
import System.Environment

--Misc

zipWithLong :: a -> (a->a->b) -> [a]->[a]->[b]
zipWithLong i f [] (b:bs) = f i b : zipWithLong i f [] bs
zipWithLong i f (a:as) [] = f a i : zipWithLong i f as []
zipWithLong i f (a:as) (b:bs) = f a b : zipWithLong i f as bs
zipWithLong _ _ _ _ = []

myDec :: Int -> Int
myDec x
	| x > 0 = x-1
	| otherwise = x

--Parser

csvParser = sepEndBy row (char '\n')
row = sepBy cell (char ',')
cell = qcell <|> many (escChar <|> noneOf ",\n")
qcell = do
	char '"'
	res <- many (escChar <|> noneOf "\"")
	char '"'
	return res
escChar = char '\\' >> anyChar

--Parse function

texFmt :: String -> IO ()
texFmt inp = case (parse csvParser "" inp) of
	Left err -> putStrLn "Parse error at " >> print err
	Right x -> putStrLn $ fmtCsv x

--Formatting

colSizes :: [[String]] -> [Int]
colSizes csv = foldr (zipWithLong 0 max . fmap length) [] csv

padString :: Int -> String -> String
padString 0 [] = []
padString x [] = ' ' : padString (myDec x) []
padString x (s:ss) = s : padString (myDec x) ss

fmtLine :: [Int] -> [String] -> String
fmtLine _ [] = ""
fmtLine (w:ws) (l:[]) = padString w l ++ " \\\\\n"
fmtLine (w:ws) (l:ls) = padString w l ++ " & " ++ (fmtLine ws ls)

fmtCsv :: [[String]] -> String
fmtCsv csv = concat $ fmap (fmtLine (colSizes csv)) csv

--Main

main = (liftM head) getArgs  >>= readFile >>= texFmt
