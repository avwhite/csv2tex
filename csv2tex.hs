module Main where	

import Text.Parsec 
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

repSepBy1 n p sep = do
	x <- p
	xs <- replicateM (n-1) (sep >> p)
	return (x:xs)
	

--New Parser

csvParser = do
	head <- header
	newline
	tail <- sepEndBy (row $ length head) newline
	return (head:tail)
header = sepBy1 cell (char ',')
row n = repSepBy1 n cell (char ',')
cell = qcell <|> ncell
ncell = many (escChar <|> noneOf ",\n")
qcell = between (char '"') (char '"') (many $ escChar <|> noneOf "\"") 
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
