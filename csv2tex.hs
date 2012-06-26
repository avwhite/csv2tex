module Main where	

import Text.ParserCombinators.Parsec 
import Control.Monad
import System.Environment

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

fmtLine :: [String] -> String
fmtLine [] = ""
fmtLine (l:[]) = l ++ " \\\\\n"
fmtLine (l:ls) = l ++ " & " ++ (fmtLine ls)

fmtCsv :: [[String]] -> String
fmtCsv = concat . fmap fmtLine

--Main

main = (liftM head) getArgs  >>= readFile >>= texFmt
