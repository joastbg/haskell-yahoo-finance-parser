-- Parses a Yahoo-finance CSV-file
-- using Attoparsec http://hackage.haskell.org/package/attoparsec

import Control.Applicative
import Data.Attoparsec.Char8
import Data.Word
import Data.Time
import System.Environment
import qualified Data.ByteString as B

data YahooEntry =
    YahooEntry { date      :: LocalTime
               , open      :: Double
               , high      :: Double
               , low       :: Double
               , close     :: Double
               , volume    :: Integer
               , adjclose  :: Double
               } deriving  (Eq, Show)

type YahooEntries = [YahooEntry]

-- Parse a date (YYYY-MM-DD)
timeParser :: Parser LocalTime
timeParser = do
    y  <- count 4 digit
    char '-'
    mm <- count 2 digit
    char '-'
    d  <- count 2 digit  
    return $ LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
                     , localTimeOfDay = TimeOfDay 0 0 0
                     }

-- Parse a row (Date,Open,High,Low,Close,Volume,Adj Close)
rowParser :: Parser YahooEntry
rowParser = do
    let sepChar = ','
    let spaceSkip = many $ satisfy $ inClass [' ' , '\t']
        sepParser = spaceSkip >> char sepChar >> spaceSkip      
    spaceSkip
    t  <- timeParser
    sepParser  
    o <- double
    sepParser
    h <- double
    sepParser
    l <- double
    sepParser
    c <- double
    sepParser
    v <- decimal
    sepParser
    a <- double
    spaceSkip
    return $ YahooEntry t o h l c v a

-- Parse the Yahoo-finance CSV-file
csvParser :: Parser YahooEntries
csvParser = many $ rowParser <* endOfLine

main :: IO ()
main = do
    args <- getArgs
    let csvFile = args !! 0
    putStrLn ("Parsing: " ++ csvFile)
    file <- B.readFile csvFile
    case parseOnly csvParser file of
        Left  error     -> putStrLn $ "Error while parsing " ++ csvFile ++ ": " ++ error
        Right entries   -> mapM_ print entries
        
        

