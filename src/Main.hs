module Main where
import LoadCSV (loadCSV)

main :: IO ()
main = loadCSV "csv/test_file.csv" "t.db"


 
