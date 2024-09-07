module Log where

logFile :: String
logFile = "log.txt"

writeLog :: String -> IO ()
writeLog = appendFile logFile . flip (++) "\n"

cleanLog :: IO ()
cleanLog = writeFile logFile ""
