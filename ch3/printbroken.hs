-- printbroken.hs
module PrintBroken where

printSecond :: String -> IO()
printSecond s = do
  putStrLn s

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
  where greeting = "Yarrrr"
