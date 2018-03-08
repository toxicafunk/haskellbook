module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: Char -> String -> [String]
myLines sep s = reverse $ go s []
  where go "" acc = acc
        go l acc  = go (dropWhile isEndLine $ dropWhile (not . isEndLine) l) $ takeWhile (not . isEndLine) l : acc
          where
            isEndLine c = c == sep

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines '\n' sentences == shouldEqual)
