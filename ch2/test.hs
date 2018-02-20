sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

triple :: Int -> Int
triple x = x * 3

timesPi :: Int -> Int
timesPi x = 3.14 * x

timesPi' :: Int -> Int
timesPi' x = pi * x
