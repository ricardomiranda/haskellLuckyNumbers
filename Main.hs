import System.Environment


oddNumbers :: [Int]
oddNumbers = filter odd [1..]


evenNumbers :: [Int]
evenNumbers = filter even [1..]


luckyNumbers :: [Int] -> [Int]
luckyNumbers xs = 
  let i = 3 in
  sieve i xs
    where
      sieve i (ln:s:xs) =
        ln : sieve (i + 1) (s : [x | (n, x) <- zip [i..] xs, rem n s /= 0])


lastN :: Int -> Int
lastN j = 
  last (take j (luckyNumbers oddNumbers))


main :: IO ()
main = do
  j <- getArgs
--  putStrLn (show (lastN j))
  putStrLn (show (3))
