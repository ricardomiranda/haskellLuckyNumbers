import System.Environment
import System.Exit

data Lucky = Lucky | EvenLucky 

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

nth :: Int -> Lucky -> Int
nth j l = case l of 
  Lucky -> last (take j (luckyNumbers oddNumbers))
  EvenLucky -> last (take j (luckyNumbers evenNumbers))

lucky :: [String] -> Lucky
lucky xs =
 if (not (null (filter (== "evenLucky") xs)))
   then EvenLucky
   else Lucky

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      print "Invalid input, missing arguments"
      exitWith ExitSuccess
    else 
      let l = lucky args in do
        print (nth (read (head args) :: Int) l)
        exitWith ExitSuccess
