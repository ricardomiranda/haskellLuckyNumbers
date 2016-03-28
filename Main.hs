import System.Environment
import System.Exit
import Data.Char
import Text.Regex.Posix

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

range :: Int -> Int -> Lucky -> [Int]
range x x' l = case l of
    Lucky -> let n = length (take x (luckyNumbers oddNumbers)) in
      drop (n-1) (take x' (luckyNumbers oddNumbers))
    EvenLucky -> let n = length (take x (luckyNumbers evenNumbers)) in
      drop (n-1) (take x' (luckyNumbers evenNumbers))

interval :: Int -> Int -> Lucky -> [Int]
interval x x' l = let x'' = (-1 * x') in
  case l of
    Lucky -> let n = length (takeWhile (<x) (luckyNumbers oddNumbers)) in
      drop (n-1) (takeWhile (<x'') (luckyNumbers oddNumbers))
    EvenLucky -> let n = length (takeWhile (<x) (luckyNumbers evenNumbers)) in
      drop (n-1) (takeWhile (<x'') (luckyNumbers evenNumbers))

lucky :: [String] -> Lucky
lucky xs = 
  if (not (null (filter (== "evenLucky") xs)))
   then EvenLucky
   else Lucky

readn :: String -> Int
readn s = read s :: Int

isInt :: String -> Bool
isInt s = length (s =~ "-?[0-9]{0,10}" :: String) > 0

main :: IO ()
main = do
  args <- getArgs
  let numArgs = map readn (filter isInt args) in
    if null numArgs
      then do
        print "Invalid input, missing arguments"
        exitWith ExitSuccess
      else 
        let l = lucky args in case length numArgs of
	  1 -> do
            print (nth (head numArgs) l)
            exitWith ExitSuccess
          2 -> if last numArgs > 0
            then do
              print (range (head numArgs) (last numArgs) l)
              exitWith ExitSuccess
            else do
              print (interval (head numArgs) (last numArgs) l)
              exitWith ExitSuccess
          otherwise -> do 
            print "Invalid input, wrong number of arguments"
            exitWith ExitSuccess
