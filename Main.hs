import System.Environment
import System.Exit
import Data.Char
import Text.Regex.Posix

data Lucky = Lucky | EvenLucky 

helpMessage :: IO ()
helpMessage = do
  putStrLn "                           what is displayed  (on a single line)"
  putStrLn "     argument(s)              (optional verbiage is encouraged)"
  putStrLn "======================|==================================================="
  putStrLn " j                    | Jth       lucky number                            "
  putStrLn " j  ,          lucky  | Jth       lucky number                            "
  putStrLn " j  ,      evenLucky  | Jth  even lucky number                            "
  putStrLn "                                                                          "
  putStrLn " j  k                 | Jth  through  Kth (inclusive)       lucky numbers "
  putStrLn " j  k          lucky  | Jth  through  Kth (inclusive)       lucky numbers "
  putStrLn " j  k      evenlucky  | Jth  through  Kth (inclusive)  even lucky numbers "
  putStrLn "                                                                          "
  putStrLn " j -k                 | all       lucky numbers in the range  j -> |k|    "
  putStrLn " j -k          lucky  | all       lucky numbers in the range  j -> |k|    "
  putStrLn " j -k      evenlucky  | all  even lucky numbers in the range  j -> |k|    "
  putStrLn "======================|==================================================="

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
range x x2 l = case l of
    Lucky -> let n = length (take x (luckyNumbers oddNumbers)) in
      drop (n-1) (take x2 (luckyNumbers oddNumbers))
    EvenLucky -> let n = length (take x (luckyNumbers evenNumbers)) in
      drop (n-1) (take x2 (luckyNumbers evenNumbers))

interval :: Int -> Int -> Lucky -> [Int]
interval x x2 l = let x'' = (-1 * x2) in
  case l of
    Lucky -> let n = length (takeWhile (<x) (luckyNumbers oddNumbers)) in
      drop n (takeWhile (<=x'') (luckyNumbers oddNumbers))
    EvenLucky -> let n = length (takeWhile (<x) (luckyNumbers evenNumbers)) in
      drop n (takeWhile (<=x'') (luckyNumbers evenNumbers))

lucky :: [String] -> Lucky
lucky xs = 
  if "evenLucky" `elem` xs
   then EvenLucky
   else Lucky

readn :: String -> Int
readn s = read s :: Int

isInt :: String -> Bool
isInt s = not (null (s =~ "-?[0-9]{0,10}" :: String))

main :: IO ()
main = do
  args <- getArgs
  if head args == "--help" || null args
    then
      helpMessage
    else
      let numArgs = map readn (filter isInt args) in
        if null numArgs
          then do
            print "Invalid input, missing arguments"
            print "Type --help"
            exitSuccess
          else 
            let l = lucky args in case length numArgs of
              1 -> do
                print (nth (head numArgs) l)
                exitSuccess
              2 -> if last numArgs > 0
                then do
                  print (range (head numArgs) (last numArgs) l)
                  exitSuccess
                else do
                  print (interval (head numArgs) (last numArgs) l)
                  exitSuccess
              _ -> do 
                print "Invalid input, wrong number of arguments"
                print "Type --help"
                exitSuccess
