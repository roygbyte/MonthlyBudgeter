{-# LANGUAGE UnicodeSyntax #-}

import System.Environment
import System.IO
import System.IO.Error

{-|
 Takes a predicate and a string and returns an array of strings
 Basically, break apart a string by the delimiter!
-}
{- Note on use of ', from S/O: In general, the suffixing of a ' means one of three things: foo' is either a helper definition made for the purpose of defining foo, a modified version of foo (that is, state, state', and state'' could be an initial state and two updated versions), or a strict version of foo.-}
wordsSplitAt ∷ (Char → Bool) → String → [String]
wordsSplitAt p s = case dropWhile p s of
  "" → []
  s' → w : wordsSplitAt p s''
       where (w, s'') = break p s'
{-|
 Takes an array of strings and returns an integer
 by reading each String as an integer value
-}
sumStrings ∷ [String] -> Float
sumStrings [] = 0
sumStrings (x:xs) =  (read x) + (sumStrings xs)
{-|
 Take a predicate and an array and return an array with results filtered out
 which do not satisfy the predicat
-}
filter' ∷ (a → Bool) → [a] → [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
{-|
 Takes an array of string arrays and returns an array
 Basically, takes a 2D array and returns an array made up of
 tail elements of the 2D array…
-}
takeLastElements ∷ [[a]] → [a]
takeLastElements [] = []
takeLastElements ((x:xs):ys) = [head $ flipArray xs] ++ takeLastElements ys
{-|
 Take a 2D array and returns a 1D array containing all first
 ∈ of each array
-}
takeFirstElems ∷ [[a]] → [a]
takeFirstElems [] = []
takeFirstElems ((x:xs):ys) = [x] ++ takeFirstElems ys
{-|
 Take a 2D array and return a 1D array containing all ∈ at specified position
-}
takeSpecificElements ∷ [[a]] → Int → [a]
takeSpecificElements [] _ = []
takeSpecificElements ((x):ys) index = [(x !! index)] ++ takeSpecificElements ys index
{-|
 Take a 1D array and flip ∈ ordering
-}
flipArray ∷ [a] → [a]
flipArray [] = []
flipArray (x:xs) = flipArray xs ++ [x]
{-|
 Takes a string, asks for an input, returns input 
-}
prompt msg = do
  putStrLn msg
  value ← getLine
  return value
{-|
 Take a integer of days, integer of amount, inter of days,
 and return an integer.
 Basically, take an amount period (x/initialDays) and return an amount
 adjusted for the desired periode (x/endDays)

 https://stackoverflow.com/questions/10303251/haskell-converting-int-to-float
-}
amountForPeriod ∷ Int → Float → Int → Float
amountForPeriod x y z = do 
  ( y' / x' ) * z'
  where x' = fromIntegral x ∷ Float
        y' = y ∷ Float
        z' = fromIntegral z ∷ Float
{-|
 Take a fileName, read a file, return a list of column values
-}
csv_getLastCols ∷ String → [String]
csv_getLastCols contents = do
  lastCols
  where rows = map (wordsSplitAt (==',')) $ lines contents
        lastCols = takeLastElements rows
{-|
 Take a fileName, read a file, return a list of column values
-}
csv_getColsAt ∷ String → Int → [String]
csv_getColsAt contents index = do
  cols
  where rows = map (wordsSplitAt (==',')) $ lines contents
        cols = takeSpecificElements rows index

main = do

  -- Get Visa CSV
  contents ← readFile "statements/accountactivity.csv"

  let visa = csv_getColsAt contents 2
      visa_filtered = map (filter (`elem` ['0'..'9'] ++ ['.'])) visa
      visa_summed = sumStrings visa_filtered

  print $ visa_filtered
  print $ visa_summed

  -- Get Earnings Information
  let biweekly = 3000.00 ∷ Float
      earnings = (*2) biweekly 

  -- Get Expense Information
  handle ← openFile "expenses.txt" ReadMode
  contents ← hGetContents handle

  -- Calculate earnings and expenses
  let expenseItems = map (wordsSplitAt (==':')) $ lines contents
      expenseValues = takeLastElements expenseItems
      expenseTotalCost = sumStrings expenseValues
      earningsMinusExpenses = earnings - expenseTotalCost

  -- Show earnings and expenses report
  putStrLn $ show expenseItems
  putStrLn $ "Total monthly earnings: " ++ show earnings
  putStrLn $ "\x1b[31m" ++ "Total monthly expenses: " ++ show expenseTotalCost ++ "\x1b[0m"
  putStrLn $ ""
  putStrLn $ "Left for savings: " ++ show earningsMinusExpenses
  putStrLn $ ""
  putStrLn $ "Yearly savings: " ++ show ((*12) earningsMinusExpenses)
  putStrLn $ "Yearly earnings: " ++ show (amountForPeriod 15 biweekly 365)
  putStrLn $ "Yearly expenses: " ++ show (amountForPeriod 30 expenseTotalCost 365)

-- https://bugfactory.io/blog/implementing-rubys-array-flatten-in-haskell/
