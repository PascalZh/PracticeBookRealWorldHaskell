import Data.Char (digitToInt, isDigit)

asInt_fold :: String -> Integer
asInt_fold "" = 0
asInt_fold ('-':ss) = negate $ asInt_fold ss
asInt_fold s = foldl step 0 s
    where step :: Integer -> Char -> Integer
          step x y = 10 * x + (toInteger $ digitToInt y)

asInt_fold' "" = 0
asInt_fold' ('-':ss) = negate $ asInt_fold' ss
asInt_fold' s = foldl (+) 0 (zipWith (*) (map digitToInt (reverse s)) (map (10^) [0..]))


type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Right 0
asInt_either ('-':ss) = negate <$> asInt_either ss
asInt_either s = foldl step (Right 0) s
    where step :: Either ErrorMessage Int -> Char -> Either ErrorMessage Int
          step (Left msg) y = Left msg
          step (Right x) y
              | isDigit y = Right $ 10 * x + (digitToInt y)
              | otherwise = Left ("non-digit '"++[y]++"'")

concat_fold :: [[a]] -> [a]
concat_fold = foldr (++) []

takeWhile_recursive :: (a -> Bool) -> [a] -> [a]
takeWhile_recursive p [] = []
takeWhile_recursive p (x:xs)
    | p x       = x : takeWhile_recursive p xs
    | otherwise = []

takeWhile_foldl :: (a -> Bool) -> [a] -> [a]
takeWhile_foldl p l = fst $ foldl (\(xs, flag) x -> if (p x) && flag then (xs++[x], flag) else (xs, False)) ([], True) l

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr p l = foldr step [] l
    where step x xs
              | not (p x) = []
              | otherwise = x : xs

test_takeWhile :: IO ()
test_takeWhile = if r1 == r2 && r1 == r3
                 then do putStrLn "Test takeWhile success."
                 else do print r1
                         print r2
                         print r3
    where p = odd
          l = [1, 3, 5, 6, 7, 8]
          r1 = takeWhile_recursive p l
          r2 = takeWhile_foldl p l
          r3 = takeWhile_foldr p l

main :: IO ()
main = test_takeWhile

