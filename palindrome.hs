isPalindromicInt :: Int -> Bool
isPalindromicInt n
    | length(show n) == 1 = True
    | isPalindromicString(show n) = True
    | otherwise = False

isPalindromicString :: String -> Bool
isPalindromicString n
    | n == [] = True
    | length n == 1 = True
    | head n /= last n = False
    | isPalindromicString(tail(init n)) == True = True
    | otherwise = False

hasTwoFactorsOfLength :: Int -> Int -> Bool
hasTwoFactorsOfLength x n
    | x `isMultipleOf` factors = True
    | otherwise = False
    where factors = [xs | xs <- factorsOf x, xs < (10 ^ n) && xs > 10 ^ (n - 1)]

isMultipleOf :: Int -> [Int] -> Bool
isMultipleOf x factors
    | factors == [] = False
    | head factors * last factors == x = True
    | x `isMultipleOf` init factors = True
    | x `isMultipleOf` tail factors = True
    | otherwise = False

factorsOf :: Int -> [Int]
factorsOf n = [x | x <- [1..n], n `rem` x == 0]

largestPossibleProduct :: Int -> Int
largestPossibleProduct n = ((10 ^ n) ^ 2-1)

palindromicNumbersWithFactorsOfLength :: Int -> [Int]
palindromicNumbersWithFactorsOfLength n = [x | x <- [largestPossibleProduct n,largestPossibleProduct n -1..1], isPalindromicInt x && x `hasTwoFactorsOfLength` n]

palindromicNumbersWithFactorsOfLengthSmallFirst :: Int -> [Int]
palindromicNumbersWithFactorsOfLengthSmallFirst n = [x | x <- [1..largestPossibleProduct n], isPalindromicInt x && x `hasTwoFactorsOfLength` n]

largestPossiblePalindromicNumberWithFactorsOfLength :: Int -> Int
largestPossiblePalindromicNumberWithFactorsOfLength n = head (palindromicNumbersWithFactorsOfLength n)