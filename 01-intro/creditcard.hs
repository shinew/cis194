numToList :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigit :: Integer -> Integer
sumDigits :: [Integer] -> Integer
procedure :: Integer -> Integer
luhn :: Integer -> Bool

numToList n
    | rest == 0 = [n]
    | otherwise = first : numToList rest
    where
        first = n `mod` 10
        rest = n `div` 10

doubleEveryOther [] = []
doubleEveryOther (x:[])  = x:[]
doubleEveryOther (x:y:xs) = x : (y * 2) : doubleEveryOther xs

sumDigit n = sum $ numToList n

sumDigits [] = 0
sumDigits (x:xs) = sumDigit x + sumDigits xs

procedure = (\x -> mod x 10) . sumDigits . doubleEveryOther . numToList

luhn n = procedure n == 0
