-- first assignment - validation of credit card numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 = []
  | otherwise =
    case (n `divMod` 10) of
      (0, r) -> [r]
      (m, r) -> r : toDigitsRev m

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ zipWith (*) (cycle [1, 2]) (reverse xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (go x) + sumDigits xs
    where go n = (div n 10) + (mod n 10)

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- second assignment - towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)