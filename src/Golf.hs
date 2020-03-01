module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs =
  [[v | (i, v) <- zip [1 ..] xs, i `mod` n == 0] | n <- [1 .. (length xs)]]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:z:xs) =
  if (x < y && z < y)
    then y : (localMaxima (y : z : xs))
    else localMaxima (y : z : xs)
localMaxima _ = []

drawLine :: [(Integer, Int)] -> Int -> String
drawLine cs h =
  [ if ((snd (cs !! n)) >= h)
    then '*'
    else ' '
  | n <- [0 .. 9]
  ] ++
  "\n"

histogram :: [Int] -> String
histogram xs =
  let count c = length . filter (== c)
      counts = zip [0 .. 9] [count n xs | n <- [0 .. 9]]
      height = maximum $ map (snd) counts
   in concat [drawLine counts h | h <- [height,(height - 1) .. 1]] ++
      "==========\n0123456789\n"
