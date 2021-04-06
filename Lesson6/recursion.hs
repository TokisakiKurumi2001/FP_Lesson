fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [y | y <- xs, y < x]
    larger = [z | z <- xs, x <= z]

main = do
  print(fac 4)
  --print(fac (-1))
  print(product' [1..4])
  print(length' [1,3,5,2,6])
  print(qsort [1,6,2,7])
