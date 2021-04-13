twice :: (a -> a) -> a -> a
twice f x = f (f x)

doubleVal :: (Num a) => a -> a
doubleVal x = 2 * x

product' :: (Num a) => [a] -> a
product' = foldr (*) 1

length' :: [a] -> a
length' xs = foldr (\_ n -> n + 1) xs

main = do
  print(twice doubleVal 3)
  print(product' [1..4])