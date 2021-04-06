flatten :: [[a]] -> [a]
flatten xss = [x | xs <- xss, x <- xs]

factors :: (Integral a) => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: (Integral a) => a -> Bool
prime n = (factors n == [1,n])

allprimeto :: (Integral a) => a -> [a]
allprimeto n = [x | x <- [2..n], prime x]

pairadjacent :: [a] -> [(a,a)]
pairadjacent xs = zip xs (tail xs)

issort :: (Ord a) => a -> Bool
issort xs = [x <= y | (x, y) <- pairs xs]

main = do
  print([(x, y) | x <- [1,2,3], y <- [4,5]])
  -- order matter
  print([(y, x) | x <- [1,2,3], y <- [4,5]])
  -- dependant generator
  print([(x, y) | x <- [1..3], y <- [x..3]])
  -- flatten
  print(flatten [[1,2,3], [4,5], [6]])
  -- guard
  print([x | x <- [1..10], even x])
  -- more complex example
  print(factors 15)
  -- prime number - haskell style
  print(prime 13)
  print(prime 40)
  -- all prime number from 2 to n
  print(allprimeto 40)
  -- zip function
  print(zip ['a', 'b', 'c'] [1..4])
  -- pairs all the adjacent element in the list
  print(pairadjacent [1..4])
  -- check if the array is sort
  print(issort [1..4])
