-- Declare new data by providing Value to them
data Answer = Yes | No | Unk deriving Show

flip_answer :: Answer -> Answer
flip_answer Yes = No
flip_answer No = Yes
flip_answer Unk = Unk

-- Declare new data with more new Data
data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi^2 * r
area (Rect w h) = w * h

square :: Float -> Shape
square n = Rect n n

-- Param to new Data
data Option a = None | Some a

safediv :: Int -> Int -> Option Int
safediv _ 0 = None
safediv x y = Some (x `div` y)

extractable :: Option a -> Bool
extractable None = False
extractable (Some x) = True

extract :: Option a -> a
extract None = error "This is an error"
extract (Some x) = x

proc_safediv :: Int -> Int -> IO()
proc_safediv x y = do
  if (extractable (safediv x y)) then do
    let val = extract (safediv x y)
    print(val) 
    return()
  else do
    print("Cannot divide by 0")
    return()

-- Recursive data
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add x y = int2nat (nat2int x + nat2int y)

-- No conversions is needed
add_recur :: Nat -> Nat -> Nat
add_recur Zero n = n
add_recur (Succ m) n = Succ (add_recur m n)

main = do
  print(flip_answer No)
  print(flip_answer Unk)
  print(area (Circle 6.0))
  print(area (Rect 5.0 10.0))
  print(area (square 4.0))
  proc_safediv 10 0
  proc_safediv 5 5
  print(nat2int (Succ (Succ (Succ (Succ Zero)))))
  print(nat2int (int2nat 4))
  print(nat2int (add (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ (Succ Zero)))))
  print(nat2int (add_recur (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ (Succ Zero)))))