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

-- Multiplication
mult_recur :: Nat -> Nat -> Nat
mult_recur Zero _ = Zero
mult_recur (Succ Zero) x = x
mult_recur (Succ x) y = add_recur y (mult_recur x y)

-- Expression for data declaration
data Expr = Val Int | Add Expr Expr | Mult Expr Expr

size :: Expr -> Int
size (Val _) = 1
size (Add x y) = size x + size y
size (Mult x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mult x y) = eval x * eval y

-- Building Tree
-- In this tree, each leaf must have its value. Therefore, this is a binary tree
data Tree a = Leaf a | Node a (Tree a) (Tree a)
-- More custom tree is
-- data AdvTree a = Leaf | Node a (Tree a) (Tree a)
-- This time, if it is Leaf, it likes NULL on other languages

printTree :: Tree Int -> IO()
printTree (Leaf x) = do
  print(x)
  return()
printTree (Node x y z) = do
  print(x)
  printTree y
  printTree z
  return()

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
  print(nat2int (mult_recur (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ (Succ Zero)))))
  print(size (Add (Val 1) (Mult (Val 2) (Val 3))))
  print(eval (Add (Val 1) (Mult (Val 2) (Val 3))))
  printTree (Node 3 (Node 4 (Leaf 5) (Leaf 7)) (Node 10 (Leaf 7) (Node 8 (Leaf 13) (Leaf 15))))