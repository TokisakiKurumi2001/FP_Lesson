-- Make code more readable
type Pos = (Int, Int)

origin :: Pos
origin  = (0, 0)

mv_left :: Pos -> Pos
mv_left (x, y) = (x - 1, y)

-- Type with parameter
type Pair a = (a, a)

mult :: Pair Float -> Float
mult (x, y) = x * y

double :: Char -> Pair Char
double c = (c, c)

-- Nested type
type Trans = Pos -> Pos

mv_right :: Trans
mv_right (x, y) = (x + 1, y)

main = do
  print(mv_left (1, 1))
  print(mult (1.5, 2.0))
  print(double 'a')
  print(mv_right origin)