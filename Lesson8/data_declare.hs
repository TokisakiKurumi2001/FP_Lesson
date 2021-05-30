data Answer = Yes | No | Unk deriving Show

flip_answer :: Answer -> Answer
flip_answer Yes = No
flip_answer No = Yes
flip_answer Unk = Unk

main = do
  print(flip_answer No)
  print(flip_answer Unk)