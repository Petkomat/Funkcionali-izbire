n = 8

type Move = Int 
type Position = (Move, Move)
type R = Bool

attacks :: Position -> Position -> Bool
attacks (x, y) (a, b) = x == a  ||  y == b  ||  abs(x - a) == abs(y - b)

valid :: [Position] -> Bool
valid [] = True
valid (u : vs) =  not(any (\v -> attacks u v) vs) && valid vs

p :: [Move] -> R
p ms = valid (zip ms [0..(n-1)])

type J r x = (x -> r) -> x

epsilons :: [[Move] -> J R Move]
epsilons = replicate n epsilon
  where epsilon h = find ([0..(n-1)] `setMinus` h)

find :: [x] -> J Bool x
find []     p = undefined
find [x]    p = x
find (x:xs) p = if p x then x else find xs p

otimes :: J r x -> (x -> J r [x]) -> J r [x]
otimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline (e1 x0) (\x1 -> p(x0:x1)))
        a1 = e1 a0 (\x1 -> p(a0 : x1))
        overline e p = p(e p)

bigotimes :: [[x] -> J r x] -> J r [x]
bigotimes [] = \p -> []
bigotimes (e : es) =  
 e [] `otimes` (\x -> bigotimes[\xs->d(x:xs) | d <- es])

optimalPlay :: [Move]
optimalPlay = bigotimes epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

optimalStrategy :: [Move] -> Move
optimalStrategy as = head(bigotimes epsilons' p')
 where epsilons' = drop (length as) epsilons
       p' xs = p(as ++ xs)

delete :: Ord x => x -> [x] -> [x]
delete x [] = []
delete x (vs@(y : ys))
    | x == y    = ys 
    | x <  y    = vs
    | otherwise = y : delete x ys 

setMinus :: Ord x => [x] -> [x] -> [x]
setMinus xs [] = xs
setMinus xs (y : ys) = setMinus (delete y xs) ys


main = 
  putStr ("An optimal play for " ++ show n ++  "-Queens is "
  ++ show optimalPlay ++ "\nand the optimal outcome is " 
  ++ show optimalOutcome ++ "\n")
