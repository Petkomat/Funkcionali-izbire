-- | Board size.
n = 8

-- | Move is the number of column in which a queen is placed.
type Move =  Int

-- | Coordinates on the board.
type Position = (Int, Int)

-- | Type of truth values that represent game outcomes.
type R = Bool

-- | Call @attacks p1 p2@ checks if a queen in position @p1@ attacks position @p2@.
attacks :: Position -> Position -> Bool
attacks (x, y) (a, b) = x == a  ||  y == b  ||  abs(x - a) == abs(y - b)

-- | Function that checks if placement of queens on given positions is valid.
valid :: [Position] -> Bool
valid [] = True
valid (u : vs) =  not(any (\v -> attacks u v) vs) && valid vs

-- | Predicate on move sequences which tests the validity of resulting queen placement.
p :: [Move] -> R
p ms = valid (zip ms [0..(n-1)])

-- to spet importava
type J r x = (x -> r) -> x

-- | List of history depentent selection functions of moves, 
-- where k-th selection function returns optimal move on k-th turn.
epsilons :: [[Move] -> J R Move]
epsilons = replicate n epsilon
  where epsilon h = find ([0..(n-1)] `setMinus` h)

-- to tut
find :: [x] -> J Bool x
find []     q = undefined
find [x]    q = x
find (x:xs) q = if q x then x else find xs q

-- to tut
otimes :: J r x -> (x -> J r [x]) -> J r [x]
otimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline (e1 x0) (\x1 -> p(x0:x1)))
        a1 = e1 a0 (\x1 -> p(a0 : x1))
        overline e p = p(e p)

-- to tut
bigotimes :: [[x] -> J r x] -> J r [x]
bigotimes [] = \p -> []
bigotimes (e : es) =  
 e [] `otimes` (\x -> bigotimes[\xs->d(x:xs) | d <- es])

-- |Optimal play for NQueens.
optimalPlay :: [Move]
optimalPlay = bigotimes epsilons p

-- | Optimal outcome of NQueens
optimalOutcome :: R
optimalOutcome = p optimalPlay

-- | Returns an optimal move, given previous moves.
optimalStrategy :: [Move] -> Move
optimalStrategy as = head(bigotimes epsilons' p')
 where epsilons' = drop (length as) epsilons
       p' xs = p(as ++ xs)

-- | Function that removes an element from an ordered list.
delete :: Ord x => x -> [x] -> [x]
delete x [] = []
delete x (vs@(y : ys))
    | x == y    = ys 
    | x <  y    = vs
    | otherwise = y : delete x ys 

-- | Call @setMinus xs ys@ returns a list with elements of @xs@ that are not contained in @ys@.
setMinus :: Ord x => [x] -> [x] -> [x]
setMinus xs [] = xs
setMinus xs (y : ys) = setMinus (delete y xs) ys


main = 
  putStr ("An optimal play for " ++ show n ++  "-Queens is "
  ++ show optimalPlay ++ "\nand the optimal outcome is " 
  ++ show optimalOutcome ++ "\n")
