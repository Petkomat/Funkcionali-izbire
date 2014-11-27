-- The board and moves are
--                                    012
--                                    345
--                                    678
-- R is the set 3 = {-1,0,1}
-- assumption: player X starts

-- | Type of truth values that represent game outcomes.
type R = Int

-- | Type used to describe player moves. 
type Move = Int

-- | Type used to describe board state. Each list represents sequence of moves of a player.
type Board = ([Move], [Move])

-- | Type of players - X and O.
data Player = X | O

-- | Function that checks if the game ends given after a given sequence of moves.
wins :: [Move] -> Bool
wins = 
 someContained [[0,1,2],[3,4,5],[6,7,8],
                [0,3,6],[1,4,7],[2,5,8],
                [0,4,8],[2,4,6]]

-- | Function that computes outcome of a board. 
value :: Board -> R
value (x,o) | wins x    =  1 
            | wins o    = -1 
            | otherwise =  0

-- | Function that takes the current player and a list of moves of both players 
-- and computes final state of the board.
outcome :: Player -> [Move] -> Board -> Board
outcome whoever [] board = board
outcome X (m : ms) (x, o) = 
  if wins o then (x, o) else outcome O ms (insert m x, o)
outcome O (m : ms) (x, o) = 
  if wins x then (x, o) else outcome X ms (x, insert m o)

 -- | Predicate on lists of moves, that computes outcome of a game given a sequence of moves.
p :: [Move] -> R
p ms = value(outcome X ms ([],[]))

-- to bova importala
type J r x = (x -> r) -> x

-- | List of history depentent selection functions of moves, where n-th selection function returns optimal move on n-th turn.
epsilons :: [[Move] -> J R Move]
epsilons = take 9 all
  where all = epsilonX : epsilonO : all
        epsilonX history = argsup ([0..8] `setMinus` history) 
        epsilonO history = arginf ([0..8] `setMinus` history) 

-- to bova importala
argsup, arginf :: [x] -> J Int x
argsup    []  p = undefined
argsup (x:xs) p = f xs x (p x)
  where f    xs  a   1  = a 
        f    []  a   r  = a
        f (x:xs) a (-1) = f xs x (p x) 
        f    xs  a   0  = g xs
         where g (x:xs) = if p x == 1 then x else g xs
               g    []  = a

-- to tut
arginf xs p = argsup xs (\x -> - p x)

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

-- | Optimal moves for Tic Tac Toe.
optimalPlay :: [Move]
optimalPlay = bigotimes epsilons p

-- | Optimal outcome of Tic Tac Toe.
optimalOutcome :: R
optimalOutcome = p optimalPlay

-- | Returns an optimal move, given previous moves.
optimalStrategy :: [Move] -> Move
optimalStrategy as = head(bigotimes epsilons' p')
   where epsilons' = drop (length as) epsilons
         p' xs = p(as ++ xs)

-- | Call @contained xs ys@ checks if an ordered list @ys@ contains all elements of an ordered list @xs@.
contained :: Ord x => [x] -> [x] -> Bool
contained [] ys = True
contained xs [] = False
contained (us@(x : xs)) (y : ys) 
    | x == y    = contained xs ys
    | x >= y    = contained us ys
    | otherwise = False

-- | Call @someContained xss ys@ returns true iff some list @xs@ in @xss@ is contained in @ys@.
someContained :: Ord x => [[x]] -> [x] -> Bool
someContained [] ys = False
someContained xss [] = False
someContained (xs : xss) ys = contained xs ys || someContained xss ys

-- | Function that inserts an element in an ordered list so that it remains ordered.
insert :: Ord x => x -> [x] -> [x]
insert x [] = [x]
insert x (vs@(y : ys)) 
    | x == y       = vs
    | x <  y       = x : vs
    | otherwise    = y : insert x ys

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


main :: IO ()
main = putStr ("An optimal play for Tic-Tac-Toe is "
             ++ show optimalPlay
             ++ "\nand the optimal outcome is " ++ show optimalOutcome ++ "\n")
