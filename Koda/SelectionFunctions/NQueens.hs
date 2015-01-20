{-|
Module      : NQueens
Description : Finding optimal strategy for NQueens game.
Maintainer  : matej.petkovic@student.fmf.uni-lj.si,tomaz.stepisnik@student.fmf.uni-lj.si
Stability   : beta

We have a N x N chess board. Two players take turns placing queens on the board. On i-th turn a queen must be placed in i-th column so that previously placed queens are not attacking it.
This module can be used to calculate optimal moves using selection functions implemented as described in paper What Sequential Games, 
the Tychonoff Theorem and the Double-Negation Shift have in Common, 2010, by Martin Escardo.
-}
module SelectionFunctions.NQueens
where

import SelectionFunctions.Paper (J,find,dotimes,dbigotimes)

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


-- | List of history depentent selection functions of moves, 
-- where k-th selection function returns optimal move on k-th turn.
epsilons :: [[Move] -> J R Move]
epsilons = replicate n epsilon
  where epsilon h = find ([0..(n-1)] `setMinus` h)


-- |Optimal play for NQueens.
optimalPlay :: [Move]
optimalPlay = dbigotimes epsilons p

-- | Optimal outcome of NQueens
optimalOutcome :: R
optimalOutcome = p optimalPlay

-- | Returns an optimal move, given previous moves.
optimalStrategy :: [Move] -> Move
optimalStrategy as = head(dbigotimes epsilons' p')
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


-- main = 
  -- putStr ("An optimal play for " ++ show n ++  "-Queens is "
  -- ++ show optimalPlay ++ "\nand the optimal outcome is " 
  -- ++ show optimalOutcome ++ "\n")
