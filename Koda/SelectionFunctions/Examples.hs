{-|
Module      : Examples
Description : Module for testing selection functions from module SelectionFunctions 
Maintainer  : matej.petkovic@student.fmf.uni-lj.si,tomaz.stepisnik@student.fmf.uni-lj.si
Stability   : beta

Existential quantifier for Cantor space was defined with help from this blog post by Martin Escardo http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/
-}
module SelectionFunctions.Examples
(
	-- * Examples for @Nat@
	fiN,
	p11,
	p12,
	p13,
	p14,
	-- * Examples for @Baire@
	fiNN,
	fiCantor,
	p21,
	p22,
	p23,
	p24,
	p25,
	-- * Exapmles for @Baire -> Nat@
	fiNNN,
	fiImage,
	p31,
	p32,
	p33,
	p34
)
where

import SelectionFunctions.Selections (K,Nat,Baire,findN,findNN,findNNN,memoNN)

-- | Existential quantifier for set {2,4,6,8,10}.
fiN :: K Nat
fiN p = any p [2,4,6,8,10]

-- | Some predicates on Nat.
p11 n = n `mod` 3 == 0
p12 n = True
p13 n = False
p14 n = n > 9

-- | Existential quantifier for the set of three functions in Baire.
fiNN :: K Baire
fiNN p = any p [\n -> 2*n, \n -> n+1, \n -> 42]

-- | Some predicates on Baire.
p21 f = f 3 == 42
p22 f = f 2 < 4
p23 f = True
p24 f = False
p25 f = f (f 1) == 3

-- | Existential quantifier for the set of zwei functions in Baire -> Nat.
fiNNN :: K (Baire -> Nat)
fiNNN p = p (\f -> 2) || p (\f -> f 1)

-- | Some predicates on Baire -> Nat.
p31,p32,p33,p34 :: (Baire -> Nat) -> Bool
p31 = \fF -> fF (\n -> 3) == fF(\n -> 2 * n+1)
p32 = \fF -> fF (\n -> 1) == 2
p33 = \fF -> fF (\n -> 1) == 1
p34 = \fF -> fF (\n -> 0) == 0

-- Functions needed for fiCantor.
findBit :: (Nat -> Bool) -> Nat
findBit p = if p 0 then 0 else 1
branch :: Nat -> Baire -> Baire -> Baire
branch x l r n | n == 0 = x
               | odd n = l ((n-1) `div` 2)
               | otherwise = r ((n-2) `div` 2)
find :: (Baire -> Bool) -> Baire
find p = branch x l r
    where x = findBit(\x -> forsome(\l -> forsome(\r -> p(branch x l r))))
          l = find(\l -> forsome(\r -> p(branch x l r)))
          r = find(\r -> p(branch x l r))
          forsome p = p(find(\a -> p a))

-- | Existential quantifier for Cantor space in Baire.
fiCantor :: K Baire
fiCantor p = p (find p)


image :: Baire -> (Baire -> Nat)
image c = \f -> sum (zipWith (*) (map c [0..(f 2)]) (map f [0..(f 2)]))

-- | Existential quantifier for continuous image of Cantor space in Baire -> Nat. 
fiImage :: K (Baire -> Nat)
fiImage p = fiCantor (p . image)


