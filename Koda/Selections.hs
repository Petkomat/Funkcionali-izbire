{-|
Module      : Selections
Description : Module implementing mappings of existential quantifiers to selection functions.
Maintainer  : matej.petkovic@student.fmf.uni-lj.si,tomaz.stepisnik@student.fmf.uni-lj.si
Stability   : beta

These functions are constructed as described in paper Exhaustible Sets in Higher-Type Computation, Logical Methods in Computer Science, 2008, by Martin Escardo.
-}
module Selections
(
	-- * Type definitions
    Nat,
    Baire,
    J,
    K,
	-- * Selection functions
    findN,
    findNN,
    memoNN,
    findNNN
)
where

type Nat = Int
type Baire = Nat -> Nat
type J t = (t -> Bool) -> t
type K t = (t -> Bool) -> Bool

-- | Function mini finds the smallest natural number satisfying a given predicate, provided it exists.
mini :: (Nat -> Bool) -> Nat
mini p = mini' 0
    where mini' t = if p t then t else mini' (t+1)


-- | Mapping of existential quantifier for a subset of @Nat@ to its selection function.
findN :: K Nat -> J Nat
findN fi p = if fi p
             then mini (\t -> fi (\n -> p n && n == t))
             else findN fi (\n -> True)

-----------------------------------------------------------------------------------------------------

-- | Mapping of existential quantifier for a subset of @Baire@ to its selection function.
findNN :: K Baire -> J Baire
findNN fi p = if fi p
              then pomo
              else findNN fi (\x -> True)
              where pomo n = mini (\t -> fi (\b -> (prefix b pomo n) && b n == t && p b))

-- | Call @prefix a b n@ checks if sequences @a,b :: Baire@ match on their first @n@ terms.
prefix :: Baire -> Baire -> Nat -> Bool
prefix _ _ 0 = True
prefix a b n = (a (n-1) == b (n-1)) && prefix a b (n-1)

-- | Function @findNN@ implemented with memoization.
memoNN :: K Baire -> J Baire
memoNN fi p = if fi p
              then g
              else memoNN fi (\x -> True)
              where g = (map g' [0 ..] !!)
                    g' n = mini (\t -> fi (\b -> (prefix b g n) && b n == t && p b))

-------------------------------------------------------------------------------------------------------

-- | Bijection from @Nat@ to @(Nat,Nat)@.
pair :: Nat -> (Nat,Nat)
pair 0 = (0, 0)
pair n = let (p,xr) = divMod n 2
             (q,yr) = divMod p 2
             (x,y) = pair q
         in (xr + 2*x, yr + 2*y)

-- | Bijection from @Nat@ to finite lists of @Nat@.
lst :: Nat -> [Nat]
lst 0 = []
lst n = let (a,b) = pair (n-1) in a : lst b

-- | Dense sequence in @Baire@.
d :: Nat -> Baire
d n = \m -> let l = lst n in
            if m >= length l
            then 0
            else l !! m

-- | Encoding of functions in @Baire -> Nat@ with functions in @Baire@.
pP :: (Baire -> Nat) -> Baire
pP f = \n -> f (d n)

-- | Retraction of @Baire@ to its subset given by its existential quantifier.
r :: K Baire -> Baire -> Baire
r fi a = \n -> if fi (\b -> (prefix b (r fi a) n) && b n == a n)
               then a n
               else mini (\t -> fi (\b -> (prefix b (r fi a) n) && b n == t ))

-- | "Inverse" of function @pP@, such that (eE o pP) is retraction of @Baire -> Nat@ on a subset given by its existential quantifier.
eE :: K (Baire -> Nat) -> Baire -> (Baire -> Nat)
eE fi a x = mini (\t -> fi (\f -> (prefix (pP f) (r (\p -> fi (p . pP)) a) nN) && f x == t))
    where nN = mini (\n -> forall (\f -> forall (\g -> not (prefix' a (pP f) (pP g) n) || f x == g x)))
          forall p = not (fi (\b -> not (p b)))
          prefix' _ _ _ 0 = True
          prefix' f g h n = prefix' f g h (n-1) && f (n-1) == g (n-1) && f (n-1) == h (n-1)

-- | Mapping of existential quantifier for a subset of @Baire -> Nat@ to its selection function.
findNNN :: K (Baire -> Nat) -> J (Baire -> Nat)
findNNN fi p = eE fi f'
    where f' = memoNN (\p -> fi (p . pP)) (\y -> p (eE fi y))
