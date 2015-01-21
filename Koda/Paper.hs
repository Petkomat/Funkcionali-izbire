{-|
Module      : Paper
Description : Module implementing selection functions.
Maintainer  : matej.petkovic@student.fmf.uni-lj.si,tomaz.stepisnik@student.fmf.uni-lj.si
Stability   : beta

Module implements functions and examples described in paper What Sequential Games,
the Tychonoff Theorem and the Double-Negation Shift have in Common, 2010, by Martin Escardo.
-}

module Paper 
(
	J,
	K,
	overline,
	find,
	findnot,
	forsome,
	forevery,
	argsupBool,
	supBool,
	findBool,
	argsup,
	arginf,
	otimes,
	bigotimes,
	dotimes,
	dbigotimes,
	bigunion,
	mcons,
	sequence',
	hsequence,
	Id,
	fibonacci
)
where

-- | Selection functions on type @x@, where @r@ is a type of generalized truth values.
type J r x = (x -> r) -> x

-- | Generalized quantifiers over the type @x@, where @r@ is a type of generalized truth values.
type K r x = (x -> r) -> r

-- | Function mapping selection functions to quantifiers.
overline :: J r x -> K r x
overline e = \p -> p(e p)

-- | Finds an element of non-empty finite list that satisfies a predicate.
--   If no such element exists it returns the last element.
find :: [x] -> J Bool x
find [] p = undefined
find [x] p = x
find (x : xs) p = if p x then x else find xs p

-- | Finds an element of non-empty finite list that doesn't satisfy a predicate.
--   If no such element exists it returns the last element.
findnot :: [x] -> J Bool x
findnot [] p = undefined
findnot [x] p = x
findnot (x : xs) p = if p x then findnot xs p else x

-- | Existential quantifier over non-empty finite lists.
forsome :: [x] -> K Bool x
forsome = overline.find

-- | Universal quantifier over non-empty finite lists. The two definitions are equivalent.
forevery, forevery' :: [x] -> K Bool x
forevery xs p = not(forsome xs (not.p))
forevery' = overline.findnot

-- | Returns argsup of predicate @Bool -> Int@.
argsupBool :: J Int Bool
argsupBool p = p True > p False

-- | Returns sup of predicates @Bool -> Int@.
supBool :: K Int Bool
supBool = overline argsupBool

-- | Function @find@ on a list @[True, False]@.
findBool :: J Bool Bool
findBool p = p True

-- | Equivalent definition of function @findBool@.
findBool' :: J Bool Bool
findBool' p = if p True then True else False

-- | Returns argsup of predicate @x -> Int@ over a list.
argsup' :: [x] -> J Int x
argsup' [] p = undefined
argsup' [x] p = x
argsup' (x:y:zs) p = argsup' ((if p x < p y then y else x):zs) p

-- | More efficient implementation of function @argsup'@ if range of the predicate is -1,0,1.
-- It avoids re-evaluations of p and stops when the maximum value is reached.
argsup :: [x] -> J Int x
argsup     [] p = undefined
argsup (x:xs) p = f xs x (p x)
  where f    xs  a   1  = a
        f     [] a   r  = a
        f (x:xs) a (-1) = f xs x (p x)
        f    xs  a   0  = g xs
         where g [] = a
               g (x:xs) | p x == 1  = x
                        | otherwise = g xs


-- | Returns arginf of predicate @x -> Int@ whose range is -1,0,1.
-- It avoids re-evaluations of p and stops when the minimum value is reached.
arginf :: [x] -> J Int x
arginf     [] p = undefined
arginf (x:xs) p = f xs x (p x)
  where f    xs  a (-1) = a
        f    []  a   r  = a
        f (x:xs) a   1  = f xs x (p x)
        f    xs  a   0  = g xs
         where g (x:xs) = if p x == -1 then x else g xs
               g    []  = a

-- | Equivalent definition of function @arginf@.
arginf' xs p = argsup xs ((1-).p)

-- | Binary product of selection functions.
otimes :: J r x0 -> J r x1 -> J r (x0,x1)
otimes e0 e1 p = (a0,a1)
  where a0 = e0(\x0 -> overline e1 (\x1 -> p(x0,x1)))
        a1 = e1(\x1 -> p(a0,x1))

-- | Special case of binary product of selection functions needed for the implementation of iterated product.
otimes' :: J r x -> J r [x] -> J r [x]
otimes' e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline e1 (\x1 -> p(x0:x1)))
        a1 = e1(\x1 -> p(a0 : x1))

-- | Iterated product of a list of selection functions.
bigotimes :: [J r x] -> J r [x]
bigotimes [] = \p -> []
bigotimes (e:es) = e `otimes'` bigotimes es

-- | Equivalent definition of iterated product.
bigotimes' :: [J r x] -> J r [x]
bigotimes' [] p = []
bigotimes' (e : es) p = x0 : bigotimes' es (p.(x0:))
   where x0 = e(\x -> p(x : bigotimes' es (p.(x:))))


-- | "History" dependent binary product of selection functions.
dotimes :: J r x -> (x -> J r [x]) -> J r [x]
dotimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline (e1 x0) (\x1 -> p(x0:x1)))
        a1 = e1 a0 (\x1 -> p(a0:x1))

-- | "History" dependent iterated product of selection functions.
dbigotimes :: [[x] -> J r x] -> J r [x]
dbigotimes [] = \p -> []
dbigotimes (e : es) =
 (e [])
 `dotimes`
 (\x -> dbigotimes (map (\e xs -> e(x:xs)) es))

-- | Multiplication in monad @J r@.
bigunion :: J r (J r x) -> J r x
bigunion e = \p -> e(\d -> overline d p) p

-- | Generalization of function @otimes'@ which works on any monad.
mcons :: Monad m => m x -> m[x] -> m[x]
xm `mcons` xsm =
    do x <- xm
       xs <- xsm
       return (x:xs)

-- | Alternative definition of the standard prelude function @sequence@,
-- which is a generalization of function @bigotimes@ to any monad.
sequence' :: Monad m => [m x] -> m[x]
sequence' [] = return []
sequence' (xm : xms) =
    do x <- xm
       xs <- sequence' xms
       return(x : xs)

-- | Generalization of function @dbigotimes@ to any monad.
hsequence :: Monad m => [[x] -> m x] -> m[x]
hsequence [] = return []
hsequence (xm : xms) =
    do x <- xm []
       xs <- hsequence[\ys -> ym(x:ys) | ym <- xms]
       return(x : xs)

-- | Identity monad on type x.
newtype Id x = Id { di :: x} deriving (Show)

instance Monad Id where
    return a     = Id a
    (Id x) >>= f = f x

-- | Computing Fibonacci numbers using @hsequence@ on type @Id Integer@.
fibonacci :: [Integer]
fibonacci = di(hsequence (repeat f))
  where f [ ] = Id 1
        f [_] = Id 1
        f xs  = Id((xs !! (i - 1)) + (xs !! i))
          where i = length xs - 1
