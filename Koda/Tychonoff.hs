import Paper (J,K,overline,findBool,otimes,bigotimes)


-- | Selection function on Cantor space.
findCantor :: J Bool [Bool]
findCantor = bigotimes(repeat findBool)

-- | Functor of a monad @J r@.
image :: (x -> y) -> J r x -> J r y
image f e = \q -> f(e(\x -> q(f x)))

-- | Function that codes functions @Integer -> Bool@ as boolean lists.
code :: (Integer -> Bool) -> [Bool]
code f = [f(reindex i)| i<-[0..]]
  where reindex i | even i    =      i `div` 2
                  | otherwise = -((i+1)`div` 2)

-- | Inverse of function @code@.
decode :: [Bool] -> (Integer -> Bool)
decode xs i | i >= 0    =  xs `at`   (i * 2)
            | otherwise =  xs `at` ((-i * 2) - 1)

-- | Call @at xs n@ returns (n+1)-th element of a list @xs@.
at :: [x] -> Integer -> x
at (x:xs) 0 = x
at (x:xs) n = at xs (n-1)

-- | Selection function of functions @Integer -> Bool@.
findFunction :: J Bool (Integer -> Bool)
findFunction = image decode findCantor

-- | Existential quantifier over functions @Integer -> Bool@.
forsomeFunction :: K Bool (Integer -> Bool)
forsomeFunction = overline findFunction

-- | Universal quantifier over functions @Integer -> Bool@.
foreveryFunction :: K Bool (Integer -> Bool)
foreveryFunction p = 
   not(forsomeFunction(not.p))

-- | Function that checks equality of functionals @(Integer -> Bool) -> z@.
equal :: Eq z => ((Integer -> Bool) -> z) -> ((Integer -> Bool) -> z) -> Bool
equal f g = foreveryFunction(\u -> f u == g u)

-- | Function that maps @True@ to @1@ and @False@ to @0@.
coerce :: Bool -> Integer
coerce False = 0
coerce True  = 1

-- | Examples of functional useful for testing function @equal@.
f, g, h :: (Integer -> Bool) -> Integer

f a = coerce(a(7 * coerce(a 4) +  4 * (coerce(a 7)) + 4))

g a = coerce(a(7 * coerce(a 5) +  4 * (coerce(a 7)) + 4))

h a = if not(a 7)
      then if not(a 4) then coerce(a  4) else coerce(a 11)
      else if a 4      then coerce(a 15) else coerce(a  8)

-- | Calculates modulus of uniform continuity of given functional.
fan :: Eq z => ([Bool] -> z) -> Int
fan f = least(\n -> forevery(\a -> forevery(\b -> agree n a b --> (f a == f b))))

-- | Call @least p@ returns the smallest natural number n, such that @p n = True@.
least :: (Int -> Bool) -> Int
least p = if p 0 then 0 else 1 + least(\n -> p(n+1))

-- | Logical implication.
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- | Call @agree n as bs@ checks if sequences @as@ and @bs@ agree on the first @n@ terms.
agree :: Int -> [Bool] -> [Bool] -> Bool
agree n a b = take n a == take n b

-- | Existential and universal quantifier over Cantor space.
forsome, forevery :: K Bool [Bool]
forsome = overline findCantor 
forevery p = not(forsome(not.p))
