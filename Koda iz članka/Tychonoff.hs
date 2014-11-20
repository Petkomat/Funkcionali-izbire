type J r x = (x -> r) -> x 
type K r x = (x -> r) -> r

overline :: J r x -> K r x
overline e = \p -> p(e p)



findBool :: J Bool Bool
findBool p = p True

otimes :: J r x -> J r [x] -> J r [x]
otimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline e1 (\x1 -> p(x0:x1)))
        a1 = e1(\x1 -> p(a0 : x1))

bigotimes :: [J r x] -> J r [x]
bigotimes [] = \p -> []
bigotimes (e:es) = e `otimes` bigotimes es 

findCantor :: J Bool [Bool]
findCantor = bigotimes(repeat findBool)

image :: (x -> y) -> J r x -> J r y
image f e = \q -> f(e(\x -> q(f x)))

code :: (Integer -> Bool) -> [Bool]
code f = [f(reindex i)| i<-[0..]]
  where reindex i | even i    =      i `div` 2
                  | otherwise = -((i+1)`div` 2)

decode :: [Bool] -> (Integer -> Bool)
decode xs i | i >= 0    =  xs `at`   (i * 2)
            | otherwise =  xs `at` ((-i * 2) - 1)

at :: [x] -> Integer -> x
at (x:xs) 0 = x
at (x:xs) (n+1) = at xs n


findFunction :: J Bool (Integer -> Bool)
findFunction = image decode findCantor

forsomeFunction :: K Bool (Integer -> Bool)
forsomeFunction = overline findFunction

foreveryFunction :: K Bool (Integer -> Bool)
foreveryFunction p = 
   not(forsomeFunction(not.p))

equal :: Eq z => ((Integer -> Bool) -> z) -> ((Integer -> Bool) -> z) -> Bool
equal f g = foreveryFunction(\u -> f u == g u)


coerce :: Bool -> Integer
coerce False = 0
coerce True  = 1

f, g, h :: (Integer -> Bool) -> Integer

f a = coerce(a(7 * coerce(a 4) +  4 * (coerce(a 7)) + 4))

g a = coerce(a(7 * coerce(a 5) +  4 * (coerce(a 7)) + 4))

h a = if not(a 7)
      then if not(a 4) then coerce(a  4) else coerce(a 11)
      else if a 4      then coerce(a 15) else coerce(a  8)

fan :: Eq z => ([Bool] -> z) -> Int
fan f = least(\n -> forevery(\a -> forevery(\b -> agree n a b --> (f a == f b))))

least :: (Int -> Bool) -> Int
least p = if p 0 then 0 else 1 + least(\n -> p(n+1))

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

agree :: Int -> [Bool] -> [Bool] -> Bool
agree n a b = take n a == take n b

forsome, forevery :: K Bool [Bool]
forsome = overline findCantor 
forevery p = not(forsome(not.p))
