bigotimes' :: [(x -> r) -> x] -> ([x] -> r) -> [x]
bigotimes' [] p = []
bigotimes' (e : es) p = x0 : bigotimes' es (p.(x0:))
   where x0 = e(\x -> p(x : bigotimes' es (p.(x:))))

type J r x = (x -> r) -> x 
type K r x = (x -> r) -> r

overline :: J r x -> K r x
overline e = \p -> p(e p)

find :: [x] -> J Bool x
find [] p = undefined
find [x] p = x
find (x : xs) p = if p x then x else find xs p

findnot :: [x] -> J Bool x
findnot [] p = undefined
findnot [x] p = x
findnot (x : xs) p = if p x then findnot xs p else x

forsome, forevery' :: [x] -> K Bool x
forsome = overline.find 
forevery xs p = not(forsome xs (not.p))
forevery' = overline.findnot

argsupBool :: J Int Bool
argsupBool p = p True > p False

sup :: K Int Bool
sup = overline argsupBool  

findBool :: J Bool Bool
findBool p = p True

findBool' p = if p True then True else False

argsup' :: [x] -> J Int x
argsup' [] p = undefined
argsup' [x] p = x
argsup' (x:y:zs) p = argsup' ((if p x < p y then y else x):zs) p


{--

argsup :: [x] -> J Int x
argsup       [] p = undefined
argsup (x : xs) p = f xs x (p x)
   where f           xs  a  1 = a 
         f           []  a  r = a
         f     (x : xs)  a(-1)= f xs x (p x) 
         f (us@(x : xs)) a  0 = g us
            where g (x : xs) = if p x == 1 then x else g xs
                  g       [] = a
--}

argsup :: [x] -> J Int x
argsup     [] p = undefined
argsup (x:xs) p = f xs x (p x)
  where f    xs  a   1  = a 
        f    []  a   r  = a
        f (x:xs) a (-1) = f xs x (p x) 
        f    xs  a   0  = g xs
         where g (x:xs) = if p x == 1 then x else g xs
               g    []  = a


arginf :: [x] -> J Int x
arginf     [] p = undefined
arginf (x:xs) p = f xs x (p x)
  where f    xs  a (-1) = a 
        f    []  a   r  = a
        f (x:xs) a   1  = f xs x (p x) 
        f    xs  a   0  = g xs
         where g (x:xs) = if p x == -1 then x else g xs
               g    []  = a

arginf' xs p = argsup xs ((1-).p)

otimes :: J r x -> J r [x] -> J r [x]
otimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline e1 (\x1 -> p(x0:x1)))
        a1 = e1(\x1 -> p(a0 : x1))

bigotimes :: [J r x] -> J r [x]
bigotimes [] = \p -> []
bigotimes (e:es) = e `otimes` bigotimes es 

otimes' :: J r x0 -> J r x1 -> J r (x0,x1)
otimes' e0 e1 p = (a0,a1)
  where a0 = e0(\x0 -> overline e1 (\x1 -> p(x0,x1)))
        a1 = e1(\x1 -> p(a0,x1))

argsup'' :: [x] -> J Int x
argsup''     [] p = undefined
argsup'' (x:xs) p = f xs x (p x)
  where f    xs  a   1  = a 
        f     [] a   r  = a
        f (x:xs) a (-1) = f xs x (p x) 
        f    xs  a   0  = g xs
         where g [] = a
               g (x:xs) | p x == 1  = x 
                        | otherwise = g xs
               

dotimes :: J r x -> (x -> J r [x]) -> J r [x]
dotimes e0 e1 p = a0 : a1
  where a0 = e0(\x0 -> overline (e1 x0) (\x1 -> p(x0:x1)))
        a1 = e1 a0 (\x1 -> p(a0:x1))

dbigotimes :: [[x] -> J r x] -> J r [x]
dbigotimes [] = \p -> []
dbigotimes (e : es) =  
 (e []) 
 `dotimes` 
 (\x -> dbigotimes (map (\e xs -> e(x:xs)) es))

bigunion :: J r (J r x) -> J r x
bigunion e = \p -> e(\d -> overline d p) p


mcons :: Monad m => m x -> m[x] -> m[x]
xm `mcons` xsm =
    do x <- xm            
       xs <- xsm
       return (x:xs)

sequence' :: Monad m => [m x] -> m[x]
sequence' [] = return []
sequence' (xm : xms) = 
    do x <- xm
       xs <- sequence' xms
       return(x : xs)

hsequence :: Monad m => [[x] -> m x] -> m[x]
hsequence [] = return []
hsequence (xm : xms) = 
    do x <- xm []
       xs <- hsequence[\ys -> ym(x:ys) | ym <- xms]
       return(x : xs)

newtype Id x = Id { di :: x} deriving (Show)
 
instance Monad Id where
    return a     = Id a   
    (Id x) >>= f = f x

fibonacci :: [Integer]
fibonacci = di(hsequence (repeat f))
  where f [ ] = Id 1
        f [_] = Id 1
        f xs  = Id((xs !! (i - 1)) + (xs !! i))
          where i = length xs - 1
