-- izčrpljive in iskalne množice
type Nat = Int
type Baire = Nat -> Nat
type J t = (t -> Bool) -> t
type K t = (t -> Bool) -> Bool

-- Lahki primer
f :: K Nat -> J Nat
f fi p = if fi p
		 then (f' fi p 0)
		 else (f fi (\n -> True))


f' fi p t = if fi (\n -> p n && n <= t)
			then t
			else f' fi p (t+1)
			
-- testic p = any p nji
testfi p = (p 2) || (p 4) || (p 6) || (p 8) || (p 10)
p11 n = n `mod` 3 == 0
p12 n = True
p13 n = False
p14 n = n > 10


-- Nelahki primer
g :: K Baire -> J Baire
g fi p = if fi p
		 then g' fi p
		 else g fi (\x -> True)

g' :: (K Baire) -> (Baire -> Bool) -> Baire
g' fi p n = miniT fi p n 0

prvihN :: Baire -> Baire -> Nat -> Bool
prvihN _ _ 0 = True
prvihN a b n = (a (n-1) == b (n-1)) && prvihN a b (n-1)

miniT fi p n t = if fi (\b -> (prvihN b (g' fi p) n) && b n == t && p b)
				 then t
				 else miniT fi p n (t+1)


memog :: K Baire -> J Baire
memog fi p = if fi p
		 then g' 
		 else g fi (\x -> True)
	where g' = (map g'' [0 ..] !!)
		where g'' n = miniT n 0
			where miniT n t = if fi (\b -> (prvihN b g' n) && b n == t && p b)
							  then t
							  else miniT n (t+1)
							  
testFi p = p (\n -> 2*n) || p (\n -> n+1) || p(\n -> 42)
p21 f = f 3 == 42
p22 f = f 2 < 4
p23 f = True
p24 f = False
p25 f = f 1 == 1


-- Grdi primer
pair :: Nat -> (Nat,Nat)
pair 0 = (0, 0)
pair n = let (p,xr) = divMod n 2
             (q,yr) = divMod p 2
             (x,y) = pair q
         in (xr + 2*x, yr + 2*y)


lst :: Nat -> [Nat]
lst 0 = []
lst n = let (a,b) = pair (n-1) in a : lst b

-- gosto zaporedje v Baire
d :: Nat -> Baire
d n = \m -> let l = lst n in 
						if m >= length l 
						then 0
						else l !! m

pP :: (Baire -> Nat) -> Baire
pP f = \n -> f (d n)

r :: K Baire -> Baire -> Baire
r fi a = \n -> if fi (\b -> (prvihN b (r fi a) n) && b n == a n)
			   then a n
			   else miniM n 0
					where miniM n m = if fi (\b -> (prvihN b (r fi a) n) && b n == m )
									  then m
									  else miniM n (m+1)
								  
eE :: K (Baire -> Nat) -> Baire -> (Baire -> Nat)
eE fi a = \x -> miniY x 0 where
						miniY x y = (if fi (\f -> (prvihN (pP f) (r (\p -> fi (p . pP)) a) (miniN x a 0)) && f x == y)
									 then y
									 else miniY x (y+1))
						miniN x a n = (if forall (\f -> forall (\g -> not (prvihNa a (pP f) (pP g) n) || f x == g x))
									   then n
									   else miniN x a (n+1))
						forall p = not (fi (\b -> not (p b)))
						prvihNa a f g 0 = True
						prvihNa a f g n = prvihNa a f g (n-1) && (f (n-1) == g (n-1) ) && (f (n-1) == a (n-1))
							  
h :: K (Baire -> Nat) -> J (Baire -> Nat)
h fi p = eE fi f'
	where f' = g (\p -> fi (p . pP)) (\y -> p (eE fi y))

fiNNN :: ((Baire -> Nat) -> Bool) -> Bool 
fiNNN p = p (\f -> 0) || p (\f -> f 1)
p31,p32,p33 :: (Baire -> Nat) -> Bool
p31 = \fF -> fF (\n -> 1) == 0
p32 = \fF -> fF (\n -> 1) == 2
p33 = \fF -> fF (\n -> 1) == 1

-- drugi poskus finda na Baire:
-- Imamo gosto zaporednje d_n funkcij na Baire

-- isciBaire fi p = if (fi p)
				 -- then pomo p 0
				 -- else isciBaire fi (\f -> True)
					-- where pomo p n = if p (d n)
									 -- then d n
									 -- else pomo p (n + 1)

-- prvih 20 lst-ov
-- [[0],[1],[0,0],[1,0],[2],[3],[2,0],[3,0],[0,1],[1,1],[0,0,0],[1,0,0],
-- [2,1],[3,1],[2,0,0],[3,0,0],[4],[5],[4,0],[5,0]]
-- opomba: d0 == d2, ampak je vseen ...

-- pTest :: Baire -> Bool
-- pTest = \f -> (f 0 == 2) && (f 1 == 0)

-- fiTest :: (Baire -> Bool) -> Bool
-- fiTest p = any p (map d [0..5]) -- vrne d_5


-- tezak primer 2:

-- ce je d gosto zaporednje tipa Baire, potem gosto zaporedje {F_n} tipa Baire -> Nat konstruiramo tako:
-- Fn (g) = 1. najdi ii =  argmin g =_M d_i, 0 <= i <= n, kjer je M = max(length lst i)
--			2. ce ii obstaja, vrni (d n) !! ii sicer 0

-- maksiDolz :: Nat -> Nat
-- maksiDolz n = if n == 0
			  -- then length $ lst 0
			  -- else max (maksiDolz (n-1)) (length $ lst n)
				
-- gostiF :: Nat -> (Baire -> Nat)
-- gostiF n = \g -> (pomo g 0) 
			-- where pomo g k | k > n = 0
						   -- | k <= n && (foldl (&&) True (zipWith (==) (map (d k) [0..dolz]) (map g [0..]))) = d n k -- ker je lst ravno bijekcija Nat -> končni [Nat], bo d n k ok vrednost
						   -- | otherwise = pomo g (k+1)
						   -- where dolz = (maksiDolz n) - 1
						   
-- stestirano gostiF: na majhnih ok, tudi na n = 142 (lst n = [3, 1, 1])

-- isciNNN fi p = \a -> if (fi p)
				     -- then pomo 0
				     -- else isciNNN fi (\f -> True)
						-- where pomo n = if p (gostiF n)
									   -- then gostiF n
									   -- else pomo p (n + 1)
												

















