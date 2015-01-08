type Nat = Int
type Baire = Nat -> Nat
type J t = (t -> Bool) -> t
type K t = (t -> Bool) -> Bool

-- Lahek primer
f :: K Nat -> J Nat
f fi p = if fi p
		 then (f' fi p 0)
		 else (f fi (\n -> True))


f' fi p t = if fi (\n -> p n && n <= t)
			then t
			else f' fi p (t+1)
			

testfi p = (p 2) || (p 4) || (p 6) || (p 8) || (p 10)
p11 n = n `mod` 3 == 0
p12 n = True
p13 n = False
p14 n = n > 10


-- Nelahek primer
g :: K Baire -> J Baire
g fi p = if fi p
		 then g' 
		 else g fi (\x -> True)
	where g' = (map g'' [0 ..] !!)
		where g'' n = miniT n 0
			where miniT n t = if fi (\b -> (prvihN b g' n) && b n == t && p b)
							  then t
							  else miniT n (t+1)

prvihN :: Baire -> Baire -> Nat -> Bool
prvihN _ _ 0 = True
prvihN a b n = (a (n-1) == b (n-1)) && prvihN a b (n-1)


testFi p = p (\n -> 2*n) || p (\n -> n+1) || p(\n -> 42)
p21 f = f 3 == 42
p22 f = f 2 < 4
p23 f = True
p24 f = False
p25 f = f 1 == 1