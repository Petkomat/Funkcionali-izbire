import Izbire (K,Nat,Baire,isciN,isciNN,isciNNN,memoNN)

fiN :: K Nat
fiN p = any p [2,4,6,8,10]

p11 n = n `mod` 3 == 0
p12 n = True
p13 n = False
p14 n = n > 9

fiNN :: K Baire
fiNN p = p (\n -> 2*n) || p (\n -> n+1) || p(\n -> 42)

p21 f = f 3 == 42
p22 f = f 2 < 4
p23 f = True
p24 f = False
p25 f = f 1 == 1


fiNNN :: K (Baire -> Nat)
fiNNN p = p (\f -> 0) || p (\f -> f 1)

p31,p32,p33 :: (Baire -> Nat) -> Bool
p31 = \fF -> fF (\n -> 1) == 0
p32 = \fF -> fF (\n -> 1) == 2
p33 = \fF -> fF (\n -> 1) == 1
