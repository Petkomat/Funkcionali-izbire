module Izbire
(
    Nat,
    Baire,
    J,
    K,
    isciN,
    isciNN,
    isciNNN,
    memoNN
)
where

type Nat = Int
type Baire = Nat -> Nat
type J t = (t -> Bool) -> t
type K t = (t -> Bool) -> Bool

-- Funkcija, ki poisče najmanjse naravno število, ki zadošča predikatu.
-- Kot funkcija izbire na celih naravnih številih, uporabimo jo lahko le, če vemo da rešitev obstaja.
mini :: (Nat -> Bool) -> Nat
mini p = mini' 0
    where mini' t = if p t then t else mini' (t+1)


-- Lahki primer
isciN :: K Nat -> J Nat
isciN fi p = if fi p
             then mini (\t -> fi (\n -> p n && n == t))
             else (isciN fi (\n -> True))

-----------------------------------------------------------------------------------------------------

-- Nelahki primer
isciNN :: K Baire -> J Baire
isciNN fi p = if fi p
              then pomo
              else isciNN fi (\x -> True)
              where pomo n = mini (\t -> fi (\b -> (prvihN b pomo n) && b n == t && p b))

-- Funkcija, ki za dve zaporedji naravnih števil preveri, če se ujemata na prvih nekaj členih.
prvihN :: Baire -> Baire -> Nat -> Bool
prvihN _ _ 0 = True
prvihN a b n = (a (n-1) == b (n-1)) && prvihN a b (n-1)

-- Funkcija isciNN z memoizacijo.
memoNN :: K Baire -> J Baire
memoNN fi p = if fi p
              then g'
              else memoNN fi (\x -> True)
              where g' = (map g'' [0 ..] !!)
                    g'' n = mini (\t -> fi (\b -> (prvihN b g' n) && b n == t && p b))

-------------------------------------------------------------------------------------------------------

-- Grdi primer

-- Bijekcija med naravnimi števili in pari naravnih števil.
pair :: Nat -> (Nat,Nat)
pair 0 = (0, 0)
pair n = let (p,xr) = divMod n 2
             (q,yr) = divMod p 2
             (x,y) = pair q
         in (xr + 2*x, yr + 2*y)

-- Bijekcija med naravnimi števili in končnimi seznami naravnih števil.
lst :: Nat -> [Nat]
lst 0 = []
lst n = let (a,b) = pair (n-1) in a : lst b

-- gosto zaporedje v Baire
d :: Nat -> Baire
d n = \m -> let l = lst n in
            if m >= length l
            then 0
            else l !! m

-- Kodiranje funkcij
pP :: (Baire -> Nat) -> Baire
pP f = \n -> f (d n)

-- Retrakcija funkcij na podmnožico Baire, ki jo podamo z njenim eksistenčnim kvantifikatorjem.
r :: K Baire -> Baire -> Baire
r fi a = \n -> if fi (\b -> (prvihN b (r fi a) n) && b n == a n)
               then a n
               else mini (\t -> fi (\b -> (prvihN b (r fi a) n) && b n == t ))

-- Tako odkodiranje funkcij, da je (eE o pP) retrakt prostora Baire -> Nat na množico, na množico, podano z eksistenčnim kvantifikatorjem.
eE :: K (Baire -> Nat) -> Baire -> (Baire -> Nat)
eE fi a x = mini (\t -> fi (\f -> (prvihN (pP f) (r (\p -> fi (p . pP)) a) nN) && f x == t))
    where nN = mini (\n -> forall (\f -> forall (\g -> not (prvihN' a (pP f) (pP g) n) || f x == g x)))
          forall p = not (fi (\b -> not (p b)))
          prvihN' _ _ _ 0 = True
          prvihN' f g h n = prvihN' f g h (n-1) && f (n-1) == g (n-1) && f (n-1) == h (n-1)


isciNNN :: K (Baire -> Nat) -> J (Baire -> Nat)
isciNNN fi p = eE fi f'
    where f' = isciNN (\p -> fi (p . pP)) (\y -> p (eE fi y))
