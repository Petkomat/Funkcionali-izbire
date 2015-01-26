#Funkcije izbire

#####Avtorja: Matej Petkovič, Tomaž Stepišnik Perdih


Repozitorij za projekt Funkcije izbire (Matematika s funkcijskim programiranjem)

Datoteke s kodo se nahajajo v mapi `Koda`.
Lažje so berljive, če nastavimo širino tabulatorja na `4`.
To storimo tako, da URL-ju strani, na kateri beremo kodo,
na koncu dodamo `?ts=4`.


##Paper.hs

V tej datoteki je dokumentirana implementacija funkcij izbire in
spremeljevalne kode avtorja Martina Escarda, narejene po njegovem članku
What Sequential Games, the Tychonoff Theorem and the Double-Negation
Shift have in Common, 2010.

##TicTacToe.hs

V tej datoteki je dokumentirana implementacija uporabe funkcij izbire za
iskanje optimalnega igranja igre Tic Tac Toe. Avtor kode je Martin Escardo.

##NQueens.hs

V tej datoteki je dokumentirana implementacija uporabe funkcij izbire za
iskanje optimalnega igranja igre NQueens. Avtor kode je Martin Escardo.

##Tychonoff.hs

V tej datoteki je dokumentirana implementacija funkcije izbire na
Cantorjevem prostoru. Njen obstoj nam zagotavlja izrek Tihonova, ki
zagotavlja, da je produkt kompaktnih (iskalnih) množic tudi sam
kompaktna (iskalna) množica. Avtor kode je Martin Escardo.

##Selections.hs

V tej datoteki sva implementirala preslikave iz kvantifikatorjev v
funkcije izbire za prostore `Nat`, `Baire` in `Baire -> Nat`. Funkcije
so narejene po članku Exhaustible Sets in Higher-Type Computation,
Logical Methods in Computer Science, 2008, avtorja Martina Escarda.


##Examples.hs

V tej datoteki je nekaj testnih eksistenčnih kvantifikatorjev in predikatov
za preizkus funkcij definiranih v `Selections.hs`. Eksistenčni kvantifikator
za Cantorjev prostor je narejen po teh
[zapiskih](http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/)
Martina Escarda.
