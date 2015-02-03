{-
TP4 Programmation Fonctionnelle
Fichier contenant 
Matthieu Caron
Arnaud Cojez
-}

--Q1
data Arbre coul val = Vide | Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show

--Q2
mmap :: (valA -> valB) -> Arbre coul valA -> Arbre coul valB
mmap _ Vide = Vide
mmap f (Noeud c v (ag) (ad)) = Noeud c (f v) (mmap f ag) (mmap f ad)

mfold :: (coul -> val -> b -> b -> b) -> b -> Arbre coul val -> b
mfold _ r Vide = r
mfold op r (Noeud c v (ag) (ad)) = 
  op c v (mfold op r ag) (mfold op r ad)
  
test = Noeud "rouge" 1 (Noeud "rouge" 2 (Noeud "rouge" 4 Vide Vide) Vide) (Noeud "rouge" 3 Vide Vide)
       
{-

*Main> mmap (\x -> -x) test
Noeud "rouge" (-1) (Noeud "rouge" (-2) (Noeud "rouge" (-4) Vide Vide) Vide) (Noeud "rouge" (-3) Vide Vide)

*Main> mmap (\x -> 1) test
Noeud "rouge" 1 (Noeud "rouge" 1 (Noeud "rouge" 1 Vide Vide) Vide) (Noeud "rouge" 1 Vide Vide)


*Main> mfold (+) 0 test
10
*Main> mfold (:) [] test
[1,3,2,4]
-}

--Q3

