{-
TP4 Programmation Fonctionnelle
Fichier contenant 
Matthieu Caron
Arnaud Cojez
-}

--Q1
data Arbre coul val = Vide | Cons coul val (Arbre coul val) (Arbre coul val) deriving Show

--Q2
mmap :: (valA -> valB) -> Arbre coul valA -> Arbre coul valB
mmap _ Vide = Vide
mmap f (Cons c v (ag) (ad)) = Cons c (f v) (mmap f ag) (mmap f ad)