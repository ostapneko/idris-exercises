-- 3.2.4

import Data.Vect

length' : List a -> Nat
length' [] = 0
length' (x :: xs) = 1 + length xs

append : (x : a) -> List a -> List a
append x xs = xs ++ [x]

reverse' : List a -> List a
reverse' [] = []
reverse' (x :: xs) = append x (reverse xs)

map' : (a -> b) -> List a -> List b
map' f [] = []
map' f (x :: xs) = f x :: map' f xs

map'' : (a -> b) -> Vect n a -> Vect n b
map'' f [] = []
map'' f (x :: xs) = f x :: map'' f xs

-- 3.3.3

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

createZeros : Num a => Vect n a
createZeros = replicate _ 0

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

addColumn : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect 1 (Vect m a)
addColumn x y = [zipWith (+) x y]

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: []) (y :: []) = addColumn x y
addMatrix (x :: xs) (y :: ys) = addColumn x y ++ addMatrix xs ys

scalarProd : Num a => Vect m a -> Vect m a -> a
scalarProd xs ys = sum $ zipWith (*) xs ys

multLine : Num a => (yTrans : Vect p (Vect m a)) -> Vect m a -> Vect p a
multLine yTrans xs = map (scalarProd xs) yTrans

multMatrix' : Num a => (xs : Vect n (Vect m a)) -> (yTrans : Vect p (Vect m a)) -> Vect n (Vect p a)
multMatrix' xs yTrans = map (multLine yTrans) xs

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = let yTrans = transposeMat ys in
                       multMatrix' xs yTrans
