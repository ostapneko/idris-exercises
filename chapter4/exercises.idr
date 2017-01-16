-- 4.1.5
import Data.Vect

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

insert : Ord a => (x : a) -> (tree : Tree a) -> Tree a
insert x Empty = Node Empty x Empty
insert x t@(Node l e r) =
    case compare x e of
         LT => Node (insert x l) e r
         EQ => t
         GT => Node l e (insert x r)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = let rest = listToTree xs in
                           insert x rest

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node x y z) = treeToList x ++ (y :: treeToList z)

data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = Just $ (the Double x) * y / 2
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate _ y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

-- 4.2.4
data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : Vehicle Electric
  ElectricCar : Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels Tram = 10
wheels ElectricCar = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50

vectTakeReverse : (n : Nat) -> Vect (n + p) a -> Vect p a
vectTakeReverse Z xs = xs
vectTakeReverse (S k) (x :: xs) = vectTakeReverse k xs

vectTake : (n : Nat) -> Vect (n + p) a -> Vect p a
vectTake n xs = reverse $ vectTakeReverse n (reverse xs)

maybeAt : Num a => (pos : Integer) -> Vect n a -> Maybe a
maybeAt {n} pos xs =
    case integerToFin pos n of
         Just pos' => Just $ Vect.index pos' xs
         _ => Nothing

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys =
    case (maybeAt pos xs, maybeAt pos ys) of
         (Just a, Just b) => Just (a + b)
         _ => Nothing
