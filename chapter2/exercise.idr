module Exercise

-- 1
exo1dot1  :  (String, String, String)
exo1dot1 = ("A", "B", "C")

exo1dot2 :  List String 
exo1dot2 = ["A", "B", "C"]

exo1dot3 : ((Char, String), Char)
exo1dot3 = (('A', "B"), 'C')

-- 2
palindrome : String -> Bool
palindrome x = reverse x == x

-- 3
palindrome': String -> Bool
palindrome' x = (toLower . reverse) x == (toLower x)

-- 4
palindrome'' : String -> Bool
palindrome'' str = length str > 10 && palindrome' str

-- 5
palindrome''' : Nat -> String -> Bool
palindrome''' n str = length str > n && palindrome' str

-- 6
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- 7
top_ten : Ord a => List a -> List a
top_ten = Prelude.List.take 10 . reverse . sort

-- 8
over_length : Nat -> List String -> Nat
over_length n = Prelude.List.length . filter (> n) . map length
