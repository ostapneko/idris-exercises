module Main

palindrome': String -> Bool
palindrome' x = (toLower . reverse) x == (toLower x)

main : IO ()
main = repl "Enter a string: " (\ s => show (palindrome' s) ++ "\n")
