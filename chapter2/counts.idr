module Main

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

main : IO ()
main = repl "Enter a string: " (\ s => show (counts s) ++ "\n")
