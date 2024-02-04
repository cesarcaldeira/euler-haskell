solution = sumEvenFibsTo 4000000

fibs = 1 : 1 : zipWith (+) fibs (tail fibs) -- Recursive Fibonacci numbers

sumEvenFibsTo x = sum (evenFibsTo x)
    where
        evenFibsTo x = [s | s <- takeWhile (<= x) fibs, even s]