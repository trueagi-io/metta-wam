
;; makes this file be treated as if the command line --compile=false  was supplied
!(pragma! compile false)

(= (fib $n)
    (if (== $n 0)
        0
        (if (== $n 1)
            1
            (+ (fib (- $n 1)) (fib (- $n 2))))))

!(println! (fib 20))



