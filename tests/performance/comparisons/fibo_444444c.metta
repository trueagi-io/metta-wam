
;; Turn off our memoization feature so we get a fair "raw" recursion test against RustHE
;; however our interpeter TCO's  EVERYTHING so we are in this case 10M+ times faster than RustHE
;; !(pragma! tabling false)
;; makes this file be treated as if the command line --compile=full  was supplied
!(pragma! compile full)

(= (fib $n)
    (if (== $n 0)
        0
        (if (== $n 1)
            1
            (+ (fib (- $n 1)) (fib (- $n 2))))))

!(println! (fib 444444))

; Execution took 63.44 seconds.




