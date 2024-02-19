
;; makes this file be treated as if the command line --compile=full  was supplied
!(pragma! compile full)

(= (fib $n)
    (if (== $n 0)
        0
        (if (== $n 1)
            1
            (+ (fib (- $n 1)) (fib (- $n 2))))))


!(call-fn (argv-metta 1))
!(println! (fib (call-fn (argv-metta 1))))



