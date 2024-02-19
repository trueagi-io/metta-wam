;; Takes HVM   Regular           0.0009 seconds ;; Guessed
;; Takes Rust  Regular           0.0009 seconds ;; Guessed
;; Takes Rust MeTTa              24.611 seconds ;; Tested
;; Takes MeTTaLog   Regular       90.6667 seconds ;; Tested
;; Takes MeTTaLog   Compiled-Tabling   0.017 seconds  ;; Tested
;; Takes MeTTaLog   Compiled           2.017 seconds ;; Guessed

;; MeTTaLog only: 
!(pragma! tabling true)
;; MeTTaLog only: 
!(pragma! compiler full)

 (=
  (fib $n)
  (if
    (== $n 0) 0
    (if
      (== $n 1) 1
      (+
        (fib
          (- $n 1))
        (fib
          (- $n 2))))))

!(fib 10)

!(fib 20)

!(fib 80)


