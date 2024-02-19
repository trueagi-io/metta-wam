;; Takes Rust MeTTa  8.101 seconds
;; Takes MeTTaLog    0.002 seconds
;MeTTaLog only: !(pragma! compile full)
;MeTTaLog only: !(pragma! compile false)

(= (named-new-space $name)
   (bind! $name (new-space)))

(= (other-new-space)
   (bind! &other (new-space)))

(= (other2-new-space)
   (named-new-space &other2 (new-space)))

!(other-new-space)

!(add-atom
 &other 
 (= (fib $n)
  (if (== $n 0) 0 
    (if (== $n 1) 1
      (+ (fib (- $n 1))
         (fib (- $n 2)))))))

;; this would work in &self
!(match &other (= (fib 2) $code) $code)

;; this would not work in $code was called &self
!(match &other (= (fib 3) $code) $code)

!(other2-new-space)
!(bind! &other2  (new-space))
!(add-atom &other2 (= (foo) foo1))
!(add-atom &other2 (= (foo) foo2))

!(match &other2 (= (foo) $fooN) $fooN)


!(eval-in-space &other (fib 50))

!(eval-in-space &other2 (foo))

