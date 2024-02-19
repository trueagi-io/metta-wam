;!(extend-py! mettalog)


; these examples actually need the compiler!
!(pragma! compile full)

!(pragma! trace-on-pass False)
!(pragma! trace-on-fail True)
!(pragma! trace-on-overflow True)
!(pragma! trace-on-error True)
!(pragma! trace-length 2)

; Recursive Fibonacci
(= (fib 0) 0)
(= (fib 1) 1)
(= (fib $n) (+ (fib (- $n 1)) (fib (- $n 2))))

; Tail-Recursive Fibonacci
(= (fib-tail $n) (fib-tail-helper $n 0 1))
(= (fib-tail-helper 0 $a $_) $a)
(= (fib-tail-helper $n $a $b) (fib-tail-helper (- $n 1) $b (+ $a $b)))

; Fibonacci Using Accumulators
(= (fib-acc $n) (fib-acc-helper $n (, 0 1)))
(= (fib-acc-helper 0 (, $a $_)) $a)
(= (fib-acc-helper $n (, $a $b)) (fib-acc-helper (- $n 1) (, $b (+ $a $b))))


;!(assertEqual (fib 0) 0)
;!(assertEqual (fib-tail 0) 0)
;!(assertEqual (fib-acc 0) 0)

;!(assertEqual (fib 1) 1)
;!(assertEqual (fib-tail 1) 1)
;!(assertEqual (fib-acc 1) 1)

;!(assertEqual (fib 2) 1)
;!(assertEqual (fib-tail 2) 1)
;!(assertEqual (fib-acc 2) 1)

!(assertEqual (fib 3) 2)
;!(assertEqual (fib-tail 3) 2)
;!(assertEqual (fib-acc 3) 2)

;!(assertEqual (fib 4) 3)
;!(assertEqual (fib-tail 4) 3)
;!(assertEqual (fib-acc 4) 3)

;!(assertEqual (fib 5) 5)
;!(assertEqual (fib-tail 5) 5)
;!(assertEqual (fib-acc 5) 5)

;!(assertEqual (fib 6) 8)
;!(assertEqual (fib-tail 6) 8)
;!(assertEqual (fib-acc 6) 8)

;!(assertEqual (fib 7) 13)
;!(assertEqual (fib-tail 7) 13)
;!(assertEqual (fib-acc 7) 13)

;!(assertEqual (fib 8) 21)
;!(assertEqual (fib-tail 8) 21)
;!(assertEqual (fib-acc 8) 21)

;!(assertEqual (fib 9) 34)
;!(assertEqual (fib-tail 9) 34)
;!(assertEqual (fib-acc 9) 34)

;!(assertEqual (fib 10) 55)
;!(assertEqual (fib-tail 10) 55)
;!(assertEqual (fib-acc 10) 55)


;!(mettalog::vspace-main)

