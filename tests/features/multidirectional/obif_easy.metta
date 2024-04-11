; Define the base cases.
(= (fib  0 0) )
(= (fib  1 1) )
; Define the recursive case using CLP(FD) constraints.
(= (fib $N $F) 
  (#>-p $N 1)  ;;Ensure that N is greater than 1.
  (- $N 1 $N1) ;;Define N1 as N - 1.
  (- $N 2 $N2) ;;Define N2 as N - 2.
  (fib $N1 $F1) ;;Recursive call to find the (N-1)th Fibonacci number.
  (fib $N2 $F2) ;;Recursive call to find the (N-2)th Fibonacci number. 
  (+ $F1 $F2 $F)) ; The Nth Fibonacci number is the sum of the (N-1)th and (N-2)th Fibonacci numbers.

; Example query for finding the Nth Fibonacci number:
;
; ?- fib(10, F).
;
; F = 55.
!(let $_ (fib 10 $Y) $Y)

; Example query for finding the position N of a given Fibonacci number:
;
; ?- fib(N, 55).
;
; N = 10.
!(let $_ (fib $X 55) $X)




