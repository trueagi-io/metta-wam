;
;                   ---- Naive Recursion Method ----

;
;                    Summary: Simple and direct implementation using the basic definition of Fibonacci numbers.

;
;                    Pros: Easy to understand and implement.

;
;                    Cons: Highly inefficient for large N due to recalculations of already computed Fibonacci numbers; exponential time complexity.


  (= 
    (fib_naive  0 0) True)
  (= 
    (fib_naive  1 1) True)
  (= 
    (fib-naive $N $F) 
    ( (> $N 1) 
      (is $N1 
        (- $N 1)) 
      (is $N2 
        (- $N 2)) 
      (fib-naive $N1 $F1) 
      (fib-naive $N2 $F2) 
      (is $F 
        (+ $F1 $F2))))

;
;                      ---- Memoization (Dynamic Programming) Method ----

;
;                      Summary: Uses memoization to store previously calculated Fibonacci numbers, avoiding recalculations and improving efficiency.

;
;                      Pros: Efficient even for large N due to no recalculations; polynomial time complexity.

;
;                      Cons: Uses additional memory to store the previously calculated Fibonacci numbers.


  !(dynamic (/ was-fib-memo 2))

  (= 
    (fib-memo 0 0) 
    (set-det))
  (= 
    (fib-memo 1 1) 
    (set-det))
  (= 
    (fib-memo $N $F) 
    ( (> $N 1) 
      (is $N1 
        (- $N 1)) 
      (is $N2 
        (- $N 2)) 
      (det-if-then-else 
        (was-fib-memo $N1 $F1) True 
        (, 
          (fib-memo $N1 $F1) 
          (add-atom  &self 
            (was_fib_memo  $N1 $F1)))) 
      (det-if-then-else 
        (was-fib-memo $N2 $F2) True 
        (, 
          (fib-memo $N2 $F2) 
          (add-atom  &self 
            (was_fib_memo  $N2 $F2)))) 
      (is $F 
        (+ $F1 $F2)) 
      (add-atom  &self 
        (was_fib_memo  $N $F))))


;
;                        ---- Tail Recursion with Accumulators Method ----

;
;                         Summary: Calculates the Fibonacci sequence using tail recursion with accumulators, maintaining a constant stack size.

;
;                         Pros: Efficient and uses a constant amount of stack space.

;
;                         Cons: Slightly more complex due to the use of accumulators and a helper predicate.


  (= 
    (fib-tail-recursive $N $F) 
    (fib-tail-recursive $N 0 1 $F))
  (= 
    (fib_tail_recursive  0 $A $_ $A) True)
  (= 
    (fib-tail-recursive $N $A $B $F) 
    ( (> $N 0) 
      (is $N1 
        (- $N 1)) 
      (is $Sum 
        (+ $A $B)) 
      (fib-tail-recursive $N1 $B $Sum $F)))



;
;                           ---- Tabling Method ----

;
;                           Summary: Uses tabling to store intermediate results, avoiding redundant calculations and improving efficiency.

;
;                           Pros: Efficient even for large N due to no recalculations; polynomial time complexity.

;
;                            Cons: Uses additional memory to store the intermediate results; support for tabling is not available in all MeTTa implementations.


  !(table (/ fib-tabled 2))

  (= 
    (fib_tabled  0 0) True)
  (= 
    (fib_tabled  1 1) True)
  (= 
    (fib-tabled $N $F) 
    ( (> $N 1) 
      (is $N1 
        (- $N 1)) 
      (is $N2 
        (- $N 2)) 
      (fib-tabled $N1 $F1) 
      (fib-tabled $N2 $F2) 
      (is $F 
        (+ $F1 $F2))))





;
;                              List of all the Fibonacci implementations


  (= 
    (fibonaccis  
      (fib_memo fib_tail_recursive fib_tabled fib_naive)) True)

;
;                               Utility to run and time each Fibonacci implementation


  (= 
    (time-fibonaccis $N) 
    ( (remove-all-atoms  &self 
        (was_fib_memo  $_ $_)) 
      (fibonaccis $Fibonaccis) 
      (member $Fib $Fibonaccis) 
      (format '~N~n% =====================================================~n' Nil) 
      (=.. $Goal 
        (:: $Fib $N $_)) 
      (statistics walltime 
        (Cons  $Start $_)) 
      (catch 
        (call $Goal) $E 
        (format '~N~nError in Goal: ~q ~q ~n' 
          (:: $Goal $E))) 
      (statistics walltime 
        (Cons  $End $_)) 
      (is $Time 
        (- $End $Start)) 
      (format '~N~n~w(~w) took 	~w ms~n' 
        (:: $Fib $N $Time)) 
      (fail)))
;                                ; Clear any memoized results




  (= 
    (time-fibonaccis $_) 
    ( (format '~N~n% =====================================================~n' Nil) (set-det)))

;
;                                 Running the utility with an example, N=30.


;
;                                :- writeln(':- time_fibonaccis(30000)').




