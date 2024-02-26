;
;                   Standard Recursive Approach

;
;                   Pros: Simple and easy to understand.

;
;                    Cons: Not efficient for large N as it is not tail recursive.


  (= 
    (factorial_standard  0 1) True)
  (= 
    (factorial-standard $N $Result) 
    ( (> $N 0) 
      (is $M 
        (- $N 1)) 
      (factorial-standard $M $SubResult) 
      (is $Result 
        (* $N $SubResult))))

;
;                     Basic Tail Recursive Approach with Accumulator

;
;                     Pros: Tail recursive, efficient in stack frame usage for large N.

;
;                     Cons: Requires understanding of accumulators and tail recursion.


  (= 
    (factorial-tail-basic2 $N $Result) 
    (factorial-tail-basic3 $N 1 $Result))

  (= 
    (factorial_tail_basic3  0 $Accumulator $Accumulator) True)
  (= 
    (factorial-tail-basic3 $N $Accumulator $Result) 
    ( (> $N 0) 
      (is $NewAccumulator 
        (* $N $Accumulator)) 
      (is $M 
        (- $N 1)) 
      (factorial-tail-basic $M $NewAccumulator $Result)))

;
;                       Factorial with between/3 Predicate

;
;                       Pros: Utilizes between/3 predicate, providing a clear range.

;
;                       Cons: Not tail recursive, can be inefficient for large N.


  (= 
    (factorial-between $N $Result) 
    (factorial-between $N 1 $Result))
  (= 
    (factorial-between 0 $Product $Product) 
    (set-det))
  (= 
    (factorial-between $N $Product $Result) 
    ( (> $N 0) 
      (is $NewProduct 
        (* $N $Product)) 
      (is $Next 
        (- $N 1)) 
      (factorial-between $Next $NewProduct $Result)))

;
;                         Factorial using findall/3 and product/2

;
;                         Pros: Utilizes findall/3 to generate a list, making it more adaptable.

;
;                         Cons: Generates a list of all numbers from 1 to N, can be memory-intensive for large N.


  (= 
    (product $List $Product) 
    (foldl multiply $List 1 $Product))

  (= 
    (multiply $X $Y $Z) 
    (is $Z 
      (* $X $Y)))

  (= 
    (factorial-findall $N $Result) 
    ( (>= $N 0) 
      (findall $X 
        (between 1 $N $X) $List) 
      (product $List $Result)))

;
;                           Using between/3 Predicate in Tail Recursive Manner

;
;                           Pros: Combines tail recursion with between/3 for clear range definition.

;
;                           Cons: Slightly more complex due to the combination of concepts.


  (= 
    (factorial-tail-between $N $Result) 
    (factorial-tail-between $N 1 $Result))
  (= 
    (factorial-tail-between 0 $Product $Product) 
    (set-det))
  (= 
    (factorial-tail-between $N $Product $Result) 
    ( (> $N 0) 
      (is $NewProduct 
        (* $N $Product)) 
      (is $Next 
        (- $N 1)) 
      (factorial-tail-between $Next $NewProduct $Result)))

;
;                             Tail Recursion with Explicit Cut

;
;                             Pros: Uses explicit cut to avoid unnecessary backtracking, optimizing performance.

;
;                             Cons: The use of cut (!) requires caution as it can affect the logic if misplaced.


  (= 
    (factorial-tail-cut $N $Result) 
    (factorial-tail-cut $N 1 $Result))
  (= 
    (factorial-tail-cut 0 $Accumulator $Accumulator) 
    (set-det))
  (= 
    (factorial-tail-cut $N $Accumulator $Result) 
    ( (> $N 0) 
      (is $NewAccumulator 
        (* $N $Accumulator)) 
      (is $M 
        (- $N 1)) 
      (factorial-tail-cut $M $NewAccumulator $Result)))

;
;                               Accumulator Version with Explicit Cut

;
;                               Pros: Efficient for large N due to tail recursion and avoids unnecessary backtracking due to cut.

;
;                               Cons: Requires understanding of both accumulators and the effect of cut on logic.


  (= 
    (factorial-acc-cut $N $Result) 
    (factorial-acc-cut $N 1 $Result))
  (= 
    (factorial-acc-cut 0 $Accumulator $Accumulator) 
    (set-det))
  (= 
    (factorial-acc-cut $N $Accumulator $Result) 
    ( (> $N 0) 
      (is $NewAccumulator 
        (* $N $Accumulator)) 
      (is $M 
        (- $N 1)) 
      (factorial-acc-cut $M $NewAccumulator $Result)))

;
;                                 Accumulator Version with Guard Clauses

;
;                                 Pros: Uses guard clauses for clear distinction between base and recursive cases. Efficient for large N.

;
;                                 Cons: Slightly more verbose due to the explicit guard clauses.


  (= 
    (factorial-acc-guard $N $Result) 
    (factorial-acc-guard $N 1 $Result))
  (= 
    (factorial-acc-guard $N $Accumulator $Accumulator) 
    (=< $N 0))
  (= 
    (factorial-acc-guard $N $Accumulator $Result) 
    ( (> $N 0) 
      (is $NewAccumulator 
        (* $N $Accumulator)) 
      (is $M 
        (- $N 1)) 
      (factorial-acc-guard $M $NewAccumulator $Result)))

;
;                                   Tabling Method

;
;                                   Summary: Uses tabling to store intermediate results, avoiding redundant calculations and improving efficiency.

;
;                                   Pros: Efficient even for large N due to no recalculations; polynomial time complexity.

;
;                                   Cons: Uses additional memory to store the intermediate results; support for tabling is not available in all MeTTa implementations.


  !(table (/ factorial-tabled 2))

  (= 
    (factorial-tabled 0 1) 
    (set-det))
  (= 
    (factorial-tabled $N $F) 
    ( (> $N 0) 
      (is $N1 
        (- $N 1)) 
      (factorial-tabled $N1 $F1) 
      (is $F 
        (* $N $F1))))

;
;                                     Memoization (Dynamic Programming) Method

;
;                                     Summary: Uses memoization to store previously calculated factorials, improving efficiency.

;
;                                     Pros: Efficient even for large N due to no recalculations.

;
;                                      Cons: Uses additional memory to store the previously calculated factorials.


  !(dynamic (/ was-factorial-memo 2))

  (= 
    (factorial-memo 0 1) 
    (set-det))
  (= 
    (factorial-memo $N $F) 
    ( (> $N 0) (det-if-then-else (was-factorial-memo $N $F) True (, (is $N1 (- $N 1)) (factorial-memo $N1 $F1) (is $F (* $N $F1)) (add-atom  &self (was_factorial_memo  $N $F))))))



;
;                                        List of all the factorial implementations


  (= 
    (factorials  
      (factorial_standard factorial_between factorial_findall factorial_tail_basic2 factorial_tail_between factorial_tabled factorial_memo factorial_tail_cut factorial_acc_cut factorial_acc_guard)) True)

;
;                                        Utility to run and time each Factorial implementation


  (= 
    (time-factorials $N) 
    ( (remove-all-atoms  &self 
        (was_factorial_memo  $_ $_)) 
      (factorials $Factorials) 
      (member $Fib $Factorials) 
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
;                                         ; Clear any memoized results




  (= 
    (time-factorials $_) 
    ( (format '~N~n% =====================================================~n' Nil) (set-det)))

;
;                                          Running the utility with an example, N=30.

;
;                                         :- writeln(':- time_factorials(61111).').





