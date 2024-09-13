

!(pragma! eval True)
!(pragma! trace-on-eval False)

; =============================================================
; Purpose: Test parallel execution with 'concurrent-forall!'
; Operation: Launches a sleep task concurrently across 4 threads.
; Expectation: Total execution time should be significantly reduced, demonstrating parallel processing.
; Note: `get-time!` is the host system function `get_time(X)`
;       If a function is not found that contains ! it searches the host system for the function definition.
; =============================================================
(: concurrent-forall! (-> Atom Atom $t))
(: concurrent-forall! (-> Atom Atom Atom $t))
!(let $start (get-time!)  ; Store the start CPU time in $start
    (sequential  ; Begin a sequential block
         (concurrent-forall! (between! 1 4)  ; For each number between 1 and 4
                (sleep! 5)  ; Sleep for 5 seconds
               (threads 4))  ; Use 4 separate threads for the operation
     (< (- (get-time!) $start) 12))))  ; Check if the total time taken is less than 12 seconds


!("cpu_count=" (current-prolog-flag! cpu_count))

; =============================================================
; Purpose: Define predicate 'is-even' to check if a number is even.
; Operation: Uses modulo operator to determine if a number is divisible by 2 without remainder.
; Usage: General utility function for checking evenness of numbers.
; =============================================================
; Define 'is-even' predicate
(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))

; =============================================================
; Purpose: Define predicate 'incr' to increment a monadic state variable.
; Operation: Increases state by 1 and retrieves the updated state, ensuring sequential processing.
; Usage: Ideal for managing sequential state changes in concurrent environments.
; =============================================================
; Define 'incr' predicate
(: incr (-> (MonadicState Number) Number))
(= (incr $var)
     (sequential
           ((do (change-state! $var (+ 1 (get-state $var))))
               (get-state $var))))

; =============================================================
; Purpose: Showcase how concurrency handles shared state references
; Operation: Concurrently processes numbers, incrementing '$odds' or '$evens' state based on numbers evenness.
; Expectation: Parallel processing of a range of numbers, updating shared states.
; =============================================================
!(let*
    (($odds (new-state 0))
     ($evens (new-state 0)))
     (sequential
      (concurrent-forall! (== (between! 1 100) $X) (if (is-even $X) (incr $evens) (incr $odds)))
       (print! ("odds=" (get-state $odds) " evens="  (get-state $evens)))))

; =============================================================
; Purpose: Illustrate handling of global bind states in a concurrent setting
; Operation: Uses 'concurrent-forall!' to process numbers, updating global bind states based on numbers evenness.
; Expectation: Parallel processing of a number range, efficiently updating global states.
; =============================================================
!(sequential
  ((bind! &odds (new-state 0))
   (bind! &evens (new-state 0))
    (concurrent-forall! (== (between! 1 100) $X) (if (is-even $X) (incr &evens) (incr &odds)))
    (print! ( "odds=" (get-state &odds) " evens="  (get-state &evens)))))


; =============================================================
; Purpose: Define `factorial` function for arithmetic operation.
; Operation: Calculates the factorial of a number using a recursive approach.
; Usage: Useful in scenarios requiring factorial computations.
; =============================================================
(: factorial (-> Number Number))
(= (factorial $n) (if (== $n 0) 1 (* $n (factorial (- $n 1)))))

; =============================================================
; Purpose: Demonstrate parallel processing of a list using `concurrent-maplist`.
; Operation: Applies `factorial` to each number in a list concurrently.
; Expectation: Efficient computation of factorial for each list element in parallel.
; =============================================================
(: concurrent-maplist! (-> Atom (List Atom) (List $t)))
(: concurrent-maplist! (-> Atom (List Atom) (List Atom) (List $t)))
(: concurrent-maplist! (-> Atom (List Atom) (List Atom) (List Atom) (List $t)))
!(concurrent-maplist! factorial (1 2 3 4 5 6 7 8 9))

; =============================================================
; Purpose: Define 'hyperpose' for concurrent evaluation of expressions in a list.
; Operation: Applies `concurrent-maplist!` to a list for parallel evaluation of each element.
; Usage: Can be used to speed up evaluation of independent operations.
; =============================================================
; Define higher-order function `hyperpose`
(:  hyperpose (-> Atom $t))
(= (hyperpose $lst) (concurrent-maplist! eval $lst))

;!(let $body
;    (collapse (factorial (superpose (range 1 10))))
;    (hyperpose $body))

!(hyperpose ((factorial 1) (factorial 2) (factorial 3) (factorial 4)
                       (factorial 5) (factorial 6) (factorial 7) (factorial 8)
		       (factorial 9)))



!(==
    (concurrent-maplist! factorial (1 2 3 4 5 6 7 8 9))

   (hyperpose ((factorial 1) (factorial 2) (factorial 3) (factorial 4)
                       (factorial 5) (factorial 6) (factorial 7) (factorial 8)
		       (factorial 9))))
