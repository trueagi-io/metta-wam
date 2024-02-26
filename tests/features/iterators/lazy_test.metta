;
;                  !  lazy_list(:Next, +State0, -List)

;
; 

;
;                     Create a lazy list where the next element is defined by

;
; 

;
;                          call(Next, State0, State1, Head)

;
; 

;
;                      The example below uses this  predicate   to  define  a lazy list

;
;                      holding the Fibonacci numbers. Our state  keeps the two previous

;
;                       Fibonacci numbers.

;
; 

;
;                         ```


  (= 
    (fibonacci-numbers $L) 
    (lazy-list fib 
      (state - -) $L))


  (= 
    (fib 
      (state - -) 
      (state 0 -) 0) 
    (set-det))
  (= 
    (fib 
      (state 0 -) 
      (state 1 0) 1) 
    (set-det))
  (= 
    (fib 
      (state $P $Q) 
      (state $F $P) $F) 
    (is $F 
      (+ $P $Q)))

;
;                           ```

;
; 

;
;                          The above can be used to retrieve   the Nth Fibonacci number. As

;
;                          fib/2 provides no access  to  the   complete  list  of Fibonacci

;
;                          numbers, this can be used to generate large Fibonacci numbers.


  (= 
    (fib $N $F) 
    ( (lazy-list fib 
        (state - -) $L) (nth1 $N $L $F)))
;                         ;fibonacci_numbers(L),





  (= 
    (list-iter $Iter $L) 
    (lazy-iter $Iter nth0 $L))
;                         /*
;                         numbers(N) :-
;                             lazy_list(next_number, L),
; 
;                             ;fibonacci_numbers(L),
;                            lazy_list(fib, state(-,-), L),
; 
; 
;                         call(Next, List, Tail)
;                         */




  (= 
    (space-iter $Iter $L) 
    (lazy-iter $Iter get-next-symbol $L))


  (= 
    (get-next-atom $_ $Space $Atom) 
    (metta-atom $Space $Atom))

    

  (= 
    (lazy-iter $Iter $Next $L) 
    (lazy-list 
      (lazy-iter $Iter $Next) 
      (state 0) $L))

;
;                            lazy_iter(Iter,state(0),state(1),V):- get_item(0,Iter,V).

  (= 
    (lazy-iter $Iter $Next 
      (state $N) 
      (state $N1) $V) 
    ( (is $N1 
        (+ $N 1)) (call $Next $N $Iter $V)))

