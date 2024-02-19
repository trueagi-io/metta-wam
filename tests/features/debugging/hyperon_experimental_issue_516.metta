; https://github.com/trueagi-io/hyperon-experimental/issues/516



;; Example usage in the Pattern Miner work,

;; Define List
(: List (-> $a Type))
(: Cons (-> $a (List $a) (List $a)))
(: Nil (List $a))

;; Define DeBruijn Index
(: DeBruijn Type)
(: VarIdx (-> Nat DeBruijn))

;; Map a DeBruijn Index to an given variable
(: idx2var (-> DeBruijn (List Variable) Atom))
(= (idx2var (VarIdx Z) (Cons $head $tail)) $head)
(= (idx2var (VarIdx (S $k)) (Cons $head $tail)) (idx2var (VarIdx $k) $tail))

;; Map a DeBruijn Index in a given pattern to a variable
(: Debruijn2var (-> Atom (List Variable) Atom))
(= (Debruijn2var (VarIdx $k) $varlist)
    (idx2var (VarIdx $k) $varlist))

(= (Debruijn2var $symbol $varlist)
    (if (== (get-type $symbol) %Undefined%) $symbol (empty)))

(= (Debruijn2var ($link $first $second) $varlist)
    ($link (Debruijn2var $first $varlist) (Debruijn2var $second $varlist)))


;; The Debruijn index here is used to wrap the MeTTa Variables to make it easy for the synthesizer to do the unification.
;; And during the match query, we unwrap them to variables and Debruijn2var function above is doing that.

!(,  (Debruijn2var (Inheritance (VarIdx Z) human) (Cons $Xvar (Cons $Yvar Nil)))
    (Debruijn2var (Inheritance (VarIdx Z) man) (Cons $Xvar (Cons $Yvar Nil))))

;returns
;[(, (Inheritance $Xvar human) (Inheritance $X#311 man))]
; Expected result, to be able to do pattern matching is,
;[(, (Inheritance $Xvar human) (Inheritance $Xvar man))]


