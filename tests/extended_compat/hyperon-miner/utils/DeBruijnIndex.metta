
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

; For conjunctions
(= (Deb2var $ptrn $varlist)
    (case $ptrn (
      ((, $p1 $p2) (, (Debruijn2var $p1 $varlist) (Debruijn2var $p2 $varlist)))
      ((, $p1 $p2 $p3) (, (Debruijn2var $p1 $varlist) (Debruijn2var $p2 $varlist) (Debruijn2var $p3 $varlist)))
      ($_ (Debruijn2var $ptrn $varlist)))))
