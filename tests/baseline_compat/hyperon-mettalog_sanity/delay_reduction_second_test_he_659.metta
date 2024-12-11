;; ===== Type Definitions =====
;; Natural Numbers
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Lists
(: List Type)
(: Nil List)
(: Cons (-> $a List List))

;; ===== Utility Functions =====
;; Greater than zero predicate
(: 0< (-> Number Bool))
(= (0< $x) (< 0 $x))


;; ===== Backward Chainer =====
(: bc (-> $a    ; Knowledge base space
          $b    ; Query
          Nat   ; Maximum depth
          $b))  ; Result

;; Base cases
(= (bc $kb (: $prf $ccln) $_)
   (match $kb (: $prf $ccln) (: $prf $ccln)))

(= (bc $kb (: CPU $check (0⍃ $x)) $_)
   (if (0< $x) (: CPU $check (0⍃ $x)) (empty)))

;; Recursive step
(= (bc $kb (: ($prfabs $prmlst) $ccln) (S $k))
   (let* (((: $prfabs (-> $prmlst $ccln))
           (bc $kb (: $prfabs (-> $prmlst $ccln)) $k))
          ($prmlst (handle-prmlst $kb $k $prmlst)))
     (: ($prfabs $prmlst) $ccln)))

;; ===== Parameter List Handler =====
(: handle-prmlst (-> $a $b List $d))
(= (handle-prmlst $kb $k Nil) Nil)
(= (handle-prmlst $kb $k (Cons (: $prfarg $prms) $xs))
   (Cons (bc $kb (: $prfarg $prms) $k) (handle-prmlst $kb $k $xs)))
(= (handle-prmlst $kb $k (Cons (: CPU $check $prms) $xs))
   (if $check
       ;; If check is true, evaluate CPU term first, then rest of list
       (Cons (bc $kb (: CPU $var $prms) $k) (handle-prmlst $kb $k $xs))
       ;; If check is false, evaluate rest of list first, then CPU term
       (let $xs (handle-prmlst $kb $k $xs)
            (Cons (bc $kb (: CPU $var $prms) $k) $xs))))

;; ===== Closure Check =====
(: is-closed (-> Atom Bool))
(= (is-closed $x)
   (case (get-metatype $x)
         ((Symbol True)
          (Grounded True)
          (Variable False)
          (Expression (if (== $x ())
                         True
                         (and (let $head (car-atom $x) (is-closed $head))
                              (let $tail (cdr-atom $x) (is-closed $tail))))))))

;; ===== Knowledge Base Setup =====
!(bind! &kb (new-space))
!(add-atom &kb (: 2 Prime))
!(add-atom &kb (: Rule (-> (Cons (: CPU (is-closed $x) (0⍃ $x)) (Cons (: $x Prime) Nil))
                          (0⍃' $x))))

;; ===== Test =====
!(bc &kb (: $prf (0⍃' $x)) (S (S Z)))

