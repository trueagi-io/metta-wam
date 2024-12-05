; Additional Tests for Delaying Reduction Until Bindings Are Ready
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/659

; Test Case 1: Reordering Premises in Knowledge Base
!(add-atom &kb (: Rule (-> (: $x Prime)
                           (-> (: $_ (0⍃ $x))
                               (0⍃' $x)))))
!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Expected proof with reordered premises

; Test Case 2: Using is-closed to Delay Reduction
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

(: 0< (-> Number Bool))
(= (0< $x) (if (is-closed $x) (< 0 $x) (empty)))

!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Valid proof with is-closed applied

; Test Case 3: Recursive Parameter List Handling
(: handle-prmlst (-> $a $b List $d))
(= (handle-prmlst $kb $k Nil) Nil)
(= (handle-prmlst $kb $k (Cons (: $prfarg $prms) $xs))
   (Cons (bc $kb (: $prfarg $prms) $k) (handle-prmlst $kb $k $xs)))
(= (handle-prmlst $kb $k (Cons (: CPU $check $prms) $xs))
   (if $check
       (Cons (bc $kb (: CPU $var $prms) $k) (handle-prmlst $kb $k $xs))
       (let $xs (handle-prmlst $kb $k $xs)
            (Cons (bc $kb (: CPU $var $prms) $k) $xs))))

!(add-atom &kb (: Rule (-> (Cons (: CPU (is-closed $x) (0⍃ $x)) 
                              (Cons (: $x Prime) Nil))
                          (0⍃' $x))))

!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Valid proof with parameter list handling
