;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define List
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;; Define some-value returning various types
(= (some-value) 1)                     ; Number
(= (some-value) "a")                   ; String
(= (some-value) 'c')                   ; Char
(= (some-value) $V)                    ; Variable
(= (some-value) (Cons 1 (Cons 2 (Cons 3 Nil)))) ; Example List
(= (some-value) (+ 2 3))               ; Returns number  
(= (some-value) (println! ""))   ; Returns empty
(= (some-value) (empty))               ; Returns empty
(= (some-value) ())                    ; Empty Expression (not empty like teh tow above)
(= (some-value) (format-args "x: {} y: {}" (1 2))) ; Formatted string
(= (some-value) S)                     ; Symbol (not a Nat)
(= (some-value) Z)                     ; Nat
(= (some-value) (S Z))                 ; Nat
(= (some-value) String)                ; Type
(= (some-value) (S (S Z)))             ; Nat
; (= (some-value) &self)                 ; Grounded Space

;; Filter definitions
(: Filter-String (-> String $t))
(: Filter-Number (-> Number $t))
(: Filter-Nat (-> Nat $t))
(: Filter-Symbol (-> Symbol $t))
(: Filter-Variable (-> Variable $t))
(: Filter-Grounded (-> Grounded $t))
(: Filter-Type (-> Type $t))
(: Filter-%Undefined% (-> %Undefined% $t))
(: Filter-TVar (-> $t $t))
(: Filter-Undeclared (-> Undeclared $t)) ; undeclared type
(: Filter-Expression (-> Expression $t))
(: Filter-Atom (-> Atom $t))
(: Filter-ExpressionNumber (-> Expression Number))
(: Filter-ExpressionUnit (-> Expression (->)))
(: Filter-ExpressionAtom (-> Expression Atom))
(: Filter-AtomAtom (-> Atom Atom))
(: Filter-Char (-> Char $t))
(: Filter-Empty (-> Empty $t))
(: Filter-List (-> (List $a) $t))

(= (Filter-String $a) $a)
(= (Filter-Number $a) $a)
(= (Filter-Nat $a) $a)
(= (Filter-Symbol $a) $a)
(= (Filter-Variable $a) $a)
(= (Filter-Grounded $a) $a)
(= (Filter-Type $a) $a)
(= (Filter-%Undefined% $a) $a)
(= (Filter-TVar $a) $a)
(= (Filter-Undeclared $a) $a)
(= (Filter-Expression $a) $a)
(= (Filter-Atom $a) $a)
(= (Filter-ExpressionNumber $a) $a)
(= (Filter-ExpressionUnit $a) $a)
(= (Filter-ExpressionAtom $a) $a)
(= (Filter-AtomAtom $a) $a)
(= (Filter-Char $a) $a)
(= (Filter-Empty $a) $a)
(= (Filter-List $a) $a)

(: MeTTa-Test (-> String Atom Atom Atom Atom))

(= (MeTTa-Test $Name $Form $List $passes)
  (let ($n $res $qform) ($Name (collapse $Form) (quote $Form))
   (case $res 
    ( 
     ($List (println! (format-args "
     (MeTTa-Test \"{}\" {} 
        {} 
        {})" ($Name $Form $List $passes))))
     ($_    (println! (format-args "
     (MeTTa-Test \"{}\" {} 
       {} 
       {})" ($Name $Form $List $res))))
     ))))

!(MeTTa-Test "Should be Passing"  (+ 1 1) (2) $passes)

;; returns of appropriate type
!(MeTTa-Test "Filter-Number" (Filter-Number (some-value))  (1 5) ($X 1 String () 5))  ;; hyperon returns [1, String, (), 5]

!(MeTTa-Test "passing"  (+ 1 1) (2) $passes)


!(MeTTa-Test "Filter-String" (Filter-String (some-value)) ("a" "x: 1 y: 2") $passes )  ;; "a", String, "x: 1 y: 2", ()

;; Tests for Filter-Nat
!(MeTTa-Test "Filter-Nat"  (Filter-Nat (some-value)) (Z (S Z) (S (S Z))) $passes)

;; Tests for Filter-Symbol
!(MeTTa-Test "Filter-Symbol"  (Filter-Symbol (some-value)) (S &self) $passes) 

;; Tests for Filter-Variable
!(MeTTa-Test "Filter-Variable"  (Filter-Variable (some-value)) (&self) $passes)  ;; Example case

;; Tests for Filter-Grounded
!(MeTTa-Test "Filter-Grounded"  (Filter-Grounded (some-value)) (1 "a" 'c' Z (S Z)) $passes)

;; Tests for Filter-Type
!(MeTTa-Test "Filter-Type"  (Filter-Type (some-value)) (String) $passes)

;; Tests for Filter-%Undefined%
!(MeTTa-Test "Filter-%Undefined%"  (Filter-%Undefined% (some-value)) (all the values) $passes)

;; Tests for Filter-TVar
!(MeTTa-Test "Filter-TVar"  (Filter-TVar (some-value)) (1 "a" Z (S Z) (S (S Z))) $passes)

;; Tests for Filter-Undeclared
!(MeTTa-Test "Filter-Undeclared"  (Filter-Undeclared (some-value)) () $passes)

;; Tests for Filter-Expression
!(MeTTa-Test "Filter-Expression"  (Filter-Expression (some-value)) ((+ 2 3) (format-args "x: {} y: {}" (1 2))) $passes)

;; Tests for Filter-Atom
!(MeTTa-Test "Filter-Atom"  (Filter-Atom (some-value)) (1 "a" 'c' S Z (S Z)) $passes)

;; Tests for Filter-ExpressionNumber
!(MeTTa-Test "Filter-ExpressionNumber"  (Filter-ExpressionNumber (some-value)) ((+ 2 3)) $passes)

;; Tests for Filter-ExpressionUnit
!(MeTTa-Test "Filter-ExpressionUnit"  (Filter-ExpressionUnit (some-value)) (empty (println! "did it")) $passes)

;; Tests for Filter-ExpressionAtom
!(MeTTa-Test "Filter-ExpressionAtom"  (Filter-ExpressionAtom (some-value)) () $passes)

;; Tests for Filter-AtomAtom
!(MeTTa-Test "Filter-AtomAtom"  (Filter-AtomAtom (some-value))  
  (Z String "a" 1 'c' $X S (Cons 1 (Cons 2 (Cons 3 Nil))) () 5 (S Z) "x: 1 y: 2" (S (S Z)) ()) $passes)

;; Tests for Filter-Char
!(MeTTa-Test "Filter-Char"  (Filter-Char (some-value)) ('c') $passes)

;; Tests for Filter-Empty
!(MeTTa-Test "Filter-Empty"  (Filter-Empty (some-value)) (empty (println! "did it") ()) $passes)

;; Tests for Filter-List
!(MeTTa-Test "Filter-List"  (Filter-List (some-value)) ((Cons 1 (Cons 2 (Cons 3 Nil)))) $passes)
