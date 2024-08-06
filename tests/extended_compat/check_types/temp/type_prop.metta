;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import! &corelib corlib)

(: = (-> Atom Atom Atom)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Types as propositions can be used in MeTTa but with nuances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entities
(: Entity Type)
(: Socrates Entity)
(: Plato Entity)

; Predicates (functions) from `Entity` to `Type`
(: Human (-> Entity Type))
(: Mortal (-> Entity Type))

; `(Mortal Plato)` is an expression of `Type` type
; It represents the proposition "Plato is mortal"
; but doesn't mean this proposition is inhabited/proven
!(assertEqual
  (get-type (Mortal Plato))
  Type)

; A function type as an implication
; `HumansAreMortal` represents the implication:
; "For all x, if Human(x) then Mortal(x)"
(: HumansAreMortal (-> (Human $t) (Mortal $t)))
; NOTE: MeTTa doesn't differentiate between type constructors and
;       other symbols (including functional) of the same type.
;       Thus, ATM, we cannot prove any proposition with universal
;       quantification (e.g., that all humans are mortal) by
;       providing a total function as an instance of a
;       corresponding type.

; These might be considered as assertions that Plato and
; Socrates are Humans:
(: SocratesIsHuman (Human Socrates))
(: PlatoIsHuman (Human Plato))

; `(HumansAreMortal SocratesIsHuman)` might be considered
; as a proof that that `(Mortal Socrates)`.
!(assertEqual
  (get-type (HumansAreMortal SocratesIsHuman))
  (Mortal Socrates))
; Bad typing
!(assertEqualToResult
  (get-type (HumansAreMortal (Human Socrates)))
  ())

; We can write this code, but it will make not too much
; difference for the assertion that "all humans are mortal"
(: SocratesIsMortal (Mortal Socrates))
(= (HumansAreMortal SocratesIsHuman) SocratesIsMortal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Equality can be typed. If some equality is well-typed,
; it means that it can be true.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(nop (remove-atom &self (: = (-> Atom Atom Atom))))

(: = (-> $t $t Type))
;!(assertEqual
;  (get-type (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
;  Type)
; This equality is well-typed, because both its arguments are of `Type`.
; It doesn't mean they are both inhabited (true propositions), and
; it doesn't mean that the equality itself is true (an inhabited type).
;!(assertEqual
;  (get-type (= (Mortal Socrates) (Mortal Plato)))
;  Type)

;; mettalog fail
; Wrong proof
!(assertEqualToResult
  (get-type (= SocratesIsMortal (HumansAreMortal PlatoIsHuman)))
  ())

;; mettalog fail
; Wrong equalities
!(assertEqualToResult
  (get-type (= SocratesIsHuman PlatoIsHuman))
  ())

;; mettalog fail
!(assertEqualToResult
  (get-type (= SocratesIsHuman SocratesIsMortal))
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A standard type-theoretic way of saying that some equality is true
; is to present an instance of the corresponding equality type.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (: Refl (-> (: $x $T) (= $x $x))).
; As per https://github.com/trueagi-io/hyperon-experimental/issues/177 ,
; there is no way to match both argument and its type in type definition.
; Such syntax `(: (Refl $x) (-> Type (= $x $x)))` also doesn't work now.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Another (MeTTa) way is to have explicit reduction of types to True
; (equalities over equalities), although a more practical way would
; be just to use a dedicated comparison operator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: T Type)
(= (= $x $x) T)

; These equalities will be true
!(assertEqual
  (= SocratesIsMortal (HumansAreMortal SocratesIsHuman))
  T)
!(assertEqual
  (= (+ 1 1) (- 3 1))
  T)

; One can also define an equality for directly inhabited types
(= (= $type T)
   (match &self (: $x $type) T))
!(assertEqual
  (= (Mortal Socrates) T)
  T)

; It will not immediately work for implications
!(assertEqualToResult
  (= (Mortal Plato) T)
  ())

; But we can add 'reasoning'
(= (= $type T)
   (match &self (: $impl (-> $cause $type))
      (= $cause T)))
!(assertEqual
  (= (Mortal Plato) T)
  T)
(: Sam Entity)
!(assertEqualToResult
  (= (Human Sam) T)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Types by themselves will not be reduced
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqualToResult
   (Mortal Plato)
  ((Mortal Plato)))
; Refl rule itself equals to True
!(assertEqual
  (= (= $x $x) T)
  T)

; We can also add direct equalities
; (= (Human Socrates) T)
(= (Human Plato) T)
(= (Mortal Socrates) T)
(= (Mortal $x) (Human $x))
; Then, the type of type itself will be reduced
!(assertEqual
  (Mortal Plato)
  T)

; We can even try backward chaining
(: ift (-> Type $t $t))
(= (ift T $then) $then)
!(assertEqualToResult
  (ift (Mortal $x) $x)
  (Socrates Plato))





!(let $o (new-state True)  (get-type $o))
;[Expression]
!(let $o (new-state (new-state 1))  (get-type $o))
;[Expression]

;;[]
!(let $o (new-state (quote 1)) (get-metatype $o))
;[Grounded]
!(let $o (new-state $var) (get-metatype $o))
;[Grounded]
!(let $o (new-state (->)) (get-metatype $o))
;[Grounded]
!(let $o (new-state "string") (get-metatype $o))
;[Grounded]
!(let $o (new-state &self)  (get-metatype $o))
;[Grounded]
!(let $o (new-state True)  (get-metatype $o))
;[Grounded]
!(let $o (new-state (new-state 1))  (get-metatype $o))
;[Grounded]
!(get-metatype (get-state (let $x (new-state 1) $x)))
;[Expression]
!(get-type $var)
;[%Undefined%]
!(get-type "string")
;[String]
!(get-type True)
;[Bool]
!(get-type  1)
;[Number]
!(get-metatype $var)
;[Expression]
!(get-metatype "string")
;[Grounded]
!(get-metatype  1)
;[Grounded]

!(get-type get-state)
;[(-> (StateMonad $tgso) $tgso)]
!(get-type echo)
;[%Undefined%]
!(get-type =)
;[(-> $t $t Type)]
!(get-type ==)
;[(-> $t $t Bool)]
!(get-type :)
;[%Undefined%]
!(get-type ->)
;[%Undefined%]
!(get-type new-state)
;[(-> $tnso (StateMonad $tnso))]

!(let $o (new-state &self)  (get-type $o))
;[Expression]

!(get-metatype True)
;[Grounded]

!(get-metatype (->))
;[Expression]

!(get-metatype (quote 1))
;[Expression]

!(get-metatype (superpose (1 "s" 'c')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Equality can be typed. If some equality is well-typed,
; it means that it can be true.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(get-type (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
; This equality is well-typed, because both its arguments are of `Type`.
; It doesn't mean they are both inhabited (true propositions), and
; it doesn't mean that the equality itself is true (an inhabited type).
!(get-type (= (Mortal Socrates) (Mortal Plato)))

!(let $o (new-state (->)) (get-type $o))
;[Expression]
!(let $o (new-state "string") (get-type $o))
;[Expression]
!(let $o (new-state 'x') (get-type $o))

!(let $o (new-state (quote 1)) (get-type $o))
;[Expression]
!(let $o (new-state $var) (get-type $o))
;[Expression]
!(let $o (new-state (superpose (1 "s" 'c')))  (get-type $o))
;[Expression]
!(get-type (get-state (let $x (new-state 1) $x)))
;[Expression]
!(get-type (new-state (quote 1)))
;;[(StateMonad Atom)]
!(get-type (new-state $var))
;;[(StateMonad %Undefined%)]


!(get-metatype &corelib)
;[Symbol]
!(get-type (superpose (1 "s" 'c')))
;[%Undefined%]
!(get-type (new-state (->)))
;;[(StateMonad %Undefined%)]
!(get-type (new-state "string"))
;;[(StateMonad String)]
!(get-type (new-state 'x'))
;;[(StateMonad Char)]
!(get-type (new-state &self))
;;[(StateMonad hyperon::space::DynSpace)]
!(get-type (new-state True))
;;[(StateMonad Bool)]
!(get-type (new-state (new-state 1)))
;;[(StateMonad (StateMonad Number))]
!(get-type (new-state (superpose (1 "s" 'c'))))
;;[(StateMonad %Undefined%)]
!(get-type (get-state (let $x (new-state 1) $x)))
!(get-type (quote 1))
;[Atom]
!(get-type (->))
;[%Undefined%]
!(get-type 'x')
;[Char]
!(let $o (new-state 'x') (get-type $o))
;[(StateMonad Char)]
!(let $o (new-state (superpose (1 "s" 'c')))  (get-metatype $o))
;[Grounded,Grounded,Grounded]
!(new-state (superpose (1 "s" 'c')))
;[(State 'c'), (State "s"), (State 1)]
!(let $o (new-state (superpose (1 "s" 'c'))) (get-type $o))
;[(StateMonad Char), (StateMonad String), (StateMonad Number)]
!(new-state (superpose (1 "s" (quote 'c' ))))
;[(State (quote 'c')), (State "s"), (State 1)]
!(let $o (new-state (superpose (1 "s" (quote 'c')))) (get-type $o))
;[(StateMonad Expression), (StateMonad String), (StateMonad Number)]


!(let $o (superpose (1 "s" 'c')) (get-metatype $o))
;[Expression]
!(let $o (get-state (let $x (new-state 1) $x)) (get-metatype $o))
;[Expression]

!(get-type &corelib)
;[hyperon::space::DynSpace]
!(get-type (get-state (let $x (new-state  1) $x)))
;[]
!(get-metatype 'x')
;[Variable]
!(get-type get-type)
;[(-> Atom Atom Atom)]


