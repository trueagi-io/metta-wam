(: Any Type)
(: Atom Type)
(: Bool Type)
(@doc Bool (@desc "Boolean type of True or False."))
(: Expression Type)
(: Number Type)
(: hyperon::space::DynSpace Type)
(: ReturnType Type)
(: Symbol Type)
(: StateMonad Type)
(: Type Type)
(: True Bool)
(: False Bool)
(: %Undefined% Type)
(: Variable Type)
; (: if-decons (-> Atom Variable Variable Atom Atom Atom))  was a misnamed if-decons-expr 
(: if-empty (-> Atom Atom Atom Atom))
(: if-non-empty-expression (-> Atom Atom Atom Atom))
(: if-not-reducible (-> Atom Atom Atom Atom))
;(: apply (-> Atom Variable Atom Atom))
;(: cons (-> Atom Atom Atom))
;(: decons (-> Atom Atom))

(: return (-> Atom ReturnType))
(: switch (-> %Undefined% Expression Atom))
(: unify (-> Atom Atom Atom Atom %Undefined%))
(: get-type0 (-> Atom Atom))
(: get-ftype (-> Atom Atom))
(: : %Undefined%)
(: function-arity (-> Symbol Number))
(: predicate-arity (-> Symbol Number))
(: pragma! (-> Atom Atom (->)))
(: = (-> Atom Atom %Undefined%))
(: match (-> hyperon::space::DynSpace Atom Atom %Undefined%))
(: case (-> Expression Atom Atom))
(: combine (-> $t $t $t))
(: import! (-> hyperon::space::DynSpace Atom (->)))
(: get-type (-> Atom Type))
(: PredicateArity (-> Symbol Number))
(: If (-> Bool Atom Atom Atom))
(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (let $n 0 (let $n 1 $n)))
(= (If $cond $then $else) (if $cond $then $else))
(PredicateArity PredicateArity 2)
(PredicateArity : 2)
(= (: $F P1) (PredicateArity $F 1))
(: : SrcPredicate)
(: If SrcFunction)

; Public MeTTa
(@doc = 
  (@desc "A symbol used to define reduction rules for expressions.")
  (@params (
    (@param "Pattern to be matched against expression to be reduced")
    (@param "Result of reduction or transformation of the first pattern")))
  (@return "Not reduced itself unless custom equalities over equalities are added") )
(: = (-> $t $t Atom))
;; Implemented from Interpreters

(: = (-> Atom Atom Atom))

; Public MeTTa
(@doc ErrorType (@desc "Type of the atom which contains error"))
(: ErrorType Type)
;; Implemented from Interpreters

; Public MeTTa
(@doc Error
  (@desc "Error constructor")
  (@params (
    (@param "Atom which contains error")
    (@param "Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments")))
  (@return "Instance of the error atom"))
(: Error (-> Atom Atom ErrorType))
;; Implemented from Interpreters

; Public MinimalMeTTa
(@doc return
  (@desc "Returns value from the (function ...) expression")
  (@params (
    (@param "Value to be returned")))
  (@return "Passed argument"))
(: return (-> $t $t))
;; Implemented from Interpreters

; Public MinimalMeTTa
(@doc function
  (@desc "Evaluates the argument until it becomes (return <result>). Then (function (return <result>)) is reduced to the <result>.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of atom's evaluation"))
(: function (-> Atom Atom))
;; Implemented from Interpreters

; Public MinimalMeTTa
(@doc eval
  (@desc "Evaluates input Atom, performs one step of the evaluation. Empty results (Empty, ()) are removed from the result set. If no results are produced for a non-grounded function, eval returns NotReducible.")
  (@params (
    (@param "Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function")))
  (@return "Result of evaluation"))
(: eval (-> Atom Atom))
;; Implemented from Interpreters

; Public MinimalMeTTa
(@doc chain
  (@desc "Evaluates first argument Atom, binds it to the Variable (second argument) and then evaluates third argument Template with Variable substituted in. When evaluation of the first Atom brings more than a single result, chain returns one instance of the Template expression for each result. The first argument Atom is only evaluated if it is part of the Minimal MeTTa specification; evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation).")
  (@params (
    (@param "Atom to be evaluated")
    (@param "Variable")
    (@param "Template which will be evaluated at the end with Variable substituted")))
  (@return "Result of evaluating third input argument"))
(: chain (-> Atom Variable Atom Atom))
;; Implemented from Interpreters

;; Helper MinimalMeTTa
(@doc if-unify
  (@desc "Matches two first arguments and returns third argument if they are matched and forth argument otherwise")
  (@params (
	(@param "First atom to unify with")
	(@param "Second atom to unify with")
	(@param "Result if two atoms unified successfully")
	(@param "Result otherwise")))
  (@return "Third argument when first two atoms are unifiable or forth one otherwise"))
(: if-unify (-> Atom Atom Atom Atom %Undefined%))
;; Implemented from Interpreters

(ALT= (if-unify $a $a $then $else) $then)
(ALT= (if-unify $a $b $then $else)
  (case (if-unify-or-empty $a $b) ((Empty $else))) )

;; Helper MinimalMeTTa
(: if-unify-or-empty (-> Atom Atom Atom))
(= (if-unify-or-empty $a $a) unified)
(= (if-unify-or-empty $a $b) (empty))


;; Public MinimalMeTTa
(@doc cons-atom
  (@desc "Constructs an expression using two arguments")
  (@params (
    (@param "Head of an expression")
    (@param "Tail of an expression")))
  (@return "New expression consists of two input arguments"))
(: cons-atom (-> Atom Expression Expression))
;; Implemented from Interpreters
; AKA? (: cons (-> Atom Atom Atom))

;; Public MinimalMeTTa
(@doc decons-atom
  (@desc "Works as a reverse to cons-atom function. It gets Expression as an input and returns it splitted to head and tail, e.g. (decons-atom (Cons X Nil)) -> (Cons (X Nil))")
  (@params (
    (@param "Expression")))
  (@return "Deconsed expression"))
(: decons-atom (-> Expression Expression))
;; Implemented from Interpreters
; AKA? (: decons (-> Atom Atom))

;; Public MinimalMeTTa
(@doc collapse-bind
  (@desc "Evaluates the Atom (first argument) and returns an expression which contains all alternative evaluations in a form (Atom Bindings). Bindings are represented in a form of a grounded atom { <var> <- <binding>, ... }. See also the complement superpose-bind. Note that, like chain, collapse-bind only evaluates Minimal Metta expressions. Evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation).")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "All alternative evaluations"))
;; collapse-bind because `collapse` doesnt guarantee shared bindings
(: collapse-bind (-> Atom Expression))
;; Implemented from Interpreters

;; Public MinimalMeTTa
(@doc superpose-bind
  (@desc "Complement to the collapse-bind. It takes result of collapse-bind (first argument) and returns only result atoms without bindings. Primarily used with some filtering step on the collapse-bind results, i.e. collapse-bind -> <filter> -> superpose-bind.")
  (@params (
    (@param "Expression in form (Atom Binding)")))
  (@return "Non-deterministic list of Atoms"))
;; superpose-bind because `superpose` doesnt guarentee shared bindings
(: superpose-bind (-> Expression Atom))
;; Implemented from Interpreters

; Helper Minimal Metta?
(@doc metta
  (@desc "Run MeTTa interpreter on atom.")
  (@params (
    (@param "Atom to be interpreted")
    (@param "Type of input atom")
    (@param "Atomspace where intepretation should take place")))
  (@return "Result of interpretation"))
(: metta (-> Atom Type Grounded Atom))
;; Implemented from Interpreters

;; Public MinimalMeTTa?
(@doc id
  (@desc "Returns its argument")
  (@params (
    (@param "Input argument")))
  (@return "Input argument"))
(: id (-> Atom Atom))
(= (id $x) $x)

;; Public MeTTa?
(@doc atom-subst
  (@desc "Substitutes variable passed as a second argument in the third argument by the first argument")
  (@params (
    (@param "Value to use for replacement")
    (@param "Variable to replace")
    (@param "Template to replace variable by the value")))
  (@return "Template with substituted variable"))
(: atom-subst (-> Atom Variable Atom Atom))
;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
(= (atom-subst $atom $var $templ)
  (function (chain (eval (id $atom)) $var (return $templ))) )

;; Helper MinimalMeTTa
(@doc if-decons-expr
  (@desc "Checks if first argument is non empty expression. If so gets tail and head from the first argument and returns forth argument using head and tail values. Returns fifth argument otherwise.")
  (@params (
    (@param "Expression to be deconstructed")
    (@param "Head variable")
    (@param "Tail variable")
    (@param "Template to return if first argument is a non-empty expression")
    (@param "Default value to return otherwise")))
  (@return "Either template with head and tail replaced by values or default value"))
(: if-decons-expr (-> Expression Variable Variable Atom Atom Atom))
;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
(ALT= (if-decons-expr $atom $head $tail $then $else)
  (function (eval (if-equal $atom ()
    (return $else)
    (chain (decons-atom $atom) $list
      (if-unify $list ($head $tail) (return $then) (return $else)) )))))

;; Helper MinimalMeTTa
(@doc if-error
  (@desc "Checks if first argument is an error atom. Returns second argument if so or third argument otherwise.")
  (@params (
    (@param "Atom to be checked for the error")
    (@param "Value to return if first argument is an error")
    (@param "Value to return otherwise")))
  (@return "Second or third argument"))
(: if-error (-> Atom Atom Atom Atom))
;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
(= (if-error $atom $then $else)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $meta Expression
      (eval (if-equal $atom ()
        (return $else)
        (chain (decons-atom $atom) $list
          (if-unify $list ($head $tail)
            (eval (if-equal $head Error (return $then) (return $else)))
            (return $else) ))))
      (return $else) )))))

;; Helper MinimalMeTTa
(@doc return-on-error
  (@desc "Returns first argument if it is Empty or an error. Returns second argument otherwise.")
  (@params (
    (@param "Previous evaluation result")
    (@param "Atom for further evaluation")))
  (@return "Return previous result if it is an error or Empty or continue evaluation"))
(: return-on-error (-> Atom Atom Atom))
(= (return-on-error $atom $then)
  (function (eval (if-equal $atom Empty (return (return Empty))
    (eval (if-error $atom (return (return $atom))
      (return $then) ))))))


; Difference between `switch` and `case` is a way how they interpret `Empty`
; result. `CaseOp` interprets first argument inside itself and then manually
; checks whether result is empty. `switch` is interpreted in a context of
; main interpreter. Minimal interpreter correctly passes `Empty` as an
; argument to the `switch` but when `switch` is called from MeTTa interpreter
; (for example user evaluates `!(switch (if-unify A B ok Empty) ...)` then
; emptiness of the first argument is checked by interpreter and it will
; break execution when `Empty` is returned.

;; Helper MinimalMeTTa
(@doc switch
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom to be matched with patterns")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result which corresponds to the pattern which is matched with the passed atom first"))

;; Dumb MeTTaLog? will we implement it?
(: switch (-> Atom Atom Atom))
(= (switch $atom $list) (case (eval $atom) $list))




; BEGIN - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
(: (
(: switch (-> %Undefined% Expression Atom))
(= (switch $atom $cases)
  (function (chain (decons-atom $cases) $list
    (chain (eval (switch-internal $atom $list)) $res
      (chain (eval (if-equal $res NotReducible Empty $res)) $x (return $x)) ))))

; Helper MinimalMeTTa
(@doc switch-internal
  (@desc "This function is being called inside switch function to test one of the cases and it calls switch once again if current condition is not met")
  (@params (
    (@param "Atom (it will be evaluated)")
    (@param "Deconsed tuple of pairs mapping condition patterns to results")))
  (@return "Result of evaluating of Atom bound to met condition"))
(= (switch-internal $atom (($pattern $template) $tail))
  (function (if-unify $atom $pattern
    (return $template)
    (chain (eval (switch $atom $tail)) $ret (return $ret)) )))

) Comment)  
; ENDOF - Yes, Douglas turned this sourcecode form into a a Value with the type Comment



; Helper MinimalMeTTa
; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
(@doc is-function
  (@desc "Function checks if input type is a function type")
  (@params (
    (@param "Type atom")))
  (@return "True if type is a function type, False - otherwise"))
(: is-function (-> Type Bool))
;; This impl is old and maybe not sufficiant?
(= (is-function $type)
  (function (chain (eval (get-metatype $type)) $meta
    (eval (switch ($type $meta) (
      (($type Expression)
        (eval (if-decons-expr $type $head $_tail
          (if-unify $head -> (return True) (return False))
          (return (Error (is-function $type) "is-function non-empty expression as an argument")) )))
      (($type $meta) (return False))
    ))))))

;; Public MeTTa
(@doc type-cast
  (@desc "Casts atom passed as a first argument to the type passed as a second argument using space as a context")
  (@params (
    (@param "Atom to be casted")
    (@param "Type to cast atom to")
    (@param "Context atomspace")))
  (@return "Atom if casting is successful, (Error ... BadType) otherwise"))
(: type-cast (-> Atom Atom Atom Atom))
;; This impl is old and maybe not sufficiant?
(ALT= (type-cast $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type $meta
      (return $atom)
      (chain (eval (collapse-bind (eval (get-type $atom $space)))) $collapsed
        (chain (eval (map-atom $collapsed $pair (eval (first-from-pair $pair)))) $actual-types
            (chain (eval (foldl-atom $actual-types False $a $b (eval (match-type-or $a $b $type)))) $is-some-comp
              (eval (if $is-some-comp
                (return $atom)
                (return (Error $atom BadType)) ))))))))))

;; Public MeTTa
(@doc match-types
  (@desc "Checks if two types can be unified and returns third argument if so, fourth - otherwise")
  (@params (
    (@param "First type")
    (@param "Second type")
    (@param "Atom to be returned if types can be unified")
    (@param "Atom to be returned if types cannot be unified")))
  (@return "Third or fourth argument"))
(: match-types (-> Atom Atom Atom Atom Atom))
(= (match-types $type1 $type2 $then $else)
  (function (eval (if-equal $type1 %Undefined%
    (return $then)
    (eval (if-equal $type2 %Undefined%
      (return $then)
      (eval (if-equal $type1 Atom
        (return $then)
        (eval (if-equal $type2 Atom
          (return $then)
          (if-unify $type1 $type2 (return $then) (return $else)) ))))))))))

; Helper MinimalMeTTa?
(@doc first-from-pair
  (@desc "Gets a pair as a first argument and returns first atom from pair")
  (@params (
    (@param "Pair")))
  (@return "First atom from a pair"))
(= (first-from-pair $pair)
  (function
    (if-unify $pair ($first $second)
      (return $first)
      (return (Error (first-from-pair $pair) "incorrect pair format")))))

; Helper MinimalMeTTa?
(@doc match-type-or
  (@desc "Checks if two types (second and third arguments) can be unified and returns result of OR operation between first argument and type checking result")
  (@params (
    (@param "Boolean value")
    (@param "First type")
    (@param "Second type")))
  (@return "True or False"))
(= (match-type-or $folded $next $type)
  (function
    (chain (eval (match-types $next $type True False)) $matched
      (chain (eval (or $folded $matched)) $or (return $or)) )))

;; Public MeTTa
(@doc filter-atom
  (@desc "Function takes list of atoms (first argument), variable (second argument) and filter predicate (third argument) and returns list with items which passed filter. E.g. (filter-atom (1 2 3 4) $v (eval (> $v 2))) will give (3 4)")
  (@params (
    (@param "List of atoms")
    (@param "Variable")
    (@param "Filter predicate")))
  (@return "Filtered list"))
(: filter-atom (-> Expression Variable Atom Expression))
(= (filter-atom $list $var $filter)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (filter-atom $tail $var $filter)) $tail-filtered
      (chain (eval (atom-subst $head $var $filter)) $filter-expr
        (chain $filter-expr $is-filtered
          (eval (if $is-filtered
            (chain (cons-atom $head $tail-filtered) $res (return $res))
            (return $tail-filtered) )))))
    (return ()) ))))

;; Public MeTTa
(@doc map-atom
  (@desc "Function takes list of atoms (first argument), variable to be used inside (second variable) and an expression which will be evaluated for each atom in list (third argument). Expression should contain variable. So e.g. (map-atom (1 2 3 4) $v (eval (+ $v 1))) will give (2 3 4 5)")
  (@params (
    (@param "List of atoms")
    (@param "Variable name")
    (@param "Template using variable")))
  (@return "Result of evaluating template for each atom in a list"))
(: map-atom (-> Expression Variable Atom Expression))
(= (map-atom $list $var $map)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (map-atom $tail $var $map)) $tail-mapped
      (chain (eval (atom-subst $head $var $map)) $map-expr
        (chain $map-expr $head-mapped
          (chain (cons-atom $head-mapped $tail-mapped) $res (return $res)) )))
    (return ()) ))))

;; Public MeTTa
(@doc foldl-atom
  (@desc "Function takes list of values (first argument), initial value (second argument) and operation (fifth argument) and applies it consequently to the list of values, using init value as a start. It also takes two variables (third and fourth argument) to use them inside")
  (@params (
    (@param "List of values")
    (@param "Init value")
    (@param "Variable")
    (@param "Variable")
    (@param "Operation")))
  (@return "Result of applying operation to the list of values"))
(: foldl-atom (-> Expression Atom Variable Variable Atom Atom))
(= (foldl-atom $list $init $a $b $op)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (atom-subst $init $a $op)) $op-init
      (chain (eval (atom-subst $head $b $op-init)) $op-head
        (chain $op-head $head-folded
          (chain (eval (foldl-atom $tail $head-folded $a $b $op)) $res (return $res)) )))
    (return $init) ))))

;; Helper MinimalMeTTa
(: separate-errors (-> Expression Expression Expression))
(= (separate-errors $succ-err $res)
  (function (if-unify $succ-err ($suc $err)
    (if-unify $res ($a $_b)
      (eval (if-error $a
        (chain (cons-atom $res $err) $err' (return ($suc $err')))
        (chain (cons-atom $res $suc) $suc' (return ($suc' $err))) ))
      (return $succ-err))
    (return $succ-err) )))

;; Helper MinimalMeTTa
(= (check-alternatives $atom)
  (function
    (chain (collapse-bind $atom) $collapsed
      ;(chain (eval (print-alternatives! $atom $collapsed)) $_
      (chain (eval (foldl-atom $collapsed (() ()) $succ-err $res
        (eval (separate-errors $succ-err $res)))) $separated
        (if-unify $separated ($success $error)
          (chain (eval (if-equal $success () $error $success)) $filtered
            (chain (superpose-bind $filtered) $ret (return $ret)) )
          (return (Error (check-alternatives $atom) "list of results was not filtered correctly")) )))))

;; Public MeTTa
(= (interpret $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type Atom
      (return $atom)
      (eval (if-equal $type $meta
        (return $atom)
        (eval (switch ($type $meta) (
          (($type Variable) (return $atom))
          (($type Symbol)
            (chain (eval (type-cast $atom $type $space)) $ret (return $ret)))
          (($type Grounded)
            (chain (eval (type-cast $atom $type $space)) $ret (return $ret)))
          (($type Expression)
            (chain (eval (check-alternatives (eval (interpret-expression $atom $type $space)))) $ret (return $ret)))
        ))))))))))

;; Helper MinimalMeTTa
(= (interpret-expression $atom $type $space)
  (function (eval (if-decons-expr $atom $op $args
    (chain (eval (get-type $op $space)) $op-type
      (chain (eval (is-function $op-type)) $is-func
        (if-unify $is-func True
          (chain (eval (interpret-func $atom $op-type $type $space)) $reduced-atom
            (chain (eval (metta-call $reduced-atom $type $space)) $ret (return $ret)) )
          (chain (eval (interpret-tuple $atom $space)) $reduced-atom
            (chain (eval (metta-call $reduced-atom $type $space)) $ret (return $ret)) ))))
    (chain (eval (type-cast $atom $type $space)) $ret (return $ret)) ))))

;; Helper MinimalMeTTa
(= (interpret-func $expr $type $ret-type $space)
  (function (eval (if-decons-expr $expr $op $args
    (chain (eval (interpret $op $type $space)) $reduced-op
      (eval (return-on-error $reduced-op
        (eval (if-decons-expr $type $arrow $arg-types
          (chain (eval (interpret-args $expr $args $arg-types $ret-type $space)) $reduced-args
            (eval (return-on-error $reduced-args
              (chain (cons-atom $reduced-op $reduced-args) $r (return $r)))))
          (return (Error $type "Function type expected")) )))))
    (return (Error $expr "Non-empty expression atom is expected")) ))))

;; Helper MinimalMeTTa
(= (interpret-args $atom $args $arg-types $ret-type $space)
  (function (if-unify $args ()
    (eval (if-decons-expr $arg-types $actual-ret-type $type-tail
      (chain (eval (== () $type-tail)) $correct-type-len
        (eval (if $correct-type-len
      (eval (match-types $actual-ret-type $ret-type
        (return ())
        (return (Error $atom BadType)) ))
          (return (Error $atom BadType)) )))
      (return (Error $atom "Too many arguments")) ))
    (eval (if-decons-expr $args $head $tail
      (eval (if-decons-expr $arg-types $head-type $tail-types
        (chain (eval (interpret $head $head-type $space)) $reduced-head
          ; check that head was changed otherwise Error or Empty in the head
          ; can be just an argument which is passed by intention
          (eval (if-equal $reduced-head $head
            (chain (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $ret-type $space)) $ret (return $ret))
            (eval (return-on-error $reduced-head
              (chain (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $ret-type $space)) $ret (return $ret)) )))))
        (return (Error $atom BadType)) ))
      (return (Error (interpret-atom $atom $args $arg-types $space) "Non-empty expression atom is expected")) )))))

;; Helper MinimalMeTTa
(= (interpret-args-tail $atom $head $args-tail $args-tail-types $ret-type $space)
  (function (chain (eval (interpret-args $atom $args-tail $args-tail-types $ret-type $space)) $reduced-tail
    (eval (return-on-error $reduced-tail
      (chain (cons-atom $head $reduced-tail) $ret (return $ret)) )))))

;; Helper MinimalMeTTa
(= (interpret-tuple $atom $space)
  (function (if-unify $atom ()
    (return $atom)
    (eval (if-decons-expr $atom $head $tail
      (chain (eval (interpret $head %Undefined% $space)) $rhead
        (eval (if-equal $rhead Empty (return Empty)
          (chain (eval (interpret-tuple $tail $space)) $rtail
            (eval (if-equal $rtail Empty (return Empty)
              (chain (cons-atom $rhead $rtail) $ret (return $ret)) ))))))
      (return (Error (interpret-tuple $atom $space) "Non-empty expression atom is expected as an argument")) )))))

;; Helper MinimalMeTTa?
(= (metta-call $atom $type $space)
  (function (eval (if-error $atom (return $atom)
    (chain (eval $atom) $result
      (eval (if-equal $result NotReducible (return $atom)
        (eval (if-equal $result Empty (return Empty)
          (eval (if-error $result (return $result)
            (chain (eval (interpret $result $type $space)) $ret (return $ret)) )))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Standard library written in MeTTa ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Public MinimalMeTTa?
(@doc if-equal
  (@desc "Checks if first two arguments are equal and evaluates third argument if equal, fourth argument - otherwise")
  (@params (
    (@param "First argument")
    (@param "Second argument")
    (@param "Atom to be evaluated if arguments are equal")
    (@param "Atom to be evaluated if arguments are not equal")))
  (@return "Evaluated third or fourth argument"))
;; Implemented from Interpreters
  
; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
;; Public MeTTa or Helper?
(@doc is-function-type
  (@desc "Function checks if input type is a function type")
  (@params (
    (@param "Type notation")))
  (@return "True if input type notation is a function type, False - otherwise"))
(: is-function-type (-> Type Bool))
(= (is-function-type $type)
  (let $type-meta (get-metatype $type)
    (case $type-meta (
      (Expression
        (let $first (car-atom $type)
          (if (== $first ->) True False) ))
      ($_ False) ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MeTTa interpreter implementation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Public MeTTa
(@doc if
  (@desc "Replace itself by one of the arguments depending on condition.")
  (@params (
    (@param "Boolean condition")
    (@param "Result when condition is True")
    (@param "Result when condition is False")))
  (@return "Second or third argument") )
(: if (-> Bool Atom Atom $t))
;; Implemented from Interpreters
(ALT= (if True $then $else) $then)
(ALT= (if False $then $else) $else)
;`$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference


;; Public MeTTa
(@doc xor
  (@desc "Exclusive disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Return values are the same as logical disjunction, but when both arguments are True xor will return False"))
;; Implemented from Interpreters
(: xor (-> Bool Bool Bool))

;; Public MeTTa
(@doc flip
  (@desc "Produces random boolean value")
  (@params ())
  (@return "Random boolean value"))
;; Implemented from Interpreters

;; Public MeTTa
(@doc or
  (@desc "Logical disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "True if any of input arguments is True, False - otherwise"))
(: or (-> Bool Bool Bool))
;; Implemented from Interpreters
(ALT= (or False False) False)
(ALT= (or False True) True)
(ALT= (or True False) True)
(ALT= (or True True) True)

;; Public MeTTa
(@doc and
  (@desc "Logical conjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if both arguments are True, False - otherwise"))
(: and (-> Bool Bool Bool))
;; Implemented from Interpreters
(ALT= (and False False) False)
(ALT= (and False True) False)
(ALT= (and True False) False)
(ALT= (and True True) True)

;; Public MeTTa
(@doc not
  (@desc "Logical negation")
  (@params (
    (@param "Argument")))
  (@return "Negates boolean input argument (False -> True, True -> False)"))
(: not (-> Bool Bool))
;; Implemented from Interpreters
(ALT= (not True) False)
(ALT= (not False) True)

;; Public MeTTa
(@doc let
  (@desc "Let function is utilized to establish temporary variable bindings within an expression. It allows introducing variables (first argument), assign values to them (second argument), and then use these values within the scope of the let block")
  (@params (
    (@param "Variable name (or several variables inside brackets ())")
    (@param "Expression to be bound to variable (it is being reduced before bind)")
    (@param "Expression which will be reduced and in which variable (first argument) could be used")))
  (@return "Result of third argument's evaluation"))

;; Public MeTTa
(: let (-> Atom %Undefined% Atom Atom))
;; Implemented from Interpreters
(ALT= (let $pattern $atom $template)
  (if-unify $atom $pattern $template Empty))

;; Public MeTTa
(@doc let*
  (@desc "Same as let, but first argument is a tuple containing tuples of variables and their bindings, e.g. (($v (+ 1 2)) ($v2 (* 5 6)))")
  (@params (
    (@param "Tuple of tuples with variables and their bindings")
    (@param "Expression which will be reduced and in which variable (first argument) could be used")))
  (@return "Result of second argument's evaluation"))
(: let* (-> Expression Atom Atom))
;; Implemented from Interpreters
(ALT= (let* $pairs $template)
  (eval (if-decons-expr $pairs ($pattern $atom) $tail
    (let $pattern $atom (let* $tail $template))
    $template )))


;; Public MeTTa?
(:> hyperon::space::DynSpace Grounded)

(: stringToChars (-> String Expression))
(: charsToString (-> Expression String))
(: parse (-> String Atom))
(: repr (-> Atom String))

;; Public MeTTa
(@doc add-reduct
  (@desc "Prevents atom from being reduced")
  (@params (
    (@param "Atom")))
  (@return "Quoted atom"))
;; Implemented from Interpreters
(@doc add-reduct-rust1
  (@desc "Adds atom into the atomspace reducing it first")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
  
(: add-reduct-rust1 (-> hyperon::space::DynSpace %Undefined% (->)))
(= (add-reduct-minimal $dst $atom) (add-atom $dst $atom))
;; Public MeTTa
(: add-reduct (-> Grounded %Undefined% (->)))
(= (add-reduct $dst $atom) (add-atom $dst $atom))

;; Public MeTTa
(@doc car-atom
  (@desc "Extracts the first atom of an expression as a tuple")
  (@params (
    (@param "Expression")))
  (@return "First atom of an expression"))
;; Implemented from Interpreters
(: car-atom (-> Expression Atom))
(= (car-atom $atom)
  (eval (if-decons-expr $atom $head $_
    $head
    (Error (car-atom $atom) "car-atom expects a non-empty expression as an argument") )))

;; Public MeTTa
(@doc cdr-atom
  (@desc "Extracts the tail of an expression (all except first atom)")
  (@params (
    (@param "Expression")))
  (@return "Tail of an expression"))
(: cdr-atom (-> Expression Expression))
;; Implemented from Interpreters
(= (cdr-atom $atom)
  (eval (if-decons-expr $atom $_ $tail
    $tail
    (Error (cdr-atom $atom) "cdr-atom expects a non-empty expression as an argument") )))

;; Public MeTTa
(@doc quote
  (@desc "Prevents atom from being reduced")
  (@params (
    (@param "Atom")))
  (@return "Quoted atom"))
;; Implemented from Interpreters
(: quote (-> Atom Atom))
(= (quote $atom) NotReducible)

;; Public MeTTa
(@doc unquote
  (@desc "Unquotes quoted atom, e.g. (unquote (quote $x)) returns $x")
  (@params (
    (@param "Quoted atom")))
  (@return "Unquoted atom"))
;; Implemented from Interpreters
(: unquote (-> %Undefined% %Undefined%))
(= (unquote (quote $atom)) $atom)

; TODO: there is no way to define operation which consumes any number of
; arguments  and returns unit
;; Public MeTTa
(@doc nop
  (@desc "Outputs unit atom for any input")
  (@params (
    (@param "Anything")))
  (@return "Unit atom"))
(: nop (-> Atom (->)))
;; Implemented from Interpreters
(ALT= (nop $x) ())
(: nop (-> EmptyType))
(ALT= (nop) ())

;; Public MeTTa
(@doc empty
  (@desc "Cuts evaluation of the non-deterministic branch and removes it from the result")
  (@params ())
  (@return "Nothing"))
(: empty (-> %Undefined%))
;; Implemented from Interpreters
; (ALT= (empty) (let a b never-happens))
;For testing
;(= (empty) Empty)

;(= (empty-rust1) (let a b never-happens))
; TODO: MINIMAL added for compatibility, remove after migration
;(= (empty-minimal) Empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Documentation formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Public MeTTa
(@doc @doc
  (@desc "Used for documentation purposes. Function documentation starts with @doc")
  (@params (
    (@param "Function name")
    (@param "Function description. Starts with @desc")
    (@param "(Optional) parameters description starting with @params which should contain one or more @param symbols")
    (@param "(Optional) description of what function will return. Starts with @return")))
  (@return "Function documentation using @doc-formal"))
(: @doc (-> Atom DocDescription DocInformal))
(: @doc (-> Atom DocDescription DocParameters DocReturnInformal DocInformal))
;; Is Data Functor

;; Public MeTTa
(@doc @desc
  (@desc "Used for documentation purposes. Description of function starts with @desc as a part of @doc")
  (@params (
    (@param "String containing function description")))
  (@return "Function description"))
(: @desc (-> String DocDescription))
;; Is Data Functor

;; Public MeTTa
(@doc @param
  (@desc "Used for documentation purposes. Description of function parameter starts with @param as a part of @params which is a part of @doc")
  (@params (
    (@param "String containing parameter description")))
  (@return "Parameter description"))
(: @param (-> String DocParameterInformal))
(: @param (-> DocType DocDescription DocParameter))
;; Is Data Functor

;; Public MeTTa
(@doc @return
  (@desc "Used for documentation purposes. Description of function return value starts with @return as a part of @doc")
  (@params (
    (@param "String containing return value description")))
  (@return "Return value description"))
(: @return (-> String DocReturnInformal))
(: @return (-> DocType DocDescription DocReturn))
;; Is Data Functor

;; Public MeTTa
(@doc @doc-formal
  (@desc "Used for documentation purposes. get-doc returns documentation starting with @doc-formal symbol. @doc-formal contains 6 or 4 parameters depending on the entity being described (functions being described using 6 parameters, atoms - 4 parameters)")
  (@params (
    (@param "Function/Atom name for which documentation is to be displayed. Format (@item name)")
    (@param "Contains (@kind function) or (@kind atom) depends on entity which documentation is displayed")
    (@param "Contains type notation of function/atom")
    (@param "Function/atom description")
    (@param "(Functions only). Description of function parameters")
    (@param "(Functions only). Description of function's return value")))
  (@return "Expression containing full documentation on function"))
(: @doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: @doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))
;; Is Data Functor

;; Public MeTTa
(@doc @item
  (@desc "Used for documentation purposes. Converts atom/function's name to DocItem")
  (@params (
    (@param "Atom/Function name to be documented")))
  (@return "(@item Atom) entity"))
(: @item (-> Atom DocItem))
;; Is Data Functor

; TODO: help! gives two outputs
;Atom (@kind function): (%Undefined% (-> Atom Atom)) Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
;Atom (@kind function): DocKindFunction Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
;; Public MeTTa
(@doc (@kind function)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case"))
(: (@kind function) DocKindFunction)
;; Is Data Functor

;; Public MeTTa
(@doc (@kind atom)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind atom) in this case"))
(: (@kind atom) DocKindAtom)
;; Is Data Functor

;; Public MeTTa
(@doc @type
  (@desc "Used for documentation purposes. Converts atom/function's type to DocType")
  (@params (
    (@param "Atom/Function type to be documented")))
  (@return "(@type Type) entity"))
(: @type (-> Type DocType))
;; Is Data Functor

;; Public MeTTa
(@doc @params
  (@desc "Used for function documentation purposes. Contains several @param entities with description of each @param")
  (@params (
    (@param "Several (@param ...) entities")))
  (@return "DocParameters containing description of all parameters of function in form of (@params ((@param ...) (@param ...) ...))"))
(: @params (-> Expression DocParameters))
;; Is Data Functor

;; Public MeTTa
(@doc get-doc
  (@desc "Returns documentation for the given Atom/Function")
  (@params (
    (@param "Atom/Function name for which documentation is needed")))
  (@return "Documentation for the given atom/function"))
(: get-doc (-> Atom Atom))
(: metta-get-doc (-> Atom Atom))
(= (metta-get-doc $atom)
  (let $meta-type (get-metatype $atom)
    (case $meta-type (
      (Expression (get-doc-atom $atom))
      ($_ (get-doc-single-atom $atom)) ))))

;; Helper Library
(@doc get-doc-single-atom
  (@desc "Function used by get-doc to get documentation on either function or atom. It checks if input name is the name of function or atom and calls correspondent function")
  (@params (
    (@param "Atom/Function name for which documentation is needed")))
  (@return "Documentation for the given atom/function"))
(: get-doc-single-atom (-> Atom Atom))
(= (get-doc-single-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (if (is-function-type $type)
      (get-doc-function $atom $type)
      (get-doc-atom $atom) ))))

;; Helper Library
(@doc get-doc-function
  (@desc "Function used by get-doc-single-atom to get documentation on a function. It returns documentation on a function if it exists or default documentation with no description otherwise")
  (@params (
    (@param "Function name for which documentation is needed")
    (@param "Type notation for this function")))
  (@return "Documentation for the given function"))
(: get-doc-function (-> Atom Type Atom))
(= (get-doc-function $name $type)
  (let $top-space (mod-space! top)
  (unify $top-space (@doc $name $desc (@params $params) $ret)
    (let $type' (if (== $type %Undefined%) (undefined-doc-function-type $params) (cdr-atom $type))
    (let ($params' $ret') (get-doc-params $params $ret $type')
      (@doc-formal (@item $name) (@kind function) (@type $type) $desc (@params $params') $ret')))
    (@doc-formal (@item $name) (@kind function) (@type $type) (@desc "No documentation")) )))

;; Helper Library
(@doc undefined-doc-function-type
  (@desc "Function used by get-doc-single-atom in case of absence of function's type notation")
  (@params (
    (@param "List of parameters for the function we want to get documentation for")))
  (@return "List of %Undefined% number of which depends on input list size. So for two parameters function will return (%Undefined% %Undefined% %Undefined%)"))
(: undefined-doc-function-type (-> Expression Type))
(= (undefined-doc-function-type $params)
  (if (== () $params) (%Undefined%)
    (let $params-tail (cdr-atom $params)
    (let $tail (undefined-doc-function-type $params-tail)
      (cons-atom %Undefined% $tail) ))))

;; Helper Library
(@doc get-doc-params
  (@desc "Function used by get-doc-function to get function's parameters documentation (including return value)")
  (@params (
    (@param "List of parameters in form of ((@param Description) (@param Description)...)")
    (@param "Return value's description in form of (@return Description)")
    (@param "Type notation without -> starting symbol e.g. (Atom Atom Atom)")))
  (@return "United list of params and return value each augmented with its type. E.g. (((@param (@type Atom) (@desc Description)) (@param (@type Atom) (@desc Description2))) (@return (@type Atom) (@desc Description)))"))
(: get-doc-params (-> Expression Atom Expression (Expression Atom)))
(= (get-doc-params $params $ret $types)
  (let $head-type (car-atom $types)
  (let $tail-types (cdr-atom $types)
    (if (== () $params)
      (let (@return $ret-desc) $ret
        (() (@return (@type $head-type) (@desc $ret-desc))) )
      (let (@param $param-desc) (car-atom $params)
        (let $tail-params (cdr-atom $params)
        (let ($params' $result-ret) (get-doc-params $tail-params $ret $tail-types)
        (let $result-params (cons-atom (@param (@type $head-type) (@desc $param-desc)) $params')
          ($result-params $result-ret) ))))))))

;; Helper Library
(@doc get-doc-atom
  (@desc "Function used by get-doc (in case of input type Expression) and get-doc-single-atom (in case input value is not a function) to get documentation on input value")
  (@params (
    (@param "Atom's name to get documentation for")))
  (@return "Documentation on input Atom"))
(: get-doc-atom (-> Atom Atom))
(= (get-doc-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (unify $top-space (@doc $atom $desc)
      (@doc-formal (@item $atom) (@kind atom) (@type $type) $desc)
      (unify $top-space (@doc $atom $desc' (@params $params) $ret)
        (get-doc-function $atom %Undefined%)
        (@doc-formal (@item $atom) (@kind atom) (@type $type) (@desc "No documentation")) )))))

;; Public MeTTa
(@doc help!
  (@desc "Function prints documentation for the input atom.")
  (@params (
    (@param "Input to get documentation for")))
  (@return "Unit atom"))
(: help! (-> Atom (->)))
(: metta-help! (-> Atom (->)))
(= (metta-help! $atom)
  (case (get-doc $atom) (
    ((@doc-formal (@item $item) (@kind function) (@type $type) (@desc $descr)
                 (@params $params)
                 (@return (@type $ret-type) (@desc $ret-desc)))
      (let () (println! (format-args "Function {}: {} {}" ($item $type $descr)))
      (let () (println! (format-args "Parameters:" ()))
      (let () (for-each-in-atom $params help-param!)
      (let () (println! (format-args "Return: (type {}) {}" ($ret-type $ret-desc)))
      () )))))
    ((@doc-formal (@item $item) (@kind function) (@type $type) (@desc $descr))
      (let () (println! (format-args "Function {} (type {}) {}" ($item $type $descr)))
      () ))
    ((@doc-formal (@item $item) (@kind atom) (@type $type) (@desc $descr))
      (let () (println! (format-args "Atom {}: {} {}" ($item $type $descr)))
      () ))
    ($other (Error $other "Cannot match @doc-formal structure") ))))

;; Helper Library
(@doc help-param!
  (@desc "Function used by function help! to output parameters using println!")
  (@params (
    (@param "Parameters list")))
  (@return "Unit atom"))
(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (@param (@type $type) (@desc $desc)) $param
    (println! (format-args "  {} {}" ((type $type) $desc))) ))

;; Helper Library
(@doc for-each-in-atom
  (@desc "Applies function passed as a second argument to each atom inside first argument")
  (@params (
    (@param "Expression to each atom in which function will be applied")
    (@param "Function to apply")))
  (@return "Unit atom"))
(: for-each-in-atom (-> Expression Atom (->)))
(= (for-each-in-atom $expr $func)
  (if (noreduce-eq $expr ())
    ()
    (let $head (car-atom $expr)
      (let $tail (cdr-atom $expr)
      (let $_ ($func $head)
      (for-each-in-atom $tail $func) )))))

;; Helper Library
(@doc noreduce-eq
  (@desc "Checks equality of two atoms without reducing them")
  (@params (
    (@param "First atom")
    (@param "Second atom")))
  (@return "True if not reduced atoms are equal, False - otherwise"))
(: noreduce-eq (-> Atom Atom Bool))
(= (noreduce-eq $a $b) (== (quote $a) (quote $b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grounded function's documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Public MeTTa
(@doc add-atom
  (@desc "Adds atom into the atomspace without reducing it")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
(: add-atom (-> hyperon::space::DynSpace Atom (->)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc new-space
  (@desc "Creates new Atomspace which could be used further in the program as a separate from &self Atomspace")
  (@params ())
  (@return "Reference to a new space"))
(: new-space (-> hyperon::space::DynSpace))
;; Implemented from Interpreters

;; Public MeTTa
(@doc remove-atom
  (@desc "Removes atom from the input Atomspace")
  (@params (
    (@param "Reference to the space from which the Atom needs to be removed")
    (@param "Atom to be removed")))
  (@return "Unit atom"))
(: remove-atom (-> hyperon::space::DynSpace Atom (->)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc get-atoms
  (@desc "Shows all atoms in the input Atomspace")
  (@params (
    (@param "Reference to the space")))
  (@return "List of all atoms in the input space"))

(: get-atoms (-> hyperon::space::DynSpace Atom))

;; Implemented from Interpreters

;; Public MeTTa
(@doc new-state
  (@desc "Creates a new state atom wrapping its argument")
  (@params (
    (@param "Atom to be wrapped")))
  (@return "Returns (State $value) where $value is an argument to a new-state"))
(: new-state (-> $tnso (StateMonad $tnso)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc change-state!
  (@desc "Changes input state's wrapped atom to another value (second argument). E.g. (change-state! (State 5) 6) -> (State 6)")
  (@params (
    (@param "State created by new-state function")
    (@param "Atom which will replace wrapped atom in the input state")))
  (@return "State with replaced wrapped atom"))
(: change-state! (-> (StateMonad $tcso) $tcso (StateMonad $tcso)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc get-state
  (@desc "Gets a state as an argument and returns its wrapped atom. E.g. (get-state (State 5)) -> 5")
  (@params (
    (@param "State")))
  (@return "Atom wrapped by state"))
(: get-state (-> (StateMonad $tgso) $tgso))
;; Implemented from Interpreters

;; Public MeTTa
(@doc get-type
  (@desc "Returns type notation of input atom")
  (@params (
    (@param "Atom to get type for")))
  (@return "Type notation or %Undefined% if there is no type for input Atom"))
(: get-type (-> Atom Atom))
(: get-type (-> Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc get-type-space
  (@desc "Returns type notation of input Atom (second argument) relative to a specified atomspace (first argument)")
  (@params (
    (@param "Atomspace where type notation for input atom will be searched")
    (@param "Atom to get type for")))
  (@return "Type notation or %Undefined% if there is no type for input Atom in provided atomspace"))
;; Implemented from Interpreters?
(: get-type-space (-> Atom Atom Atom))
(= (get-type-space $space $atom)
	(get-type $atom $space))

;; Public MeTTa
(@doc get-metatype
  (@desc "Returns metatype of the input atom")
  (@params (
    (@param "Atom to get metatype for")))
  (@return "Metatype of input atom"))
(: get-metatype (-> Atom Atom))
;; Implemented from Interpreters  

;; Public MeTTa
(@doc unify
  (@desc "Like Match but allows any sort of container for the first argument. (Match only allows MeTTa spaces.)")
  (@params (
	(@param "The collection or space to match")
	(@param "Second atom to unify with")
	(@param "Result if two atoms unified successfully")
	(@param "Result otherwise")))
  (@return "Third argument when found or forth one otherwise"))
(: unify (-> Atom Atom Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc match
  (@desc "Searches for all declared atoms corresponding to the given pattern (second argument) and produces the output pattern (third argument)")
  (@params (
    (@param "A grounded atom referencing a Space")
    (@param "Pattern atom to be matched")
    (@param "Output pattern typically containing variables from the input pattern")))
  (@return "If match was successful it outputs pattern (third argument) with filled variables (if any were present in pattern) using matched pattern (second argument). Nothing - otherwise"))
(: match (-> Atom Atom Atom %Undefined%))
;; Implemented from Interpreters
;(ALT= (match $space $pattern $template)
;  (unify $space $pattern $template Empty))

;; Public MeTTa
(@doc register-module!
  (@desc "Takes a file system path (first argument) and loads the module into the runner")
  (@params (
    (@param "File system path")))
  (@return "Unit atom"))
(: register-module! (-> Atom (->)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc mod-space!
  (@desc "Returns the space of the module (first argument) and tries to load the module if it is not loaded into the module system")
  (@params (
    (@param "Module name")))
  (@return "Space name"))
(: mod-space! (-> Atom hyperon::space::DynSpace))
;; Implemented from Interpreters
(= (mod-space! self) &self)
(= (mod-space! top) &self)
(= (mod-space! corelib) &corelib)
(= (mod-space! stdlib) &stdlib)
(= (mod-space! catalog) &catalog)


;; Public MeTTa
(@doc print-mods!
  (@desc "Prints all modules with their correspondent spaces")
  (@params ())
  (@return "Unit atom"))
(: print-mods! (-> (->)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc assertEqual
  (@desc "Compares (sets of) results of evaluation of two expressions")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "Unit atom if both expression after evaluation is equal, error - otherwise"))
(: assertEqual (-> Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc assertEqualToResult
  (@desc "Same as assertEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation")
  (@params (
    (@param "First expression (it will be evaluated)")
    (@param "Second expression (it won't be evaluated)")))
  (@return "Unit atom if both expression after evaluation is equal, error - otherwise"))
(: assertEqualToResult (-> Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc collapse
  (@desc "Converts a nondeterministic result into a tuple")
  (@params (
    (@param "Atom which will be evaluated")))
  (@return "Tuple"))
(: collapse (-> Atom Atom))
;; Implemented from Interpreters
  
;; Public MeTTa
(@doc capture
  (@desc "Wraps an atom and capture the current space")
  (@params (
    (@param "Function name which space need to be captured")))
  (@return "Function"))
(: capture (-> Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc case
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom (it will be evaluated)")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result of evaluating of Atom bound to met condition"))
(: case (-> Atom Expression Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc superpose
  (@desc "Turns a tuple (first argument) into a nondeterministic result")
  (@params (
    (@param "Tuple to be converted")))
  (@return "Argument converted to nondeterministic result"))
(: superpose (-> Expression %Undefined%))
;; Implemented from Interpreters

;; Public MeTTa
(@doc hyperpose
  (@desc "Turns a tuple (first argument) into a nondeterministic result, as superpose, but is explicitly concurrent. Each item of the tuple may be processed in parallel, depending on the number of threads available (which is the minimum of the tuple size and the number of cores available).")
  (@params (
    (@param "Tuple to be converted")))
  (@return "Argument converted to nondeterministic result"))
(: hyperpose (-> Expression %Undefined%))

;; Public MeTTa
(@doc sequential
  (@desc "Turns a tuple (first argument) into a nondeterministic result, as superpose, but evaluation order of the elements of the tuple is fixed left to right. In that sense the result order is deterministic iff evaluating the tuple elements is deterministic.")
  (@params (
    (@param "Tuple to be evaluated")))
  (@return "Sequential results of the tuple's elements."))
(: sequential (-> Expression %Undefined%))

;; Public MeTTa
(@doc do
  (@desc "Completely evaluates form, returning nothing. Typically used for side-effects. A common pattern is (sequential ((do <side-effect-form>) <form-that-needs-side-effect>)).")
  (@params (
    (@param "Form")))
  (@return "None"))
(: do (-> Expression %Undefined%))

;; Public MeTTa
(@doc pragma!
  (@desc "Changes global key's (first argument) value to a new one (second argument)")
  (@params (
    (@param "Key's name")
    (@param "New value")))
  (@return "Unit atom"))
(: pragma! (-> Symbol %Undefined% (->)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc import!
  (@desc "Imports module using its relative path (second argument) and binds it to the token (first argument) which will represent imported atomspace. If first argument is &self then everything will be imported to current atomspace")
  (@params (
    (@param "Symbol, which is turned into the token for accessing the imported module")
    (@param "Module name")))
  (@return "Unit atom"))
;; Implemented from Interpreters
(: import! (-> Atom Atom (->)))

;; Public MeTTa
(@doc include
  (@desc "Works just like import! but with &self as a first argument. So everything from input file will be included in the current atomspace and evaluated")
  (@params (
    (@param "Name of metta script to import")))
  (@return "Unit atom"))
(: include (-> Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc bind!
  (@desc "Registers a new token which is replaced with an atom during the parsing of the rest of the program")
  (@params (
    (@param "Token name")
    (@param "Atom, which is associated with the token after reduction")))
  (@return "Unit atom"))
(: bind! (-> Symbol %Undefined% (->)))
;; Implemented from Interpreters
  
;; Public MeTTa
(@doc trace!
  (@desc "Prints its first argument and returns second. Both arguments will be evaluated before processing")
  (@params (
    (@param "Atom to print")
    (@param "Atom to return")))
  (@return "Evaluated second input"))
(: trace! (-> %Undefined% $a $a))
;; Implemented from Interpreters

;; Public MeTTa
(@doc println!
  (@desc "Prints a line of text to the console")
  (@params (
    (@param "Expression/atom to be printed out")))
  (@return "Unit atom"))
(: println! (-> %Undefined% (->)))
;; Implemented from Interpreters

;; Public MeTTa
(@doc format-args
  (@desc "Fills {} symbols in the input expression with atoms from the second expression. E.g. (format-args (Probability of {} is {}%) (head 50)) gives [(Probability of head is 50%)]. Atoms in the second input value could be variables")
  (@params (
    (@param "Expression with {} symbols to be replaced")
    (@param "Atoms to be placed inside expression instead of {}")))
  (@return "Expression with replaced {} with atoms"))
(: format-args (-> String Atom String))
;; Implemented from Interpreters

;; Public MeTTa
(@doc sealed
  (@desc "Replaces all occurrences of any var from var list (first argument) inside atom (second argument) by unique variable. Can be used to create a locally scoped variables")
  (@params (
	(@param "Variable list e.g. ($x $y)")
	(@param "Atom which uses those variables")))
  (@return "Second argument but with variables being replaced with unique variables"))
(: sealed (-> Expression Atom Atom))
;; Implemented from Interpreters

(:> ValueAtom Atom)
(:> ValueAtom Grounded)
(:> ForeignObject ValueAtom)
(:> PyObject ForeignObject)

;; Public MeTTa
(@doc py-list
  (@desc "Converts a MeTTa Expression into a Python list and returns it as a PyObject.")
  (@params (
    (@param (@desc "A MeTTa List that will be converted into a Python list"))
  ))
  (@return (@desc "A Python list object that represents the given MeTTa Expression as a PyObject"))
)
(: py-list (-> Expression PyObject))
;; Implemented from Interpreters

;; Public MeTTa
(@doc py-chain
  (@desc "Chains together a list of Python objects contained in a MeTTa Expression, applying the Python vertical bar | OR operation jointly to all elements.")
  (@params (
    (@param (@desc "A MeTTa list of atoms, each embedding a Python object.")) 
  ))
  (@return (@desc "A MeTTa atom which embeds the result of applying the Python OR | operator to all elements of the list."))
)
(: py-chain (-> Expression PyObject))
;; Implemented from Interpreters

;; Public MeTTaLog
(@doc py-eval
  (@desc "Evaluates a Python expression from a string and returns the result as a PyObject.")
  (@params (
    (@param (@desc "A string representing a Python expression that will be evaluated"))
  ))
  (@return (@desc "The result of evaluating the Python expression as a PyObject"))
)
(: py-eval (-> String PyObject))
;; Implemented from Interpreters

;; Public MeTTaLog
(@doc py-exec!
  (@desc "Executes some Python sourcecode from a string !(py-exec! \"import torch\") -> True.  !(py-exec! \"import torche\") -> False. ")
  (@params (
    (@param (@type String) (@desc "A string representing a Python sourcecode that will be executed"))
  ))
  (@return (@desc "The True|False results of executing the Python expression"))
)
(: py-exec! (-> String Bool))
;; Implemented from Interpreters

; TODO: Segmentation fault (core dumped) when calling !(help &self)
; TODO: help! not working for &self (segmentation fault)
;(@doc &self
;  (@desc "Returns reference to the current atomspace")
;  (@params ())
;  (@return "Reference to the current atomspace"))

; TODO: help! not working for operations which are defined in both Python and
; Rust standard library: +, -, *, /, %, <, >, <=, >=, ==
;; Public MeTTa
(@doc +
  (@desc "Sums two numbers")
  (@params (
    (@param "Addend")
    (@param "Augend")))
  (@return "Sum"))
(: + (-> Number Number Number))
;; Implemented from Interpreters

;; Public MeTTa
(@doc -
  (@desc "Subtracts second argument from first one")
  (@params (
    (@param "Minuend")
    (@param "Deductible")))
  (@return "Difference"))
(: - (-> Number Number Number))
;; Implemented from Interpreters

;; Public MeTTa
(@doc *
  (@desc "Multiplies two numbers")
  (@params (
    (@param "Multiplier")
    (@param "Multiplicand")))
  (@return "Product"))
(: * (-> Number Number Number))
;; Implemented from Interpreters

;; Public MeTTa
(@doc /
  (@desc "Divides first argument by second one")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Fraction"))
(: / (-> Number Number Number))
;; Implemented from Interpreters

;; Public MeTTa
(@doc %
  (@desc "Modulo operator. It returns remainder of dividing first argument by second argument")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Remainder"))
(: % (-> Number Number Number))
;; Implemented from Interpreters

;; Public MeTTa
(@doc <
  (@desc "Less than. Checks if first argument is less than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than second, False - otherwise"))
(: < (-> Number Number Bool))
;; Implemented from Interpreters

;; Public MeTTa
(@doc >
  (@desc "Greater than. Checks if first argument is greater than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than second, False - otherwise"))
(: > (-> Number Number Bool))
;; Implemented from Interpreters

;; Public MeTTa
(@doc <=
  (@desc "Less than or equal. Checks if first argument is less than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than or equal to second, False - otherwise"))
(: <= (-> Number Number Bool))
;; Implemented from Interpreters

;; Public MeTTa
(@doc >=
  (@desc "Greater than or equal. Checks if first argument is greater than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than or equal to second, False - otherwise"))
(: >= (-> Number Number Bool))
;; Implemented from Interpreters

;; Public MeTTa
(@doc ==
  (@desc "Checks equality for two arguments of the same type")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if two arguments are equal, False - otherwise. If arguments are of different type function returns Error currently"))
(: == (-> $t $t Bool))
;; Implemented from Interpreters

;; Public MeTTa
(@doc unique
  (@desc "Function takes non-deterministic input (first argument) and returns only unique entities. E.g. (unique (superpose (a b c d d))) -> [a, b, c, d]")
  (@params (
    (@param "Non-deterministic set of values")))
  (@return "Unique non-deterministic values from input set"))
(: unique (-> Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc union
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their union. E.g. (union (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Union of sets"))
(: union (-> Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc intersection
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their intersection. E.g. (intersection (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Intersection of sets"))
(: intersection (-> Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc subtraction
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their subtraction. E.g. !(subtraction (superpose (a b b c)) (superpose (b c c d))) -> [a, b]")
  (@params (
    (@param "Non-deterministic set of values")
    (@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Subtraction of sets"))
(: subtraction (-> Atom Atom Atom))
;; Implemented from Interpreters

;; Public MeTTa
(@doc git-module!
  (@desc "Provides access to module in a remote git repo, from within MeTTa code. Similar to `register-module!`, this op will bypass the catalog search")
  (@params (
    (@param "URL to github repo")))
  (@return "Unit atom"))
(: git-module! (-> Atom (->)))
;; Implemented from Interpreters	

; !(import! &corelib "src/canary/stdlib_mettalog.metta")
;!(println! "!(import! &corelib \"src/canary/stdlib_mettalog.metta\")")

;; Public MeTTa
(@doc findall!
  (@desc "Takes a Template and a Goal. Returns the list resulting from substituting all bindings from solving Goal into Template.
  See Prolog's built-in predicate findall/3.")
  (@params (
    (@param "Template")
    (@param "Goal")))
  (@return "Result list of all bindings for Goal substituted into Template"))
(: findall! (-> Expression Expression Expression))

;; Functional Programming
(@doc maplist!
  (@desc "Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). 
 The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).
 F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).
 Use concurrent-maplist! for a multi-threaded, nondeterministic version.
 See Prolog's built-in predicate maplist.")
  (@params (
    (@param "Function to be applied")
    (@param "List")))
  (@return "Result of applying Function to List(s)"))
(: maplist! (-> Function Expression Expression))
(: maplist! (-> Function Expression Expression Expression))
(: maplist! (-> Function Expression Expression Expression Expression))

;; Functional Programming
(@doc concurrent-maplist!
  (@desc "Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). 
 The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).
 F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).
 The applications of F to the list items are processed in parallel. Because of the overhead of this approach, a speedup is only likely if F is expensive to evaluate.
 Use maplist! for a single-threaded, deterministic version.
 See Prolog's built-in predicate concurrent-maplist.")
  (@params (
    (@param "Function to be applied")
    (@param "List")))
  (@return "Result of applying Function to List(s)"))
(: concurrent-maplist! (-> Function Expression Expression))
(: concurrent-maplist! (-> Function Expression Expression Expression))
(: concurrent-maplist! (-> Function Expression Expression Expression Expression))

;; Public MeTTa
(@doc throw
  (@desc "Raises an exception. See also `catch`; the system will look for the innermost catch such that Exception unifies with Catcher.")
  (@params (
    (@param "Exception")))
  (@return "Does not return - raises an exception"))
(: throw (-> Atom ErrorType))

;; Public MeTTa
(@doc catch
  (@desc "Executes Form. If an exception is raised with `throw` during execution of Form while this is the innermost catch such that Catcher unifies with Exception, the exception is caught. Recover is then executed with bindings from Catcher.")
  (@params (
    (@param "Form")
    (@param "Catcher")
    (@param "Recover")))
  (@return "Result of Form if no exception is raised. Result of Recover (with bindings from Catcher) if an exception is caught."))
(: catch (-> Atom Atom Atom Atom))

;; Public MeTTa
(@doc max-time!
  (@desc "Executes Form. If execution takes longer than Time, will raise a time_limit_exceeded exception. See also `catch`.")
  (@params (
    (@param "Time (in seconds)")
    (@param "Form")))
  (@return "Result of Form if execution completes within Time. Raises an exception otherwise."))
(: max-time! (-> Number Atom Atom))

;; Public MeTTa
(@doc sleep!
  (@desc "Sleeps for N seconds.")
  (@params (
    (@param "N")))
  (@return "Returns True after sleeping completes."))
(: sleep! (-> Number Bool))

;; Public MeTTa
(@doc limit!
  (@desc "Executes Form generating at most Limit results. Results are returned as soon as they become available.")
  (@params (
    (@param "Limit")
    (@param "Form")))
  (@return "First Limit results of Form."))
(: limit! (-> Number Atom Atom))

;; Public MeTTa
(@doc number-of
  (@desc "Returns the number of results Form generates")
  (@params (
    (@param "Form")))
  (@return "Number of results of Form."))
(: number-of (-> Atom Number))

;; Public MeTTa
(@doc offset!
  (@desc "Executes Form ignoring the first Count results. Results are returned as soon as they become available.")
  (@params (
    (@param "Count")
    (@param "Form")))
  (@return "Results of Form after ignoring the first Count results that are generated."))
(: offset! (-> Number Atom Atom))

;; Public MeTTa
(@doc call!
  (@desc "Trampolines to Prolog's call. Only works when the predicate and each arg are provided separately. e.g. !(call! ls /) will print the root directory but !(call! ls(/)) will fail.")
  (@params (
    (@param "Form")))
  (@return "True if the call succeeds, False otherwise."))
(: call! (-> Atom Atom))

;; Public MeTTa
(@doc call-string!
  (@desc "Executes String as Prolog code. For example, (call-string! \"member(1,[1,2,3])\") returns [()] while (call-string! \"member(4,[1,2,3])\") returns []. (call-string! \"member(X,[1,2,3])\") returns [(1), (2), (3)].")
  (@params (
    (@param "String")))
  (@return "A list of the binding values. If there are no bindings but the Prolog query is True, returns the empty list."))
(: call-string! (-> String Atom))

;; Public MeTTa
(@doc call-cleanup!
  (@desc "Same as (setup-call-cleanup! true Form Cleanup). setup-call-cleanup! is recommended instead if Cleanup is intended to undo prior side-effects - place those side-effects in Setup.")
  (@params (
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: call-cleanup! (-> Atom Atom Atom))

;; Public MeTTa
(@doc setup-call-cleanup!
  (@desc "Executes Setup, then Form, then finally Cleanup. Setup is protected from interrupts (e.g. max-time!). In most uses, Setup will perform temporary side-effects required by Form that are finally undone by Cleanup. Cleanup is run even if Form raises an exception. For each result of Setup, Form is run to completion, then Cleanup is run.")
  (@params (
    (@param "Setup")
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: setup-call-cleanup! (-> Atom Atom Atom Atom))
