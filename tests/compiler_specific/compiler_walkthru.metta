








!(pfcWatch!)


; importing host predicates/functions
(: sys-imported (-> Symbol PosInt PosInt Symbol))
(sys-imported current_prolog_flag 2 2 current-prolog-flag-fn)
(: current-prolog-flag-fn (-> Symbol Atom))


!(pragma! load show)
!(pragma! log True)
!(pragma! test False)
!(pragma! compile false)
!(pragma! load debug)

!(load-ontology!)


; Predicate that returns programs argv
; !(add-atom &self (: prog-argv (-> List)))
(: prog-argv (-> List))


;!(test_fwc!)

!(get-type prog-argv)

(= (prog-argv)
   (call-fn-nth! 2 (current_prolog_flag argv)))

!(function-arity prog-argv)


;; introspection
!(assertEqual (function-arity prog-argv) 0)
!(assertEqual (predicate-arity prog-argv) 1)
; !(xlisting! prog-argv)

;; Calling #'prog-argv
!(prog-argv)

;; Matching with #'prog-argv
;EXPANDED !(match &self (prog-argv $res) $res)

; Predicate that takes an argument and does something somewhere
!(-> show-info (-> Atom (->)))
(= (show-info $obj)
   (call! (xlisting $obj)))
   
; !(show-info show-info)

!(assertEqual (predicate-arity show-info) 1)
!(assertEqual (function-arity show-info) 1)


(: is-variable (-> Atom Bool))


!(assertEqual (predicate-arity is-variable) 1)
!(assertEqual  (function-arity is-variable) 1)


; metta_atom_file_buffer(True) ==> True.


(= (is-variable $x) (== (get-metatype $x) Variable))


; !(xlisting! is-variable)
; !(pp_rules!)
; !(pp_facts!)
;  pfcWhy1(bb).
;  pfcWhy1(function_arity('is-variable',1)).
;  mpred_why(function_arity('is-variable',1)).
; !(compile! is-variable)

(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)

(: is-expression (-> Atom Bool))
(= (is-expression $x) (== (get-metatype $x) Expression))

(: is-closed (-> Atom Bool))

(= (is-closed $x) (if (is-variable $x)
					  False
					  (if (== () $x) True
					  (if (is-expression $x)
						  (and (let $head (car-atom $x) (is-closed $head))
							   (let $tail (cdr-atom $x) (is-closed $tail)))
						  True))))

!(compile! is-variable)
!(compile! is-closed)
!(compile-easy!)
!(listing! can_compile)
!(listing! metta_compiled_predicate)


!(pragma! eval trace)


!(is-expression $x) ; rust/mettalog False
!(is-expression (? 4 3)) ; rust/mettalog True
!(is-expression (? 4 $x)) ; rust/mettalog True
!(is-expression 4) ; rust/mettalog False

!(is-closed $x) ; rust/mettalog False
!(is-closed 4) ; rust/mettalog True
!(is-closed (? $y 3)) ; rust gets stuck / mettalog false
!(is-closed (? 4 $x)) ; rust gets stuck / mettalog false
!(is-closed (? 4 3)) ; rust gets stuck / mettalog true




