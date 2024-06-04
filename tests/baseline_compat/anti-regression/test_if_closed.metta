

(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

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



;!(pragma! eval trace)

!(is-variable $x) ; rust/mettalog True
!(is-variable 1) ; rust/mettalog False


!(is-expression $x) ; rust/mettalog False
!(is-expression (⍃ 4 3)) ; rust/mettalog True
!(is-expression (⍃ 4 $x)) ; rust/mettalog True
!(is-expression 4) ; rust/mettalog False

!(is-closed $x) ; rust/mettalog False
!(is-closed 4) ; rust/mettalog True
!(is-closed (⍃ $y 3)) ; rust gets stuck / mettalog false
!(is-closed (⍃ 4 $x)) ; rust gets stuck / mettalog false
!(is-closed (⍃ 4 3)) ; rust gets stuck / mettalog true



