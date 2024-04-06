

;`$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference
(: if (-> Bool Atom Atom $t))
(= (if True $then $else) $then)
(= (if False $then $else) $else)

(: Error (-> Atom Atom ErrorType))

(= (if-non-empty-expression $atom $then $else)
  (chain (eval (get-metatype $atom)) $type
    (eval (if-equal $type Expression
      (eval (if-equal $atom () $else $then))
      $else ))))

(= (if-decons $atom $head $tail $then $else)
  (eval (if-non-empty-expression $atom
    (chain (decons $atom) $list
      (match $list ($head $tail) $then $else) )
    $else )))

(= (if-empty $atom $then $else)
  (eval (if-equal $atom Empty $then $else)))

(= (if-error $atom $then $else)
  (eval (if-decons $atom $head $_
    (eval (if-equal $head Error $then $else))
    $else )))

(= (return-on-error $atom $then)
  (eval (if-empty $atom Empty
    (eval (if-error $atom $atom
      $then )))))

(= (car $atom)
  (eval (if-decons $atom $head $_
    $head
    (Error (car $atom) "car expects a non-empty expression as an argument") )))

(= (switch $atom $cases)
  (chain (decons $cases) $list (eval (switch-internal $atom $list))))

(= (switch-internal $atom (($pattern $template) $tail))
  (match $atom $pattern $template (eval (switch $atom $tail))))

(= (subst $atom $var $templ)
  (match $atom $var $templ
    (Error (subst $atom $var $templ)
      "subst expects a variable as a second argument") ))

(= (reduce $atom $var $templ)
  (chain (eval $atom) $res
    (eval (if-error $res $res
      (eval (if-empty $res
        (eval (subst $atom $var $templ))
        (eval (reduce $res $var $templ)) ))))))

(= (type-cast $atom $type $space)
  (chain (eval (get-type $atom $space)) $actual-type
    (eval (switch ($actual-type $type)
      (
        ((%Undefined% $_) $atom)
        (($_ %Undefined%) $atom)
        (($type $_) $atom)
        ($_ (Error $atom BadType)) )))))

(= (is-function $type)
  (chain (eval (get-metatype $type)) $meta
    (eval (switch ($type $meta)
      (
        (($_ Expression)
          (chain (eval (car $type)) $head
            (match $head -> True False) ))
        ($_ False) )))))

(= (interpret $atom $type $space)
  (chain (eval (get-metatype $atom)) $meta
    (eval (switch ($type $meta)
      (
        ((Atom $_meta) $atom)
        (($meta $meta) $atom)
        (($_type Variable) $atom)

        (($_type Symbol) (eval (type-cast $atom $type $space)))
        (($_type Grounded) (eval (type-cast $atom $type $space)))
        (($_type Expression) (eval (interpret-expression $atom $type $space))) )))))

(= (interpret-expression $atom $type $space)
  (eval (if-decons $atom $op $args
    (chain (eval (get-type $op $space)) $op-type
      (chain (eval (is-function $op-type)) $is-func
        (match $is-func True
          (chain (eval (interpret-func $atom $op-type $space)) $reduced-atom
            (eval (call $reduced-atom $type $space)) )
          (chain (eval (interpret-tuple $atom $space)) $reduced-atom
            (eval (call $reduced-atom $type $space)) ))))
    (eval (type-cast $atom $type $space)) )))

(= (interpret-func $expr $type $space)
  (eval (if-decons $expr $op $args
    (chain (eval (interpret $op $type $space)) $reduced-op
      (eval (return-on-error $reduced-op
        (eval (if-decons $type $arrow $arg-types
          (chain (eval (interpret-args $expr $args $arg-types $space)) $reduced-args
            (eval (return-on-error $reduced-args
              (cons $reduced-op $reduced-args) )))
          (Error $type "Function type expected") )))))
    (Error $expr "Non-empty expression atom is expected") )))

(= (interpret-args $atom $args $arg-types $space)
  (match $args ()
    (match $arg-types ($ret) () (Error $atom BadType))
    (eval (if-decons $args $head $tail
      (eval (if-decons $arg-types $head-type $tail-types
        (chain (eval (interpret $head $head-type $space)) $reduced-head
          ; check that head was changed otherwise Error or Empty in the head
          ; can be just an argument which is passed by intention
          (eval (if-equal $reduced-head $head
            (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $space))
            (eval (return-on-error $reduced-head
              (eval (interpret-args-tail $atom $reduced-head $tail $tail-types $space)) )))))
        (Error $atom BadType) ))
      (Error (interpret-atom $atom $args $arg-types $space)
        "Non-empty expression atom is expected") ))))

(= (interpret-args-tail $atom $head $args-tail $args-tail-types $space)
  (chain (eval (interpret-args $atom $args-tail $args-tail-types $space)) $reduced-tail
    (eval (return-on-error $reduced-tail
      (cons $head $reduced-tail) ))))

(= (interpret-tuple $atom $space)
  (match $atom ()
    $atom
    (eval (if-decons $atom $head $tail
      (chain (eval (interpret $head %Undefined% $space)) $rhead
        (chain (eval (interpret-tuple $tail $space)) $rtail
          (cons $rhead $rtail) ))
      (Error (interpret-tuple $atom $space) "Non-empty expression atom is expected as an argument") ))))

(= (call $atom $type $space)
  (chain (eval $atom) $result
    (eval (if-empty $result $atom
      (eval (if-error $result $result
        (eval (interpret $result $type $space)) ))))))


