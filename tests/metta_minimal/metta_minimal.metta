;For example the following code (written using MeTTa runner syntax) returns B:

(= (foo $b) (function (chain (unify B $b () ()) $_ (return ()))))
!(chain (eval (foo $a)) $_ $a)

;if two separate expressions in the space have a variable with the same name, but the variables reside in independent scopes, then the variables are different. Consider the following example:

(= (foo) (function (chain (unify A $a () ()) $_ (return ()))))
!(chain (eval (foo)) $_ $a)



;Recursive switch implementation:

(= (switch $atom $cases)
  (function
    (chain (decons-atom $cases) $list
      (chain (eval (switch-internal $atom $list)) $res
        (unify $res NotReducible (return Empty) (return $res)) ))))

(= (switch-internal $atom (($pattern $template) $tail))
  (function
    (unify $atom $pattern
      (return $template)
      (chain (eval (switch $atom $tail)) $ret (return $ret)) )))

;Evaluate atom in a loop until result is calculated:

(= (subst $atom $var $templ)
  (unify $atom $var $templ
    (Error (subst $atom $var $templ)
      \"subst expects a variable as a second argument\") ))

(= (reduce $atom $var $templ)
  (chain (eval $atom) $res
    (unify $res Empty
    Empty
    (unify $res (Error $a $m)
      (Error $a $m)
      (unify $res NotReducible
        (eval (subst $atom $var $templ))
        (eval (reduce $res $var $templ)) )))))


;The following program implements a Turing machine using the minimal MeTTa instruction set (the full code of the example can be found here):

(= (tm $rule $state $tape)
  (function (eval (tm-body $rule $state $tape))) )

(= (tm-body $rule $state $tape)
  (unify $state HALT
    (return $tape)
    (chain (eval (read $tape)) $char
      (chain (eval ($rule $state $char)) $res
        (unify $res ($next-state $next-char $dir)
          (chain (eval (move $tape $next-char $dir)) $next-tape
            (eval (tm-body $rule $next-state $next-tape)) )
          (return (Error (tm-body $rule $state $tape) \"Incorrect state\")) )))))

(= (read ($head $hole $tail)) $hole)

(= (move ($head $hole $tail) $char N) ($head $char $tail))
(= (move ($head $hole $tail) $char L) (function
  (chain (cons-atom $char $head) $next-head
    (chain (decons-atom $tail) $list
      (unify $list ($next-hole $next-tail)
        (return ($next-head $next-hole $next-tail))
        (return ($next-head 0 ())) )))))
(= (move ($head $hole $tail) $char R) (function
  (chain (cons-atom $char $tail) $next-tail
    (chain (decons-atom $head) $list
      (unify $list ($next-hole $next-head)
        (return ($next-head $next-hole $next-tail))
        (return (() 0 $next-tail)) )))))


;Each instruction in a minimal instruction set is a total function. Nevertheless Empty allows defining partial functions in MeTTa. For example partial if can be defined as follows:

(= (if $condition $then) (unify $condition True $then Empty))




;then such instruction should be wrapped into a non-minimal MeTTa expression and unwrapped after the substitution is made.

  (chain (quote (eval (foo))) $x
    (unify $x (quote $y)
      $y
      (Error $x "quote expression expected") ))


;Then it returns the <atom> as a result. If one need to make a substitution it is possible using:

  (chain (function (return <atom>)) <var> <templ>)

;One more option is to make chain (and other atoms which can have nested evaluation loops) recognize return. In such case the evaluation loop is executed by the chain itself and function instruction is not needed. Substitution gets the simpler form:

  (chain (return <atom>) <var> <templ>)



