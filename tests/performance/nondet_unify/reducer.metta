;
;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                        A Graph Reducer for T-Combinators:

;
;                        Reduces a T-combinator expression to a final answer.  Recognizes

;
;                         the combinators I,K,S,B,C,S',B',C', cond, apply, arithmetic, tests,

;
;                         basic list operations, and function definitions in the data base stored

;
;                         as facts of the form t_def(_func, _args, _expr).

;
;                         Written by Peter Van Roy


;
;                          Uses write/1, compare/3, functor/3, arg/3.


  (= 
    (top) 
    ( (try 
        (fac 3) $ans1) (try (quick (:: 3 1 2)) $ans2)))
;                          ;	write(_ans1), nl,


;
;                         	write(_ans2), nl.



  (= 
    (try $inpexpr $anslist) 
    ( (listify $inpexpr $list) 
      (curry $list $curry) 
      (t-reduce $curry $ans) 
      (make-list $ans $anslist)))

;
;                          ;	SWI-MeTTa V7 compatibility hacks.



  (= 
    (end $X) 
    ( (atom $X) (set-det)))
  (= 
    (end $X) 
    (== $X Nil))


  (= 
    (list-functor-name $Name) 
    ( (functor 
        (Cons  $_ $_) $Name0 $_) (= $Name $Name0)))
;                             ; workaround a functor/3 error-checking issue with GNU MeTTa



;
;                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                              Examples of applicative functions which can be compiled & executed.

;
;                              This test version compiles them just before each execution.


;
;                              Factorial function:


  (= 
    (t_def  fac 
      ($N) 
      (cond  
        (= $N 0) 1 
        (*  $N 
          (fac  
            (-  $N 1))))) True)

;
;                               Quicksort:

  (= 
    (t_def  quick 
      ($l) 
      (cond  
        (= $l ()) () 
        (cond  
          (= 
            (tl  $l) ()) $l 
          (quick2  
            (split  
              (hd  $l) 
              (tl  $l)))))) True)
  (= 
    (t_def  quick2 
      ($l) 
      (append  
        (quick  
          (hd  $l)) 
        (quick  
          (tl  $l)))) True)

  (= 
    (t_def  split 
      ($e $l) 
      (cond  
        (= $l ()) 
        ( ($e)) 
        (cond  
          (=<  
            (hd  $l) $e) 
          (inserthead  
            (hd  $l) 
            (split  $e 
              (tl  $l))) 
          (inserttail  
            (hd  $l) 
            (split  $e 
              (tl  $l)))))) True)
  (= 
    (t_def  inserthead 
      ($e $l) 
      (Cons  
        (Cons  $e 
          (hd  $l)) 
        (tl  $l))) True)
  (= 
    (t_def  inserttail 
      ($e $l) 
      (Cons  
        (hd  $l) 
        (Cons  $e 
          (tl  $l)))) True)

  (= 
    (t_def  append 
      ($a $b) 
      (cond  
        (= $a ()) $b 
        (Cons  
          (hd  $a) 
          (append  
            (tl  $a) $b)))) True)

;
;                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                   Full reduction:

;
;                                   A dot '.' is printed for each reduction step.



  (= 
    (t-reduce $expr $ans) 
    ( (atomic $expr) 
      (set-det) 
      (= $ans $expr)))
;
;                                    The reduction of '$cons' must be here to avoid an infinite loop

  (= 
    (t-reduce 
      (Cons  $y 
        (Cons  $x $LF)) 
      (Cons  $yr 
        (Cons  $xr $LF))) 
    ( (list-functor-name $LF) 
      (t-reduce $x $xr) 
      (set-det) 
      (t-reduce $y $yr) 
      (set-det)))
  (= 
    (t-reduce $expr $ans) 
    ( (t-append $next $red $form $expr) 
      (t-redex $form $red) 
      (set-det) 
      (t-reduce $next $ans) 
      (set-det)))
;                                    ;	write('.'),




  (= 
    (t_append  $link $link $l $l) True)
  (= 
    (t-append 
      (Cons  $a $l1) $link $l2 
      (Cons  $a $l3)) 
    (t-append $l1 $link $l2 $l3))

;
;                                      One step of the reduction:


;
;                                      Combinators:


  (= 
    (t-redex 
      (Cons  $x 
        (Cons  $g 
          (Cons  $f 
            (Cons  $k sp)))) 
      (Cons  
        (Cons  $xr $g) 
        (Cons  
          (Cons  $xr $f) $k))) 
    (t-reduce $x $xr))
  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f 
            (Cons  $k bp)))) 
      (Cons  
        (Cons  $x $g) 
        (Cons  $f $k))) True)
  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f 
            (Cons  $k cp)))) 
      (Cons  $g 
        (Cons  
          (Cons  $x $f) $k))) True)
  (= 
    (t-redex 
      (Cons  $x 
        (Cons  $g 
          (Cons  $f s))) 
      (Cons  
        (Cons  $xr $g) 
        (Cons  $xr $f))) 
    (t-reduce $x $xr))
  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f b))) 
      (Cons  
        (Cons  $x $g) $f)) True)
  (= 
    (t_redex  
      (Cons  $x 
        (Cons  $g 
          (Cons  $f c))) 
      (Cons  $g 
        (Cons  $x $f))) True)
  (= 
    (t_redex  
      (Cons  $y 
        (Cons  $x k)) $x) True)
  (= 
    (t_redex  
      (Cons  $x i) $x) True)

;
;                                          Conditional:

  (= 
    (t-redex 
      (Cons  $elsepart 
        (Cons  $ifpart 
          (Cons  $cond cond))) $ifpart) 
    ( (t-reduce $cond $bool) 
      (= $bool True) 
      (set-det)))
	;
;                                          Does NOT work if _bool is substituted in the call!

  (= 
    (t_redex  
      (Cons  $elsepart 
        (Cons  $ifpart 
          (Cons  $cond cond))) $elsepart) True)

;
;                                          Apply:

  (= 
    (t-redex 
      (Cons  $f apply) $fr) 
    (t-reduce $f $fr))

;
;                                          List operations:

  (= 
    (t-redex 
      (Cons  $arg hd) $x) 
    ( (list-functor-name $LF) (t-reduce $arg (Cons  $y (Cons  $x $LF)))))
  (= 
    (t-redex 
      (Cons  $arg tl) $y) 
    ( (list-functor-name $LF) (t-reduce $arg (Cons  $y (Cons  $x $LF)))))

;
;                                          Arithmetic:

  (= 
    (t-redex 
      (Cons  $y 
        (Cons  $x $op)) $res) 
    ( (end $op) 
      (my-member $op 
        (:: + - * // mod)) 
      (t-reduce $x $xres) 
      (t-reduce $y $yres) 
      (number $xres) 
      (number $yres) 
      (eval $op $res $xres $yres)))

;
;                                          Tests:

  (= 
    (t-redex 
      (Cons  $y 
        (Cons  $x $test)) $res) 
    ( (end $test) 
      (my-member $test 
        (:: < > =< >= =\= =:=)) 
      (t-reduce $x $xres) 
      (t-reduce $y $yres) 
      (number $xres) 
      (number $yres) 
      (det-if-then-else 
        (relop $test $xres $yres) 
        (= $res True) 
        (= $res False)) 
      (set-det)))

;
;                                          Equality:

  (= 
    (t-redex 
      (Cons  $y 
        (Cons  $x =)) $res) 
    ( (t-reduce $x $xres) 
      (t-reduce $y $yres) 
      (det-if-then-else 
        (= $xres $yres) 
        (= $res True) 
        (= $res False)) 
      (set-det)))

;
;                                          Arithmetic functions:

  (= 
    (t-redex 
      (Cons  $x $op) $res) 
    ( (end $op) 
      (my-member $op 
        (:: -)) 
      (t-reduce $x $xres) 
      (number $xres) 
      (eval1 $op $t $xres)))

;
;                                          Definitions:

;
;                                          Assumes a fact t_def(_func,_def) in the database for every

;
;                                          defined function.

  (= 
    (t-redex $in $out) 
    ( (my-append $par $func $in) 
      (end $func) 
      (t-def $func $args $expr) 
      (t $args $expr $def) 
      (my-append $par $def $out)))

;
;                                          Basic arithmetic and relational operators:



  (= 
    (eval + $C $A $B) 
    (is $C 
      (+ $A $B)))
  (= 
    (eval - $C $A $B) 
    (is $C 
      (- $A $B)))
  (= 
    (eval * $C $A $B) 
    (is $C 
      (* $A $B)))
  (= 
    (eval // $C $A $B) 
    (is $C 
      (// $A $B)))
  (= 
    (eval mod $C $A $B) 
    (is $C 
      (mod $A $B)))


  (= 
    (eval1 - $C $A) 
    (is $C 
      (- $A)))


  (= 
    (relop < $A $B) 
    (< $A $B))
  (= 
    (relop > $A $B) 
    (> $A $B))
  (= 
    (relop =< $A $B) 
    (=< $A $B))
  (= 
    (relop >= $A $B) 
    (>= $A $B))
  (= 
    (relop =\= $A $B) 
    (=\= $A $B))
  (= 
    (relop =:= $A $B) 
    (=:= $A $B))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          Scheme T:

;
;                                          A Translation Scheme for T-Combinators


;
;                                          Translate an expression to combinator form

;
;                                          by abstracting out all variables in _argvars:


  (= 
    (t $argvars $expr $trans) 
    ( (listify $expr $list) 
      (curry $list $curry) 
      (t-argvars $argvars $curry $trans) 
      (set-det)))


  (= 
    (t_argvars  () $trans $trans) True)
  (= 
    (t-argvars 
      (Cons  $x $argvars) $in $trans) 
    ( (t-argvars $argvars $in $mid) 
      (t-vars $mid $vars) 
      (t-trans $x $mid $vars $trans)))
;                                         ; calculate variables in each subexpression

 ;
;                                          main translation routine


;
;                                          Curry the original expression:

;
;                                          This converts an applicative expression of any number

;
;                                          of arguments and any depth of nesting into an expression

;
;                                          where all functions are curried, i.e. all function

;
;                                          applications are to one argument and have the form

;
;                                          [_arg|_func] where _func & _arg are also of that form.

;
;                                          Input is a nested function application in list form.

;
;                                          Currying makes t_trans faster.


  (= 
    (curry $a $a) 
    ( (or 
        (var $a) 
        (atomic $a)) (set-det)))
  (= 
    (curry 
      (Cons  $func $args) $cargs) 
    (currylist $args $cargs $func))

;
;                                          Transform [_a1, ..., _aN] to [_cN, ..., _c1|_link]-_link


  (= 
    (currylist Nil $link $link) 
    (set-det))
  (= 
    (currylist 
      (Cons  $a $args) $cargs $link) 
    ( (curry $a $c) (currylist $args $cargs (Cons  $c $link))))

;
;                                          Calculate variables in each subexpression:

;
;                                          To any expression a list of the form

;
;                                          [_vexpr, _astr, _fstr] is matched.

;
;                                          If the expression is a variable or an atom

;
;                                          then this list only has the first element.

;
;                                          _vexpr = List of all variables in the expression.

;
;                                          _astr, _fstr = Similar structures for argument & function.


  (= 
    (t-vars $v 
      (:: (:: $v))) 
    ( (var $v) (set-det)))
  (= 
    (t-vars $a 
      (:: Nil)) 
    ( (atomic $a) (set-det)))
  (= 
    (t-vars 
      (:: $func) 
      (:: Nil)) 
    ( (atomic $func) (set-det)))
  (= 
    (t-vars 
      (Cons  $arg $func) 
      (:: $g 
        (Cons  $g1 $af1) 
        (Cons  $g2 $af2))) 
    ( (t-vars $arg 
        (Cons  $g1 $af1)) 
      (t-vars $func 
        (Cons  $g2 $af2)) 
      (unionv $g1 $g2 $g)))

;
;                                          The main translation routine:

;
;                                          trans(_var, _curriedexpr, _varexpr, _result)

;
;                                          The translation scheme T in the article is followed literally.

;
;                                          A good example of MeTTa as a specification language.


  (= 
    (t-trans $x $a $_ 
      (Cons  $a k)) 
    ( (or 
        (atomic $a) 
        (, 
          (var $a) 
          (\== $a $x))) (set-det)))
  (= 
    (t-trans $x $y $_ i) 
    ( (== $x $y) (set-det)))
  (= 
    (t-trans $x $e 
      (Cons  $ve $_) 
      (Cons  $e k)) 
    (notinv $x $ve))
  (= 
    (t-trans $x 
      (Cons  $f $e) 
      (:: $vef $sf $se) $res) 
    ( (= $sf 
        (Cons  $vf $_)) 
      (= $se 
        (Cons  $ve $other)) 
      (or 
        (end $e) 
        (, 
          (= $other 
            (:: $_ 
              (Cons  $ve1 $_))) 
          (\== $ve1 Nil))) 
      (t-rule1 $x $e $ve $se $f $vf $sf $res)))
  (= 
    (t-trans $x 
      (Cons  $g 
        (Cons  $f $e)) 
      (:: $vefg $sg $sef) $res) 
    ( (= $sg 
        (Cons  $vg $_)) 
      (= $sef 
        (:: $vef $sf $se)) 
      (= $se 
        (Cons  $ve $_)) 
      (= $sf 
        (Cons  $vf $_)) 
      (t-rule2 $x $e $f $vf $sf $g $vg $sg $res)))

;
;                                          First complex rule of translation scheme T:


  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf $e) 
    ( (notinv $x $ve) 
      (== $x $f) 
      (set-det)))
  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf 
      (Cons  $resf 
        (Cons  $e b))) 
    ( (notinv $x $ve) 
      (inv $x $vf) 
      (\== $x $f) 
      (set-det) 
      (t-trans $x $f $sf $resf)))
  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf 
      (Cons  $f 
        (Cons  $rese c))) 
    ( (notinv $x $vf) 
      (set-det) 
      (t-trans $x $e $se $rese)))
;                                         /* inv(_x, _ve), */


  (= 
    (t-rule1 $x $e $ve $se $f $vf $sf 
      (Cons  $resf 
        (Cons  $rese s))) 
    ( (t-trans $x $e $se $rese) (t-trans $x $f $sf $resf)))
;                                         /* inv(_x, _ve), inv(_x, _vf), */



;
;                                          Second complex rule of translation scheme T:


  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $g 
        (Cons  $e c))) 
    ( (== $x $f) 
      (notinv $x $vg) 
      (set-det)))
  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $resg 
        (Cons  $e s))) 
    ( (== $x $f) 
      (set-det) 
      (t-trans $x $g $sg $resg)))
;                                         /* inv(_x, _vg), */


  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $g 
        (Cons  $resf 
          (Cons  $e cp)))) 
    ( (inv $x $vf) 
      (notinv $x $vg) 
      (set-det) 
      (t-trans $x $f $sf $resf)))
;                                         /* _x\==_f, */


  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $resg 
        (Cons  $resf 
          (Cons  $e sp)))) 
    ( (inv $x $vf) 
      (set-det) 
      (t-trans $x $f $sf $resf) 
      (t-trans $x $g $sg $resg)))
;                                         /* _x\==_f, */

;                                         /* inv(_x, _vg), */


  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $f $e)) 
    ( (== $x $g) (set-det)))
;                                         /* notinv(_x, _vf), */


  (= 
    (t-rule2 $x $e $f $vf $sf $g $vg $sg 
      (Cons  $resg 
        (Cons  $f 
          (Cons  $e bp)))) 
    (t-trans $x $g $sg $resg))
;                                         /* notinv(_x, _vf), inv(_x, _vg), _x\==_g, */



;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          List utilities:


;
;                                          Convert curried list into a regular list:


  (= 
    (make-list $a $a) 
    (atomic $a))
  (= 
    (make-list 
      (Cons  $b 
        (Cons  $a $LF)) 
      (Cons  $a $rb)) 
    ( (list-functor-name $LF) (make-list $b $rb)))


  (= 
    (listify $X $X) 
    ( (or 
        (var $X) 
        (atomic $X)) (set-det)))
  (= 
    (listify $Expr 
      (Cons  $Op $LArgs)) 
    ( (functor $Expr $Op $N) (listify-list 1 $N $Expr $LArgs)))


  (= 
    (listify-list $I $N $_ Nil) 
    ( (> $I $N) (set-det)))
  (= 
    (listify-list $I $N $Expr 
      (Cons  $LA $LArgs)) 
    ( (=< $I $N) 
      (set-det) 
      (arg $I $Expr $A) 
      (listify $A $LA) 
      (is $I1 
        (+ $I 1)) 
      (listify-list $I1 $N $Expr $LArgs)))


  (= 
    (my_member  $X 
      (Cons  $X $_)) True)
  (= 
    (my-member $X 
      (Cons  $_ $L)) 
    (my-member $X $L))


  (= 
    (my_append  () $L $L) True)
  (= 
    (my-append 
      (Cons  $X $L1) $L2 
      (Cons  $X $L3)) 
    (my-append $L1 $L2 $L3))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          Set utilities:

;
;                                          Implementation inspired by R. O'Keefe, Practical MeTTa.

;
;                                          Sets are represented as sorted lists without duplicates.

;
;                                          Predicates with 'v' suffix work with sets containing uninstantiated vars.


;
;                                          *** Intersection


  (= 
    (intersectv  () $_ ()) True)
  (= 
    (intersectv 
      (Cons  $A $S1) $S2 $S) 
    (intersectv-2 $S2 $A $S1 $S))


  (= 
    (intersectv_2  () $_ $_ ()) True)
  (= 
    (intersectv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (intersectv-3 $Order $A $S1 $B $S2 $S)))


  (= 
    (intersectv-3 < $_ $S1 $B $S2 $S) 
    (intersectv-2 $S1 $B $S2 $S))
  (= 
    (intersectv-3 = $A $S1 $_ $S2 
      (Cons  $A $S)) 
    (intersectv $S1 $S2 $S))
  (= 
    (intersectv-3 > $A $S1 $_ $S2 $S) 
    (intersectv-2 $S2 $A $S1 $S))


  (= 
    (intersectv_list  () ()) True)
  (= 
    (intersectv-list 
      (Cons  $InS $Sets) $OutS) 
    (phrase 
      (intersectv-list $Sets) $InS $OutS))


  (= 
    (-->  
      (intersectv_list  ()) ()) True)
  (= 
    (-->  
      (intersectv_list  
        (Cons  $S $Sets)) 
      (,  
        (call  
          (intersectv  $S)) 
        (intersectv_list  $Sets))) True)

;
;                                          *** Difference


  (= 
    (diffv  () $_ ()) True)
  (= 
    (diffv 
      (Cons  $A $S1) $S2 $S) 
    (diffv-2 $S2 $A $S1 $S))


  (= 
    (diffv_2  () $A $S1 
      (Cons  $A $S1)) True)
  (= 
    (diffv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (diffv-3 $Order $A $S1 $B $S2 $S)))


  (= 
    (diffv-3 < $A $S1 $B $S2 
      (Cons  $A $S)) 
    (diffv $S1 
      (Cons  $B $S2) $S))
  (= 
    (diffv-3 = $A $S1 $_ $S2 $S) 
    (diffv $S1 $S2 $S))
  (= 
    (diffv-3 > $A $S1 $_ $S2 $S) 
    (diffv-2 $S2 $A $S1 $S))

;
;                                          *** Union


  (= 
    (unionv  () $S2 $S2) True)
  (= 
    (unionv 
      (Cons  $A $S1) $S2 $S) 
    (unionv-2 $S2 $A $S1 $S))


  (= 
    (unionv_2  () $A $S1 
      (Cons  $A $S1)) True)
  (= 
    (unionv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (unionv-3 $Order $A $S1 $B $S2 $S)))


  (= 
    (unionv-3 < $A $S1 $B $S2 
      (Cons  $A $S)) 
    (unionv-2 $S1 $B $S2 $S))
  (= 
    (unionv-3 = $A $S1 $_ $S2 
      (Cons  $A $S)) 
    (unionv $S1 $S2 $S))
  (= 
    (unionv-3 > $A $S1 $B $S2 
      (Cons  $B $S)) 
    (unionv-2 $S2 $A $S1 $S))

;
;                                          *** Subset


  (= 
    (subsetv  () $_) True)
  (= 
    (subsetv 
      (Cons  $A $S1) 
      (Cons  $B $S2)) 
    ( (compare $Order $A $B) (subsetv-2 $Order $A $S1 $S2)))


  (= 
    (subsetv-2 = $_ $S1 $S2) 
    (subsetv $S1 $S2))
  (= 
    (subsetv-2 > $A $S1 $S2) 
    (subsetv 
      (Cons  $A $S1) $S2))

;
;                                          For unordered lists S1:


  (= 
    (small_subsetv  () $_) True)
  (= 
    (small-subsetv 
      (Cons  $A $S1) $S2) 
    ( (inv $A $S2) (small-subsetv $S1 $S2)))

;
;                                          *** Membership


  (= 
    (inv $A 
      (Cons  $B $S)) 
    ( (compare $Order $A $B) (inv-2 $Order $A $S)))


  (= 
    (inv_2  = $_ $_) True)
  (= 
    (inv-2 > $A $S) 
    (inv $A $S))

;
;                                          *** Non-membership


  (= 
    (notinv $A $S) 
    (notinv-2 $S $A))


  (= 
    (notinv_2  () $_) True)
  (= 
    (notinv-2 
      (Cons  $B $S) $A) 
    ( (compare $Order $A $B) (notinv-3 $Order $A $S)))


  (= 
    (notinv_3  < $_ $_) True)
  (= 
    (notinv-3 > $A $S) 
    (notinv-2 $S $A))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


