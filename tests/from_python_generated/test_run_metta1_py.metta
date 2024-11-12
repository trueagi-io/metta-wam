
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 1../../../hyperon-experimental/python/tests/test_run_metta.py :10
; <pre>
;        program = '''
; 1. ***************************************
           (isa red color)
           (isa green color)
           (isa blue color)
 ;           !(match &self (isa $color color) $color)
    ; ready works 
!(assertEqualToResult ( match &self (isa $color color) $color )  ( red green blue )  )

; 2.
            (= (f) (+ 2 3))
;            !(f)
    ; ready works 
!(assertEqualToResult ( f )  ( 5 )  )

;       '''
;        metta = MeTTa(env_builder=Environment.test_env())
;        self.
;<br>1 20241007 R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  ) 
;<br>
;(metta.run(program),
;            [metta.parse_all('red  green  blue'), metta.parse_all('5')])
 ;   def test_run_complex_query(self):
  ;      program = '''
 ; 3.  *********************************
      (A B)
      (C B)
;      !(match &self (, (A $x) (C $x)) $x)
; ready works 
!(assertEqualToResult ( match &self (, (A $x) (C $x)) $x )  ( B )  )

   ;     '''
;        result = MeTTa(env_builder=Environment.test_env()).run(program)
        
;<br>1 20241007 R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  ) 
;<br>
;('[[B]]', repr(result))
;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br> lastline 35 <br> 
; <pre>
;        program = '''
; 4. *********************************
            (= (Concat (Cons $head1 Nil) $list2)
               (Cons $head1 $list2))
            (= (Concat (Cons $head1 (Cons $t1 $t11)) $list2)
               (Cons $head1 (Concat (Cons $t1 $t11) $list2)))
            (= (lst1) (Cons a1 (Cons a2 Nil)))
            (= (lst2) (Cons b1 (Cons b2 Nil)))
;    !(Concat (lst1) (lst2))
; ready works !!
!(assertEqualToResult ( Concat (lst1) (lst2) )  ( (Cons a1   (Cons a2    (Cons b1    (Cons b2 Nil)))) )  )


;       '''
;        result = MeTTa(env_builder=Environment.test_env()).run(program)
;<br>2 20241007 R.v.Vessum converted python source to metta !(assertEqualToResult ( )  (  )  )
;<br>
;('[[
; (Cons a1 (Cons a2 (Cons b1 (Cons b2 Nil))))
 ; ]]', repr(result))
 ;   def test_comments(self):
;        program = '''
; 4)
; 5 **********************************
;                (a  5)
;                !(match &self (a $W) $W)
 ;           '''
;        result = MeTTa(env_builder=Environment.test_env()).run(program)
 ;       self.assertEqual('[[5]]', repr(result))
;        program = '''
;</pre>
; <br>  lastline 69 <br> 
