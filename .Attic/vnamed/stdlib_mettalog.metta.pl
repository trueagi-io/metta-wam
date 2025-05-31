%% Generated from /home/deb12user/metta-wam/prolog/vnamed/stdlib_mettalog.metta at 2025-05-30T01:56:17-07:00
:- style_check(-discontiguous).
:- style_check(-singleton).
:- include(library(metta_lang/metta_transpiled_header)).

%  ;; Type Declarations with Documentation
%  ; match hyperons weirdd return value of !(get-type :)
%  ;(: current-predicate-arity (-> Predicate Number)) ; returns only the first type of a symbol
%  ; returns only the first type of a symbol
%  ; returns only the first type of a symbol
%  ; returns only the first type of a symbol
%  ; returns only the first type of a symbol
%  ; returns only the data value types of a symbol
%  ; returns only the data value types of a symbol
%  ; returns only the function types of a symbol
%  ; returns only the function types of a symbol
%  ;(: = (-> Atom Atom %Undefined%))
%  ;(: case (-> Expression Atom Atom))
%  ;; MettaMorph-If Function
transpiler_clause_store('MettaMorph-If',[2],2,['Bool','Atom'],'Atom',[x(doeval,eager,[boolean]),x(noeval,lazy,[])],x(noeval,lazy,[]),['MettaMorph-If','True',_then],_then).
optimized_code(fa('MettaMorph-If',2),ca).
/*


'mi__1_2_MettaMorph-If'(B,C,D) :- 
  E='mc__1_2_MettaMorph-If'(B,C,D) , 
  ci(true,'MettaMorph-If',2,['MettaMorph-If',B,C],D,true,E).


 */
optimized_code(fa('MettaMorph-If',2),ca).
/*


'me__1_2_MettaMorph-If'(B,C,D):-'mc__1_2_MettaMorph-If'(B,C,D).


 */
transpiler_clause_store('MettaMorph-If',[2],3,['Bool','Atom'],'Atom',[x(doeval,eager,[boolean]),x(noeval,lazy,[])],x(noeval,lazy,[]),['MettaMorph-If','False',_then],[let,_n,0,[let,_n,1,_n]]).
optimized_code(fa('MettaMorph-If',2),ca).
/*


'mi__1_2_MettaMorph-If'(B,C,D) :- 
  E='mc__1_2_MettaMorph-If'(B,C,D) , 
  ci(true,'MettaMorph-If',2,['MettaMorph-If',B,C],D,true,E).


 */
optimized_code(fa('MettaMorph-If',2),ca).
/*


'me__1_2_MettaMorph-If'(B,C,D):-'mc__1_2_MettaMorph-If'(B,C,D).


 */
%  ; Placeholder for False condition
transpiler_clause_store('MettaMorph-If',[3],1,['Bool','Atom','Atom'],'Atom',[x(doeval,eager,[boolean]),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['MettaMorph-If',_cond,_then,_else],[if,_cond,_then,_else]).
optimized_code(fa('MettaMorph-If',3),ca).
/*


'mi__1_3_MettaMorph-If'(B,C,D,E) :- 
  F='mc__1_3_MettaMorph-If'(B,C,D,E) , 
  ci( true, 
    'MettaMorph-If', 
    3, 
    ['MettaMorph-If',B,C,D], E,true,F).


 */
optimized_code(fa('MettaMorph-If',3),ca).
/*


'me__1_3_MettaMorph-If'(B,C,D,E) :-  
  'mc__1_3_MettaMorph-If'(B,C,D,E).


 */
%  ;; Arity Assignments
%  ;(predicate-arity : 3)
%  ; (= (: $F P1) (predicate-arity $F 1))
%  ;; Source Predicate and Function Types
%  ;; MeTTaResult Type and Values
%  ;; Subtype Relations
%  ;; Space Subtype Relation
%  ; !(import! &corelib "src/canary/stdlib_mettalog.metta")
%  ;!(println! "!(import! &corelib \"src/canary/stdlib_mettalog.metta\")")
%  ;; Functional Programming
%  ;; Functional Programming
%  ;(: = (-> Atom Atom Atom))
%  ; probably should be (: return (-> $t $t)) 
transpiler_clause_store('if-unify-or-empty',[2],2,['Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-unify-or-empty',_a,_a],unified).
optimized_code(fa('if-unify-or-empty',2),ca).
/*


'mi__1_2_if-unify-or-empty'(B,C,D) :- 
  E='mc__1_2_if-unify-or-empty'(B,C,D) , 
  ci( true, 
    'if-unify-or-empty', 
    2, 
    ['if-unify-or-empty',B,C], D,true,E).


 */
optimized_code(fa('if-unify-or-empty',2),ca).
/*


'me__1_2_if-unify-or-empty'(B,C,D) :-  
  'mc__1_2_if-unify-or-empty'(B,C,D).


 */
transpiler_clause_store('if-unify-or-empty',[2],3,['Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-unify-or-empty',_a,_b],[empty]).
optimized_code(fa('if-unify-or-empty',2),ca).
/*


'mi__1_2_if-unify-or-empty'(B,C,D) :- 
  E='mc__1_2_if-unify-or-empty'(B,C,D) , 
  ci( true, 
    'if-unify-or-empty', 
    2, 
    ['if-unify-or-empty',B,C], D,true,E).


 */
optimized_code(fa('if-unify-or-empty',2),ca).
/*


'me__1_2_if-unify-or-empty'(B,C,D) :-  
  'mc__1_2_if-unify-or-empty'(B,C,D).


 */
%  ; AKA? (: cons (-> Atom Atom Atom))
%  ; AKA? (: decons (-> Atom Atom))
transpiler_clause_store('min-atom',[1],1,['Expression'],'Number',[x(noeval,eager,[])],x(doeval,eager,[number]),['min-atom',_L],['call-fn!',min_list,_L]).
optimized_code(fa('min-atom',1),ca).
/*


'mi__1_1_min-atom'(B,C) :- 
  D='mc__1_1_min-atom'(B,C) , 
  ci(true,'min-atom',1,['min-atom',B],C,true,D).


 */
optimized_code(fa('min-atom',1),ca).
/*


'me__1_1_min-atom'(B,C):-'mc__1_1_min-atom'(B,C).


 */
%  ;; ==> (= (index-atom $L $N) (call-fn! nth0 $N $L))
%  ; Define a variable using a function call to compute the power of a base number to an exponent
%  ; The result of the computation is stored in the variable `pow-math`
%  ; Assign the result of a function call to `pow-math`
transpiler_clause_store('pow-math',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['pow-math',_B,_P],['call-fn!',pow,_B,_P]).
optimized_code(fa('pow-math',2),ca).
/*


'mi__1_2_pow-math'(B,C,D) :- 
  E='mc__1_2_pow-math'(B,C,D) , 
  ci(true,'pow-math',2,['pow-math',B,C],D,true,E).


 */
optimized_code(fa('pow-math',2),ca).
/*


'me__1_2_pow-math'(B,C,D):-'mc__1_2_pow-math'(B,C,D).


 */
%  ; this is deduced: (: isinf (-> Number Bool))
%  ;; Some code still uses the old syntax
transpiler_clause_store('file-read',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['file-read',_filepath],['call-fn',file_read_content,_filepath]).
optimized_code(fa('file-read',1),ca).
/*


'mi__1_1_file-read'(B,C) :- 
  D='mc__1_1_file-read'(B,C) , 
  ci(true,'file-read',1,['file-read',B],C,true,D).


 */
optimized_code(fa('file-read',1),ca).
/*


'me__1_1_file-read'(B,C):-'mc__1_1_file-read'(B,C).


 */
transpiler_clause_store('file-write',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['file-write',_filepath,_content],['call-fn',file_write_content,_filepath,_content]).
optimized_code(fa('file-write',2),ca).
/*


'mi__1_2_file-write'(B,C,D) :- 
  E='mc__1_2_file-write'(B,C,D) , 
  ci(true,'file-write',2,['file-write',B,C],D,true,E).


 */
optimized_code(fa('file-write',2),ca).
/*


'me__1_2_file-write'(B,C,D):-'mc__1_2_file-write'(B,C,D).


 */
%  ;; collapse-bind because `collapse` doesnt guarantee shared bindings
%  ;; superpose-bind because `superpose` doesnt guarentee shared bindings
%  ; Helper Minimal Metta?
transpiler_clause_store(id,[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),[id,_x],_x).
optimized_code(fa(id,1),ca).
/*


mi__1_1_id(B,C) :- 
  D=mc__1_1_id(B,C) , 
  ci(true,id,1,[id,B],C,true,D).


 */
optimized_code(fa(id,1),ca).
/*


me__1_1_id(B,C):-mc__1_1_id(B,C).


 */
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
transpiler_clause_store('atom-subst',[3],1,['Atom','Variable','Atom'],'Atom',[x(noeval,lazy,[]),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,lazy,[]),['atom-subst',_atom,_var,_templ],[function,[let,_var,[id,_atom],[return,_templ]]]).
optimized_code(fa('atom-subst',3),ca).
/*


'mi__1_3_atom-subst'(B,C,D,E) :- 
  F='mc__1_3_atom-subst'(B,C,D,E) , 
  ci( true, 
    'atom-subst', 
    3, 
    ['atom-subst',B,C,D], E,true,F).


 */
optimized_code(fa('atom-subst',3),ca).
/*


'me__1_3_atom-subst'(B,C,D,E) :-  
  'mc__1_3_atom-subst'(B,C,D,E).


 */
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
transpiler_clause_store('if-decons-expr',[5],1,['Expression','Variable','Variable','Atom','Atom'],'Atom',[x(noeval,eager,[]),x(doeval,eager,['Variable']),x(doeval,eager,['Variable']),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-decons-expr',_atom,_head,_tail,_then,_else],[if,['decons-ht',_atom,_head,_tail],_then,_else]).
optimized_code(fa('if-decons-expr',5),ca).
/*


'mi__1_5_if-decons-expr'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_if-decons-expr'(B,C,D,E,F,G) , 
  ci( true, 
    'if-decons-expr', 
    5, 
    [ 'if-decons-expr', B,C,D,E, 
      F], G,true,H).


 */
optimized_code(fa('if-decons-expr',5),ca).
/*


'me__1_5_if-decons-expr'(B,C,D,E,F,G) :-  
  'mc__1_5_if-decons-expr'(B,C,D,E,F,G).


 */


'mc__1_5_if-decons-expr'(A,B,C,D,E,F) :-  
  F =  
    ispeEnN( G, 
        ((
         (  ((
             (  ((
                 ('mi__1_3_decons-ht'(A,B,C,H),is_True(H))*->
                 (I=D)  )));
             (I=E)  ))),
         (as_p1_exec(I,G))  )), 
      J, 
        ((
         (  ((
             (  ((
                 ('mi__1_3_decons-ht'(A,B,C,H),is_True(H))*->
                 (K=D)  )));
             (K=E)  ))),
         (as_p1_expr(K,J))  ))).


%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
transpiler_clause_store('if-error',[3],1,['Atom','Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-error',_atom,_then,_else],[function,[let,_meta,['get-metatype',_atom],[eval,[if,['metta-equal',_meta,'Expression'],[eval,[if,['metta-equal',_atom,[]],[return,_else],[let,_list,['decons-atom',_atom],[if,['metta-unify',_list,[_head,_tail]],[eval,[if,['metta-equal',_head,'Error'],[return,_then],[return,_else]]],[return,_else]]]]],[return,_else]]]]]).
optimized_code(fa('if-error',3),ca).
/*


'mi__1_3_if-error'(B,C,D,E) :- 
  F='mc__1_3_if-error'(B,C,D,E) , 
  ci( true, 
    'if-error', 
    3, 
    ['if-error',B,C,D], E,true,F).


 */
optimized_code(fa('if-error',3),ca).
/*


'me__1_3_if-error'(B,C,D,E) :-  
  'mc__1_3_if-error'(B,C,D,E).


 */
transpiler_clause_store('return-on-error',[2],1,['Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['return-on-error',_atom,_then],[function,[eval,[if,['metta-equal',_atom,'Empty'],[return,[return,'Empty']],[eval,['if-error',_atom,[return,[return,_atom]],[return,_then]]]]]]).
optimized_code(fa('return-on-error',2),ca).
/*


'mi__1_2_return-on-error'(B,C,D) :- 
  E='mc__1_2_return-on-error'(B,C,D) , 
  ci(true,'return-on-error',2,['return-on-error',B,C],D,true,E).


 */
optimized_code(fa('return-on-error',2),ca).
/*


'me__1_2_return-on-error'(B,C,D):-'mc__1_2_return-on-error'(B,C,D).


 */
%  ; Difference between `switch` and `case` is a way how they interpret `Empty`
%  ; result. `CaseOp` interprets first argument inside itself and then manually
%  ; checks whether result is empty. `switch` is interpreted in a context of
%  ; main interpreter. Minimal interpreter correctly passes `Empty` as an
%  ; argument to the `switch` but when `switch` is called from MeTTa interpreter
%  ; (for example user evaluates `!(switch (if-unify A B ok Empty) ...)` then
%  ; emptiness of the first argument is checked by interpreter and it will
%  ; break execution when `Empty` is returned.
transpiler_clause_store(switch,[2],1,['%Undefined%','Expression'],'Atom',[x(doeval,eager,['%Undefined%']),x(noeval,eager,[])],x(noeval,lazy,[]),[switch,_atom,[]],[case,[eval,_atom],[]]).
optimized_code(fa(switch,2),ca).
/*


mi__1_2_switch(B,C,D) :- 
  E=mc__1_2_switch(B,C,D) , 
  ci(true,switch,2,[switch,B,C],D,true,E).


 */
optimized_code(fa(switch,2),ca).
/*


me__1_2_switch(B,C,D):-mc__1_2_switch(B,C,D).


 */
%  ; BEGIN - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
%  ; ENDOF - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
%  ; TODO: Type is used here, but there is no definition for the -> type
%  ; constructor for instance, thus in practice it matches because -> has
%  ; %Undefined% type. We need to assign proper type to -> and other type
%  ; constructors but it is not possible until we support vararg types.
%  ;; This impl is old and maybe not sufficiant?
transpiler_clause_store('is-function',[1],1,['Type'],'Bool',[x(doeval,eager,['Type'])],x(doeval,eager,[boolean]),['is-function',_type],[function,[let,_meta,['get-metatype',_type],[eval,[switch,[_type,_meta],[[[_type,'Expression'],[eval,['if-decons-expr',_type,_head,_tail,['if-unify',_head,->,[return,'True'],[return,'False']],[return,['Error',['is-function',_type],"is-function non-empty expression as an argument"]]]]],[[_type,_meta],[return,'False']]]]]]]).
optimized_code(fa('is-function',1),ca).
/*


'mi__1_1_is-function'(B,C) :- 
  D='mc__1_1_is-function'(B,C) , 
  ci(true,'is-function',1,['is-function',B],C,true,D).


 */
optimized_code(fa('is-function',1),ca).
/*


'me__1_1_is-function'(B,C):-'mc__1_1_is-function'(B,C).


 */
optimized_code(fa('is-function',1),ca).
/*


'mc__1_1_is-function'(A,B) :-  
  catch( 
     ( 'mi__1_1_get-metatype'(A,C)  ,
       D=C , 
       transpiler_apply( mc__1_1_, 
         A, 
         [A,D], 
         E, 
         [D], 
         [D], 
         [x(noeval,eager,[])], 
         [true], 
         [true]) , 
       F =  
         ispuU( G, 
           ( throw(metta_return('True'))  ,
             throw(metta_return('False')) , 
             G =  
               [ 'if-unify', H,->,'True','False'])) , 
       I =  
         ispuU( 
            [ 'Error', 
              ['is-function',A], 
              "is-function non-empty expression as an argument"], 
            throw( metta_return( [ 'Error', 
                                   ['is-function',A], 
                                   "is-function non-empty expression as an argument"]))) , 
       J =  
         [ 'if-decons-expr', A,H,_,F, 
           I] , 
       as_p1_expr(J,K) , 
       mi__1_1_eval(K,L) , 
       throw(metta_return('False')) , 
       transpiler_apply( mc__2_1_1_, 
         A, 
         [ [A,D], 
           'False'], 
         M, 
         [D,'False'], 
         [D,'False'], 
         [ x(noeval,eager,[]), 
           x(noeval,eager,[])], 
         [ true, 
           throw(metta_return('False'))], 
         [ true, 
           throw(metta_return('False'))]) , 
       N =  
         [ [ [A,'Expression'], 
             L], 
           M] , 
       O=[switch,E,N] , 
       as_p1_expr(O,P) , 
       mi__1_1_eval(P,B)), metta_return(Q),Q=B).


 */
%  ;; This implementation is old and may not be sufficient.
transpiler_clause_store('match-types',[4],1,['Atom','Atom','Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[]),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['match-types',_type1,_type2,_then,_else],[function,[eval,[if,['metta-equal',_type1,'%Undefined%'],[return,_then],[eval,[if,['metta-equal',_type2,'%Undefined%'],[return,_then],[eval,[if,['metta-equal',_type1,'Atom'],[return,_then],[eval,[if,['metta-equal',_type2,'Atom'],[return,_then],[if,['metta-unify',_type1,_type2],[return,_then],[return,_else]]]]]]]]]]]).
optimized_code(fa('match-types',4),ca).
/*


'mi__1_4_match-types'(B,C,D,E,F) :- 
  G =  
    'mc__1_4_match-types'(B,C,D,E,F) , 
  ci( true, 
    'match-types', 
    4, 
    [ 'match-types', B,C,D,E], F,true,G).


 */
optimized_code(fa('match-types',4),ca).
/*


'me__1_4_match-types'(B,C,D,E,F) :-  
  'mc__1_4_match-types'(B,C,D,E,F).


 */
%  ; Helper MinimalMeTTa
transpiler_clause_store('first-from-pair',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['first-from-pair',_pair],[function,[if,['metta-unify',_pair,[_first,_second]],[return,_first],[return,['Error',['first-from-pair',_pair],"incorrect pair format"]]]]).
optimized_code(fa('first-from-pair',1),ca).
/*


'mi__1_1_first-from-pair'(B,C) :- 
  D='mc__1_1_first-from-pair'(B,C) , 
  ci(true,'first-from-pair',1,['first-from-pair',B],C,true,D).


 */
optimized_code(fa('first-from-pair',1),ca).
/*


'me__1_1_first-from-pair'(B,C):-'mc__1_1_first-from-pair'(B,C).


 */
optimized_code(fa('first-from-pair',1),ca).
/*


'mc__1_1_first-from-pair'(A,B) :-  
  catch( 
       ((
        (  ((
            (( C=[D,_]  ,
               'mi__1_2_metta-unify'(A,C,E) , 
               is_True(E)))*->
            (throw(metta_return(D)),B=D)  )));
        (  ((
            (throw( metta_return(['Error',['first-from-pair',A],"incorrect pair format"]))),
            (B =  
               [ 'Error', 
                 ['first-from-pair',A], 
                 "incorrect pair format"])  )))  )), metta_return(F),F=B).


 */
transpiler_clause_store('match-type-or',[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['match-type-or',_folded,_next,_type],[function,[let,_matched,['match-types',_next,_type,'True','False'],[let,_or,[or,_folded,_matched],[return,_or]]]]).
optimized_code(fa('match-type-or',3),ca).
/*


'mi__1_3_match-type-or'(B,C,D,E) :- 
  F='mc__1_3_match-type-or'(B,C,D,E) , 
  ci( true, 
    'match-type-or', 
    3, 
    ['match-type-or',B,C,D], E,true,F).


 */
optimized_code(fa('match-type-or',3),ca).
/*


'me__1_3_match-type-or'(B,C,D,E) :-  
  'mc__1_3_match-type-or'(B,C,D,E).


 */
optimized_code(fa('match-type-or',3),ca).
/*


'mc__1_3_match-type-or'(A,B,C,D) :-  
  catch( 
     ( E=ispu(B)  ,
       F=ispu(C) , 
       G=ispu('True') , 
       H=ispu('False') , 
       'mi__1_4_match-types'(E,F,G,H,I) , 
       as_p1_exec(I,J) , 
       K=J , 
       is_True(A)*->L='True';L=K , 
       D=L , 
       throw(metta_return(D))), metta_return(M),M=D).


 */
transpiler_clause_store('filter-atom',[3],1,['Expression','Variable','Atom'],'Expression',[x(noeval,eager,[]),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,eager,[]),['filter-atom',_list,_var,_filter],[function,[eval,[if,['decons-ht',_list,_head,_tail],[let,_tail_filtered,['filter-atom',_tail,_var,_filter],[let,_filter_expr,['atom-subst',_head,_var,_filter],[let,_is_filtered,_filter_expr,[eval,[if,_is_filtered,[let,_res,['cons-atom',_head,_tail_filtered],[return,_res]],[return,_tail_filtered]]]]]],[return,[]]]]]).
optimized_code(fa('filter-atom',3),ca).
/*


'mi__1_3_filter-atom'(B,C,D,E) :- 
  F='mc__1_3_filter-atom'(B,C,D,E) , 
  ci( true, 
    'filter-atom', 
    3, 
    ['filter-atom',B,C,D], E,true,F).


 */
optimized_code(fa('filter-atom',3),ca).
/*


'me__1_3_filter-atom'(B,C,D,E) :-  
  'mc__1_3_filter-atom'(B,C,D,E).


 */
optimized_code(fa('filter-atom',3),ca).
/*


'mc__1_3_filter-atom'(A,B,C,D) :-  
  catch(   ((
            (  ((
                (  ((
                    ('mi__1_3_decons-ht'(A,E,F,G),is_True(G))*->
                    (( 'mi__1_3_filter-atom'(F,B,C,H)  ,
                       I=H , 
                       J=ispu(E) , 
                       'mi__1_3_atom-subst'(J,B,C,K) , 
                       as_p1_exec(K,L) , 
                       M=L , 
                       N=M , 
                         ((
                          (  ((
                              (is_True(N))*->
                              (( 'mi__1_2_cons-atom'(E,I,O)  ,
                                 P=O , 
                                 throw(metta_return(P)) , 
                                 Q=P))  )));
                          (throw(metta_return(I)),Q=I)  )) , 
                       R=[eval,Q] , 
                       S=R))  )));
                (throw(metta_return([])),S=[])  ))),
            (mi__1_1_eval(S,D))  )), 
    metta_return(T), 
    T=D).


 */
transpiler_clause_store('map-atom',[3],1,['Expression','Variable','Atom'],'Expression',[x(noeval,eager,[]),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,eager,[]),['map-atom',_list,_var,_map],[function,[eval,[if,['decons-ht',_list,_head,_tail],[let,_tail_mapped,['map-atom',_tail,_var,_map],[let,_map_expr,['atom-subst',_head,_var,_map],[let,_head_mapped,_map_expr,[let,_res,['cons-atom',_head_mapped,_tail_mapped],[return,_res]]]]],[return,[]]]]]).
optimized_code(fa('map-atom',3),ca).
/*


'mi__1_3_map-atom'(B,C,D,E) :- 
  F='mc__1_3_map-atom'(B,C,D,E) , 
  ci( true, 
    'map-atom', 
    3, 
    ['map-atom',B,C,D], E,true,F).


 */
optimized_code(fa('map-atom',3),ca).
/*


'me__1_3_map-atom'(B,C,D,E) :-  
  'mc__1_3_map-atom'(B,C,D,E).


 */
optimized_code(fa('map-atom',3),ca).
/*


'mc__1_3_map-atom'(A,B,C,D) :-  
  catch(   ((
            (  ((
                (  ((
                    ('mi__1_3_decons-ht'(A,E,F,G),is_True(G))*->
                    (( 'mi__1_3_map-atom'(F,B,C,H)  ,
                       I=H , 
                       J=ispu(E) , 
                       'mi__1_3_atom-subst'(J,B,C,K) , 
                       as_p1_exec(K,L) , 
                       M=L , 
                       N=M , 
                       'mi__1_2_cons-atom'(N,I,O) , 
                       P=O , 
                       throw(metta_return(P)) , 
                       Q=P))  )));
                (throw(metta_return([])),Q=[])  ))),
            (mi__1_1_eval(Q,D))  )), 
    metta_return(R), 
    R=D).


 */
transpiler_clause_store('foldl-atom',[5],1,['Expression','Atom','Variable','Variable','Atom'],'Atom',[x(noeval,eager,[]),x(noeval,lazy,[]),x(doeval,eager,['Variable']),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,lazy,[]),['foldl-atom',_list,_init,_a,_b,_op],[function,[eval,[if,['decons-ht',_list,_head,_tail],[let,_op_init,['atom-subst',_init,_a,_op],[let,_op_head,['atom-subst',_head,_b,_op_init],[let,_head_folded,_op_head,[let,_res,['foldl-atom',_tail,_head_folded,_a,_b,_op],[return,_res]]]]],[return,_init]]]]).
optimized_code(fa('foldl-atom',5),ca).
/*


'mi__1_5_foldl-atom'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_foldl-atom'(B,C,D,E,F,G) , 
  ci( true, 
    'foldl-atom', 
    5, 
    [ 'foldl-atom', B,C,D,E, 
      F], G,true,H).


 */
optimized_code(fa('foldl-atom',5),ca).
/*


'me__1_5_foldl-atom'(B,C,D,E,F,G) :-  
  'mc__1_5_foldl-atom'(B,C,D,E,F,G).


 */
transpiler_clause_store('separate-errors',[2],1,['Expression','Expression'],'Expression',[x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['separate-errors',_succ_err,_res],[function,[if,['metta-unify',_succ_err,[_suc,_err]],[if,['metta-unify',_res,[_a,_b]],[eval,['if-error',_a,[let,_err_39,['cons-atom',_res,_err],[return,[_suc,_err_39]]],[let,_suc_39,['cons-atom',_res,_suc],[return,[_suc_39,_err]]]]],[return,_succ_err]],[return,_succ_err]]]).
optimized_code(fa('separate-errors',2),ca).
/*


'mi__1_2_separate-errors'(B,C,D) :- 
  E='mc__1_2_separate-errors'(B,C,D) , 
  ci(true,'separate-errors',2,['separate-errors',B,C],D,true,E).


 */
optimized_code(fa('separate-errors',2),ca).
/*


'me__1_2_separate-errors'(B,C,D):-'mc__1_2_separate-errors'(B,C,D).


 */
optimized_code(fa('separate-errors',2),ca).
/*


'mc__1_2_separate-errors'(A,B,C) :-  
  catch( 
       ((
        (  ((
            (( D=[E,F]  ,
               'mi__1_2_metta-unify'(A,D,G) , 
               is_True(G)))*->
            (  ((
                (  ((
                    (  ((
                        (( H=[I,_]  ,
                           'mi__1_2_metta-unify'(B,H,J) , 
                           is_True(J)))*->
                        (( K=ispu(I)  ,
                           L =  
                             ispeEnNC( M, 
                                 ((
                                  (transpiler_apply( mc__1_1_, 
                                     E, 
                                     [E,N], 
                                     M, 
                                     [N], 
                                     [N], 
                                     [x(noeval,eager,[])], 
                                     [true], 
                                     [true])),
                                  (throw(metta_return(M)))  )), 
                               O, 
                               O=[E,N],throw(metta_return(O)), 
                               'mi__1_2_cons-atom'(B,F,P),N=P) , 
                           Q =  
                             ispeEnNC( R, 
                                 ((
                                  (transpiler_apply( mc__1_1_, 
                                     S, 
                                     [S,F], 
                                     R, 
                                     [F], 
                                     [F], 
                                     [x(noeval,eager,[])], 
                                     [true], 
                                     [true])),
                                  (throw(metta_return(R)))  )), 
                               T, 
                               T=[S,F],throw(metta_return(T)), 
                               'mi__1_2_cons-atom'(B,E,U),S=U) , 
                           V=['if-error',K,L,Q] , 
                           as_p1_expr(V,W) , 
                           mi__1_1_eval(W,X) , 
                           Y=X))  )));
                    (throw(metta_return(A)),Y=A)  ))),
                (C=Y)  )))  )));
        (throw(metta_return(A)),C=A)  )), metta_return(Z),Z=C).


 */
transpiler_clause_store('check-alternatives',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['check-alternatives',_atom],[function,[let,_collapsed,['collapse-bind',_atom],[let,_separated,['foldl-atom',_collapsed,[[],[]],_succ_err,_res,[eval,['separate-errors',_succ_err,_res]]],[if,['metta-unify',_separated,[_success,_error]],[let,_filtered,[if,['metta-equal',_success,[]],_error,_success],[let,_ret,['superpose-bind',_filtered],[return,_ret]]],[return,['Error',['check-alternatives',_atom],"list of results was not filtered correctly"]]]]]]).
optimized_code(fa('check-alternatives',1),ca).
/*


'mi__1_1_check-alternatives'(B,C) :- 
  D='mc__1_1_check-alternatives'(B,C) , 
  ci(true,'check-alternatives',1,['check-alternatives',B],C,true,D).


 */
optimized_code(fa('check-alternatives',1),ca).
/*


'me__1_1_check-alternatives'(B,C):-'mc__1_1_check-alternatives'(B,C).


 */
optimized_code(fa('check-alternatives',1),ca).
/*


'mc__1_1_check-alternatives'(A,B) :-  
  catch( 
     ( C=['collapse-bind',A]  ,
       D=C , 
       E =  
         ispuU(F,F=[[],[]]) , 
       G =  
         ispeEnNC( H, 
           mi__1_1_eval(I,H), J,J=[eval,I], 
           I=['separate-errors',K,L]) , 
       'mi__1_5_foldl-atom'(D,E,K,L,G,M) , 
       as_p1_exec(M,N) , 
       O=N , 
         ((
          (  ((
              (( P=[Q,R]  ,
                 'mi__1_2_metta-unify'(O,P,S) , 
                 is_True(S)))*->
              ((   ((
                    (  ((
                        (T=['metta-equal',Q,[]],is_True(T))*->
                        (U=R)  )));
                    (U=Q)  ))  ,
                 V=U , 
                 W=['superpose-bind',V] , 
                 X=W , 
                 throw(metta_return(X)) , 
                 B=X))  )));
          (  ((
              (throw( metta_return( [ 'Error', 
                                      ['check-alternatives',A], 
                                      "list of results was not filtered correctly"]))),
              (B =  
                 [ 'Error', 
                   ['check-alternatives',A], 
                   "list of results was not filtered correctly"])  )))  ))), metta_return(Y),Y=B).


 */
transpiler_clause_store(interpret,[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),[interpret,_atom,_type,_space],[function,[let,_meta,['get-metatype',_atom],[eval,[if,['metta-equal',_type,'Atom'],[return,_atom],[eval,[if,['metta-equal',_type,_meta],[return,_atom],[eval,[switch,[_type,_meta],[[[_type,'Variable'],[return,_atom]],[[_type,'Symbol'],[let,_ret,['type-cast',_atom,_type,_space],[return,_ret]]],[[_type,'Grounded'],[let,_ret,['type-cast',_atom,_type,_space],[return,_ret]]],[[_type,'Expression'],[let,_ret,['check-alternatives',[eval,['interpret-expression',_atom,_type,_space]]],[return,_ret]]]]]]]]]]]]).
optimized_code(fa(interpret,3),ca).
/*


mi__1_3_interpret(B,C,D,E) :- 
  F=mc__1_3_interpret(B,C,D,E) , 
  ci(true,interpret,3,[interpret,B,C,D],E,true,F).


 */
optimized_code(fa(interpret,3),ca).
/*


me__1_3_interpret(B,C,D,E) :-  
  mc__1_3_interpret(B,C,D,E).


 */
optimized_code(fa(interpret,3),ca).
/*


mc__1_3_interpret(A,B,C,D) :-  
  catch( 
     ( 'mi__1_1_get-metatype'(A,E)  ,
       F=E , 
         ((
          (  ((
              (G=['metta-equal',B,'Atom'],is_True(G))*->
              (throw(metta_return(A)),H=A)  )));
          ((   ((
                (  ((
                    (I=['metta-equal',B,F],is_True(I))*->
                    (throw(metta_return(A)),J=A)  )));
                (( transpiler_apply( mc__1_1_, 
                     B, 
                     [B,F], 
                     K, 
                     [F], 
                     [F], 
                     [x(noeval,eager,[])], 
                     [true], 
                     [true])  ,
                   throw(metta_return(A)) , 
                   L=['type-cast',A,B,C] , 
                   M=L , 
                   throw(metta_return(M)) , 
                   transpiler_apply( mc__2_1_1_, 
                     B, 
                     [ [B,'Symbol'], 
                       M], 
                     N, 
                     ['Symbol',M], 
                     ['Symbol',M], 
                     [ x(noeval,eager,[]), 
                       x(noeval,eager,[])], 
                     [ true, 
                       ( L=['type-cast',A,B,C]  ,
                         M=L , 
                         throw(metta_return(M)))], 
                     [ true, 
                       ( L=['type-cast',A,B,C]  ,
                         M=L , 
                         throw(metta_return(M)))]) , 
                   O=['type-cast',A,B,C] , 
                   M=O , 
                   throw(metta_return(M)) , 
                   transpiler_apply( mc__2_1_1_, 
                     B, 
                     [ [B,'Grounded'], 
                       M], 
                     P, 
                     ['Grounded',M], 
                     ['Grounded',M], 
                     [ x(noeval,eager,[]), 
                       x(noeval,eager,[])], 
                     [ true, 
                       ( O=['type-cast',A,B,C]  ,
                         M=O , 
                         throw(metta_return(M)))], 
                     [ true, 
                       ( O=['type-cast',A,B,C]  ,
                         M=O , 
                         throw(metta_return(M)))]) , 
                   Q=['interpret-expression',A,B,C] , 
                   mi__1_1_eval(Q,R) , 
                   'mi__1_1_check-alternatives'(R,S) , 
                   M=S , 
                   throw(metta_return(M)) , 
                   transpiler_apply( mc__2_1_1_, 
                     B, 
                     [ [B,'Expression'], 
                       M], 
                     T, 
                     ['Expression',M], 
                     ['Expression',M], 
                     [ x(noeval,eager,[]), 
                       x(noeval,eager,[])], 
                     [ true, 
                       ( Q=['interpret-expression',A,B,C]  ,
                         mi__1_1_eval(Q,R) , 
                         'mi__1_1_check-alternatives'(R,S) , 
                         M=S , 
                         throw(metta_return(M)))], 
                     [ true, 
                       ( Q=['interpret-expression',A,B,C]  ,
                         mi__1_1_eval(Q,R) , 
                         'mi__1_1_check-alternatives'(R,S) , 
                         M=S , 
                         throw(metta_return(M)))]) , 
                   U =  
                     [ [ [B,'Variable'], 
                         A], N,P,T] , 
                   V=[switch,K,U] , 
                   as_p1_expr(V,W) , 
                   X=[eval,W] , 
                   J=X))  ))  ,
             Y=[eval,J] , 
             H=Y))  )) , 
       mi__1_1_eval(H,D)), metta_return(Z),Z=D).


 */
transpiler_clause_store('interpret-expression',[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-expression',_atom,_type,_space],[function,[eval,[if,['decons-ht',_atom,_op,_args],[let,_op_type,['get-type',_op,_space],[let,_is_func,['is-function',_op_type],[if,['metta-unify',_is_func,'True'],[let,_reduced_atom,['interpret-func',_atom,_op_type,_type,_space],[let,_ret,['metta-call',_reduced_atom,_type,_space],[return,_ret]]],[let,_reduced_atom,['interpret-tuple',_atom,_space],[let,_ret,['metta-call',_reduced_atom,_type,_space],[return,_ret]]]]]],[let,_ret,['type-cast',_atom,_type,_space],[return,_ret]]]]]).
optimized_code(fa('interpret-expression',3),ca).
/*


'mi__1_3_interpret-expression'(B,C,D,E) :- 
  F='mc__1_3_interpret-expression'(B,C,D,E) , 
  ci( true, 
    'interpret-expression', 
    3, 
    ['interpret-expression',B,C,D], E,true,F).


 */
optimized_code(fa('interpret-expression',3),ca).
/*


'me__1_3_interpret-expression'(B,C,D,E) :-  
  'mc__1_3_interpret-expression'(B,C,D,E).


 */
optimized_code(fa('interpret-expression',3),ca).
/*


'mc__1_3_interpret-expression'(A,B,C,D) :-  
  catch( 
       ((
        (  ((
            (  ((
                ('mi__1_3_decons-ht'(A,E,_,F),is_True(F))*->
                (( G=['get-type',E,C]  ,
                   H=G , 
                   'mi__1_1_is-function'(H,I) , 
                   J=I , 
                     ((
                      (  ((
                          ('mi__1_2_metta-unify'(J,'True',K),is_True(K))*->
                          (( 'mi__1_4_interpret-func'(A,H,B,C,L)  ,
                             M=L , 
                             'mi__1_3_metta-call'(M,B,C,N) , 
                             O=N , 
                             throw(metta_return(O)) , 
                             P=O))  )));
                      (( 'mi__1_2_interpret-tuple'(A,C,Q)  ,
                         M=Q , 
                         'mi__1_3_metta-call'(M,B,C,R) , 
                         O=R , 
                         throw(metta_return(O)) , 
                         P=O))  )) , 
                   S=P))  )));
            (( T=['type-cast',A,B,C]  ,
               O=T , 
               throw(metta_return(O)) , 
               S=O))  ))),
        (mi__1_1_eval(S,D))  )), metta_return(U),U=D).


 */
transpiler_clause_store('interpret-func',[4],1,['Any','Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-func',_expr,_type,_ret_type,_space],[function,[eval,[if,['decons-ht',_expr,_op,_args],[let,_reduced_op,[interpret,_op,_type,_space],[eval,['return-on-error',_reduced_op,[eval,[if,['decons-ht',_type,_arrow,_arg_types],[let,_reduced_args,['interpret-args',_expr,_args,_arg_types,_ret_type,_space],[eval,['return-on-error',_reduced_args,[let,_r,['cons-atom',_reduced_op,_reduced_args],[return,_r]]]]],[return,['Error',_type,"Function type expected"]]]]]]],[return,['Error',_expr,"Non-empty expression atom is expected"]]]]]).
optimized_code(fa('interpret-func',4),ca).
/*


'mi__1_4_interpret-func'(B,C,D,E,F) :- 
  G =  
    'mc__1_4_interpret-func'(B,C,D,E,F) , 
  ci( true, 
    'interpret-func', 
    4, 
    [ 'interpret-func', B,C,D,E], F,true,G).


 */
optimized_code(fa('interpret-func',4),ca).
/*


'me__1_4_interpret-func'(B,C,D,E,F) :-  
  'mc__1_4_interpret-func'(B,C,D,E,F).


 */
optimized_code(fa('interpret-func',4),ca).
/*


'mc__1_4_interpret-func'(A,B,C,D,E) :-  
  catch( 
       ((
        (  ((
            (  ((
                ('mi__1_3_decons-ht'(A,F,G,H),is_True(H))*->
                (( mi__1_3_interpret(F,B,D,I)  ,
                   J=I , 
                   K=ispu(J) , 
                   L =  
                     ispeEnNC( M, 
                       mi__1_1_eval(N,M), O,O=[eval,N], 
                         ((
                          (  ((
                              ('mi__1_3_decons-ht'(B,_,P,Q),is_True(Q))*->
                              (( 'mi__1_5_interpret-args'(A,G,P,C,D,R)  ,
                                 S=R , 
                                 T=ispu(S) , 
                                 U =  
                                   ispuU( V, 
                                     'mi__1_2_cons-atom'(J,S,W),V=W,throw(metta_return(V))) , 
                                 X=['return-on-error',T,U] , 
                                 as_p1_expr(X,Y) , 
                                 Z=[eval,Y] , 
                                 N=Z))  )));
                          (  ((
                              (throw(metta_return(['Error',B,"Function type expected"]))),
                              (N =  
                                 [ 'Error', B,"Function type expected"])  )))  ))) , 
                   A1=['return-on-error',K,L] , 
                   as_p1_expr(A1,B1) , 
                   C1=[eval,B1] , 
                   D1=C1))  )));
            (  ((
                (throw(metta_return(['Error',A,"Non-empty expression atom is expected"]))),
                (D1 =  
                   [ 'Error', A,"Non-empty expression atom is expected"])  )))  ))),
        (mi__1_1_eval(D1,E))  )), metta_return(E1),E1=E).


 */
transpiler_clause_store('interpret-args',[5],1,['Any','Any','Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-args',_atom,_args,_arg_types,_ret_type,_space],[function,[if,['metta-unify',_args,[]],[eval,[if,['decons-ht',_arg_types,_actual_ret_type,_type_tail],[let,_correct_type_len,[==,[],_type_tail],[eval,[if,_correct_type_len,[eval,['match-types',_actual_ret_type,_ret_type,[return,[]],[return,['Error',_atom,'BadType']]]],[return,['Error',_atom,'BadType']]]]],[return,['Error',_atom,"Too many arguments"]]]],[eval,[if,['decons-ht',_args,_head,_tail],[eval,[if,['decons-ht',_arg_types,_head_type,_tail_types],[let,_reduced_head,[interpret,_head,_head_type,_space],[eval,[if,['metta-equal',_reduced_head,_head],[let,_ret,['interpret-args-tail',_atom,_reduced_head,_tail,_tail_types,_ret_type,_space],[return,_ret]],[eval,['return-on-error',_reduced_head,[let,_ret,['interpret-args-tail',_atom,_reduced_head,_tail,_tail_types,_ret_type,_space],[return,_ret]]]]]]],[return,['Error',_atom,'BadType']]]],[return,['Error',['interpret-atom',_atom,_args,_arg_types,_space],"Non-empty expression atom is expected"]]]]]]).
optimized_code(fa('interpret-args',5),ca).
/*


'mi__1_5_interpret-args'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_interpret-args'(B,C,D,E,F,G) , 
  ci( true, 
    'interpret-args', 
    5, 
    [ 'interpret-args', B,C,D,E, 
      F], G,true,H).


 */
optimized_code(fa('interpret-args',5),ca).
/*


'me__1_5_interpret-args'(B,C,D,E,F,G) :-  
  'mc__1_5_interpret-args'(B,C,D,E,F,G).


 */
optimized_code(fa('interpret-args',5),ca).
/*


'mc__1_5_interpret-args'(A,B,C,D,E,F) :-  
  catch( 
       ((
        (  ((
            ('mi__1_2_metta-unify'(B,[],G),is_True(G))*->
            ((   ((
                  (  ((
                      ('mi__1_3_decons-ht'(C,H,I,J),is_True(J))*->
                      (( 'mi__1_2_=='([],I,K)  ,
                         L=K , 
                           ((
                            (  ((
                                (is_True(L))*->
                                (( M=ispu(H)  ,
                                   N=ispu(D) , 
                                   O =  
                                     ispuU([],throw(metta_return([]))) , 
                                   P =  
                                     ispuU( ['Error',A,'BadType'], 
                                       throw(metta_return(['Error',A,'BadType']))) , 
                                   Q =  
                                     [ 'match-types', M,N,O,P] , 
                                   as_p1_expr(Q,R) , 
                                   S=[eval,R] , 
                                   T=S))  )));
                            (  ((
                                (throw(metta_return(['Error',A,'BadType']))),
                                (T=['Error',A,'BadType'])  )))  )) , 
                         U=[eval,T] , 
                         V=U))  )));
                  (  ((
                      (throw(metta_return(['Error',A,"Too many arguments"]))),
                      (V =  
                         [ 'Error', A,"Too many arguments"])  )))  ))  ,
               mi__1_1_eval(V,W) , 
               F=W))  )));
        ((   ((
              (  ((
                  ('mi__1_3_decons-ht'(B,X,Y,Z),is_True(Z))*->
                  ((   ((
                        (  ((
                            ('mi__1_3_decons-ht'(C,A1,B1,C1),is_True(C1))*->
                            (( mi__1_3_interpret(X,A1,E,D1)  ,
                               E1=D1 , 
                                 ((
                                  (  ((
                                      (F1=['metta-equal',E1,X],is_True(F1))*->
                                      (( 'mi__1_6_interpret-args-tail'(A,E1,Y,B1,D,E,G1)  ,
                                         H1=G1 , 
                                         throw(metta_return(H1)) , 
                                         I1=H1))  )));
                                  (( J1=ispu(E1)  ,
                                     K1 =  
                                       ispuU( H1, 
                                         ( 'mi__1_6_interpret-args-tail'(A,E1,Y,B1,D,E,L1)  ,
                                           H1=L1 , 
                                           throw(metta_return(H1)))) , 
                                     M1=['return-on-error',J1,K1] , 
                                     as_p1_expr(M1,N1) , 
                                     O1=[eval,N1] , 
                                     I1=O1))  )) , 
                               P1=[eval,I1] , 
                               Q1=P1))  )));
                        (  ((
                            (throw(metta_return(['Error',A,'BadType']))),
                            (Q1=['Error',A,'BadType'])  )))  ))  ,
                     R1=[eval,Q1] , 
                     S1=R1))  )));
              (  ((
                  (throw( metta_return( [ 'Error', 
                                          [ 'interpret-atom', A,B,C,E], 
                                          "Non-empty expression atom is expected"]))),
                  (S1 =  
                     [ 'Error', 
                       [ 'interpret-atom', A,B,C,E], 
                       "Non-empty expression atom is expected"])  )))  ))  ,
           mi__1_1_eval(S1,T1) , 
           F=T1))  )), metta_return(U1),U1=F).


 */
transpiler_clause_store('interpret-args-tail',[6],1,['Any','Any','Any','Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-args-tail',_atom,_head,_args_tail,_args_tail_types,_ret_type,_space],[function,[let,_reduced_tail,['interpret-args',_atom,_args_tail,_args_tail_types,_ret_type,_space],[eval,['return-on-error',_reduced_tail,[let,_ret,['cons-atom',_head,_reduced_tail],[return,_ret]]]]]]).
optimized_code(fa('interpret-args-tail',6),ca).
/*


'mi__1_6_interpret-args-tail'(B,C,D,E,F,G,H) :- 
  I =  
    'mc__1_6_interpret-args-tail'(B,C,D,E,F,G,H) , 
  ci( true, 
    'interpret-args-tail', 
    6, 
    [ 'interpret-args-tail', B,C,D,E, 
      F,G], H,true,I).


 */
optimized_code(fa('interpret-args-tail',6),ca).
/*


'me__1_6_interpret-args-tail'(B,C,D,E,F,G,H) :-  
  'mc__1_6_interpret-args-tail'(B,C,D,E,F,G,H).


 */
optimized_code(fa('interpret-args-tail',6),ca).
/*


'mc__1_6_interpret-args-tail'(A,B,C,D,E,F,G) :-  
  catch( 
     ( 'mi__1_5_interpret-args'(A,C,D,E,F,H)  ,
       I=H , 
       J=ispu(I) , 
       K =  
         ispuU( L, 
           'mi__1_2_cons-atom'(B,I,M),L=M,throw(metta_return(L))) , 
       N=['return-on-error',J,K] , 
       as_p1_expr(N,O) , 
       mi__1_1_eval(O,G)), metta_return(P),P=G).


 */
transpiler_clause_store('interpret-tuple',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-tuple',_atom,_space],[function,[if,['metta-unify',_atom,[]],[return,_atom],[eval,[if,['decons-ht',_atom,_head,_tail],[let,_rhead,[interpret,_head,'%Undefined%',_space],[eval,[if,['metta-equal',_rhead,'Empty'],[return,'Empty'],[let,_rtail,['interpret-tuple',_tail,_space],[eval,[if,['metta-equal',_rtail,'Empty'],[return,'Empty'],[let,_ret,['cons-atom',_rhead,_rtail],[return,_ret]]]]]]]],[return,['Error',['interpret-tuple',_atom,_space],"Non-empty expression atom is expected as an argument"]]]]]]).
optimized_code(fa('interpret-tuple',2),ca).
/*


'mi__1_2_interpret-tuple'(B,C,D) :- 
  E='mc__1_2_interpret-tuple'(B,C,D) , 
  ci(true,'interpret-tuple',2,['interpret-tuple',B,C],D,true,E).


 */
optimized_code(fa('interpret-tuple',2),ca).
/*


'me__1_2_interpret-tuple'(B,C,D):-'mc__1_2_interpret-tuple'(B,C,D).


 */
optimized_code(fa('interpret-tuple',2),ca).
/*


'mc__1_2_interpret-tuple'(A,B,C) :-  
  catch( 
       ((
        (  ((
            ('mi__1_2_metta-unify'(A,[],D),is_True(D))*->
            (throw(metta_return(A)),C=A)  )));
        ((   ((
              (  ((
                  ('mi__1_3_decons-ht'(A,E,F,G),is_True(G))*->
                  (( mi__1_3_interpret(E,'%Undefined%',B,H)  ,
                     I=H , 
                       ((
                        (  ((
                            (J=['metta-equal',I,'Empty'],is_True(J))*->
                            (throw(metta_return('Empty')),K='Empty')  )));
                        (( 'mi__1_2_interpret-tuple'(F,B,L)  ,
                           M=L , 
                             ((
                              (  ((
                                  (N=['metta-equal',M,'Empty'],is_True(N))*->
                                  (throw(metta_return('Empty')),O='Empty')  )));
                              (( 'mi__1_2_cons-atom'(I,M,P)  ,
                                 Q=P , 
                                 throw(metta_return(Q)) , 
                                 O=Q))  )) , 
                           R=[eval,O] , 
                           K=R))  )) , 
                     S=[eval,K] , 
                     T=S))  )));
              (  ((
                  (throw( metta_return( [ 'Error', 
                                          ['interpret-tuple',A,B], 
                                          "Non-empty expression atom is expected as an argument"]))),
                  (T =  
                     [ 'Error', 
                       ['interpret-tuple',A,B], 
                       "Non-empty expression atom is expected as an argument"])  )))  ))  ,
           mi__1_1_eval(T,U) , 
           C=U))  )), metta_return(V),V=C).


 */
transpiler_clause_store('metta-call',[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['metta-call',_atom,_type,_space],[function,[eval,['if-error',_atom,[return,_atom],[let,_result,_atom,[eval,[if,['metta-equal',_result,'NotReducible'],[return,_atom],[eval,[if,['metta-equal',_result,'Empty'],[return,'Empty'],[eval,['if-error',_result,[return,_result],[let,_ret,[interpret,_result,_type,_space],[return,_ret]]]]]]]]]]]]).
optimized_code(fa('metta-call',3),ca).
/*


'mi__1_3_metta-call'(B,C,D,E) :- 
  F='mc__1_3_metta-call'(B,C,D,E) , 
  ci( true, 
    'metta-call', 
    3, 
    ['metta-call',B,C,D], E,true,F).


 */
optimized_code(fa('metta-call',3),ca).
/*


'me__1_3_metta-call'(B,C,D,E) :-  
  'mc__1_3_metta-call'(B,C,D,E).


 */
optimized_code(fa('metta-call',3),ca).
/*


'mc__1_3_metta-call'(A,B,C,D) :-  
  catch( 
     ( E=ispu(A)  ,
       F =  
         ispuU(A,throw(metta_return(A))) , 
       G =  
         ispeEnNC( H, 
           mi__1_1_eval(I,H), J,J=[eval,I], 
             ((
              (K=A),
              (  ((
                  (  ((
                      (L=['metta-equal',K,'NotReducible'],is_True(L))*->
                      (throw(metta_return(A)),I=A)  )));
                  ((   ((
                        (  ((
                            (M=['metta-equal',K,'Empty'],is_True(M))*->
                            (throw(metta_return('Empty')),N='Empty')  )));
                        (( O=ispu(K)  ,
                           P =  
                             ispuU(K,throw(metta_return(K))) , 
                           Q =  
                             ispuU( R, 
                               ( mi__1_3_interpret(K,B,C,S)  ,
                                 R=S , 
                                 throw(metta_return(R)))) , 
                           T=['if-error',O,P,Q] , 
                           as_p1_expr(T,U) , 
                           V=[eval,U] , 
                           N=V))  ))  ,
                     W=[eval,N] , 
                     I=W))  )))  ))) , 
       X=['if-error',E,F,G] , 
       as_p1_expr(X,Y) , 
       mi__1_1_eval(Y,D)), metta_return(Z),Z=D).


 */
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Standard library written in MeTTa ;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; TODO: Type is used here, but there is no definition for the -> type
%  ; constructor for instance, thus in practice it matches because -> has
%  ; %Undefined% type. We need to assign proper type to -> and other type
%  ; constructors but it is not possible until we support vararg types.
%  ; or Helper?
transpiler_clause_store('is-function-type',[1],1,['Type'],'Bool',[x(doeval,eager,['Type'])],x(doeval,eager,[boolean]),['is-function-type',_type],[let,_type_meta,['get-metatype',_type],[case,_type_meta,[['Expression',[let,_first,['car-atom',_type],[if,[==,_first,->],'True','False']]],[_,'False']]]]).
optimized_code(fa('is-function-type',1),ca).
/*


'mi__1_1_is-function-type'(B,C) :- 
  D='mc__1_1_is-function-type'(B,C) , 
  ci(true,'is-function-type',1,['is-function-type',B],C,true,D).


 */
optimized_code(fa('is-function-type',1),ca).
/*


'me__1_1_is-function-type'(B,C):-'mc__1_1_is-function-type'(B,C).


 */
optimized_code(fa('is-function-type',1),ca).
/*


'mc__1_1_is-function-type'(A,B) :- 
  'mi__1_1_get-metatype'(A,C) , 
  D=C , 
  true*->E=D;E='Empty' , 
    ((
     (  ((
         (E='Expression')*->
         (( 'mi__1_1_car-atom'(A,F)  ,
            G=F , 
              ((
               ('mi__1_2_=='(G,->,H),is_True(H)*->B='True');
               (B='False')  )) , 
            B=B))  )));
     (  ((
         (E=_*->B='False';mc__1_0_empty(I),B=I),
         (B=B)  )))  )).


 */
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; MeTTa interpreter implementation ;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;`$then`, `$else` should be of `Atom` type to avoid evaluation
%  ; and infinite cycle in inference
transpiler_clause_store('add-reduct-minimal',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-reduct-minimal',_dst,_atom],['add-atom',_dst,_atom]).
optimized_code(fa('add-reduct-minimal',2),ca).
/*


'mi__1_2_add-reduct-minimal'(B,C,D) :- 
  E='mc__1_2_add-reduct-minimal'(B,C,D) , 
  ci( true, 
    'add-reduct-minimal', 
    2, 
    ['add-reduct-minimal',B,C], D,true,E).


 */
optimized_code(fa('add-reduct-minimal',2),ca).
/*


'me__1_2_add-reduct-minimal'(B,C,D) :-  
  'mc__1_2_add-reduct-minimal'(B,C,D).


 */
transpiler_clause_store('add-reduct',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-reduct',_dst,_atom],['add-atom',_dst,_atom]).
optimized_code(fa('add-reduct',2),ca).
/*


'mi__1_2_add-reduct'(B,C,D) :- 
  E='mc__1_2_add-reduct'(B,C,D) , 
  ci(true,'add-reduct',2,['add-reduct',B,C],D,true,E).


 */
optimized_code(fa('add-reduct',2),ca).
/*


'me__1_2_add-reduct'(B,C,D):-'mc__1_2_add-reduct'(B,C,D).


 */
transpiler_clause_store('cdr-atom',[1],1,['Expression'],'Expression',[x(noeval,eager,[])],x(noeval,eager,[]),['cdr-atom',_atom],[eval,[if,['decons-ht',_atom,_,_tail],_tail,['Error',['cdr-atom',_atom],"cdr-atom expects a non-empty expression as an argument"]]]).
optimized_code(fa('cdr-atom',1),ca).
/*


'mi__1_1_cdr-atom'(B,C) :- 
  D='mc__1_1_cdr-atom'(B,C) , 
  ci(true,'cdr-atom',1,['cdr-atom',B],C,true,D).


 */
optimized_code(fa('cdr-atom',1),ca).
/*


'me__1_1_cdr-atom'(B,C):-'mc__1_1_cdr-atom'(B,C).


 */
transpiler_clause_store(quote,[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),[quote,_atom],'NotReducible').
optimized_code(fa(quote,1),ca).
/*


mi__1_1_quote(B,C) :- 
  D=mc__1_1_quote(B,C) , 
  ci(true,quote,1,[quote,B],C,true,D).


 */
optimized_code(fa(quote,1),ca).
/*


me__1_1_quote(B,C):-mc__1_1_quote(B,C).


 */
transpiler_clause_store(unquote,[1],1,['%Undefined%'],'%Undefined%',[x(doeval,eager,['%Undefined%'])],x(doeval,eager,['%Undefined%']),[unquote,[quote,_atom]],_atom).
optimized_code(fa(unquote,1),ca).
/*


mi__1_1_unquote(B,C) :- 
  D=mc__1_1_unquote(B,C) , 
  ci(true,unquote,1,[unquote,B],C,true,D).


 */
optimized_code(fa(unquote,1),ca).
/*


me__1_1_unquote(B,C):-mc__1_1_unquote(B,C).


 */
%  ; TODO: there is no way to define operation which consumes any number of
%  ; arguments  and returns unit
%  ; TODO: can be replaced by Empty atom and removed, kept for compatibility
%  ;For testing
%  ;(= (empty) Empty)
%  ;(= (empty-rust1) (let a b never-happens))
%  ; TODO: MINIMAL added for compatibility, remove after migration
%  ;(= (empty-minimal) Empty)
transpiler_clause_store('add-reducts',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-reducts',_space,_tuple],['foldl-atom',_tuple,[],_a,_b,[eval,['add-atom',_space,_b]]]).
optimized_code(fa('add-reducts',2),ca).
/*


'mi__1_2_add-reducts'(B,C,D) :- 
  E='mc__1_2_add-reducts'(B,C,D) , 
  ci(true,'add-reducts',2,['add-reducts',B,C],D,true,E).


 */
optimized_code(fa('add-reducts',2),ca).
/*


'me__1_2_add-reducts'(B,C,D):-'mc__1_2_add-reducts'(B,C,D).


 */
optimized_code(fa('add-reducts',2),ca).
/*


'mc__1_2_add-reducts'(A,B,C) :- 
  D=ispu([]) , 
  E =  
    ispeEnNC( F, 
      mi__1_1_eval(G,F), H,H=[eval,G], 
      G=['add-atom',A,I]) , 
  'mi__1_5_foldl-atom'(B,D,_,I,E,J) , 
  as_p1_exec(J,C).


 */
transpiler_clause_store('add-atoms',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-atoms',_space,_tuple],['foldl-atom',_tuple,[],_a,_b,[eval,['add-atom',_space,_b]]]).
optimized_code(fa('add-atoms',2),ca).
/*


'mi__1_2_add-atoms'(B,C,D) :- 
  E='mc__1_2_add-atoms'(B,C,D) , 
  ci(true,'add-atoms',2,['add-atoms',B,C],D,true,E).


 */
optimized_code(fa('add-atoms',2),ca).
/*


'me__1_2_add-atoms'(B,C,D):-'mc__1_2_add-atoms'(B,C,D).


 */
optimized_code(fa('add-atoms',2),ca).
/*


'mc__1_2_add-atoms'(A,B,C) :- 
  D=ispu([]) , 
  E =  
    ispeEnNC( F, 
      mi__1_1_eval(G,F), H,H=[eval,G], 
      G=['add-atom',A,I]) , 
  'mi__1_5_foldl-atom'(B,D,_,I,E,J) , 
  as_p1_exec(J,C).


 */
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Documentation formatting functions
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; TODO: help! gives two outputs
%  ;Atom (@kind function): (%Undefined% (-> Atom Atom)) Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
%  ;Atom (@kind function): DocKindFunction Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
transpiler_clause_store('get-doc',[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),['get-doc',_atom],[let,_meta_type,['get-metatype',_atom],[case,_meta_type,[['Expression',['get-doc-atom',_atom]],[_,['get-doc-single-atom',_atom]]]]]).
optimized_code(fa('get-doc',1),ca).
/*


'mi__1_1_get-doc'(B,C) :- 
  D='mc__1_1_get-doc'(B,C) , 
  ci(true,'get-doc',1,['get-doc',B],C,true,D).


 */
optimized_code(fa('get-doc',1),ca).
/*


'me__1_1_get-doc'(B,C):-'mc__1_1_get-doc'(B,C).


 */
optimized_code(fa('get-doc',1),ca).
/*


'mc__1_1_get-doc'(A,B) :-  
  B =  
    ispeEnNC( C, 
        ((
         (  ((
             (  ((
                 (D='Expression')*->
                 (  ((
                     (E =  
                        ispeEnN( F, 
                          'mi__1_1_get-doc-atom'(A,G),as_p1_exec(G,F), 
                          H, 
                          I=['get-doc-atom',A],as_p1_expr(I,H))),
                     (J=E)  )))  )));
             (  ((
                 (K =  
                    ispeEnN( L, 
                        ((
                         (  ((
                             (  ((
                                 (D=M)*->
                                 (  ((
                                     (N =  
                                        ispeEnN( O, 
                                          'mi__1_1_get-doc-single-atom'(A,P),as_p1_exec(P,O), 
                                          Q, 
                                          R=['get-doc-single-atom',A],as_p1_expr(R,Q))),
                                     (S=N)  )))  )));
                             (T=ispuU(U,mc__1_0_empty(U)),S=T)  ))),
                         (as_p1_exec(S,L))  )), 
                      V, 
                        ((
                         (  ((
                             (  ((
                                 (D=M)*->
                                 (R=['get-doc-single-atom',A],S=R)  )));
                             (mc__1_0_empty(U),S=U)  ))),
                         (as_p1_expr(S,V))  )))),
                 (J=K)  )))  ))),
         (as_p1_exec(J,C))  )), 
      W, 
        ((
         (  ((
             (  ((
                 (D='Expression')*->
                 (I=['get-doc-atom',A],J=I)  )));
             (  ((
                 (  ((
                     (  ((
                         (D=M)*->
                         (R=['get-doc-single-atom',A],S=R)  )));
                     (mc__1_0_empty(U),S=U)  ))),
                 (J=S)  )))  ))),
         (as_p1_expr(J,W))  )), 
      ( as_p1_expr(A,X)  ,
        'mi__1_1_get-metatype'(X,Y) , 
        Z=Y , 
        true*->D=Z;D='Empty')).


 */
transpiler_clause_store('get-doc-single-atom',[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),['get-doc-single-atom',_atom],[let,_top_space,['mod-space!',top],[let,_type,['get-type-space',_top_space,_atom],[if,['is-function-type',_type],['get-doc-function',_atom,_type],['get-doc-atom',_atom]]]]).
optimized_code(fa('get-doc-single-atom',1),ca).
/*


'mi__1_1_get-doc-single-atom'(B,C) :- 
  D='mc__1_1_get-doc-single-atom'(B,C) , 
  ci(true,'get-doc-single-atom',1,['get-doc-single-atom',B],C,true,D).


 */
optimized_code(fa('get-doc-single-atom',1),ca).
/*


'me__1_1_get-doc-single-atom'(B,C):-'mc__1_1_get-doc-single-atom'(B,C).


 */
optimized_code(fa('get-doc-single-atom',1),ca).
/*


'mc__1_1_get-doc-single-atom'(A,B) :-  
  B =  
    ispeEnNC( C, 
        ((
         (  ((
             (  ((
                 ('mi__1_1_is-function-type'(D,E),is_True(E))*->
                 ('mi__1_2_get-doc-function'(A,D,F),G=F)  )));
             ('mi__1_1_get-doc-atom'(A,H),G=H)  ))),
         (as_p1_exec(G,C))  )), 
      I, 
        ((
         (  ((
             (  ((
                 ('mi__1_1_is-function-type'(D,E),is_True(E))*->
                 (J=['get-doc-function',A,D],K=J)  )));
             (L=['get-doc-atom',A],K=L)  ))),
         (as_p1_expr(K,I))  )), 
      ( M=ispu(top)  ,
        'mi__1_1_mod-space!'(M,N) , 
        O=N , 
        'mi__1_2_get-type-space'(O,A,P) , 
        as_p1_exec(P,Q) , 
        D=Q)).


 */
transpiler_clause_store('get-doc-function',[2],1,['Atom','Type'],'Atom',[x(noeval,lazy,[]),x(doeval,eager,['Type'])],x(noeval,lazy,[]),['get-doc-function',_name,_type],[let,_top_space,['mod-space!',top],[unify,_top_space,['@doc',_name,_desc,['@params',_params],_ret],[let,_type_39,[if,[==,_type,'%Undefined%'],['undefined-doc-function-type',_params],['cdr-atom',_type]],[let,[_params_39,_ret_39],['get-doc-params',_params,_ret,_type_39],['@doc-formal',['@item',_name],['@kind',function],['@type',_type],_desc,['@params',_params_39],_ret_39]]],['@doc-formal',['@item',_name],['@kind',function],['@type',_type],['@desc',"No documentation"]]]]).
optimized_code(fa('get-doc-function',2),ca).
/*


'mi__1_2_get-doc-function'(B,C,D) :- 
  E='mc__1_2_get-doc-function'(B,C,D) , 
  ci(true,'get-doc-function',2,['get-doc-function',B,C],D,true,E).


 */
optimized_code(fa('get-doc-function',2),ca).
/*


'me__1_2_get-doc-function'(B,C,D):-'mc__1_2_get-doc-function'(B,C,D).


 */
optimized_code(fa('get-doc-function',2),ca).
/*


'mc__1_2_get-doc-function'(A,B,C) :-  
  C =  
    ispeEnNC( D, 
      mi__1_4_unify(E,F,G,H,D), 
      I, 
      I =  
        [ unify, E,F,G,H], 
      ( J=ispu(top)  ,
        'mi__1_1_mod-space!'(J,K) , 
        E=K , 
        as_p1_exec(A,L) , 
        M=['@params',N] , 
        F =  
          [ '@doc', L,O,M,P] , 
        G =  
          ispeEnNC( Q, 
            ( as_p1_exec(A,R)  ,
              S=['@item',R] , 
              T=['@kind',function] , 
              U=['@type',B] , 
              V=['@params',W] , 
              Q =  
                [ '@doc-formal', S,T,U,O, 
                  V,X]), 
            Y, 
            ( as_p1_expr(A,Z)  ,
              A1=['@item',Z] , 
              B1=['@kind',function] , 
              C1=['@type',B] , 
              D1=['@params',W] , 
              Y =  
                [ '@doc-formal', A1,B1,C1,O, 
                  D1,X]), 
            (   ((
                 (  ((
                     ('mi__1_2_=='(B,'%Undefined%',E1),is_True(E1))*->
                     ('mi__1_1_undefined-doc-function-type'(N,F1),G1=F1)  )));
                 ('mi__1_1_cdr-atom'(B,H1),G1=H1)  ))  ,
              I1=G1 , 
              J1=ispu(P) , 
              'mi__1_3_get-doc-params'(N,J1,I1,K1) , 
              [W,X]=K1)) , 
        H =  
          ispeEnN( L1, 
            ( as_p1_exec(A,M1)  ,
              N1=['@item',M1] , 
              O1=['@kind',function] , 
              P1=['@type',B] , 
              Q1 =  
                [ '@desc', 
                  "No documentation"] , 
              L1 =  
                [ '@doc-formal', N1,O1,P1,Q1]), 
            R1, 
            ( as_p1_expr(A,S1)  ,
              T1=['@item',S1] , 
              U1=['@kind',function] , 
              V1=['@type',B] , 
              W1 =  
                [ '@desc', 
                  "No documentation"] , 
              R1 =  
                [ '@doc-formal', T1,U1,V1,W1])))).


 */
transpiler_clause_store('undefined-doc-function-type',[1],1,['Expression'],'Type',[x(noeval,eager,[])],x(doeval,eager,['Type']),['undefined-doc-function-type',_params],[if,[==,[],_params],['%Undefined%'],[let,_params_tail,['cdr-atom',_params],[let,_tail,['undefined-doc-function-type',_params_tail],['cons-atom','%Undefined%',_tail]]]]).
optimized_code(fa('undefined-doc-function-type',1),ca).
/*


'mi__1_1_undefined-doc-function-type'(B,C) :- 
  D='mc__1_1_undefined-doc-function-type'(B,C) , 
  ci( true, 
    'undefined-doc-function-type', 
    1, 
    ['undefined-doc-function-type',B], C,true,D).


 */
optimized_code(fa('undefined-doc-function-type',1),ca).
/*


'me__1_1_undefined-doc-function-type'(B,C) :-  
  'mc__1_1_undefined-doc-function-type'(B,C).


 */
optimized_code(fa('undefined-doc-function-type',1),ca).
/*


'mc__1_1_undefined-doc-function-type'(A,B) :-  
    ((
     (  ((
         ('mi__1_2_=='([],A,C),is_True(C))*->
         (D=['%Undefined%'],B=D)  )));
     (( 'mi__1_1_cdr-atom'(A,E)  ,
        F=E , 
        'mi__1_1_undefined-doc-function-type'(F,G) , 
        H=G , 
        'mi__1_2_cons-atom'('%Undefined%',H,I) , 
        B=I))  )).


 */
transpiler_clause_store('get-doc-params',[3],1,['Expression','Atom','Expression'],['Expression','Atom'],[x(noeval,eager,[]),x(noeval,lazy,[]),x(noeval,eager,[])],x(doeval,eager,[['Expression','Atom']]),['get-doc-params',_params,_ret,_types],[let,_head_type,['car-atom',_types],[let,_tail_types,['cdr-atom',_types],[if,[==,[],_params],[let,['@return',_ret_desc],_ret,[[],['@return',['@type',_head_type],['@desc',_ret_desc]]]],[let,['@param',_param_desc],['car-atom',_params],[let,_tail_params,['cdr-atom',_params],[let,[_params_39,_result_ret],['get-doc-params',_tail_params,_ret,_tail_types],[let,_result_params,['cons-atom',['@param',['@type',_head_type],['@desc',_param_desc]],_params_39],[_result_params,_result_ret]]]]]]]]).
optimized_code(fa('get-doc-params',3),ca).
/*


'mi__1_3_get-doc-params'(B,C,D,E) :- 
  F='mc__1_3_get-doc-params'(B,C,D,E) , 
  ci( true, 
    'get-doc-params', 
    3, 
    ['get-doc-params',B,C,D], E,true,F).


 */
optimized_code(fa('get-doc-params',3),ca).
/*


'me__1_3_get-doc-params'(B,C,D,E) :-  
  'mc__1_3_get-doc-params'(B,C,D,E).


 */
optimized_code(fa('get-doc-params',3),ca).
/*


'mc__1_3_get-doc-params'(A,B,C,D) :- 
  'mi__1_1_car-atom'(C,E) , 
  F=E , 
  'mi__1_1_cdr-atom'(C,G) , 
  H=G , 
    ((
     (  ((
         ('mi__1_2_=='([],A,I),is_True(I))*->
         (( as_p1_exec(B,J)  ,
            ['@return',K]=J , 
            L=['@type',F] , 
            M=['@desc',K] , 
            N=['@return',L,M] , 
            O=[[],N] , 
            D=O))  )));
     (( 'mi__1_1_car-atom'(A,P)  ,
        ['@param',Q]=P , 
        'mi__1_1_cdr-atom'(A,R) , 
        S=R , 
        'mi__1_3_get-doc-params'(S,B,H,T) , 
        [U,V]=T , 
        W=['@type',F] , 
        X=['@desc',Q] , 
        Y=['@param',W,X] , 
        'mi__1_2_cons-atom'(Y,U,Z) , 
        A1=Z , 
        transpiler_apply( mc__1_1_, 
          A1, 
          [A1,V], 
          B1, 
          [V], 
          [V], 
          [x(noeval,eager,[])], 
          [true], 
          [true]) , 
        D=B1))  )).


 */
transpiler_clause_store('get-doc-atom',[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),['get-doc-atom',_atom],[let,_top_space,['mod-space!',top],[let,_type,['get-type-space',_top_space,_atom],[unify,_top_space,['@doc',_atom,_desc],['@doc-formal',['@item',_atom],['@kind',atom],['@type',_type],_desc],[unify,_top_space,['@doc',_atom,_desc_39,['@params',_params],_ret],['get-doc-function',_atom,'%Undefined%'],['@doc-formal',['@item',_atom],['@kind',atom],['@type',_type],['@desc',"No documentation"]]]]]]).
optimized_code(fa('get-doc-atom',1),ca).
/*


'mi__1_1_get-doc-atom'(B,C) :- 
  D='mc__1_1_get-doc-atom'(B,C) , 
  ci(true,'get-doc-atom',1,['get-doc-atom',B],C,true,D).


 */
optimized_code(fa('get-doc-atom',1),ca).
/*


'me__1_1_get-doc-atom'(B,C):-'mc__1_1_get-doc-atom'(B,C).


 */
optimized_code(fa('get-doc-atom',1),ca).
/*


'mc__1_1_get-doc-atom'(A,B) :-  
  B =  
    ispeEnNC( C, 
      mi__1_4_unify(D,E,F,G,C), 
      H, 
      H =  
        [ unify, D,E,F,G], 
      ( I=ispu(top)  ,
        'mi__1_1_mod-space!'(I,J) , 
        D=J , 
        'mi__1_2_get-type-space'(D,A,K) , 
        as_p1_exec(K,L) , 
        M=L , 
        as_p1_exec(A,N) , 
        E=['@doc',N,O] , 
        F =  
          ispeEnN( P, 
            ( as_p1_exec(A,Q)  ,
              R=['@item',Q] , 
              S=['@kind',atom] , 
              T=['@type',M] , 
              P =  
                [ '@doc-formal', R,S,T,O]), 
            U, 
            ( as_p1_expr(A,V)  ,
              W=['@item',V] , 
              X=['@kind',atom] , 
              Y=['@type',M] , 
              U =  
                [ '@doc-formal', W,X,Y,O])) , 
        G =  
          ispeEnNC( Z, 
            mi__1_4_unify(D,A1,B1,C1,Z), 
            D1, 
            D1 =  
              [ unify, D,A1,B1,C1], 
            ( as_p1_exec(A,E1)  ,
              F1=['@params',_] , 
              A1 =  
                [ '@doc', E1,_,F1,_] , 
              B1 =  
                ispeEnN( G1, 
                  'mi__1_2_get-doc-function'(A,'%Undefined%',H1),as_p1_exec(H1,G1), 
                  I1, 
                    ((
                     (J1=['get-doc-function',A,'%Undefined%']),
                     (as_p1_expr(J1,I1))  ))) , 
              C1 =  
                ispeEnN( K1, 
                  ( as_p1_exec(A,L1)  ,
                    M1=['@item',L1] , 
                    N1=['@kind',atom] , 
                    O1=['@type',M] , 
                    P1 =  
                      [ '@desc', 
                        "No documentation"] , 
                    K1 =  
                      [ '@doc-formal', M1,N1,O1,P1]), 
                  Q1, 
                  ( as_p1_expr(A,R1)  ,
                    S1=['@item',R1] , 
                    T1=['@kind',atom] , 
                    U1=['@type',M] , 
                    V1 =  
                      [ '@desc', 
                        "No documentation"] , 
                    Q1 =  
                      [ '@doc-formal', S1,T1,U1,V1])))))).


 */
transpiler_clause_store('help!',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['help!',_atom],[case,['get-doc',_atom],[[['@doc-formal',['@item',_item],['@kind',function],['@type',_type],['@desc',_descr],['@params',_params],['@return',['@type',_ret_type],['@desc',_ret_desc]]],[let,[],['println!',['format-args',"Function {}: {} {}",[_item,_type,_descr]]],[let,[],['println!',['format-args',"Parameters:",[]]],[let,[],['for-each-in-atom',_params,'help-param!'],[let,[],['println!',['format-args',"Return: (type {}) {}",[_ret_type,_ret_desc]]],[]]]]]],[['@doc-formal',['@item',_item],['@kind',function],['@type',_type],['@desc',_descr]],[let,[],['println!',['format-args',"Function {} (type {}) {}",[_item,_type,_descr]]],[]]],[['@doc-formal',['@item',_item],['@kind',atom],['@type',_type],['@desc',_descr]],[let,[],['println!',['format-args',"Atom {}: {} {}",[_item,_type,_descr]]],[]]],[_other,['Error',_other,"Cannot match @doc-formal structure"]]]]).
optimized_code(fa('help!',1),ca).
/*


'mi__1_1_help!'(B,C) :- 
  D='mc__1_1_help!'(B,C) , 
  ci(true,'help!',1,['help!',B],C,true,D).


 */
optimized_code(fa('help!',1),ca).
/*


'me__1_1_help!'(B,C):-'mc__1_1_help!'(B,C).


 */
optimized_code(fa('help!',1),ca).
/*


'mc__1_1_help!'(A,B) :- 
    ((
     (  ((
         (C=ispu(A),'mi__1_1_get-doc'(C,D),as_p1_exec(D,E))*->
         (F=E)  )));
     (F='Empty')  )) , 
  G=['@item',H] , 
  I=['@kind',function] , 
  J=['@type',K] , 
  L=['@desc',M] , 
  N=['@params',O] , 
  P=['@type',Q] , 
  R=['@desc',S] , 
  T=['@return',P,R] , 
  U =  
    [ '@doc-formal', G,I,J,L, 
      N,T] , 
    ((
     (  ((
         (F=U)*->
         (( V=[H,K,M]  ,
            'mi__1_2_format-args'("Function {}: {} {}",V,W) , 
            'mi__1_1_println!'(W,X) , 
            []=X , 
            'mi__1_2_format-args'("Parameters:",[],Y) , 
            'mi__1_1_println!'(Y,Z) , 
            []=Z , 
            'mi__1_2_for-each-in-atom'(O,'help-param!',A1) , 
            []=A1 , 
            B1=[Q,S] , 
            'mi__1_2_format-args'("Return: (type {}) {}",B1,C1) , 
            'mi__1_1_println!'(C1,D1) , 
            []=D1 , 
            B=[]))  )));
     (( E1=['@item',H]  ,
        F1=['@kind',function] , 
        G1=['@type',K] , 
        H1=['@desc',M] , 
        I1 =  
          [ '@doc-formal', E1,F1,G1,H1] , 
          ((
           (  ((
               (F=I1)*->
               (( J1=[H,K,M]  ,
                  'mi__1_2_format-args'("Function {} (type {}) {}",J1,K1) , 
                  'mi__1_1_println!'(K1,L1) , 
                  []=L1 , 
                  B=[]))  )));
           (( M1=['@item',H]  ,
              N1=['@kind',atom] , 
              O1=['@type',K] , 
              P1=['@desc',M] , 
              Q1 =  
                [ '@doc-formal', M1,N1,O1,P1] , 
                ((
                 (  ((
                     (F=Q1)*->
                     (( R1=[H,K,M]  ,
                        'mi__1_2_format-args'("Atom {}: {} {}",R1,S1) , 
                        'mi__1_1_println!'(S1,T1) , 
                        []=T1 , 
                        B=[]))  )));
                 (  ((
                     (  ((
                         (  ((
                             (F=U1)*->
                             (B =  
                                [ 'Error', U1,"Cannot match @doc-formal structure"])  )));
                         (mc__1_0_empty(V1),B=V1)  ))),
                     (B=B)  )))  )) , 
              B=B))  )) , 
        B=B))  )).


 */
transpiler_clause_store('help!',[0],1,[],'Any',[],x(doeval,eager,[]),['help!'],[let,_top_space,['mod-space!',top],[unify,_top_space,['@doc',_name,['@desc',_desc]],[let,[],['println!',['format-args',"{}\n\t{}",[_name,_desc]]],'Empty'],'Empty']]).
optimized_code(fa('help!',0),ca).
/*


'mi__1_0_help!'(B) :- 
  C='mc__1_0_help!'(B) , 
  ci(true,'help!',0,['help!'],B,true,C).


 */
optimized_code(fa('help!',0),ca).
/*


'me__1_0_help!'(B):-'mc__1_0_help!'(B).


 */
optimized_code(fa('help!',0),ca).
/*


'mc__1_0_help!'(A) :- 
  B=ispu(top) , 
  'mi__1_1_mod-space!'(B,C) , 
  D=C , 
  E=['@desc',F] , 
  G=['@doc',H,E] , 
  I =  
    ispuU( 'Empty', 
      ( J=[H,F]  ,
        'mi__1_2_format-args'("{}\n\t{}",J,K) , 
        'mi__1_1_println!'(K,L) , 
        []=L)) , 
  M=ispu('Empty') , 
  mi__1_4_unify(D,G,I,M,A).


 */
transpiler_clause_store('help-param!',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['help-param!',_param],[let,['@param',['@type',_type],['@desc',_desc]],_param,['println!',['format-args',"  {} {}",[[type,_type],_desc]]]]).
optimized_code(fa('help-param!',1),ca).
/*


'mi__1_1_help-param!'(B,C) :- 
  D='mc__1_1_help-param!'(B,C) , 
  ci(true,'help-param!',1,['help-param!',B],C,true,D).


 */
optimized_code(fa('help-param!',1),ca).
/*


'me__1_1_help-param!'(B,C):-'mc__1_1_help-param!'(B,C).


 */
optimized_code(fa('help-param!',1),ca).
/*


'mc__1_1_help-param!'(A,B) :- 
  [ '@param', 
    ['@type',C], 
    ['@desc',D]] =  
    A , 
  E=[type,C] , 
  F=[E,D] , 
  'mi__1_2_format-args'("  {} {}",F,G) , 
  'mi__1_1_println!'(G,B).


 */
transpiler_clause_store('for-each-in-atom',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['for-each-in-atom',_expr,_func],[if,['noreduce-eq',_expr,[]],[],[let,_head,['car-atom',_expr],[let,_tail,['cdr-atom',_expr],[let,_,[_func,_head],['for-each-in-atom',_tail,_func]]]]]).
optimized_code(fa('for-each-in-atom',2),ca).
/*


'mi__1_2_for-each-in-atom'(B,C,D) :- 
  E='mc__1_2_for-each-in-atom'(B,C,D) , 
  ci(true,'for-each-in-atom',2,['for-each-in-atom',B,C],D,true,E).


 */
optimized_code(fa('for-each-in-atom',2),ca).
/*


'me__1_2_for-each-in-atom'(B,C,D):-'mc__1_2_for-each-in-atom'(B,C,D).


 */
optimized_code(fa('for-each-in-atom',2),ca).
/*


'mc__1_2_for-each-in-atom'(A,B,C) :-  
    ((
     (  ((
         (( D=ispu(A)  ,
            E=ispu([]) , 
            'mi__1_2_noreduce-eq'(D,E,F) , 
            is_True(F)))*->
         (C=[])  )));
     (( 'mi__1_1_car-atom'(A,G)  ,
        H=G , 
        'mi__1_1_cdr-atom'(A,I) , 
        J=I , 
        transpiler_apply( mc__1_1_, 
          B, 
          [B,H], 
          K, 
          [H], 
          [H], 
          [x(noeval,eager,[])], 
          [true], 
          [true]) , 
        _=K , 
        'mi__1_2_for-each-in-atom'(J,B,L) , 
        C=L))  )).


 */
transpiler_clause_store('noreduce-eq',[2],1,['Atom','Atom'],'Bool',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(doeval,eager,[boolean]),['noreduce-eq',_a,_b],[==,[quote,_a],[quote,_b]]).
optimized_code(fa('noreduce-eq',2),ca).
/*


'mi__1_2_noreduce-eq'(B,C,D) :- 
  E='mc__1_2_noreduce-eq'(B,C,D) , 
  ci(true,'noreduce-eq',2,['noreduce-eq',B,C],D,true,E).


 */
optimized_code(fa('noreduce-eq',2),ca).
/*


'me__1_2_noreduce-eq'(B,C,D):-'mc__1_2_noreduce-eq'(B,C,D).


 */
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Grounded function's documentation
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
transpiler_clause_store('get-type-space',[2],1,['hyperon::space::DynSpace','Atom'],'Atom',[x(doeval,eager,['hyperon::space::DynSpace']),x(noeval,lazy,[])],x(noeval,lazy,[]),['get-type-space',_space,_atom],['get-type',_atom,_space]).
optimized_code(fa('get-type-space',2),ca).
/*


'mi__1_2_get-type-space'(B,C,D) :- 
  E='mc__1_2_get-type-space'(B,C,D) , 
  ci(true,'get-type-space',2,['get-type-space',B,C],D,true,E).


 */
optimized_code(fa('get-type-space',2),ca).
/*


'me__1_2_get-type-space'(B,C,D):-'mc__1_2_get-type-space'(B,C,D).


 */
%  ;(ALT= (match $space $pattern $template)
%  ;  (unify $space $pattern $template Empty))
%  ; TODO: help! not working for operations which are defined in both Python and
%  ; Rust standard library: +, -, *, /, %, <, >, <=, >=, ==
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;              NON-DETERMINISTIC SET OPERATIONS WITH PREDICATES          ;;
%  ;;                                                                        ;;
%  ;; These functions extend basic set operations by introducing a comparator;;
%  ;; predicate as the first argument. The predicate determines how          ;;
%  ;; elements are compared for equality, unification, or identity.          ;;
%  ;;                                                                        ;;
%  ;; A few examples of comparator predicates:                              ;;
%  ;;   - =alpha       : Structural equivalence, allowing variable renaming  ;;
%  ;;   - =identical   : Strict structural identity, including bindings       ;;
%  ;;   - =will   : Checks if unification is possible in any way         ;;
%  ;;   - =u=       : Standard unification with side effects               ;;
%  ;;   - =references  : Ensures atoms refer to the same memory object        ;;
%  ;;                                                                        ;;
%  ;; Functions:                                                             ;;
%  ;;   - `unique-by`       : Removes duplicates using a custom predicate  ;;
%  ;;   - `union-by`        : Merges two sets with predicate-based checks  ;;
%  ;;   - `intersection-by` : Computes intersection using a predicate      ;;
%  ;;   - `subtraction-by`  : Subtracts elements based on a predicate      ;;
%  ;;                                                                        ;;
%  ;; These functions handle **non-deterministic data**, using `superpose`.  ;;
%  ;;                                                                        ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;                    UNIFICATION AND EQUIVALENCE PREDICATES              ;;
%  ;;                                                                        ;;
%  ;; These predicates determine whether two atoms can unify or are          ;;
%  ;; structurally equivalent. Some predicates actively perform unification, ;;
%  ;; while others only check whether unification *could* occur.             ;;
%  ;;                                                                        ;;
%  ;; **Theoretical Equivalence & Unification Predicates (Check Possibility Only):** ;;
%  ;;   - `=alpha`        : Checks if two atoms are structurally equivalent, ;;
%  ;;                       allowing renaming, but does **not** unify them.  ;;
%  ;;   - `=will`         : Checks if two atoms *could* unify in theory, but ;;
%  ;;                       does **not** modify bindings or unify variables. ;;
%  ;;                                                                        ;;
%  ;; **Unification Predicates (Perform Binding):**                          ;;
%  ;;   - `=u=`        : **Performs** unification and binds variables.    ;;
%  ;;   - `=alpha-unify`  : **Performs** unification with variable renaming. ;;
%  ;;                                                                        ;;
%  ;; **Strict Identity & Reference Predicates:**                            ;;
%  ;;   - `=identical`    : Checks if two atoms are strictly identical,      ;;
%  ;;                       including variable names and bindings.           ;;
%  ;;   - `=references`   : Checks if two atoms reference the **same**       ;;
%  ;;                       underlying entity in memory.                     ;;
%  ;;                                                                        ;;
%  ;; Predicates in the "Perform Binding" category actively modify variable  ;;
%  ;; bindings when successful. The "Check Possibility Only" predicates do   ;;
%  ;; **not** modify bindings; they merely verify whether equivalence or     ;;
%  ;; unification might be possible.                                         ;;
%  ;;                                                                        ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;                   TUPLE-BASED SET OPERATIONS WITH PREDICATES           ;;
%  ;;                                                                        ;;
%  ;; These functions extend tuple-based set operations by introducing a     ;;
%  ;; comparator predicate as the first argument. This allows elements to be ;;
%  ;; compared using unification, alpha equivalence, or strict identity.     ;;
%  ;;                                                                        ;;
%  ;; A few examples of comparator predicates:                              ;;
%  ;;   - =alpha       : Structural equivalence, allowing variable renaming  ;;
%  ;;   - =identical   : Strict structural identity, including bindings       ;;
%  ;;   - =will   : Checks if unification is possible in any way         ;;
%  ;;   - =u=       : Standard unification with side effects               ;;
%  ;;   - =references  : Ensures atoms refer to the same memory object        ;;
%  ;;                                                                        ;;
%  ;; Functions:                                                             ;;
%  ;;   - `unique-atom-by`       : Removes duplicates using a predicate    ;;
%  ;;   - `union-atom-by`        : Merges two tuples with predicate-based  ;;
%  ;;   - `intersection-atom-by` : Computes intersection using a predicate ;;
%  ;;   - `subtraction-atom-by`  : Subtracts elements based on a predicate ;;
%  ;;                                                                        ;;
%  ;; These functions handle **tuple-based data**, using `superpose` where   ;;
%  ;; necessary to ensure compatibility with non-deterministic operations.   ;;
%  ;;                                                                        ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
transpiler_clause_store('unique-atom-by',[2],1,['Atom','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[])],x(noeval,eager,[]),['unique-atom-by',_Pred,_L1],[collapse,['unique-by',_Pred,[superpose,_L1]]]).
optimized_code(fa('unique-atom-by',2),ca).
/*


'mi__1_2_unique-atom-by'(B,C,D) :- 
  E='mc__1_2_unique-atom-by'(B,C,D) , 
  ci(true,'unique-atom-by',2,['unique-atom-by',B,C],D,true,E).


 */
optimized_code(fa('unique-atom-by',2),ca).
/*


'me__1_2_unique-atom-by'(B,C,D):-'mc__1_2_unique-atom-by'(B,C,D).


 */
optimized_code(fa('unique-atom-by',2),ca).
/*


'mc__1_2_unique-atom-by'(A,B,C) :- 
  D =  
    ispeEnN( E, 
      ( as_p1_exec(A,F)  ,
        mi__1_1_superpose(B,G) , 
        E=['unique-by',F,G]), 
      H, 
      ( as_p1_expr(A,I)  ,
        J=[superpose,B] , 
        H=['unique-by',I,J])) , 
  mi__1_1_collapse(D,C).


 */
transpiler_clause_store('union-atom-by',[3],1,['Atom','Expression','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['union-atom-by',_Pred,_L1,_L2],[collapse,['union-by',_Pred,[superpose,_L1],[superpose,_L2]]]).
optimized_code(fa('union-atom-by',3),ca).
/*


'mi__1_3_union-atom-by'(B,C,D,E) :- 
  F='mc__1_3_union-atom-by'(B,C,D,E) , 
  ci( true, 
    'union-atom-by', 
    3, 
    ['union-atom-by',B,C,D], E,true,F).


 */
optimized_code(fa('union-atom-by',3),ca).
/*


'me__1_3_union-atom-by'(B,C,D,E) :-  
  'mc__1_3_union-atom-by'(B,C,D,E).


 */
optimized_code(fa('union-atom-by',3),ca).
/*


'mc__1_3_union-atom-by'(A,B,C,D) :- 
  E =  
    ispeEnN( F, 
      ( as_p1_exec(A,G)  ,
        mi__1_1_superpose(B,H) , 
        mi__1_1_superpose(C,I) , 
        F=['union-by',G,H,I]), 
      J, 
      ( as_p1_expr(A,K)  ,
        L=[superpose,B] , 
        M=[superpose,C] , 
        J=['union-by',K,L,M])) , 
  mi__1_1_collapse(E,D).


 */


'mc__1_3_union-atom-by'(A,B,C,D) :-  
  mi__1_1_collapse( 
     ispeEnN( E, 
       ( as_p1_exec(A,F)  ,
         mi__1_1_superpose(B,G) , 
         mi__1_1_superpose(C,H) , 
         E=['union-by',F,G,H]), 
       I, 
         ((
          (as_p1_expr(A,J)),
          (  ((
              (true),
              (  ((
                  (true),
                  (I =  
                     [ 'union-by', 
                       J, 
                       [superpose,B], 
                       [superpose,C]])  )))  )))  ))), 
     D).


transpiler_clause_store('intersection-atom-by',[3],1,['Atom','Expression','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['intersection-atom-by',_Pred,_L1,_L2],[collapse,['intersection-by',_Pred,[superpose,_L1],[superpose,_L2]]]).
optimized_code(fa('intersection-atom-by',3),ca).
/*


'mi__1_3_intersection-atom-by'(B,C,D,E) :- 
  F='mc__1_3_intersection-atom-by'(B,C,D,E) , 
  ci( true, 
    'intersection-atom-by', 
    3, 
    ['intersection-atom-by',B,C,D], E,true,F).


 */
optimized_code(fa('intersection-atom-by',3),ca).
/*


'me__1_3_intersection-atom-by'(B,C,D,E) :-  
  'mc__1_3_intersection-atom-by'(B,C,D,E).


 */
optimized_code(fa('intersection-atom-by',3),ca).
/*


'mc__1_3_intersection-atom-by'(A,B,C,D) :- 
  E =  
    ispeEnN( F, 
      ( as_p1_exec(A,G)  ,
        mi__1_1_superpose(B,H) , 
        mi__1_1_superpose(C,I) , 
        F=['intersection-by',G,H,I]), 
      J, 
      ( as_p1_expr(A,K)  ,
        L=[superpose,B] , 
        M=[superpose,C] , 
        J=['intersection-by',K,L,M])) , 
  mi__1_1_collapse(E,D).


 */


'mc__1_3_intersection-atom-by'(A,B,C,D) :-  
  mi__1_1_collapse( 
     ispeEnN( E, 
       ( as_p1_exec(A,F)  ,
         mi__1_1_superpose(B,G) , 
         mi__1_1_superpose(C,H) , 
         E=['intersection-by',F,G,H]), 
       I, 
         ((
          (as_p1_expr(A,J)),
          (  ((
              (true),
              (  ((
                  (true),
                  (I =  
                     [ 'intersection-by', 
                       J, 
                       [superpose,B], 
                       [superpose,C]])  )))  )))  ))), 
     D).


transpiler_clause_store('subtraction-atom-by',[3],1,['Atom','Expression','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['subtraction-atom-by',_Pred,_L1,_L2],[collapse,['subtraction-by',_Pred,[superpose,_L1],[superpose,_L2]]]).
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_subtraction-atom-by'(B,C,D,E) :- 
  F='mc__1_3_subtraction-atom-by'(B,C,D,E) , 
  ci( true, 
    'subtraction-atom-by', 
    3, 
    ['subtraction-atom-by',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_subtraction-atom-by'(B,C,D,E) :-  
  'mc__1_3_subtraction-atom-by'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mc__1_3_subtraction-atom-by'(A,B,C,D) :- 
  E =  
    ispeEnN( F, 
      ( as_p1_exec(A,G)  ,
        mi__1_1_superpose(B,H) , 
        mi__1_1_superpose(C,I) , 
        F=['subtraction-by',G,H,I]), 
      J, 
      ( as_p1_expr(A,K)  ,
        L=[superpose,B] , 
        M=[superpose,C] , 
        J=['subtraction-by',K,L,M])) , 
  mi__1_1_collapse(E,D).


 */


'mc__1_3_subtraction-atom-by'(A,B,C,D) :-  
  mi__1_1_collapse( 
     ispeEnN( E, 
       ( as_p1_exec(A,F)  ,
         mi__1_1_superpose(B,G) , 
         mi__1_1_superpose(C,H) , 
         E=['subtraction-by',F,G,H]), 
       I, 
         ((
          (as_p1_expr(A,J)),
          (  ((
              (true),
              (  ((
                  (true),
                  (I =  
                     [ 'subtraction-by', 
                       J, 
                       [superpose,B], 
                       [superpose,C]])  )))  )))  ))), 
     D).


%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;                        TUPLE-BASED SET OPERATIONS                      ;;
%  ;;                                                                        ;;
%  ;; These functions operate on **tuples** instead of non-deterministic     ;;
%  ;; sets. They provide the same core operations but assume that inputs     ;;
%  ;; are lists of values rather than `superpose` expressions.               ;;
%  ;;                                                                        ;;
%  ;; Functions:                                                             ;;
%  ;;   - `unique-atom`        : Removes duplicate values in a tuple         ;;
%  ;;   - `union-atom`         : Merges two tuples, preserving duplicates    ;;
%  ;;   - `intersection-atom`  : Returns values found in both tuples         ;;
%  ;;   - `subtraction-atom`   : Removes elements found in the second tuple  ;;
%  ;;                                                                        ;;
%  ;; These functions assume strict equality comparisons.                    ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%% Finished generating /home/deb12user/metta-wam/prolog/vnamed/stdlib_mettalog.metta at 2025-05-30T01:56:33-07:00

:- normal_IO.
:- initialization(transpiled_main, program).
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_get-type'(B,C) :- 
  D='mc__1_1_get-type'(B,C) , 
  ci(true,'get-type',1,['get-type',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_get-type'(B,C):-'mc__1_1_get-type'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_3_If(B,C,D,E) :- 
  F=mc__1_3_If(B,C,D,E) , 
  ci(true,'If',3,['If',B,C,D],E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_3_If(B,C,D,E):-mc__1_3_If(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_3_if(B,C,D,E) :- 
  F=mc__1_3_if(B,C,D,E) , 
  ci(true,if,3,[if,B,C,D],E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_3_if(B,C,D,E):-mc__1_3_if(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_If(B,C,D) :- 
  E=mc__1_2_If(B,C,D) , 
  ci(true,'If',2,['If',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_If(B,C,D):-mc__1_2_If(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_if(B,C,D) :- 
  E=mc__1_2_if(B,C,D) , 
  ci(true,if,2,[if,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_if(B,C,D):-mc__1_2_if(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_+'(B,C,D) :- 
  E='mc__1_2_+'(B,C,D) , 
  ci(true,+,2,[+,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_+'(B,C,D):-'mc__1_2_+'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_-'(B,C,D) :- 
  E='mc__1_2_-'(B,C,D) , 
  ci(true,-,2,[-,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_-'(B,C,D):-'mc__1_2_-'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_*'(B,C,D) :- 
  E='mc__1_2_*'(B,C,D) , 
  ci(true,*,2,[*,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_*'(B,C,D):-'mc__1_2_*'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_/'(B,C,D) :- 
  E='mc__1_2_/'(B,C,D) , 
  ci(true,/,2,[/,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_/'(B,C,D):-'mc__1_2_/'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_%'(B,C,D) :- 
  E='mc__1_2_%'(B,C,D) , 
  ci(true,'%',2,['%',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_%'(B,C,D):-'mc__1_2_%'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_and(B,C,D) :- 
  E=mc__1_2_and(B,C,D) , 
  ci(true,and,2,[and,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_and(B,C,D):-mc__1_2_and(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_or(B,C,D) :- 
  E=mc__1_2_or(B,C,D) , 
  ci(true,or,2,[or,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_or(B,C,D):-mc__1_2_or(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_not(B,C) :- 
  D=mc__1_1_not(B,C) , 
  ci(true,not,1,[not,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_not(B,C):-mc__1_1_not(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_=='(B,C,D) :- 
  E='mc__1_2_=='(B,C,D) , 
  ci(true,==,2,[==,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_=='(B,C,D):-'mc__1_2_=='(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_<'(B,C,D) :- 
  E='mc__1_2_<'(B,C,D) , 
  ci(true,<,2,[<,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_<'(B,C,D):-'mc__1_2_<'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_>'(B,C,D) :- 
  E='mc__1_2_>'(B,C,D) , 
  ci(true,>,2,[>,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_>'(B,C,D):-'mc__1_2_>'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_>='(B,C,D) :- 
  E='mc__1_2_>='(B,C,D) , 
  ci(true,>=,2,[>=,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_>='(B,C,D):-'mc__1_2_>='(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_<='(B,C,D) :- 
  E='mc__1_2_<='(B,C,D) , 
  ci(true,<=,2,[<=,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_<='(B,C,D):-'mc__1_2_<='(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_car-atom'(B,C) :- 
  D='mc__1_1_car-atom'(B,C) , 
  ci(true,'car-atom',1,['car-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_car-atom'(B,C):-'mc__1_1_car-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_cdr-atom'(B,C) :- 
  D='mc__1_1_cdr-atom'(B,C) , 
  ci(true,'cdr-atom',1,['cdr-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_cdr-atom'(B,C):-'mc__1_1_cdr-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_cons-atom'(B,C,D) :- 
  E='mc__1_2_cons-atom'(B,C,D) , 
  ci(true,'cons-atom',2,['cons-atom',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_cons-atom'(B,C,D):-'mc__1_2_cons-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_decons-atom'(B,C) :- 
  D='mc__1_1_decons-atom'(B,C) , 
  ci(true,'decons-atom',1,['decons-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_decons-atom'(B,C):-'mc__1_1_decons-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_size-atom'(B,C) :- 
  D='mc__1_1_size-atom'(B,C) , 
  ci(true,'size-atom',1,['size-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_size-atom'(B,C):-'mc__1_1_size-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_subtraction(B,C,D) :- 
  E=mc__1_2_subtraction(B,C,D) , 
  ci(true,subtraction,2,[subtraction,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_subtraction(B,C,D):-mc__1_2_subtraction(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_union(B,C,D) :- 
  E=mc__1_2_union(B,C,D) , 
  ci(true,union,2,[union,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_union(B,C,D):-mc__1_2_union(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_unique(B,C) :- 
  D=mc__1_1_unique(B,C) , 
  ci(true,unique,1,[unique,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_unique(B,C):-mc__1_1_unique(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_unique-atom'(B,C) :- 
  D='mc__1_1_unique-atom'(B,C) , 
  ci(true,'unique-atom',1,['unique-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_unique-atom'(B,C):-'mc__1_1_unique-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_limit(B,C,D) :- 
  E=mc__1_2_limit(B,C,D) , 
  ci(true,limit,2,[limit,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_limit(B,C,D):-mc__1_2_limit(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_limit!'(B,C,D) :- 
  E='mc__1_2_limit!'(B,C,D) , 
  ci(true,'limit!',2,['limit!',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_limit!'(B,C,D):-'mc__1_2_limit!'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_superpose(B,C) :- 
  D=mc__1_1_superpose(B,C) , 
  ci(true,superpose,1,[superpose,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_superpose(B,C):-mc__1_1_superpose(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_collapse(B,C) :- 
  D=mc__1_1_collapse(B,C) , 
  ci(true,collapse,1,[collapse,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_collapse(B,C):-mc__1_1_collapse(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_bind!'(B,C,D) :- 
  E='mc__1_2_bind!'(B,C,D) , 
  ci(true,'bind!',2,['bind!',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_bind!'(B,C,D):-'mc__1_2_bind!'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_0_new-space'(B) :- 
  C='mc__1_0_new-space'(B) , 
  ci(true,'new-space',0,['new-space'],B,true,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_0_new-space'(B):-'mc__1_0_new-space'(B).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_add-atom'(B,C,D) :- 
  E='mc__1_2_add-atom'(B,C,D) , 
  ci(true,'add-atom',2,['add-atom',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_add-atom'(B,C,D):-'mc__1_2_add-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_remove-atom'(B,C,D) :- 
  E='mc__1_2_remove-atom'(B,C,D) , 
  ci(true,'remove-atom',2,['remove-atom',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_remove-atom'(B,C,D):-'mc__1_2_remove-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_get-atoms'(B,C) :- 
  D='mc__1_1_get-atoms'(B,C) , 
  ci(true,'get-atoms',1,['get-atoms',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_get-atoms'(B,C):-'mc__1_1_get-atoms'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_3_match(B,C,D,E) :- 
  F=mc__1_3_match(B,C,D,E) , 
  ci(true,match,3,[match,B,C,D],E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_3_match(B,C,D,E):-mc__1_3_match(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_3_unify(B,C,D,E) :- 
  F=mc__1_3_unify(B,C,D,E) , 
  ci(true,unify,3,[unify,B,C,D],E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_3_unify(B,C,D,E):-mc__1_3_unify(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_4_unify(B,C,D,E,F) :- 
  G =  
    mc__1_4_unify(B,C,D,E,F) , 
  ci( true, 
    unify, 
    4, 
    [ unify, B,C,D,E], F,true,G).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_4_unify(B,C,D,E,F) :-  
  mc__1_4_unify(B,C,D,E,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_time(B,C) :- 
  D=mc__1_1_time(B,C) , 
  ci(true,time,1,[time,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_time(B,C):-mc__1_1_time(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_0_empty(B) :- 
  C=mc__1_0_empty(B) , 
  ci(true,empty,0,[empty],B,true,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_0_empty(B):-mc__1_0_empty(B).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_eval(B,C) :- 
  D=mc__1_1_eval(B,C) , 
  ci(true,eval,1,[eval,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_eval(B,C):-mc__1_1_eval(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_get-metatype'(B,C) :- 
  D='mc__1_1_get-metatype'(B,C) , 
  ci(true,'get-metatype',1,['get-metatype',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_get-metatype'(B,C):-'mc__1_1_get-metatype'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_println!'(B,C) :- 
  D='mc__1_1_println!'(B,C) , 
  ci(true,'println!',1,['println!',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_println!'(B,C):-'mc__1_1_println!'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_format-args'(B,C,D) :- 
  E='mc__1_2_format-args'(B,C,D) , 
  ci(true,'format-args',2,['format-args',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_format-args'(B,C,D):-'mc__1_2_format-args'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_stringToChars(B,C) :- 
  D=mc__1_1_stringToChars(B,C) , 
  ci(true,stringToChars,1,[stringToChars,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_stringToChars(B,C):-mc__1_1_stringToChars(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_repr(B,C) :- 
  D=mc__1_1_repr(B,C) , 
  ci(true,repr,1,[repr,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_repr(B,C):-mc__1_1_repr(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_charsToString(B,C) :- 
  D=mc__1_1_charsToString(B,C) , 
  ci(true,charsToString,1,[charsToString,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_charsToString(B,C):-mc__1_1_charsToString(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_assertEqual(B,C,D) :- 
  E=mc__1_2_assertEqual(B,C,D) , 
  ci(true,assertEqual,2,[assertEqual,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_assertEqual(B,C,D):-mc__1_2_assertEqual(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_assertEqualToResult(B,C,D) :- 
  E=mc__1_2_assertEqualToResult(B,C,D) , 
  ci( true, 
    assertEqualToResult, 
    2, 
    [assertEqualToResult,B,C], D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_assertEqualToResult(B,C,D) :-  
  mc__1_2_assertEqualToResult(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_assertAlphaEqual(B,C,D) :- 
  E=mc__1_2_assertAlphaEqual(B,C,D) , 
  ci(true,assertAlphaEqual,2,[assertAlphaEqual,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_assertAlphaEqual(B,C,D):-mc__1_2_assertAlphaEqual(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_assertNotAlphaEqual(B,C,D) :- 
  E=mc__1_2_assertNotAlphaEqual(B,C,D) , 
  ci( true, 
    assertNotAlphaEqual, 
    2, 
    [assertNotAlphaEqual,B,C], D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_assertNotAlphaEqual(B,C,D) :-  
  mc__1_2_assertNotAlphaEqual(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_quote(B,C) :- 
  D=mc__1_1_quote(B,C) , 
  ci(true,quote,1,[quote,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_quote(B,C):-mc__1_1_quote(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_random-int'(B,C,D,E) :- 
  F='mc__1_3_random-int'(B,C,D,E) , 
  ci( true, 
    'random-int', 
    3, 
    ['random-int',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_random-int'(B,C,D,E) :-  
  'mc__1_3_random-int'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_random-float'(B,C,D,E) :- 
  F='mc__1_3_random-float'(B,C,D,E) , 
  ci( true, 
    'random-float', 
    3, 
    ['random-float',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_random-float'(B,C,D,E) :-  
  'mc__1_3_random-float'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_set-random-seed'(B,C,D) :- 
  E='mc__1_2_set-random-seed'(B,C,D) , 
  ci(true,'set-random-seed',2,['set-random-seed',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_set-random-seed'(B,C,D):-'mc__1_2_set-random-seed'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_new-random-generator'(B,C) :- 
  D='mc__1_1_new-random-generator'(B,C) , 
  ci(true,'new-random-generator',1,['new-random-generator',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_new-random-generator'(B,C):-'mc__1_1_new-random-generator'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_reset-random-generator'(B,C) :- 
  D='mc__1_1_reset-random-generator'(B,C) , 
  ci(true,'reset-random-generator',1,['reset-random-generator',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_reset-random-generator'(B,C):-'mc__1_1_reset-random-generator'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_0_prolog-trace'(B) :- 
  C='mc__1_0_prolog-trace'(B) , 
  ci(true,'prolog-trace',0,['prolog-trace'],B,true,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_0_prolog-trace'(B):-'mc__1_0_prolog-trace'(B).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_0_transpiler-listing'(B) :- 
  C='mc__1_0_transpiler-listing'(B) , 
  ci(true,'transpiler-listing',0,['transpiler-listing'],B,true,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_0_transpiler-listing'(B):-'mc__1_0_transpiler-listing'(B).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_metta-equals'(B,C,D) :- 
  E='mc__1_2_metta-equals'(B,C,D) , 
  ci(true,'metta-equals',2,['metta-equals',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_metta-equals'(B,C,D):-'mc__1_2_metta-equals'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_metta-unify'(B,C,D) :- 
  E='mc__1_2_metta-unify'(B,C,D) , 
  ci(true,'metta-unify',2,['metta-unify',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_metta-unify'(B,C,D):-'mc__1_2_metta-unify'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_decons-ht'(B,C,D,E) :- 
  F='mc__1_3_decons-ht'(B,C,D,E) , 
  ci( true, 
    'decons-ht', 
    3, 
    ['decons-ht',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_decons-ht'(B,C,D,E) :-  
  'mc__1_3_decons-ht'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_eval-string'(B,C) :- 
  D='mc__1_1_eval-string'(B,C) , 
  ci(true,'eval-string',1,['eval-string',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_eval-string'(B,C):-'mc__1_1_eval-string'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_eval-in-only'(B,C) :- 
  D='mc__1_1_eval-in-only'(B,C) , 
  ci(true,'eval-in-only',1,['eval-in-only',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_eval-in-only'(B,C):-'mc__1_1_eval-in-only'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_MettaMorph-If'(B,C,D) :- 
  E='mc__1_2_MettaMorph-If'(B,C,D) , 
  ci(true,'MettaMorph-If',2,['MettaMorph-If',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_MettaMorph-If'(B,C,D):-'mc__1_2_MettaMorph-If'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_MettaMorph-If'(B,C,D,E) :- 
  F='mc__1_3_MettaMorph-If'(B,C,D,E) , 
  ci( true, 
    'MettaMorph-If', 
    3, 
    ['MettaMorph-If',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_MettaMorph-If'(B,C,D,E) :-  
  'mc__1_3_MettaMorph-If'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_if-unify-or-empty'(B,C,D) :- 
  E='mc__1_2_if-unify-or-empty'(B,C,D) , 
  ci( true, 
    'if-unify-or-empty', 
    2, 
    ['if-unify-or-empty',B,C], D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_if-unify-or-empty'(B,C,D) :-  
  'mc__1_2_if-unify-or-empty'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_min-atom'(B,C) :- 
  D='mc__1_1_min-atom'(B,C) , 
  ci(true,'min-atom',1,['min-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_min-atom'(B,C):-'mc__1_1_min-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_pow-math'(B,C,D) :- 
  E='mc__1_2_pow-math'(B,C,D) , 
  ci(true,'pow-math',2,['pow-math',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_pow-math'(B,C,D):-'mc__1_2_pow-math'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_id(B,C) :- 
  D=mc__1_1_id(B,C) , 
  ci(true,id,1,[id,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_id(B,C):-mc__1_1_id(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_atom-subst'(B,C,D,E) :- 
  F='mc__1_3_atom-subst'(B,C,D,E) , 
  ci( true, 
    'atom-subst', 
    3, 
    ['atom-subst',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_atom-subst'(B,C,D,E) :-  
  'mc__1_3_atom-subst'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_if-error'(B,C,D,E) :- 
  F='mc__1_3_if-error'(B,C,D,E) , 
  ci( true, 
    'if-error', 
    3, 
    ['if-error',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_if-error'(B,C,D,E) :-  
  'mc__1_3_if-error'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_return-on-error'(B,C,D) :- 
  E='mc__1_2_return-on-error'(B,C,D) , 
  ci(true,'return-on-error',2,['return-on-error',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_return-on-error'(B,C,D):-'mc__1_2_return-on-error'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_2_switch(B,C,D) :- 
  E=mc__1_2_switch(B,C,D) , 
  ci(true,switch,2,[switch,B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_2_switch(B,C,D):-mc__1_2_switch(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_is-function'(B,C) :- 
  D='mc__1_1_is-function'(B,C) , 
  ci(true,'is-function',1,['is-function',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_is-function'(B,C):-'mc__1_1_is-function'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_4_match-types'(B,C,D,E,F) :- 
  G =  
    'mc__1_4_match-types'(B,C,D,E,F) , 
  ci( true, 
    'match-types', 
    4, 
    [ 'match-types', B,C,D,E], F,true,G).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_4_match-types'(B,C,D,E,F) :-  
  'mc__1_4_match-types'(B,C,D,E,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_first-from-pair'(B,C) :- 
  D='mc__1_1_first-from-pair'(B,C) , 
  ci(true,'first-from-pair',1,['first-from-pair',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_first-from-pair'(B,C):-'mc__1_1_first-from-pair'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_match-type-or'(B,C,D,E) :- 
  F='mc__1_3_match-type-or'(B,C,D,E) , 
  ci( true, 
    'match-type-or', 
    3, 
    ['match-type-or',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_match-type-or'(B,C,D,E) :-  
  'mc__1_3_match-type-or'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_filter-atom'(B,C,D,E) :- 
  F='mc__1_3_filter-atom'(B,C,D,E) , 
  ci( true, 
    'filter-atom', 
    3, 
    ['filter-atom',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_filter-atom'(B,C,D,E) :-  
  'mc__1_3_filter-atom'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_map-atom'(B,C,D,E) :- 
  F='mc__1_3_map-atom'(B,C,D,E) , 
  ci( true, 
    'map-atom', 
    3, 
    ['map-atom',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_map-atom'(B,C,D,E) :-  
  'mc__1_3_map-atom'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_5_foldl-atom'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_foldl-atom'(B,C,D,E,F,G) , 
  ci( true, 
    'foldl-atom', 
    5, 
    [ 'foldl-atom', B,C,D,E, 
      F], G,true,H).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_5_foldl-atom'(B,C,D,E,F,G) :-  
  'mc__1_5_foldl-atom'(B,C,D,E,F,G).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_separate-errors'(B,C,D) :- 
  E='mc__1_2_separate-errors'(B,C,D) , 
  ci(true,'separate-errors',2,['separate-errors',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_separate-errors'(B,C,D):-'mc__1_2_separate-errors'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_check-alternatives'(B,C) :- 
  D='mc__1_1_check-alternatives'(B,C) , 
  ci(true,'check-alternatives',1,['check-alternatives',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_check-alternatives'(B,C):-'mc__1_1_check-alternatives'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_3_interpret(B,C,D,E) :- 
  F=mc__1_3_interpret(B,C,D,E) , 
  ci(true,interpret,3,[interpret,B,C,D],E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_3_interpret(B,C,D,E) :-  
  mc__1_3_interpret(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_interpret-expression'(B,C,D,E) :- 
  F='mc__1_3_interpret-expression'(B,C,D,E) , 
  ci( true, 
    'interpret-expression', 
    3, 
    ['interpret-expression',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_interpret-expression'(B,C,D,E) :-  
  'mc__1_3_interpret-expression'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_4_interpret-func'(B,C,D,E,F) :- 
  G =  
    'mc__1_4_interpret-func'(B,C,D,E,F) , 
  ci( true, 
    'interpret-func', 
    4, 
    [ 'interpret-func', B,C,D,E], F,true,G).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_4_interpret-func'(B,C,D,E,F) :-  
  'mc__1_4_interpret-func'(B,C,D,E,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_5_interpret-args'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_interpret-args'(B,C,D,E,F,G) , 
  ci( true, 
    'interpret-args', 
    5, 
    [ 'interpret-args', B,C,D,E, 
      F], G,true,H).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_5_interpret-args'(B,C,D,E,F,G) :-  
  'mc__1_5_interpret-args'(B,C,D,E,F,G).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_6_interpret-args-tail'(B,C,D,E,F,G,H) :- 
  I =  
    'mc__1_6_interpret-args-tail'(B,C,D,E,F,G,H) , 
  ci( true, 
    'interpret-args-tail', 
    6, 
    [ 'interpret-args-tail', B,C,D,E, 
      F,G], H,true,I).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_6_interpret-args-tail'(B,C,D,E,F,G,H) :-  
  'mc__1_6_interpret-args-tail'(B,C,D,E,F,G,H).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_interpret-tuple'(B,C,D) :- 
  E='mc__1_2_interpret-tuple'(B,C,D) , 
  ci(true,'interpret-tuple',2,['interpret-tuple',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_interpret-tuple'(B,C,D):-'mc__1_2_interpret-tuple'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_metta-call'(B,C,D,E) :- 
  F='mc__1_3_metta-call'(B,C,D,E) , 
  ci( true, 
    'metta-call', 
    3, 
    ['metta-call',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_metta-call'(B,C,D,E) :-  
  'mc__1_3_metta-call'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_is-function-type'(B,C) :- 
  D='mc__1_1_is-function-type'(B,C) , 
  ci(true,'is-function-type',1,['is-function-type',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_is-function-type'(B,C):-'mc__1_1_is-function-type'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_add-reduct-minimal'(B,C,D) :- 
  E='mc__1_2_add-reduct-minimal'(B,C,D) , 
  ci( true, 
    'add-reduct-minimal', 
    2, 
    ['add-reduct-minimal',B,C], D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_add-reduct-minimal'(B,C,D) :-  
  'mc__1_2_add-reduct-minimal'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_add-reduct'(B,C,D) :- 
  E='mc__1_2_add-reduct'(B,C,D) , 
  ci(true,'add-reduct',2,['add-reduct',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_add-reduct'(B,C,D):-'mc__1_2_add-reduct'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


mi__1_1_unquote(B,C) :- 
  D=mc__1_1_unquote(B,C) , 
  ci(true,unquote,1,[unquote,B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


me__1_1_unquote(B,C):-mc__1_1_unquote(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_get-doc'(B,C) :- 
  D='mc__1_1_get-doc'(B,C) , 
  ci(true,'get-doc',1,['get-doc',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_get-doc'(B,C):-'mc__1_1_get-doc'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_metta-get-doc'(B,C) :- 
  D='mc__1_1_metta-get-doc'(B,C) , 
  ci(true,'metta-get-doc',1,['metta-get-doc',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_metta-get-doc'(B,C):-'mc__1_1_metta-get-doc'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_get-doc-single-atom'(B,C) :- 
  D='mc__1_1_get-doc-single-atom'(B,C) , 
  ci(true,'get-doc-single-atom',1,['get-doc-single-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_get-doc-single-atom'(B,C):-'mc__1_1_get-doc-single-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_get-doc-function'(B,C,D) :- 
  E='mc__1_2_get-doc-function'(B,C,D) , 
  ci(true,'get-doc-function',2,['get-doc-function',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_get-doc-function'(B,C,D):-'mc__1_2_get-doc-function'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_undefined-doc-function-type'(B,C) :- 
  D='mc__1_1_undefined-doc-function-type'(B,C) , 
  ci( true, 
    'undefined-doc-function-type', 
    1, 
    ['undefined-doc-function-type',B], C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_undefined-doc-function-type'(B,C) :-  
  'mc__1_1_undefined-doc-function-type'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_get-doc-params'(B,C,D,E) :- 
  F='mc__1_3_get-doc-params'(B,C,D,E) , 
  ci( true, 
    'get-doc-params', 
    3, 
    ['get-doc-params',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_get-doc-params'(B,C,D,E) :-  
  'mc__1_3_get-doc-params'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_get-doc-atom'(B,C) :- 
  D='mc__1_1_get-doc-atom'(B,C) , 
  ci(true,'get-doc-atom',1,['get-doc-atom',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_get-doc-atom'(B,C):-'mc__1_1_get-doc-atom'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_help!'(B,C) :- 
  D='mc__1_1_help!'(B,C) , 
  ci(true,'help!',1,['help!',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_help!'(B,C):-'mc__1_1_help!'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_metta-help!'(B,C) :- 
  D='mc__1_1_metta-help!'(B,C) , 
  ci(true,'metta-help!',1,['metta-help!',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_metta-help!'(B,C):-'mc__1_1_metta-help!'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_0_metta-help!'(B) :- 
  C='mc__1_0_metta-help!'(B) , 
  ci(true,'metta-help!',0,['metta-help!'],B,true,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_0_metta-help!'(B):-'mc__1_0_metta-help!'(B).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_help-param!'(B,C) :- 
  D='mc__1_1_help-param!'(B,C) , 
  ci(true,'help-param!',1,['help-param!',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_help-param!'(B,C):-'mc__1_1_help-param!'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_for-each-in-atom'(B,C,D) :- 
  E='mc__1_2_for-each-in-atom'(B,C,D) , 
  ci(true,'for-each-in-atom',2,['for-each-in-atom',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_for-each-in-atom'(B,C,D):-'mc__1_2_for-each-in-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_noreduce-eq'(B,C,D) :- 
  E='mc__1_2_noreduce-eq'(B,C,D) , 
  ci(true,'noreduce-eq',2,['noreduce-eq',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_noreduce-eq'(B,C,D):-'mc__1_2_noreduce-eq'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_get-type-space'(B,C,D) :- 
  E='mc__1_2_get-type-space'(B,C,D) , 
  ci(true,'get-type-space',2,['get-type-space',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_get-type-space'(B,C,D):-'mc__1_2_get-type-space'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_mod-space!'(B,C) :- 
  D='mc__1_1_mod-space!'(B,C) , 
  ci(true,'mod-space!',1,['mod-space!',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_mod-space!'(B,C):-'mc__1_1_mod-space!'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_union-atom'(B,C,D) :- 
  E='mc__1_2_union-atom'(B,C,D) , 
  ci(true,'union-atom',2,['union-atom',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_union-atom'(B,C,D):-'mc__1_2_union-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_intersection-atom'(B,C,D) :- 
  E='mc__1_2_intersection-atom'(B,C,D) , 
  ci( true, 
    'intersection-atom', 
    2, 
    ['intersection-atom',B,C], D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_intersection-atom'(B,C,D) :-  
  'mc__1_2_intersection-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_subtraction-atom'(B,C,D) :- 
  E='mc__1_2_subtraction-atom'(B,C,D) , 
  ci(true,'subtraction-atom',2,['subtraction-atom',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_subtraction-atom'(B,C,D):-'mc__1_2_subtraction-atom'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_listing!'(B,C) :- 
  D='mc__1_1_listing!'(B,C) , 
  ci(true,'listing!',1,['listing!',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_listing!'(B,C):-'mc__1_1_listing!'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_1_file-read'(B,C) :- 
  D='mc__1_1_file-read'(B,C) , 
  ci(true,'file-read',1,['file-read',B],C,true,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_1_file-read'(B,C):-'mc__1_1_file-read'(B,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_file-write'(B,C,D) :- 
  E='mc__1_2_file-write'(B,C,D) , 
  ci(true,'file-write',2,['file-write',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_file-write'(B,C,D):-'mc__1_2_file-write'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_5_if-decons-expr'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_if-decons-expr'(B,C,D,E,F,G) , 
  ci( true, 
    'if-decons-expr', 
    5, 
    [ 'if-decons-expr', B,C,D,E, 
      F], G,true,H).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_5_if-decons-expr'(B,C,D,E,F,G) :-  
  'mc__1_5_if-decons-expr'(B,C,D,E,F,G).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_add-reducts'(B,C,D) :- 
  E='mc__1_2_add-reducts'(B,C,D) , 
  ci(true,'add-reducts',2,['add-reducts',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_add-reducts'(B,C,D):-'mc__1_2_add-reducts'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_add-atoms'(B,C,D) :- 
  E='mc__1_2_add-atoms'(B,C,D) , 
  ci(true,'add-atoms',2,['add-atoms',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_add-atoms'(B,C,D):-'mc__1_2_add-atoms'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_0_help!'(B) :- 
  C='mc__1_0_help!'(B) , 
  ci(true,'help!',0,['help!'],B,true,C).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_0_help!'(B):-'mc__1_0_help!'(B).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_2_unique-atom-by'(B,C,D) :- 
  E='mc__1_2_unique-atom-by'(B,C,D) , 
  ci(true,'unique-atom-by',2,['unique-atom-by',B,C],D,true,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_2_unique-atom-by'(B,C,D):-'mc__1_2_unique-atom-by'(B,C,D).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_union-atom-by'(B,C,D,E) :- 
  F='mc__1_3_union-atom-by'(B,C,D,E) , 
  ci( true, 
    'union-atom-by', 
    3, 
    ['union-atom-by',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_union-atom-by'(B,C,D,E) :-  
  'mc__1_3_union-atom-by'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_intersection-atom-by'(B,C,D,E) :- 
  F='mc__1_3_intersection-atom-by'(B,C,D,E) , 
  ci( true, 
    'intersection-atom-by', 
    3, 
    ['intersection-atom-by',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_intersection-atom-by'(B,C,D,E) :-  
  'mc__1_3_intersection-atom-by'(B,C,D,E).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'mi__1_3_subtraction-atom-by'(B,C,D,E) :- 
  F='mc__1_3_subtraction-atom-by'(B,C,D,E) , 
  ci( true, 
    'subtraction-atom-by', 
    3, 
    ['subtraction-atom-by',B,C,D], E,true,F).


 */
optimized_code(fa('subtraction-atom-by',3),ca).
/*


'me__1_3_subtraction-atom-by'(B,C,D,E) :-  
  'mc__1_3_subtraction-atom-by'(B,C,D,E).


 */
