% ==============================
% MeTTa to Prolog transpilation (which uses the Host SWI-Prolog compiler)
% Aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================

% Setting the file encoding to ISO-Latin-1
:- encoding(iso_latin_1).
% Flushing the current output
:- flush_output.
% Setting the Rust backtrace to Full
:- setenv('RUST_BACKTRACE',full).
% Loading various library files
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
% =======================================
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(flybase_main).
% =======================================
%:- set_option_value(encoding,utf8).

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).

:- op(700,xfx,'=~').

compound_non_cons(B):-  compound(B),  \+ B = [_|_].
iz_conz(B):- compound(B), B=[_|_].

'=~'(A,B):- var(A),iz_conz(B),!,A=B.
'=~'(A,B):- iz_conz(A),var(B),!,A=B.
'=~'(A,B):- iz_conz(A),iz_conz(B),!,A=B.
'=~'(A,B):- compound_non_cons(A),var(B),!,compound_name_arguments(A,FA,Args),B=[FA|Args].
'=~'(A,B):- compound_non_cons(A),!,into_list_args(A,AA),!,'=~'(AA,B).
'=~'(A,B):- compound_non_cons(B),!,into_list_args(B,BB),!,'=~'(A,BB).

'=~'(A,B):- '=..'(A,B).

%into_list_args(A,AA):- is_ftVar(A),AA=A.
%into_list_args(C,[C]):- \+ compound(C),!.
into_list_args(C,C):- \+ compound(C),!.
into_list_args(A,AA):- is_ftVar(A),AA=A.
into_list_args([H|T],[H|T]):- \+ is_list(T),!.
into_list_args([H,List,A],HT):- H == u_assign,!,append(List,[A],HT),!.
into_list_args([H|T],[H|T]):- is_list(T),!.
into_list_args(u_assign(List, A),[H|T]):- append(List,[A],[H|T]),!.
into_list_args(holds(A),AA):- !, into_list_args(A,AA),!.
into_list_args(C,FArgs):- compound_name_arguments(C,F,Args),!,into_list_args([F|Args],FArgs).



compound_name_list(AsPred,FP,PredArgs):- var(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- iz_conz(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- into_list_args(AsPred,[FP|PredArgs]),!.
compound_name_list(AsPred,FP,PredArgs):- compound_non_cons(AsPred),!,compound_name_arguments(AsPred,FP,PredArgs).
% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% ===============================


% Conversion is possible between a function and a predicate of arity when the result is at the nth arg
:- dynamic decl_functional_predicate_arg/3.

% Converion is possible between a  function and predicate is tricky
functional_predicate_arg_tricky(is, 2, 1). % E.g. eval(is(+(1,2)),Result) converts to is(Result,+(1,2)).
% Defining standard mappings for some common functions/predicates
decl_functional_predicate_arg(append, 3, 3).
decl_functional_predicate_arg(+, 3, 3).
decl_functional_predicate_arg(pi, 1, 1).
decl_functional_predicate_arg('Empty', 1, 1).
decl_functional_predicate_arg(call,4,4).
decl_functional_predicate_arg(eval, 2, 2).
decl_functional_predicate_arg(edge, 2, 2).
decl_functional_predicate_arg('==', 2, 2).
decl_functional_predicate_arg('is-same', 2, 2).
decl_functional_predicate_arg(assertTrue, 2, 2).
decl_functional_predicate_arg(case, 3, 3).
decl_functional_predicate_arg(assertFalse, 2, 2).
decl_functional_predicate_arg(match,4,4).
decl_functional_predicate_arg('TupleConcat',3,3).
decl_functional_predicate_arg('new-space',1,1).

decl_functional_predicate_arg(superpose, 2, 2).

do_predicate_function_canonical(F,FF):- predicate_function_canonical(F,FF),!.
do_predicate_function_canonical(F,F).
predicate_function_canonical(is_Empty,'Empty').

% Mapping any current predicate F/A to a function, if it's not tricky
functional_predicate_arg(F, A, L):- decl_functional_predicate_arg(F, A, L).
functional_predicate_arg(F, A, L):- (atom(F)->true;trace), predicate_arity(F,A),
  \+ functional_predicate_arg_tricky(F,A,_), L=A,
  \+ decl_functional_predicate_arg(F, A, _).
functional_predicate_arg(F, A, L):- functional_predicate_arg_tricky(F, A, L).

predicate_arity(F,A):- metta_atom('&self',[:,F,[->|Args]]), length(Args,A).
predicate_arity(F,A):- current_predicate(F/A).
% Certain constructs should not be converted to functions.
not_function(P):- atom(P),!,not_function(P,0).
not_function(P):- callable(P),!,functor(P,F,A),not_function(F,A).
not_function(F,A):- is_arity_0(F,FF),!,not_function(FF,A).
not_function(!,0).
not_function(print,1).
not_function((':-'),2).
not_function((','),2).
not_function((';'),2).
not_function(('='),2).
not_function(('or'),2).

not_function('a',0).
not_function('b',0).
not_function(F,A):- is_control_structure(F,A).
not_function(A,0):- atom(A),!.
not_function('True',0).
not_function(F,A):- predicate_arity(F,A),AA is A+1, \+ decl_functional_predicate_arg(F,AA,_).

needs_call_fr(P):- is_function(P,_Nth),functor(P,F,A),AA is A+1, \+ current_predicate(F/AA).

is_control_structure(F,A):- atom(F), atom_concat('if-',_,F),A>2.

'=='(A, B, Res):- as_tf(equal_enough(A, B),Res).
'or'(G1,G2):- once((G1 ; G2)).
'or'(G1,G2,Res):- as_tf((G1 ; G2),Res).

% Function without arguments can be converted directly.
is_arity_0(AsFunction,F):- compound(AsFunction), compound_name_arity(AsFunction,F,0).

% Determines whether a given term is a function and retrieves the position
% in the predicate where the function Result is stored/retrieved
is_function(AsFunction, _):- is_ftVar(AsFunction),!,fail.
is_function(AsFunction, _):- AsFunction=='$VAR',!, trace, fail.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction,F), \+ not_function(F,0), !,Nth=1.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction,_), !,Nth=1.
is_function(AsFunction, Nth) :-
    callable(AsFunction),
    functor(AsFunction, Functor, A),
    \+ not_function(Functor, A),
    AA is A + 1,
    functional_predicate_arg_maybe(Functor, AA, Nth).

functional_predicate_arg_maybe(F, AA, Nth):- functional_predicate_arg(F, AA, Nth),!.
functional_predicate_arg_maybe(F, AA, _):- A is AA - 1,functional_predicate_arg(F,A,_),!,fail.
functional_predicate_arg_maybe(F, Nth, Nth):- asserta(decl_functional_predicate_arg(F, Nth, Nth)),!.

% --------------------------------
%    FUNCTS_TO_PREDS EXPLANATION
% --------------------------------

% functs_to_preds is a predicate that converts all Term functions to their equivalent predicates.
% It takes three arguments - RetResult, which will hold the result of the function evaluation,
% Convert, which is the function that needs to be converted, and Converted, which will hold the equivalent predicate.
% Example:
%
%     ?- functs_to_preds(RetResult, is(pi+pi), Converted).
%
%     Converted =  (pi(_A),
%                   +(_A, _A, _B),
%                   _C is _B,
%                   eval(_C, RetResult)).
%

functs_to_preds([Eq,H,B],OO):- Eq == '=', compile_for_assert(H, B, OO),!.
functs_to_preds(EqHB,OO):- compile_head_for_assert(EqHB,OO),!.

functs_to_preds(I,OO):-
   sexpr_s2p(I, M),
   /*trace,*/
   f2p(_,_,M,O),
   expand_to_hb(O,H,B),
   head_preconds_into_body(H,B,HH,BB),
   OO = (HH:-BB).


% ?- compile_for_exec(RetResult, is(pi+pi), Converted).
compile_for_exec(Res,I,BB):-
   HeadIs = [exec],
   AsBodyFn = I,
   compile_for_assert(HeadIs, AsBodyFn, Converted),
   (Converted = (exec(Res):- BB)),!,
   (portray_clause((Converted))),!.


compile_for_exec(Res,I,BB):-
   =(I, M),
   f2p(exec(),_,(exec()=M),O),
   expand_to_hb(O,H,B),
   head_preconds_into_body(H,B,HH,BB),
   ignore(exec(Res)=HH),
   (portray_clause((exec2(Res):- BB))),!.

compile_for_exec(Res,I,O):-
  =(I, M), f2p(exec(),Res,M,O),
  (portray_clause((exec1(Res):- O))),!.


% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_head_for_assert(HeadIs, (Head:-Body)):-
  compile_head_for_assert(HeadIs, NewHeadIs,Converted),
  head_preconds_into_body(NewHeadIs,Converted,Head,Body).

compile_head_for_assert(HeadIs, NewHeadIs,Converted) :- /*trace,*/
   as_functor_args(HeadIs,F,A,ArgsL),
   maplist(f2p_assign(HeadIs),NewArgs,ArgsL,CodeForValueL),
   as_functor_args(NewHeadIs,F,A,NewArgs),
   list_to_conjuncts(CodeForValueL,Converted),!.


as_functor_args(AsPred,F,A,ArgsL):- nonvar(AsPred),!,into_list_args(AsPred,[F|ArgsL]),length(ArgsL,A).
as_functor_args(AsPred,F,A,ArgsL):- nonvar(F),length(ArgsL,A),AsPred =~ [F|ArgsL].

compile_for_assert(HeadIs, AsBodyFn, Converted) :-
     (AsBodyFn =@= HeadIs ; AsBodyFn == []), !,/*trace,*/
     compile_head_for_assert(HeadIs,Converted).

compile_for_assert(HeadIs, AsBodyFn, Converted) :- is_ftVar(AsBodyFn), /*trace,*/
     AsFunction = HeadIs,!,
     must_det_ll((
     Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
     %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
     f2p(HeadIs,HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
       funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
     NextBody = u_assign(AsBodyFn,Result),
     head_preconds_into_body(Head,NextBody,HeadC,BodyC),
     nop(ignore(Result = '$VAR'('HeadRes'))))),!.

compile_for_assert(HeadIs, AsBodyFn, Converted) :-
     AsFunction = HeadIs,
     must_det_ll((
     Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
     %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
     f2p(HeadIs,HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
        funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
     verbose_unify(Convert),
     f2p(HeadIs,Result,AsBodyFn,NextBody),
     %RetResult = Converted,
     %RetResult = _,
     head_preconds_into_body(Head,NextBody,HeadC,NextBodyC),
     fbug([convert(Convert),head_preconds_into_body(HeadC:-NextBodyC)]),
     %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),

     nop(ignore(Result = '$VAR'('HeadRes'))))),!.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   as_functor_args(Head,F,A,ArgsL),
   maplist(f2p_assign(HeadIs),NewArgs,ArgsL,CodeForValueL),
   as_functor_args(HeadC,F,A,NewArgs),
   list_to_conjuncts(CodeForValueL,CodeForHeadArgs),
   f2p(HeadIs,Result,AsBodyFn,NextBody),
   combine_code(CodeForHeadArgs,NextBody,BodyC),!,
   head_preconds_into_body(HeadC,BodyC,HeadCC,BodyCC),!.


/*
*/

head_preconds_into_body(Head,Body,Head,Body):- \+ compound(Head),!.
head_preconds_into_body((PreHead,True),Converted,Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body((True,PreHead),Converted,Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body(PreHead,(True,Converted),Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body(PreHead,(Converted,True),Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body((AsPredO,Pre),Converted,Head,Body):-
  head_preconds_into_body(Pre,(AsPredO,Converted),Head,Body).
head_preconds_into_body(AHead,Body,Head,BodyNew):-
    assertable_head(AHead,Head),
    optimize_body(Head,Body,BodyNew).

assertable_head(u_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), atom(F),!, Head=..[F|NewArgs].
assertable_head(Head,Head).

optimize_body(_Head,Body,BodyNew):- var(Body),!,Body=BodyNew.
optimize_body(Head,(B1*->B2;B3),(BN1*->BN2;BN3)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2), optimize_body(Head,B3,BN3).
optimize_body(Head,(B1->B2;B3),(BN1->BN2;BN3)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2), optimize_body(Head,B3,BN3).
optimize_body(Head,(B1,B2),(BN1)):- B2==true,!, optimize_body(Head,B1,BN1).
optimize_body(Head,(B2,B1),(BN1)):- B2==true,!, optimize_body(Head,B1,BN1).
optimize_body(Head,(B1,B2),(BN1,BN2)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).
optimize_body(Head,(B1:-B2),(BN1:-BN2)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).
optimize_body(Head,(B1;B2),(BN1;BN2)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).
optimize_body(_Head,Body,BodyNew):- Body=BodyNew.


compile_test_then_else(RetResult,If,Then,Else,Converted):-
  f2p(HeadIs,ThenResult,Then,ThenCode),
  f2p(HeadIs,ElseResult,Else,ElseCode),
  Converted=(If*->(ThenCode,ThenResult=RetResult);(ElseCode,ElseResult=RetResult)).

:- discontiguous(compile_flow_control/4).

compile_flow_control(_HeadIs,_RetResult,Convert,_):- \+ compound(Convert),!,fail.
compile_flow_control(_HeadIs,_RetResult,Convert,_):- compound_name_arity(Convert,_,0),!,fail.
compile_flow_control(HeadIs,RetResult,Convert, (Code1,Eval1Result=Result,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ chain(Eval1,Result,Eval2),!,
   f2p(HeadIs,Eval1Result,Eval1,Code1),
   f2p(HeadIs,RetResult,Eval2,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ if(Cond,Then,Else),!,Test = is_True(CondResult),
  f2p(HeadIs,CondResult,Cond,CondCode),
  compile_test_then_else(RetResult,(CondCode,Test),Then,Else,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-error'(Value,Then,Else),!,Test = is_Error(ValueResult),
  f2p(HeadIs,ValueResult,Value,ValueCode),
  compile_test_then_else(RetResult,(ValueCode,Test),Then,Else,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-empty'(Value,Then,Else),!,Test = is_Empty(ValueResult),
  f2p(HeadIs,ValueResult,Value,ValueCode),
  compile_test_then_else(RetResult,(ValueCode,Test),Then,Else,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  (Convert =~ 'if-non-empty-expression'(Value,Then,Else)),!,
  (Test = ( \+ is_Empty(ValueResult))),
  f2p(HeadIs,ValueResult,Value,ValueCode),
  compile_test_then_else(RetResult,(ValueCode,Test),Then,Else,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ ['if-equals',Value1,Value2,Then,Else],!,Test = equal_enough(ResValue1,ResValue2),
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    f2p(HeadIs,ResValue2,Value2,CodeForValue2),
  compile_test_then_else(RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).

cname_var(Sym,Src):-  gensym(Sym,SrcV),Src='$VAR'(SrcV).
compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
    Convert =~ ['assertEqual',Value1,Value2],!,
    cname_var('Src_',Src),
    cname_var('FA_',ResValue1),
    cname_var('FA_',ResValue2),
    cname_var('FARL_',L1),
    cname_var('FARL_',L2),
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    f2p(HeadIs,ResValue2,Value2,CodeForValue2),
    Converted =
              (Src = Convert,
               loonit_assert_source_tf(Src,
                (findall(ResValue1,CodeForValue1,L1),
                 findall(ResValue2,CodeForValue2,L2)),
                 equal_enough(L1,L2),RetResult)).
compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
    Convert =~ ['assertEqualToResult',Value1,Value2],!,
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    Converted = loonit_assert_source_tf(Convert,
                findall(ResValue1,CodeForValue1,L1),
                 equal_enough(L1,Value2),RetResult).


compile_flow_control(_HeadIs,RetResult,Convert, Converted) :-
     Convert =~ 'add-atom'(Where,What), !,
     =(What,WhatP),
     Converted = as_tf('add-atom'(Where,WhatP),RetResult).

compile_flow_control(_HeadIs,RetResult,Convert, Converted) :-
     Convert =~ 'add-atom'(Where,What,RetResult), !,
     =(What,WhatP),
     Converted = as_tf('add-atom'(Where,WhatP),RetResult).


compile_flow_control(HeadIs,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',UValueL],
    maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    maplist(f2p_assign(HeadIs,URetResult),ValueL,CodeForValueL),
    list_to_disjuncts(CodeForValueL,Converted),!.


maybe_unlistify([UValueL],ValueL,RetResult,[URetResult]):- is_list(UValueL),!,
  maybe_unlistify(UValueL,ValueL,RetResult,URetResult).
maybe_unlistify(ValueL,ValueL,RetResult,RetResult).

list_to_disjuncts([],false).
list_to_disjuncts([A],A):- !.
list_to_disjuncts([A|L],(A;D)):-  list_to_disjuncts(L,D).


f2p_assign(_HeadIs,V,Value,is_True(V)):- Value=='True'.
f2p_assign(_HeadIs,ValueR,Value,ValueR=Value):- \+ compound(Value),!.
f2p_assign(_HeadIs,ValueR,Value,ValueR=Value):- is_ftVar(Value),!.
f2p_assign(HeadIs,ValueResult,Value,Converted):-
   f2p(HeadIs,ValueResultR,Value,CodeForValue),
   %into_equals(ValueResultR,ValueResult,ValueResultRValueResult),
   ValueResultRValueResult = (ValueResultR=ValueResult),
   combine_code(CodeForValue,ValueResultRValueResult,Converted).

compile_flow_control(HeadIs,RetResult,Convert, (ValueCode, Converted)) :-
  Convert =~ ['case',Value|Options],
  \+ is_ftVar(Value),!,
  cname_var('CASE_EVAL_',ValueResult),
  ConvertCases =~ ['case',ValueResult|Options],
  compile_flow_control(HeadIs,RetResult,ConvertCases, Converted),
  f2p(HeadIs,ValueResult,Value,ValueCode).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,[Opt|Options]],nonvar(Opt),!,
   must_det_ll((
    compile_case_bodies(HeadIs,Opt,caseStruct(Value,If,RetResult,Then)),
    Converted = ( If -> Then ; Else ),
    ConvertCases =~ ['case',Value,Options],
    compile_flow_control(HeadIs,RetResult,ConvertCases,Else))).

compile_flow_control(_HeadIs,RetResult,Convert, Converted) :-
  Convert =~ ['case',_Value,[]],!,Converted = (fail,RetResult=[]),!.

/*
compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,Options],!,
   must_det_ll((
    maplist(compile_case_bodies(HeadIs),Options,Cases),
    Converted =
        (( AllCases = Cases,
           once((member(caseStruct(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
                 (MatchCode,unify_enough(Value,MatchVar)))),
           (BodyCode),
           BodyResult=RetResult)))).

compile_flow_control(HeadIs,_,Convert, Converted) :-
  Convert =~ ['case',Value,Options,RetResult],!,
   must_det_ll((
    f2p(HeadIs,ValueResult,Value,ValueCode),
    maplist(compile_case_bodies(HeadIs),Options,Cases),
    Converted =
        (( AllCases = Cases,
           call(ValueCode),
           once((member(caseStruct(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
                 both_of(ValueResult,MatchCode,unify_enough(ValueResult,MatchVar)))),
           call(BodyCode),
           BodyResult=RetResult)))).


both_of(Var,G1,G2):- nonvar(Var),!,call(G2),call(G1).
both_of(_Var,G1,G2):- call(G1),call(G2).

*/

compile_case_bodies(HeadIs,[Match,Body],caseStruct(_,true,BodyResult,BodyCode)):- Match == '%void%',!,
      f2p(HeadIs,BodyResult,Body,BodyCode).
compile_case_bodies(HeadIs,[Match,Body],caseStruct(MatchResult,If,BodyResult,BodyCode)):- !,
      f2p(HeadIs,MatchResultV,Match,MatchCode),
      combine_code(MatchCode,unify_enough(MatchResult,MatchResultV),If),
      f2p(HeadIs,BodyResult,Body,BodyCode).
compile_case_bodies(HeadIs,MatchBody,CS):- compound(MatchBody), MatchBody =~ MB,compile_case_bodies(HeadIs,MB,CS).

compile_flow_control(HeadIs,RetResult,Convert,CodeForValueConverted) :-
    Convert =~ [Plus,N,Value], atom(Plus), current_predicate(Plus/3), number(N), \+ number(Value), \+ is_ftVar(Value),!,
    f2p(HeadIs,ValueResult,Value,CodeForValue),!,
    Converted =.. [Plus,N,ValueResult,RetResult],
    combine_code(CodeForValue,Converted,CodeForValueConverted).

compound_equals(COL1,COL2):- COL1=@=COL2,!,COL1=COL2.
compound_equals(COL1,COL2):- compound_equals1(COL1,COL2).
compound_equals1(COL1,COL2):- is_ftVar(COL1),!,is_ftVar(COL2),ignore(COL1=COL2),!.
compound_equals1(COL1,COL2):- compound(COL1),!,compound(COL2), COL1=COL2.

compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
    Convert =~ ['superpose',COL],compound_equals(COL,'collapse'(Value1)),
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    Converted = (findall(ResValue1,CodeForValue1,Gathered),member(RetResult,Gathered)).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
    Convert =~ ['collapse',Value1],!,
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    Converted = (findall(ResValue1,CodeForValue1,RetResult)).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
    Convert =~ ['compose',Value1],!,
    Convert2 =~ ['collapse',Value1],!,
    compile_flow_control(HeadIs,RetResult,Convert2, Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['unify',Value1,Value2,Then,Else],!,Test = metta_unify(ResValue1,ResValue2),
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    f2p(HeadIs,ResValue2,Value2,CodeForValue2),
  compile_test_then_else(RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['let',Var,Value1,Body],!,
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    f2p(HeadIs,RetResult,Body,BodyCode),
  into_equals(Var,ResValue1,VarResValue1),
  list_to_conjuncts([CodeForValue1,VarResValue1,BodyCode],Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ ['let*',Bindings,Body],!,
   must_det_ll((
    maplist(compile_let_star(HeadIs),Bindings,CodeList),
    list_to_conjuncts(CodeList,BindingCode),
    f2p(HeadIs,RetResult,Body,BodyCode),
    combine_code(BindingCode,BodyCode,Converted))).

  compile_let_star(HeadIs,NV,Converted):-
     must_det_ll((NV =~ [Expression,Var],
     (var(Var)-> f2p(HeadIs,Var,Expression,Converted);
     (var(Expression)-> f2p(HeadIs,Expression,Var,Converted);
     (f2p(HeadIs,Eval1Result,Expression,Code),
      into_equals(Eval1Result,Var,Eval1ResultVar),
      combine_code(Code,Eval1ResultVar,Converted)))))).



% match('&self',f(1)=Y,Y)
compile_flow_control(HeadIs,Y,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert=~ match('&self',AsFunctionY,YY), nonvar(AsFunctionY),( AsFunctionY =~ (AsFunction=Y)), nonvar(AsFunction),
    !, Y==YY,
    f2p(HeadIs,Y,AsFunction,Converted),!.

compile_flow_control(HeadIs,RetResult,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ ['match',_Self,Pattern,Template],!,
    f2p(HeadIs,_,Pattern,PatternCode),
    f2p(HeadIs,RetResult,Template,TemplateCode),
    combine_code(PatternCode,TemplateCode,Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ ['match',_Self,Pattern,Template],!,
   must_det_ll((
    f2p(HeadIs,_,Pattern,PatternCode),
    into_equals(RetResult,Template,TemplateCode),
    combine_code(PatternCode,TemplateCode,Converted))).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
   Convert =~ ['if-decons',Atom,Head,Tail,Then,Else],!,Test = unify_cons(AtomResult,ResHead,ResTail),
    f2p(HeadIs,AtomResult,Atom,AtomCode),
    f2p(HeadIs,ResHead,Head,CodeForHead),
    f2p(HeadIs,ResTail,Tail,CodeForTail),
    compile_test_then_else(RetResult,(AtomCode,CodeForHead,CodeForTail,Test),Then,Else,Converted).



compile_flow_control(_HeadIs,RetResult,Convert,is_True(RetResult)) :- is_compiled_and(AND),
   Convert =~ [AND],!.

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body],!,
   f2p(HeadIs,RetResult,Body,BodyCode),
    compile_test_then_else(RetResult,BodyCode,'True','False',Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(HeadIs,B1Res,Body1,Body1Code),
   f2p(HeadIs,RetResult,Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   Converted = (Body1Code,AE,Body2Code),!.


compile_flow_control(HeadIs,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(HeadIs,B1Res,Body1,Body1Code),
   f2p(HeadIs,_,Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   compile_test_then_else(RetResult,(Body1Code,AE,Body2Code),'True','False',Converted).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2|BodyMore],!,
   And2 =~ [AND,Body2|BodyMore],
   Next =~ [AND,Body1,And2],
   compile_flow_control(HeadIs,RetResult, Next, Converted).

compile_flow_control(HeadIs,RetResult,sequential(Convert), Converted) :- !,
   compile_flow_control(HeadIs,RetResult,transpose(Convert), Converted).

compile_flow_control(HeadIs,RetResult,transpose(Convert), Converted,Code) :- !,
   maplist(each_result(HeadIs,RetResult),Convert, Converted),
   list_to_disjuncts(Converted,Code).


compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ if(Cond,Then),!,
   f2p(HeadIs,CondResult,Cond,CondCode),
   f2p(HeadIs,RetResult,Then,ThenCode),
   Converted = ((CondCode,is_True(CondResult)),ThenCode).

each_result(HeadIs,RetResult,Convert,Converted):-
   f2p(HeadIs,OneResult,Convert,Code1),
   into_equals(OneResult,RetResult,Code2),
   combine_code(Code1,Code2,Converted).

compile_flow_control(HeadIs,RetResult,Converter, Converted):- de_eval(Converter,Convert),!,
   compile_flow_control(HeadIs,RetResult,Convert, Converted).

compile_flow_control(HeadIs,_Result,Convert, Converted) :- fail,
   functor(Convert,Func,PA),
   functional_predicate_arg(Func,PA,Nth),
   Convert =~ [Func|PredArgs],
   nth1(Nth,PredArgs,Result,FuncArgs),
   RetResult = Result,
   AsFunct =~ [Func|FuncArgs],
   compile_flow_control(HeadIs,RetResult,AsFunct, Converted).

dif_functors(HeadIs,_):- var(HeadIs),!,fail.
dif_functors(HeadIs,_):- \+ compound(HeadIs),!.
dif_functors(HeadIs,Convert):- compound(HeadIs),compound(Convert),
  compound_name_arity(HeadIs,F,A),compound_name_arity(Convert,F,A).

is_compiled_and(AND):- member(AND,[ (','), ('and')]).

flowc.


 :- discontiguous f2p/4.

f2p(_HeadIs,Convert, Convert, true) :-
     (is_ftVar(Convert);number(Convert)),!.% Check if Convert is a variable

% If Convert is a variable, the corresponding predicate is just eval(Convert, RetResult)
f2p(_HeadIs,RetResult,Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = eval(Convert, RetResult).  % Set Converted to eval(Convert, RetResult)
f2p(_HeadIs,RetResult,Convert, RetResultConverted) :-
     number(Convert),!,into_equals(RetResult,Convert,RetResultConverted).

f2p(_HeadIs,RetResult,Convert, Converted) :- % HeadIs\=@=Convert,
     is_arity_0(Convert,F), !, Converted = u_assign([F],RetResult),!.



/*f2p(HeadIs,RetResult, ConvertL, (Converted,RetResultL=RetResult)) :- is_list(ConvertL),
   maplist(f2p_assign(HeadIs),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Converted).*/

% If Convert is an "eval" function, we convert it to the equivalent "is" predicate.
f2p(HeadIs,RetResult,EvalConvert,Converted):- EvalConvert =~ eval(Convert),  !,
  must_det_ll((f2p(HeadIs,RetResult,Convert, Converted))).

f2p(HeadIs,RetResult,Convert, Converted):-
    compound(Convert), \+ compound_name_arity(Convert,_,0),
    compile_flow_control(HeadIs,RetResult,Convert, Converted),!.

f2p(HeadIs,RetResult,Convert, Converted):-
    compound(Convert), Convert = u_assign(C, Var), compound_non_cons(C),into_list_args(C,CC),!,
    f2p(HeadIs,RetResult,u_assign(CC, Var), Converted).

f2p(_HeadIs,_RetResult,Convert, Converted):-
    compound(Convert), Convert = u_assign(C, _Var), is_list(C),Converted = Convert,!.

f2p(HeadIs,RetResult,Convert, Converted) :-
     atom(Convert),  functional_predicate_arg(Convert,Nth,Nth2),
      Nth==1,Nth2==1,
      HeadIs\=@=Convert,
      Convert = F,!,
      must_det_ll((
        do_predicate_function_canonical(FP,F),
        compound_name_list(Converted,FP,[RetResult]))).

% If Convert is a number or an atom, it is considered as already converted.
f2p(_HeadIs,RetResult, Convert, RetResult = Convert) :- % HeadIs\=@=Convert,
    once(number(Convert); atom(Convert); data_term(Convert)),  % Check if Convert is a number or an atom
    !.  % Set RetResult to Convert as it is already in predicate form

% If Convert is an "is" function, we convert it to the equivalent "is" predicate.
f2p(HeadIs,RetResult,is(Convert),(Converted,is(RetResult,Result))):- !,
   must_det_ll((f2p(HeadIs,Result,Convert, Converted))).

% If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
f2p(HeadIs,RetResult,or(AsPredI,Convert), or(AsPredO, Converted)) :- !,
  must_det_ll((f2p(HeadIs,RetResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))).
f2p(HeadIs,RetResult,(AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_ll((f2p(HeadIs,RetResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))).

% If Convert is a "," (and) function, we convert it to the equivalent "," (and) predicate.
f2p(HeadIs,RetResult,(AsPredI, Convert), (AsPredO, Converted)) :- !,
  must_det_ll((f2p(HeadIs,_RtResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))).

% If Convert is a ":-" (if) function, we convert it to the equivalent ":-" (if) predicate.
f2p(_HeadIs,RetResult, Convert, Converted) :- Convert =(H:-B),!,
  RetResult=(H:-B), Converted = true.

f2p(_HeadIs,_RetResult, N=V, Code) :- !, into_equals(N,V,Code).


into_equals(RetResultL,RetResult,Equals):- into_u_assign(RetResultL,RetResult,Equals).
into_u_assign(RetResultL,RetResult,true):- is_ftVar(RetResultL), is_ftVar(RetResult), RetResult=RetResultL,!.
into_u_assign(RetResultL,RetResult,Code):- var(RetResultL), Code = u_assign(RetResult,RetResultL).
into_u_assign(RetResultL,RetResult,Code):- Code = u_assign(RetResultL,RetResult).



% If Convert is a list, we convert it to its termified form and then proceed with the functs_to_preds conversion.
f2p(HeadIs,RetResult,Convert, Converted) :- is_list(Convert),
   once((sexpr_s2p(Convert,IS), \+ IS=@=Convert)), !,  % Check if Convert is a list and not in predicate form
   must_det_ll((f2p(HeadIs,RetResult, IS, Converted))).  % Proceed with the conversion of the predicate form of the list.

f2p(HeadIs,RetResult, ConvertL, Converted) :- is_list(ConvertL),
   maplist(f2p_assign(HeadIs),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Conjs),
   into_u_assign(RetResultL,RetResult,Code),
   combine_code(Conjs,Code,Converted).



f2p(HeadIs,RetResultL, ConvertL, Converted) :- is_list(ConvertL),
   ConvertL = [Convert],
   f2p(HeadIs,RetResult,Convert, Code),!,
   into_equals(RetResultL,[RetResult],Equals),
   combine_code(Code,Equals,Converted).


% If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
f2p(HeadIs,RetResult,Convert, Converted) :-
    rev_sub_sterm(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
  %  sub_term(AsFunction, Convert), AsFunction\==Convert,
    callable(AsFunction),  % Check if AsFunction is callable
    compile_flow_control(HeadIs,Result,AsFunction, AsPred),
    HeadIs\=@=AsFunction,!,
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    f2p(HeadIs,RetResult,(AsPred,Converting), Converted).  % Proceed with the conversion of the remaining terms

% If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
f2p(HeadIs,RetResult,Convert, Converted) :-
    rev_sub_sterm(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
    callable(AsFunction),  % Check if AsFunction is callable
    is_function(AsFunction, Nth),  % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
    HeadIs\=@=AsFunction,
    funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    f2p(HeadIs,RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

% If AsFunction is a recognized function, convert it to a predicate.
f2p(HeadIs,RetResult,AsFunction,AsPred):- % HeadIs\=@=AsFunction,
   is_function(AsFunction, Nth),  % Check if AsFunction is a recognized function and get the position Nth where the result is stored/retrieved
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, RetResult, Nth, AsPred),
   \+ ( compound(AsFunction), arg(_,AsFunction, Arg), is_function(Arg,_)),!.

% If any sub-term of Convert is an eval/2, convert that sub-term and then proceed with the conversion.
f2p(HeadIs,RetResult,Convert, Converted) :-
    rev_sub_sterm0(ConvertFunction, Convert), % Get the deepest sub-term AsFunction of Convert
    callable(ConvertFunction),  % Check if AsFunction is callable
    ConvertFunction = eval(AsFunction,Result),
    ignore(is_function(AsFunction, Nth)),
    funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, ConvertFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    f2p(HeadIs,RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

/* MAYBE USE ?
% If Convert is a compound term, we need to recursively convert its arguments.
f2p(HeadIs,RetResult, Convert, Converted) :- fail,
    compound(Convert), !,
    Convert =~ [Functor|Args],  % Deconstruct Convert to functor and arguments
    maplist(convert_argument, Args, ConvertedArgs),  % Recursively convert each argument
    Converted =~ [Functor|ConvertedArgs],  % Reconstruct Converted with the converted arguments
    (callable(Converted) -> f2p(HeadIs,RetResult, Converted, _); true).  % If Converted is callable, proceed with its conversion
% Helper predicate to convert an argument of a compound term
convert_argument(Arg, ConvertedArg) :-
    (callable(Arg) -> ftp(_, _, Arg, ConvertedArg); ConvertedArg = Arg).
*/

% The catch-all If no specific case is matched, consider Convert as already converted.
f2p(_HeadIs,_RetResult,u_assign(Convert,Res), u_assign(Convert,Res)):-!.
f2p(_HeadIs,RetResult,Convert, Code):- into_u_assign(Convert,RetResult,Code).



data_term(Convert):- self_eval(Convert),!.


de_eval(eval(X),X):- compound(X),!.

call1(G):- call(G).
call2(G):- call(G).
call3(G):- call(G).
call4(G):- call(G).
call5(G):- call(G).

trace_break:- trace,break.

u_assign(FList,R):- var(FList),nonvar(R), !, u_assign(R,FList).
u_assign(FList,R):- FList=@=R,!,FList=R.
u_assign(FList,R):- number(FList), var(R),!,R=FList.
u_assign(FList,R):- self_eval(FList), var(R),!,R=FList.
u_assign(FList,R):- var(FList),!,/*trace,*/freeze(FList,u_assign(FList,R)).
u_assign([V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(_Eq,V),!,maplist(eval_args,VI,VO).
u_assign((F:-List),R):- !, R = (F:-List).
u_assign(FList,R):- \+ compound(FList), var(R),!,R=FList.
u_assign([F|List],R):- F == ':-',!, trace_break,as_tf(clause(F,List),R).
u_assign([F|List],R):- atom(F),append(List,[R],ListR),
  catch(quietly(apply(F,ListR)),error(existence_error(procedure,F/_),_),
     catch(quietly(as_tf(apply(F,List),R)),error(existence_error(procedure,F/_),_),
        quietly(catch(eval_args([F|List],R),_, R=[F|List])))),!.
u_assign(FList,RR):- iz_conz(FList),!,as_tf(FList,RR).
u_assign(FList,RR):- (compound_non_cons(FList),u_assign_c(FList,RR))*->true;FList=~RR.

u_assign_c(FList,RR):-
  functor(FList,F,_),
  (catch(quietlY(call(FList,R)),error(existence_error(procedure,F/_),_),
     catch(quietlY(as_tf(FList,R)),error(existence_error(procedure,F/_),_),
        quietlY((p2m(FList,[F|List]),catch(eval_args([F|List],R),_, R=~[F|List])))))),!,R=RR.
u_assign_c(FList,R):- compound(FList), FList=~R.

quietlY(G):- call(G).

call_fr(G,Result,FA):- current_predicate(FA),!,call(G,Result).
call_fr(G,Result,_):- Result=G.

% This predicate is responsible for converting functions to their equivalent predicates.
% It takes a function 'AsFunction' and determines the predicate 'AsPred' which will be
% equivalent to the given function, placing the result of the function at the 'Nth' position
% of the predicate arguments. The 'Result' will be used to store the result of the 'AsFunction'.
%
% It handles cases where 'AsFunction' is a variable and when it's an atom or a compound term.
% For compound terms, it decomposes them to get the functor and arguments and then reconstructs
% the equivalent predicate with the 'Result' at the 'Nth' position.
%
% Example:
% funct_with_result_is_nth_of_pred(HeadIs,+(1, 2), Result, 3, +(1, 2, Result)).

into_callable(Pred,AsPred):- is_ftVar(Pred),!,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=AsPred,!.
into_callable(Pred,AsPred):- iz_conz(Pred), !,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=call_fr(_,_,_),!,AsPred=Pred.
into_callable(Pred,AsPred):- Pred =~ Cons,  !,AsPred=holds(Cons).

funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred):-
  var(AsPred),!,
  funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, Pred),
  into_callable(Pred,AsPred).

funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred):-
  var(AsFunction),!,
  funct_with_result_is_nth_of_pred0(HeadIs,Function, Result, Nth, AsPred),
  into_callable(Function,AsFunction).

funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred):-
  funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, AsPred).

% Handles the case where AsFunction is a variable.
% It creates a compound term 'AsPred' and places the 'Result' at the 'Nth' position
% of the predicate arguments, and the 'AsFunction' represents the functional form with
% arguments excluding the result.
funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, AsPred) :-
    is_ftVar(AsFunction),!,
   compound(AsPred),
    compound_name_list(AsPred,FP,PredArgs),
    nth1(Nth,PredArgs,Result,FuncArgs),
    do_predicate_function_canonical(FP,F),
    AsFunction =~ [F,FuncArgs].

% Handles the case where 'AsFunction' is not a variable.
% It decomposes 'AsFunction' to get the functor and arguments (FuncArgs) of the function
% and then it constructs the equivalent predicate 'AsPred' with 'Result' at the 'Nth'
% position of the predicate arguments.
funct_with_result_is_nth_of_pred0(HeadIs,AsFunctionO, Result, Nth, (AsPred)) :-
   de_eval(AsFunctionO,AsFunction),!,funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, AsPred).

funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, _Nth, AsPred) :-
   nonvar(AsFunction),
   compound(AsFunction),
   \+ is_arity_0(AsFunction,_),
   functor(AsFunction,F,A),
   HeadIs\=@=AsFunction,
   \+ (compound(HeadIs), (is_arity_0(HeadIs,HF);functor(HeadIs,HF,_))-> HF==F),
   (into_u_assign(AsFunction, Result,AsPred)
       -> true
       ; (AA is A+1,
           (FAA=(F/AA)),
           \+ current_predicate(FAA), !,
           AsPred = call_fr(AsFunction,Result,FAA))).


funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, (AsPred)) :-
   (atom(AsFunction)->AsFunction =~ [F | FuncArgs]; compound_name_list(AsFunction,F,FuncArgs)),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
    nth1(Nth, PredArgs, Result, FuncArgs), % It places 'Result' at the 'Nth' position
    AA is Nth+1, \+ current_predicate(F/AA),
    do_predicate_function_canonical(FP,F),
    AsPred =~ [FP | PredArgs]. % It forms the predicate 'AsPred' by joining the functor with the modified arguments list.



funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, (AsPred)) :-
    nonvar(AsFunction),
    AsFunction =~ [F | FuncArgs],
    do_predicate_function_canonical(FP,F),
    length(FuncArgs, Len),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
   ((number(Nth),Nth > Len + 1) -> throw(error(index_out_of_bounds, _)); true),
   (var(Nth)->(between(1,Len,From1),Nth is Len-From1+1);true),
    nth1(Nth,PredArgs,Result,FuncArgs),
    AsPred =~ [FP | PredArgs].

% optionally remove next line
funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, _, _, _) :-
    var(AsFunction),
    throw(error(instantiation_error, _)).

% The remove_funct_arg/3 predicate is a utility predicate that removes
% the Nth argument from a predicate term, effectively converting a
% predicate to a function. The first argument is the input predicate term,
% the second is the position of the argument to be removed, and the third
% is the output function term.
remove_funct_arg(AsPred, Nth, AsFunction) :-
    % Decompose AsPred into its functor and arguments.
    AsPred =~ [F | PredArgs],
    % Remove the Nth element from PredArgs, getting the list FuncArgs.
    nth1(Nth,PredArgs,_Result,FuncArgs),
    % Construct AsFunction using the functor and the list FuncArgs.
    do_predicate_function_canonical(F,FF),
    compound_name_list(AsFunction,FF,FuncArgs).

% rev_sub_sterm/2 predicate traverses through a given Term
% and finds a sub-term within it. The sub-term is unifiable with ST.
% This is a helper predicate used in conjunction with others to inspect
% and transform terms.

rev_sub_sterm(ST, Term):- rev_sub_sterm0(ST, Term), ST\=@=Term.
rev_sub_sterm0(_, Term):- never_subterm(Term),!,fail.
rev_sub_sterm0(ST, Term):- Term =~ if(Cond,_Then,_Else),!,rev_sub_sterm0(ST, Cond).
rev_sub_sterm0(ST, Term):- Term =~ 'if-error'(Cond,_Then,_Else),!,rev_sub_sterm0(ST, Cond).
rev_sub_sterm0(ST, Term):- Term =~ 'if-decons'(Cond,_Then,_Else),!,rev_sub_sterm0(ST, Cond).
rev_sub_sterm0(ST, Term):- Term =~ 'chain'(Expr,_Var,_Next),!,rev_sub_sterm0(ST, Expr).
rev_sub_sterm0(ST, Term):-
    % If Term is a list, it reverses the list and searches for a member
    % in the reversed list that is unifiable with ST.
    is_list(Term),!,rev_member(E,Term),rev_sub_sterm0(ST, E).
rev_sub_sterm0(ST, Term):-
    % If Term is a compound term, it gets its arguments and then recursively
    % searches in those arguments for a sub-term unifiable with ST.
    compound(Term), compound_name_list(Term,_,Args),rev_sub_sterm0(ST, Args).
rev_sub_sterm0(ST, ST):-
    % If ST is non-var, not an empty list, and callable, it unifies
    % ST with Term if it is unifiable.
    nonvar(ST), ST\==[], callable(ST).

never_subterm(Term):- is_ftVar(Term).
never_subterm([]).
never_subterm('Nil').
%never_subterm(F):- atom(F),not_function(F,0).

% rev_member/2 predicate is a helper predicate used to find a member
% of a list. It is primarily used within rev_sub_sterm/2 to
% traverse through lists and find sub-terms. It traverses the list
% from the end to the beginning, reversing the order of traversal.
rev_member(E,[_|L]):- rev_member(E,L).
rev_member(E,[E|_]).

% Continuing from preds_to_functs/2
% Converts a given predicate representation to its equivalent function representation
preds_to_functs(Convert, Converted):-
  % Verbose_unify/1 here may be used for debugging or to display detailed unification information
  verbose_unify(Convert),
  % Calls the auxiliary predicate preds_to_functs0/2 to perform the actual conversion
  preds_to_functs0(Convert, Converted).

% if Convert is a variable, Converted will be the same variable
preds_to_functs0(Convert, Converted) :-
    is_ftVar(Convert), !,
    Converted = Convert.

% Converts the rule (Head :- Body) to its function equivalent
preds_to_functs0((Head:-Body), Converted) :- !,
  % The rule is converted by transforming Head to a function AsFunction and the Body to ConvertedBody
 (
   pred_to_funct(Head, AsFunction, Result),
   %ignore(Result = '$VAR'('HeadRes')),
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   compound(BE),arg(Nth,BE,ArgRes),sub_var(Result,ArgRes),
   remove_funct_arg(BE, Nth, AsBodyFunction),
   append(Left,[eval(AsBodyFunction,Result)|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody),
   preds_to_functs0(NewBody,ConvertedBody),
   % The final Converted term is constructed
   into_equals(AsFunction,ConvertedBody,Converted)).

% Handles the case where Convert is a conjunction, and AsPred is not not_function.
% It converts predicates to functions inside a conjunction
preds_to_functs0((AsPred, Convert), Converted) :-
    \+ not_function(AsPred),
    pred_to_funct(AsPred, AsFunction, Result),
    sub_var(Result, Convert), !,
    % The function equivalent of AsPred replaces Result in Convert
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs0(Converting, Converted).

% Handles the special case where eval/2 is used and returns the function represented by the first argument of eval/2
preds_to_functs0(eval(AsFunction, _Result), AsFunction) :- !.

% Handles the general case where Convert is a conjunction.
% It converts the predicates to functions inside a conjunction
preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

% Handles the case where AsPred is a compound term that can be converted to a function
preds_to_functs0(AsPred, eval(AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.

% any other term remains unchanged
preds_to_functs0(X, X).

% Converts a given predicate AsPred to its equivalent function term AsFunction
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), % Checks if AsPred is a compound term
    functor(AsPred, F, A), % Retrieves the functor F and arity A of AsPred
    functional_predicate_arg(F, A, Nth),!, % Finds the Nth argument where the result should be
    arg(Nth, AsPred, Result), % Retrieves the result from the Nth argument of AsPred
    remove_funct_arg(AsPred, Nth, AsFunction). % Constructs the function AsFunction by removing the Nth argument from AsPred

% If not found in functional_predicate_arg/3, it tries to construct AsFunction by removing the last argument from AsPred
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    functor(AsPred, _, Nth),
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

% body_member/4 is utility predicate to handle manipulation of body elements in the clause, but the exact implementation details and usage are not provided in the given code.
body_member(Body,BE,NewBE,NewBody):-
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   append(Left,[NewBE|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody).
% combine_clauses/3 is the main predicate combining clauses with similar heads and bodies.
% HeadBodiesList is a list of clauses (Head:-Body)
% NewHead will be the generalized head representing all clauses in HeadBodiesList
% NewCombinedBodies will be the combined bodies of all clauses in HeadBodiesList.
combine_clauses(HeadBodiesList, NewHead, NewCombinedBodies) :-
    % If HeadBodiesList is empty, then NewCombinedBodies is 'false' and NewHead is an anonymous variable.
    (HeadBodiesList = [] -> NewCombinedBodies = false, NewHead = _ ;
    % Find all Heads in HeadBodiesList and collect them in the list Heads
    findall(Head, member((Head:-_), HeadBodiesList), Heads),
    % Find the least general head among the collected Heads
    least_general_head(Heads, LeastHead),
    functor(LeastHead,F,A),functor(NewHead,F,A),
    % Transform and combine bodies according to the new head found
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)),
    \+ \+ (
     Print=[converting=HeadBodiesList,newHead=NewHead],
     numbervars(Print,0,_,[]),fbug(Print),in_cmt(portray_clause(( NewHead :- NewCombinedBodies)))),!.

% Predicate to find the least general unified head (LGU) among the given list of heads.
% Heads is a list of head terms, and LeastGeneralHead is the least general term that unifies all terms in Heads.
least_general_head(Heads, LeastGeneralHead) :-
    lgu(Heads, LeastGeneralHead).

% the LGU of a single head is the head itself.
lgu([Head], Head) :- !.
% find the LGU of the head and the rest of the list.
lgu([H1|T], LGU) :-
    lgu(T, TempLGU),
    % Find generalization between head H1 and temporary LGU
    generalization(H1, TempLGU, LGU).

% generalization/3 finds the generalization of two heads, Head1 and Head2, which is represented by GeneralizedHead.
% This predicate is conceptual and will require more complex processing depending on the actual structures of the heads.
generalization(Head1, Head2, GeneralizedHead) :-
    % Ensure the functor names and arities are the same between Head1 and Head2.
    functor(Head1, Name, Arity),
    functor(Head2, Name, Arity),
    functor(GeneralizedHead, Name, Arity),
    % Generalize the arguments of the heads.
    generalize_args(Arity, Head1, Head2, GeneralizedHead).

% no more arguments to generalize.
generalize_args(0, _, _, _) :- !.
% generalize the corresponding arguments of the heads.
generalize_args(N, Head1, Head2, GeneralizedHead) :-
    arg(N, Head1, Arg1),
    arg(N, Head2, Arg2),
    % If the corresponding arguments are equal, use them. Otherwise, create a new variable.
    (Arg1 = Arg2 -> arg(N, GeneralizedHead, Arg1); arg(N, GeneralizedHead, _)),
    % Continue with the next argument.
    N1 is N - 1,
    generalize_args(N1, Head1, Head2, GeneralizedHead).

% transform_and_combine_bodies/3 takes a list of clause heads and bodies, a new head, and produces a combined body representing all the original bodies.
% The new body is created according to the transformations required by the new head.
transform_and_combine_bodies([(Head:-Body)|T], NewHead, CombinedBodies) :-
    % Transform the body according to the new head.
    transform(Head, NewHead, Body, TransformedBody),
    % Combine the transformed body with the rest.
    combine_bodies(T, NewHead, TransformedBody, CombinedBodies).

/* OLD
% Define predicate combine_clauses to merge multiple Prolog clauses with the same head.
% It receives a list of clauses as input and returns a combined clause.
combine_clauses([Clause], Clause) :- !.  % If there's only one clause, return it as is.
combine_clauses(Clauses, (Head :- Body)) :-  % If there are multiple clauses, combine them.
    Clauses = [(Head :- FirstBody)|RestClauses],  % Decompose the list into the first clause and the rest.
    combine_bodies(RestClauses, FirstBody, Body).  % Combine the bodies of all the clauses.

% Helper predicate to combine the bodies of a list of clauses.
% The base case is when there are no more clauses to combine; the combined body is the current body.
combine_bodies([], Body, Body).
combine_bodies([(Head :- CurrentBody)|RestClauses], PrevBody, Body) :-
    % Combine the current body with the previous body using a conjunction (,).
    combine_two_bodies(PrevBody, CurrentBody, CombinedBody),
    % Recursively combine the rest of the bodies.
    combine_bodies(RestClauses, CombinedBody, Body).

% Predicate to combine two bodies.
% Handles the combination of different Prolog constructs like conjunctions, disjunctions, etc.
combine_two_bodies((A, B), (C, D), (A, B, C, D)) :- !.  % Combine conjunctions.
combine_two_bodies((A; B), (C; D), (A; B; C; D)) :- !.  % Combine disjunctions.
combine_two_bodies(A, B, (A, B)).  % Combine simple terms using conjunction.
*/

% if there are no more bodies, the accumulated Combined is the final CombinedBodies.
combine_bodies([], _, Combined, Combined).
% combine the transformed body with the accumulated bodies.
combine_bodies([(Head:-Body)|T], NewHead, Acc, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    % Create a disjunction between the accumulated bodies and the transformed body.
    NewAcc = (Acc;TransformedBody),
    combine_bodies(T, NewHead, NewAcc, CombinedBodies).

% combine_code/3 combines Guard and Body to produce either Guard, Body, or a conjunction of both, depending on the values of Guard and Body.
combine_code(Guard, Body, Guard) :- Body==true, !.
combine_code(Guard, Body, Body) :- Guard==true, !.
combine_code(Guard, Body, (Guard, Body)).

% create_unifier/3 creates a unification code that unifies OneHead with NewHead.
% If OneHead and NewHead are structurally equal, then they are unified and the unification Guard is 'true'.
% Otherwise, the unification code is 'metta_unify(OneHead,NewHead)'.

create_unifier(OneHead,NewHead,Guard):- OneHead=@=NewHead,OneHead=NewHead,!,Guard=true.
create_unifier(OneHead,NewHead,Guard):- compound(OneHead),
  compound_name_list(OneHead,_,Args1),
  compound_name_list(NewHead,_,Args2),
  create_unifier_goals(Args1,Args2,Guard),!.
create_unifier(OneHead,NewHead,u(OneHead,NewHead)).

create_unifier_goals([V1],[V2],u(V1,V2)):-!.
create_unifier_goals([V1|Args1],[V2|Args2],RightGuard):-!,
  create_unifier_goals(Args1,Args2,Guard),
  combine_code(u(V1,V2),Guard,RightGuard).
create_unifier_goals([],[],true).


% transform/4 combines unification code with Body to produce NewBody according to the transformations required by NewHead.
% It uses create_unifier/3 to generate the unification code between OneHead and NewHead.
transform(OneHead, NewHead, Body, NewBody):- create_unifier(OneHead,NewHead,Guard),
   combine_code(Guard,Body,NewBody).




% ===============================
%  Compile in memory buffer
% ===============================

add_assertion(NewAssertion) :-
     expand_to_hb(NewAssertion,H,_),
     functor(H,F,A), functor(HH,F,A),
     assert(NewAssertion),
    % Get the current clauses of my_predicate/1
    findall(HH:-B,clause(HH,B),Prev),
    % Create a temporary file and add the new assertion along with existing clauses
    abolish(F/A),
    create_and_consult_temp_file(F/A, Prev).

% Predicate to create a temporary file and write the tabled predicate
create_and_consult_temp_file(PredName/Arity, PredClauses) :-
    % Generate a unique temporary memory buffer
    tmp_file_stream(text, TempFileName, TempFileStream),
    % Write the tabled predicate to the temporary file
    format(TempFileStream, ':- multifile((~q)/~w).~n', [PredName, Arity]),
    format(TempFileStream, ':- dynamic((~q)/~w).~n', [PredName, Arity]),
    %if_t( \+ option_value('tabling',false),
    if_t(option_value('tabling',true),format(TempFileStream,':- ~q.~n',[table(PredName/Arity)])),
    maplist(write_clause(TempFileStream), PredClauses),
    % Close the temporary file
    close(TempFileStream),
    % Consult the temporary file
    consult(TempFileName),
    % Delete the temporary file after consulting
    delete_file(TempFileName),
    assert(metta_compiled_predicate(PredName,Arity)).

% Helper predicate to write a clause to the file
write_clause(Stream, Clause) :-
    subst_vars(Clause,Can),
    write_canonical(Stream, Can),
    write(Stream, '.'),
    nl(Stream).


