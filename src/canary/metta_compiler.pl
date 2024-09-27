/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 */
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
:- ensure_loaded(metta_parser).
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_compiler_inlining).
:- ensure_loaded(metta_mizer).
% =======================================
% TODO move non flybase specific code between here and the compiler
%:- ensure_loaded(flybase_main).
% =======================================
%:- set_option_value(encoding,utf8).

eopfc:- ensure_loaded(mettalog('metta_ontology.pfc.pl')).

:- dynamic(metta_compiled_predicate/2).
:- multifile(metta_compiled_predicate/2).
:- dynamic(metta_compiled_predicate/3).
:- multifile(metta_compiled_predicate/3).


w_cl(P1,F):- atom(F),!,w_cl(P1,F/_).
w_cl(P1,F/A):- atom(F),integer(A),!,functor(P,F,A),w_cl(P1,P).
w_cl(P1,F/A):- forall((current_predicate(F/A),A>0),w_cl(P1,F/A)).
w_cl(P1,P):- call(P1,P).



dedupe_p1(P):- current_predicate(_,P),
  forall((copy_term(P,P2),
     clause(P,Bd,Ref),
     clause(P2,Bd2,Ref2), Ref@<Ref2,
     a2(P,Bd)=@=a2(P2,Bd2),
     erase(Ref2), fail),true).

info_ls(F):- pp_about((F-compiled_clauses)),ls_p1(F).
ls_p1(P):- @(listing(P),user).
dedupe_cl(F):- w_cl(dedupe_p1,F).

dedupe_ls(F):- dedupe_cl(F), info_ls(F).

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).

:- op(700,xfx,'=~').
:- op(690,xfx, =~ ).

:- dynamic(is_absorbed_return/3).
:- dynamic(is_absorbed_return/4).
:- dynamic(is_non_absorbed_return/3).
:- dynamic(is_non_absorbed_return/4).

is_absorbed_return(F,A,T):-
   current_self(Self),
   is_absorbed_return(Self,F,A,T).

is_non_absorbed_return(F,A,T):-
   current_self(Self),
   is_non_absorbed_return(Self,F,A,T).

compound_non_cons(B):-  compound(B),  \+ B = [_|_].
iz_conz(B):- compound(B), B=[_|_].


%'=~'(A,B):- var(A),iz_conz(B),!,A=B.
%'=~'(A,B):- iz_conz(A),var(B),!,A=B.
%'=~'(A,B):- iz_conz(A),iz_conz(B),!,A=B.
'=~'(A,B):- into_list_args0(A,AA),!,into_list_args0(B,BB),
  !,AA=BB.

% non-singleton Variable
is_nsVar(NS):- is_ftVar(NS), NS\=@= '$VAR'('_').

into_list_args(A,AA):- into_list_args0(A,AAA),!,AAA=AA.
into_list_args0(A,A):- is_ftVar(A).
into_list_args0(A,A):- is_nsVar(A).
into_list_args0([],[]):-!.
into_list_args0(C,[C]):- \+ compound(C),!.
into_list_args0([H|T],[H|T]):- \+ is_list(T),!.
into_list_args0([H,List,A],HT):- H == u_assign,!,append(List,[A],HT),!.
into_list_args0([H|T],[H|T]):-!.
into_list_args0(u_assign(List, A),[H|T]):- append(List,[A],[H|T]),!.
into_list_args0(holds(A),AA):- !, into_list_args(A,AA),!.
into_list_args0(C,[F|Args]):- compound_name_arguments(C,F,Args),!.



compound_name_list(AsPred,FP,PredArgs):- var(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- iz_conz(AsPred),!,AsPred=[FP|PredArgs].
%compound_name_list(AsPred,FP,PredArgs):- into_list_args(AsPred,[FP|PredArgs]),!.
compound_name_list(AsPred,FP,PredArgs):- compound_non_cons(AsPred),!,compound_name_arguments(AsPred,FP,PredArgs).
% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% ===============================
count_var(HB,C,N):- findall(V,(sub_term(V,HB),C==V),L),!,length(L,N).



% Conversion is possible between a function and a predicate of arity when the result is at the nth arg
:- dynamic decl_functional_predicate_arg/3.

% Converion is possible between a  function and predicate is tricky
functional_predicate_arg_tricky(is, 2, 1). % E.g. u_assign(is(+(1,2)),Result) converts to is(Result,+(1,2)).
% Defining standard mappings for some common functions/predicates
decl_functional_predicate_arg(append, 3, 3).
decl_functional_predicate_arg(+, 3, 3).
decl_functional_predicate_arg(-, 3, 3).
decl_functional_predicate_arg(*, 3, 3).
decl_functional_predicate_arg(pi, 1, 1).
decl_functional_predicate_arg('Empty', 1, 1). %:- trace.
decl_functional_predicate_arg(call,4,4).
decl_functional_predicate_arg(u_assign, 2, 2).
decl_functional_predicate_arg(iz, 2, 2).
decl_functional_predicate_arg(edge, 2, 2).
decl_functional_predicate_arg('==', 2, 2).
decl_functional_predicate_arg('=', 2, 2).
decl_functional_predicate_arg('is-same', 2, 2).
decl_functional_predicate_arg(assertEqual, 2, 2).
decl_functional_predicate_arg(assertTrue, 2, 2).
decl_functional_predicate_arg(case, 3, 3).
decl_functional_predicate_arg(collapse, 2, 2).
decl_functional_predicate_arg('PredArity', 2, 2).
decl_functional_predicate_arg('car-atom', 2, 2).
decl_functional_predicate_arg('cdr-atom', 2, 2).

decl_functional_predicate_arg(superpose, 2, 2).

decl_functional_predicate_arg(assertFalse, 2, 2).
decl_functional_predicate_arg(match,4,4).
decl_functional_predicate_arg('TupleConcat',3,3).
decl_functional_predicate_arg('new-space',1,1).

%decl_functional_predicate_arg('If', 4, 4).
%decl_functional_predicate_arg('If', 3, 3).
%decl_functional_predicate_arg('if', 4, 4).
%decl_functional_predicate_arg('if', 3, 3).

decl_functional_predicate_arg('exec0',1,1).
decl_functional_predicate_arg('exec1',1,1).

decl_functional_predicate_arg(superpose, 2, 2).

decl_functional_predicate_arg(+, 3, 3).

do_predicate_function_canonical(F,FF):- predicate_function_canonical(F,FF),!.
do_predicate_function_canonical(F,F).
predicate_function_canonical(is_Empty,'Empty').
%predicate_function_canonical(is_Empty,empty).

pi(PI):- PI is pi.

metta_atom_file_buffer_isa(I,T):- metta_atom_file_buffer([':',I,T]).


always_function_in_src(F,_):- \+ symbol(F), fail.
always_function_in_src('If',_).
always_function_in_src(F,_):- metta_atom_file_buffer_isa(F,'SrcFunction'),!.
always_function_in_src(F,A):- A1 is A + 1, predicate_arity(F,A1).

always_predicate_in_src(F,_):- \+ symbol(F), fail.
always_predicate_in_src(F,_):- metta_atom_file_buffer_isa(F,'SrcPredicate'),!.
always_predicate_in_src(':',2).
always_predicate_in_src('iz',2).
always_predicate_in_src('=',_).
always_predicate_in_src(F,A):- predicate_arity(F,A).
%always_function_in_src(F,A):- A1 is A+1, decl_functional_predicate_arg(F,_,A1).

symbol_or_var(V):- var(V),!.
symbol_or_var(F):- symbol(F),!.

eval_false(X):- \+ eval_true(X).
do(X):- ignore(eval(X,_)).

is_devel.

functional_predicate_arg(F, A, L):- \+ symbol(F), \+ var(F),!,
  is_devel, throw(datatype_functional_predicate_arg(F, A, L)).
functional_predicate_arg(F, A, L):-
  decl_functional_predicate_arg(F, A, L), \+ is_data_functor(F,A),
  \+ is_absorbed_return(F,_,_Bool).

% Mapping any current predicate F/A to a function, if it is not tricky
functional_predicate_arg(F, A, L):-
  defined_arity(F,A),
  \+ is_absorbed_return(F,_,_Bool),
  \+ functional_predicate_arg_tricky(F,A,_), L=A,
  \+ is_data_functor(F,_),
  \+ decl_functional_predicate_arg(F, A, _).
functional_predicate_arg(F, A, L):- functional_predicate_arg_tricky(F, A, L),
  \+ is_absorbed_return(F,_,_Bool).

metta_atom_file_buffer(Atom):- metta_file_buffer(+, Atom,_NamedVarsList,_Filename,_LineCount).
metta_atom_file_buffer(Atom):- metta_atom(Atom).

file_decl_arity(F,A):- freeze(Arity, 'PredArity' == Arity), metta_atom_file_buffer([Arity,F,A]).
:- dynamic(function_arity/3).
:- dynamic(predicate_arity/3).
predicate_arity(F,A):- current_self(KB), predicate_arity(KB,F,A).
predicate_arity(F,A):- file_decl_arity(F,A).
%predicate_arity(F,A):- metta_atom_file_buffer([:,F,[Ar|Args]]),Ar == '->',
%  \+ file_decl_arity(F,_), length(Args,AA),!,A=AA.
function_arity(F,A):- current_self(KB), function_arity(KB,F,A).


defined_arity(F,A):- predicate_arity(F,A).
defined_arity(F,A):- current_predicate(F/A), \+ predicate_arity(F,_).

% defined as (= .. .....)
decl_arity(F,A):- metta_atom_file_buffer([Eq,[FF|Len]|_]),
   Eq=='=',nonvar(FF),F==FF,is_list(Len),length([FF|Len],A).

import_arity(_,_):- fail, todo(metta_file_buffer(_Atom,_NamedVarsList,_Filename,_LineCount)).
is_data_functor(DataFunctor,DenotationalArity):- nonvar(DataFunctor),
       metta_atom_file_buffer(['DataFunctor',DataFunctor,DenotationalArity]).
is_data_functor(F,_):- \+ import_arity(F,_), \+ decl_arity(F,_).

% Certain constructs should not be converted to functions.
not_function(P):- symbol(P),!,not_function(P,0).
not_function(P):- callable(P),!,as_functor_args(P,F,A),not_function(F,A).
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
not_function(A,0):- symbol(A),!.
not_function('True',0).
not_function(F,A):- predicate_arity(F,A),AA is A+1, \+ decl_functional_predicate_arg(F,AA,_).

needs_call_fr(P):- is_function(P,_Nth),as_functor_args(P,F,A),AA is A+1, \+ current_predicate(F/AA).

is_control_structure(F,A):- symbol(F), atom_concat('if-',_,F),A>2.

'=='(A, B, Res):- as_tf(equal_enough(A, B),Res).
'or'(G1,G2):- G1 *-> true ; G2.
'or'(G1,G2,Res):- as_tf((G1 ; G2),Res).

% Function without arguments can be converted directly.
is_arity_0(AsFunction,F):- compound(AsFunction), compound_name_arity(AsFunction,F,0).

% Determines whether a given term is a function and retrieves the position
% in the predicate where the function Result is stored/retrieved
is_function(AsFunction, _):- is_ftVar(AsFunction),!,fail.
is_function(AsFunction, _):- AsFunction=='$VAR',!, trace, fail.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction,F), \+ not_function(F,0), !,Nth=1.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction,_), !,Nth=1.
is_function([F|Function], Nth) :-
  is_list(Function),length(Function,N),
  functional_predicate_arg_maybe(F, N, Nth).

is_function(AsFunction, Nth) :-
    callable(AsFunction),
    as_functor_args(AsFunction, Functor, A),
    \+ not_function(Functor, A),
    AA is A + 1,
    functional_predicate_arg_maybe(Functor, AA, Nth).

functional_predicate_arg_maybe(F, _, _):- \+ symbol(F),!,fail.
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
%                   u_assign(_C, RetResult)).
%
functs_to_preds(I,OO):-
   notrace(is_html->true; non_compat_io(color_g_mesg('yellow', (write_src(I),nl)))),
   must_det_ll(functs_to_preds0(I,OO)),!.

functs_to_preds0(I,OO):- \+ compound(I),!,OO=I.
%functs_to_preds0(I,OO):- data_term(I),!,OO=I.
functs_to_preds0(I,OO):- \+ is_conz(I), once(into_list_args(I,II)), I\=@=II, functs_to_preds(II,OO),!.
functs_to_preds0([Eq,H,B],OO):- Eq == '=', !, compile_for_assert(H, B, OO),!.
functs_to_preds0(=(H,B),OO):- !, compile_for_assert(H, B, OO),!.
functs_to_preds0(EqHB,OO):- compile_for_assert(EqHB,(X==X),OO),!.
functs_to_preds0(I,OO):-
  must_det_ll((
   sexpr_s2p(I, M),
   f2p(_,_,M,O),
   expand_to_hb(O,H,B),
   optimize_head_and_body(H,B,HH,BB),!,
   OO = ':-'(HH,BB))).

% ?- compile_for_exec(RetResult, is(pi+pi), Converted).


compile_for_exec(Res,I,O):-
   %ignore(Res='$VAR'('RetResult')),
   compile_for_exec0(Res,I,O),!.

compile_for_exec0(Res,I,u_assign(I,Res)):- is_ftVar(I),!.
compile_for_exec0(Res,(:- I),O):- !,
  compile_for_exec0(Res,I,O).
compile_for_exec0(Res,(?- I),O):- !,
  compile_for_exec0(Res,I,O).
compile_for_exec0(Res,I,BB):-
   %ignore(Res='$VAR'('RetResult')),
   compound_name_arguments(EXEC1, exec1, []),
   f2p(EXEC1,Res,I,O),
   optimize_head_and_body(exec1(Res),O,_,BB).

compile_for_exec0(Res,I,BB):- fail,
   compound_name_arguments(EXEC0, exec0, []),
   compile_for_assert(EXEC0, I, H:-BB),
   arg(1,H,Res).



compile_metta_defn(_KB,_F,_Len,Args,_BodyFn,_Clause):- \+ is_list(Args),!,fail.
%compile_metta_defn(_KB,_F,_Len,_Args,BodyFn,_Clause):- var(BodyFn),!,fail.
compile_metta_defn(KB,F,Len,Args,[WB|AL],ClauseU):- 'wam-body'==WB,!,
  must_det_ll((
    if_t(var(Len), ignore((function_arity(KB,F,Len)))),
    if_t(var(Arity),ignore((is_non_absorbed_return(KB,F,Len,_), ignore(Arity is Len+1)))),
    if_t(var(Arity),ignore((is_absorbed_return(KB,F,Arity,_), ignore(Len is Arity)))),
    if_t(var(Arity),ignore((predicate_arity(KB,F,Arity)))),
    if_t(var(Arity),length(Args,Arity)),
    if_t(var(Len),ignore(Len is Arity-1)),
    if_t(var(Len),if_t(integer(SLen),Len = SLen)),
    pfcAdd(metta_compiled_predicate(KB,F,Len)),
    Clause=(H:-B), s2c([F|Args],H), maplist(s2c,AL,ALB),
    list_to_conjuncts(ALB,B),
    %nl,print_tree(Clause),nl,
    add_unnumbered_clause(KB,F,Len,Clause,ClauseU))),!.
compile_metta_defn(KB,F,Len,Args,BodyFn,ClauseU):-
   must_det_ll((
        if_t(var(Len),length(Args,Len)),
        pfcAdd(metta_compiled_predicate(KB,F,Len)),
        compile_for_assert([F|Args],BodyFn,Clause),
        add_unnumbered_clause(KB,F,Len,Clause,ClauseU))).

add_unnumbered_clause(KB,F,Len,ClauseN,Clause):-
    must_det_ll((
       unnumbervars_clause(ClauseN,Clause),
       pfcAdd(metta_compiled_predicate(KB,F,Len)),
       add_assertion(KB,Clause))),!.

compile_for_assert_eq('=',HeadInC, AsBodyFnC, Converted):-
    subst_vars(['=',HeadInC, AsBodyFnC],['=',HeadIn, AsBodyFn],NamedVarsList),
    maplist(cname_var,NamedVarsList),!,
    compile_for_assert(HeadIn, AsBodyFn, Converted).
compile_for_assert_eq(':-',HeadIn, BodyIn, Converted):-
    call(ensure_compiler_ready),
    Converted=(H:-B), s2p(HeadIn,H), s2p(BodyIn,B),!.

ensure_compiler_ready:- ensure_loaded(mettalog('metta_ontology.pfc.pl')), ensure_corelib_types.
%ensure_compiler_ready:- eopfc.
/*
compile_for_assert_01(HeadIs, AsBodyFn, Converted) :-
  ( AsBodyFn =@= HeadIs ; AsBodyFn == [] ), !,
    compile_fact_for_assert(HeadIs,Converted).

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.

compile_for_assert_01(Head, AsBodyFn, Converted) :-
   once(compile_head_variablization(Head, HeadC, HeadCode)),
   \+ atomic(HeadCode), !,
   compile_for_assert_01(HeadC,
    (HeadCode,AsBodyFn), Converted),!.

compile_for_assert_01(HeadIn, AsBodyFn, Converted) :-
    r2p(HeadIn,HResult,Head),
    compile_for_assert_02(HResult,Head, AsBodyFn, Converted),!.
compile_for_assert_01(HeadIn, AsBodyFn, Converted) :-
    compile_for_assert_02(_HResult, HeadIn, AsBodyFn, Converted),!.


compile_for_assert_02(HResult,HeadIs, AsBodyFn, Converted)
 :- is_nsVar(AsBodyFn),
    AsFunction = HeadIs,!,
    must_det_ll((
     Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
     %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
     f2p(HeadIs,HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
       funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
     NextBody = u_assign(AsBodyFn,Result),
     optimize_head_and_body(Head,NextBody,HeadC,BodyC),
     cname_var('HEAD_RES',Result))),!.

compile_for_assert_02(HResult,HeadIs, AsBodyFn, Converted) :-
    h2p(HeadIs,HResult,NewHead),
     AsFunction = HeadIs,
     must_det_ll((
     Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   % funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
     f2p(NewHead,HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
        funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   %verbose_unify(Converted),
     f2p(HeadIs,Result,AsBodyFn,NextBody),
     %RetResult = Converted,
     %RetResult = _,
   optimize_head_and_body(Head,NextBody,HeadC,NextBodyC),
   %fbug([convert(Convert),optimize_head_and_body(HeadC:-NextBodyC)]),
     %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),

     cname_var('HEAD_RES',Result))),!.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert_02(HResult,HeadIs, AsBodyFn, Converted) :-
   Result = HResult,
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   compile_head_args(Head,HeadC,HeadCode),
   f2p(HeadIs,Result,AsBodyFn,NextBody),
   combine_code(HeadCode,NextBody,BodyC),!,
   optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),!.


*/
merge_structures([F|HeadAsFunction0], AsBodyFn0,A,B,(=(NewVar,Cept),C)):- fail,
   append(Left,[Merge|Right],HeadAsFunction0), nonvar(Merge),
   append(Left,[Cept|Right],HeadAsFunctionM),
   %HeadAsFunctionM=REPH,
   HeadAsFunction0=REPH,
   subst(AsBodyFn0+REPH,Merge,NewVar,NextBodyFn+NextHead),
   NextBodyFn+NextHead \=@= AsBodyFn0+HeadAsFunctionM,
   merge_structures([F|NextHead], NextBodyFn,A,B,C),
   Cept=Merge.
merge_structures(A,B,A,B,true).

compile_for_assert(HeadAsFunction0, AsBodyFn0, ConvertedO) :-
 must_det_ll((
    call(ensure_compiler_ready),
    merge_structures(HeadAsFunction0, AsBodyFn0,HeadAsFunction, AsBodyFn,PreCode),
    as_functor_args(HeadAsFunction,_F,Len),
    h2p(Which,HeadAsFunction,ResultToHead,HeadAsPred),
    compile_head_for_assert(Which,HeadAsPred,HeadC,_SupposedRT,
           Len, NarrowRetType,ResultToHead, ResultFromBody,HeadCode,ResultCode),
    f2p(HeadC,NarrowRetType,ResultFromBody,AsBodyFn,NextBody),
    combine_code([PreCode,HeadCode,NextBody,ResultCode],BodyC),!,
    optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),
    Convert = (HeadCC :- BodyCC),
    fix_equals_in_head(Convert,Converted),!,
    continue_opimize(Converted,ConvertedO))).


compile_head_for_assert(Which,Head, NewHead, SupposedRT, Len, NarrowRetType,ResultToHead,ResultFromBody,PreBodyCode,ResultCode):-
  \+ is_list(Head),
  as_functor_args(Head,F,_,ArgsL),!,
  compile_head_for_assert(Which,[F|ArgsL], NewHead, SupposedRT, Len, NarrowRetType,ResultToHead,ResultFromBody,PreBodyCode,ResultCode),!.

% compile_head_for_assert(Head, Head, true):- head_as_is(Head),!.
compile_head_for_assert(_Which,HeadAsPred,NewestHead,SupposedRT,Len,NarrowRetType,
    ResultToHead,ResultFromBody,
    PreBodyCode,ResultCode):-
 must_det_ll((
 HeadAsPred=[F|PredArgs],
 length(PredArgs,Arity),
 length(NewPredArgs,Arity),
 length(ParamTypes,Len),
 length(FunctionArgs,Len),length(NewFunctionArgs,Len),
 append(FunctionArgs,RetL,PredArgs),
 append(NewFunctionArgs,RetL,NewPredArgs),
 (RetL==[] -> true ; RetL=[ResultFromBody|_]),
 get_operator_typedef(Self,F,ParamTypes,BodyRetType),
 narrow_types(SupposedRT,BodyRetType,NarrowRetType),
 compile_head_args(20,HeadAsPred,Self,F,1,ParamTypes,FunctionArgs,NewFunctionArgs,ParamCode),
 FutureHead = [F|NewPredArgs],
 compile_head_variablization(FutureHead, NewestHead, VHeadCode),
 combine_code([ParamCode,VHeadCode],PreBodyCode),
 ResultCode = eval_for(ret,BodyRetType,ResultFromBody,ResultToHead))).


compile_head_variablization(Head, NewHead, PreBodyCode) :-
   must_det_ll(
     (as_functor_args(Head,Functor,A,Args),
      % Find non-singleton variables in Args
      fix_non_singletons(Args, NewArgs, Conditions),
      list_to_conjunction(Conditions,PreBodyCode),
      as_functor_args(NewHead,Functor,A,NewArgs))),!.





% Construct the new head args
compile_head_args(Depth,HeadIs,Self,F,Nth,[PT|ParamTypes],[A|Args],[N|NewArgs],CCode):- !,
  compile_one_head_arg(Depth,HeadIs,Self,F,Nth,PT,A,N,C),!,
  Nth1 is Nth+1,
  compile_head_args(Depth,HeadIs,Self,F,Nth1,ParamTypes,Args,NewArgs,Code),!,
  combine_code(C,Code,CCode).
compile_head_args(_Depth,_HeadIs,_Slf,_F,_Nth,[],Args,Args,true).
compile_head_args(_Depth,_HeadIs,_Slf,_F,_Nth,_ParamTypes,[],[],true).



%compile_one_head_arg(_Head, NewArg, Arg, (NewArg=~Arg)):- data_term(Arg),!.
%compile_one_head_arg(_Head, NewArg, Arg, (NewArg=~Arg)):- !.
%compile_one_head_arg(Head, NewArg, Arg, Code):- f2p_assign(10,Head,NewArg,Arg,Code).

compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,_PT,Arg,NewArg,eval_true(NewArg)):- Arg=='True',!.
compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,_PT,Arg,NewArg,eval_false(NewArg)):- Arg=='False',!.

compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(h5,PT,N,A)):- PT\=='Atom', is_list(A),!.
compile_one_head_arg(Depth,HeadIs,Slf,F,Nth,RetType,Arg,NewArgO,CodeOut):- \+ is_list(Arg),
    compound(Arg), as_functor_args(Arg,AF,_A,Args), Compile = [AF|Args], !,
compile_one_head_arg(Depth,HeadIs,Slf,F,Nth,RetType,Compile,NewArgO,CodeOut),!.
compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(h5,PT,N,A)):- PT\=='Atom', is_list(A),!.
compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(h5,PT,N,A)):- is_list(A),!.
compile_one_head_arg(_Depth,_HeadIs,_Slf,F,Nth,PT,A,N,eval_for(h4(Nth,F),PT,N,A)):- var(PT), var(A),!.
compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(h3,PT,N,A)):- var(PT), nonvar(A), get_type(A,PT),nonvar(PT),!.
compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,once(get_type(A,PT))):- A=N,var(PT), !.
compile_one_head_arg(_Depth,_HeadIs,_Slf,_F,_Nth,_PT,A,A,true).




















% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_fact_for_assert(HeadIs, (Head:-Body)):-
  compile_head_for_assert(HeadIs, NewHeadIs,Converted),
   optimize_head_and_body(NewHeadIs,Converted,Head,Body).

head_as_is(Head):-
   as_functor_args(Head,Functor,A,_),!,
   head_as_is(Functor,A).
head_as_is('If',3).

rewrite_sym(S,F):- \+ atomic(S),!,F=S.
rewrite_sym(':',F):- var(F),!, 'iz' == F,!.
rewrite_sym(F,F).

as_functor_args(AsPred,F,A):-
  as_functor_args(AsPred,F,A,_ArgsL),!.

as_functor_args(AsPred,F,A,ArgsL):-var(AsPred),!,
  (is_list(ArgsL);(integer(A),A>=0)),!,
   length(ArgsL,A),
   (symbol(F)->AsPred =..[F|ArgsL]; (AsPred = [F|ArgsL])).

as_functor_args(AsPred,_,_,_Args):- is_ftVar(AsPred),!,fail.
as_functor_args(AsPred,F,A,ArgsL):- \+ iz_conz(AsPred),
  AsPred=..List,!, as_functor_args(List,F,A,ArgsL),!.
%as_functor_args([Eq,R,Stuff],F,A,ArgsL):- (Eq == '='),
%   into_list_args(Stuff,List),append(List,[R],AsPred),!,
%   as_functor_args(AsPred,F,A,ArgsL).
as_functor_args([F|ArgsL],F,A,ArgsL):-  length(ArgsL,A),!.





is_f('S'):- fail.
is_mf(','). is_mf(';'). is_mf('call').
is_lf(':').


s2c(Args,true):- Args==[],!.
s2c(Args,call(Args)):- \+ iz_conz(Args),!.
s2c([F|Args],C):- \+ symbol(F), !, C=[F|Args].
s2c([F|Args],C):- is_lf(F), !, C=[F|Args].
s2c([At,F|Args],C):- symbol(F), At== '@', is_list(Args),!,maplist(s2c,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2c([F|Args],C):- is_f(F), is_list(Args),!,maplist(s2ca,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2c([F|Args],C):- is_mf(F), is_list(Args),!,maplist(s2c,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2c([F|Args],C):- is_list(Args),!,maplist(s2ca,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2c(C,call(C)).


s2ca(Args,Args):- \+ iz_conz(Args),!.
s2ca([H|T],[HH|TT]):- \+ symbol(H), !, s2ca(H,HH),s2ca(T,TT).
s2ca([F|Args],C):- is_lf(F), !, C=[F|Args].
s2ca([At,F|Args],C):- symbol(F), At== '@', is_list(Args),!,maplist(s2ca,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2ca([F|Args],C):- is_f(F), is_list(Args),!,maplist(s2ca,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2ca([F|Args],C):- is_mf(F), is_list(Args),!,maplist(s2c,Args,ArgsL), compound_name_arguments(C,F,ArgsL).
s2ca([H|T],[HH|TT]):- s2ca(H,HH),s2ca(T,TT).








fix_non_singletons(Args, NewArgs, [Code|Conditions]) :-
   sub_term_loc(Var, Args, Loc1), is_nsVar(Var),
   sub_term_loc_replaced(==(Var), _Var2, Args, Loc2, ReplVar2, NewArgsM),
   Loc1 \=@= Loc2,
   Code = same(ReplVar2,Var),
   fix_non_singletons(NewArgsM, NewArgs, Conditions),!.
fix_non_singletons(Args, Args, []):-!.


sub_term_loc(A,A,self).
sub_term_loc(E,Args,e(N,nth1)+Loc):- is_list(Args),!, nth1(N,Args,ST),sub_term_loc(E,ST,Loc).
sub_term_loc(E,Args,e(N,arg)+Loc):- compound(Args),arg(N,Args,ST),sub_term_loc(E,ST,Loc).

sub_term_loc_replaced(P1,E,Args,LOC,Var,NewArgs):- is_list(Args), !, sub_term_loc_l(nth1,P1,E,Args,LOC,Var,NewArgs).
sub_term_loc_replaced(P1,E,FArgs,LOC,Var,NewFArgs):- compound(FArgs), \+ is_nsVar(FArgs),!,
   compound_name_arguments(FArgs, Name, Args),
   sub_term_loc_l(arg,P1,E,Args,LOC,Var,NewArgs),
   compound_name_arguments(NewCompound, Name, NewArgs),NewFArgs=NewCompound.
   sub_term_loc_replaced(P1,A,A,self,Var,Var):- call(P1,A),!.


sub_term_loc_l(Nth,P1,E,Args,e(N,Nth)+Loc,Var,NewArgs):-
   reverse(Args,RevArgs),
   append(Left,[ST|Right],RevArgs),
   sub_term_loc_replaced(P1,E,ST,Loc,Var,ReplaceST),
   append(Left,[ReplaceST|Right],RevNewArgs),
   reverse(RevNewArgs,NewArgs),
   length([_|Right], N),!.


% Convert a list of conditions into a conjunction
list_to_conjunction([], true).
list_to_conjunction([Cond], Cond).
list_to_conjunction([H|T], RestConj) :- H==true,
   list_to_conjunction(T, RestConj).
list_to_conjunction([H|T], (H, RestConj)) :-
   list_to_conjunction(T, RestConj),!.

fix_equals_in_head(Convert,Convert):- \+ compound(Convert),!.
fix_equals_in_head(Convert:-Vert,Comp:-Vert):-!,
  fix_equals_in_head(Convert,Converted),
  as_compound_head(Converted,Comp).
fix_equals_in_head(R=C,Converted):-
   append_term(C,R,Converted).

fix_equals_in_head((A:B),iz(A,B)):- !.
fix_equals_in_head(Convert,Convert).

as_compound_head([F|Converted],Comp):- symbol(F),!,compound_name_arguments(Comp,F,Converted).
as_compound_head(Comp,Comp).

:- op(700,xfx,'=~').


filter_head_arg(H,F):- var(H),!,H=F.
filter_head_arge(H,F):- H = F.

code_callable(Term,_CTerm):- var(Term),!,fail.
code_callable(Term, CTerm):- current_predicate(_,Term),!,Term=CTerm.
%code_callable(Term, CTerm):- current_predicate(_,Term),!,Term=CTerm.

compile_test_then_else(Depth,RetResult,If,Then,Else,Converted):-
  f2p(Depth,HeadIs,RetType,ThenResult,Then,ThenCode),
  f2p(Depth,HeadIs,RetType,ElseResult,Else,ElseCode),
  Converted=(If*->(ThenCode,ThenResult=RetResult);
                  (ElseCode,ElseResult=RetResult)).

:- discontiguous(f2q/6).
%:- discontiguous(f2q/6).


dif_functors(HeadIs,_):- var(HeadIs),!,fail.
dif_functors(HeadIs,_):- \+ compound(HeadIs),!.
dif_functors(HeadIs,Convert):- compound(HeadIs),compound(Convert),
  compound_name_arity(HeadIs,F,A),compound_name_arity(Convert,F,A).

is_compiled_and(AND):- member(AND,[ /*(','), ('and'),*/ ('and2')]).

flowc.

no_lists(Args):- maplist(not_a_function_in_arg,Args).

not_a_function_in_arg(Arg):- is_ftVar(Arg),!.
not_a_function_in_arg(Arg):- \+ is_list(Arg),!.




    %is_data_functor(F,A),!.
%f2q(_Depth,_HeadIs,_RetType,_RetResult, ie(N=V, Code)) :- !, into_equals(N,V,Code).

% The catch-all If no specific case is matched, consider Convert as already converted.

%f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Code):- into_u_assign(Convert,RetResult,Code).





de_eval(u_assign(X),X):- compound(X),!.

call1(G):- call(G).
call2(G):- call(G).
call3(G):- call(G).
call4(G):- call(G).
call5(G):- call(G).

trace_break:- trace,break.

%:- table(u_assign/2).
%u_assign(FList,R):- is_list(FList),!,u_assign(FList,R).
u_assign(FList,R):- var(FList),nonvar(R), !, u_assign(R,FList).
u_assign(FList,R):- FList=@=R,!,FList=R.
u_assign([F|List],[F|R]):-  List=R, !.
%u_assign(FList,R):- number(FList), var(R),!,R=FList.
u_assign(FList,R):- \+ compound(FList), var(R),!,R=FList.
u_assign(FList,R):- self_eval(FList), var(R),!,R=FList.
u_assign(FList,RR):- (compound_non_cons(FList),u_assign_c(FList,RR))*->true;FList=~RR.
u_assign(FList,R):- FList =~ R, !.
u_assign(FList,R):- var(FList),!,/*trace,*/freeze(FList,u_assign(FList,R)).
u_assign([F|List],R):- F == ':-',!, trace_break,as_tf(clause(F,List),R).
%u_assign(FList,RR):- u_assign_list1(FList,RR)*->true;u_assign_list2(FList,RR).

u_assign_list1([F|List],R):- fail,u_assign([F|List],R), nonvar(R), R\=@=[F|List].
u_assign_list2([F|List],R):- symbol(F),append(List,[R],ListR),
  catch(quietly(apply(F,ListR)),error(existence_error(procedure,F/_),_),
     catch(quietly(as_tf(apply(F,List),R)),error(existence_error(procedure,F/_),_),
        (fail, quietly(catch(u_assign([F|List],R),_, R=[F|List]))))).

%u_assign([V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(_Eq,V),!,maplist(u_assign,VI,VO).

u_assign_c((F:-List),R):- !, R = (F:-List).
u_assign_c(FList,RR):-
  as_functor_args(FList,F,_),
  (catch(quietlY(call(FList,R)),error(existence_error(procedure,F/_),_),
     catch(quietlY(as_tf(FList,R)),error(existence_error(procedure,F/_),_),
        quietlY((p2m(FList,[F|List]),catch(u_assign([F|List],R),_, R=~[F|List])))))),!,R=RR.
u_assign_c(FList,RR):- as_tf(FList,RR),!.
u_assign_c(FList,R):- compound(FList), !, FList=~R.

quietlY(G):- call(G).

call_fr(G,Result,FA):- current_predicate(FA),!,call(G,Result).
call_fr(G,Result,_):- Result=G.

% This predicate is responsible for converting functions to their equivalent predicates.
% It takes a function 'AsFunction' and determines the predicate 'AsPred' which will be
% equivalent to the given function, placing the result of the function at the 'Nth' position
% of the predicate arguments. The 'Result' will be used to store the result of the 'AsFunction'.
%
% It handles cases where 'AsFunction' is a variable and when it's an symbol or a compound term.
% For compound terms, it decomposes them to get the as_functor_args and arguments and then reconstructs
% the equivalent predicate with the 'Result' at the 'Nth' position.
%
% Example:
% funct_with_result_is_nth_of_pred(HeadIs,+(1, 2), Result, 3, +(1, 2, Result)).

into_callable(Pred,AsPred):- is_ftVar(Pred),!,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=AsPred,!.
into_callable(Pred,AsPred):- iz_conz(Pred), !,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=call_fr(_,_,_),!,AsPred=Pred.
into_callable(Pred,AsPred):- Pred =~ Cons,  !,AsPred=holds(Cons).


%r2p(MeTTa,Result,IsPred):- r2p(_,MeTTa,Result,IsPred),!.

%r2p(What,MeTTa,Result,IsPred):- h2p(What,MeTTa,Result,IsPred),!.
%r2p(What,MeTTa,Result,IsPred):- ar2q(What,MeTTa,Result,IsPred),!.


%h2p(MeTTa,Result,IsPred):- h2p(_,MeTTa,Result,IsPred).


absorbed_default('Bool',_AsPred,'True').
absorbed_default(_,_AsPred,_).

is_absorbed_return_value(F,A,Result):-
  is_absorbed_return_value(F,A,_,Result).
is_absorbed_return_value(F,A,AsPred,Result):-
  is_absorbed_return(F,A,Bool),
  absorbed_default(Bool,AsPred,Result).

h2p(boolean,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,Len,PArgs),
    is_absorbed_return_value(F,Len,Result),!,
    safe_univ(IsPred,F,PArgs),!.

h2p(func,AsFunction,Result,IsPred):-
    as_functor_args(AsFunction,F,Len,Args),
    is_non_absorbed_return(F,Len,_Type),
    append(Args,[Result],PArgs),
    safe_univ(IsPred,F,PArgs),!.

h2p(pred,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,Len,PArgs),
    is_absorbed_return(F,Len,_Type),
    Result = 'True',
    cname_var('AbsorbedRetTrue',Result),
    safe_univ(IsPred,F,PArgs),!.

h2p(pred,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,PArgs),
    always_predicate_in_src(F,A),!,
%   once(functional_predicate_arg(F, A, Nth);Nth=A),
    %is_absorbed_return(F,A, _Bool),
    %nth1(Nth,Args,Result),
    Result = 'True',
    cname_var('PRetTrue',Result),
    safe_univ(IsPred,F,PArgs).

h2p(func,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    always_function_in_src(F,A),
    append(Args,[Result],PArgs),
    cname_var('FRet',Result),
    safe_univ(IsPred,F,PArgs),!.

h2p(func,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,_A,Args),
    append(Args,[Result],PArgs),
    cname_var('Ret',Result),
    safe_univ(IsPred,F,PArgs),!.

safe_univ(IsPred,F,PArgs):- is_list(PArgs),atom(F),!,IsPred=..[F|PArgs].
safe_univ(IsPred,F,PArgs):- compound(IsPred),var(F),!,IsPred=..[F|PArgs].
safe_univ(IsPred,F,PArgs):- IsPred=fL(F,PArgs).

/*


h2p(func,AsFunction,Result,IsPred):-
    as_functor_args(AsFunction,F,Len,Args),
    is_non_absorbed_return(F,Len, _Type),
    append(Args,[Result],PArgs),
    safe_univ(IsPred,F,PArgs),!.


h2p(W,Data,Result,IsPred):-
    W\== boolean,
    as_functor_args(Data,F,A,_Args),
    is_data_functor(F,AA),!,
    (AA=A
       -> (IsPred = (Data =~ Result))
       ; was_predicate(Data,Result,IsPred)).
h2p(pred,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    always_predicate_in_src(F,A),!,
    once(functional_predicate_arg(F, A, Nth);Nth=A),
    \+ is_absorbed_return(F,_, _Bool),
    nth1(Nth,Args,Result),
    IsPred=..[F|Args].
h2p(func,AsFunction,Result,IsPred):-
   as_functor_args(AsFunction,F,A0,FArgs),
   \+ is_absorbed_return(F,A0, _Bool),
   always_function_in_src(F,A0),!,A is A0 + 1,
   once(functional_predicate_arg(F, A, Nth);Nth=A),
   nth1(Nth,Args,Result,FArgs),
   IsPred=..[F|Args].
*/

ar2q(MeTTa,Result,IsPred):- ar2q(_,MeTTa,Result,IsPred).
ar2q(pred,AsPred,Result,IsPred):-
  as_functor_args(AsPred,F,A,Args),
  once(functional_predicate_arg(F, A, Nth);Nth=A),
  nth1(Nth,Args,Result),
  \+ is_absorbed_return(F,_, _Bool),
  IsPred=..[F|Args].
ar2q(funct,AsFunction,Result,IsPred):-
  as_functor_args(AsFunction,F,A0,FArgs),A is A0 + 1,
  \+ is_absorbed_return(F,_, _Bool),
  once(functional_predicate_arg(F, A, Nth);Nth=A),
  nth1(Nth,Args,Result,FArgs),
  IsPred=..[F|Args].

ar2q(pred,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    is_absorbed_return_value(F,A,AsPred,Result),
    IsPred=..[F|Args],!.

was_predicate(AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    is_absorbed_return_value(F,A,AsPred,Result),
    IsPred=..[F|Args],!.

was_predicate(AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    once(functional_predicate_arg(F, A, Nth);Nth=A),
    \+ is_non_absorbed_return(F,A, _Bool),
    nth1(Nth,Args,Result),
    IsPred=..[F|Args].


was_function(AsFunction,Result,IsPred):-
    as_functor_args(AsFunction,F,A0,FArgs),
    ( ( \+ is_absorbed_return(F,A0,_)) ; is_non_absorbed_return(F,A0,_)),
    A is A0 + 1,
    once(functional_predicate_arg(F, A, Nth);Nth=A),
    nth1(Nth,Args,Result,FArgs),
    IsPred=..[F|Args].


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
    is_nsVar(AsFunction),!,
   compound(AsPred),
    compound_name_list(AsPred,FP,PredArgs),
    nth1(Nth,PredArgs,Result,FuncArgs),
    do_predicate_function_canonical(FP,F),
    AsFunction =~ [F,FuncArgs].

% Handles the case where 'AsFunction' is not a variable.
% It decomposes 'AsFunction' to get the as_functor_args and arguments (FuncArgs) of the function
% and then it constructs the equivalent predicate 'AsPred' with 'Result' at the 'Nth'
% position of the predicate arguments.
funct_with_result_is_nth_of_pred0(HeadIs,AsFunctionO, Result, Nth, (AsPred)) :-
   de_eval(AsFunctionO,AsFunction),!,funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, AsPred).

funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, _Nth, AsPred) :-
   nonvar(AsFunction),
   compound(AsFunction),
   \+ is_arity_0(AsFunction,_),
   as_functor_args(AsFunction,F,A),
   HeadIs\=@=AsFunction,
   \+ (compound(HeadIs), (is_arity_0(HeadIs,HF);as_functor_args(HeadIs,HF,_))-> HF==F),
   (into_u_assign(AsFunction, Result,AsPred)
       -> true
       ; (AA is A+1,
           (FAA=(F/AA)),
           \+ current_predicate(FAA), !,
           AsPred = call_fr(AsFunction,Result,FAA))).


funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, (AsPred)) :-
   (symbol(AsFunction)->AsFunction =~ [F | FuncArgs]; compound_name_list(AsFunction,F,FuncArgs)),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
    nth1(Nth, PredArgs, Result, FuncArgs), % It places 'Result' at the 'Nth' position
    AA is Nth+1, \+ current_predicate(F/AA),
    do_predicate_function_canonical(FP,F),
    AsPred =~ [FP | PredArgs]. % It forms the predicate 'AsPred' by joining the as_functor_args with the modified arguments list.



funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, (AsPred)) :-
    nonvar(AsFunction),
    AsFunction =~ [F | FuncArgs],
    do_predicate_function_canonical(FP,F),
    length(FuncArgs, Len),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
   ((number(Nth),Nth > Len + 1) -> (trace,throw(error(index_out_of_bounds, _))); true),
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
    % Decompose AsPred into its as_functor_args and arguments.
    AsPred =~ [F | PredArgs],
    % Remove the Nth element from PredArgs, getting the list FuncArgs.
    nth1(Nth,PredArgs,_Result,FuncArgs),
    % Construct AsFunction using the as_functor_args and the list FuncArgs.
    do_predicate_function_canonical(F,FF),
    compound_name_list(AsFunction,FF,FuncArgs).

% deep_lhs_sub_sterm/2 predicate traverses through a given Term
% and finds a sub-term within it. The sub-term is unifiable with ST.
% This is a helper predicate used in conjunction with others to inspect
% and transform terms.

deep_lhs_sub_sterm(ST, Term):- deep_lhs_sub_sterm0(ST, Term), ST\=@=Term.
deep_lhs_sub_sterm0(_, Term):- never_subterm(Term),!,fail.
deep_lhs_sub_sterm0(ST, Term):- Term =~ if(Cond,_Then,_Else),!,deep_lhs_sub_sterm0(ST, Cond).
deep_lhs_sub_sterm0(ST, Term):- Term =~ 'if-error'(Cond,_Then,_Else),!,deep_lhs_sub_sterm0(ST, Cond).
deep_lhs_sub_sterm0(ST, Term):- Term =~ 'if-decons'(Cond,_Then,_Else),!,deep_lhs_sub_sterm0(ST, Cond).
deep_lhs_sub_sterm0(ST, Term):- Term =~ 'chain'(Expr,_Var,_Next),!,deep_lhs_sub_sterm0(ST, Expr).
deep_lhs_sub_sterm0(ST, Term):-
    % If Term is a list, it reverses the list and searches for a member
    % in the reversed list that is unifiable with ST.
    is_list(Term),!,member(E,Term),deep_lhs_sub_sterm0(ST, E).
deep_lhs_sub_sterm0(ST, Term):-
    % If Term is a compound term, it gets its arguments and then recursively
    % searches in those arguments for a sub-term unifiable with ST.
    compound(Term), compound_name_list(Term,_,Args),deep_lhs_sub_sterm0(ST, Args).
deep_lhs_sub_sterm0(ST, ST):-
    % If ST is non-var, not an empty list, and callable, it unifies
    % ST with Term if it is unifiable.
    nonvar(ST), ST\==[], callable(ST).

never_subterm(Term):- is_ftVar(Term).
never_subterm([]).
never_subterm('Nil').
%never_subterm(F):- symbol(F),not_function(F,0).

% rev_member/2 predicate is a helper predicate used to find a member
% of a list. It is primarily used within deep_lhs_sub_sterm/2 to
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
   cname_var('HEAD_RES',Result),
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   compound(BE),arg(Nth,BE,ArgRes),sub_var(Result,ArgRes),
   remove_funct_arg(BE, Nth, AsBodyFunction),
   append(Left,[u_assign(AsBodyFunction,Result)|Right],NewRevList),
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

% Handles the special case where u_assign/2 is used and returns the function represented by the first argument of u_assign/2
preds_to_functs0(u_assign(AsFunction, _Result), AsFunction) :- !.

% Handles the general case where Convert is a conjunction.
% It converts the predicates to functions inside a conjunction
preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

% Handles the case where AsPred is a compound term that can be converted to a function
preds_to_functs0(AsPred, u_assign(AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.

% any other term remains unchanged
preds_to_functs0(X, X).

% Converts a given predicate AsPred to its equivalent function term AsFunction
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), % Checks if AsPred is a compound term
    as_functor_args(AsPred, F, A), % Retrieves the as_functor_args F and arity A of AsPred
    functional_predicate_arg(F, A, Nth),!, % Finds the Nth argument where the result should be
    arg(Nth, AsPred, Result), % Retrieves the result from the Nth argument of AsPred
    remove_funct_arg(AsPred, Nth, AsFunction). % Constructs the function AsFunction by removing the Nth argument from AsPred

% If not found in functional_predicate_arg/3, it tries to construct AsFunction by removing the last argument from AsPred
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    as_functor_args(AsPred, _, Nth),
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
    as_functor_args(LeastHead,F,A),as_functor_args(NewHead,F,A),
    % Transform and combine bodies according to the new head found
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)),
    \+ \+ (
     Print=[converting=HeadBodiesList,newHead=NewHead],
     numbervars(Print,0,_,[]),fbug(Print),
        nop(in_cmt(print_pl_source(( NewHead :- NewCombinedBodies))))),!.

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
    % Ensure the as_functor_args names and arities are the same between Head1 and Head2.
    as_functor_args(Head1, Name, Arity),
    as_functor_args(Head2, Name, Arity),
    as_functor_args(GeneralizedHead, Name, Arity),
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
combine_code((A,B,C), Body, Out):- combine_code(C,Body,CBody),combine_code(B,CBody,BCBody),combine_code(A,BCBody,Out).
combine_code((AB,C), Body, Out):- combine_code(C,Body,CBody),combine_code(AB,CBody,Out).
combine_code(Guard, Body, (Guard, Body)).


combine_code([A|Nil],O):- Nil==[],!,combine_code(A,O).
combine_code([A|B],O):- \+ is_list(B),combine_code(A,AA),combine_code(B,BB),!,
  combine_code([AA,BB],O).
combine_code([A,B|C],O):- \+ is_list(B),
  combine_code(A,AA),combine_code(B,BB),!,
  combine_code(AA,BB,AB),
  combine_code([AB|C],O),!.
combine_code((A;O),(AA;OO)):- !, combine_code(A,AA),combine_code(O,OO).
combine_code(AO,AO).



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


unnumbervars_clause(Cl,ClU):-
  copy_term_nat(Cl,AC),unnumbervars(AC,UA),copy_term_nat(UA,ClU).
% ===============================
%  Compile in memory buffer
% ===============================
is_clause_asserted(AC):- unnumbervars_clause(AC,UAC),
  expand_to_hb(UAC,H,B),clause(H,B,Ref),clause(HH,BB,Ref),
  strip_m(HH,HHH),HHH=@=H,
  strip_m(BB,BBB),BBB=@=B,!.

strip_m(M:BB,BB):- nonvar(BB),nonvar(M),!.
strip_m(BB,BB).

get_clause_pred(UAC,F,A):- expand_to_hb(UAC,H,_),strip_m(H,HH),functor(HH,F,A).

:- dynamic(needs_tabled/2).

add_assertion(Space,List):- is_list(List),!,maplist(add_assertion(Space),List).
add_assertion(Space,AC):- unnumbervars_clause(AC,UAC), add_assertion1(Space,UAC).
add_assertion1(_,AC):- /*'&self':*/is_clause_asserted(AC),!.
add_assertion1(_,AC):- get_clause_pred(AC,F,A), \+ needs_tabled(F,A), !, pfcAdd(/*'&self':*/AC),!.

add_assertion1(_KB,ACC) :-
     copy_term(ACC,AC,_),
     expand_to_hb(AC,H,_),
     as_functor_args(H,F,A), as_functor_args(HH,F,A),
    % assert(AC),
    % Get the current clauses of my_predicate/1
    findall(HH:-B,clause(/*'&self':*/HH,B),Prev),
    copy_term(Prev,CPrev,_),
    % Create a temporary file and add the new assertion along with existing clauses
    append(CPrev,[AC],NewList),
    cl_list_to_set(NewList,Set),
    length(Set,N),
    if_t(N=2,
        (Set=[X,Y],
          numbervars(X),
          numbervars(Y),
        nl,display(X),
        nl,display(Y),
        nl)),
    %wdmsg(list_to_set(F/A,N)),
    abolish(/*'&self':*/F/A),
    create_and_consult_temp_file(F/A, Set).


cl_list_to_set([A|List],Set):-
  member(B,List),same_clause(A,B),!,
  cl_list_to_set(List,Set).
cl_list_to_set([New|List],[New|Set]):-!,
  cl_list_to_set(List,Set).
cl_list_to_set([A,B],[A]):- same_clause(A,B),!.
cl_list_to_set(List,Set):- list_to_set(List,Set).

same_clause(A,B):- A==B,!.
same_clause(A,B):- A=@=B,!.
same_clause(A,B):- unnumbervars_clause(A,AA),unnumbervars_clause(B,BB),same_clause1(AA,BB).
same_clause1(A,B):- A=@=B.
same_clause1(A,B):- expand_to_hb(A,AH,AB),expand_to_hb(B,BH,BB),AB=@=BB, AH=@=BH,!.

%clause('is-closed'(X),OO1,Ref),clause('is-closed'(X),OO2,Ref2),Ref2\==Ref, OO1=@=OO2.

% Predicate to create a temporary file and write the tabled predicate
create_and_consult_temp_file(F/A, PredClauses) :-
    % Generate a unique temporary memory buffer
    tmp_file_stream(text, TempFileName, TempFileStream),
    % Write the tabled predicate to the temporary file
    format(TempFileStream, ':- multifile((~q)/~w).~n', [F, A]),
    format(TempFileStream, ':- dynamic((~q)/~w).~n', [F, A]),
    %if_t( \+ option_value('tabling',false),
    if_t(option_value('tabling','True'),format(TempFileStream,':- ~q.~n',[table(F/A)])),
    maplist(write_clause(TempFileStream), PredClauses),
    % Close the temporary file
    close(TempFileStream),
    % Consult the temporary file
    % abolish(F/A),
    /*'&self':*/consult(TempFileName),
    % Delete the temporary file after consulting
    %delete_file(TempFileName),
    true.


% Helper predicate to write a clause to the file
write_clause(Stream, Clause) :-
    subst_vars(Clause,Can),
    write_canonical(Stream, Can),
    write(Stream, '.'),
    nl(Stream).

same(X,Y):- X =~ Y.


end_of_file.



    % If any sub-term of Convert is a control flow imperative, convert that sub-term and then proceed with the conversion.
    f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-   fail,
        % Get the least deepest sub-term AsFunction of Convert
        get_first_p1(AsFunction,Convert,N1Cmpd),
        arg(2,N1Cmpd,Cmpd),
        Cmpd \= ( ==(_,_) ),
        (Cmpd = [EE,_,_] -> (EE \== '==') ; true ),
        AsFunction\=@= Convert,
        callable(AsFunction),  % Check if AsFunction is callable
        Depth2 is Depth -0,
        % check that that is is a control flow imperative
        f2q(Depth2,HeadIs,RetType,Result,AsFunction, AsPred),
        HeadIs\=@=AsFunction,!,
        subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
        f2p(Depth2,HeadIs,RetType,RetResult,(AsPred,Result==AsFunction,Converting), Converted).  % Proceed with the conversion of the remaining terms


        % If any sub-term of Convert is a control flow imperative, convert that sub-term and then proceed with the conversion.
    f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-   fail,
            deep_lhs_sub_sterm(AsFunction, Convert),
            AsFunction\=@= Convert,
            % Get the deepest sub-term AsFunction of Convert
          %  sub_term(AsFunction, Convert), AsFunction\==Convert,
            callable(AsFunction),  % Check if AsFunction is callable
        Depth2 is Depth -0,
            f2q(Depth2,HeadIs,RetType,Result,AsFunction, AsPred),
            HeadIs\=@=AsFunction,!,
            subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
            f2p(Depth2,HeadIs,RetType,RetResult,(AsPred,Converting), Converted).  % Proceed with the conversion of the remaining terms

    % If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
    f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,
        deep_lhs_sub_sterm(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
        AsFunction\=@= Convert,
        callable(AsFunction),  % Check if AsFunction is callable
        %is_function(AsFunction, Nth),  % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
        HeadIs\=@=AsFunction,
        funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, AsPred),  % Convert AsFunction to a predicate AsPred
        subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
        f2p(Depth,HeadIs,RetType,RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms
    /*
    % If AsFunction is a recognized function, convert it to a predicate.
    f2q(Depth,HeadIs,RetType,RetResult,AsFunction,AsPred):- % HeadIs\=@=AsFunction,
       is_function(AsFunction, Nth),  % Check if AsFunction is a recognized function and get the position Nth where the result is stored/retrieved
       funct_with_result_is_nth_of_pred(HeadIs,AsFunction, RetResult, Nth, AsPred),
       \+ ( compound(AsFunction), arg(_,AsFunction, Arg), is_function(Arg,_)),!.
    */

    % If any sub-term of Convert is an u_assign/2, convert that sub-term and then proceed with the conversion.
    f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,
        deep_lhs_sub_sterm0(ConvertFunction, Convert), % Get the deepest sub-term AsFunction of Convert
        callable(ConvertFunction),  % Check if AsFunction is callable
        ConvertFunction = u_assign(AsFunction,Result),
        ignore(is_function(AsFunction, Nth)),
        funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
        subst(Convert, ConvertFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
        f2p(Depth,HeadIs,RetType,RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

