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
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
decl_functional_predicate_arg(assertTrue, 1, 1).
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

metta_atom_file_buffer(Atom):- metta_file_buffer(+,Atom,_NamedVarsList,_Filename,_LineCount).
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




%compile_for_exec0(Res,I,O):- f2p(exec(),Res,I,O).


% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_fact_for_assert(HeadIs,RetType, (Head:-Body)):-
  compile_head_for_assert(HeadIs,RetType, NewHeadIs,Converted),
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








compile_head_for_assert(Head, NewHead, BodyOut):- \+ is_list(Head),
  as_functor_args(Head,F,_,ArgsL),!,
  compile_head_for_assert([F|ArgsL], NewHead, BodyOut).

% compile_head_for_assert(Head, Head, true):- head_as_is(Head),!.
compile_head_for_assert(Head, NewestHead, HeadCode):-
 must_det_ll(
  (compile_head_variablization(Head, NewHead, VHeadCode),
   compile_head_args(NewHead, NewestHead, AHeadCode),
   combine_code(VHeadCode,AHeadCode,HeadCode))).



% Construct the new head and the match body
compile_head_args(Head, NewHead, HeadCode) :-
   must_det_ll(
     (as_functor_args(Head,Functor,A,Args),
      maplist(compile_one_head_arg(Head),NewArgs,Args,CodeL),
      as_functor_args(NewHead,Functor,A,NewArgs),
      list_to_conjuncts(CodeL,HeadCode))),!.

compile_one_head_arg(_Head, NewArg, Arg, true):- is_ftVar(Arg),NewArg=Arg,!.
compile_one_head_arg(_Head, NewArg, Arg, u_assign(NewArg,Arg)):- is_ftVar(Arg),!.
compile_one_head_arg(_Head, NewArg, Arg, eval_true(NewArg)):- Arg=='True',!.
compile_one_head_arg(_Head, NewArg, Arg, eval_false(NewArg)):- Arg=='False',!.
compile_one_head_arg(_Head, NewArg, Arg, u_assign(NewArg,Arg)):-!.
%compile_one_head_arg(_Head, NewArg, Arg, (NewArg=~Arg)):- data_term(Arg),!.
%compile_one_head_arg(_Head, NewArg, Arg, (NewArg=~Arg)):- !.
%compile_one_head_arg(Head, NewArg, Arg, Code):- f2p_assign(10,Head,NewArg,Arg,Code).


compile_head_variablization(Head, NewHead, HeadCode) :-
   must_det_ll(
     (as_functor_args(Head,Functor,A,Args),
      % Find non-singleton variables in Args
      fix_non_singletons(Args, NewArgs, Conditions),
      list_to_conjunction(Conditions,HeadCode),
      as_functor_args(NewHead,Functor,A,NewArgs))).


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
   fix_non_singletons(NewArgsM, NewArgs, Conditions), !.
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


compile_for_assert_0(boolean,HeadC,_Nth,CodeForHeadArgs,Result,AsBodyFn, Converted):-
   \+ is_ftVar(Result),
   must_det_ll((f2p(HeadC,'True',AsBodyFn,NextBody),
            combine_code(CodeForHeadArgs,NextBody,BodyC),
            optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),
            Convert = (HeadCC :- BodyCC),
            fix_equals_in_head(Convert,Converted))).


compile_for_assert_0(_Which,HeadC,_Nth,CodeForHeadArgs,Result,AsBodyFn, Converted):-
   \+ is_ftVar(Result),
   must_det_ll((f2p(HeadC,'True',AsBodyFn,NextBody),
            combine_code(CodeForHeadArgs,NextBody,BodyC),
            optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),
            Convert = (HeadCC :- BodyCC),
            fix_equals_in_head(Convert,Converted))).

compile_for_assert_0(Which,HeadC,_Nth,CodeForHeadArgs,Result,AsBodyFn, Converted):-
  must_det_ll((
    (Which == func ->
      f2p(HeadC,Result,AsBodyFn,NextBody);
      f2p(HeadC,Result,AsBodyFn,NextBody)),
    combine_code(CodeForHeadArgs,NextBody,BodyC),
    optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),
    Convert = (HeadCC :- BodyCC),
    fix_equals_in_head(Convert,Converted))).



compile_for_assert_eq('=',HeadInC, AsBodyFnC, Converted):-
    subst_vars(['=',HeadInC, AsBodyFnC],['=',HeadIn, AsBodyFn],NamedVarsList),
    maplist(cname_var,NamedVarsList),!,
    compile_for_assert(HeadIn, AsBodyFn, Converted).
compile_for_assert_eq(':-',HeadIn, BodyIn, Converted):-
    call(ensure_corelib_types),
    Converted=(H:-B), s2p(HeadIn,H), s2p(BodyIn,B),!.


ensure_corelib_types.


compile_for_assert(HeadInC, AsBodyFn, ConvertedO):-
 call(ensure_corelib_types),
 compile_for_assert_now(HeadInC, AsBodyFn, Converted),
 continue_opimize(Converted,ConvertedO).

compile_for_assert_now(HeadInC, AsBodyFn, Converted):-
    r2p(boolean,HeadInC,ResultV,HeadInP),
    as_functor_args(HeadInP,F,A,PArgs),
    compile_head_for_assert([F|PArgs], [F|NewFArgs],
       CodeForHeadArgs),
    as_functor_args(HeadC,F,A,NewFArgs),
    compile_for_assert_0(boolean,HeadC,
      -1,(ResultV = Result, CodeForHeadArgs),Result,AsBodyFn, Converted).

compile_for_assert_now(HeadInC, AsBodyFn, Converted):-
 must_det_ll(
   (r2p(Which,HeadInC,Result,HeadInP),
    as_functor_args(HeadInP,F,A,PArgs),
    once((nth1(Nth,PArgs,Arg,FArgs), Arg == Result)),
    compile_head_for_assert([F|FArgs], [F|NewFArgs], CodeForHeadArgs),
    nth1(Nth, NewPArgs, Arg, NewFArgs),
    as_functor_args(HeadC,F,A,NewPArgs),
    compile_for_assert_0(Which,HeadC,Nth,CodeForHeadArgs,Result,AsBodyFn, Converted))).


compile_for_assert(HeadIs,RetType, AsBodyFn, ConvertedO) :-
  compile_for_assert_01(HeadIs,RetType, AsBodyFn, Convert),
  fix_equals_in_head(Convert,Converted),
  continue_opimize(Converted,ConvertedO).

compile_for_assert_01(HeadIs,RetType, AsBodyFn, Converted) :-
     (AsBodyFn =@= HeadIs,RetType ; AsBodyFn == []), !,/*trace,*/
   compile_fact_for_assert(HeadIs,RetType,Converted).

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.

compile_for_assert_01(Head, AsBodyFn, Converted) :-
   once(compile_head_variablization(Head, HeadC, CodeForHeadArgs)),
   \+(atomic(CodeForHeadArgs)), !,
   compile_for_assert_01(HeadC,
      (CodeForHeadArgs,AsBodyFn), Converted).

compile_for_assert_01(HeadIn, AsBodyFn, Converted) :-
    r2p(HeadIn,HResult,Head),
    compile_for_assert_02(HResult,Head, AsBodyFn, Converted).
compile_for_assert_01(HeadIn, AsBodyFn, Converted) :-
    compile_for_assert_02(_, HeadIn, AsBodyFn, Converted).



compile_for_assert_02(HResult,HeadIs,RetType, AsBodyFn, Converted)
 :- is_nsVar(AsBodyFn),
     AsFunction = HeadIs,!,
     must_det_ll((
     Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
     %funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, _Nth, Head),
     f2p(HeadIs,RetType,HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
       funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, _Nth, Head)),
     NextBody = u_assign(AsBodyFn,Result),
   optimize_head_and_body(Head,NextBody,HeadC,BodyC),
     cname_var('HEAD_RES',Result))),!.

compile_for_assert_02(HResult,HeadIs,RetType, AsBodyFn, Converted) :-
    ar2p(HeadIs,RetType,HResult,NewHead),
     AsFunction = HeadIs,RetType,
     must_det_ll((
     Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   /*funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, _Nth, Head),*/
     f2p(NewHead,HResult,AsFunction,HHead),
     (var(HResult) -> (Result = HResult, HHead = Head) ;
        funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, _Nth, Head)),
   %verbose_unify(Converted),
     f2p(HeadIs,RetType,Result,AsBodyFn,NextBody),
     %RetResult = Converted,
     %RetResult = _,
   optimize_head_and_body(Head,NextBody,HeadC,NextBodyC),
   %fbug([convert(Convert),optimize_head_and_body(HeadC:-NextBodyC)]),
     %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),

     cname_var('HEAD_RES',Result))),!.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert_02(HResult,HeadIs,RetType, AsBodyFn, Converted) :-
   Result = HResult,
   AsFunction = HeadIs,RetType, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, _Nth, Head),
   compile_head_args(Head,HeadC,CodeForHeadArgs),
   f2p(HeadIs,RetType,Result,AsBodyFn,NextBody),
   combine_code(CodeForHeadArgs,NextBody,BodyC),!,
   optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),!.

/*
*/
optimize_head_and_body(Head,Body,HeadNewest,BodyNewest):-
   label_body_singles(Head,Body),
   (merge_and_optimize_head_and_body(Head,Body,HeadNew,BodyNew),
      (((Head,Body)=@=(HeadNew,BodyNew))
      ->  (HeadNew=HeadNewest,BodyNew=BodyNewest)
      ;

  (color_g_mesg('#404064',print_pl_source(( HeadNew :- BodyNew))),
    optimize_head_and_body(HeadNew,BodyNew,HeadNewest,BodyNewest)))).

continue_opimize(HB,(H:-BB)):- expand_to_hb(HB,H,B), must_optimize_body(HB,B,BB),!.
continue_opimize(Converted,Converted).



merge_and_optimize_head_and_body(Head,Converted,HeadO,Body):- nonvar(Head),
   Head = (PreHead,True),!,
   merge_and_optimize_head_and_body(PreHead,(True,Converted),HeadO,Body).
merge_and_optimize_head_and_body(AHead,Body,Head,BodyNew):-
    assertable_head(AHead,Head),
   must_optimize_body(Head,Body,BodyNew).

assertable_head(u_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), symbol(F), Head=..[F|NewArgs],!.
assertable_head(Head,Head).

label_body_singles(Head,Body):-
   term_singletons(Body+Head,BodyS),
   maplist(label_body_singles_2(Head),BodyS).
label_body_singles_2(Head,Var):- sub_var(Var,Head),!.
label_body_singles_2(_,Var):- ignore(Var='$VAR'('_')).



metta_predicate(u_assign(evaluable,eachvar)).
metta_predicate(eval_true(matchable)).
metta_predicate(with_space(space,matchable)).
metta_predicate(limit(number,matchable)).
metta_predicate(findall(template,matchable,listvar)).
metta_predicate(match(space,matchable,template,eachvar)).


must_optimize_body(A,B,CC):- once(optimize_body(A,B,C)), C \=@= B,!, must_optimize_body(A,C,CC).
must_optimize_body(_,B,C):- B =C.

optimize_body(_HB,Body,BodyNew):- is_nsVar(Body),!,Body=BodyNew.
%optimize_body( HB,u_assign(VT,R),u_assign(VT,R)):-!, must_optimize_body(HB,VT,VTT).
optimize_body( HB,with_space(V,T),with_space(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,call(T),call(TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,rtrace_on_error(T),rtrace_on_error(TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,limit(V,T),limit(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,findall_ne(V,T,R),findall_ne(V,TT,R)):-!,
 expand_to_hb(HB,H,_), must_optimize_body((H:-findall_ne(V,T,R)),T,TT).
optimize_body( HB,findall(V,T,R),findall(V,TT,R)):-!,
 expand_to_hb(HB,H,_),
 must_optimize_body((H:-findall(V,T,R)),T,TT).
optimize_body( HB,loonit_assert_source_tf(V,T,R3,R4),
  loonit_assert_source_tf(V,TT,R3,R4)):-!,
  must_optimize_body(HB,T,TT).
optimize_body( HB,loonit_assert_source_empty(V,X,Y,T,R3,R4),
  loonit_assert_source_empty(V,X,Y,TT,R3,R4)):-!,
  must_optimize_body(HB,T,TT).

optimize_body( HB,(B1*->B2;B3),(BN1*->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2), optimize_body(HB,B3,BN3).
optimize_body( HB,(B1->B2;B3),(BN1->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), must_optimize_body(HB,B2,BN2), must_optimize_body(HB,B3,BN3).
optimize_body( HB,(B1:-B2),(BN1:-BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1*->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1;B2),(BN1;BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1,B2),(BN1)):- optimize_conjuncts(HB,(B1,B2),BN1).
%optimize_body(_HB,==(Var, C), Var=C):- self_eval(C),!.
optimize_body( HB,u_assign(A,B),R):- optimize_u_assign_1(HB,A,B,R),!.
optimize_body( HB,eval(A,B),R):- optimize_u_assign_1(HB,A,B,R),!.
%optimize_body(_HB,u_assign(A,B),u_assign(AA,B)):- p2s(A,AA),!.
optimize_body(_HB,Body,BodyNew):- optimize_body_unit(Body,BodyNew).

optimize_body_unit(I,O):-
   copy_term(I,II),
   optimize_unit1(I,Opt),I=@=II,!,
   optimize_body_unit(Opt,O).
optimize_body_unit(O,O).


ok_to_append('$VAR'):- !, fail.
ok_to_append(_).

number_wang(A,B,C):-
  (numeric(C);numeric(A);numeric(B)),!,
  maplist(numeric_or_var,[A,B,C]),
  maplist(decl_numeric,[A,B,C]),!.

optimize_u_assign_1(_,Var,_,_):- is_nsVar(Var),!,fail.


optimize_u_assign_1(_HB,[H|T],R,Code):- symbol(H),length([H|T],Arity),
   predicate_arity(F,A),Arity==A, \+ (predicate_arity(F,A2),A2\=A),
    append([H|T],[R],ArgsR),Code=..ArgsR,!.
optimize_u_assign_1(HB,Compound,R,Code):- \+ compound(Compound),!, optimize_u_assign(HB,Compound,R,Code).
optimize_u_assign_1(HB,[H|T],R,Code):- !, optimize_u_assign(HB,[H|T],R,Code).
optimize_u_assign_1(_ ,Compound,R,Code):-
   is_list(R),var(Compound),
   into_u_assign(R,Compound,Code),!.

optimize_u_assign_1(_,Compound,R,Code):- ar2p(Compound,R,Code),!.
optimize_u_assign_1(_,Compound,R,Code):-
  compound(Compound),
  as_functor_args(Compound,F,N0), N is N0 +1,
  (predicate_arity(F,N); functional_predicate_arg(F, N, N)),
   append_term_or_call(Compound,R,Code).
optimize_u_assign_1(HB,Compound,R,Code):- p2s(Compound,MeTTa),   optimize_u_assign(HB,MeTTa,R,Code).
%optimize_u_assign_1(_,[Pred| ArgsL], R, u_assign([Pred| ArgsL],R)).

append_term_or_call([F|Compound],R,Code):- symbol(F),
  is_list(Compound),append(Compound,[R],CodeL),
            Code=..[F|CodeL],!.
append_term_or_call(F,R,Code):- symbol(F),!, Code=..[F,R].
append_term_or_call(F,R,Code):- append_term(F,R,Code),!.
append_term_or_call(F,R,call(F,R)).


optimize_unit1(
  eval_true([==, ['get-metatype', A], 'Expression']),
  call('get-metatype',A,'Expression')).

optimize_unit1( eval_true([==, [GM, Eval], Val]), call(GM,Eval,Val)):-
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

optimize_unit1( ==([GM,Eval],Val,C), call(GM,Eval,Val)):- C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

optimize_u_assign(_,[Var|_],_,_):- is_nsVar(Var),!,fail.
optimize_u_assign(_,[Empty], _, (!,fail)):-  Empty == empty,!.
optimize_u_assign(_,[EqEq,[GM,Eval],Val],C, call(GM,Eval,Val)):-
    EqEq == '==',C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.


optimize_u_assign(_,[+, A, B], C, plus(A , B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, plus(B , C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[+, A, B], C, +(A , B, C)):- !.
optimize_u_assign(_,[-, A, B], C, +(B , C, A)):- !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,['/', A, B], C, *(B , C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- !.
optimize_u_assign(_,['/', A, B], C, *(B , C, A)):- !.
optimize_u_assign(_,[fib, B], C, fib(B, C)):- !.
optimize_u_assign(_,[fib1, A,B,C,D], R, fib1(A, B, C, D, R)):- !.
optimize_u_assign(_,['pragma!',N,V],Empty,set_option_value_interp(N,V)):-
   nonvar(N),ignore((fail,Empty='Empty')), !.
optimize_u_assign((H:-_),Filter,A,filter_head_arg(A,Filter)):- fail, compound(H), arg(_,H,HV),
  HV==A, is_list(Filter),!.
optimize_u_assign(_,[+, A, B], C, '#='(C , A + B)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, '#='(C , A - B)):- number_wang(A,B,C), !.
optimize_u_assign(_,[match,KB,Query,Template], R, Code):-  match(KB,Query,Template,R) = Code.

optimize_u_assign(HB,MeTTaEvalP, R, Code):- \+ is_nsVar(MeTTaEvalP),
  compound_non_cons(MeTTaEvalP), p2s(MeTTaEvalP,MeTTa),
  MeTTa\=@=MeTTaEvalP,!, optimize_body(HB, u_assign(MeTTa, R), Code).

% optimize_u_assign(_,_,_,_):- !,fail.
optimize_u_assign((H:-_),[Pred| ArgsL], R, Code):- var(R), symbol(Pred), ok_to_append(Pred),
  append([Pred| ArgsL],[R], PrednArgs),Code=..PrednArgs,
  (H=..[Pred|_] -> nop(set_option_value('tabling',true)) ; current_predicate(_,Code)),!.

p2s(P,S):- into_list_args(P,S).

get_decl_type(N,DT):- attvar(N),get_atts(N,AV),sub_term(DT,AV),symbol(DT).

numeric(N):- number(N),!.
numeric(N):- get_attr(N,'Number','Number').
numeric(N):- get_decl_type(N,DT),(DT=='Int',DT=='Number').
decl_numeric(N):- numeric(N),!.
decl_numeric(N):- ignore((var(N),put_attr(N,'Number','Number'))).
numeric_or_var(N):- var(N),!.
numeric_or_var(N):- numeric(N),!.
numeric_or_var(N):- \+ compound(N),!,fail.
numeric_or_var('$VAR'(_)).

non_compound(S):- \+ compound(S).

did_optimize_conj(Head,B1,B2,B12):- optimize_conj(Head,B1,B2,B12), B12\=@=(B1,B2),!.

optimize_conjuncts(Head,(B1,B2,B3),BN):- B3\=(_,_),
  did_optimize_conj(Head,B2,B3,B23),
  must_optimize_body(Head,(B1,B23),BN), !.
optimize_conjuncts(Head,(B1,B2,B3),BN):-
  did_optimize_conj(Head,B1,B2,B12),
  must_optimize_body(Head,(B12,B3),BN),!.
%optimize_conjuncts(Head,(B1,B2),BN1):- optimize_conj(Head,B1,B2,BN1).
optimize_conjuncts(Head,(B1,B2),BN1):- did_optimize_conj(Head,B1,B2,BN1),!.
optimize_conjuncts(Head,(B1*->B2),(BN1*->BN2)):- !,
  optimize_conjuncts(Head,B1,BN1),
  optimize_conjuncts(Head,B2,BN2).
optimize_conjuncts(Head,(B1->B2),(BN1->BN2)):- !,
  optimize_conjuncts(Head,B1,BN1),
  optimize_conjuncts(Head,B2,BN2).
optimize_conjuncts(Head,(B1;B2),(BN1;BN2)):- !,
  optimize_conjuncts(Head,B1,BN1),
  optimize_conjuncts(Head,B2,BN2).
optimize_conjuncts(Head,(B1,B2),(BN1,BN2)):- !,
   must_optimize_body(Head,B1,BN1), must_optimize_body(Head,B2,BN2).
optimize_conjuncts(_,A,A).

optimize_conj(_Head, B1,B2,eval_true(E)):-
        B2 = is_True(True_Eval),
        B1 = eval(E,True_Eval1),
        True_Eval1 == True_Eval,!.

optimize_conj(HB, RR, C=A, RR):- compound(RR),is_nsVar(C),is_nsVar(A),
  as_functor_args(RR,_,_,Args),is_list(Args), member(CC,Args),var(CC), CC==C,
    count_var(HB,C,N),N=2,C=A,!.

optimize_conj(_, u_assign(Term, C), u_assign(True,CC), eval_true(Term)):-
   'True'==True, CC==C.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, u_assign(Term, C), C=A, u_assign(Term,A)):- is_ftVar(C),is_ftVar(A),count_var(HB,C,N),N=2,!.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, B1,BT,B1):- assumed_true(HB,BT),!.
optimize_conj(HB, BT,B1,B1):- assumed_true(HB,BT),!.
%optimize_conj(Head, u_assign(Term, C), u_assign(True,CC), Term):- 'True'==True,
%     optimize_conj(Head, u_assign(Term, C), is_True(CC), CTerm).
%optimize_conj(Head,B1,BT,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
%optimize_conj(Head,BT,B1,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
optimize_conj(Head,B1,B2,(BN1,BN2)):-
   optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).

assumed_true(_ ,B2):- var(B2),!,fail.
assumed_true(HB,eval_true(B2)):-!,assumed_true(HB,B2).
assumed_true(_ ,B2):- B2==is_True('True').
assumed_true(_ ,B2):- B2=='True'.
assumed_true(_ ,B2):- B2== true,!.
assumed_true(_ ,B2):- B2==u_assign('True', '$VAR'('_')),!.
assumed_true(HB,X==Y):- !, assumed_true(HB,X=Y).
assumed_true( _,X=Y):- X==Y,!.
assumed_true(HB,X=Y):- is_nsVar(X),is_nsVar(Y),
  ( \+ (X\=Y)),(count_var(HB,Y,2);count_var(HB,X,2)),
  X=Y,!.


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



f2p(HeadIs,RetResult,Convert, Converted):-
  f2p(40,HeadIs,_ANY_,RetResult,Convert, Converted),!.


f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted):-
   Depth2 is Depth-1,
  f2q(Depth2,HeadIs,RetType,RetResult,Convert, Converting),
  convert_fromi(Depth2,Converting, Converted),!.
f2p(_Depth,_HeadIs,_RetType,RetResult,Convert, eval(Convert,RetResult)).


convert_fromi(_Depth,Converted, Converted):- is_ftVar(Converted),!.
convert_fromi(_Depth,Converted, Converted):- \+ compound(Converted),!.
%convert_fromi(_Depth, u_assign(E,R),  UA):-  !, u_assign(E,R)=UA.
convert_fromi(Depth,(A,B), (AA,BB)):- !, convert_fromi(Depth,A,AA), convert_fromi(Depth,B,BB).
convert_fromi(Depth,Converting, Converted):- is_list(Converting),!,maplist(convert_fromi(Depth),Converting, Converted).
convert_fromi(Depth,Converting, Converted):- compound_name_arguments(Converting,F,Args),!,
   maplist(convert_fromi(Depth),Args, NewArgs),!,
   compound_name_arguments(Converted,F,NewArgs).

%convert_fromi(Depth,Converting, Converted):- f2q(Depth,Converting, Converted).
is_fqVar(Var2):- is_ftVar(Var2),!.
is_fqVar(Var2):- symbol(Var2),!.


f2q(_Depth,_HeadIs,_,Var1, Var2,  ((Var1=Var2))):-
    is_fqVar(Var1),is_fqVar(Var2),!.

f2q(_Depth,_HeadIs,_RetType,RetVar, Convert, u_assign(Convert,RetVar)) :-
        is_ftVar(Convert),!.% Check if Convert is a variable

f2q(_Depth,_HeadIs,_RetType,_RetResult, u_assign(E,R),  UA):-  !,
    u_assign(E,R)=UA.


f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- % HeadIs,RetType\=@=Convert,
     is_arity_0(Convert,F), !, Converted = u_assign([F],RetResult),!.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert,eval_true(Convert)):-
   %nl,print(Convert),nl,
   as_functor_args(Convert,F,A,Args),
   \+ maplist(is_list,Args),
   is_absorbed_return_value(F,A,_Bool,RResult),RResult=RetResult.


f2q(Depth,HeadIs,RetType,RetResult,Convert,Converted) :- %dif_functors(HeadIs,Convert),
    Convert =~ 'match'(ESpace,Pattern,Template),!,
  must_det_ll((
    f2p(Depth,HeadIs,_SpaceT,SpaceV,ESpace,Code),
    %term_variables(Template,TemplateVars),
    compile_pattern(Depth,HeadIs,SpaceV,Pattern,SpacePatternCode),
    f2p(Depth,HeadIs,RetType,RetResult,Template,TemplateCode),
    combine_code((Code,SpacePatternCode),TemplateCode,Converted))).

/*
  f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(_Space,Match,Template),!,
   must_det_ll((
    f2p(Depth,HeadIs,RetType,_,Match,MatchCode),
    into_equals(RetResult,Template,TemplateCode),
    combine_code(MatchCode,TemplateCode,Converted))).
*/

compile_pattern(_Depth,_HeadIs,Space,Match,SpaceMatchCode):-
  SpaceMatchCode = metta_atom_iter(Space,Match).


interpet_this(_Convert):-!, fail.

interpet_this(Convert):- as_functor_args(Convert,F,A,Args), interpet_this(Convert,F,A,Args).
interpet_this(_,F,_,_):- \+ symbolic(F),!.
interpet_this(_,F,_,_):- compile_this_s(F),!,fail.
interpet_this(_,F,_,_):- interpet_this_f(F),!.
% stable workarround until the '=~' bug is fixed for numbers
interpet_this(Convert,F,A,Args):- compile_this(Convert,F,A,Args),!,fail.
interpet_this(_,_,_,_).

interpet_this_f(F):- metta_atom_file_buffer_isa(F,'Compiled'),!,fail.
interpet_this_f(F):- metta_atom_file_buffer_isa(F,'Interpreted'),!.
interpet_this_f(F):- op_decl(F, [ 'Number', 'Number' ], 'Number').

compile_this(Convert):- as_functor_args(Convert,F,A,Args), compile_this(Convert,F,A,Args).
compile_this(_,F,_,_):- \+ symbolic(F),!, fail.
compile_this(_,F,_,_):- compile_this_f(F),!.

compile_this_f(F):- metta_atom_file_buffer_isa(F,'Compiled').
compile_this_f(F):- interpet_this_f(F),!,fail.
compile_this_f(F):- compile_this_s(F),!.
compile_this_f(F):- metta_atom_file_buffer([':',F,[Ar|_]]), Ar=='->', !.
compile_this_s('superpose').
    compile_this_s('match').
    compile_this_s('do').
compile_this_s('do-all').


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,  dif_functors(HeadIs,Convert),
  get_inline_def(Convert,NewDef),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,NewDef,Converted))).

f2q(Depth,HeadIs,RetType,RetResult,Convert, do(Converted)) :- % dif_functors(HeadIs,Convert),
  ignore(RetResult='Empty'),
  Convert =~ ['do',Body],!,
  f2p(Depth,HeadIs,RetType,_RetResult,Body, Converted).

f2q(Depth,HeadIs,_RetTypeD,RetResult,Convert, (doall(Converted),RetResult='Empty')) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['do-all',Body],!,
  f2p(Depth,HeadIs,_RetTypeB,_RetResultB,Body, Converted).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-  % dif_functors(HeadIs,Convert),
  Convert =~ ['let',Var,Value1,Body],!,
    f2p(Depth,HeadIs,_,ResValue1,Value1,CodeForValue1),
    into_equals(Var,ResValue1,CodeEquals),
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
  list_to_conjuncts([CodeForValue1,CodeEquals,BodyCode],Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['let',Var,Value1,Body],!,
  f2p(Depth,HeadIs,_,Var,Value1, BindingCode),
  f2p(Depth,HeadIs,RetType,RetResult,Body, BodyCode),
   combine_code(BindingCode,BodyCode,Converted).




is_Nil(Nil):- Nil==[],!.
is_Nil(Nil):- Nil=='Nil',!.
is_Nil(Nil):- Nil=='()',!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- %dif_functors(HeadIs,Convert),
  Convert =~ ['let*',Nil,Body],is_Nil(Nil), !,
   must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,Body, Converted))).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- %dif_functors(HeadIs,Convert),
  Convert =~ ['let*',AAAA,Body],AAAA=~[VE|Bindings],VE=~[V,E],
  f2q(Depth,HeadIs,RetType,RetResult,['let',V,E,['let*',Bindings,Body]], Converted).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail, dif_functors(HeadIs,Convert),
  Convert =~ ['let*',Bindings,Body],!,
   must_det_ll((
    maplist(compile_let_star(Depth,HeadIs,RetType),Bindings,CodeList),
    list_to_conjuncts(CodeList,BindingCode),
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    combine_code(BindingCode,BodyCode,Converted))).

compile_let_star(Depth,HeadIs,RetType,NV,Converted):-
 must_det_ll((NV =~ [Expression,Var],
 (var(Var)-> f2p(Depth,HeadIs,RetType,Var,Expression,Converted);
 (var(Expression)-> f2p(Depth,HeadIs,RetType,Expression,Var,Converted);
 (f2p(Depth,HeadIs,RetType,Eval1Result,Expression,Code),
  into_equals(Eval1Result,Var,Eval1ResultVar),
  combine_code(Code,Eval1ResultVar,Converted)))))).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['superpose',COL],compound_equals(COL,'collapse'(Value1)), !,
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    Converted = (find_ne(ResValue1,CodeForValue1,Gathered),member(RetResult,Gathered)).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
   Convert =~ ['sequential'|ValueL],
   ReConvert =~ ['superpose'|ValueL],!,
   f2q(Depth,HeadIs,RetType,RetResult,ReConvert, Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, (Converted)) :-
    Convert =~ ['sequential',ValueL],is_list(ValueL),!,
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    maplist(f2p_assign(Depth,HeadIs,RetType),RetResultL,ValueL,CodeForValueL),
    last(RetResultL,RetResult),
    list_to_conjuncts(CodeForValueL,Converted),!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_list(ValueL),!,
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    must_det_ll(( ignore(cname_var('SP_Ret',RetResult)),
    maplist(f2p(Depth,HeadIs,RetType,RetResult),ValueL,CodeForValueL),
    list_to_disjuncts(CodeForValueL,Converted))),!.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_nsVar(ValueL),!,
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    Converted = call('superpose'(ValueL,RetResult)),
    cname_var('MeTTa_SP_',ValueL).


:- op(700,xfx, =~).
f2q(Depth,HeadIs,RetType,RetResult,Convert, (Code1,Eval1Result=Result,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'chain'(Eval1,Result,Eval2),!,
   f2p(Depth,HeadIs,RetType,Eval1Result,Eval1,Code1),
   f2p(Depth,HeadIs,RetType,RetResult,Eval2,Converted).

f2q(Depth,HeadIs,RetType,ResValue2,Convert, (CodeForValue1,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['eval-in-space',Value1,Value2],
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
   Converted = with_space(ResValue1,CodeForValue2).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  once(Convert =~ 'if'(Cond,Then,Else);Convert =~ 'If'(Cond,Then,Else)),
  !,Test = is_True(CondResult),
  f2p(Depth,HeadIs,RetType,CondResult,Cond,CondCode),
  compile_test_then_else(Depth,RetResult,(CondCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  once(Convert =~ 'if'(Cond,Then);Convert =~ 'If'(Cond,Then)),
  f2p(Depth,HeadIs,RetType,CondResult,Cond,CondCode),
    f2p(Depth,HeadIs,RetType,RetResult,Then,ThenCode),
    Converted = ((CondCode,iz_True(CondResult)),ThenCode).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-error'(Value,Then,Else),!,Test = is_Error(ValueResult),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
  compile_test_then_else(Depth,RetResult,(ValueCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-empty'(Value,Then,Else),!,Test = is_Empty(ValueResult),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
  compile_test_then_else(Depth,RetResult,(ValueCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  (Convert =~ 'if-non-empty-expression'(Value,Then,Else)),!,
  (Test = ( \+ is_Empty(ValueResult))),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
  compile_test_then_else(Depth,RetResult,(ValueCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ ['if-equals',Value1,Value2,Then,Else],!,Test = equal_enough(ResValue1,ResValue2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
  compile_test_then_else(Depth,RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).

cname_var(Sym,_Src):- var(Sym),!.
cname_var(Sym,Src):-  var(Src),!,must_det_ll((gensym(Sym,SrcV),Src='$VAR'(SrcV))).
cname_var(Sym,Src):-  Src='$VAR'(_),!,must_det_ll((gensym(Sym,SrcV),nb_setarg(1,Src,SrcV))).
cname_var(_Sym,_Src).
cname_var(Name=Var):- cname_var(Name,Var).
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['assertEqual',Value1,Value2],!,
    cname_var('Src_',Src),
    cname_var('FA_',ResValue1),
    cname_var('FA_',ResValue2),
    cname_var('FARL_',L1),
    cname_var('FARL_',L2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
    Converted =
              (Src = Convert,
               loonit_assert_source_tf_empty(Src,L1,L2,
                (findall_ne(ResValue1,CodeForValue1,L1),
                 findall_ne(ResValue2,CodeForValue2,L2)),
                 equal_enough_for_test(L1,L2),RetResult)).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['assertEqualToResult',Value1,Value2],!,
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    Src = Convert,
    Goal = findall_ne(ResValue1,CodeForValue1,L1),
    Converted = (
                loonit_assert_source_tf_empty(Src,L1,Value2,
                     Goal,
                     equal_enough_for_test(L1,Value2),RetResult)).

maybe_unlistify([UValueL],ValueL,RetResult,[URetResult]):- fail, is_list(UValueL),!,
  maybe_unlistify(UValueL,ValueL,RetResult,URetResult).
maybe_unlistify(ValueL,ValueL,RetResult,RetResult).

list_to_disjuncts([],false).
list_to_disjuncts([A],A):- !.
list_to_disjuncts([A|L],(A;D)):-  list_to_disjuncts(L,D).


%f2p_assign(Depth,_HeadIs,_RetType,V,Value,is_True(V)):- Value=='True'.
f2p_assign(_Depth,_HeadIs,_RetType,ValueR,Value,ValueR=Value):- is_nsVar(Value),!.
f2p_assign(_Depth,_HeadIs,_RetType,ValueR,Value,ValueR=Value):- \+ compound(Value),!.
f2p_assign(_Depth,_HeadIs,_RetType,ValueResult,Value,Converted):-
  ar2p(Value,ValueResult,Converted),!.
f2p_assign(Depth,HeadIs,RetType,ValueResult,Value,Converted):-
   f2p(Depth,HeadIs,RetType,ValueResultR,Value,CodeForValue),
   %into_equals(ValueResultR,ValueResult,ValueResultRValueResult),
   ValueResultRValueResult = (ValueResultR=ValueResult),
   combine_code(CodeForValue,ValueResultRValueResult,Converted).


f2p_arg(_Depth,_HeadIs,_RetType,Value,Value,true):- is_nsVar(Value),!.
f2p_arg(_Depth,_HeadIs,_RetType,Value,Value,true):- \+ compound(Value),!.
f2p_arg(_Depth,_HeadIs,_RetType,ValueResult,Value,Converted):- ar2p(Value,ValueResult,Converted),!.
f2p_arg(Depth,HeadIs,RetType,ValueResult,Value,Converted):-
   f2p_assign(Depth,HeadIs,RetType,ValueResult,Value,Converted).


f2q(Depth,HeadIs,RetType,RetResult,Convert, keep(Converted)) :-
  Convert =~ ['case',Value,PNil],[]==PNil,!,Converted = (ValueCode,RetResult=[]),
      f2p(Depth,HeadIs,RetType,_ValueResult,Value,ValueCode).


f2q(Depth,HeadIs,RetType,RetResult,Convert, (ValueCode, Converted)) :-
  Convert =~ ['case',Value|Options], \+ is_nsVar(Value),!,
  cname_var('CASE_VAR_',ValueResult),
  f2q(Depth,HeadIs,RetType,RetResult,['case',ValueResult|Options], Converted),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,Options],!,
   must_det_ll((
    maplist(compile_case_bodies(Depth,HeadIs,RetType),Options,Cases),
    cname_var('SWITCH_',AllCases),
    cname_var('CASE_RESULT_',RetResult),
    Converted =
           ( AllCases = Cases,
             select_case(AllCases,Value,RetResult)))).

select_case(AllCases,Value,BodyResult):-
       once((member(caseOption(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
             rtrace_on_error(MatchCode),unify_enough(Value,MatchVar)))
             ,!,
       rtrace_on_error(BodyCode).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,[Opt|Options]],nonvar(Opt),!,
   must_det_ll((
    compile_case_bodies(Depth,HeadIs,RetType,Opt,caseOption(Value,If,RetResult,Then)),
    Converted = ( If -> Then ; Else ),
    ConvertCases =~ ['case',Value,Options],
    f2q(Depth,HeadIs,RetType,RetResult,ConvertCases,Else))).


/*
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,Options],!,
   must_det_ll((
    maplist(compile_case_bodies(Depth,HeadIs,RetType),Options,Cases),
    Converted =
        (( AllCases = Cases,
           once((member(caseOption(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
                 (MatchCode,unify_enough(Value,MatchVar)))),
           (BodyCode),
           BodyResult=RetResult)))).

f2q(Depth,HeadIs,RetType,_,Convert, Converted) :-
  Convert =~ ['case',Value,Options,RetResult],!,
   must_det_ll((
    f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
    maplist(compile_case_bodies(Depth,HeadIs,RetType),Options,Cases),
    Converted =
        (( AllCases = Cases,
           call(ValueCode),
           once((member(caseOption(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
                 both_of(ValueResult,MatchCode,unify_enough(ValueResult,MatchVar)))),
           call(BodyCode),
           BodyResult=RetResult)))).


both_of(Var,G1,G2):- nonvar(Var),!,call(G2),call(G1).
both_of(_Var,G1,G2):- call(G1),call(G2).

*/

compile_case_bodies(Depth,HeadIs,RetType,[Match,Body],caseOption(_,true,BodyResult,BodyCode)):- Match == '%void%',!,
      f2p(Depth,HeadIs,RetType,BodyResult,Body,BodyCode).
compile_case_bodies(Depth,HeadIs,RetType,[Match,Body],caseOption(MatchResult,If,BodyResult,BodyCode)):- !,
      f2p(Depth,HeadIs,RetType,MatchResultV,Match,MatchCode),
      combine_code(MatchCode,unify_enough(MatchResult,MatchResultV),If),
      f2p(Depth,HeadIs,RetType,BodyResult,Body,BodyCode).
compile_case_bodies(Depth,HeadIs,RetType,MatchBody,CS):- compound(MatchBody), MatchBody =~ MB,compile_case_bodies(Depth,HeadIs,RetType,MB,CS).


compound_equals(COL1,COL2):- COL1=@=COL2,!,COL1=COL2.
compound_equals(COL1,COL2):- compound_equals1(COL1,COL2).
compound_equals1(COL1,COL2):- is_nsVar(COL1),!,is_nsVar(COL2),ignore(COL1=COL2),!.
compound_equals1(COL1,COL2):- compound(COL1),!,compound(COL2), COL1=COL2.


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['collapse',Value1],!,
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    Converted = (findall_ne(ResValue1,CodeForValue1,RetResult)).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['compose',Value1],!,
    Convert2 =~ ['collapse',Value1],!,
    f2q(Depth,HeadIs,RetType,RetResult,Convert2, Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['unify',Value1,Value2,Then,Else],!,Test = metta_unify(ResValue1,ResValue2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
  compile_test_then_else(Depth,RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).




f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),

   Convert =~ ['if-decons',Atom,Head,Tail,Then,Else],!,Test = unify_cons(AtomResult,ResHead,ResTail),
    f2p(Depth,HeadIs,RetType,AtomResult,Atom,AtomCode),
    f2p(Depth,HeadIs,RetType,ResHead,Head,CodeForHead),
    f2p(Depth,HeadIs,RetType,ResTail,Tail,CodeForTail),
    compile_test_then_else(Depth,RetResult,(AtomCode,CodeForHead,CodeForTail,Test),Then,Else,Converted).


f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, was_True(RetResult)) :- is_compiled_and(AND),
   Convert =~ [AND],!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body],!,
   f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    compile_test_then_else(Depth,RetResult,BodyCode,'True','False',Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(Depth,HeadIs,RetType,B1Res,Body1,Body1Code),
   f2p(Depth,HeadIs,RetType,RetResult,Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   Converted = (Body1Code,AE,Body2Code),!.


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(Depth,HeadIs,RetType,B1Res,Body1,Body1Code),
   f2p(Depth,HeadIs,RetType,_,Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   compile_test_then_else(Depth,RetResult,(Body1Code,AE,Body2Code),'True','False',Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2|BodyMore],!,
   And2 =~ [AND,Body2|BodyMore],
   Next =~ [AND,Body1,And2],
   f2q(Depth,HeadIs,RetType,RetResult, Next, Converted).

% If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
f2q(Depth,HeadIs,RetType,RetResult,SOR,or(AsPredO, Converted)) :-
  SOR =~ or(AsPredI, Convert),
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.
f2q(Depth,HeadIs,RetType,RetResult,or(AsPredI,Convert), (AsPredO *-> true; Converted)) :- fail, !,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).
f2q(Depth,HeadIs,RetType,RetResult,(AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).

'True'(X):- ignore(is_True(X)).
'False'(X):- is_False(X).

get_inline_case_list(HeadDef,Quot,CaseList):-
   findall([HeadDef,NewDef],get_inline_def1(HeadDef,NewDef),DefList),DefList\==[],
   findall([Quot,NewDef],member([HeadDef,NewDef],DefList),CaseList).

get_inline_def(HeadDef,NewDef):-
   findall(NewDef,get_inline_def1(HeadDef,NewDef),EachDef), EachDef\==[],
   disj_def(EachDef,NewDef).



get_inline_def1(HeadDef,NewDef):-
   into_list_args(HeadDef,UHeadDef),
   copy_term(UHeadDef,CopyUHeadDef),
   [UHead|_UArgs] = UHeadDef, nonvar(UHead),
   metta_atom_file_buffer([Eq,UHeadDef|Body]),Eq=='=', once(xform_body(Body,NewDef)),
   (UHeadDef=@=CopyUHeadDef).

%xform_body([Body],Body):-!.
%xform_body(Items,[progn|Body]).

xform_body(Var,Var):-is_ftVar(Var), !.
xform_body([],call(true)):-!.
xform_body([Body],Body):-!.
xform_body([Body1,Body2],(Body1,Body2)):-!.
xform_body([Body1|Body2L],(Body1,Body2)):-xform_body(Body2L,Body2).

disj_def(Var,Var):-is_ftVar(Var), !.
disj_def([],call(fail)):-!.
disj_def([Body],Body):-!.
disj_def([Body1,Body2],(Body1;Body2)):-!.
disj_def([Body1|Body2L],(Body1;Body2)):-disj_def(Body2L,Body2).


/*
f2q(Depth,HeadIs,RetType,RetResult,transpose(Convert), Converted,Code) :- !,
   maplist(each_result(Depth,HeadIs,RetType,RetResult),Convert, Converted),
   list_to_disjuncts(Converted,Code).
*/
/*
f2q(Depth,HeadIs,RetType,RetResult,Convert, once(u_assign(Body,RetResult))) :-
  Convert=~ first_of(Body), is_ftVar(Body),!.
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert=~ first_of(Body),
  must_det_ll((as_functor_args(Body,F,A,Args),
  as_functor_args(Quot,quot,A,NewArgs),
  as_functor_args(QConvert,quot,A,Args))),
  get_inline_case_list([F|NewArgs],Quot,DefList),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,[case,QConvert,DefList],Converted))).*/
f2q(Depth,HeadIs,RetType,RetResult,Convert, once(Converted)) :-
  Convert=~ first_of(Body),!, f2p(Depth,HeadIs,RetType,RetResult,Body,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, catch(BodyCode,Ex,HandlerCode)) :-
    Convert=~ catch(Body,E,Handler),!, s2p(E,Ex),
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    f2p(Depth,HeadIs,RetType,RetResult,Handler,HandlerCode).

f2q(Depth,HeadIs,RetType,RetResult,Convert, call_cleanup(BodyCode,HandlerCode)) :-
    Convert=~ finally(Body,Handler),!,
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    f2p(Depth,HeadIs,RetType,RetResult,Handler,HandlerCode).



f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail, dif_functors(HeadIs,Convert),
  get_inline_def(Convert,InlineDef),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,InlineDef,Converted))).

% If Convert is a ":-" (if) function, we convert it to the equivalent ":-" (if) predicate.
f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, Converted) :- Convert =(H:-B),!,
  RetResult=(H:-B), Converted = true.

% If Convert is a "," (and) function, we convert it to the equivalent "," (and) predicate.
f2q(Depth,HeadIs,RetType,RetResult,SOR,[Comma,AsPredO, Converted]) :-
      SOR =~ [Comma, AsPredI, Convert], ',' == Comma,
      must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
                   f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.

f2q(Depth,HeadIs,RetType,RetResult,SOR, (AsPredO, Converted)) :-
      SOR =~ [Comma, AsPredI, Convert], 'and' == Comma,
            must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
                   f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.


% If Convert is a "not" function, we convert it to the equivalent ";" (or) predicate.
f2q(Depth,HeadIs,RetType,RetResult,Convert, \+ eval_true(AsPredO)) :- !,
  Convert =~ not(AsPredI),
  must_det_ll(f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO)).

each_result(Depth,HeadIs,RetType,RetResult,Convert,Converted):-
   f2p(Depth,HeadIs,RetType,OneResult,Convert,Code1),
   into_equals(OneResult,RetResult,Code2),
   combine_code(Code1,Code2,Converted).

 /*
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ if(Cond,Then),!,
   f2p(Depth,HeadIs,RetType,CondResult,Cond,CondCode),
   f2p(Depth,HeadIs,RetType,RetResult,Then,ThenCode),
   Converted = ((CondCode,is_True(CondResult)),ThenCode).

f2q(Depth,HeadIs,RetType,RetResult,Converter, Converted):-
   de_eval(Converter,Convert),!,
   f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted).

f2q(Depth,HeadIs,RetType,_Result,Convert, Converted)
  :- fail,
  as_functor_args(Convert,Func,PA),
   functional_predicate_arg(Func,PA,Nth),
   Convert =~ [Func|PredArgs],
   nth1(Nth,PredArgs,Result,FuncArgs),
   RetResult = Result,
   AsFunct =~ [Func|FuncArgs],
  f2p(Depth,HeadIs,RetType,RetResult,AsFunct, Converted).
*/

f2q(Depth,_HeadIs,_RetType,RetVar, Convert, u_assign(Convert,RetVar)) :- Depth=<0,!.



f2q(_Depth,_HeadIs,_RetType,Convert, Convert, true) :-
 (number(Convert)),!.% Check if Convert is a variable

f2q(_Depth,_HeadIs,_RetType,Convert, Convert, true) :-
    (data_term(Convert)),!.% Check if Convert is a variable

f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, (RetResult =~ Convert)) :-
     (data_term(Convert)),!.% Check if Convert is a variable

% If Convert is a variable, the corresponding predicate is just u_assign(Convert, RetResult)
f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = u_assign(Convert, RetResult).  % Set Converted to u_assign(Convert, RetResult)
f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, RetResultConverted) :-
     number(Convert),!,into_equals(RetResult,Convert,RetResultConverted).


f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, eval(Convert,RetResult)):-
   interpet_this(Convert),!.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert,Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ [H|_], \+ symbol(H), \+ is_non_evaluatable(H),
    Converted = (Convert=RetResult),!.


get_first_p1(_,Cmpd,_):- \+ compound(Cmpd),!, fail.
get_first_p1(E,Cmpd,set_nth1(N1,Cmpd)):- is_list(Cmpd),   nth1(N1,Cmpd,E).
get_first_p1(E,Cmpd,Result)           :- is_list(Cmpd),!, member(Ele,Cmpd), get_first_p1(E,Ele,Result).
get_first_p1(_,Cmpd,_)                :- is_conz(Cmpd),!,fail.
get_first_p1(E,Cmpd,set_arg(N1,Cmpd)) :- arg(N1,Cmpd,E).
get_first_p1(E,Cmpd,Result)           :- arg(_,Cmpd,Ele),!,get_first_p1(E,Ele,Result).

non_simple_arg(E):- compound(E),!, \+ is_ftVar(E).


f2q(Depth,HeadIs,RetType,RetResult,Converting, (PreArgs,Converted)):-
     as_functor_args(Converting,F,A,Args),
        \+ \+ (member(E,Args), non_simple_arg(E)),
          cname_var('Self',Self),
          %Self = '$VAR'('RetType'),
          maplist(type_fit_childs('=',Depth,Self),_RetTypes1,ArgsCode,Args,NewArgs),
            list_to_conjunction(ArgsCode,PreArgs),
            nop(non_compat_io(color_g_mesg('magenta',
              ((write_src(type_fit_childs('=',Depth,F,_RetTypes2,PreArgs,Args,NewArgs)),nl))))),
        as_functor_args(Convert,F,A,NewArgs),
        \+ (member(E,NewArgs), non_simple_arg(E)),!,
        f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted).



%   f2q(_Depth,_HeadIs,_RetType, _RetVal, Convert, Convert) :- compound(Convert), (Convert= (_,_)),!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,
 must_det_ll((
  as_functor_args(Convert,F,A,Args),
  as_functor_args(Quot,quot,A,NewArgs),
  as_functor_args(QConvert,quot,A,Args))),
  get_inline_case_list([F|NewArgs],Quot,DefList),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,case(QConvert,DefList),Converted))).

is_non_evaluatable(S):- \+ compound(S),!.
is_non_evaluatable(S):- is_ftVar(S),!.
is_non_evaluatable([H|_]):- \+ symbol(H), \+ is_non_evaluatable(H).
f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- is_non_evaluatable(Convert),
   Converted = (Convert=RetResult),!.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'bind!'(Var,Value),is_ftVar(Value),!,
   Converted = u_assign('bind!'(Var,Value),RetResult).

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ 'bind!'(Var,Value), Value =~ 'new-space'(),!,
    Converted = u_assign('bind!'(Var,Value),RetResult).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ 'bind!'(Var,Value), !,
    f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
    Eval = u_assign('bind!'(Var,ValueResult),RetResult),
   combine_code(ValueCode,Eval,Converted).

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :-
     Convert =~ 'add-atom'(Where,What), !,
     =(What,WhatP),
     Converted = as_tf('add-atom'(Where,WhatP),RetResult).

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :-
     Convert =~ 'add-atom'(Where,What,RetResult), !,
     =(What,WhatP),
     Converted = as_tf('add-atom'(Where,WhatP),RetResult).

f2q(Depth,HeadIs,RetType,RetResult,Convert,Converted) :-
  Convert =~ ['println!',Value],!,
  Converted = (ValueCode,u_assign(['println!',ValueResult], RetResult)),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode).



f2q(Depth,HeadIs,RetType,RetResult,Convert,CodeForValueConverted) :-
    Convert =~ [Plus,N,Value], symbol(Plus), current_predicate(Plus/3), number(N),
    \+ number(Value), \+ is_nsVar(Value),!,
    f2p(Depth,HeadIs,RetType,ValueResult,Value,CodeForValue),!,
    Converted =.. [Plus,N,ValueResult,RetResult],
    combine_code(CodeForValue,Converted,CodeForValueConverted).
/*
% match(Space,f(1)=Y,Y)
f2q(Depth,HeadIs,RetType,Y,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert=~ match(Space,AsFunctionY,YY),
    nonvar(AsFunctionY),( AsFunctionY =~ (AsFunction=Y)), nonvar(AsFunction),
    !, Y==YY,
    f2p(Depth,HeadIs,RetType,Y,AsFunction,Converted),!.
*/
f2q(Depth,HeadIs,RetType,Atom,Convert,Converted) :-
   Convert=~ match(Space,Q,T),Q==T,Atom=Q,!,
  f2p(Depth,HeadIs,RetType,Atom,'get-atoms'(Space),Converted).

f2q(Depth,HeadIs,RetType,AtomsVar,Convert,Converted) :-
    Convert=~ 'get-atoms'(Space), Pattern = AtomsVar,
    compile_pattern(Depth,HeadIs,RetType,Space,Pattern,AtomsVar,Converted).

metta_atom_iter(Space,Match):-
  metta_atom_iter('=',10,Space,Space,Match).

make_with_space(Space,MatchCode,MatchCode):- Space=='&self',!.
make_with_space(Space,MatchCode,with_space(Space,MatchCode)):- Space\=='&self'.

% If Convert is a Value, and RetResult is a Variable bind them together and mark the compiler used them
f2q(_Depth,_HeadIs,_RetType, _RetResult,(A =~ B), (A =~ B)) :-!.


% If Convert is an "u_assign" function, we convert it to the equivalent "is" predicate.
f2q(Depth,HeadIs,RetType,RetResult,EvalConvert,Converted):-
 EvalConvert =~ eval(Convert),  !,
   must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted):-
    compound(Convert), Convert = u_assign(C, Var), compound_non_cons(C),into_list_args(C,CC),!,
    f2p(Depth,HeadIs,RetType,RetResult,u_assign(CC, Var), Converted).

f2q(_Depth,_HeadIs,_RetType,_RetResult,Convert, Converted):-
    compound(Convert),
    Convert = u_assign(C, _Var),
    is_list(C),Converted = Convert,!.


f2q(_Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
     symbol(Convert),  functional_predicate_arg(Convert,Nth,Nth2),
      Nth==1,Nth2==1,
      HeadIs,RetType\=@=Convert,
      Convert = F,!,
      must_det_ll((
        do_predicate_function_canonical(FP,F),
        compound_name_list(Converted,FP,[RetResult]))).

% If Convert is a number or an symbol, it is considered as already converted.
f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, RetResult = Convert) :- % HeadIs,RetType\=@=Convert,
    once(number(Convert); symbol(Convert); data_term(Convert)),  % Check if Convert is a number or an symbol
    !.  % Set RetResult to Convert as it is already in predicate form

% If Convert is an "is" function, we convert it to the equivalent "is" predicate.
f2q(Depth,HeadIs,RetType,RetResult,is(Convert),(Converted,is(RetResult,Result))):- !,
   must_det_ll((f2p(Depth,HeadIs,RetType,Result,Convert, Converted))).

f2q(_Depth,_HeadIs,_RetType,_RetResult, N=V, Code) :- !, into_equals(N,V,Code).

f2q(_Depth,_HeadIs,_RetType,RetVar, Data, '=~'(Data,RetVar)) :-
    as_functor_args(Data,F,A,_),
    is_data_functor(F,A),!.

into_equals(Eval,Result,Code):-
  into_u_assign(Eval,Result,Code).

into_u_assign(Eval,Result,true):- is_nsVar(Eval), is_nsVar(Result), Eval=Result,!.
into_u_assign(Eval,Result,Code):- Result=='True',!,r2p(Eval,_Result,Code).
into_u_assign(Eval,Result,Code):- var(Eval), \+ var(Result), !, into_u_assign(Result,Eval,Code).
into_u_assign(Eval,Result,Code):- ar2p(Eval,Result,Code),!.
into_u_assign(Eval,Result,Code):- Code = u_assign(Eval,Result).

% check if this is a flow control operation
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted):-
  compound(Convert), \+ compound_name_arity(Convert,_,0),
  f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted),!.



% If Convert is a list, we convert it to its termified form and then proceed with the functs_to_preds conversion.
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_list(Convert),
   once((sexpr_s2p(Convert,IS), \+ IS=@=Convert)), !,  % Check if Convert is a list and not in predicate form
   must_det_ll((f2p(Depth,HeadIs,RetType,RetResult, IS, Converted))).  % Proceed with the conversion of the predicate form of the list.

f2q(Depth,HeadIs,RetType,RetResult, ConvertL, Converted) :- is_list(ConvertL),
   maplist(f2p_assign(Depth,HeadIs,RetType),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Conjs),
   into_u_assign(RetResultL,RetResult,Code),
   combine_code(Conjs,Code,Converted).


f2q(Depth,HeadIs,RetType,RetResultL, ConvertL, Converted) :- is_list(ConvertL),
   ConvertL = [Convert],
   f2p(Depth,HeadIs,RetType,RetResult,Convert, Code),!,
   into_equals(RetResultL,[RetResult],Equals),
   combine_code(Code,Equals,Converted).

/* MAYBE USE ?
% If Convert is a compound term, we need to recursively convert its arguments.
f2q(Depth,HeadIs,RetType,RetResult, Convert, Converted) :- fail,
    compound(Convert), !,
    Convert =~ [Functor|Args],  % Deconstruct Convert to as_functor_args and arguments
    maplist(convert_argument, Args, ConvertedArgs),  % Recursively convert each argument
    Converted =~ [Functor|ConvertedArgs],  % Reconstruct Converted with the converted arguments
    (callable(Converted) -> f2p(Depth,HeadIs,RetType,RetResult, Converted, _); true).  % If Converted is callable, proceed with its conversion
% Helper predicate to convert an argument of a compound term
convert_argument(Arg, ConvertedArg) :-
    (callable(Arg) -> ftp(_, _, Arg, ConvertedArg); ConvertedArg = Arg).
*/

f2q(_Depth,_HeadIs,_RetType,ResultVar,'cdr-atom'(Atom), 'cdr-atom'(Atom,ResultVar)) :- !.
f2q(_Depth,_HeadIs,_RetType,ResultVar,'car-atom'(Atom), 'car-atom'(Atom,ResultVar)) :- !.
% convert Funtion
% f2q(Depth,HeadIs,RetType,ResultVar,Convert, Converted) :- ar2p(Convert, ResultVar, Converted).


/*
f2q(Depth,_HeadIs,_RetType,RetResult,AsPred,Converted):-
   compound(AsPred),
   as_functor_args(AsPred,F,A,Args),
   no_lists(Args),
   always_predicate_in_src(F,A),
   was_predicate(AsPred,RetResult,Converted).

f2q(Depth,_HeadIs,_RetType,RetResult,AsPred,Converted):-
   compound(AsPred),
   as_functor_args(AsPred,F,A,Args),
   no_lists(Args),
   always_function_in_src(F,A),
   was_predicate(AsPred,RetResult,Converted).
*/

f2q(_Depth,_HeadIs,_RetType,_RetResult,u_assign(Convert,Res), u_assign(Convert,Res)):-!.

% The catch-all If no specific case is matched, consider Convert as already converted.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Code):- into_u_assign(Convert,RetResult,Code).






data_term(Convert):-
 self_eval(Convert),!,
 (iz_conz(Convert) ;  \+ compound(Convert)).


de_eval(u_assign(X),X):- compound(X),!.

call1(G):- call(G).
call2(G):- call(G).
call3(G):- call(G).
call4(G):- call(G).
call5(G):- call(G).

trace_break:- trace,break.

:- table(u_assign/2).
u_assign(FList,R):- is_list(FList),!,u_assign(FList,R).
u_assign(FList,R):- var(FList),nonvar(R), !, u_assign(R,FList).
u_assign(FList,R):- FList=@=R,!,FList=R.
u_assign(FList,R):- number(FList), var(R),!,R=FList.
u_assign(FList,R):- self_eval(FList), var(R),!,R=FList.
u_assign(FList,R):- var(FList),!,/*trace,*/freeze(FList,u_assign(FList,R)).
u_assign(FList,R):- \+ compound(FList), var(R),!,R=FList.
u_assign([F|List],R):- F == ':-',!, trace_break,as_tf(clause(F,List),R).
u_assign(FList,RR):- (compound_non_cons(FList),u_assign_c(FList,RR))*->true;FList=~RR.
u_assign(FList,RR):-
  u_assign_list1(FList,RR)*->true;u_assign_list2(FList,RR).

u_assign_list1([F|List],R):- u_assign([F|List],R), nonvar(R), R\=@=[F|List].
u_assign_list2([F|List],R):- symbol(F),append(List,[R],ListR),
  catch(quietly(apply(F,ListR)),error(existence_error(procedure,F/_),_),
     catch(quietly(as_tf(apply(F,List),R)),error(existence_error(procedure,F/_),_),
        quietly(catch(u_assign([F|List],R),_, R=[F|List])))).

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
% funct_with_result_is_nth_of_pred(HeadIs,RetType,+(1, 2), Result, 3, +(1, 2, Result)).

into_callable(Pred,AsPred):- is_ftVar(Pred),!,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=AsPred,!.
into_callable(Pred,AsPred):- iz_conz(Pred), !,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=call_fr(_,_,_),!,AsPred=Pred.
into_callable(Pred,AsPred):- Pred =~ Cons,  !,AsPred=holds(Cons).


r2p(MeTTa,Result,IsPred):- r2p(_,MeTTa,Result,IsPred).
r2p(What,MeTTa,Result,IsPred):- ar2p(What,MeTTa,Result,IsPred),!.
r2p(What,MeTTa,Result,IsPred):- ar2q(What,MeTTa,Result,IsPred),!.


ar2p(MeTTa,Result,IsPred):- ar2p(_,MeTTa,Result,IsPred).


absorbed_default('Bool',_AsPred,'True').
absorbed_default(_,_AsPred,_).

is_absorbed_return_value(F,A,Result):-
  is_absorbed_return(F,A,Bool),
  absorbed_default(Bool,_AsPred,Result).

is_absorbed_return_value(F,A,Bool,Result):-
  is_absorbed_return(F,A,Bool),
  nonvar(Bool),
  absorbed_default(Bool,_AsPred,Result).

ar2p(boolean,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    is_absorbed_return(F,A,Bool),
    nonvar(Bool),absorbed_default(Bool,AsPred,Result),
    IsPred=..[F|Args].

ar2p(pred,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    is_absorbed_return(F,A,Bool),
    nonvar(Bool),absorbed_default(Bool,AsPred,Result),
    IsPred=..[F|Args].

ar2p(W,Data,Result,IsPred):-
    W\== boolean,
    as_functor_args(Data,F,A,_Args),
    is_data_functor(F,AA),!,
    (AA=A
       -> (IsPred = (Data =~ Result))
       ; was_predicate(Data,Result,IsPred)).
ar2p(pred,AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    always_predicate_in_src(F,A),!,
    once(functional_predicate_arg(F, A, Nth);Nth=A),
    \+ is_absorbed_return(F,_, _Bool),
    nth1(Nth,Args,Result),
    IsPred=..[F|Args].
ar2p(func,AsFunction,Result,IsPred):-
   as_functor_args(AsFunction,F,A0,FArgs),
   \+ is_absorbed_return(F,A0, _Bool),
   always_function_in_src(F,A0),!,A is A0 + 1,
   once(functional_predicate_arg(F, A, Nth);Nth=A),
   nth1(Nth,Args,Result,FArgs),
   IsPred=..[F|Args].

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
    is_absorbed_return(F,A,Bool),
    nonvar(Bool),absorbed_default(Bool,AsPred,Result),
    IsPred=..[F|Args].

was_predicate(AsPred,Result,IsPred):-
    as_functor_args(AsPred,F,A,Args),
    is_absorbed_return(F,A,Bool),
    nonvar(Bool),absorbed_default(Bool,AsPred,Result),!,
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


funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, Nth, AsPred):-
  var(AsPred),!,
  funct_with_result_is_nth_of_pred0(HeadIs,RetType,AsFunction, Result, Nth, Pred),
  into_callable(Pred,AsPred).

funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, Nth, AsPred):-
  var(AsFunction),!,
  funct_with_result_is_nth_of_pred0(HeadIs,RetType,Function, Result, Nth, AsPred),
  into_callable(Function,AsFunction).

funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, Nth, AsPred):-
  funct_with_result_is_nth_of_pred0(HeadIs,RetType,AsFunction, Result, Nth, AsPred).

% Handles the case where AsFunction is a variable.
% It creates a compound term 'AsPred' and places the 'Result' at the 'Nth' position
% of the predicate arguments, and the 'AsFunction' represents the functional form with
% arguments excluding the result.
funct_with_result_is_nth_of_pred0(_HeadIs,_RetType,AsFunction, Result, Nth, AsPred) :-
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
funct_with_result_is_nth_of_pred0(HeadIs,RetType,AsFunctionO, Result, Nth, (AsPred)) :-
   de_eval(AsFunctionO,AsFunction),!,funct_with_result_is_nth_of_pred0(HeadIs,RetType,AsFunction, Result, Nth, AsPred).

funct_with_result_is_nth_of_pred0(HeadIs,RetType,AsFunction, Result, _Nth, AsPred) :-
   nonvar(AsFunction),
   compound(AsFunction),
   \+ is_arity_0(AsFunction,_),
   as_functor_args(AsFunction,F,A),
   HeadIs,RetType\=@=AsFunction,
   \+ (compound(HeadIs,RetType), (is_arity_0(HeadIs,RetType,HF);as_functor_args(HeadIs,RetType,HF,_))-> HF==F),
   (into_u_assign(AsFunction, Result,AsPred)
       -> true
       ; (AA is A+1,
           (FAA=(F/AA)),
           \+ current_predicate(FAA), !,
           AsPred = call_fr(AsFunction,Result,FAA))).


funct_with_result_is_nth_of_pred0(_HeadIs,_RetType,AsFunction, Result, Nth, (AsPred)) :-
   (symbol(AsFunction)->AsFunction =~ [F | FuncArgs]; compound_name_list(AsFunction,F,FuncArgs)),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
    nth1(Nth, PredArgs, Result, FuncArgs), % It places 'Result' at the 'Nth' position
    AA is Nth+1, \+ current_predicate(F/AA),
    do_predicate_function_canonical(FP,F),
    AsPred =~ [FP | PredArgs]. % It forms the predicate 'AsPred' by joining the as_functor_args with the modified arguments list.



funct_with_result_is_nth_of_pred0(_HeadIs,_RetType,AsFunction, Result, Nth, (AsPred)) :-
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
funct_with_result_is_nth_of_pred0(_HeadIs,_RetType,AsFunction, _, _, _) :-
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
    /*'&self':*/
    consult(TempFileName),
    % Delete the temporary file after consulting
    %delete_file(TempFileName),
    asserta(metta_compiled_predicate(F,A)),
    true.

:- dynamic(metta_compiled_predicate/2).

metta_compiled_predicate(_,F,A):-
  metta_compiled_predicate(F,A).

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
        funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, _Nth, AsPred),  % Convert AsFunction to a predicate AsPred
        subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
        f2p(Depth,HeadIs,RetType,RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms
    /*
    % If AsFunction is a recognized function, convert it to a predicate.
    f2q(Depth,HeadIs,RetType,RetResult,AsFunction,AsPred):- % HeadIs,RetType\=@=AsFunction,
       is_function(AsFunction, Nth),  % Check if AsFunction is a recognized function and get the position Nth where the result is stored/retrieved
       funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, RetResult, Nth, AsPred),
       \+ ( compound(AsFunction), arg(_,AsFunction, Arg), is_function(Arg,_)),!.
    */

    % If any sub-term of Convert is an u_assign/2, convert that sub-term and then proceed with the conversion.
    f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,
        deep_lhs_sub_sterm0(ConvertFunction, Convert), % Get the deepest sub-term AsFunction of Convert
        callable(ConvertFunction),  % Check if AsFunction is callable
        ConvertFunction = u_assign(AsFunction,Result),
        ignore(is_function(AsFunction, Nth)),
        funct_with_result_is_nth_of_pred(HeadIs,RetType,AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
        subst(Convert, ConvertFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
        f2p(Depth,HeadIs,RetType,RetResult, (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

