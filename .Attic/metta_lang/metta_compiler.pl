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
% Prolog to MeTTa transpilation (which uses the Host SWI-Prolog compiler)
% This Prolog code block is mainly aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================

% Setting the file encoding to ISO-Latin-1
%:- encoding(iso_latin_1).
% Flushing the current output
:- flush_output.
% Setting the Rust backtrace to Full
:- setenv('RUST_BACKTRACE',full).
% Loading various library files
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
%:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_compiler_stdlib).

% ==============================
% MeTTa to Prolog transpilation (which uses the Host SWI-Prolog compiler)
% Aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================
:- dynamic(metta_compiled_predicate/3).
:- multifile(metta_compiled_predicate/3).


% =======================================
% TODO move non flybase specific code between here and the compiler
%:- ensure_loaded(flybase_main).
% =======================================
%:- set_option_value(encoding,utf8).

:- initialization(mutex_create(transpiler_mutex_lock)).
:- at_halt(mutex_destroy(transpiler_mutex_lock)).

%transpile_prefix('').
transpile_prefix('mc__').

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).

:- op(700,xfx,'=~').

compound_non_cons(B):-  compound(B),  \+ B = [_|_].
iz_conz(B):- compound(B), B=[_|_].

'=~'(A,B):- compound_non_cons(B),!,into_list_args(B,BB),!,'=~'(A,BB).
'=~'(B,A):- compound_non_cons(B),!,into_list_args(B,BB),!,'=~'(A,BB).
'=~'(A,B):- iz_conz(A),iz_conz(B),!,A=B.
'=~'(A,B):- var(A),iz_conz(B),!,A=B.
'=~'(A,B):- iz_conz(A),var(B),!,A=B.
'=~'(A,B):- compound_non_cons(A),var(B),!,A=..B.
'=~'(A,B):- compound_non_cons(B),!,A=B.
'=~'(A,B):- '=..'(A,B).

%into_list_args(A,AA):- is_ftVar(A),AA=A.
%into_list_args(C,[C]):- \+ compound(C),!.
into_list_args(C,C):- \+ compound(C),!.
into_list_args(A,AA):- is_ftVar(A),AA=A.
into_list_args([H|T],[H|T]):- \+ is_list(T),!.
into_list_args([H,List,A],HT):- H == x_assign,!,append(List,[A],HT),!.
into_list_args([H|T],[H|T]):- is_list(T),!.
into_list_args(x_assign(List, A),[H|T]):- append(List,[A],[H|T]),!.
into_list_args(holds(A),AA):- !, into_list_args(A,AA),!.
into_list_args(C,FArgs):- compound_name_arguments(C,F,Args),!,into_list_args([F|Args],FArgs).

compound_name_list(AsPred,FP,PredArgs):- var(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- iz_conz(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- into_list_args(AsPred,[FP|PredArgs]),!.
compound_name_list(AsPred,FP,PredArgs):- compound_non_cons(AsPred),!,compound_name_arguments(AsPred,FP,PredArgs).

strip_m(M:BB,BB):- nonvar(BB),nonvar(M),!.
strip_m(BB,BB).

% ?- compile_for_exec(RetResult, is(pi+pi), Converted).

compile_for_exec(Res,I,O):-
   %format("~w ~w\n",[Res,I]),
   %ignore(Res='$VAR'('RetResult')),
   compile_for_exec0(Res,I,O),!.

compile_for_exec0(Res,I,eval_args(I,Res)):- is_ftVar(I),!.
compile_for_exec0(Res,(:- I),O):- !, compile_for_exec0(Res,I,O).

compile_for_exec0(Res,I,BB):-
   compile_for_exec1(I, H:-BB),
   arg(1,H,Res).

compile_for_exec0(Res,I,BB):- fail,
   %ignore(Res='$VAR'('RetResult')),
   compile_flow_control(exec(),Res,I,O),
   head_preconds_into_body(exec(Res),O,_,BB).

%compile_for_exec0(Res,I,O):- f2p(exec(),Res,I,O).

compile_for_exec1(AsBodyFn, Converted) :-
   Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   f2p([exec0],HResult,AsBodyFn,NextBody),
   optimize_head_and_body(x_assign([exec0],HResult),NextBody,HeadC,NextBodyB),
   replace_x_assign([],NextBodyB,NextBodyC).

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
%                   eval_args(_C, RetResult)).
%
functs_to_preds(I,OO):-
   must_det_ll(functs_to_preds0(I,OO)),!.

functs_to_preds0([Eq,H,B],OO):- Eq == '=', compile_for_assert(H, B, OO),!.
functs_to_preds0(EqHB,OO):- compile_head_for_assert(EqHB,OO),!.

functs_to_preds0(I,OO):-
   sexpr_s2p(I, M),
   f2p(_,_,M,O),
   expand_to_hb(O,H,B),
   head_preconds_into_body(H,B,HH,BB),!,
   OO = ':-'(HH,BB).

optimize_head_and_body(Head,Body,HeadNewest,BodyNewest):-
   label_body_singles(Head,Body),
   color_g_mesg('#404064',print_pl_source(( Head :- Body))),
   (merge_and_optimize_head_and_body(Head,Body,HeadNew,BodyNew),
      % iterate to a fixed point
      (((Head,Body)=@=(HeadNew,BodyNew))
      ->  (HeadNew=HeadNewest,BodyNew=BodyNewest)
      ;  optimize_head_and_body(HeadNew,BodyNew,HeadNewest,BodyNewest))).

merge_and_optimize_head_and_body(Head,Converted,HeadO,Body):- nonvar(Head),
   Head = (PreHead,True),!,
   merge_and_optimize_head_and_body(PreHead,(True,Converted),HeadO,Body).
merge_and_optimize_head_and_body(AHead,Body,Head,BodyNew):-
   assertable_head(AHead,Head),
   must_optimize_body(Head,Body,BodyNew).

assertable_head(x_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), atom(F), Head=..[F|NewArgs],!.
assertable_head(Head,Head).

label_body_singles(Head,Body):-
   term_singletons(Body+Head,BodyS),
   maplist(label_body_singles_2(Head),BodyS).
label_body_singles_2(Head,Var):- sub_var(Var,Head),!.
label_body_singles_2(_,Var):- ignore(Var='$VAR'('_')).

must_optimize_body(A,B,CC):- once(optimize_body(A,B,C)), C \=@= B,!, must_optimize_body(A,C,CC).
must_optimize_body(_,B,C):- B =C.

optimize_body(_HB,Body,BodyNew):- is_ftVar(Body),!,Body=BodyNew.
%optimize_body( HB,eval_args(VT,R),eval_args(VT,R)):-!, must_optimize_body(HB,VT,VTT).
optimize_body( HB,with_space(V,T),with_space(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,limit(V,T),limit(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,findall(V,T,R),findall(V,TT,R)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,loonit_assert_source_tf(V,T,R3,R4), loonit_assert_source_tf(V,TT,R3,R4)):-!,
  must_optimize_body(HB,T,TT).

optimize_body( HB,(B1*->B2;B3),(BN1*->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2), optimize_body(HB,B3,BN3).
optimize_body( HB,(B1->B2;B3),(BN1->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), must_optimize_body(HB,B2,BN2), must_optimize_body(HB,B3,BN3).
optimize_body( HB,(B1:-B2),(BN1:-BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1*->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1;B2),(BN1;BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
% TODO FIXME optimize_body( HB,(B1,B2),(BN1)):- optimize_conjuncts(HB,(B1,B2),BN1).
%optimize_body(_HB,==(Var, C), Var=C):- self_eval(C),!.
optimize_body( HB,x_assign(A,B),R):- optimize_x_assign_1(HB,A,B,R),!.
%optimize_body(_HB,x_assign(A,B),x_assign(AA,B)):- p2s(A,AA),!.
optimize_body(_HB,Body,BodyNew):- Body=BodyNew.

optimize_x_assign_1(_,Var,_,_):- is_ftVar(Var),!,fail.
optimize_x_assign_1(HB,Compound,R,Code):- \+ compound(Compound),!, optimize_x_assign(HB,Compound,R,Code).
optimize_x_assign_1(HB,[H|T],R,Code):- !, optimize_x_assign(HB,[H|T],R,Code).
optimize_x_assign_1(HB,Compound,R,Code):- p2s(Compound,MeTTa),   optimize_x_assign(HB,MeTTa,R,Code).
%optimize_x_assign_1(_,[Pred| ArgsL], R, x_assign([Pred| ArgsL],R)).

optimize_x_assign(_,[Var|_],_,_):- is_ftVar(Var),!,fail.
optimize_x_assign(_,[Empty], _, (!,fail)):-  Empty == empty,!.
optimize_x_assign(_,[+, A, B], C, plus(A , B, C)):- number_wang(A,B,C), !.
optimize_x_assign(_,[-, A, B], C, plus(B , C, A)):- number_wang(A,B,C), !.
%optimize_x_assign(_,[+, A, B], C, +(A , B, C)):- !.
%optimize_x_assign(_,[-, A, B], C, +(B , C, A)):- !.
optimize_x_assign(_,[*, A, B], C, *(A , B, C)):- number_wang(A,B,C), !.
optimize_x_assign(_,['/', A, B], C, *(B , C, A)):- number_wang(A,B,C), !.
%optimize_x_assign(_,[*, A, B], C, *(A , B, C)):- !.
%optimize_x_assign(_,['/', A, B], C, *(B , C, A)):- !.
%optimize_x_assign(_,[fib, B], C, fib(B, C)):- !.
%optimize_x_assign(_,[fib1, A,B,C,D], R, fib1(A, B, C, D, R)):- !.
optimize_x_assign(_,['pragma!',N,V],Empty,set_option_value_interp(N,V)):-
   nonvar(N),ignore((fail,Empty='Empty')), !.
optimize_x_assign((H:-_),Filter,A,filter_head_arg(A,Filter)):- fail, compound(H), arg(_,H,HV),
  HV==A, is_list(Filter),!.
%optimize_x_assign(_,[+, A, B], C, '#='(C , A + B)):- number_wang(A,B,C), !.
%optimize_x_assign(_,[-, A, B], C, '#='(C , A - B)):- number_wang(A,B,C), !.
optimize_x_assign(_,[match,KB,Query,Template], R, Code):-  match(KB,Query,Template,R) = Code.

optimize_x_assign(HB,MeTTaEvalP, R, Code):- \+ is_ftVar(MeTTaEvalP),
  compound_non_cons(MeTTaEvalP), p2s(MeTTaEvalP,MeTTa),
  MeTTa\=@=MeTTaEvalP,!, optimize_body(HB, x_assign(MeTTa, R), Code).

% optimize_x_assign(_,_,_,_):- !,fail.
optimize_x_assign((H:-_),[Pred| ArgsL], R, Code):- var(R), atom(Pred), ok_to_append(Pred),
  append([Pred| ArgsL],[R], PrednArgs),Code=..PrednArgs,
  (H=..[Pred|_] -> nop(set_option_value('tabling',true)) ; current_predicate(_,Code)),!.

number_wang(A,B,C):-
  (numeric(C);numeric(A);numeric(B)),!,
  maplist(numeric_or_var,[A,B,C]),
  maplist(decl_numeric,[A,B,C]),!.

data_term(Convert):- self_eval(Convert),!.

into_equals(RetResultL,RetResult,Equals):- into_x_assign(RetResultL,RetResult,Equals).

into_x_assign(RetResultL,RetResult,true):- is_ftVar(RetResultL), is_ftVar(RetResult), RetResult=RetResultL,!.
into_x_assign(RetResultL,RetResult,Code):- var(RetResultL), Code = x_assign(RetResult,RetResultL).
into_x_assign(RetResultL,RetResult,Code):- Code = x_assign(RetResultL,RetResult).

numeric(N):- number(N),!.
numeric(N):- get_attr(N,'Number','Number').
numeric(N):- get_decl_type(N,DT),(DT=='Int',DT=='Number').

decl_numeric(N):- numeric(N),!.
decl_numeric(N):- ignore((var(N),put_attr(N,'Number','Number'))).

numeric_or_var(N):- var(N),!.
numeric_or_var(N):- numeric(N),!.
numeric_or_var(N):- \+ compound(N),!,fail.
numeric_or_var('$VAR'(_)).

get_decl_type(N,DT):- attvar(N),get_atts(N,AV),sub_term(DT,AV),atom(DT).

replace_x_assign(_,A,A) :- var(A),!.
replace_x_assign(DontStub,x_assign(A,[F|Args0]),R) :- var(A),atom(F),!,
   maplist(replace_x_assign(DontStub),Args0,Args1),
   transpile_prefix(Prefix),
   atom_concat(Prefix,F,Fp),
   length(Args0,LArgs),
   LArgs1 is LArgs+1,
   append(Args1,[A],Args2),
   R=..[Fp|Args2],
   ((current_predicate(Fp/LArgs1);member(Fp/LArgs1,DontStub)) ->
      true
   ; check_supporting_predicates('&self',F/LArgs1)).
replace_x_assign(_,x_assign(A,B),R) :- var(A),!,R=(A=B).
replace_x_assign(DontStub,x_assign(A,B),R) :- var(B),\+ var(A),!,
   replace_x_assign(DontStub,x_assign(B,A),R).
replace_x_assign(DontStub,A,B) :-
   compound(A),
   A=..A0,!,
   maplist(replace_x_assign(DontStub),A0,B0),
   B=..B0.
replace_x_assign(_,A,A).

check_supporting_predicates(Space,F/A) :- % already exists
   transpile_prefix(Prefix),
   atom_concat(Prefix,F,Fp),
   with_mutex(transpiler_mutex_lock,
      (current_predicate(Fp/A) -> true ;
         findall(Atom0, (between(1, A, I0) ,Atom0='$VAR'(I0)), AtomList0),
         H=..[Fp|AtomList0],
         Am1 is A-1,
         findall(Atom1, (between(1, Am1, I1), Atom1='$VAR'(I1)), AtomList1),
         B=..[u_assign,[F|AtomList1],'$VAR'(A)],
         create_and_consult_temp_file(Space,Fp/A,[H:-(format("######### warning: using stub for:~w\n",[F]),B)]))).

% Predicate to create a temporary file and write the tabled predicate
create_and_consult_temp_file(Space,F/A, PredClauses) :-
  must_det_ll((
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

    listing(F/A),
    % Delete the temporary file after consulting
    %delete_file(TempFileName),
    asserta(metta_compiled_predicate(Space,F,A)),
    current_predicate(F/A),
    listing(metta_compiled_predicate/3),
    true)).

%metta_compiled_predicate(_,F,A):- metta_compiled_predicate(F,A).

% Helper predicate to write a clause to the file
write_clause(Stream, Clause) :-
    subst_vars(Clause,Can),
    write_canonical(Stream, Can),
    write(Stream, '.'),
    nl(Stream).

u_assign(FList,R):- is_list(FList),!,eval_args(FList,R).
u_assign(FList,R):- var(FList),nonvar(R), !, u_assign(R,FList).
u_assign(FList,R):- FList=@=R,!,FList=R.
u_assign(FList,R):- number(FList), var(R),!,R=FList.
u_assign(FList,R):- self_eval(FList), var(R),!,R=FList.
u_assign(FList,R):- var(FList),!,freeze(FList,u_assign(FList,R)).
u_assign(FList,R):- \+ compound(FList), var(R),!,R=FList.
u_assign([F|List],R):- F == ':-',!, trace_break,as_tf(clause(F,List),R).
u_assign(FList,RR):- (compound_non_cons(FList),u_assign_c(FList,RR))*->true;FList=~RR.
u_assign(FList,RR):-
  u_assign_list1(FList,RR)*->true;u_assign_list2(FList,RR).

u_assign_list1([F|List],R):- eval_args([F|List],R), nonvar(R), R\=@=[F|List].
u_assign_list2([F|List],R):- atom(F),append(List,[R],ListR),
  catch(quietly(apply(F,ListR)),error(existence_error(procedure,F/_),_),
     catch(quietly(as_tf(apply(F,List),R)),error(existence_error(procedure,F/_),_),
        quietly(catch(eval_args([F|List],R),_, R=[F|List])))).

%u_assign([V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(_Eq,V),!,maplist(eval_args,VI,VO).

u_assign_c((F:-List),R):- !, R = (F:-List).

/*
u_assign_c(Cmp,RR):-
  functor(Cmp,F,_),
  current_predicate(F,_),
  debug(todo,'u_assign_c INTERP: ~q',[Cmp]),!,
  call(Cmp,RR).*/
u_assign_c(FList,RR):-
  functor(FList,F,_), % (F == 'car-atom' -> trace ; true),
  (catch(quietlY(call(FList,R)),error(existence_error(procedure,F/_),_),
     catch(quietlY(as_tf(FList,R)),error(existence_error(procedure,F/_),_),
      ((p2m(FList,[F0|List0]),catch(eval_args([F0|List0],R),_, R=~[F0|List0])))))),!,
         R=RR.
u_assign_c(FList,RR):- as_tf(FList,RR),!.
u_assign_c(FList,R):- compound(FList), !, FList=~R.

quietlY(G):- call(G).

:- discontiguous f2p/4.

f2p(_HeadIs,Convert, Convert, true) :-
     (is_ftVar(Convert);number(Convert)),!.% Check if Convert is a variable

% If Convert is a number or an atom, it is considered as already converted.
f2p(_HeadIs,RetResult, Convert, RetResult = Convert) :- % HeadIs\=@=Convert,
    once(number(Convert); atom(Convert); data_term(Convert)),  % Check if Convert is a number or an atom
    % For OVER-REACHING categorization of dataobjs %
    % wdmsg(data_term(Convert)),
    %trace_break,
    !.  % Set RetResult to Convert as it is already in predicate form

f2p(HeadIs,RetResult,Convert, Converted):-
    compound(Convert), \+ compound_name_arity(Convert,_,0),
    compile_flow_control(HeadIs,RetResult,Convert, Converted),!.

f2p(HeadIs,RetResult, Convert, Converted) :- HeadIs\=@=Convert,
   Convert=[Fn|Args],
   atom(Fn),!,
   length(Args,Largs),
   get_operator_typedef(_,Fn,Largs,Types,_RetType),
   maplist(is_arg_eval,Types,EvalArgs),
   %,
   maplist(do_arg_eval(HeadIs),Args,EvalArgs,NewArgs,NewCodes),
   append(NewCodes,CombinedNewCode),
   into_x_assign([Fn|NewArgs],RetResult,Code),
   append(CombinedNewCode,[Code],CombinedNewCode1),
   combine_code_list(CombinedNewCode1,Converted).

combine_code_list(A,R) :- !,
   combine_code_list_aux(A,R0),
   (R0=[] -> R=true
   ; R0=[R1] -> R=R1
   ; R0=[H|T],
      combine_code_list(T,T0),
      R=..[',',H,T0]).

combine_code_list_aux([],[]).
combine_code_list_aux([true|T],R) :- !,combine_code_list_aux(T,R).
combine_code_list_aux([H|T],R) :- H=..[','|H0],!,append(H0,T,T0),combine_code_list_aux(T0,R).
combine_code_list_aux([H|T],[H|R]) :- combine_code_list_aux(T,R).

% temporary placeholder
is_arg_eval('Number',yes) :- !.
is_arg_eval('Any',yes) :- !.
is_arg_eval(_,no).

do_arg_eval(_,Arg,no,Arg,[]).
do_arg_eval(HeadIs,Arg,yes,NewArg,[Code]) :- f2p(HeadIs,NewArg,Arg,Code).

% The catch-all If no specific case is matched, consider Convert as already converted.
f2p(_HeadIs,_RetResult,x_assign(Convert,Res), x_assign(Convert,Res)):- !.
f2p(_HeadIs,RetResult,Convert, Code):- into_x_assign(Convert,RetResult,Code).

f2p(HeadIs,_RetResult,Convert,_Code):-
   format("Error in f2p ~w ~w\n",[HeadIs,Convert]),
   throw(0).

compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   format("compile_for_assert: ~w ~w\n",[HeadIs, AsBodyFn]),
   AsFunction = HeadIs,
   must_det_ll((
   Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   /*funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),*/
   f2p(HeadIs,HResult,AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   %verbose_unify(Converted),
   f2p(HeadIs,Result,AsBodyFn,NextBody),
   %RetResult = Converted,
   %RetResult = _,
   optimize_head_and_body(Head,NextBody,HeadC,NextBodyB),
   replace_x_assign([],NextBodyB,NextBodyC),
   %fbug([convert(Convert),head_preconds_into_body(HeadC:-NextBodyC)]),
   %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

unnumbervars_clause(Cl,ClU):-
  copy_term_nat(Cl,AC),unnumbervars(AC,UA),copy_term_nat(UA,ClU).
% ===============================
%  Compile in memory buffer
% ===============================
is_clause_asserted(AC):- unnumbervars_clause(AC,UAC),
  expand_to_hb(UAC,H,B),
  H=..[Fh|Args],
  transpile_prefix(Prefix),
  atom_concat(Prefix,Fh,FPrefixed),
  H2=..[FPrefixed|Args],
  clause(H2,B,Ref),clause(HH,BB,Ref),
  strip_m(HH,HHH),HHH=@=H2,
  strip_m(BB,BBB),BBB=@=B,!.

%get_clause_pred(UAC,F,A):- expand_to_hb(UAC,H,_),strip_m(H,HH),functor(HH,F,A).


% :- dynamic(needs_tabled/2).

add_assertion(Space,List):- is_list(List),!,
   maplist(add_assertion(Space),List).
add_assertion(Space,AC):- unnumbervars_clause(AC,UAC), add_assertion1(Space,UAC).
add_assertion1(_,AC):- /*'&self':*/is_clause_asserted(AC),!.
%add_assertion1(_,AC):- get_clause_pred(AC,F,A), \+ needs_tabled(F,A), !, pfcAdd(/*'&self':*/AC),!.

add_assertion1(Space,ACC) :-
   must_det_ll((
     % add the prefix
      (ACC = (ACCH :- ACCB),ACCH=..[ACCf|ACCa] ->
         transpile_prefix(Prefix),
         atom_concat(Prefix,ACCf,ACCfp),
         ACCH2=..[ACCfp|ACCa],
         ACC2=(ACCH2 :- ACCB)
      ;
         ACC2=ACC),
     copy_term(ACC2,AC,_),
     expand_to_hb(AC,H,_),
     as_functor_args(H,F,A), as_functor_args(HH,F,A),
    with_mutex(transpiler_mutex_lock,(
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
      create_and_consult_temp_file(Space,F/A, Set)
    ))
)).

as_functor_args(AsPred,F,A):- as_functor_args(AsPred,F,A,_ArgsL),!.

as_functor_args(AsPred,F,A,ArgsL):-var(AsPred),!,
  (is_list(ArgsL);(integer(A),A>=0)),!,
   length(ArgsL,A),
   (symbol(F)->
      AsPred =..[F|ArgsL]
   ;
      (AsPred = [F|ArgsL])).

%as_functor_args(AsPred,_,_,_Args):- is_ftVar(AsPred),!,fail.
as_functor_args(AsPred,F,A,ArgsL):- \+ iz_conz(AsPred),
  AsPred=..List,!, as_functor_args(List,F,A,ArgsL),!.
%as_functor_args([Eq,R,Stuff],F,A,ArgsL):- (Eq == '='),
%   into_list_args(Stuff,List),append(List,[R],AsPred),!,
%   as_functor_args(AsPred,F,A,ArgsL).
as_functor_args([F|ArgsL],F,A,ArgsL):-  length(ArgsL,A),!.

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

end_of_file.

compile_head_variablization(Head, NewHead, HeadCode) :-
   must_det_ll((
      as_functor_args(Head,Functor,A,Args),
      % Find non-singleton variables in Args
      fix_non_singletons(Args, NewArgs, Conditions),
      list_to_conjunction(Conditions,HeadCode),
      as_functor_args(NewHead,Functor,A,NewArgs))).

fix_non_singletons(Args, NewArgs, [Code|Conditions]) :-
   sub_term_loc(Var, Args, Loc1), is_ftVar(Var),
   sub_term_loc_replaced(==(Var), _Var2, Args, Loc2, ReplVar2, NewArgsM),
   Loc1 \=@= Loc2,
   Code = same(ReplVar2,Var),
fix_non_singletons(NewArgsM, NewArgs, Conditions).
fix_non_singletons(Args, Args, []):-!.


sub_term_loc(A,A,self).
sub_term_loc(E,Args,e(N,nth1)+Loc):- is_list(Args),!, nth1(N,Args,ST),sub_term_loc(E,ST,Loc).
sub_term_loc(E,Args,e(N,arg)+Loc):- compound(Args),arg(N,Args,ST),sub_term_loc(E,ST,Loc).

sub_term_loc_replaced(P1,E,Args,LOC,Var,NewArgs):- is_list(Args), !, sub_term_loc_l(nth1,P1,E,Args,LOC,Var,NewArgs).
sub_term_loc_replaced(P1,E,FArgs,LOC,Var,NewFArgs):- compound(FArgs), \+ is_ftVar(FArgs),!,
   compound_name_arguments(FArgs, Name, Args),
   sub_term_loc_l(arg,P1,E,Args,LOC,Var,NewArgs),
   compound_name_arguments(NewCompound, Name, NewArgs),NewFArgs=NewCompound.
   sub_term_loc_replaced(P1,A,A,self,Var,Var):- call(P1,A).


sub_term_loc_l(Nth,P1,E,Args,e(N,Nth)+Loc,Var,NewArgs):-
   reverse(Args,RevArgs),
   append(Left,[ST|Right],RevArgs),
   sub_term_loc_replaced(P1,E,ST,Loc,Var,ReplaceST),
   append(Left,[ReplaceST|Right],RevNewArgs),
   reverse(RevNewArgs,NewArgs),
   length([_|Right], N).

% Convert a list of conditions into a conjunction
list_to_conjunction([], true).
list_to_conjunction([Cond], Cond).
list_to_conjunction([H|T], (H, RestConj)) :-
   list_to_conjunction(T, RestConj).

/*
as_functor_args(AsPred,F,A,ArgsL):-    nonvar(AsPred),!,into_list_args(AsPred,[F|ArgsL]),    length(ArgsL,A).
as_functor_args(AsPred,F,A,ArgsL):-
   nonvar(F),length(ArgsL,A),AsPred = [F|ArgsL].
*/

compile_for_assert(HeadIs, AsBodyFn, Converted) :- (AsBodyFn =@= HeadIs ; AsBodyFn == []), !,/*trace,*/  compile_head_for_assert(HeadIs,Converted).

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(Head, AsBodyFn, Converted) :-
   once(compile_head_variablization(Head, HeadC, CodeForHeadArgs)),
   \+(atomic(CodeForHeadArgs)), !,
   compile_for_assert(HeadC,
      (CodeForHeadArgs,AsBodyFn), Converted).

compile_for_assert(HeadIs, AsBodyFn, Converted) :- is_ftVar(AsBodyFn), /*trace,*/
   AsFunction = HeadIs,!,
   must_det_ll((
   Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   f2p(HeadIs,HResult,AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   NextBody = x_assign(AsBodyFn,Result),
   optimize_head_and_body(Head,NextBody,HeadC,BodyC),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

% PLACEHOLDER


% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   compile_head_args(Head,HeadC,CodeForHeadArgs),
   f2p(HeadIs,Result,AsBodyFn,NextBody),
   combine_code(CodeForHeadArgs,NextBody,BodyC),!,
   optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),!.



% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% ===============================


% Conversion is possible between a function and a predicate of arity when the result is at the nth arg
:- dynamic decl_functional_predicate_arg/3.

% Converion is possible between a  function and predicate is tricky
functional_predicate_arg_tricky(is, 2, 1). % E.g. eval_args(is(+(1,2)),Result) converts to is(Result,+(1,2)).
% Defining standard mappings for some common functions/predicates
decl_functional_predicate_arg(append, 3, 3).
decl_functional_predicate_arg(+, 3, 3).
decl_functional_predicate_arg(pi, 1, 1).
decl_functional_predicate_arg('Empty', 1, 1).
decl_functional_predicate_arg(call,4,4).
decl_functional_predicate_arg(eval_args, 2, 2).
decl_functional_predicate_arg(edge, 2, 2).
decl_functional_predicate_arg('==', 2, 2).
decl_functional_predicate_arg('is-same', 2, 2).
decl_functional_predicate_arg(assertTrue, 2, 2).
decl_functional_predicate_arg(case, 3, 3).
decl_functional_predicate_arg(assertFalse, 2, 2).
decl_functional_predicate_arg('car-atom', 2, 2).
decl_functional_predicate_arg(match,4,4).
decl_functional_predicate_arg('TupleConcat',3,3).
decl_functional_predicate_arg('new-space',1,1).

decl_functional_predicate_arg(superpose, 2, 2).

do_predicate_function_canonical(F,FF):- predicate_function_canonical(F,FF),!.
do_predicate_function_canonical(F,F).
predicate_function_canonical(is_Empty,'Empty').

pi(PI):- PI is pi.

% Retrieve Head of the List
'car-atom'(List, Head):- eval_H(['car-atom', List], Head).


% Mapping any current predicate F/A to a function, if it is not tricky
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
is_function(AsFunction, Nth) :-
    callable(AsFunction),
    functor(AsFunction, Functor, A),
    \+ not_function(Functor, A),
    AA is A + 1,
    functional_predicate_arg_maybe(Functor, AA, Nth).

functional_predicate_arg_maybe(F, AA, Nth):- functional_predicate_arg(F, AA, Nth),!.
functional_predicate_arg_maybe(F, AA, _):- A is AA - 1,functional_predicate_arg(F,A,_),!,fail.
functional_predicate_arg_maybe(F, Nth, Nth):- asserta(decl_functional_predicate_arg(F, Nth, Nth)),!.


% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_head_for_assert(HeadIs, (Head:-Body)):-
   compile_head_for_assert(HeadIs, NewHeadIs,Converted),
   head_preconds_into_body(NewHeadIs,Converted,Head,Body).

head_as_is(Head):-
   as_functor_args(Head,Functor,A,_),!,
   head_as_is(Functor,A).
head_as_is(if,3).

compile_head_for_assert(Head, Head, true):-
   head_as_is(Head),!.

compile_head_for_assert(Head, NewestHead, HeadCode):-
   compile_head_variablization(Head, NewHead, VHeadCode),
   compile_head_args(NewHead, NewestHead, AHeadCode),
   combine_code(VHeadCode,AHeadCode,HeadCode).

% Construct the new head and the match body
compile_head_args(Head, NewHead, HeadCode) :-
   must_det_ll((
      as_functor_args(Head,Functor,A,Args),
      maplist(f2p_assign(Head),NewArgs,Args,CodeL),
      as_functor_args(NewHead,Functor,A,NewArgs),
      list_to_conjuncts(CodeL,HeadCode))),!.







:- op(700,xfx,'=~').



compile_for_assert(HeadIs, AsBodyFn, Converted) :- (AsBodyFn =@= HeadIs ; AsBodyFn == []), !,/*trace,*/  compile_head_for_assert(HeadIs,Converted).

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(Head, AsBodyFn, Converted) :-
   once(compile_head_variablization(Head, HeadC, CodeForHeadArgs)),
   \+(atomic(CodeForHeadArgs)), !,
   compile_for_assert(HeadC,
      (CodeForHeadArgs,AsBodyFn), Converted).

compile_for_assert(HeadIs, AsBodyFn, Converted) :- fail,is_ftVar(AsBodyFn), /*trace,*/
   AsFunction = HeadIs,!,
   must_det_ll((
   Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   f2p(HeadIs,HResult,AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   NextBody = x_assign(AsBodyFn,Result),
   optimize_head_and_body(Head,NextBody,HeadC,BodyC),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   format("~w ~w ~w\n",[HeadIs, AsBodyFn, Converted]),
   trace,
   AsFunction = HeadIs,
   must_det_ll((
   Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   /*funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),*/
   f2p(HeadIs,HResult,AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   %verbose_unify(Converted),
   f2p(HeadIs,Result,AsBodyFn,NextBody),
   %RetResult = Converted,
   %RetResult = _,
   optimize_head_and_body(Head,NextBody,HeadC,NextBodyC),
   %fbug([convert(Convert),head_preconds_into_body(HeadC:-NextBodyC)]),
   %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   compile_head_args(Head,HeadC,CodeForHeadArgs),
   f2p(HeadIs,Result,AsBodyFn,NextBody),
   combine_code(CodeForHeadArgs,NextBody,BodyC),!,
   optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),!.


/*
*/
metta_predicate(eval_args(evaluable,eachvar)).
metta_predicate(eval_true(matchable)).
metta_predicate(with_space(space,matchable)).
metta_predicate(limit(number,matchable)).
metta_predicate(findall(template,matchable,listvar)).
metta_predicate(match(space,matchable,template,eachvar)).


ok_to_append('$VAR'):- !, fail.
ok_to_append(_).

p2s(P,S):- into_list_args(P,S).

non_compound(S):- \+ compound(S).

did_optimize_conj(Head,B1,B2,B12):- optimize_conj(Head,B1,B2,B12), B12\=@=(B1,B2),!.


optimize_conjuncts(Head,(B1,B2,B3),BN):- B3\==(_,_),
  did_optimize_conj(Head,B2,B3,B23),
  optimize_conjuncts(Head,B1,B23,BN), !.
optimize_conjuncts(Head,(B1,B2,B3),BN):-
  did_optimize_conj(Head,B1,B2,B12),
  optimize_conjuncts(Head,B12,B3,BN),!.
%optimize_conjuncts(Head,(B1,B2),BN1):- optimize_conj(Head,B1,B2,BN1).
optimize_conjuncts(Head,(B1,B2),BN1):- did_optimize_conj(Head,B1,B2,BN1),!.
optimize_conjuncts(Head,B1,B2,(BN1,BN2)):-
   must_optimize_body(Head,B1,BN1), must_optimize_body(Head,B2,BN2).

optimize_conj(_, x_assign(Term, C), x_assign(True,CC), eval_true(Term)):- 'True'==True, CC==C.
optimize_conj(_, x_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(_, B1,BT,B1):- assumed_true(BT),!.
optimize_conj(_, BT,B1,B1):- assumed_true(BT),!.
%optimize_conj(Head, x_assign(Term, C), x_assign(True,CC), Term):- 'True'==True,
%     optimize_conj(Head, x_assign(Term, C), is_True(CC), CTerm).
%optimize_conj(Head,B1,BT,BN1):- assumed_true(BT),!, optimize_body(Head,B1,BN1).
%optimize_conj(Head,BT,B1,BN1):- assumed_true(BT),!, optimize_body(Head,B1,BN1).
optimize_conj(Head,B1,B2,(BN1,BN2)):-
   optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).

assumed_true(B2):- var(B2),!,fail.
assumed_true(eval_true(B2)):-!,assumed_true(B2).
assumed_true(B2):- B2== true,!.
assumed_true(B2):- B2==x_assign('True', '$VAR'('_')),!.
assumed_true(X==Y):- assumed_true(X=Y).
assumed_true(X=Y):- var(X),var(Y), X=Y.
assumed_true(X=Y):- is_ftVar(X),is_ftVar(Y), X=Y.


filter_head_arg(H,F):- var(H),!,H=F.
filter_head_arge(H,F):- H = F.

code_callable(Term,_CTerm):- var(Term),!,fail.
code_callable(Term, CTerm):- current_predicate(_,Term),!,Term=CTerm.
%code_callable(Term, CTerm):- current_predicate(_,Term),!,Term=CTerm.

compile_test_then_else(RetResult,If,Then,Else,Converted):-
  f2p(HeadIs,ThenResult,Then,ThenCode),
  f2p(HeadIs,ElseResult,Else,ElseCode),
  Converted=(If*->(ThenCode,ThenResult=RetResult);
                  (ElseCode,ElseResult=RetResult)).

:- discontiguous(compile_flow_control/4).


compile_flow_control(_HeadIs,RetResult,Convert, x_assign(Convert,RetResult)) :-   is_ftVar(Convert), var(RetResult),!.

compile_flow_control(_HeadIs,_RetResult,Convert,_):- \+ compound(Convert),!,fail.
compile_flow_control(_HeadIs,_RetResult,Convert,_):- compound_name_arity(Convert,_,0),!,fail.

:- op(700,xfx, =~).
compile_flow_control(HeadIs,RetResult,Convert, (Code1,Eval1Result=Result,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ chain(Eval1,Result,Eval2),!,
   f2p(HeadIs,Eval1Result,Eval1,Code1),
   f2p(HeadIs,RetResult,Eval2,Converted).

compile_flow_control(HeadIs,ResValue2,Convert, (CodeForValue1,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['eval-in-space',Value1,Value2],
    f2p(HeadIs,ResValue1,Value1,CodeForValue1),
    f2p(HeadIs,ResValue2,Value2,CodeForValue2),
   Converted = with_space(ResValue1,CodeForValue2).


compile_flow_control(_HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['bind!',Var,Value],is_ftVar(Value),!,
   Converted = eval_args(['bind!',Var,Value],RetResult).
compile_flow_control(_HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['bind!',Var,Value], Value =~ ['new-space'],!,
   Converted = eval_args(['bind!',Var,Value],RetResult).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['bind!',Var,Value],
   f2p(HeadIs,ValueResult,Value,ValueCode),
   Converted = (ValueCode,eval_args(['bind!',Var,ValueResult],RetResult)).

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- %  dif_functors(HeadIs,Convert),
  once(Convert =~ if(Cond,Then,Else);Convert =~ 'if'(Cond,Then,Else)),
  !,Test = is_True(CondResult),
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


compile_flow_control(_HeadIs,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_ftVar(ValueL),
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    Converted = eval_args(['superpose',ValueL],RetResult),
    cname_var('MeTTa_SP_',ValueL).

compile_flow_control(HeadIs,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_list(ValueL),
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    cname_var('SP_Ret',RetResult),
    maplist(f2p_assign(HeadIs,RetResult),ValueL,CodeForValueL),
    list_to_disjuncts(CodeForValueL,Converted),!.


maybe_unlistify([UValueL],ValueL,RetResult,[URetResult]):- fail, is_list(UValueL),!,
  maybe_unlistify(UValueL,ValueL,RetResult,URetResult).
maybe_unlistify(ValueL,ValueL,RetResult,RetResult).

list_to_disjuncts([],false).
list_to_disjuncts([A],A):- !.
list_to_disjuncts([A|L],(A;D)):-  list_to_disjuncts(L,D).


%f2p_assign(_HeadIs,V,Value,is_True(V)):- Value=='True'.
f2p_assign(_HeadIs,ValueR,Value,ValueR=Value):- \+ compound(Value),!.
f2p_assign(_HeadIs,ValueR,Value,ValueR=Value):- is_ftVar(Value),!.
f2p_assign(HeadIs,ValueResult,Value,Converted):-
   f2p(HeadIs,ValueResultR,Value,CodeForValue),
   %into_equals(ValueResultR,ValueResult,ValueResultRValueResult),
   ValueResultRValueResult = (ValueResultR=ValueResult),
   combine_code(CodeForValue,ValueResultRValueResult,Converted).

compile_flow_control(HeadIs,RetResult,Convert,Converted) :-
  Convert =~ ['println!',Value],!,
  Converted = (ValueCode,eval_args(['println!',ValueResult], RetResult)),
  f2p(HeadIs,ValueResult,Value,ValueCode).



compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,PNil],[]==PNil,!,Converted = (ValueCode,RetResult=[]),
      f2p(HeadIs,_ValueResult,Value,ValueCode).


compile_flow_control(HeadIs,RetResult,Convert, (ValueCode, Converted)) :-
  Convert =~ ['case',Value|Options], \+ is_ftVar(Value),!,
  cname_var('CASE_EVAL_',ValueResult),
  compile_flow_control(HeadIs,RetResult,['case',ValueResult|Options], Converted),
  f2p(HeadIs,ValueResult,Value,ValueCode).

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

compile_flow_control(HeadIs,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,[Opt|Options]],nonvar(Opt),!,
   must_det_ll((
    compile_case_bodies(HeadIs,Opt,caseStruct(Value,If,RetResult,Then)),
    Converted = ( If -> Then ; Else ),
    ConvertCases =~ ['case',Value,Options],
    compile_flow_control(HeadIs,RetResult,ConvertCases,Else))).


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
    % TODO: Plus seems an odd name for a variable - get an idea why?
    transpile_prefix(Prefix),
    Convert =~ [Plus,N,Value], atom(Plus),
    atom_concat(Prefix,Plus,PrefixPlus),
    current_predicate(PrefixPlus/3), number(N),
    \+ number(Value), \+ is_ftVar(Value),!,
    f2p(HeadIs,ValueResult,Value,CodeForValue),!,
    Converted =.. [PrefixPlus,N,ValueResult,RetResult],
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

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- %dif_functors(HeadIs,Convert),
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


/*
% match(Space,f(1)=Y,Y)
compile_flow_control(HeadIs,Y,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert=~ match(Space,AsFunctionY,YY),
    nonvar(AsFunctionY),( AsFunctionY =~ (AsFunction=Y)), nonvar(AsFunction),
    !, Y==YY,
    f2p(HeadIs,Y,AsFunction,Converted),!.
*/
compile_flow_control(HeadIs,Atom,Convert,Converted) :-
   Convert=~ match(Space,Q,T),Q==T,Atom=Q,!,
  compile_flow_control(HeadIs,Atom,'get-atoms'(Space),Converted).

compile_flow_control(_HeadIs,Match,Convert,Converted) :-
    Convert=~ 'get-atoms'(Space),
    Converted = metta_atom_iter(Space,Match).

compile_flow_control(HeadIs,AtomsVar,Convert,Converted) :-
    Convert=~ 'get-atoms'(Space), AtomsVar = Pattern,
    compile_pattern(HeadIs,Space,Pattern,Converted).

compile_flow_control(HeadIs,RetResult,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(Space,Pattern,Template),!,
    f2p(HeadIs,RetResult,Template,TemplateCode),
    compile_pattern(HeadIs,Space,Pattern,SpacePatternCode),
    combine_code(SpacePatternCode,TemplateCode,Converted).

compile_pattern(_HeadIs,Space,Match,SpaceMatchCode):-
  SpaceMatchCode = metta_atom_iter(Space,Match).

metta_atom_iter(Space,Match):-
  metta_atom_iter('=',10,Space,Space,Match).



make_with_space(Space,MatchCode,MatchCode):- Space=='&self',!.
make_with_space(Space,MatchCode,with_space(Space,MatchCode)):- Space\=='&self'.

compile_flow_control(HeadIs,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(_Space,Match,Template),!,
   must_det_ll((
    f2p(HeadIs,_,Match,MatchCode),
    into_equals(RetResult,Template,TemplateCode),
    combine_code(MatchCode,TemplateCode,Converted))).

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

% If Convert is a variable, the corresponding predicate is just eval_args(Convert, RetResult)
f2p(_HeadIs,RetResult,Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = eval_args(Convert, RetResult).  % Set Converted to eval_args(Convert, RetResult)

% If Convert is a variable, the corresponding predicate is just eval_args(Convert, RetResult)
f2p(_HeadIs,RetResult,Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = eval_args(Convert, RetResult).  % Set Converted to eval_args(Convert, RetResult)
f2p(_HeadIs,RetResult,Convert, RetResultConverted) :-
     number(Convert),!,into_equals(RetResult,Convert,RetResultConverted).

f2p(_HeadIs,RetResult,Convert, Converted) :- % HeadIs\=@=Convert,
     is_arity_0(Convert,F), !, Converted = x_assign([F],RetResult),!.



/*f2p(HeadIs,RetResult, ConvertL, (Converted,RetResultL=RetResult)) :- is_list(ConvertL),
   maplist(f2p_assign(HeadIs),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Converted).*/

% If Convert is an "eval_args" function, we convert it to the equivalent "is" predicate.
f2p(HeadIs,RetResult,EvalConvert,Converted):- EvalConvert =~ eval_args(Convert),  !,
  must_det_ll((f2p(HeadIs,RetResult,Convert, Converted))).

% placeholder

f2p(HeadIs,RetResult,Convert, Converted):-
    compound(Convert), Convert = x_assign(C, Var), compound_non_cons(C),into_list_args(C,CC),!,
    f2p(HeadIs,RetResult,x_assign(CC, Var), Converted).

f2p(_HeadIs,_RetResult,Convert, Converted):-
    compound(Convert), Convert = x_assign(C, _Var), is_list(C),Converted = Convert,!.

f2p(HeadIs,RetResult,Convert, Converted) :-
     atom(Convert),  functional_predicate_arg(Convert,Nth,Nth2),
      Nth==1,Nth2==1,
      HeadIs\=@=Convert,
      Convert = F,!,
      must_det_ll((
        do_predicate_function_canonical(FP,F),
        compound_name_list(Converted,FP,[RetResult]))).

% PLACEHOLDER

% If Convert is an "is" function, we convert it to the equivalent "is" predicate.
f2p(HeadIs,RetResult,is(Convert),(Converted,is(RetResult,Result))):- !,
   must_det_ll((f2p(HeadIs,Result,Convert, Converted))).

% If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
f2p(HeadIs,RetResult,or(AsPredI,Convert), (AsPredO *-> true; Converted)) :- fail, !,
  must_det_ll((f2p(HeadIs,RetResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))).

f2p(HeadIs,RetResult,(AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_ll((f2p(HeadIs,RetResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))).
f2p(HeadIs,RetResult,SOR,or(AsPredO, Converted)) :-
  SOR =~ or(AsPredI, Convert),
  must_det_ll((f2p(HeadIs,RetResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))),!.

% If Convert is a "," (and) function, we convert it to the equivalent "," (and) predicate.
f2p(HeadIs,RetResult,(AsPredI, Convert), (AsPredO, Converted)) :- !,
  must_det_ll((f2p(HeadIs,_RtResult,AsPredI, AsPredO),
               f2p(HeadIs,RetResult,Convert, Converted))).

% If Convert is a ":-" (if) function, we convert it to the equivalent ":-" (if) predicate.
f2p(_HeadIs,RetResult, Convert, Converted) :- Convert =(H:-B),!,
  RetResult=(H:-B), Converted = true.

f2p(_HeadIs,_RetResult, N=V, Code) :- !, into_equals(N,V,Code).





% If Convert is a list, we convert it to its termified form and then proceed with the functs_to_preds conversion.
f2p(HeadIs,RetResult,Convert, Converted) :- fail,
   is_list(Convert),
   once((sexpr_s2p(Convert,IS), \+ IS=@=Convert)), !,  % Check if Convert is a list and not in predicate form
   must_det_ll((f2p(HeadIs,RetResult, IS, Converted))).  % Proceed with the conversion of the predicate form of the list.

f2p(HeadIs,RetResult, ConvertL, Converted) :- fail,
   is_list(ConvertL),
   maplist(f2p_assign(HeadIs),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Conjs),
   into_x_assign(RetResultL,RetResult,Code),
   combine_code(Conjs,Code,Converted).


f2p(HeadIs,RetResultL, ConvertL, Converted) :- fail,
   is_list(ConvertL),
   ConvertL = [Convert],
   f2p(HeadIs,RetResult,Convert, Code), !,
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

% If any sub-term of Convert is an eval_args/2, convert that sub-term and then proceed with the conversion.
f2p(HeadIs,RetResult,Convert, Converted) :-
    rev_sub_sterm0(ConvertFunction, Convert), % Get the deepest sub-term AsFunction of Convert
    callable(ConvertFunction),  % Check if AsFunction is callable
    ConvertFunction = eval_args(AsFunction,Result),
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



de_eval(eval(X),X):- compound(X),!.

call1(G):- call(G).
call2(G):- call(G).
call3(G):- call(G).
call4(G):- call(G).
call5(G):- call(G).

trace_break:- trace,break.

:- if(debugging(metta(compiler_bugs))).
:- set_prolog_flag(gc,false).
:- endif.

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
   (into_x_assign(AsFunction, Result,AsPred)
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
   append(Left,[eval_args(AsBodyFunction,Result)|Right],NewRevList),
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
    % The function equivalent of AsPred _xs Result in Convert
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs0(Converting, Converted).

% Handles the special case where eval_args/2 is used and returns the function represented by the first argument of eval_args/2
preds_to_functs0(eval_args(AsFunction, _Result), AsFunction) :- !.

% Handles the general case where Convert is a conjunction.
% It converts the predicates to functions inside a conjunction
preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

% Handles the case where AsPred is a compound term that can be converted to a function
preds_to_functs0(AsPred, eval_args(AsFunction, Result)) :-
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



compile_for_assert_eq(_Eq,H,B,Result):-
  compile_for_assert(H,B,Result), !.

:- dynamic(metta_compiled_predicate/3).

same(X,Y):- X =~ Y.



