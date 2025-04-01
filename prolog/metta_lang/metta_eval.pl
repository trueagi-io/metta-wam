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

% When the the `metta_interp` library is loaded, it makes sure the rest of the files are intially loaded in
% the correct order independent of which file is loaded first the needed predicates and ops are defined.
:- ensure_loaded(metta_interp).

% post match modew
%:- style_check(-singleton).
:- multifile(fake_notrace/1).
:- meta_predicate(fake_notrace(0)).
:- meta_predicate(color_g_mesg(+,0)).
:- multifile(color_g_mesg/2).

%self_eval0(X):- var(X),!,fail.
self_eval0(X):- \+ callable(X),!.
self_eval0(X):- is_valid_nb_state(X),!.
%self_eval0(X):- string(X),!.
%self_eval0(X):- number(X),!.
%self_eval0([]).
self_eval0(X):- is_metta_declaration(X),!.
self_eval0([_,Ar,_|T]):- T==[],(Ar=='-->';Ar=='<->';Ar=='<--';Ar==':-'),!.
self_eval0([F|X]):- !, self_eval_ht(F,X).
self_eval0(X):- compound(X),!.
self_eval0(X):- typed_list(X,_,_),!.
self_eval0(X):- py_is_py(X),!.
%self_eval0(X):- py_type(X,List), List\==list,!.
%self_eval0(X):- is_ref(X),!,fail.
self_eval0('True'). self_eval0('False'). % self_eval0('F').
%self_eval0('Empty').
self_eval0([]).
self_eval0('%Undefined%').
self_eval0(X):- atom(X),!, X\=='NotReducible', \+ nb_bound(X,_),!.

self_eval_ht(F,X):- !, nonvar(F),is_list(X),length(X,Len),is_self_eval_l_fa(F,Len),!.
self_eval_ht(F,X):- sub_term_safely(E,[F|X]), nonvar(E), E\==[], E\=='.', ( \+ is_list(E) ),!,fail.
self_eval_ht(_,_).


nb_bound(Name,X):- atom(Name), % atom_concat('&', _, Name),
  nb_current(Name, X),!. % spaces and states are stored as compounds
nb_bound(Name,X):- atom(Name), % atom_concat('&', _, Name),
  call_in_shared_space(nb_current(Name, X)),!.  % spaces and states are stored as compounds

call_in_shared_space(G):- call_in_shared_thread(main,G).
call_in_shared_thread(Thread,Goal):- thread_self(Self),Thread==Self,!,call(Goal).
call_in_shared_thread(_Thread,Goal):- call(Goal). % should use call_in_thread/2 (but it blocks lazy calls)

nb_bind(Name,Value):- nb_current(Name,Was),same_term(Value,Was),!.
%nb_bind(Name,Value):- call_in_shared_space(nb_current(Name,Was)),same_term(Value,Was),!.
nb_bind(Name,Value):-
   duplicate_deep_term(Value,NewValue),
   call_in_shared_space(nb_linkval(Name,NewValue)),!.

duplicate_deep_term(Value,NewValueValue):- duplicate_term(Value,NewValue),
  deep_setarg(NewValue,NewValueValue).


deep_setarg(NewValue,Value):- \+ compound(NewValue),!, Value=NewValue.
deep_setarg(NewValue,Value):- Value=NewValue,!.
/*
deep_setarg(NewValue,Value):- functor(NewValue,_,N),deep_setarg(N,NewValue,Value).
deep_setarg(N,NewValue,Value):- arg(N,NewValue,E),deep_setarg(E,EV),nb_linkarg(N,Value,EV),
   (N==1 -> true ; (succ(Nm1,N),deep_setarg(Nm1,NewValue,Value))).
*/

coerce(Type,Value,Result):- nonvar(Value),Value=[Echo|EValue], Echo == echo, EValue = [RValue],!,coerce(Type,RValue,Result).
coerce(Type,Value,Result):- var(Type), !, Value=Result, freeze(Type,coerce(Type,Value,Result)).

coerce('Atom',Value,Result):- !, Value=Result.

coerce('Bool',Value,Result):- var(Value), !, Value=Result, freeze(Value,coerce('Bool',Value,Result)).
coerce('Bool',Value,Result):- Value=0, !, Result='False'.
coerce('Bool',Value,Result):- Value='False', !, Result='False'.
coerce('Bool',Value,Result):- is_list(Value), length(Value, 0), !, Result='False'.
coerce('Bool',_Valu,Result):- !, Result='True'.

coerce('Number',Value,Result):- number(Value), !, Value=Result.
coerce('Number',Value,Result):- string(Value), !, number_string(Result, Value).
coerce('Number',Value,Result):- Value='False', !, Result=0.
coerce('Number',Value,Result):- Value='True', !, Result=1.
coerce('Number',Value,Result):- atom(Value), !, atom_number(Value, Result).

coerce('String', Value, Result):- string(Value), !, Value=Result.
coerce('String', Value, Result):- term_string(Value,Result), !.

coerce(Type, Value, Result):-
  (get_type(Value,ValuesType);ValuesType='Any'),
   freeze(Nonvar,Nonvar='def-coerce'),
   current_self(KB),metta_atom(KB,[Nonvar,ValuesType,Type,Function]),nonvar(Nonvar),
   eval([Function,Value],Result),!.

set_list_value(Value,Result):- nb_setarg(1,Value,echo),nb_setarg(1,Value,[Result]).

%is_self_eval_l_fa('S',1). % cheat to comment

% these should get uncomented with a flag
%is_self_eval_l_fa(':',2).
% is_self_eval_l_fa('=',2).
% eval_20(Eq,RetType,Depth,Self,['quote',Eval],RetVal):- !, Eval = RetVal, check_returnval(Eq,RetType,RetVal).
is_self_eval_l_fa('quote',1).
is_self_eval_l_fa('Error',_).
is_self_eval_l_fa('{...}',_).
is_self_eval_l_fa('[...]',_).

self_eval(X):- notrace(self_eval0(X)).

:-  set_prolog_flag(access_level,system).
hyde(F/A):- functor(P,F,A), redefine_system_predicate(P),'$hide'(F/A), '$iso'(F/A).
:- 'hyde'(option_else/2).
:- 'hyde'(atom/1).
:- 'hyde'(quietly/1).
%:- 'hyde'(fake_notrace/1).
:- 'hyde'(var/1).
:- 'hyde'(is_list/1).
:- 'hyde'(copy_term/2).
:- 'hyde'(nonvar/1).
:- 'hyde'(quietly/1).
%:- 'hyde'(option_value/2).


is_metta_declaration([F|_]):- F == '->',!.
is_metta_declaration([F,H,_|T]):- T ==[], is_metta_declaration_f(F,H).

is_metta_declaration_f(F,H):- F == ':<', !, nonvar(H).
is_metta_declaration_f(F,H):- F == ':>', !, nonvar(H).
is_metta_declaration_f(F,H):- F == '=', !, is_list(H),  \+ (current_self(Space), is_user_defined_head_f(Space,F)).

% is_metta_declaration([F|T]):- is_list(T), is_user_defined_head([F]),!.

% Sets the current self space to '&self'. This is likely used to track the current context or scope during the evaluation of Metta code.
%:- nb_setval(self_space, '&self').


%current_self(Space):- nb_current(self_space,Space).

do_expander('=',_,X,X):-!.
do_expander(':',_,X,Y):- !, get_type(X,Y)*->X=Y.

get_type(Arg,Type):- eval_H(['get-type',Arg],Type).


%! eval_true(+X) is semidet.
% Evaluates the given term X and succeeds if X is not a constraint (i.e. \+ iz_conz(X)) and is callable, and calling X succeeds.
%
% If X is not callable, this predicate will attempt to evaluate the arguments of X (using eval_args/2) and succeed if the result is not False.
eval_true(X):- \+ iz_conz(X), callable(X),!, call(X).
eval_true(X):- eval_args(X,Y), is_True(Y) , !.

eval(Depth,Self,X,Y):- eval('=',_,Depth,Self,X,Y).
eval(Eq,RetType,Depth,Self,X,Y):-
  catch_metta_return(eval_args(Eq,RetType,Depth,Self,X,Y),Y).

%:- set_prolog_flag(gc,false).
/*
eval_args(Eq,RetTyp e,Depth,Self,X,Y):-
   locally(set_prolog_flag(gc,true),
      rtrace_on_existence_error(
     eval(Eq,RetType,Depth,Self,X,Y))).
*/


%! eval_args(+X,-Y) is semidet.
eval_args(X,Y):- current_self(Self),
   default_depth(DEPTH),
   eval_args(DEPTH,Self,X,Y).
%eval_args(Eq,RetType,Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval_args(Eq,RetType,X)),fail.
eval_args(Depth,Self,X,Y):- eval_args('=',_RetType,Depth,Self,X,Y).

%! eval_to(+X,+Y) is semidet.
% checks if X evals to Y
evals_to(XX,Y):- Y=@=XX,!.
evals_to(XX,Y):- Y=='True',!, is_True(XX),!.

eval_args(_Eq,_RetType,_Dpth,_Slf,X,Y):- var(X),nonvar(Y),!,X=Y.
eval_args(_Eq,_RetType,_Dpth,_Slf,X,Y):- notrace(self_eval(X)),!,Y=X.

eval_args(Eq,RetType,Depth,Self,X,Y):-
    notrace(nonvar(Y)), var(RetType),
% super safety checks is optional code that can be ran .. normally this is done with assertion/1 but unfortionately assertion/1 is not guarenteed to keep bindings (THUS WOULDNT HAVE WORED HERE) and can be said to be wrapped in `once/1`
    super_safety_checks(copy_term(Y,YC)),
    get_type(Depth,Self,Y,WasType),
    super_safety_checks(must_det_ll(Y=@=YC)),
    can_assign(WasType,RetType),
    nonvar(RetType),!,
    eval_args(Eq,RetType,Depth,Self,X,Y).
eval_args(Eq,RetType,Depth,Self,X,Y):- notrace(nonvar(Y)),!,
   eval_args(Eq,RetType,Depth,Self,X,XX),evals_to(XX,Y).

eval_args(Eq,RetType,_Dpth,_Slf,[X|T],Y):- T==[], number(X),!, do_expander(Eq,RetType,X,YY),Y=[YY].

/*
eval_args(Eq,RetType,Depth,Self,[F|X],Y):-
  (F=='superpose' ; ( option_value(no_repeats,false))),
  notrace((D1 is Depth-1)),!,
  eval_args(Eq,RetType,D1,Self,[F|X],Y).
*/

eval_args(Eq,RetType,Depth,Self,X,Y):- atom(Eq),  ( Eq \== ('='),  Eq \== ('match')) ,!,
   call(call,Eq,'=',RetType,Depth,Self,X,Y).

eval_args(_Eq,_RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args(Eq,RetType,Depth,Self,X,Y):-
  eval_00(Eq,RetType,Depth,Self,X,Y).
%eval_ret(Eq,RetType,1000,Self,X,Y):- !,
%  catch_metta_return(eval_ret(Eq,RetType,99,Self,X,Y),Y).

eval_ret(Eq,RetType,Depth,Self,X,Y):-
    eval_00(Eq,RetType,Depth,Self,X,Y), is_returned(Y).

catch_metta_return(G,Y):-
 catch(G,metta_return(Y),ignore(retract(thrown_metta_return(Y)))).

allow_repeats_eval_(_):- !.
allow_repeats_eval_(_):- option_value(no_repeats,false),!.
allow_repeats_eval_(X):- \+ is_list(X),!,fail.
allow_repeats_eval_([F|_]):- atom(F),allow_repeats_eval_f(F).
allow_repeats_eval_f('superpose').
allow_repeats_eval_f('collapse').

as_prolog_x(_,Self,X,XX):- quietly(as_prolog(0,Self,X,XX)), !.

:- nodebug(metta(overflow)).
eval_00(_Eq,_RetType,_Depth,_Slf,X,Y):- self_eval(X),!,X=Y.
eval_00(Eq,RetType,Depth,Self,X,YO):-
   as_prolog_x(Depth,Self,X,XX),
   eval_001(Eq,RetType,Depth,Self,XX,YO).
eval_001(_Eq,_RetType,_Depth,_Slf,X,Y):- self_eval(X),!,X=Y.
eval_001(Eq,RetType,Depth,Self,X,YO):-
   eval_01(Eq,RetType,Depth,Self,X,YO).

eval_01(_Eq,_RetType,Depth,_Self,X,YO):- Depth<0,bt,trace,!,X=YO.
eval_01(Eq,RetType,Depth,Self,X,Y):-
    % X\==[empty], % speed up n-queens x60 but breaks other things

   notrace((Depth2 is Depth-1,    copy_term(X, XX))),!,

   if_t((Depth<1, trace_on_overflow), debug(metta(e))),

   trace_eval(eval_10(Eq,RetType),e,Depth2,Self,X,M),

   ((M=@=XX;M==X;M=@=X) -> Y=M ; eval_03(Eq,RetType,Depth2,Self,M,Y)).

%eval_03(_Eq,RetType,_Depth2,_Self,M,Y):- RetType=='Atom',!,M=Y.
eval_03(Eq,RetType,Depth2,Self,M,Y):- eval_01(Eq,RetType,Depth2,Self,M,Y).

eval_02(Eq,RetType,Depth,Self,Y,YO):- var(Y),!,YO=Y,var_pass(Eq,RetType,Depth,Self,Y).
eval_02(Eq,RetType,Depth2,Self,Y,YO):-  %Y\==[empty], % speed up n-queens x60  but breaks other things
  once(if_or_else((subst_args_here(Eq,RetType,Depth2,Self,Y,YO)),
    if_or_else((fail,finish_eval(Eq,RetType,Depth2,Self,Y,YO)),
        Y=YO))).

% TODO constrain Y
var_pass(_Eq,_RetType,_Depth,_Self,_Y):-!.

subst_once(G):- call(G).

% subst_args_here(Eq,RetType,Depth2,Self,Y,YO):-
%   Y =@= [ house, _59198,_59204,==,fish,fish],!,break.

% %  this needs to test itself for when it can be skipped
% %  uncommented causes 7% failure but a 10x speedup
% subst_args_here(Eq,RetType,Depth2,Self,Y,YO):- Y=YO.
% %  this next one at least causes no failures and 5x speedup
subst_args_here(_Eq,_RetType,_Depth2,_Self,Y,YO):- iz_conz(Y), \+ is_list(Y), !, bt,trace, break, Y=YO.
%subst_args_here(Eq,RetType,Depth2,Self,Y,YO):- !, Y=YO.
subst_args_here(Eq,RetType,Depth2,Self,Y,YO):- !,
  subst_once(subst_args(Eq,RetType,Depth2,Self,Y,YO)*->true;Y=YO).
%subst_args_here(Eq,RetType,Depth2,Self,Y,YO):- !, finish_eval(Eq,RetType,Depth2,Self,Y,YO).

subst_args_here(Eq,RetType,Depth,Self,H,BBBO):-
  must_det_lls(if_or_else(subst_args_there(Eq,RetType,Depth,Self,H,BBB),H=BBB)),
  sanity_check_eval(subst_args_there,BBB),BBBO=BBB.
%subst_args_here(_Eq,_RetType,_Depth2,_Self,Y,YO):- wont_need_subst(Y),!, Y=YO.

trust_wont_need_substX.
use_breakpoints(_).

%subst_args_there(Eq,RetType,Depth,Self,Y,YO):- trust_wont_need_substX,wont_need_substX(Y),!,YO=Y,var_pass(Eq,RetType,Depth,Self,Y).
subst_args_there(Eq,RetType,Depth2,Self,Y,YO):-
  wont_need_substX(Y),!,
  subst_args_there_real(Eq,RetType,Depth2,Self,Y,YO),
    use_breakpoints(if_t(Y\=@=YO,
       (if_tracemsg(unknown,wont_need_substX(Y)),
            rtrace(subst_args_there_real(Eq,RetType,Depth2,Self,Y,YO)),break))).

subst_args_there(Eq,RetType,Depth2,Self,Y,YO):-
   subst_args_there_real(Eq,RetType,Depth2,Self,Y,YO),
    use_breakpoints((if_t( \+ trust_wont_need_substX,
       if_t(Y=@=YO, (if_tracemsg(unknown,subst_args_there_real(Y,YO)),
           maybe_trace(unknown),rtrace(subst_args_there_real(Eq,RetType,Depth2,Self,Y,YO)),break))))).

subst_args_there_real(Eq,RetType,Depth2,Self,Y,YO):- !,
  subst_once(subst_args(Eq,RetType,Depth2,Self,Y,YO)*->true;Y=YO).

subst_args_there_real(Eq,RetType,Depth2,Self,Y,YO):-
  subst_once(subst_args(Eq,RetType,Depth2,Self,Y,YO)),
  %Y =YO,
  notrace(if_t_else((wont_need_subst(Y),Y\=@=YO),
     (write_src_uo(needed_subst_args(Y,YO)),bt,sleep(1.0)),
  nop(write_src_uo(unneeded_subst_args(Y))))).

wont_need_substX(List):- \+ is_list(List),!.
wont_need_substX(X):- self_eval(X),!.
wont_need_substX(V):- var(V),!.
wont_need_substX([_,A|_]):- number(A),!,fail.
wont_need_substX([F|_]):-atom(F), need_subst_f(F), !, fail.
wont_need_substX(List):- maplist(wont_need_substX,List),!.
wont_need_substX(_).


wont_need_subst(List):- \+ is_list(List),!.
wont_need_subst([_,A|_]):- number(A),!,fail.
wont_need_subst([F|_]):-atom(F), need_subst_f(F), !, fail.
wont_need_subst(List):- maplist(wont_need_subst,List),!.
wont_need_subst(_).

need_subst_f('==').
% ['Mortal','Socrates'] -> 'T'
need_subst_f('Mortal').
need_subst_f('*'). need_subst_f('+').
need_subst_f('-'). need_subst_f('/').
need_subst_f('<'). need_subst_f('=<').

if_t_else(If,Then,Else):- If -> Then ; Else.

finish_eval_here(Eq,RetType,Depth2,Self,Y,YO):-
  finish_eval(Eq,RetType,Depth2,Self,Y,YO),
  notrace(if_t(Y\=@=YO,write_src_uo(finish_eval(Y,YO)))).

:- nodebug(metta(e)).

:- discontiguous eval_09/6.
:- discontiguous eval_10/6.
:- discontiguous eval_20/6.
:- discontiguous eval_21/6.
:- discontiguous eval_30/6.
:- discontiguous eval_40/6.
:- discontiguous eval_41/6.
:- discontiguous maybe_eval_subst/6.
:- discontiguous eval_09_disabled/6.
:- discontiguous eval_10_disabled/6.
:- discontiguous eval_20_disabled/6.
:- discontiguous eval_21_disabled/6.
:- discontiguous eval_30_disabled/6.
:- discontiguous eval_40_disabled/6.
:- discontiguous eval_41_disabled/6.
:- discontiguous maybe_eval_subst_disabled/6.
%:- discontiguous eval_30fz/5.
%:- discontiguous eval_31/5.
%:- discontiguous maybe_eval_defn/5.
%:- discontiguous eval_40/5.
eval_to_name(X,Named):- sub_term_safely(Named,X),atomic(Named),Named\==[],!.
eval_to_name(X,x(X)).


is_mettalog_tracing(X,_):- var(X),!,fail.
is_mettalog_tracing([X|_],Type):- !, is_mettalog_tracing(X,Type).
is_mettalog_tracing(H,Type):- woc(metta_atom(_,[mettalog_trace,HH,Type])), \+ \+ H=HH, !.

eval_08(Eq,RetType,Depth,Self,X,Y):- is_mettalog_tracing(X,Type),!,
   with_debug(Type,eval_09(Eq,RetType,Depth,Self,X,Y)).
eval_08(Eq,RetType,Depth,Self,X,Y):- eval_09(Eq,RetType,Depth,Self,X,Y).

%eval_09(_Eq,_RetType, Depth,_Slf,X,Y):- Depth< 0, !, X=Y, fail.
%eval_09(_Eq,_RetType, Depth,_Slf,X,Y):- Depth< 1, !, X=Y.
%eval_09(_Eq,_RetType, Depth,_Slf,_X,_Y):- Depth<1, if_trace(e,bt),!, fail.

%eval_09(Eq,RetType,Depth,Self,X,Y):- woc(eval_10(Eq,RetType,Depth,Self,X,Y).


%eval_09(Eq,RetType,Depth,Self,X,Y):- !, no_repeats(X+Y,eval_10(Eq,RetType,Depth,Self,X,Y)).
eval_09(Eq,RetType,Depth,Self,X,Y):- !,
     no_repeats_var(YY),
     eval_to_name(X,XX),!,
     eval_10(Eq,RetType,Depth,Self,X,Y), %break,
     (fail_on_repeat(XX,YY,X,Y) -> true ; ( %print_last_choicepoint_upwards,
                                             break,
                                            !)).

fail_on_repeat(ThisNth,YY,X,Y):-
     ((copy_term(X+Y,YC), YC = YY)
        -> nop(debug(metta(todo),'no_repeat in ~w: ~q',[ThisNth,X->Y]))
        ; once((dmsg('repeats in'([ThisNth,X->Y])),
               %bt,
               dumpST,
               dmsg('repeats in'([ThisNth,X->Y])),
               !,fail))).

eval_09_11(Eq,RetType,Depth,Self,X,Y):-!,
    eval_to_name(X,Named),
     (nb_current(previous_nths,NthL)->true;NthL=[]),
     append(NthL,[Named],StartNth), nb_setval(previous_nths,StartNth),nb_setval(this_nths,StartNth),
     no_repeats_var(YY),
     call_cleanup(
       (call_nth(eval_10(Eq,RetType,Depth,Self,X,Y),Nth),
         append(NthL,[Named-Nth],ThisNth),nb_setval(previous_nths,ThisNth),
         nb_setval(previous_nths,NthL),
         nb_setval(this_nths,ThisNth),
         ((copy_term(X+Y,YC), YC = YY) -> (debug(metta(todo),'no_repeat in ~w: ~q',[ThisNth,X->Y])) ; (debug(metta(todo),'repeats in ~w: ~q',[ThisNth,X->Y]),fail))),
       nb_setval(previous_nths,NthL)).

/*
eval_09(Eq,RetType,Depth,Self,X,Y):-
   if_or_else((eval_10(Eq,RetType,Depth,Self,X,Y),nonvar(Y)),
      (rtrace(eval_10(Eq,RetType,Depth,Self,X,Y)),break)).
*/
:- nodebug(metta(todo)).
eval_10(_Eq,_RetType,_Dpth,_Self,X,YO):- self_eval(X),!,YO=X.
eval_10(_Eq,_RetType,_Dpth,_Self,X,_YO):- X==[empty],!,fail.
eval_10(_Eq,_RetType,_Dpth,_Self,X,_YO):- X==['Empty'],!,fail.
eval_10(_Eq,_RetType,Depth,_Self,X,YO):- Depth<0,bt,trace,!,X=YO.
eval_10(Eq,RetType,Depth,Self,X,Y):- var(X), !, % sanity_check_eval(eval_10_var,X),
  eval_20(Eq,RetType,Depth,Self,X,Y).

insanity_check_eval(_,_):- is_testing,!,fail.
insanity_check_eval(Which,X):- var(X),!, \+ sub_var_safely(X,Which),if_tracemsg(unknown,insanity_check_eval(Which,X)),!,maybe_trace(unknown).
insanity_check_eval(Which,X):-  X=@=[_|_],if_tracemsg(unknown,insanity_check_eval(Which,X)),!,maybe_trace(unknown).

sanity_check_eval(_,_):- is_testing,!.
sanity_check_eval(_,_):- !.
sanity_check_eval(Which,X):- tracing,notrace,!,call_cleanup(\+ insanity_check_eval(Which,X), maybe_trace(unknown)),!.
sanity_check_eval(Which,X):- \+ insanity_check_eval(Which,X), !.

%eval_10(Eq,RetType,Depth,Self,X,Y):- \+ sanity_check_eval(eval_10_in,X),X=Y,!,var_pass(Eq,RetType,Depth,Self,Y).

eval_20(Eq,RetType,Depth,Self,X,Y):- var(X), !, % sanity_check_eval(eval_20_var,X),
  Y=X,!,var_pass(Eq,RetType,Depth,Self,Y).

%eval_20(Eq,RetType,Depth,Self,X,Y):- \+ sanity_check_eval(eval_20_in,X),X=Y,!,var_pass(Eq,RetType,Depth,Self,Y).

eval_10(Eq,RetType,Depth,Self,X,Y):-  \+ compound(X), !,
    as_prolog_x(Depth,Self,X,XX),
    eval_20(Eq,RetType,Depth,Self,XX,Y),sanity_check_eval(eval_20_not_compound,Y).

eval_10(Eq,RetType,Depth,Self,X,Y):-  \+ is_list(X), !,
  as_prolog_x(Depth,Self,X,XX),
  eval_20(Eq,RetType,Depth,Self,XX,Y),sanity_check_eval(eval_20_not_list,Y).

eval_10(Eq,RetType,Depth,Self,[Sym|Args],Y):- \+ atom(Sym), !,
  maplist(as_prolog_x(Depth,Self), [Sym|Args] , [ASym|Adjusted]),
  eval_20(Eq,RetType,Depth,Self, [ASym|Adjusted], Y),sanity_check_eval(eval_20_not_atom,Y).

eval_20(_Eq,_RetType,Depth,_Self,X,YO):- Depth<0,bt,trace,!,X=YO.
eval_20(Eq,RetType,_Dpth,_Slf,Name,Y):-
    atom(Name), !,
      (Name=='NotReducible'->throw(metta_NotReducible);
      (nb_bound(Name,X)->do_expander(Eq,RetType,X,Y);
       Y = Name)),
      sanity_check_eval(eval_20_atom,Y).

eval_20(_Eq,RetType,Depth,Self,[Sym|Args],Res):-
    atomic(Sym), py_is_function(Sym), is_list(Args), !,
    maplist(as_prolog_x(Depth,Self), Args , Adjusted),!,
    py_call_method_and_args(Sym,Adjusted,Ret),
    py_metta_return_value(RetType, Ret,Res).


eval_py_atom(_Eq,_RetType,_Depth,_Self,['py-atom',Arg],Res):-
    must_det_ll((py_atom(Arg,Res))).

eval_py_atom(_Eq,_RetType,_Depth,_Self,['py-atom',Arg,Type],Res):-
    must_det_ll((py_atom_type(Arg,Type,Res))).


was_py_call(Eq,RetType,Depth,Self,PyAtom,Sym,PArgs,ParamList,RRetType):-
   atomic(PyAtom), py_is_function(PyAtom), !, Sym = PyAtom,
   into_param_types(Eq,RetType,Depth,Self,Sym,PArgs,ParamList,RRetType).
was_py_call(Eq,RetType,Depth,Self,[PyAtom|Args],Sym,PArgs,ParamList,RRetType):- fail, PyAtom == 'py-atom',!,
  eval_py_atom(Eq,RetType,Depth,Self,[PyAtom|Args],Sym),
  into_param_types(Eq,RetType,Depth,Self,Args,PArgs,ParamList,RRetType).

into_param_types(_Eq,RetType,_Depth,_Self,_SymList,PArgs,ParamList,RetType):- length(PArgs,N),length(ParamList,N),!,
   maplist(=('%Undefined%'),ParamList).

apply_param_types_return(Depth,Self, Args,Res, ParamList,_RetType, Adjusted,Ret):-
    apply_param_types(Depth, Self,ParamList, Args , Adjusted),
    Res = Ret.


apply_param_type(Depth, Self,T,M,Y):- into_typed_arg(Depth, Self, T, M, Y).

apply_param_types(_Depth,_Self,_, Nil , O):- Nil == [] ,!, O = Nil.
apply_param_types(_Depth,_Self,_, Var , O):- var(Var), !, O = Var.
apply_param_types(Depth, Self,PT, [Y,A|Args] , [Z,Val|Adjusted]):-
   var(Y),nonvar(A), de_pcons(PT,X,TPT),de_pcons(TPT,T,RTPT),
   var(X),nonvar(T),!,
   apply_param_type(Depth, Self,T,A,Val), apply_param_types(Depth, Self,[X|RTPT],[Y|Args],[Z|Adjusted]).

apply_param_types(Depth, Self,PT, [A|Args] , [Val|Adjusted]):-
    de_pcons(PT,T,ParamList),
    apply_param_type(Depth, Self,T,A,Val), apply_param_types(Depth, Self,ParamList,Args,Adjusted).

de_pcons(Var,Var,VarT):- var(Var),!,copy_term(Var,VarT).
de_pcons([P|T],P,T):- T\==[], !.
de_pcons([P],P,[P]):- !.
de_pcons(Var,Var,VarT):-copy_term(Var,VarT).

/*
eval_20_disabled(Eq,RetType,Depth,Self,[PyAtom|Args],Res):-  fail,  is_list(Args), nonvar(PyAtom),
    was_py_call(Eq,RetType,Depth,Self,PyAtom,Sym,Args,ParamList,DeclRetType),!,
    narrow_types(RetType,DeclRetType,CombinedRetType),!,
    apply_param_types_return(Depth, Self,Args,Res,ParamList,CombinedRetType,Adjusted,Ret),
    py_call_method_and_args(Sym,Adjusted,Ret),
    py_metta_return_value(RetType,Ret,Res).
*/

legal_op(X):- must_det_lls(nonvar(X)).

is_py_atom(Var):- var(Var),!,fail.
is_py_atom('py-atom').
eval_20(Eq,RetType,Depth,Self,[PyAtom2|Args],Res):- is_list(PyAtom2), fail,
   PyAtom2 = [PyAtom,Sym,ArrowType],
   is_py_atom(PyAtom),!,
   Op = [PyAtom,Sym],
   eval_10(Eq,RetType,Depth,Self,['invoke-ftype',Op,ArrowType|Args],Res).

% !(invoke-ftype println! (-> Atom (->)) (+ 1 1))

eval_20(Eq,RetType,Depth,Self,[PyAtom1|Args],Res):-  is_list(PyAtom1), fail,
   PyAtom1 = [PyAtom,Sym],
   is_py_atom(PyAtom),!,
   Op = [PyAtom,Sym],
   eval_10(Eq,RetType,Depth,Self,['invoke',Op|Args],Res).

eval_10(Eq,RetType,Depth,Self,['invoke-for',Op,DeclRetType|Args],Res):- !, legal_op(Op),
   narrow_types(RetType,DeclRetType,CombinedRetType),
   eval_10(Eq,CombinedRetType,Depth,Self,['invoke',Op|Args],Res).

eval_10(Eq,RetType,_Depth,Self,['invoke-ftype',Op,ArrowType|Args],Ret):- !, legal_op(Op),
   arrow_type(ArrowType,ParamList,DeclRetType),
   narrow_types(RetType,DeclRetType,CombinedRetType),
   apply_param_types_return(Depth, Self,Args,Res,ParamList,CombinedRetType,Adjusted,Ret),
   eval_10(Eq,CombinedRetType,Depth,Self,['invoke',Op|Adjusted],Res).

eval_10(_Eq,RetType,_Depth,_Self,['invoke',Op|Args],Res):- !, legal_op(Op),
   must_det_lls(op_to_pred_call_ret(Op,Pred2,Ret3)),
   call(Pred2,Args, Ret),
   call(Ret3,RetType,Ret,Res).

op_to_pred_call_ret(PyAtom,Pred2,Ret3):- is_list(PyAtom),!,
   maybe_trace(unknown) ,eval_py_atom(_Eq,_RetType,_Depth,_Self,PyAtom,Res),!,op_to_pred_call_ret(Res,Pred2,Ret3).
op_to_pred_call_ret(PyAtom,Pred2,Ret3):-
  py_is_function(PyAtom), !, Sym = PyAtom,
  Pred2= py_call_method_and_args(Sym),
  Ret3= py_metta_return_value().

eval_10(_Eq,_RetType,Depth,Self, ['apply-param-types',Convert,Args],Res):- !, apply_param_types(Depth, Self, Convert, Args , Res).

eval_10(_Eq,_RetType,Depth,Self, ['apply-param-type',Convert,Arg],Res):- !, apply_param_type(Depth, Self, Convert, Arg, Res).


% ((py-atom type) "string")    <class 'str'>

py_metta_return_value(_Suggest,Ret,[]):- Ret=='@'(none),!.
py_metta_return_value(_Suggest,Ret,'True'):- Ret=='@'(true),!.
py_metta_return_value(_Suggest,Ret,'False'):- Ret=='@'(false),!.
py_metta_return_value(_Suggest,IO,IO).

eval_20(Eq,RetType,_Dpth,_Slf,X,Y):- no_eval(X),!,do_expander(Eq,RetType,X,Y).

args_not_evaled(X):- ( \+ is_list(X); maplist(no_eval,X)),!.
no_eval(X):- self_eval(X),!.
no_eval([SL|X]):- atomic(SL), !, is_sl(SL), args_not_evaled(X).
no_eval([SL|X]):- ( \+ atom(SL), \+ is_list(SL)), !,
          args_not_evaled(X).
is_sl(N):- number(N),!.
is_sl('ExtSet').
is_sl('IntSet').
%is_sl('___').

% =================================================================
% =================================================================
% =================================================================
%  VAR HEADS/ NON-LISTS
% =================================================================
% =================================================================
% =================================================================

eval_20(Eq,RetType,_Dpth,_Slf,[X|T],Y):- T==[], \+ callable(X),!, do_expander(Eq,RetType,X,YY),Y=[YY].

%eval_20(Eq,RetType,_Dpth,Self,[X|T],Y):- T==[],  atom(X),
%   \+ is_user_defined_head_f(Self,X),
%   do_expander(Eq,RetType,X,YY),!,Y=[YY].

eval_20(Eq,RetType,Depth,Self,X,Y):- atom(Eq),  ( Eq \== ('=')) ,!,
   call(Eq,'=',RetType,Depth,Self,X,Y).

% metta_defn()
eval_20_disabled(Eq,RetType,Depth,Self,X,Y):- fail,
    compound(X), copy_term(X,XCopy,Goals),copy_term_nat(XCopy,XCopyNat),
    curried_arity(X,_,A), if_t(number(A),Len is A),
    SetArgs=did(n),
    quietly((if_trace(defn, (curried_arity(X,F,A),finfo(F,A,X))),
     findall(guarded_defn(XCopyNat,ParamTypes,FRetType,Body), (metta_defn(Self,Head,Body),copy_term(Head,HeadCopy),
        Head = XCopyNat,
        XCopyNat=@=XCopy,
        XCopyNat=XCopy,
        once((sub_term_safely(Op,HeadCopy),atom(Op),sub_var(Op,X))),
        maplist(call,Goals),
        %strace,
        get_operator_typedef(Self,Op,Len,ParamTypes,FRetType),
        trace,
        if_t(SetArgs==did(n), (nb_setarg(1,SetArgs,t),
                               do_eval_args_for(X,ParamTypes,NewXXCopy))),

        NewXXCopy= Head),XXB0L))),
        XXB0L \== [], !, %trace,
        catch(eval_defn_bodies_guarded(Eq,RetType,Depth,Self,X,Y,XXB0L),metta_NotReducible,X=Y).

do_eval_args_for(X,_ParamTypes,X):-!.

eval_20(Eq,RetType,Depth,Self,[X|T],Y):- T==[], is_list(X),!,
  eval_ne(Eq,RetType,Depth,Self,X,YY),Y=[YY].

eval_20_disabled(Eq,RetType,Depth,Self,[F,[Eval,V]|VI],VO):- fail, Eval == eval,!,
  ((eval_args(Eq,_FRype,Depth,Self,V,VV), V\=@=VV)*-> true; VV = V),
  eval_args(Eq,RetType,Depth,Self,[F,VV|VI],VO).

eval_20_disabled(Eq,RetType,Depth,Self,[[Eval,V]|VI],VO):- Eval == eval,!,
  ((eval_args(Eq,_FRype,Depth,Self,V,VV), V\=@=VV)*-> true; VV = V),
  eval_args(Eq,RetType,Depth,Self,[VV|VI],VO).

% DMILES @ TODO make sure this isnt an implicit curry
eval_20(Eq,_RetType,Depth,Self,[V|VI],VO):-  \+ callable(V), is_list(VI),!,
  maplist(eval_ret_5(Eq,Depth,Self),[V|VI],VOO),VO=VOO.


eval_20(Eq,RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval_args(Eq,RetType,Depth,Self,VI,VM),
  ( VM\==VI -> eval_args(Eq,RetType,Depth,Self,[V|VM],VVO) ;
    (eval_args(Eq,RetType,Depth,Self,V,VV), (V\==VV -> eval_args(Eq,RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_20(Eq,RetType,_Dpth,_Slf,X,Y):- \+ is_list(X),!,do_expander(Eq,RetType,X,Y).

% covered by \+ callable(V), is_list(VI)
%eval_20(Eq,_RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!, maplist(eval_ne(Eq,_ArgRetType,Depth,Self),VI,VO).


% eval_20 CAN NOW USE ATOMS (not jsut eval_10)
eval_20(_,_,_,_,['echo',Value],Value):- !.
%eval_20(=,Type,_,_,['coerce',Type,Value],Result):- !, coerce(Type,Value,Result).

eval_40(=,_RetType,_,_,['make-var'|Types],Var):- !, 'mc__1_0+_make-var'(Types,Var).

eval_40(=,_RetType,_,_,['bless-var',Var|Types],Var):- !, 'mc__1_1+_bless-var'(Var,Types,Var).

% =================================================================
% =================================================================
% =================================================================
%  LET*
% =================================================================
% =================================================================
% =================================================================

%eval_20(Eq,RetType,Depth2,Self,[Qw,X,Y],YO):- Qw == ('=='),!,
%  eval_args(X,XX),eval_args(Y,YY), !, as_tf(XX==YY,YO).

eval_20(_Eq,_RetType,_Depth,_Self,['is-mettalog'],'True'):- !.

eval_20(Eq,RetType,Depth,Self,['let*',Lets,Body],RetVal):-
    expand_let_star(Lets,Body,NewLet),!,
        eval_10(Eq,RetType,Depth,Self,NewLet,RetVal).



expand_let_star(Lets,Body,Body):- Lets==[],!.
expand_let_star([H|LetRest],Body,['let',V,E,NewBody]):-
    is_list(H), H = [V,E], !,
    expand_let_star(LetRest,Body,NewBody).

eval_20(Eq,RetType,Depth,Self,X,RetVal):-
    once(expand_eval(X,XX)),X\==XX,!,sanity_check_eval(expand_eval,XX),
        %fbug(expand_eval(X,XX)),
        eval_10(Eq,RetType,Depth,Self,XX,RetVal).

expand_eval(X,Y):- \+ is_list(X),!, X=Y.
expand_eval([H|A],[H|AA]):- \+ ground(H),!,maplist(expand_eval,A,AA).
expand_eval(['let*',Lets,Body],NewBody):- expand_let_star(Lets,Body,NewBody),!.
expand_eval([H|A],[H|AA]):- maplist(expand_eval,A,AA).

% =================================================================
% =================================================================
% =================================================================
%  EVAL LAZY
% =================================================================
% =================================================================
% =================================================================


is_progn(C):- var(C),!,fail.
is_progn('chain-body').
is_progn('progn').

eval_20(Eq,RetType,Depth,Self,[Comma,X  ],Res):- is_progn(Comma),!, eval_args(Eq,RetType,Depth,Self,X,Res).
%eval_20(Eq,RetType,Depth,Self,[Comma,X,Y],Res):- is_progn(Comma),!, eval_args(Eq,_,Depth,Self,X,_),
%  eval_args(Eq,RetType,Depth,Self,Y,Res).
eval_20(Eq,RetType,Depth,Self,[Comma,X|Y],Res):- is_progn(Comma),!, eval_args(Eq,_,Depth,Self,X,_),
  eval_args(Eq,RetType,Depth,Self,[Comma|Y],Res).

eval_20(Eq,RetType,Depth,Self,['chain',Atom,Var|Y],Res):-  !,  eval_args(Eq,_RetType,Depth,Self,Atom,R),
  Var = R, eval_args(Eq,RetType,Depth,Self,['chain-body'|Y],Res).

%eval_20(Eq,RetType,Depth,Self,['chain-body',X],Res):- !,eval_args(Eq,RetType,Depth,Self,X,Res).
%eval_20(Eq,RetType,Depth,Self,['chain-body',X|Y],Res):-  !, eval_args(Eq,RetType,Depth,Self,X,_), eval_args(Eq,RetType,Depth,Self,['chain-body'|Y],Res).

% simple version of Minimal MeTTa's `evalc` function
eval_20(Eq,RetType,Depth,Self,['evalc',Eval,Other],Result):-!,
    into_space(Depth,Self,Other,Space),
    eval_args_once(Eq,RetType,Depth,Space,Eval,Result).


% @TODO needs to only reduce one steps
eval_args_once(Eq,RetType,Depth,Space,Eval,Result):-
   eval_10(Eq,RetType,Depth,Space,Eval,Result)*->true;(Eval=Result).


eval_20(Eq,RetType,Depth,Self,['eval',X],Res):- !,
   eval_args(Eq,RetType,Depth,Self,X, Res).


eval_20(Eq,RetType,Depth,Self,['eval-for',Type,X],Res):- !,
    ignore(Type=RetType),
    eval_args(Eq,Type,Depth,Self,X, Res).

eval_20(Eq,RetType,Depth,Self,['eval-for',_Why,Type,X],Res):- !,
    ignore(Type=RetType),
    eval_args(Eq,Type,Depth,Self,X, Res).

% simple version of Minimal MeTTa's `metta` function (we dont call evalc/2 as it will be corrected to only reduce once)
eval_20(Eq,_Maybe_TODO_RetType,Depth,Self,['metta',Eval,RetType,Other],Result):-!,
    into_space(Depth,Self,Other,Space),
    eval_args(Eq,RetType,Depth,Space,Eval,Result),
    filter_type(Result,RetType).

filter_type(Result,RetType):-
  get_type(Result, ResultType),
  type_conform(ResultType, RetType),!.


/* Function takes list of atoms (first argument), variable (second argument) and filter predicate (third argument) and returns list with items which passed filter.
     E.g. (filter-atom (1 2 3 4) $v (eval (> $v 2))) will give (3 4)") */

eval_20(Eq,RetType,Depth,Self,['filter-atom',List,Var,Pred],Res):- !,
   call_filter_atom(Eq,RetType,Depth,Self,List,Var,Pred,Res).

call_filter_atom(_Eq,_RetType,_Depth,_Self,[],_Var,_Pred1,[]):-!.
call_filter_atom(Eq,RetType,Depth,Self,[E|List],Var,Pred,Out):-
    (( \+ \+ (E = Var, eval_args_true(Eq,RetType,Depth,Self,Pred))) -> Out = [E|Res] ; Out = Res),
    call_filter_atom(Eq,RetType,Depth,Self,List,Var,Pred,Res).

% "Function takes list of atoms (first argument), variable to be used inside (second variable) and an expression which will be evaluated for each atom in list (third argument). Expression should contain variable. So e.g. (map-atom (1 2 3 4) $v (eval (+ $v 1))) will give (2 3 4 5)"
eval_20(Eq,RetType,Depth,Self,['map-atom',List,V,Eval],Res):- !,
   call_map_atom(Eq,RetType,Depth,Self,List,V,Eval,Res).

call_map_atom(Eq,RetType,Depth,Self,[E|List],V,Eval,[CR|Res]):- !, faster_replace(V,E,Eval,CEval),
    eval_args(Eq,RetType,Depth,Self,CEval,CR),
    call_map_atom(Eq,RetType,Depth,Self,List,V,Eval,Res).
call_map_atom(_Eq,_RetType,_Depth,_Self,[],_V,_Pred,[]):-!.

% which is faster?
%faster_replace(B,E,Eval,CEval):- subst0011a(B,E,Eva2,CEval).
faster_replace(B,E,Eval,CEval):-  copy_term(B+Eval,CB+CEval), E = CB.
%faster_replace(A,Init,B,E,Eval,CEval):- copy_term(A+B+Eval,CA+CB+CEval), CA = Init, CB = E.


% Function takes list of values (first argument), initial value (second argument) and operation (fifth argument) and applies it consequently to the list of values, using init value as a start. It also takes two variables (third and fourth argument) to use them inside
eval_20(Eq,RetType,Depth,Self,['foldl-atom',List,Init,A,B,Eval],Res):- !,
   call_foldl_atom(Eq,RetType,Depth,Self,List,Init,A,B,Eval,Res).

% Last Cail
call_foldl_atom(Eq,RetType,Depth,Self,[E],Init,A,B,Eval,Res):- !, A = Init, B = E,
  eval_args(Eq,RetType,Depth,Self,Eval,Res).
% Recursive Cail
call_foldl_atom(Eq,RetType,Depth,Self,[E|List],Init,A,B,Eval,Res):- !, faster_replace(A,Init,B,E,Eval,CEval),
     eval_args(Eq,RetType,Depth,Self,CEval,CR),
     call_foldl_atom(Eq,RetType,Depth,Self,List,CR,A,B,Eval,Res).
% Empty List
call_foldl_atom(_Eq,_RetType,_Depth,_Self,[],Init,_A,_B,_Eval,Init):-!.

% which is faster?
faster_replace(A,Init,B,E,Eval,CEval):- subst0011a(A,Init,Eval,Eva2), subst0011a(B,E,Eva2,CEval).
%faster_replace(A,Init,B,E,Eval,CEval):- copy_term(A+B+Eval,CA+CB+CEval), CA = Init, CB = E.

% =================================================================
% =================================================================
% =================================================================
%  LET
% =================================================================
% =================================================================
% =================================================================

unified(X,Y):- X=@=Y,X=Y,!.
unified(X,Y):- X=Y,!.
unified(X,Y):- eval(X,XX),X\=@=XX,unified(Y,XX),!.
unified(X,Y):- eval(Y,YY),Y\=@=YY,unified(YY,X),!.

%eval_until_unify(_Eq,_RetType,_Dpth,_Slf,X,X):- !.


%eval_until_unify_self([h_e|_],Eq,RetType,Depth,Self,X,Y, Res):- var(Y),!,as_tf(X==Y, Res).
%eval_until_unify_self([h_e|_],Eq,RetType,Depth,Self,X,Y, Res):- var(X),!,as_tf(X==Y, Res).
%eval_until_unify_self(Flags,Eq,RetType,Depth,Self,X,Y,Res):- as_tf(eval_until_eq(Flags,Eq,RetType,Depth,Self,X,Y),Res).

eval_until_eq_tf(_Flags, _Eq, XType,YType,_Depth,_Self,X,Y,TF):- (var(X);var(Y)),!,ignore(XType=YType),as_tf(X==Y, TF).
eval_until_eq_tf(Flags, Eq, XType,YType,Depth,Self,X,Y,TF):-
  eval_until_eq(Flags, Eq, XType,YType,Depth,Self,X,Y,TF).


eval_until_eq(_Flags, Eq, XType, YType,_Dpth,_Slf,X,Y,TF):-  X==Y,!,check_returnval(Eq,XType,X),check_returnval(Eq,YType,Y),TF='True'.
eval_until_eq(_Flags,_Eq,_XType,_YType,_Dpth,_Slf,X,Y,TF):- notrace(as_tf_nowarn(X=:=Y,TF)),!.
eval_until_eq(_Flags,_Eq,_XType,_YType,_Dpth,_Slf,X,Y,TF):- notrace(as_tf_nowarn('#='(X,Y),TF)),!.
%eval_until_eq(Flags,Eq,XType,YType,_Dpth,_Slf,X,Y,TF):-  X\=@=Y,X=Y,!,check_returnval(Eq,XType,YType,Y,TF).
eval_until_eq(_Flags,Eq,XType,YType,_Depth,_Self,X,Y,TF):- var(X),var(Y),!,as_tf_traceable(X=Y,TF),check_returnval(Eq,XType,X),check_returnval(Eq,YType,Y),!.
%eval_until_eq(_Flags,Eq,XType,YType,_Dpth,_Slf,X,Y,TF):-  X=Y,!,check_returnval(Eq,XType,YType,Y,TF).
eval_until_eq(_Flags,Eq, XType, YType,Depth,Self,X,Y,TF):- var(Y),!,ignore(XType=YType),eval_argsteps(Eq,XType,Depth,Self,X,XX),as_tf_traceable(XX=Y,TF).
eval_until_eq(_Flags,Eq, XType, YType,Depth,Self,X,Y,TF):- var(X),!,ignore(XType=YType),eval_argsteps(Eq,YType,Depth,Self,Y,YY),as_tf_traceable(X=YY,TF).
%eval_until_eq(_Flags,Eq, XType,_YType,Depth,Self,X,Y,TF):- \+is_list(Y),!,eval_in_steps_some_change(Eq,XType,Depth,Self,X,XX),XX=Y.
%eval_until_eq(_Flags,Eq,_XType, YType,Depth,Self,X,Y,TF):- \+is_list(X),!,eval_in_steps_some_change(Eq,YType,Depth,Self,Y,YY),X=YY.
eval_until_eq(_Flags,Eq, XType,_YType,Depth,Self,X,Y,TF):- \+is_list(Y),!,eval_arg_maybe_steps(Eq,XType,Depth,Self,X,XX),as_tf_traceable(XX=Y,TF).
eval_until_eq(_Flags,Eq,_XType, YType,Depth,Self,X,Y,TF):- \+is_list(X),!,eval_arg_maybe_steps(Eq,YType,Depth,Self,Y,YY),as_tf_traceable(X=YY,TF).

eval_until_eq([Fn|Flags],Eq,XType,YType,Depth,Self,X,Y,TF):-
 if_or_else(eval_args_down(Eq,XType,YType,Depth,Self,X,Y,TF),
 if_or_else(eval_until_eq_l(Flags,Eq,XType,YType,Depth,Self,X,Y,TF),
 if_or_else(eval_args_slow_down(Flags,Eq,XType,YType,Depth,Self,X,Y,TF),
            TF=[Fn,X,Y]))).


%eval_arg_maybe_steps(Eq,YType,Depth,Self,Y,YY):- eval_in_steps_or_same(Eq,YType,Depth,Self,Y,YY).
eval_arg_maybe_steps(Eq,YType,Depth,Self,Y,YY):- eval_args(Eq,YType,Depth,Self,Y,YY).

eval_args_down(Eq,XType,YType,Depth,Self,X,Y,TF):-
  eval_args(Eq,XType,Depth,Self,X,XX),
  eval_args(Eq,YType,Depth,Self,Y,YY),
  as_tf_unify(XX,YY,TF).

as_tf_unify(XX,YY,TF):- as_tf_traceable(XX=YY,TF).

eval_args_slow_down(_Flags,Eq,XType,YType,Depth,Self,X,Y,TF):-
  as_tf(((eval_in_steps_some_change(Eq,XType,Depth,Self,X,XX),eval_in_steps_some_change(Eq,YType,Depth,Self,Y,YY),
  XX=YY)),TF).

%eval_until_eq(Flags,Eq,XType,YType,Depth,Self,X,Y):- eval_1change(Eq,XType,YType,Depth,Self,X,XX),eval_until_eq(Flags,Eq,XType,YType,Depth,Self,Y,XX),!.
%eval_until_eq(Flags,Eq,XType,YType,Depth,Self,X,Y):- eval_in_steps_some_change(Eq,XType,YType,Depth,Self,X,XX),eval_until_eq(Flags,Eq,XType,YType,Depth,Self,Y,XX),!.


eval_until_eq_l(_Flags,_Eq,_XType,_YType,_Dpth,_Slf,X,Y,_):- (X==[];Y==[]),!,X=Y.
eval_until_eq_l(_Flags,_Eq,_XType,_YType,_Dpth,_Slf,X,Y,_):- length(X,Len), \+ length(Y,Len),!,fail.
eval_until_eq_l(Flags,Eq,XType,YType,Depth,Self,[FX|X],[FY|Y],TF):-
  if_or_else(eval_until_eq_l1(Flags,Eq,XType,YType,Depth,Self,[FX|X],[FY|Y],TF),
             eval_until_eq_l2(Flags,Eq,XType,YType,Depth,Self,[FX|X],[FY|Y],TF)).

eval_until_eq_l1(Flags,Eq,XType,YType,Depth,Self,[FX|X],[FY|Y],TF):-
  (atom(FX);atom(FY)),FX=FY,!,
  length(X,LenX),length(Y,LenY),
  make_todo_args(1,LenX,XTodo),
  make_todo_args(1,LenY,YTodo),!,
  eval_until_eq_l_args(XTodo,YTodo,FX,FY,LenX,LenY,Flags,Eq,XType,YType,_XParamTypes,_YParamTypes,Depth,Self,X,Y,TF).

eval_until_eq_l2(Flags,Eq,XType,YType,Depth,Self,X,Y,TF):-
  length(X,LenX),length(Y,LenY),
  make_todo_args(1,LenX,XTodo),
  make_todo_args(1,LenY,YTodo),!,
  maplist(get_vtype(Depth,Self),X,XParamTypes),
  maplist(get_vtype(Depth,Self),Y,YParamTypes),
  eval_until_eq_l_args(XTodo,YTodo,_FX,_FY,LenX,LenY,Flags,Eq,XType,YType,XParamTypes,YParamTypes,Depth,Self,X,Y,TF).

get_vtype(Depth,Self,Obj,VType):- findall(Type, get_type_each(Depth, Self, Obj, Type), List), list_to_set(List,Set), narrow_to_vtype(Set,VType).
narrow_to_vtype([],'%Undefined%'):-!.
narrow_to_vtype([X],X):-!.
narrow_to_vtype(List,X):- predsort(freeist,List,[X|_]).

make_todo_args(N,LenX,[]):- LenX<N,!.
make_todo_args(N,LenX,[N|XTodo]):- succ(N,N2),
  make_todo_args(N2,LenX,XTodo).

eval_until_eq_l_args(XTodo,YTodo,FX,FY,LenX,LenY,Flags,Eq,XType,YType,XParamTypes,YParamTypes,Depth,Self,X,Y,TF):-
  eval_until_eq_l_args_solve(XTodo,YTodo,FX,FY,LenX,LenY,Flags,Eq,XType,YType,XParamTypes,YParamTypes,Depth,Self,X,Y,TF).

eval_until_eq_l_args_solve(XTodo,YTodo,FX,FY,LenX,LenY,Flags,Eq,XRetType,YRetType,XParamTypes,YParamTypes,Depth,Self,X,Y,TF):-
   (XTodo==[];YTodo==[]),!,as_tf(XTodo==YTodo,TF),
   nop(success(eval_until_eq_l_args_solve(XTodo,YTodo,FX,FY,LenX,LenY,Flags,Eq,XRetType,YRetType,XParamTypes,YParamTypes,Depth,Self,X,Y))).

eval_until_eq_l_args_solve(XTodo,YTodo,FX,FY,LenX,LenY,Flags,Eq,XRetType,YRetType,XParamTypes,YParamTypes,Depth,Self,X,Y,TF):-
   select(NX,XTodo,XTodoNext),nth1(NX,XParamTypes,XType),nth1(NX,X,EX),
   select(NY,YTodo,YTodoNext),nth1(NY,YParamTypes,YType),nth1(NY,Y,EY),
   maybe_get_operator_typedef(Self,FX,LenX,XParamTypes,XType),
   maybe_get_operator_typedef(Self,FY,LenY,YParamTypes,YType),
   eval_until_eq_soon(Flags,Eq,XType,YType,Depth,Self,EX,EY,SubTF), \+ \+ is_True(SubTF),
   eval_until_eq_l_args_solve(XTodoNext,YTodoNext,FX,FY,LenX,LenY,Flags,Eq,XRetType,YRetType,XParamTypes,YParamTypes,Depth,Self,X,Y,TF).

maybe_get_operator_typedef(Self,FX,LenX,XParamTypes,XType):- ignore((nonvar(FX),get_operator_typedef(Self,FX,LenX,XParamTypes,XType))).

eval_until_eq_soon(Flags,Eq,XType,YType,Depth,Self,EX,EY,TF):- eval_until_eq(Flags,Eq,XType,YType,Depth,Self,EX,EY,TF).
/*
eval_until_eq_soon(_Flags,Eq,XType,YType,Depth,Self,EX,EY):-
   eval_in_steps_some_change(Eq,XType,Depth,Self,EX,NewX),
   eval_in_steps_some_change(Eq,YType,Depth,Self,EY,NewY),
   NewX=NewY.
*/

eval_1change(Eq,XType,Depth,Self,EX,EXX):- eval_10(Eq,XType,Depth,Self,EX,EXX),  EX \=@= EXX.
eval_complete_change(Eq,XType,YType,Depth,Self,EX,EXX):- eval_args(Eq,XType,YType,Depth,Self,EX,EXX),  EX \=@= EXX.

eval_in_steps_some_change(_Eq,_XType,_Dpth,_Slf,EX,Y):- \+ is_list(EX),!,Y=EX.
%eval_in_steps_some_change(_Eq,_XType,_Dpth,_Slf,EX,_):- \+ is_list(EX),!,fail.
eval_in_steps_some_change(Eq,XType,Depth,Self,EX,EXXO):-
   eval_1change(Eq,XType,Depth,Self,EX,EXX),!,
   (eval_in_steps_some_change(Eq,XType,Depth,Self,EXX,EXXO);EXXO=EXX).
eval_in_steps_some_change(Eq,XType,Depth,Self,X,Y):-
  append(L,[EX|R],X),is_list(EX),
    eval_in_steps_some_change(Eq,XType,Depth,Self,EX,EXX), EX\=@=EXX,
  append(L,[EXX|R],XX),
  eval_in_steps_or_same(Eq,XType,Depth,Self,XX,Y).

eval_in_steps_or_same(Eq,XType,Depth,Self,X,Y):-eval_in_steps_some_change(Eq,XType,Depth,Self,X,Y).
eval_in_steps_or_same(Eq,XType,_Dpth,_Slf,X,Y):- X=Y,check_returnval(Eq,XType,Y).

  % (fail,make_nop(RetType,[],Template))).


possible_type(_Self,_Var,_RetTypeV).

/*
  eval_20(Eq,RetType,Depth,Self,['let',E,V,Body],OO):- var(V), nonvar(E), !,
      %(var(V)->true;trace),
      trace, possible_type(Self,V,RetTypeV),
      eval_args(Eq,RetTypeV,Depth,Self,E,ER), V=ER,
      eval_args(Eq,RetType,Depth,Self,Body,OO).
*/
eval_20(Eq,RetType,Depth,Self,['let',V,E,Body],OO):- !, % var(V), nonvar(E), !,
        %(var(V)->true;trace),
        possible_type(Self,V,RetTypeV),
       ( let_Empty
       ->eval('=',RetTypeV,Depth,Self,E,ER)
        ; eval_ne('=',RetTypeV,Depth,Self,E,ER)),
        V=ER,
        eval_args(Eq,RetType,Depth,Self,Body,OO).

let_Empty:- !.

/*

eval_20(Eq,RetType,Depth,Self,['let',V,E,Body],OO):- nonvar(V),nonvar(E),!,
    possible_type(Self,V,RetTypeV),
    possible_type(Self,E,RetTypeV),
    ((V=E,fail) -> true;
    (eval_args(Eq,RetTypeV,Depth,Self,E,ER),
    (V=ER -> true;
    (eval_args(Eq,RetTypeV,Depth,Self,V,VR),
    (E=VR -> true; ER=VR))))),
    eval_args(Eq,RetType,Depth,Self,Body,OO).


eval_20(Eq,RetType,Depth,Self,['let',V,E,Body],OO):- var(V), nonvar(E), !,
        %(var(V)->true;trace),
        possible_type(Self,V,RetTypeV),
        eval_args(Eq,RetTypeV,Depth,Self,E,ER), V=ER,
        eval_args(Eq,RetType,Depth,Self,Body,OO).

eval_20(Eq,RetType,Depth,Self,['let',V,E,Body],OO):- var(V), var(E), !,
      V=E, eval_args(Eq,RetType,Depth,Self,Body,OO).


%eval_20(Eq,RetType,Depth,Self,['let',V,E,Body],BodyO):- !,eval_args(Eq,RetType,Depth,Self,E,V),eval_args(Eq,RetType,Depth,Self,Body,BodyO).
eval_20(Eq,RetType,Depth,Self,['let*',[],Body],RetVal):- !, eval_args(Eq,RetType,Depth,Self,Body,RetVal).
%eval_20(Eq,RetType,Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
%   eval_until_unify_self(Flags,Eq,_RetTypeV,Depth,Self,Val,Var),
%   eval_20(Eq,RetType,Depth,Self,['let*',LetRest,Body],RetVal).
eval_20(Eq,RetType,Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    eval_20(Eq,RetType,Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).
*/
% =================================================================
% =================================================================
% =================================================================
%  TRACE/PRINT
% =================================================================
% =================================================================
% =================================================================

gen_eval_20_stubs:-
  shell(clear),
  make,call(gen_eval_20_stubs2).
gen_eval_20_stubs2:-
  Clause = (impls([F|Args],Res,ParamTypes,RetType):- Body),

  forall(gen_eval_20_stubs([F|Args],Res,ParamTypes,RetType,Body),
     ignore((
     numbervars(Clause,0,_),
     nonvar(F),atom(F),
     ast_to_prolog_aux(no_caller,fn_impl(F,Args,Res),Head),
     ast_to_prolog_aux(Head,Body,Body1),
     print_tree_nl(Head:-Body1)))).


is_like_eval_20(E20):- atom(E20),atom_concat(eval,_,E20),
        %(E20 = eval_args;E20 = eval_20),
        \+ atom_concat(find,_,E20),
        \+ atom_concat(_,e,E20).

gen_eval_20_stubs([F|Args],Res,ParamTypes,RetType,Body):-
    predicate_property(eval_20(Eq,RetType,Depth,Self,[F|Args],Res),file(File)),
    predicate_property(Head,file(File)),
    Head=..[E20,Eq,RetType,Depth,Self,[F|Args],Res],
    is_like_eval_20(E20),
    clause(Head, Body),
    ignore(once((sub_term_safely(FF==Sym, Body), atom(Sym), FF == F,F=Sym))),
    %min_max_args(Args,Startl,Ends),
    (is_list(Args)->true;between(1,5,Len)),
    once(len_or_unbound(Args,Len)),
    nonvar(F),atom(F),
    ignore(Depth=666),
   % ignore(Eq= '='),
    ignore(Self= '&self'),
    once(get_operator_typedef(Self,F,Len,ParamTypes,RetType)).



eval_20(Eq,RetType,_Dpth,_Slf,['repl!'],Y):- !,  repl,check_returnval(Eq,RetType,Y).
%eval_20(Eq,RetType,Depth,Self,['enforce',Cond],Res):- !, enforce_true(Eq,RetType,Depth,Self,Cond,Res).
eval_20(Eq,RetType,Depth,Self,['!',Cond],Res):- !, call(eval_args(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(Eq,RetType,Depth,Self,['rtrace!',Cond],Res):- !, rtrace(eval_args(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(Eq,RetType,Depth,Self,['no-rtrace!',Cond],Res):- !, quietly(eval_args(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(Eq,RetType,Depth,Self,['trace!',A,B],C):- !, % writeln(trace(A)),
     stream_property(S,file_no(2)),!,
     eval_args(Eq,RetType,Depth,Self,B,C),
     ignore((eval_args(Eq,_RetType,Depth,Self,A,AA),
     with_output_to(S,(format('~N'), write_src(AA),format('~N'))))).
eval_20(Eq,RetType,Depth,Self,['trace',Cond],Res):- !, with_debug(eval_args,eval_args(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(Eq,RetType,Depth,Self,['profile!',Cond],Res):- !, time_eval(profile(Cond),profile(eval_args(Eq,RetType,Depth,Self,Cond,Res))).
eval_20(Eq,RetType,Depth,Self,['cpu-time',Cond],Res):- !, ctime_eval(eval_args(Cond),eval_args(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(Eq,RetType,Depth,Self,['wall-time',Cond],Res):- !, wtime_eval(eval_args(Cond),eval_args(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(Eq,RetType,Depth,Self,['time!',Cond],['Time',Seconds,Res]):- !, wtimed_call(eval_args(Eq,RetType,Depth,Self,Cond,Res), Seconds).
eval_20(Eq,RetType,Depth,Self,['print',Cond],Res):- !, eval_args(Eq,RetType,Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(print! $1)
eval_20(Eq,RetType,Depth,Self,['princ!'|Cond],Res):- !,
  maplist(eval_args(Eq,RetType,Depth,Self),Cond,Out),
  maplist(princ_impl,Out),
  make_nop(RetType,[],Res),check_returnval(Eq,RetType,Res).
% !(println! $1)
eval_20(Eq,RetType,Depth,Self,['println!'|Cond],Res):- !,
  maplist(eval_args(Eq,RetType,Depth,Self),Cond,Out),
  maplist(println_impl,Out),
  make_nop(RetType,[],Res),check_returnval(Eq,RetType,Res).

println_impl(X):- ttyflush,user_io((format("~N~@~N",[write_sln(X)]))),!,flush_output,ttyflush.
%println_impl(X):- user_io((ansi_format(fg('#c7ea46'),"~N~@~N",[write_sln(X)]))),flush_output.
%println_impl(X):- ((ansi_format(fg('#c7ea46'),"~N~@~N",[write_sln(X)]))),flush_output.

princ_impl(X):- format("~@",[write_sln(X)]),!,flush_output.

write_sln(X):- string(X), !, write(X),flush_output.
write_sln(X):- write_src_woi(X),flush_output.

with_output_to_str( Sxx , Goal ):-
  wots( Sxx , Goal ).

% =================================================================
% =================================================================
% =================================================================
%  UNIT TESTING/assert<STAR>
% =================================================================
% =================================================================
% =================================================================


eval_20(Eq,RetType,Depth,Self,['assertTrue', X],TF):- !,
  eval_20(Eq,RetType,Depth,Self,['assertEqual',X,'True'],TF).
eval_20(Eq,RetType,Depth,Self,['assertFalse',X],TF):- !,
 eval_20(Eq,RetType,Depth,Self,['assertEqual',X,'False'],TF).

eval_20(Eq,_RetType,Depth,Self,['assertEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertEqual',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         findall_eval(Eq,_BRetType,Depth,Self,Y,YY)),
         equal_enough_for_test_renumbered_l(strict_equals_allow_vn,XX,YY), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertNotEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertNotEqual',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         findall_eval(Eq,_BRetType,Depth,Self,Y,YY)),
         ( \+ equal_enough_for_test_renumbered_l(strict_equals_allow_vn,XX,YY)), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertEqualToResult',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertEqualToResult',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
                                    as_prolog_x(Depth,Self,Y,YY)),
         equal_enough_for_test_renumbered_l(strict_equals_allow_vn,XX,YY), RetVal).


eval_20(Eq,_RetType,Depth,Self,['assertAlphaEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertAlphaEqual',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         findall_eval(Eq,_BRetType,Depth,Self,Y,YY)),
         equal_enough_for_test_renumbered_l(alpha_equ,XX,YY), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertNotAlphaEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertNotAlphaEqual',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         findall_eval(Eq,_BRetType,Depth,Self,Y,YY)),
         equal_enough_for_test_renumbered_l(not_alpha_equ,XX,YY), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertAlphaEqualToResult',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertAlphaEqualToResult',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
                                    as_prolog_x(Depth,Self,Y,YY)),
         equal_enough_for_test_renumbered_l(alpha_equ,XX,YY), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertNotAlphaEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertNotAlphaEqualToResult',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         findall_eval(Eq,_BRetType,Depth,Self,Y,YY)),
         equal_enough_for_test_renumbered_l(not_alpha_equ,XX,YY), RetVal).

loonit_assert_source_tf_empty(Src,XX,YY,Goal,Check,RetVal):-
    loonit_assert_source_tf(Src,Goal,Check,TF),
    tf_to_empty(TF,['Error',Src,['\nGot: ',XX,'\nExpected: ',YY]],RetVal).

tf_to_empty(TF,Else,RetVal):-
  (TF=='True'->as_nop(RetVal);  subst001(Else,'Empty','Empt𝘺',RetVal)).

val_sort(Y,YY):- is_list(Y),!,sort(Y,YY).
val_sort(Y,[Y]).

'broken-unique-atom'(I,O):- unique_by_unify(I,[],O).
unique_by_unify(L,M,O):- \+ iz_conz(L),!,M=O.
unique_by_unify([H|T],M,O):- \+ member(H,M),!,unique_by_unify(T,[H|M],O).
unique_by_unify(_,M,O):- reverse(M,O).


loonit_assert_source_tf(_Src,Goal,Check,TF):- fail, \+ is_testing,!,
    reset_eval_num,
    tst_call_limited(Goal),
    as_tf(Check,TF),!.

loonit_assert_source_tf(Src,Goal,Check,TF):-
    copy_term(Goal,OrigGoal),
    reset_eval_num,
   call_cleanup(loonit_asserts(Src, time_eval('\n; EVAL TEST\n;',Goal), Check),
   (as_tf(notrace(Check),TF),!,
  ignore((
          once((TF='True', trace_on_pass);(TF='False', trace_on_fail)),
     with_debug((e),time_eval('Trace',OrigGoal)))))).

sort_result(Res,Res):- \+ compound(Res),!.
sort_result([And|Res1],Res):- is_and(And),!,sort_result(Res1,Res).
sort_result([T,And|Res1],Res):- is_and(And),!,sort_result([T|Res1],Res).
sort_result([H|T],[HH|TT]):- !, sort_result(H,HH),sort_result(T,TT).
sort_result(Res,Res).


unify_case(A,B):- A=@=B,!,A=B.
unify_case(A,B):- A=B,!.

unify_enough(L,L).
unify_enough(L,C):- into_list_args(L,LL),into_list_args(C,CC),unify_lists(CC,LL).

%unify_lists(C,L):- \+ compound(C),!,L=C.
%unify_lists(L,C):- \+ compound(C),!,L=C.
unify_lists(L,L):-!.
unify_lists([C|CC],[L|LL]):- unify_enough(L,C),!,unify_lists(CC,LL).

is_blank(X):- var(X),!,fail.
is_blank([]).
is_blank(_):- \+ is_flag(testing_fudge_parens),!,fail.
is_blank(E):- is_empty(E),!.
is_blank([X]):-!,is_blank(X).
has_let_star(Y):- sub_var_safely('let*',Y).

sort_univ(L,S):- cl_list_to_set(L,E),sort(E,S).
% !(pragma! unit-tests tollerant) ; tollerant or exact
is_tollerant:- \+ option_value('unit-tests','exact').

strict_equals_allow_vn(X,Y):- X==Y,!.
strict_equals_allow_vn(X,Y):- attvar(X),attvar(Y),get_attr(X,vn,XX),get_attr(Y,vn,YY),!,XX==YY.
strict_equals_allow_vn(X,Y):- attvar(X),var(Y),get_attr(X,vn,XX),\+ get_attr(Y,vn,_),!,XX=Y.
strict_equals_allow_vn(Y,X):- attvar(X),var(Y),get_attr(X,vn,XX),\+ get_attr(Y,vn,_),!,XX=Y.
strict_equals_allow_vn(X,Y):- X=@=Y,!.

%equal_enough_for_test_renumbered_l(P2,X,Y):- call(P2,X,Y), !.
equal_enough_for_test_renumbered_l(_P2,X,Y):- is_blank(X),is_blank(Y),!.
equal_enough_for_test_renumbered_l(P2,X,Y):-  must_be(proper_list,X), must_be(proper_list,Y),
    sort(X,X0),sort(Y,Y0),(X\==X0;Y\==Y0),!,
    equal_enough_for_test_renumbered_l(P2,X0,Y0).
equal_enough_for_test_renumbered_l(P2,[X0],[Y0]):- is_list(X0),is_list(Y0),!,equal_enough_for_test_renumbered_l(P2,X0,Y0).
equal_enough_for_test_renumbered_l(P2,X0,Y0):- maplist(equal_enough_for_test_renumbered(P2),X0,Y0).


equal_enough_for_test_l(P2,X,Y):-            must_be(proper_list,X), must_be(proper_list,Y), sort(X,X0),sort(Y,Y0),
    maplist(equal_enough_for_test(P2),X0,Y0).

equal_enough_for_test_renumbered(P2,X0,Y0):- equal_renumbered(X0,Y0,XX,YY), equal_enough_for_test(P2, XX,YY).

equal_enough_for_test(P2,X,Y):- equal_enough(P2,X,Y),!.

/*

equal_enough_for_test(_2,X,Y):- is_list(X),is_list(Y),X=[ErrorX|_],Y=[ErrorY|_],ErrorX=='Error',
      ErrorY == ErrorX,!.
equal_enough_for_test(_P2,X,Y):- is_blank(X),!,is_blank(Y).
equal_enough_for_test(_P2,X,Y):- has_let_star(Y),!,\+ is_blank(X).
equal_enough_for_test(P2,X,Y):- is_list(X),is_list(Y),
   Y=[YY],X=[XX],!,equal_enough_for_test(P2,XX,YY).
   %length(XX,XL),length(YY,YL),

%equal_enough_for_test(P2,X,Y):-!,fail.

equal_enough_for_test(P2,X,Y):- must_det_ll((subst_vars(X,XX),subst_vars(Y,YY))),!,
  equal_enough_for_test2(P2,XX,YY),!.
equal_enough_for_test2(P2,X,Y):- equal_enough(P2,X,Y).

equal_enough(P2,R,V):- is_list(R),is_list(V),sort_univ(R,RR),sort_univ(V,VV),!,equal_enouf(P2,RR,VV),!.
*/


equal_enough(P2,R,V):- copy_term(R+V,RR+VV),equal_enouf(P2,R,V),!,ignore(R=@=RR),ignore(V=@=VV). % has not altered the returned term


equal_enouf(_,_,V):- V=='...',!.
equal_enouf(P2,R,V):- call(P2,R,V), !.
equal_enouf(_2,R,V):- (var(R);var(V)),!,fail.
equal_enouf(P2,R,V):- is_ftVar(R), is_ftVar(V), call(P2,R,V), !.
equal_enouf(_,X,Y):- is_blank(X),!,is_blank(Y).
equal_enouf(_TODO,R,V):- py_is_py(R),py_is_py(V),py_pp_str(R,RR),py_pp_str(V,VV),!,RR=VV.
equal_enouf(_,X,Y):- symbol(X),symbol(Y),atom_concat('space_',_,X),atom_concat('Grounding',_,Y),!.
equal_enouf(_,X,Y):- symbol(X),symbol(Y),atom_concat('&',_,X),atom_concat('Grounding',_,Y).
equal_enouf(_2,R,V):- number(R),number(V),!, RV is abs(R-V), RV < 0.03 .
equal_enouf(P2,C,L):- \+ compound(C),!,call(P2,L,C).
equal_enouf(P2,L,C):- \+ compound(C),!,call(P2,L,C).
equal_enouf(P2,L,C):- into_list_args(L,LL),into_list_args(C,CC),!,equal_enouf_la(P2,LL,CC).

equal_enouf_la(P2,[S1,V1|_],[S2,V2|_]):- S1 == 'State', !, S2 == 'State',!, equal_enouf(P2,V1,V2).
equal_enouf_la(_2,[ErrorX|_],[ErrorY|_]):- ErrorX=='Error', !, ErrorY == ErrorX,!.
equal_enouf_la(P2,C,L):- equal_enouf_l(P2,C,L).

equal_enouf_l(P2,C,L):- \+ compound(C),!,call(P2,L,C).
equal_enouf_l(P2,L,C):- \+ compound(C),!,call(P2,L,C).
equal_enouf_l(P2,[C|CC],[L|LL]):- !, equal_enouf(P2,L,C),!,equal_enouf_l(P2,CC,LL).

/*
equal_enouf(P2,L,C):- is_tollerant, is_list(L),is_list(C),
     maybe_remove_nils(C,CC),equal_enouf(P2,L,CC).

equal_enouf(P2,R,V):- (var(R);var(V)),!, call(P2,R,V).
equal_enouf(_2,R,V):- atom(R),!,atom(V), has_unicode(R),has_unicode(V).
equal_enouf(P2,R,V):- (\+ compound(R) ; \+ compound(V)),!, call(P2,R,V).
equal_enouf(P2,L,C):- into_list_args(L,LL),into_list_args(C,CC),!,equal_enouf_l(P2,CC,LL).

*/
maybe_remove_nils(I,O):- always_remove_nils(I,O),!,I\=@=O.
always_remove_nils(I,O):- \+ compound(I),!,I=O.
always_remove_nils([H|T], TT):- H==[],!, always_remove_nils(T,TT).
always_remove_nils([H|T], TT):- H=='Empty',!, always_remove_nils(T,TT).
always_remove_nils([H|T],[H|TT]):- always_remove_nils(T,TT).

has_unicode(A):- atom_codes(A,Cs),member(N,Cs),N>127,!.

set_last_error(_).

'mi__1_2_=alpha'(X0,Y0,TF):- as_tf(equal_enough_for_test_renumbered(alpha_equ,X0,Y0),TF).
'mi__1_2_=alpha-unify'(X0,Y0,TF):- as_tf(equal_enough_for_test_renumbered(alpha_equ,X0,Y0),TF),(TF=='True',blend_vars(X0,Y0)).
/*
    % ============================
    %  Theoretical Equivalence & Unification (Check Possibility Only)
    % ============================

    % =alpha: Checks if two terms are structurally equivalent, allowing variable renaming.
    'mi__1_2_=alpha'(X0, Y0, TF) :-
        as_tf('=@='(X0, Y0), TF).

*/
% =will: Checks if unification *could* succeed without actually binding variables.
'mi__1_2_=will'(X0, Y0, TF) :-
    as_tf('=will'(X0,Y0), TF).

'=u='(X0,Y0) :- (X0 = Y0).
'=will'(X0,Y0) :- \+ \+ (X0 = Y0).
% alpha equivelant
'=alpha'(X0,Y0) :- (X0 =@= Y0).
% like =alpha, however it actualyl also unifies (if they were alpha)
'=alpha-unify'(X0,Y0) :- X0 =@= Y0, X0 = Y0.


alpha_equ(X,Y):- X=@=Y->if_tracemsg(unknown,alpha_equ(X,Y));if_tracemsg(unknown,not_alpha_equ(X,Y)).
not_alpha_equ(X,Y):- X\=@=Y->if_tracemsg(unknown,not_alpha_equ(X,Y));if_tracemsg(unknown,alpha_equ(X,Y)).

remove_attr(Attr,Var):- del_attr(Var, Attr).
with_attr(Attr,Var,Value):- get_attr(Var, Attr, Value).

% ============================
%  Unification Predicates (Perform Binding)
% ============================

% =u=: Actually unifies two terms, modifying variable bindings.
'mi__1_2_=u='(X0, Y0, TF) :-
    as_tf(X0 = Y0, TF).

% =alpha-unify: Unifies two terms while considering alpha-equivalence.
%'mi__1_2_=alpha-unify'(X0, Y0, TF) :-
%    '=@='(X0, Y0),
%    as_tf(X0 = Y0, TF).

% ============================
%  Strict Identity & Reference Predicates
% ============================

% =identical: Checks if two terms are *strictly* identical, including variables & bindings.
'mi__1_2_=identical'(X0, Y0, TF) :-
    as_tf(X0 == Y0, TF).

% =references: Checks if two terms reference the *exact same* memory object.
'mi__1_2_=references'(X0, Y0, TF) :-
    as_tf(same_terms(X0, Y0), TF).  % SWI-Prolog only


%make_some( $foo,  (made $some $foo))
%make_some( $bar,  (made $some $bar))

%! merge_same_named_vars(+XVars, +YVars, -SameExactVars, -SameNames, -LeftOverXVars, -LeftOverYVars) is det
% XVars and YVars are lists of attributed variables.
% SameExactVars contains variables that are identical in both lists.
% SameNames contains pairs of variables (from XVars and YVars) whose attributes match.
% LeftOverXVars and LeftOverYVars contain remaining variables from XVars and YVars, respectively.

merge_same_named_vars([], VYs, [], [], [], VYs):- !.
merge_same_named_vars(VXs, [], [], [], VXs, []):- !.

merge_same_named_vars([VX|VXs], VYs, [VX|MoreOfTheSames], SameNames, LeftOverXVars, LeftOverYVars) :-
    select(VY, VYs, RVYs), VX == VY, !,
    merge_same_named_vars(VXs, RVYs, MoreOfTheSames, SameNames, LeftOverXVars, LeftOverYVars).

merge_same_named_vars([VX|VXs], VYs, SameExact, Unmerged, LeftOverXVars, LeftOverYVars) :-
    get_attr(VX, vn, XVal),
    select(VY, VYs, RVYs), get_attr(VY, vn, YVal), XVal == YVal, !,
    get_attrs_or_nil(VX, Attr1),
    get_attrs_or_nil(VY, Attr2),
    push_attributes(VX, Attr2),
    push_attributes(VY, Attr1),
    merge_same_named_vars(VXs, RVYs, SameExactVars, MorePairs, LeftOverXVars, LeftOverYVars),
    ((VX=VY)
     ->  (SameExact=[VX|SameExactVars], Unmerged = MorePairs)
     ; (SameExactVars = SameExact, [(VX, VY)|MorePairs]=Unmerged)).

merge_same_named_vars([VX|VXs], VYs, SameExactVars, SameNames, [VX|LeftOverXVars], LeftOverYVars) :-
    merge_same_named_vars(VXs, VYs, SameExactVars, SameNames, LeftOverXVars, LeftOverYVars).


blend_vars(VX,VY):- VX=VY,!.
%blend_vars(VX,VY):- remerge_attrs(VX-VY),!.
blend_vars(VX,VY):-
    get_attrs_or_nil(VX, Attr1),
    get_attrs_or_nil(VY, Attr2),
    push_attributes(VX, Attr2),
    push_attributes(VY, Attr1),
    (VX=VY -> true ; remerge_attrs(VX-VY)),!.

remerge_attrs(VX-VY):- get_attrs_or_nil(VY, FinalAttr), put_attrs(VX,FinalAttr).

get_attrs_or_nil(Var, Attributes) :-
    ( get_attrs(Var, Attributes) -> true ; Attributes = [] ).


%% push_attributes(+AttributesToAdd, +Var) is det
%  Adds attributes to the attributed variable Var.
%  AttributesToAdd is a term of the form att(Module, Value, MoreAttributes).
%  If the module and value already exist, they are skipped.
push_attributes(Var,Attrs):-
  push_attributes_onto_var(Attrs,Var), ! .

push_attributes_onto_var(Attrs,_):- var(Attrs),!,maybe_trace(unknown),break. % var(Var)
%push_attributes_onto_var(Attrs,Var):- var(Attrs),var(Var),!.
push_attributes_onto_var(_,Var):- nonvar(Var),!,maybe_trace(unknown),break. % var(Var)
push_attributes_onto_var([], _Var) :- !.
push_attributes_onto_var(att(Mod, Val, Rest), Var) :-
    get_attr(Var, Mod, Existing), !,
    (Existing == Val -> true ; (get_attrs(Var,Att3Before), put_attrs(Var,att(Mod, Val, Att3Before)))),
    push_attributes_onto_var(Rest, Var).
push_attributes_onto_var(att(Mod, Val, Rest), Var) :-
    put_attr(Var, Mod, Val),
    push_attributes_onto_var(Rest, Var).

equal_renumbered(X0,Y0,XX,YY):-
   copy_term(X0+Y0,X+Y),
   term_variables(X,VXs), term_variables(Y,VYs),
   merge_same_named_vars(VXs,VYs,Same,Merges,_LOXVs,_LOYVs),
   max_var_number(X+Y,0,N),succ(N,N2),
   maplist(remerge_attrs,Merges),
   maplist(remove_attr(vn),VXs), maplist(remove_attr(vn),VYs),
   numbervars(Same,N2,_,[attvar(skip)]),
   %numbervars(_Merges,N3,_,[attvar(skip)]),
   renumber_vars_wo_confict_tu(X,XX),
   renumber_vars_wo_confict_tu(Y,YY),!.
   %if_tracemsg(unknown,equal_enough_for_test(P2,XX,YY)),


renumber_vars_wo_confict_tu(X,XXX):-
   copy_term(X,XX),
   max_var_number(XX,0,N),
   succ(N,N2),
   numbervars(XX,N2,_,[attvar(skip)]), % TODO
   unnumbervars_wco123(XX,XXX).

unnumbervars_wco123(X,XXX):- compound(X),
   sub_term_safely(E, X), compound(E), E = '$VAR'(_),!,
   subst001(X,E,_,XX),unnumbervars_wco123(XX,XXX).
unnumbervars_wco123(X,X).


% =================================================================
% =================================================================
% =================================================================
%  SCOPING
% =================================================================
% =================================================================
% =================================================================

eval_20(_Eq, _RetType, _Depth, _Self, ['sealed', InputVarList, Expr], Result) :- !,
    omit_atoms(InputVarList,OutputVarList),
    check_replace_with_local_var(OutputVarList, Expr, Result).

% omit_atoms(+input variables, -variables less atoms)
% If there are already bound values passed to sealed, no need for replacement
omit_atoms([], []).
omit_atoms([Head|Tail], Result) :-
    atomic(Head),
    !,
    omit_atoms(Tail, Result).
omit_atoms([Head|Tail], [Head|Result]) :-
    omit_atoms(Tail, Result).

% check_replace_with_local_var(+Sealed-Variables, +Expression, -NewExpression)
% Boundary case -- no remaining variables to process, just return expression.
check_replace_with_local_var([], Expr, Result) :- !, Result = Expr.

% General case -- replace sealed variable with a new variable
check_replace_with_local_var([VarHead|VarTail], Expr, Result) :-
    % '_' gives us a prolog variable
    replace_by_value(VarHead,Replacement),
    subst_same(Expr, VarHead, Replacement, NewExpr),
    check_replace_with_local_var(VarTail, NewExpr, Result).

% change the variable into an new anonymous one (but copy attributes)
replace_by_value(Var,Replacement):- var(Var),!,copy_term(Var,Replacement).
% creates a deep copy that allows caller to destructively change it
replace_by_value(Var,Replacement):- duplicate_term(Var,Replacement).

%! subst_same(+Term, +OldTerm, +NewTerm, -ResultTerm) is det.
%
% Recursively substitutes occurrences of OldTerm with NewTerm within a Prolog term (Term),
% producing a new term (ResultTerm). This predicate handles both simple and compound terms, including lists.

% Note: Matching is done with the SWI same_term predicate which states that terms are equal if the
% condition "the same variable, equivalent atomic data or a compound term allocated at the same address"
% If the current term (Term) exactly matches OldTerm (with above criteria).
subst_same(Term, OldTerm, NewTerm, NewTerm) :-
   same_term(OldTerm, Term),
   !.

% If the current term is not a compound term (like an atom, number or the wrong variable) it stays the same
subst_same(Term, _, _, Term) :- \+ compound(Term), !.

% If the current term is a list, it processes each element of the list recursively.
subst_same([Old|Structure], OldTerm, NewTerm, [New|StructureO]) :- !,
    subst_same(Old, OldTerm, NewTerm, New),
    subst_same(Structure, OldTerm, NewTerm, StructureO).

% Compound Terms are decomposed and reconstructed with the possibly modified arguments.
subst_same(OldStructure, OldTerm, NewTerm, NewStructure) :-
    OldStructure =.. [Functor|Args],
    subst_same(Args, OldTerm, NewTerm, NewArgs),
    NewStructure =.. [Functor|NewArgs].

% =================================================================
% =================================================================
% =================================================================
%  SPACE EDITING
% =================================================================
% =================================================================
% =================================================================
new_space_function_name('new-space').

is_make_new_kb([NEWKB|Props],KB,ExtraProps):- atom(NEWKB),new_space_function_name(NEWKB),!,
    oo_new('space',[],KB),
    oo_set_attibutes(ObjectID,extra_props,Props),
    oo_set_attibutes(ObjectID,extra_props,ExtraProps).


eval_20(Eq,RetType,_Dpth,_Slf,[NEWKB|Props],Space):- is_make_new_kb([NEWKB|Props],Space,[]), !, check_returnval(Eq,RetType,Space).

eval_20(Eq,RetType,Depth,Self,[Op,Space|Args],Res):- is_space_op(Op),!,
  eval_space_start(Eq,RetType,Depth,Self,[Op,Space|Args],Res).

eval_space_start(Eq,RetType,_Depth,_Self,[_Op,_Other,Atom],Res):-
  (Atom == [] ;  Atom =='Empty';  Atom =='Nil'),!,make_nop(RetType,'False',Res),check_returnval(Eq,RetType,Res).

eval_space_start(Eq,RetType,Depth,Self,[Op,Other|Args],Res):-
  into_space(Depth,Self,Other,Space),
  eval_space(Eq,RetType,Depth,Self,[Op,Space|Args],Res).


eval_space(Eq,RetType,_Dpth,_Slf,['add-atom',Space,PredDecl],Res):- !,
   do_metta(python,load,Space,PredDecl,TF),make_nop(RetType,TF,Res),check_returnval(Eq,RetType,Res).

eval_space(Eq,RetType,_Dpth,_Slf,['remove-atom',Space,PredDecl],Res):- !,
   do_metta(python,unload_all,Space,PredDecl,TF),
   make_nop(RetType,TF,Res),check_returnval(Eq,RetType,Res).

eval_space(Eq,RetType,_Dpth,_Slf,['atom-count',Space],Count):- !,
    ignore(RetType='Number'),ignore(Eq='='),
    'atom-count'(Space, Count).
    %findall(Atom, metta_atom(Space, Atom),Atoms),
    %length(Atoms,Count).

eval_space(Eq,RetType,_Dpth,_Slf,['atom-replace',Space,Rem,Add],TF):- !,
   copy_term(Rem,RCopy), as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Space,load,Add)),TF),
 check_returnval(Eq,RetType,TF).

eval_space(Eq,RetType,_Dpth,_Slf,['get-atoms',Space],Atom):- !,
  ignore(RetType='Atom'),
  get_metta_atom_from(Space, Atom),
  check_returnval(Eq,RetType,Atom).

% Match-TEMPLATE
eval_space(Eq,_RetType,Depth,Self,['match',Other,Goal,Template],TemplateO):-!,
  metta_atom_iter(Eq,Depth,Self,Other,Goal),TemplateO=Template.

% Match-ELSE
eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Then,Else],Template):- !,
  (eval_ne(Eq,RetType,Depth,Self,['match',Other,Goal,Then],Template)*->true;Template=Else).
/*
eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Template],Res):- !,
   metta_atom_iter(Eq,Depth,Self,Other,Goal),
   eval_args(Eq,RetType,Depth,Self,Template,Res).
*/

%metta_atom_iter(Eq,_Depth,_Slf,Other,[Equal,[F|H],B]):- Eq == Equal,!,  % maybe_trace(unknown),
%   metta_eq_def(Eq,Other,[F|H],B).

/*
metta_atom_iter(Eq,Depth,Self,Other,[Equal,[F|H],B]):- Eq == Equal,!,  % maybe_trace(unknown),
   metta_eq_def(Eq,Other,[F|H],BB),
   eval_sometimes(Eq,_RetType,Depth,Self,B,BB).
*/

metta_atom_iter(_Eq,Depth,_,_,_):- Depth<3,!,fail.
metta_atom_iter(Eq,Depth,Self,Other,[And|Y]):- atom(And), is_comma(And),!,
  (Y==[] -> true ;
    ( D2 is Depth -1, Y = [H|T],
       metta_atom_iter(Eq,D2,Self,Other,H),metta_atom_iter(Eq,D2,Self,Other,[And|T]))).

%metta_atom_iter(Eq,Depth,_Slf,Other,X):- dcall0000000000(eval_args_true(Eq,_RetType,Depth,Other,X)).
metta_atom_iter(Eq,Depth,Self,Other,X):-
  %copy_term(X,XX),
  dcall0000000000(metta_atom_true(Eq,Depth,Self,Other,X)). %, X=XX.

metta_atom_true(_Eq,Depth,Self,Other,H):-
      can_be_ok(metta_atom_true,H),
      into_space(Depth,Self,Other,Space),
      metta_atom(Space,H).
% is this OK?
%metta_atom_true(Eq,Depth,Self,Other,H):- nonvar(H), metta_eq_def(Eq,Other,H,B), D2 is Depth -1, eval_args_true(Eq,_,D2,Self,B).
% is this OK?
%metta_atom_true(Eq,Depth,Self,Other,H):- Other\==Self, nonvar(H), metta_eq_def(Eq,Other,H,B), D2 is Depth -1, eval_args_true(Eq,_,D2,Other,B).



eval_args_true_r(Eq,RetType,Depth,Self,X,TF1):-
  ((eval_ne(Eq,RetType,Depth,Self,X,TF1),  \+  is_False(TF1));
     ( \+  is_False(TF1),metta_atom_true(Eq,Depth,Self,Self,X))).

eval_args_true(Eq,RetType,Depth,Self,X):-
 % can_be_ok(eval_args_true,X),
 % metta_atom_true(Eq,Depth,Self,Self,X);
   (nonvar(X),eval_ne(Eq,RetType,Depth,Self,X,TF1),  \+  is_False(TF1)).


metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom_asserted(Other,H),true,Ref).
%can_be_ok(A,B):- cant_be_ok(A,B),!,fbug(cant_be_ok(A,B)),trace.
can_be_ok(_,_).

cant_be_ok(_,[Let|_]):- Let==let.













eval_10(Eq,RetType,Depth,Self,['with-debug',E,X],Y):- !,
   with_debug(E,eval_10(Eq,RetType,Depth,Self,X,Y)).


% =================================================================
% =================================================================
% =================================================================
%  CASE/SWITCH
% =================================================================
% =================================================================
% =================================================================
% Macro: case
:- nodebug(metta(case)).

eval_10(Eq,RetType,Depth,Self,['switch',A,CL|T],Res):- !,
  eval_10(Eq,RetType,Depth,Self,['case',A,CL|T],Res).

eval_10_disabled(Eq,RetType,Depth,Self,[P,X|More],YY):- fail, is_list(X),X=[_,_,_],simple_math(X),
   eval_selfless_2(X,XX),X\=@=XX,!, eval_20(Eq,RetType,Depth,Self,[P,XX|More],YY).
% if there is only a void then always return nothing for each Case
eval_10(Eq,_RetType,Depth,Self,['case',A,[[Void,Else]]],Res):-
   ('%void%' == Void),!,
   (eval_args(Eq,_UnkRetType,Depth,Self,A,_) *-> (fail) ; Res=Else).

% if there is nothing for case just treat like a collapse
eval_10(Eq,_RetType,Depth,Self,['case',A,[]], _NoResult):- !,
  forall(eval(Eq,_RetType2,Depth,Self,A,_),true),!, fail.
  %once(eval_args(Eq,_RetType2,Depth,Self,A,_)),
  %make_nop(RetType,[],NoResult).


into_case_keys(_,[],[]).
into_case_keys(Nth,[Case0|CASES],[Key-Value|KVs]):-
  Nth1 is Nth+1,
  is_case(Key,Case0,Value),
  if_trace((case),(format('~N'),writeqln(c(Nth,Key)=Value))),
  into_case_keys(Nth1,CASES,KVs).

% Macro: case
eval_10(Eq,RetType,Depth,Self,['case',A,CL|T],Res):- !,
   must_det_ll(T==[]),
   into_case_list(CL,CASES),
   into_case_keys(1,CASES,KVs),
   eval_case(Eq,RetType,Depth,Self,A,KVs,Res).

eval_10(Eq,RetType,Depth,Self,['case'|T],Res):- !,
    bt,trace,eval_10(Eq,RetType,Depth,Self,['case'|T],Res).

eval_20(Eq,RetType,Depth,Self,['case'|T],Res):- !,
    bt,trace,eval_10(Eq,RetType,Depth,Self,['case'|T],Res).


void_or_empty_value(KVs,Value):- member(Void -Value,KVs),Void=='%void%',!.  % still support void

eval_case(Eq,CaseRetType,Depth,Self,A,KVs,Res):-
   if_trace((case),(writeqln('case'=A))),
   (((eval_args(Eq,_UnkRetType,Depth,Self,A,AA)*->true;AA='Empty'),
     if_trace((case),writeqln('switch'=AA)))
      *-> (select_case(Depth,Self,AA,KVs,Value)->true;(void_or_empty_value(KVs,Value),!))
       ; (void_or_empty_value(KVs,Value),!)),
   eval_args(Eq,CaseRetType,Depth,Self,Value,Res).

  select_case(Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Depth,Self,Cases,CasES),
       (best_key(AA,CasES,Value) -> true ;
        (void_or_empty_value(CasES,Value))))).

  best_key(AA,Cases,Value):- member(Match-Value,Cases),AA = Match,!.
  best_key(AA,Cases,Value):-
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),(AA=@=Match,ignore(AA=Match)))->true;
        (member(Match-Value,Cases),AA = Match))).

    into_case_list(CASES,CASES):- is_list(CASES),!.
        is_case(AA,[AA,Value],Value):-!.
        is_case(AA,[AA|Value],Value).

   maybe_special_keys(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval_args(Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(_Depth,_Self,[],[]):-!.
   maybe_special_keys(Depth,Self,[_|KVI],KVO):-
     maybe_special_keys(Depth,Self,KVI,KVO).



% =================================================================
% =================================================================
% =================================================================
%  COLLAPSE/SUPERPOSE
% =================================================================
% =================================================================
% =================================================================

%;; collapse-bind because `collapse` doesnt guarentee shared bindings
eval_10(Eq,RetType,Depth,Self,['collapse-bind',List],Res):-!,
 maplist_ok_fails(eval_ne(Eq,RetType,Depth,Self),List,Res).

maplist_ok_fails(Pred2,[A|AA],BBB):- !,
 (call(Pred2,A,B) -> (BBB=[B|BB], maplist_ok_fails(Pred2,AA,BB))
   ; maplist_ok_fails(Pred2,AA,BBB)).
maplist_ok_fails(_Pred2,[],[]).

%;; superpose-bind because `superpose` doesnt guarentee shared bindings
% @TODO  need to keep bindings
eval_10(Eq,RetType,Depth,Self,['superpose-bind',List],Res):- !,
       re_member(Res,E,List),
       eval_ret(Eq,RetType,Depth,Self,E,Res).

re_member(Res,E,List):- term_variables(Res+E+List,TV),copy_term(TV,Copy),
    member(E,List),TV=Copy.

% Premature optimizations
/*
eval_10(Eq,RetType,Depth,Self,['collapse',Eval],RetVal):-
     Eval= [SetOp,[SuperPoseOp1,List1],[SuperPoseOp2,List2]],
     maplist(symbol,[SetOp,SuperPoseOp1,SuperPoseOp2]),
     Eval= [SetOp,['superpose',List1],['superpose',List2]],
     member(SetOp,['subtraction','union','intersection']),!,
     symbol_concat(SetOp,'-atom',SetOp_Atom),!,
     eval_args(Eq,RetType,Depth,Self,[SetOp_Atom,List1,List2],RetVal).
*/

eval_10(Eq,RetType,Depth,Self,['collapse',List],RetVal):- is_list(List),
    List = [SetOp,AsList1,AsList2], symbol(SetOp),
    member(SetOp,['subtraction','union','intersection']),!,
    symbol_concat(SetOp,'-atom',SetOp_Atom),
    findall_eval(Eq,RetType,Depth,Self,AsList1,List1),
    findall_eval(Eq,RetType,Depth,Self,AsList2,List2),
    eval_args(Eq,RetType,Depth,Self,[SetOp_Atom,List1,List2],RetVal).


%[collapse,[1,2,3]]
eval_10(Eq,RetType,Depth,Self,['collapse',List],Res):-!,
 findall_eval(Eq,RetType,Depth,Self,List,Res).


eval_10(Eq,RetType,Depth,Self,['superpose',List],Res):- !,
       member(E,List),
       eval_ret(Eq,RetType,Depth,Self,E,Res).

%[superpose,[1,2,3]]
old_eval_20(_Eq,RetType,_Depth,_Self,['superpose',List],Res):- List==[], !,
  make_empty(RetType,[],Res).
old_eval_20(Eq,RetType,Depth,Self,['superpose',List],Res):- !,
  (((
   is_user_defined_head(Eq,Self,List) ,eval_args(Eq,RetType,Depth,Self,List,UList),
   List\=@=UList)
    *->  old_eval_20(Eq,RetType,Depth,Self,['superpose',UList],Res)
       ; ((member(E,List),eval_args(Eq,RetType,Depth,Self,E,Res))*->true;make_nop(RetType,[],Res)))),
  \+ Res = 'Empty'.

%[sequential,[1,2,3]]
eval_10(Eq,RetType,Depth,Self,['sequential',List],Res):- !,
  (((fail,is_user_defined_head(Eq,Self,List) ,eval_args(Eq,RetType,Depth,Self,List,UList), List\=@=UList)
    *-> eval_10(Eq,RetType,Depth,Self,['sequential',UList],Res)
       ; ((member(E,List),eval_ne(Eq,RetType,Depth,Self,E,Res))*->true;make_nop(RetType,[],Res)))).


get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E),nocut.
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).

eval20_failed(Eq,RetType,Depth,Self, Term, Res):-
  notrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval_args(Eq,RetType,Depth,Self, Term, Res).


sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- woc(sub_sterm1(Sub,Term)).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).
eval20_failed_2(Eq,RetType,Depth,Self, Term, Res):-
   notrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, findall_eval(Eq,RetType,Depth,Self,List,Var),
   eval_args(Eq,RetType,Depth,Self, Term, Res).


% =================================================================
% =================================================================
% =================================================================
%  NOP/EQUALITU/DO
% =================================================================
% =================================================================
% ================================================================
eval_20(_Eq,RetType,_Depth,_Self,['nop'],                 NoResult ):- !,
   make_nop(RetType,[], NoResult).
eval_20(_Eq,RetType,_Depth,_Self,['empty'],                Empty ):- !, fail,
   make_empty(RetType, Empty).
eval_20(_Eq,RetType,Depth,Self,['nop',Expr], NoResult ):- !,
   make_nop(RetType,[], NoResult),
  ignore(eval_args('=',_RetType2,Depth,Self,Expr,_)).


eval_20(Eq,RetType,Depth,Self,['do',Expr], NoResult):- !,
  forall(eval_args(Eq,_RetType2,Depth,Self,Expr,_),true),
  %eval_ne(Eq,_RetType2,Depth,Self,Expr,_),!,
  make_empty(RetType,[],NoResult).

eval_20(Eq,RetType1,Depth,Self,['call!'|S], TF):- !, eval_call(Eq,RetType1,Depth,Self,S,TF).
eval_20(Eq,RetType1,Depth,Self,['call-p!'|S], TF):- !, eval_call(Eq,RetType1,Depth,Self,S,TF).
eval_20(Eq,RetType1,Depth,Self,['call-fn!'|S], R):- !, eval_call_fn(Eq,RetType1,Depth,Self,S,R).
eval_20(Eq,RetType1,Depth,Self,['call-fn-nth!',Nth|S], R):-
    length(Left,Nth),
    append(Left,Right,S),
    append(Left,[R|Right],NewS),!,
    eval_call(Eq,RetType1,Depth,Self,NewS,_).

eval_40(Eq,RetType1,Depth,Self,['call'|S], TF):- !, eval_call(Eq,RetType1,Depth,Self,S,TF).
eval_40(Eq,RetType1,Depth,Self,['call-p'|S], TF):- !, eval_call(Eq,RetType1,Depth,Self,S,TF).
eval_40(Eq,RetType1,Depth,Self,['call-fn'|S], R):- !, eval_call_fn(Eq,RetType1,Depth,Self,S,R).
eval_40(Eq,RetType1,Depth,Self,['call-fn-nth',Nth|S], R):-
    length(Left,Nth),
    append(Left,Right,S),
    append(Left,[R|Right],NewS),!,
    eval_call(Eq,RetType1,Depth,Self,NewS,_).

eval_call(Eq,RetType,Depth,Self,S,TF):-
  with_metta_ctx(Eq,RetType,Depth,Self,['call-p'|S],eval_call(S,TF)).
eval_call_fn(Eq,RetType,Depth,Self,S,TF):-
  with_metta_ctx(Eq,RetType,Depth,Self,['call-fn'|S],eval_call_fn(S,TF)).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).


% =================================================================
% =================================================================
% =================================================================
%  CONS/DECONS
% =================================================================
% =================================================================
% =================================================================

must_unify(A,A):-!.
must_unify(A,B):- fail, throw('Error-last-form'(must_unify(A,B))). % @TODO

% OLD
eval_20(_Eq,_RetType,_Depth,_Self,['decons-atom',OneArg],_):- OneArg==[], !, fail. %H=[],T=[],!.
eval_20(_Eq,_RetType,_Depth,_Self,['decons-atom',OneArg],[H,T]):- !, must_unify(OneArg,[H|T]).
eval_20(_Eq,_RetType,_Depth,_Self,['cons-atom'|TwoArgs],[H|T]):-!, must_unify(TwoArgs,[H,T]).
% NEW
eval_20(_Eq,_RetType,_Depth,_Self,['decons',OneArg],[H,T]):- !, must_unify(OneArg,[H|T]).
eval_20(_Eq,_RetType,_Depth,_Self,['cons'|TwoArgs],[H|T]):-!, must_unify(TwoArgs,[H,T]).


%eval_20(Eq,RetType,Depth,Self,['get-doc'|Args],Res):- !,with_all_spaces(eval_args(Eq,RetType,Depth,Self,['metta-get-doc'|Args],Res)),!.
%eval_20(Eq,RetType,Depth,Self,['help!'|Args],Res):-!,with_all_spaces(eval_args(Eq,RetType,Depth,Self,['metta-help!'|Args],Res)),!.

with_all_spaces(Goal):-
 locally(b_setval(with_all_spaces,t),Goal).
using_all_spaces:- nb_current(with_all_spaces,t).

% =================================================================
% =================================================================
% =================================================================
%  if/If
% =================================================================
% =================================================================
% =================================================================
metta_container_sub_part(Container,Item):- is_space(Container),!,metta_atom(Container,Item).
metta_container_sub_part(Container,Item):-  is_list(Container),!,member(Item,Container).

% GUESS `¯\\_ :( _/¯` what version of unify they are trying to use? ¯(°_o)/¯

% 1) If Arg1 is a space, then we redirect to a `match` operation.
eval_10(Eq,RetType,Depth,Self,['unify',Arg1,Arg2|Args],Res):- is_metta_space(Arg1), !,
  eval_args(Eq,RetType,Depth,Self,['match',Arg1,Arg2|Args],Res).
% 2) If Arg1 and Arg2 are nonvars and Arg1 is declared a `Container`, then use `container-unify`
eval_10(Eq,RetType,Depth,Self,['unify',Arg1,Arg2|Args],Res):- nonvar(Arg1), nonvar(Arg2), get_type(Depth,Self,Arg1,'Container'),!,
  eval_args(Eq,RetType,Depth,Self,['container-unify',Arg1,Arg2|Args],Res).
% 3) Otherwise, default to using `if-unify` for the unify operation.
eval_10(Eq,RetType,Depth,Self,['unify',Arg1,Arg2|Args],Res):- !,
  eval_args(Eq,RetType,Depth,Self,['if-unify',Arg1,Arg2|Args],Res).

eval_10(Eq,RetType,Depth,Self,['container-unify',Arg1,Arg2,Then|ElseL],Res):- !,
   ((metta_container_sub_part(Arg1,Part),eval_args_true(Eq,'Bool',Depth,Self,['==',Part,Arg2]))
    *-> eval_args(Eq,RetType,Depth,Self,Then,Res)
    ; (ElseL=[Else],eval_args(Eq,RetType,Depth,Self,Else,Res))).

eval_10(Eq,RetType,Depth,Self,['if-unify',X,Y,Then|ElseL],Res):- !,
   (if_or_else(X=Y,eval_args_true(Eq,'Bool',Depth,Self,['==',X,Y]))
     *-> eval_args(Eq,RetType,Depth,Self,Then,Res)
    ; (ElseL=[Else],eval_args(Eq,RetType,Depth,Self,Else,Res))).



eval_10(Eq,RetType,Depth,Self,['if-decons-expr',HT,H,T,Then,Else],Res):- !,
   (HT = [H|T]
     -> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).


eval_10(Eq,RetType,Depth,Self,['if-equal',X,Y,Then,Else],Res):- !,

   ( \+ \+ (eval_args(Eq,'Bool',Depth,Self,['==',X,Y],TF),is_True(TF))
     -> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).

eval_args_bool(_Eq,_Depth,_Self,Cond, TF):- var(Cond), !, member(TF,['True','False']),Cond=TF.
eval_args_bool(Eq,Depth,Self,Cond, TF):- eval_args(Eq,'Bool', Depth,Self,Cond, TF). %, \+ \+ (get_type(TryTF,Type),Type=='Bool'),TryTF=TF.

eval_10(Eq,RetType,Depth,Self,['if',Cond,Then,Else],Res):- !, %var(Cond),  !,
   eval_args_bool(Eq, Depth,Self,Cond, TF),
    (is_True(TF)  -> eval_args(Eq,RetType,Depth,Self,Then,Res) ;
    (is_False(TF) -> eval_args(Eq,RetType,Depth,Self,Else,Res) ;
     Res = ['if',Cond,Then,Else])).
/*
eval_10(Eq,RetType,Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval_args_bool(Eq, Depth,Self,Cond, TF),
   ((is_True(TF) *-> eval_args(Eq,RetType,Depth,Self,Then,Res) ;
     (nop(is_False(TF)) -> eval_args(Eq,RetType,Depth,Self,Else,Res))) *-> true ;
       (fail, Res = ['if',Cond,Then,Else])).
*/
/*
eval_10_disabled(Eq,RetType,Depth,Self,['If',Cond,Then,Else],Res):-  fail, !,
    (eval_args_true(Eq,'Bool',Depth,Self,Cond)
     *-> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).

eval_10_disabled(Eq,RetType,Depth,Self,['If',Cond,Then],Res):- fail, !,
   eval_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).
*/


eval_10_disabled(Eq,RetType,Depth,Self,['if',Cond,Then],Res):- !, %var(Cond),  !,
   eval_args_bool(Eq, Depth,Self,Cond, TF),
    (is_True(TF)  -> eval_args(Eq,RetType,Depth,Self,Then,Res) ;
    (is_False(TF) -> fail ; Res = ['if',Cond,Then])).

%eval_10(Eq,RetType,Depth,Self,['If',Cond,Then|Else],Res):- !,
%   eval_10(Eq,RetType,Depth,Self,['if',Cond,Then|Else],Res).

eval_20_failed(Eq,RetType,Depth,Self,[X|Args],Res):-
  quietly((findall(metta_defn(Self,[Y|Args],Body),(metta_defn(Self,[Y|Args],Body),X==Y),L))),
  L = [metta_defn(Self,[Y|Args],Body)], X==Y, !,
  eval_10(Eq,RetType,Depth,Self,Body,Res).


eval_20(Eq,RetType,_Dpth,_Slf,[_,Nothing],NothingO):-
   'Nothing'==Nothing,!,do_expander(Eq,RetType,Nothing,NothingO).


% =================================================================
% =================================================================
% =================================================================
%  CONS/CAR/CDR
% =================================================================
% =================================================================
% =================================================================



into_pl_list(Var,Var):- var(Var),!.
into_pl_list(Nil,[]):- Nil == 'Nil',!.
into_pl_list([Cons,H,T],[HH|TT]):- Cons == 'Cons', !, into_pl_list(H,HH),into_pl_list(T,TT),!.
into_pl_list(X,X).

into_metta_cons(Var,Var):- var(Var),!.
into_metta_cons([],'Nil'):-!.
into_metta_cons([Cons, A, B ],['Cons', AA, BB]):- 'Cons'==Cons, no_cons_reduce, !,
  into_metta_cons(A,AA), into_metta_cons(B,BB).
into_metta_cons([H|T],['Cons',HH,TT]):- into_metta_cons(H,HH),into_metta_cons(T,TT),!.
into_metta_cons(X,X).

into_listoid(AtomC,Atom):- AtomC = [Cons,H,T],Cons=='Cons',!, Atom=[H,[T]].
into_listoid(AtomC,Atom):- is_list(AtomC),!,Atom=AtomC.
into_listoid(AtomC,Atom):- typed_list(AtomC,_,Atom),!.

:- if( \+  current_predicate( typed_list / 3 )).
typed_list(Cmpd,Type,List):-  compound(Cmpd), Cmpd\=[_|_], compound_name_arguments(Cmpd,Type,[List|_]),is_list(List).
:- endif.

%eval_20(Eq,RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).

%eval_20(Eq,RetType,Depth,Self,['flatten'|List], Flat):- !, maplist(eval_args(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).

throw_metta_error(Term):- % (nb_current(previous_nths,NthL)->true;NthL=[]),notrace, nop(NthL=_),
  check_trace(errors), throw_metta_return(Term).


eval_20(Eq,RetType,_Dpth,_Slf,['car-atom',Atom],CAR_Y):- !, Atom=[CAR|_],!,do_expander(Eq,RetType,CAR,CAR_Y).
eval_20(Eq,RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).
eval_20(Eq,RetType,_Dpth,_Slf,['car-atom-or-fail',Atom],CAR_Y):- !, Atom=[CAR|_],!,do_expander(Eq,RetType,CAR,CAR_Y).
eval_20(Eq,RetType,_Dpth,_Slf,['cdr-atom-or-fail',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).

eval_20(Eq,RetType,Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- fail, no_cons_reduce, !,
  eval_args(Eq,RetType,Depth,Self,A,AA), eval_args(Eq,RetType,Depth,Self,B,BB).

%eval_20(_Eq,_RetType,Depth,Self,['::'|PL],Prolog):-  maplist(as_prolog_x(Depth,Self),PL,Prolog),!.
%eval_20(_Eq,_RetType,Depth,Self,['@'|PL],Prolog):- as_prolog_x(Depth,Self,['@'|PL],Prolog),!.

eval_20(Eq,RetType,Depth,Self,['Cons', A, B ],[AA|BB]):- fail,  \+ no_cons_reduce, !,
   eval_args(Eq,RetType,Depth,Self,A,AA), eval_args(Eq,RetType,Depth,Self,B,BB).



% =================================================================
% =================================================================
% =================================================================
%  STATE EDITING
% =================================================================
% =================================================================
% =================================================================

eval_20(Eq,RetType,Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !,
 call_in_shared_space(((eval_args(Eq,RetType,Depth,Self,StateExpr,StateMonad),
  eval_args(Eq,RetType,Depth,Self,UpdatedValue,Value),
  catch_metta_return('change-state!'(Depth,Self,StateMonad, Value, Ret),Ret)))).
eval_20(Eq,RetType,Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  call_in_shared_space(((eval_args(Eq,RetType,Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad)))).
eval_20(Eq,RetType,Depth,Self,['get-state',StateExpr],Value):- !,
  call_in_shared_space((eval_args(Eq,RetType,Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value))).

% X=bar(baz), nb_setval(foo,X), nb_current(foo,Y), nb_linkval(foo,Y), nb_current(foo,Z), nb_setarg(1,Z,[1,2]), nb_current(foo,B).


% eval_20(Eq,RetType,Depth,Self,['get-state',Expr],Value):- !, eval_args(Eq,RetType,Depth,Self,Expr,State), arg(1,State,Value).


check_state_type:- !.
check_type:- option_else(typecheck,TF,'False'),!, TF=='True'.

:- dynamic is_registered_state/1.
:- flush_output.
:- setenv('RUST_BACKTRACE',full).

% Function to check if an value is registered as a state name
:- dynamic(is_registered_state/1).
is_nb_state(G):- is_valid_nb_state(G) -> true ;
                 is_registered_state(G),nb_bound(G,S),is_valid_nb_state(S).


:- multifile(state_type_method/3).
:- dynamic(state_type_method/3).
state_type_method(is_nb_state,new_state,init_state).
state_type_method(is_nb_state,clear_state,clear_nb_values).
state_type_method(is_nb_state,add_value,add_nb_value).
state_type_method(is_nb_state,remove_value,'change-state!').
state_type_method(is_nb_state,replace_value,replace_nb_value).
state_type_method(is_nb_state,value_count,value_nb_count).
state_type_method(is_nb_state,'get-state','get-state').
state_type_method(is_nb_state,value_iter,value_nb_iter).
%state_type_method(is_nb_state,query,state_nb_query).

% Clear all values from a state
clear_nb_values(StateNameOrInstance) :-
    fetch_or_create_state(StateNameOrInstance, State),
    nb_setarg(1, State, []).



% Function to confirm if a term represents a state
is_valid_nb_state(State):- compound(State),compound_name_arity(State,'State',N),N>0.

% Find the original name of a given state
state_original_name(State, Name) :-
    is_registered_state(Name),
    call_in_shared_space(nb_bound(Name, State)).

% Register and initialize a new state
init_state(Name) :-
    State = 'State'(_,_),
    asserta(is_registered_state(Name)),
    call_in_shared_space(nb_setval(Name, State)).

% Change a value in a state
'change-state!'(Depth,Self,StateNameOrInstance, UpdatedValue, Out) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(2, State, Type),
    ( (check_type,\+ get_type(Depth,Self,UpdatedValue,Type))
     -> (Out = ['Error', UpdatedValue, 'BadType'])
     ; (nb_setarg(1, State, UpdatedValue), Out = State) ).

% Fetch all values from a state
'get-state'(StateNameOrInstance, Values) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(1, State, Values).

'new-state'(Depth,Self,Init,'State'(Init, Type)):- check_type->get_type(Depth,Self,Init,Type);true.

'new-state'(Init,'State'(Init, Type)):- check_type->get_type(10,'&self',Init,Type);true.

fetch_or_create_state(Name):- fetch_or_create_state(Name,_).
% Fetch an existing state or create a new one

fetch_or_create_state(State, State) :- is_valid_nb_state(State),!.
fetch_or_create_state(NameOrInstance, State) :-
    (   atom(NameOrInstance)
    ->  (is_registered_state(NameOrInstance)
        ->  nb_bound(NameOrInstance, State)
        ;   init_state(NameOrInstance),
            nb_bound(NameOrInstance, State))
    ;   is_valid_nb_state(NameOrInstance)
    ->  State = NameOrInstance
    ;   nop(fbug('Error: Invalid input.'))
    ),
    is_valid_nb_state(State).

% =================================================================
% =================================================================
% =================================================================
%  GET-TYPE
% =================================================================
% =================================================================
% =================================================================
type_cast(Depth,Self,Val,Into,Casted):-
    get_type(Depth,Self,Val,From),
    (type_accepted_from(Into,From)
     ->Casted=Val
      ;Casted=['Error',Val,'BadType',Into]).

type_accepted_from(Into,From):-Into=From,!.
type_accepted_from(Into,From):-write_src_uo(type_accepted_from(Into,From)).


%use default self
eval_20(Eq,RetCasted,Depth,Self,['type-cast',Val,Into,Self],Casted):-current_self(Self),!,
eval_20(Eq,RetCasted,Depth,Self,['type-cast',Val,Into],Casted).

%use other space
eval_20(Eq,RetCasted,Depth,Self,['type-cast',Val,Into,Other],Casted):-!,
    into_space(Depth,Self,Other,Space),
    eval_20(Eq,RetCasted,Depth,Space,['type-cast',Val,Into],Casted).

eval_20(_Eq,_RetCasted,Depth,Self,['type-cast',Val,Into],Casted):-is_list(Val),!,
    catch_metta_return(type_cast(Depth,Self,Val,Into,Casted),CastedM),
    var(CastedM).

eval_20(Eq,RetCasted,Depth,Self,['type-cast',Val,Into],CastedO):-!,
    if_or_else(type_cast(Depth,Self,Val,Into,Casted),Casted=Val),
    %term_singletons(Casted,[]),
    %Casted\==[],Casted\==Val,Into,!,
    do_expander(Eq,RetCasted,Casted,CastedO).

into_space_rarg(_Depth,Self,[Arg],Self,Arg):-!.
into_space_rarg( Depth,Self,[Arg,Other],Space,Arg):-
  into_space(Depth,Self,Other,Space).

eval_20(_Eq,_RetType,Depth,Self,['get-vtype'|ValOther],Type):- !,
    into_space_rarg(Depth,Self,ValOther,Other,Val),
    get_value_type(Depth,Other,Val,Type).

eval_20(_Eq,_RetType,Depth,Self,['get-types'|ValOther],Type):- !,
    into_space_rarg(Depth,Self,ValOther,Other,Val),
    get_types(Depth,Other,Val,Type).

eval_20(_Eq,_RetType,Depth,Self,['get-dtype'|ValOther],Type):- !,
    into_space_rarg(Depth,Self,ValOther,Other,Val),
    get_dtype(Other,Val,Type).

eval_20(_Eq,_RetType,Depth,Self,['get-dtypes'|ValOther],Type):- !,
    into_space_rarg(Depth,Self,ValOther,Other,Val),
    get_dtypes(Other,Val,Type).

eval_20(Eq,RetType,Depth,Self,['get-ftype'|ValOther],Type):- !,
    into_space_rarg(Depth,Self,ValOther,Other,Val),
    get_ftype(Eq,RetType,Depth,Other,Val,Type).

eval_20(Eq,RetType,Depth,Self,['get-type'|ValOther],Type):- !,
    into_space_rarg(Depth,Self,ValOther,Other,Val),
    get_type_expansion(Eq,RetType,Depth,Other,Val,Type).

eval_20(Eq,RetType,Depth,Self,['get-type-space'|OtherVal],Type):- !,
   into_space_and_arg(Depth,Self,OtherVal,Other,Val),
   get_type_expansion(Eq,RetType,Depth,Other,Val,Type).


% Get types (interfaces) for an object
get_dtypes(Space, Val, TypeSet) :-
    findall(Type, get_dtype(Space, Val, Type), TypeSet).

get_dtype(Space, Val, Type) :- get_type_expansion('=',_RetType,30, Space, Val,Type).


get_type_expansion(Eq,RetType,Depth,Other,Val,TypeO):-
  if_or_else(get_type_expansionA(Eq,RetType,Depth,Other,Val,TypeO),
            get_type_expansionB(Eq,RetType,Depth,Other,Val,TypeO)),
  TypeO\==[].

get_type_expansionA(Eq,RetType,Depth,Other,Val,TypeO):- is_list(Val),
    catch_metta_return(get_type(Depth,Other,Val,Type),TypeM),
    var(TypeM), Type \== '%Undefined%', % Type\==[],
    do_expander(Eq,RetType,Type,TypeO).
get_type_expansionB(Eq,RetType,Depth,Other,Val,TypeO):-
    if_or_else(get_type(Depth,Other,Val,Type),Type='%Undefined%'), %term_singletons(Type,[]), %Type\==[], Type\==Val,!,
    do_expander(Eq,RetType,Type,TypeO).

eval_20(Eq,RetType,Depth,Self,['length',L],Res):- !, eval_args(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_20(Eq,RetType,Depth,Self,['CountElement',L],Res):- !, eval_args(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).

eval_20(_Eq,_RetType,_Depth,_Self,['get-metatype',Val],TypeO):- !,
  'get-metatype'(Val,TypeO).

'get-metatype'(Val,Type):- get_metatype0(Val,Was),!,Type=Was.
    get_metatype0(Val,'Variable'):- var(Val),!.
    get_metatype0(Val,Type):- symbol(Val), !, get_symbol_metatype(Val,Type).
    get_metatype0(Val,'Expression'):- is_list(Val),!.
get_metatype0(_Val,'Grounded').

get_symbol_metatype(Val,Type):- get_type(Val,Want),get_symbol_metatype(Val,Want,Type).
get_symbol_metatype(_Vl,'Bool','Grounded'):- !.
get_symbol_metatype(Val,_Want,Type):- nb_current(Val,NewVal),'get-metatype'(NewVal,Type), nocut.
get_symbol_metatype(_Vl,'%Undefined%','Symbol'):- !.
get_symbol_metatype(_Vl,_Want,'Grounded').

% =================================================================
% =================================================================
% =================================================================
%  STRINGS
% =================================================================
% =================================================================
% =================================================================

as_metta_char(X,'#\\'(X)).

eval_20(Eq,RetType,Depth,Self,['stringToChars',String],Chars):- !, eval_args(Eq,RetType,Depth,Self,String,SS), string_chars(SS,Chars0), maplist(as_metta_char,Chars0,Chars).
eval_20(Eq,RetType,Depth,Self,['charsToString',Chars],String):- !, eval_args(Eq,RetType,Depth,Self,Chars,CC), maplist(as_metta_char,CC0,CC), string_chars(String,CC0).

% We deal with indexing, but not formatting (the stuff following the ':')(yet)
% https://doc.rust-lang.org/std/fmt/ used as a reference

format_args_get_index([C|FormatRest1], FormatRest2, Index2) :- char_code(C, Ccode), Ccode >= 48, Ccode =< 57, !, % in the range ['0'..'9']
    Index1 is Ccode-48,
    format_args_get_index1(FormatRest1, FormatRest2, Index1, Index2).
format_args_get_index(FormatRest, FormatRest, none).

% have at least one digit already. This is separate from format_args_get_index to distinguish {} and {0} cases
format_args_get_index1([C|FormatRest1], FormatRest2, Index1, Index3) :- char_code(C, Ccode), Ccode >= 48, Ccode =< 57, !, % in the range ['0'..'9']
    Index2 is (Index1*10)+(Ccode-48),
    format_args_get_index1(FormatRest1, FormatRest2, Index2, Index3).
format_args_get_index1(FormatRest, FormatRest, Index, Index).

% Placeholder to deal with formatting {<n>:<format>} later
format_args_get_format(FormatRest, FormatRest, _).

format_args_write(Arg,_) :- \+ compound(Arg), !, format_arg(Arg).
format_args_write('#\\'(Char),_) :- !, format_arg(Char).
format_args_write(Arg,_) :- format_arg(Arg).

format_arg(Arg) :- string(Arg), !, write(Arg).
format_arg(Arg):- \+ \+ write_src_woi(Arg).

format_nth_args([], _, _):- !.
format_nth_args(['{','{'|FormatRest], Iterator, Args) :- !, put('{'), format_nth_args(FormatRest, Iterator, Args). % escaped
format_nth_args(['}','}'|FormatRest], Iterator, Args) :- !, put('}'), format_nth_args(FormatRest, Iterator, Args). % escaped
format_nth_args(['{'|FormatRest1], Iterator1, Args) :-
    format_args_get_index(FormatRest1, FormatRest2, Index),
    format_args_get_format(FormatRest2, ['}'|FormatRest3], Format),
    % check that the closing '}' is not escaped with another '}'
    ((FormatRest3=[] ; ((FormatRest3=[C|_],C\='}')) )),
    % The Rust behaviour of advancing the iterator if an index is not specified
    (((Index == none))
        -> ((nth0(Iterator1,Args,Arg),Iterator2 is Iterator1+1))
        ; ((nth0(Index,Args,Arg), Iterator2 is Iterator1))),
    !, format_args_write(Arg,Format),
    format_nth_args(FormatRest3, Iterator2, Args).
format_nth_args([C|FormatRest], Iterator, Args) :- put(C), format_nth_args(FormatRest, Iterator, Args).

eval_20(Eq,RetType,Depth,Self,['format-args',Format,Args],Result):-
   eval_args(Eq,RetType,Depth,Self,Format,EFormat),
   %eval_args(Eq,'Expression',Depth,Self,Args,EArgs),
   Args=EArgs,
   is_list(EArgs),string_chars(EFormat, FormatChars), !,
   user_io(with_output_to_str( Result, format_nth_args(FormatChars, 0, EArgs))).
eval_20(Eq,RetType,Depth,Self,['format-args',_Fmt,Args],_Result) :-
   eval_args(Eq,RetType,Depth,Self,Args,EArgs),
   \+ is_list(EArgs),!,throw_metta_error(['Error',Args,'BadType']).

eval_20(Eq,RetType,_Depth,_Self,['flip'],Bool):-
   ignore(RetType='Bool'), !, as_tf(random(0,2,0),Bool),
   check_returnval(Eq,RetType,Bool).

eval_20( Eq, RetType, Depth, Self, [ 'parse' , L ] , Exp ):- !,
    eval_args( Eq, RetType, Depth, Self, L, Str ),
    once(read_metta( Str, Exp )).

eval_20( _Eq, _RetType, _Depth, _Self, [ 'repr' , L ] , Sxx ):- !,
   %eval_args( Eq, RetType, Depth, Self, L, Lis2 ),
    with_output_to_str( Sxx , write_src_woi( L ) ).

eval_20( Eq, RetType, Depth, Self, [ 'output-to-string' , L ] , Sxx ):- !,
   with_output_to_str( Sxx , eval_args( Eq, RetType, Depth, Self, L, _ )).

% =================================================================
% =================================================================
% =================================================================
%  IMPORT/BIND
% =================================================================
% =================================================================
% =================================================================
eval_20(_Eq,_RetType,_Dpth,_Slf,['extend-py!',Module],Res):-  !, 'extend-py!'(Module,Res).
eval_20(Eq,RetType,Depth,Self,['register-module!',Dir],RetVal):- !,
     eval_args(Eq,'Directory',Depth,Self,Dir,Folder),
     register_module(Self,Folder),!,
     %Folder = RetVal,
     ignore(make_nop(RetType,Self,RetVal)).
eval_20(Eq,RetType,Depth,Self,['register-module!',Name,Dir],RetVal):- !,
     eval_args(Eq,'Symbol',Depth,Self,Name,ModuleName),
     eval_args(Eq,'Directory',Depth,Self,Dir,Folder),
     register_module(Self,ModuleName,Folder),!,
     %Folder = RetVal,
     ignore(make_nop(RetType,Self,RetVal)).


eval_20(Eq,RetType,Depth,Self,['include!'|OtherFile],RetVal):- !,
     into_space_and_arg(Depth,Self,OtherFile,Space,File), include_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
% from metta in Rust
eval_20(Eq,RetType,Depth,Self,['include'|OtherFile],RetVal):- !,
     into_space_and_arg(Depth,Self,OtherFile,Space,File), include_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['load-ascii'|OtherFile],RetVal):- !,
     into_space_and_arg(Depth,Self,OtherFile,Space,File), include_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['import!'|OtherFile],RetVal):- !,
     into_space_and_arg(Depth,Self,OtherFile,Space,File), import_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['load-file!'|OtherFile],RetVal):- !,
     into_space_and_arg(Depth,Self,OtherFile,Space,File), load_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).

make_nr(_Eq,_RetType,RetVal):- as_nop(RetVal).

into_space_and_arg(_Depth,Self,[Arg],Self,Arg):-!.
into_space_and_arg( Depth,Self,[Other,Arg],Space,Arg):-
  into_space(Depth,Self,Other,Space).

%eval_20(Eq,RetType,Depth,Self,['bind!',Other,Expression],RetVal):- !,  with_scope(Eq,RetType,Depth,Self, 'mi_2_bind!'(Other,Expression,RetVal)).

eval_10(Eq,RetType,Depth,Self,['bind!',Other,Expression],RetVal):- !,
  must_det_lls(( must((
    must(into_name(Self,Other,Name)),!,
    must(eval_ne(Eq,_,Depth,Self,Expression,Value)),
    %dmsg((Name = (Expr->Value))),
    nb_bind(Name,Value), % oo_set(Value,bound_to,Name),
    make_nop(RetType,Value,RetVal))),
    check_returnval(Eq,RetType,RetVal))).

eval_10(Eq,RetType,Depth,Self,['bind!',Other,Expression],RetVal):- !,
   must((
    must_det_lls(into_name(Self,Other,Name)),!,maybe_trace(unknown),
    must(eval_args(Eq,_,Depth,Self,Expression,Value)),
    %dmsg((Name = (Expr->Value))),
    nb_bind(Name,Value), % oo_set(Value,bound_to,Name),
    make_nop(RetType,Value,RetVal))),
    check_returnval(Eq,RetType,RetVal).

'mi_2_bind!'(Other,Expr,RetVal):-
    peek_scope(Eq,RetType,_Depth,Self),
    must((into_name(Self,Other,Name),!,
    eval(Expr,Value),
    %dmsg((Name = (Expr->Value))),
    nb_bind(Name,Value), % oo_set(Value,bound_to,Name),
    make_nop(RetType,Value,RetVal))),
    check_returnval(Eq,RetType,RetVal).


with_scope(Eq,RetType,Depth,Self,Goal):-
   %grab_scope([eq=WEq,retType=WRetType,depth=WDepth,self=WSelf]),
   setup_call_cleanup(push_scope([eq=Eq,retType=RetType,depth=Depth,self=Self]),
       Goal,
       pop_scope([eq=Eq,retType=RetType,depth=Depth,self=Self])).



ppp_default_value(self,V):- current_self(V), nocut.
ppp_default_value(depth,StackMax):-  current_prolog_flag(max_tagged_integer,MaxTI),option_else('stack-max',StackMax,MaxTI).
ppp_default_value(retType,_).
ppp_default_value(eq,=).


peek_scope(Eq,RetType,Depth,Self):- peek_scope([eq=Eq,retType=RetType,depth=Depth,self=Self]).
peek_scope(List):- maplist(peek_scope_item,List).
peek_scope_item(N=V):- peek_scope_item(N,V).

peek_scope_item(N,V):- peek_current_scope(N,V),!.
peek_scope_item(N,V):- ppp_default_value(N,V),!.
peek_current_scope(N,V):- nb_current(N,List),first_of(List,V).

first_of(Nil,_):- Nil==[],!,fail.
first_of(Nil,V):- var(Nil),!,V=Nil.
first_of([V|_],V):-!.
first_of(V,V).

push_scope(Eq,RetType,Depth,Self):- must_det_lls(push_scope([eq=Eq,retType=RetType,depth=Depth,self=Self])).
push_scope(List):- maplist(push_scope_item,List).
push_scope_item(N=V):- push_scope_item(N,V),!.
push_scope_item(N,V):- push_current_scope(N,V),!.
push_current_scope(N,V):- nb_current(N,List)->nb_setval(N,[V|List]),nb_setval(N,[V]).


pop_scope(Eq,RetType,Depth,Self):- pop_scope([eq=Eq,retType=RetType,depth=Depth,self=Self]).
pop_scope(List):- maplist(pop_scope_item,List).
pop_scope_item(N=V):- pop_scope_item(N,V).
pop_scope_item(N,V):- pop_current_scope(N,V),!.
pop_scope_item(N,V):- ppp_default_value(N,V),!.


eval_20(Eq,RetType,Depth,Self,['pragma!',Other,Expr],RetVal):- !,
   must_det_lls((into_name(Self,Other,Name),nd_ignore((eval_args(Eq,RetType,Depth,Self,Expr,Value),
   set_option_value_interp(Name,Value))),  make_nop(RetType,Value,RetVal),
    check_returnval(Eq,RetType,RetVal))).
eval_20(Eq,RetType,_Dpth,Self,['transfer!',File],RetVal):- !, must((include_metta(Self,File),
   make_nop(RetType,Self,RetVal),check_returnval(Eq,RetType,RetVal))).


eval_20(Eq,RetType,Depth,Self,['save-space!',Other,File],RetVal):- !,
     (( into_space(Depth,Self,Other,Space), 'save-space!'(Space,File),!,make_nop(RetType,RetVal))),
     check_returnval(Eq,RetType,RetVal).


nd_ignore(Goal):- call(Goal)*->true;true.


% =================================================================
% =================================================================
% =================================================================
%  AND/OR
% =================================================================
% =================================================================
% =================================================================

is_True(T):- atomic(T),!,(T=='True';T==1),!.
is_True(T):- var(T),!,fbug(is_True(T)),!,fail.
is_True(T):- debug(metta(todo),'TODO: CALLING(~q)',[is_True(T)]),eval_true(T).

is_and(S):- \+ atom(S),!,fail.
%is_and(',').
is_and(S):- is_and(S,_).

is_and(S,_):- \+ atom(S),!,fail.
%is_and('and','True').
is_and('and2','True').
%is_and('#COMMA','True'). %is_and(',','True').  % is_and('And').

is_comma(C):- var(C),!,fail.
is_comma(',').
is_comma('{}').

bool_and(A,B) :- (A == 'True', B == 'True').
bool_ior(A,B) :- (A == 'True'; B == 'True'),!.
bool_xor(A,B) :- bool_ior(A,B), \+ (A == B).

eval_20(Eq,RetType,Depth,Self,['and',X,Y],TF):- !,
    eval_args(Eq,RetType,Depth,Self,X,  XTF),  % evaluate X
    eval_args(Eq,RetType,Depth,Self,Y,  YTF),  % evaluate Y
    as_tf(  (bool_and(XTF,YTF))              ,   TF).


eval_20(Eq,RetType,Depth,Self,['or',X,Y],TF):- !,
    eval_args(Eq,RetType,Depth,Self,X,  XTF),  % evaluate X
    eval_args(Eq,RetType,Depth,Self,Y,  YTF),  % evaluate Y
    as_tf(  (bool_ior(XTF,YTF))              ,   TF).

eval_20(Eq,RetType,Depth,Self,['xor',X,Y],TF):- !,
    eval_args(Eq,RetType,Depth,Self,X,  XTF),  % evaluate X
    eval_args(Eq,RetType,Depth,Self,Y,  YTF),  % evaluate Y
    as_tf(  (bool_xor(XTF,YTF))              ,   TF).


eval_20(Eq,RetType,Depth,Self,['not',X],TF):- !,
   eval_args(Eq,RetType,Depth,Self,X,  XTF),  % evaluate X
   as_tf(  (bool_xor(XTF,'True'))              ,   TF).


% ================================================
% === function / return of minimal metta
eval_20(Eq,RetType,Depth,Self,['function',X],Res):- !, gensym(return_,RetF),
  RetUnit=..[RetF,Res],
  catch(locally(b_setval('$rettag',RetF),
           eval_args(Eq,RetType,Depth,Self,X, Res)),
        return(RetUnitR),RetUnitR=RetUnit).
eval_20(Eq,RetType,Depth,Self,['return',X],_):- !,
  nb_current('$rettag',RetF),RetUnit=..[RetF,Val],
  eval_args(Eq,RetType,Depth,Self,X, Val), throw(return(RetUnit)).
% ================================================

% ================================================
% === catch / throw of mettalog
eval_20(Eq,RetType,Depth,Self,['catch',X,EX,Handler],Res):- !,
  catch(eval_args(Eq,RetType,Depth,Self,X, Res),
         EX,eval_args(Eq,RetType,Depth,Self,Handler, Res)).
eval_20(Eq,_TRetType,Depth,Self,['throw',X],_):- !,
  eval_args(Eq,_RetType,Depth,Self,X, Val), throw(Val).
% ================================================

eval_20(Eq,RetType,Depth,Self,['number-of',X],N):- !,
   findall_eval(Eq,RetType,Depth,Self,X,ResL),
   length(ResL,N), ignore(RetType='Number').

eval_20(Eq,RetType,Depth,Self,['number-of',X,N],TF):- !,
   findall_eval(Eq,RetType,Depth,Self,X,ResL),
   length(ResL,N), true_type(Eq,RetType,TF).

eval_20(Eq,RetType,Depth,Self,['findall',Template,X],ResL):- !,
   findall(Template,eval_args(Eq,RetType,Depth,Self,X,_),ResL).


eval_20(Eq,RetType,Depth,Self,['limit',NE,E],R):-  !,
   eval_args('=','Number',Depth,Self,NE,N),
   limit(N,eval_ne(Eq,RetType,Depth,Self,E,R)).

eval_20(Eq,RetType,Depth,Self,['offset',NE,E],R):-  !,
   eval_args('=','Number',Depth,Self,NE,N),
   offset(N,eval_ne(Eq,RetType,Depth,Self,E,R)).

eval_20(Eq,RetType,Depth,Self,['max-time',NE,E],R):-  !,
   eval_args('=','Number',Depth,Self,NE,N),
   cwtl(N,eval_ne(Eq,RetType,Depth,Self,E,R)).


eval_20(Eq,RetType,Depth,Self,['call-cleanup',NE,E],R):-  !,
   call_cleanup(eval_args(Eq,RetType,Depth,Self,NE,R),
                eval_args(Eq,_U_,Depth,Self,E,_)).

% like call-cleanup but we might might avoid certain interupts durring setup
eval_20(Eq,RetType,Depth,Self,['setup-call-cleanup',S,NE,E],R):-  !,
   sig_atomic_no_cut(eval_args(Eq,_,Depth,Self,S,_)),
   call_cleanup(eval_args(Eq,RetType,Depth,Self,NE,R),
                eval_args(Eq,_U_,Depth,Self,E,_)).


eval_20(Eq,RetType,Depth,Self,['with-output-to',S,NE],R):-  !,
   eval_args(Eq,'Sink',Depth,Self,S,OUT),
   with_output_to_stream(OUT,
      eval_args(Eq,RetType,Depth,Self,NE,R)).

eval_20(Eq,RetType,Depth,Self,[Excl|Rest],Res):-
 arg(_, v('catch!','throw!','number-of!','limit!','offset!','max-time!','findall!','setup-call-cleanup!','call-cleanup!','call-cleanup!','with-output-to!'), Excl),
 sub_atom(Excl,_,_,1,NoExcl),!,
 eval_20(Eq,RetType,Depth,Self,[NoExcl|Rest],Res).


%sig_atomic_no_cut(Goal):- sig_atomic(Goal).
sig_atomic_no_cut(Goal):- call(Goal).

% =================================================================
% =================================================================
% =================================================================
%  DATA FUNCTOR
% =================================================================
% =================================================================
% =================================================================
eval20_failked(Eq,RetType,Depth,Self,[V|VI],[V|VO]):-
    nonvar(V),is_metta_data_functor(V),is_list(VI),!,
    maplist(eval_args(Eq,RetType,Depth,Self),VI,VO).


% =================================================================
% =================================================================
% =================================================================
%  EVAL FAILED
% =================================================================
% =================================================================
% =================================================================

eval_failed(Depth,Self,T,TT):-
   eval_failed('=',_RetType,Depth,Self,T,TT).

finish_eval(Depth,Self,T,TT):-
   finish_eval('=',_RetType,Depth,Self,T,TT).

eval_failed(Eq,RetType,Depth,Self,T,TT):-
  finish_eval(Eq,RetType,Depth,Self,T,TT).

%finish_eval(Eq,RetType,_,_,X,X):-!.

finish_eval(_Eq,_RetType,_Dpth,_Slf,T,TT):- var(T),!,TT=T.
finish_eval(_Eq,_RetType,_Dpth,_Slf,[],[]):-!.
finish_eval(Eq,RetType,Depth,Self,[F|LESS],Res):-
     once(eval_selfless(Eq,RetType,Depth,Self,[F|LESS],Res)),fake_notrace([F|LESS]\==Res),!.
%finish_eval(Eq,RetType,Depth,Self,[V|Nil],[O]):- Nil==[], once(eval_args(Eq,RetType,Depth,Self,V,O)),V\=@=!.
finish_eval(Eq,RetType,Depth,Self,[H|T],[HH|TT]):- !,
    eval_args(Depth,Self,H,HH),
    finish_eval(Eq,RetType,Depth,Self,T,TT).
finish_eval(_,_,_Depth,_Self,T,T):-!.
%finish_eval(_Eq,_RetType,Depth,Self,T,TT):- eval_args(Depth,Self,T,TT).

   %eval_args(Eq,RetType,Depth,Self,X,Y):- eval_20(Eq,RetType,Depth,Self,X,Y)*->true;Y=[].

%eval_20(Eq,RetType,Depth,_,_,_):- Depth<1,!,fail.
%eval_20(Eq,RetType,Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).
%eval_20(Eq,RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

% Kills zero arity functions eval_20(Eq,RetType,Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval_args(Eq,RetType,Depth,Self,X,Y).



% =================================================================
% =================================================================
% =================================================================
%  METTLOG PREDEFS
% =================================================================
% =================================================================
% =================================================================


eval_20(Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ResL).
eval_20(Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1,ArgL2],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ResL).
eval_20(Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1,ArgL2,ArgL3],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ResL).
eval_20(Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1,ArgL2,ArgL3,ArgL4],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ArgL4,ResL).
eval_20(Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1,ArgL2,ArgL3,ArgL4,ArgL5],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ArgL4,ArgL5,ResL).

  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Res):-
      eval_args(Eq,RetType,Depth,Self,[Pred,Arg1],Res).
  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Arg2,Res):-
      eval_args(Eq,RetType,Depth,Self,[Pred,Arg1,Arg2],Res).
  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Arg2,Arg3,Res):-
      eval_args(Eq,RetType,Depth,Self,[Pred,Arg1,Arg2,Arg3],Res).
  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Arg2,Arg3,Arg4,Res):-
      eval_args(Eq,RetType,Depth,Self,[Pred,Arg1,Arg2,Arg3,Arg4],Res).
  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Arg2,Arg3,Arg4,Arg5,Res):-
      eval_args(Eq,RetType,Depth,Self,[Pred,Arg1,Arg2,Arg3,Arg4,Arg5],Res).


eval_20(Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1],ResL):- !,
      metta_concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ResL).
eval_20(Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1,ArgL2],ResL):- !,
      concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ResL).
eval_20(Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1,ArgL2,ArgL3],ResL):- !,
    metta_concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ResL).
eval_20(Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1,ArgL2,ArgL3,ArgL4],ResL):- !,
    metta_concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ArgL4,ResL).
eval_20(Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1,ArgL2,ArgL3,ArgL4,ArgL5],ResL):- !,
    metta_concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ArgL4,ArgL5,ResL).


eval_20(Eq,RetType,Depth,Self,['concurrent-forall!',Gen,Test|Options],NoResult):- !,
      maplist(s2p,Options,POptions),
      call(thread:concurrent_forall(
            user:eval_ne(Eq,RetType,Depth,Self,Gen,_),
            user:forall(eval_args(Eq,RetType,Depth,Self,Test,_),true),
            POptions)),
     make_nop(RetType,[],NoResult).

eval_20(Eq,RetType,Depth,Self,['hyperpose',ArgL],Res):- !, metta_hyperpose(Eq,RetType,Depth,Self,ArgL,Res).


% =================================================================
% =================================================================
% =================================================================
%  METTALOG COMPILER PREDEFS
% =================================================================
% =================================================================
% =================================================================

%/* TODO: this should take into account the compilation prefix but
eval_20(_Eq,_RetType,_Dpth,_Slf,['current-predicate-arity',F],A):-
% These two are no longer strictly compiler redefinitions - the compiler
% predicates should be predicate/function-arity (not "current"), for
% arities explicitly declared by the user. This pair of predicates
% should instead handle deduced arities of functions defined but without
% an explicit arity declaration.
  !,
  eval_for('Symbol',F,FF),
  current_predicate_arity(FF,A).
eval_20(_Eq,_RetType,_Dpth,_Slf,['current-function-arity',F],A):-
  !,
  eval_for('Symbol',F,FF),
  current_function_arity(FF,A).
%*/

/* TODO: This could work but the prefixed prdicate/function is not found.
eval_20(_Eq,_RetType,_Dpth,_Slf,['current-predicate-arity',F],A):-
  !,
  eval_for('Symbol',F,FF),
  transpile_prefix(P),
  atom_concat(P,FF,FF_mc),
  current_predicate_arity(FF_mc,A).
eval_20(_Eq,_RetType,_Dpth,_Slf,['current-function-arity',F],A):-
  !,
  eval_for('Symbol',F,FF),
  transpile_prefix(P),
  atom_concat(P,FF,FF_mc),
  current_function_arity(FF_mc,A).
*/

current_predicate_arity(F,A):-
  metta_atom('&self',[:,F,[->|Args]]),
  !,
  length(Args,A).
current_predicate_arity(F,A):-
  current_predicate(F/A).

current_function_arity(F,A):-
  current_predicate_arity(F,PA)
  ,A is PA - 1.



eval_20(_Eq,_RetType,_Depth,_Self,['compile-space!'],Res):- !,
    as_nop('compile-space!'(_), Res).

eval_20(_Eq,_RetType,_Depth,_Self,['compile-space!',Space],Res):- !,
    as_nop('compile-space!'(Space), Res).

'compile-space!'(X,TF):-
   as_tf('compile-space!'(X), TF).

'compile-space!'(KB):-
    load_ontology,
    %((ignore(pfcRemove(do_compile_space(X))),
   % pfcWatch,
    pfcAdd_Now(do_compile_space(KB)),
    forall(function_arity(KB,F,_Len),'compile!'(F)),
    % pfcNoWatch,
    true,!.


eval_20(_Eq,_RetType,_Depth,_Self,['compile!'],Res):- !,
    as_nop('compile!'(_), Res).

eval_20(_Eq,_RetType,_Depth,_Self,['compile!',Space],Res):- !,
    as_nop('compile!'(Space), Res).

'compile!'(X,TF):-
   as_tf('compile!'(X), TF).

'compile!'(X):- X=='S',!.
'compile!'(X):-
    load_ontology,
    current_self(KB),
    %((ignore(pfcRemove(do_compile(KB,X,_))),
   % pfcWatch,
    pfcAdd_Now(do_compile(KB,X,_)),
    if_t( \+ current_predicate(X/_),
       forall(metta_defn(KB,[X | Args] ,BodyFn),
       compile_metta_defn(KB,X,Len,Args,BodyFn,_Clause))),
    if_t( \+ current_predicate(X/_),
       (ignore(nortrace),forall(metta_defn(KB,[X | Args] ,BodyFn),
       (maybe_trace(unknown),compile_metta_defn(KB,X,Len,Args,BodyFn,_ClauseU))))),
    % pfcNoWatch,
    true,!,
     notrace(catch((write_src_uo(?-listing(X)),listing(X)),E,
    (!,write_src(E),fail))),!.

compile_metta_defn(_KB,Op,Len,Args,BodyFn, ClauseU):-
   len_or_unbound(Args,Len),
   compile_for_assert([Op|Args], BodyFn, ClauseU).

%empty('Empty').
%','(A,B,(AA,BB)):- eval_args(A,AA),eval_args(B,BB).
%':'(A,B,[':',A,B]).
'<'(A,B,TFO):- as_tf(A<B,TF),!,TF=TFO.
'>'(A,B,TFO):- as_tf(A<B,TF),!,TF=TFO.
minus(A,B,C):- plus(B,C,A).

user:pow(X,Y,Z):- quintus:pow(X,Y,Z).

eval_20(Eq,RetType,Depth,Self,[MettaPred|More],Res):-
    AE = MettaPred,
    metta_compiled_predicate(Self,AE,Len),
    len_or_unbound(More,Len),

  must_det_ll((
    current_predicate(AE/Arity),
    maplist(as_prolog_x(Depth,Self), More , Adjusted))),!,
    eval_201(Eq,RetType,Depth,Self,MettaPred,Adjusted,Arity,Len,Res),
    nonvar(Res),
      must_det_ll((check_returnval(Eq,RetType,Res))).


eval_201(_Eq,_RetType,_Depth,_Self,Pred,AdjustedM1,Arity,Len,Res):- Arity > Len,!,
    append(AdjustedM1,[Res],Adjusted),
    Call =.. [Pred|Adjusted],
    %indentq2(2,call_pl_rv(Call)),
    catch_warn(efbug(show_call,rtrace_on_error(Call))).

eval_201(_Eq,_RetType,_Depth,_Self,Pred,Adjusted,_Arity,_Len,Res):-
    Call =.. [Pred|Adjusted],
    %indentq2(2,call_pl_tf(Call)),
    catch_warn(efbug(show_call,eval_call(rtrace_on_error(Call),Res))).



% =================================================================
% =================================================================
% =================================================================
%  METTLOG EXTRA PREDEFS
% =================================================================
% =================================================================
% =================================================================

%eval_20(Eq,RetType,_Dpth,_Slf,['trace!',A],A):- !, format('~N'),fbug(A),format('~N').

eval_20(Eq,RetType,_Dpth,_Slf,List,YY):- is_list(List),maplist(self_eval,List),List=[H|_], \+ atom(H), !,Y=List,do_expander(Eq,RetType,Y,YY).

% Temporarily in this file
eval_20_disabled(Eq,_ListOfRetType,Depth,Self,['TupleConcat',A,B],OO):- fail, !,
    eval_args(Eq,RetType,Depth,Self,A,AA),
    eval_args(Eq,RetType,Depth,Self,B,BB),
    append(AA,BB,OO).

% Temporarily in this file
eval_20_disabled(Eq,OuterRetType,Depth,Self,['range',A,B],OO):- fail, (is_list(A);is_list(B)),
  ((eval_args(Eq,RetType,Depth,Self,A,AA),
    eval_args(Eq,RetType,Depth,Self,B,BB))),
    ((AA+BB)\=@=(A+B)),
    eval_20(Eq,OuterRetType,Depth,Self,['range',AA,BB],OO),!.


/*
  fromNumber(Var1,Var2):- var(Var1),var(Var2),!,
   freeze(Var1,fromNumber(Var1,Var2)),
   freeze(Var2,fromNumber(Var1,Var2)).
fromNumber(0,'Z'):-!.
fromNumber(N,['S',Nat]):- integer(N), M is N -1,!,fromNumber(M,Nat).

eval_20(Eq,RetType,Depth,Self,['fromNumber',NE],RetVal):- !,
   eval_args('=','Number',Depth,Self,NE,N),
    fromNumber(N,RetVal), check_returnval(Eq,RetType,RetVal).
*/

%% lazy_union(:P2, +E1_Call1, +E2_Call2, -E) is nondet.
%  - Performs a union operation using lazy evaluation
% Arguments:
%  - P2: Any arity 2 predicate
%  - E1^Call1: The first goal (Call1) generating elements (E1)
%  - E2^Call2: The second goal (Call2) generating elements (E2)
%  - E: The resulting element that is part of the union of the two sets
lazy_union(P2, E1^Call1, E2^Call2, E) :-
    % Step 1: Use lazy_findall/3 to declare that all elements satisfying Call1 are supposedly in List1
    lazy_findall(E1, Call1, List1),
    % Step 2: Use lazy_findall/3 to declare that all elements satisfying Call2 are supposedly in List2
    lazy_findall(E2, Call2, List2),
    % Step 3: Perform the union logic
    (   % Case 1: If E is a member of List1, include it in the result
        member(E, List1)
        % Case 2: Otherwise, check if E is a member of List2
        % Additionally, ensure that E does not already exist in List1
        ; (member(E, List2), \+ (member(E1, List1), call(P2, E1, E)))
    ).


variant_by_type(X,Y):- var(X),!,X==Y.
variant_by_type(X,Y):- X=@=Y.

call_as_p2(P2,X,Y):-
   once(eval([P2,X,Y],TF)),
   TF = 'True'.


eval_20(_Eq,_RetType,_Depth,_Self,['unique-atom',List],RetVal):- !,
   list_to_set(List,RetVal).

eval_20(Eq,RetType,Depth,Self,['unique',Eval],RetVal):- !,
   term_variables(Eval+RetVal,Vars),
   no_repeats_var(YY),
   eval_args(Eq,RetType,Depth,Self,Eval,RetVal),YY=Vars.

eval_20(Eq,RetType,Depth,Self,['unique-by',P2,Eval],RetVal):- !,
   no_repeats_var(call_as_p2(P2),YY),
   eval_args(Eq,RetType,Depth,Self,Eval,RetVal),YY=RetVal.


eval_20(_Eq,_RetType,_Depth,_Self,['subtraction-atom',List1,List2],RetVal):- !,
    exclude(is_in(variant_by_type,List2),List1,RetVal).

eval_20(Eq,RetType,Depth,Self,['subtraction',Eval1,Eval2],RetVal):- !,
    lazy_subtraction(variant_by_type,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['subtraction-by',P2,Eval1,Eval2],RetVal):- !,
    lazy_subtraction(call_as_p2(P2),RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(_Eq,_RetType,_Depth,_Self,['union-atom',List1,List2],RetVal):- !,
   exclude(is_in(variant_by_type,List2),List1,List1a), !,
    append(List1a,List2,RetVal).

eval_20(Eq,RetType,Depth,Self,['union',Eval1,Eval2],RetVal):- !,
    lazy_union(variant_by_type,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['union-by',P2,Eval1,Eval2],RetVal):- !,
    lazy_union(call_as_p2(P2),RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

%eval_20(Eq,RetType,_Dpth,_Slf,['py-list',Atom_list],CDR_Y):-
% !, Atom=[_|CDR],!,do_expander(Eq,RetType,Atom_list, CDR_Y ).

eval_20(_Eq,_RetType,_Depth,_Self,['intersection-atom',List1,List2],RetVal):-  !,
    include(is_in(variant_by_type,List2),List1,RetVal).

is_in(P2,List2,Item1):- \+ \+ (member(Item2,List2),call(P2,Item1,Item2)),!.

eval_20(Eq,RetType,Depth,Self,['intersection',Eval1,Eval2],RetVal):- !,
    lazy_intersection(variant_by_type,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['intersection-by',P2,Eval1,Eval2],RetVal):- !,
    lazy_intersection(call_as_p2(P2),RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

%% lazy_intersection(:P2, +E1_Call1, +E2_Call2, -E) is nondet.
%  - Performs a intersection operation using lazy evaluation.
%  - It intersects elements generated by Call2 from those generated by Call1.
% Arguments:
%  - P2: Any arity 2 predicate
%  - E1^Call1: The first goal (Call1) generating elements (E1).
%  - E2^Call2: The second goal (Call2) generating elements (E2).
%  - E: The resulting element after subtracting elements of the second set from the first set.
lazy_intersection(P2, E1^Call1, E2^Call2, E1) :-
    % Step 1: Evaluate Call1 to generate E1
    call(Call1),
    % Step 2: Use lazy_findall/3 to declare that all elements satisfying Call2 are supposedly in List2
    lazy_findall(E2, Call2, List2),
    % Step 3: Perform the intersection logic
    % Only return E1 if it is not a member of List2
    member(E2, List2), call(P2,E1,E2).


%% lazy_subtraction(:P2, +E1_Call1, +E2_Call2, -E) is nondet.
%  - Performs a subtraction operation using lazy evaluation.
%  - It subtracts elements generated by Call2 from those generated by Call1.
% Arguments:
%  - P2: Any arity 2 predicate
%  - E1^Call1: The first goal (Call1) generating elements (E1).
%  - E2^Call2: The second goal (Call2) generating elements (E2).
%  - E: The resulting element after subtracting elements of the second set from the first set.
lazy_subtraction(P2,E1^Call1, E2^Call2, E1) :-
    % Step 1: Evaluate Call1 to generate E1
    call(Call1),
    % Step 2: Use lazy_findall/3 to declare that all elements satisfying Call2 are supposedly in List2
    lazy_findall(E2, Call2, List2),
    % Step 3: Perform the subtraction logic
    % Only return E1 if it is not a member of List2
    \+ (member(E2, List2), call(P2, E1, E2)).


eval_20(Eq,RetType,Depth,Self,PredDecl,Res):-
  Do_more_defs = do_more_defs(true),
  clause(eval_21(Eq,RetType,Depth,Self,PredDecl,Res),Body),
  Do_more_defs == do_more_defs(true),
  call_ndet(Body,DET),
  nb_setarg(1,Do_more_defs,false),
 (DET==true -> ! ; true).

% Temporarily in this file
eval_21(_Eq,_RetType,_Depth,_Self,['fb-member',Res,List],TF):-!, as_tf(fb_member(Res,List),TF).
% Temporarily in this file
eval_21(_Eq,_RetType,_Depth,_Self,['fb-member',List],Res):-!, fb_member(Res,List).

% Temporarily in this file
eval_21(Eq,RetType,Depth,Self,['CollapseCardinality',List],Len):-!,
 findall_eval(Eq,RetType,Depth,Self,List,Res),
 length(Res,Len).
/*
eval_21(_Eq,_RetType,_Depth,_Self,['TupleCount', [N]],N):- number(N),!.
*/

% Temporarily in this file
eval_21_disabled(Eq,_RetType,Depth,Self,['Tuple-Count',List],Len):- fail,!,
 (\+ is_list(List)->findall_eval(Eq,_,Depth,Self,List,Res);Res=List),!,
 length(Res,Len).
% Temporarily in this file
eval_21(_Eq,_RetType,_Depth,_Self,['tuple-count',List],Len):-!,
 length(List,Len).


%eval_20(Eq,RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).

eval_20(_Eq,_OuterRetType,_Depth,_Self,[P,_,B],_):-P=='/',B==0,!,fail.

% Temporarily in this file
eval_20(Eq,RetType,Depth,Self,['CountElement',L],Res):- !, eval_args(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1),check_returnval(Eq,RetType,Res).
% Temporarily in this file
eval_20(Eq,RetType,_Dpth,_Slf,['make_list',List],MettaList):- !, into_metta_cons(List,MettaList),check_returnval(Eq,RetType,MettaList).

simple_math(Var):- attvar(Var),!,fail.
simple_math([F|XY]):- !, atom(F),atom_length(F,1), is_list(XY),maplist(simple_math,XY),!.
simple_math(X):- number(X),!.


eval_20(_Eq,_RetType,_Depth,_Self,['call-string!',Str],NoResult):- !,'call-string!'(Str,NoResult).

'call-string!'(Str,NoResult):-
               read_term_from_atom(Str,Term,[variables(Vars)]),!,
               call(Term),NoResult=Vars.


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
eval_40(Eq,RetType,_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
*/
% Macro Functions
%eval_20(Eq,RetType,Depth,_,_,_):- Depth<1,!,fail.
/*
eval_40(_Eq,_RetType,Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).
eval_40(Eq,RetType,Depth,Self,[F|PredDecl],Res):-
   fail,
   Depth>1,
   fake_notrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval_args(Eq,RetType,Depth,Self,SSub,Repl),
   fake_notrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   eval_args(Eq,RetType,Depth,Self,[F|Temp],Res).
*/
% =================================================================
% =================================================================
% =================================================================
%  PLUS/MINUS
% =================================================================
% =================================================================
% =================================================================
eval_40(Eq,RetType,Depth,Self,LESS,Res):-
   eval_selfless(Eq,RetType,Depth,Self,LESS,Res),fake_notrace(LESS\==Res),!.

/*
eval_40(Eq,RetType,Depth,Self,['+',N1,N2],N):- number(N1),!,
   skip_eval_args(Eq,RetType,Depth,Self,N2,N2Res), fake_notrace(catch_err(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
eval_40(Eq,RetType,Depth,Self,['-',N1,N2],N):- number(N1),!,
   skip_eval_args(Eq,RetType,Depth,Self,N2,N2Res), fake_notrace(catch_err(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
eval_40(Eq,RetType,Depth,Self,['*',N1,N2],N):- number(N1),!,
   skip_eval_args(Eq,RetType,Depth,Self,N2,N2Res),
   fake_notrace(catch_err(N is N1*N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
*/

eval_20(_Eq,_RetType,_Depth,_Self,['rust',Bang,PredDecl],Res):- Bang == '!', !,
    rust_metta_run(exec(PredDecl),Res), nop(write_src(res(Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['rust!',PredDecl],Res):- !,
    rust_metta_run(exec(PredDecl),Res), nop(write_src(res(Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['rust',PredDecl],Res):- !,
    rust_metta_run((PredDecl),Res), nop(write_src(res(Res))).


%skip_eval_args(Eq,RetType,Depth,Self,LESS,Res):- eval_args(Eq,RetType,Depth,Self,LESS,Res).
skip_eval_args(_Eq,_RetType,_Depth,_Self,LESS,Res):- LESS=Res.

%eval_20(_Eq,_RetType,_Depth,_Self,['py-list',Arg],Res):- !, must_det_ll((py_list(Arg,Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['py-dict',Arg],Res):- !,
  must_det_ll((py_dict(Arg,Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['py-list',Arg],Res):- !,
  must_det_ll((py_list(Arg,Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['py-tuple',Arg],Res):- !,
  must_det_ll((py_tuple(Arg,Res))).

eval_40(_Eq,_RetType,_Depth,_Self,['py-chain',Arg],Res):- !,
  must_det_ll((py_chain(Arg,Res))).

eval_40(Eq,RetType,Depth, Self,['py-atom'|Args],Res):- !,
    eval_py_atom(Eq,RetType,Depth,Self,['py-atom'|Args],Res).

eval_40(_Eq,_RetType,_Depth,_Self,['py-dot',Arg1,Arg2| _Specialize],Res):- !,
  make_py_dot(Arg1,Arg2,Res).
eval_40(_Eq,_RetType,_Depth,_Self,['py-type',Arg],Res):- !,
  must_det_ll((py_type(Arg,Res))).
eval_40(_Eq,_RetType,_Depth,_Self,['py-eval',Arg],Res):- !,
  must_det_ll((py_eval_string(Arg,Res))).

eval_40(Eq,RetType,Depth,Self,['length',L],Res):- !, eval_args(Depth,Self,L,LL),
   (is_list(LL)->length(LL,Res);Res=1),
   check_returnval(Eq,RetType,Res).



eval_20_disabled(Eq,RetType,Depth,Self,[X|Rest],YL):- is_list(Rest), is_list(X),!,
   eval_args(Eq,RetType,Depth,Self,X,Y),
   ((X\=@=Y,atom(Y)) -> eval_args(Eq,RetType,Depth,Self,[Y|Rest],YL)
     ; ((maplist(eval_args(Eq,RetType,Depth,Self),Rest,YRest),YL=[Y|YRest]))).

/*
eval_40(Eq,RetType,Depth,Self,[P,A,X|More],YY):- is_list(X),X=[_,_,_],simple_math(X),
   eval_selfless_2(X,XX),X\=@=XX,!,
   eval_40(Eq,RetType,Depth,Self,[P,A,XX|More],YY).
*/
%eval_40(Eq,RetType,_Dpth,_Slf,['==',X,Y],Res):-  !, subst_args(Eq,RetType,_Dpth,_Slf,['==',X,Y],Res).

eval_20(_Eq,_RetType,_Depth,_Self,['==', X,Y],TF):- (var(X) , var(Y)), !, as_tf(X==Y,TF),!.
eval_40(_Eq,_RetType,_Depth,_Self,['==', X,Y],TF):- (var(X) , var(Y)), !, as_tf(X==Y,TF),!.

eval_40(Eq,RetType,Depth,Self,['==',X,Y],TF):- !,
  ignore(get_operator_typedef(Self,'==',2,[XType,YType],RetType)),
  eval_until_eq_tf(['=='],Eq, XType, YType, Depth,Self, X, Y, TF).

eval_20(Eq,RetType,Depth,Self,_Slf,['===',X,Y],TF):- !,
    suggest_type(RetType,'Bool'),
    as_tf(\+ \+ eval_until_eq(['==='],Eq,_SharedType,Depth,Self,X,Y), TF).

eval_20(_Eq,RetType,_Dpth,_Slf,['====',X,Y],TF):- !,
    suggest_type(RetType,'Bool'),
    as_tf(same_terms(X,Y),TF).

% Main evaluation predicate with full caching

transpiler_peek(Sym,Len,Type,Fn):-
  transpiler_predicate_store(_, Sym, [Len], _, _, _, _),
  if_t(var(Type),member(Type,['mx','mi','mc'])),
  format(atom(Fn),'~w__1_~w_~w',[Type,Len,Sym]),
  succ(Len,LenP1), current_predicate(Fn/LenP1).

eval_20(Eq, RetType, Depth, Self, [Sym | Args], Res) :- symbol(Sym), is_list(Args),
    length(Args, Len),
    memoize_tf(transpiler_peek(Sym,Len,'mi',Fn)),
    append(Args, [Res], PArgs),!,
    with_metta_ctx(Eq, RetType, Depth, Self, [Sym | Args], apply(Fn, PArgs)).

eval_20(Eq, RetType, Depth, Self, [Sym | Args], Res) :- symbol(Sym), is_list(Args),
    length(Args, Len),
    memoize_tf(transpiler_peek(Sym,Len,'mx',Fn)),
    append(Args, [Res], PArgs),!,
    with_metta_ctx(Eq, RetType, Depth, Self, [Sym | Args], apply(Fn, PArgs)).

eval_40(Eq,RetType,Depth,Self,[Sym|Args],Res):- symbol(Sym), is_list(Args),
    length(Args,Len),
    memoize_tf(transpiler_peek(Sym,Len,'mc',Fn)),
    append(Args,[Res],PArgs),!,
    with_metta_ctx(Eq,RetType,Depth,Self,[Sym|Args],apply(Fn,PArgs)).



with_metta_ctx(_Eq,_RetType,_Depth,_Self,_MeTTaSrc,apply(Fn,PArgs)):- !, apply(Fn,PArgs).
with_metta_ctx(_Eq,_RetType,_Depth,_Self,_MeTTaSrc,Goal):-  Goal.

:- dynamic memoized_result/3.
memoize_tf(Goal) :-
    term_variables(Goal, Vars),
    copy_term(Goal, CopyGoal),numbervars(CopyGoal,0,_,[attvar(bind)]),
    (   memoized_result(CopyGoal, Vars, Result) ->
        (Result == fail -> fail ; true)
    ;   (   call(Goal) ->
            assertz(memoized_result(CopyGoal, Vars, true))
        ;   assertz(memoized_result(CopyGoal, Vars, fail)),
            fail
        )
    ).


suggest_type(_RetType,_Bool).

naive_eval_args:-
    false.

eval_all_args:- false, true_flag.
fail_missed_defn:- false, true_flag.
fail_on_constructor:- true_flag.

old_sys:- fail.

eval_adjust_args(_Eq,_RetType,ResIn,ResOut,_Depth,_Self,AEMore,AEAdjusted):-
   \+ iz_conz(AEMore),!,AEMore=AEAdjusted,ResIn=ResOut,!.

eval_adjust_args(Eq,RetType,ResIn,ResOut,Depth,Self,[AIn|More],[AE|Adjusted]):-
  (eval_args(Eq, _, Depth, Self, AIn, AE) *-> true ; AIn=AE),
  adjust_args_90(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted).

cache_arrow_types(AE,Len):- arg_type_n(AE,Len,_,_),!.
cache_arrow_types(AE,Len):-
  forall((metta_type(_KB,AE,Arrow),arrow_type(Arrow,Args,_Ret),length(Args,Len)),
    forall(between(1,Len,N),
     ignore((nth1(N,Args,Type),maybe_non_eval(AE,Len,N,Type))))).
  %arg_type_n(AE,Len,_,_)

:- dynamic(arg_type_n/4).

maybe_non_eval(AE,Len,N,Type):- var(Type),!, assert(arg_type_n(AE,Len,N,var)),!.
    maybe_non_eval(AE,Len,N,Type):- once(sub_var('Atom',Type);sub_var('Expression',Type)),!,
       assert(arg_type_n(AE,Len,N,non_eval(Type))),!.
maybe_non_eval(AE,Len,N,Type):- assert(arg_type_n(AE,Len,N,eval(Type))),!.


do_each_arg(_Eq, _RetType, _ResIn, _Depth, _Self, _AE, _Len, _N, [], []):-!.
do_each_arg(_Eq, _RetType, _ResIn, _Depth, _Self, _AE, Len, N, _, []) :- N > Len, !.
do_each_arg(Eq, RetType, ResIn, Depth, Self, AE, Len, N, [In|More], [Out|Adjusted]) :-
    do_arg(Eq, RetType, ResIn, Depth, Self, AE, Len, N, In, Out), succ(N, Np1),
    do_each_arg(Eq, RetType, ResIn, Depth, Self, AE, Len, Np1, More, Adjusted).

do_arg(_Eq, _RetType, _RsIn, _Dpth, _Slf, AE, Len, N, InOut, InOut) :- nonvar(AE), arg_type_n(AE, Len, N, no_eval(_)), !.
do_arg( Eq, _RetType, _RsIn, Depth, Self, _AE, _Len, _N, In, Out) :-
  eval_args(Eq, _, Depth, Self, In, Out).



adjust_args_90(Eq,RetType,ResIn,ResIn,Depth,Self,AE,More,Adjusted):- \+ old_sys,
   length(More,Len), cache_arrow_types(AE,Len), !,
   do_each_arg(Eq,RetType,ResIn,Depth,Self,AE,Len,1,More,Adjusted).

adjust_args_90(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted):- \+ is_debugging(argtypes),!,
    adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted).
adjust_args_90(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted):-
   if_or_else(adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted),
      if_or_else(with_debug(eval_args,adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted),
             if_or_else(More=Adjusted,
                if_or_else((maybe_trace(unknown), throw(adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted)))))))).



eval_adjust_args2(Eq,_RetType,ResIn,ResOut,Depth,Self,[AE|More],[AE|Adjusted]):-
   maplist(must_eval_args(Eq,_,Depth,Self),More,Adjusted),
   ResIn = ResOut.



must_eval_args(Eq,RetType,Depth,Self,More,Adjusted):- \+ is_debugging(e),!, eval_args(Eq,RetType,Depth,Self,More,Adjusted).
must_eval_args(Eq,RetType,Depth,Self,More,Adjusted):-
   (eval_args(Eq,RetType,Depth,Self,More,Adjusted)*->true;
      (with_debug(eval_args,eval_args(Eq,RetType,Depth,Self,More,Adjusted))*-> true;
         (
           %nl,writeq(eval_args(Eq,RetType,Depth,Self,More,Adjusted)),writeln('.'),
             (More=Adjusted -> true ;
                (maybe_trace(unknown), throw(must_eval_args(Eq,RetType,Depth,Self,More,Adjusted))))))).


maybe_eval_subst(Eq,RetType,Depth,Self,PredDecl,Res):- !,
  subst_args_here(Eq,RetType,Depth,Self,PredDecl,Res).


maybe_eval_subst(_Eq,_RetType,_Dpth,_Slf,[H|PredDecl],Res):- fail,
  is_rust_operation([H|PredDecl]),!, % run
  must_det_ll((rust_metta_run(exec([H|PredDecl]),Res),
  nop(write_src(res(Res))))).

maybe_eval_subst(_Eq,_RetType,_Dpth,_Slf,Res,Res):- nb_current(maybe_eval_subst,false),!.
maybe_eval_subst(Eq,RetType,Depth,Self,PredDecl,Res):-
  locally(b_setval(maybe_eval_subst,false),
   finish_eval(Eq,RetType,Depth,Self,PredDecl,Res)).

:- nb_setval(maybe_eval_subst,true).
/*
maybe_eval_subst(Eq,RetType,Depth,Self,PredDecl,Res):-
   if_or_else((finish_eval(Eq,RetType,Depth,Self,PredDecl,Res),
       PredDec\=@=Res),
       subst_args(Eq,RetType,Depth,Self,PredDecl,Res)).
*/

/*
maybe_eval_subst(Eq,RetType,Depth,Self,PredDecl,Res):-
  Do_more_defs = do_more_defs(true),
  clause(eval_80(Eq,RetType,Depth,Self,PredDecl,Res),Body),
  Do_more_defs == do_more_defs(true),
  call_ndet(Body,DET),
  nb_setarg(1,Do_more_defs,false),
 (DET==true -> ! ; true).
*/
% =================================================================
% =================================================================
% =================================================================
% inherited by system
% =================================================================
% =================================================================
% =================================================================
is_system_pred(S):- atom(S),atom_concat(_,'!',S),!.
is_system_pred(S):- atom(S),atom_concat(_,'-fn',S),!.
is_system_pred(S):- atom(S),atom_concat(_,'-p',S).
%is_system_pred(S):- atom(S),upcase_symbol(S,U),downcase_symbol(S,U).

% maybe_eval_python/6: Evaluates a Python function call within MeTTa.
% Parameters:
% - Eq: denotes get-type, match, or interpret call.
% - RetType: Expected return type of the MeTTa function.
% - Depth: Recursion depth or complexity control.
% - Self: Context or environment for the evaluation.
% - [MyFun|More]: List with MeTTa function and additional arguments.
% - RetVal: Variable to store the result of the Python function call.
eval_40(Eq, RetType, _Depth, Self, [MyFun|More], RetVal) :-
    % MyFun as a registered Python function with its module and function name.
    metta_atom(Self, ['registered-python-function', PyModule, PyFun, MyFun]),!,
    % Tries to fetch the type definition for MyFun, ignoring failures.
    %adjust_args_9(Eq,RetType,MVal,RetVal,Depth,Self,MyFun,More,Adjusted),
    More=Adjusted,MVal=RetVal,
    % Constructs a compound term for the Python function call with adjusted arguments.
    compound_name_arguments(Call, PyFun, Adjusted),
    % Optionally prints a debug tree of the Python call if tracing is enabled.
    if_trace(host;python, print_tree(py_call(PyModule:Call, RetVal))),
    % Executes the Python function call and captures the result in MVal which propagates to RetVal.
    py_call(PyModule:Call, MVal),
    % Checks the return value against the expected type and criteria.
    check_returnval(Eq, RetType, RetVal).


%eval_80(_Eq,_RetType,_Dpth,_Slf,LESS,Res):- fake_notrace((once((eval_selfless(LESS,Res),fake_notrace(LESS\==Res))))),!.

is_host_predicate([AE|More],Pred,Len):-
    is_system_pred(AE),
    length(More,Len),
    is_syspred(AE,Len,Pred),
    \+ (atom(AE),   atom_concat(_,'-fn',AE)).

% predicate inherited by system
eval_40(Eq,RetType,Depth,Self,[AE|More],TF):- allow_host_functions,
  is_host_predicate([AE|More],Pred,Len),
  current_predicate(Pred/Len),!,
  %fake_notrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  % adjust_args(Depth,Self,AE,More,Adjusted),
  maplist(as_prolog_x(Depth,Self), More , Adjusted),
  if_trace(host;prolog;e,print_tree(apply(Pred,Adjusted))),
  with_metta_ctx(Eq,RetType,Depth,Self,[AE|More],catch_warn(efbug(show_call,eval_call(apply(Pred,Adjusted),TF)))),
  check_returnval(Eq,RetType,TF).

show_ndet(G):- call(G).
%show_ndet(G):- call_ndet(G,DET),(DET==true -> ! ; fbug(show_ndet(G))).

:- if( \+  current_predicate( adjust_args / 2 )).

   :- discontiguous eval_80/6.

is_user_defined_goal(Self,Head):-
  is_user_defined_head(Self,Head).

:- endif.

adjust_args_mp(_Eq,_RetType,Res,Res,_Depth,_Self,_Pred,_Len,_AE,Args,Adjusted):- Args==[],!,Adjusted=Args.
adjust_args_mp(Eq,RetType,Res,NewRes,Depth,Self,Pred,Len,AE,Args,Adjusted):-

   functor(P,Pred,Len),
   predicate_property(P,meta_predicate(Needs)),
   account_needs(1,Needs,Args,More),!,
   adjust_args(Eq,RetType,Res,NewRes,Depth,Self,AE,More,Adjusted).
adjust_args_mp(Eq,RetType,Res,NewRes,Depth,Self,_Pred,_Len,AE,Args,Adjusted):-
   adjust_args(Eq,RetType,Res,NewRes,Depth,Self,AE,Args,Adjusted).

acct(0,A,call(eval_args(A,_))).
acct(':',A,call(eval_args(A,_))).
acct(_,A,A).
account_needs(_,_,[],[]).
account_needs(N,Needs,[A|Args],[M|More]):- arg(N,Needs,What),!,
   acct(What,A,M),plus(1,N,NP1),
   account_needs(NP1,Needs,Args,More).

:- nodebug(metta(call)).
allow_host_functions.

s2ps(S,P):- current_self(Self),s2ps(30,Self,S,P).

s2ps(_,_,_Self,S,P):- S=='Nil',!,P=[].
s2ps(D,Self,S,P):- \+ is_list(S),!,as_prolog_x(D,Self,S, P).
s2ps(D,Self,[F|S],P):- atom(F),!,maplist(as_prolog_x(D,Self),S,SS),join_s2ps(F,SS,P).
s2ps(D,Self,[F|S],P):- is_list(F),!,maplist(as_prolog_x(D,Self),[F|S],SS),join_s2ps(call,SS,P).
%s2ps(D,Self,S,P):- is_list(F),maplist(s2ps,[F|S],SS),join_s2ps(call,SS,P),!.
s2ps(D,Self,S,P):- as_prolog_x(D,Self,S, P).

join_s2ps('Cons',[H,T],[H|T]):-!.
join_s2ps(F,Args,P):-atom(F),P=..[F|Args].

eval_call(S,TF):-
  s2ps(S,P), !, fbug_eval(eval_call(P,'$VAR'('TF'))),
  as_tf_traceable(P,TF).

eval_call_fn(S,R):-
  s2ps(S,P), !, fbug_eval(eval_call_fn(P,'$VAR'('R'))),
  as_tf_traceable(call(P,R),TF),TF\=='False'.

fbug_eval(G):- if_trace(e,fbug(G)).

is_host_function([AE|More],Pred,Len):-
  is_system_pred(AE),
  length([AE|More],Len),
  is_syspred(AE,Len,Pred),
  \+ (symbol(AE), symbol_concat(_,'-p',AE)). % thus maybe -fn or !

% function inherited from system
eval_40(Eq,RetType,Depth,Self,[AE|More],Res):- allow_host_functions,
  is_host_function([AE|More],Pred,Len),  % thus maybe -fn or !
  Len1 is Len+1, current_predicate(Pred/Len1), !,
  %fake_notrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  %adjust_args(Depth,Self,AE,More,Adjusted),!,
  %Len1 is Len+1,
  %current_predicate(Pred/Len1),
  maplist(as_prolog_x(Depth,Self),More,Adjusted),
  append(Adjusted,[Res],Args),!,
  if_trace(host;prolog,print_tree(apply(Pred,Args))),
  with_metta_ctx(Eq,RetType,Depth,Self,[AE|More],efbug(show_call,catch_warn(apply(Pred,Args)))),
  check_returnval(Eq,RetType,Res).

% user defined function
%eval_20(Eq,RetType,Depth,Self,[H|PredDecl],Res):-
 %  fake_notrace(is_user_defined_head(Self,H)),!,
 %  maybe_eval_defn(Eq,RetType,Depth,Self,[H|PredDecl],Res).


:- if( \+  current_predicate( check_returnval / 3 )).
check_returnval(_,_RetType,_TF).
:- endif.

:- if( \+  current_predicate( adjust_args / 5 )).
adjust_args(_Depth,_Self,_V,VI,VI).
:- endif.


last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.



%catch_err(G,E,C):- catch(G,E,(always_rethrow(E)->(throw(E));C)).
catch_warn(G):- (catch_err(G,E,(fbug(catch_warn(G)-->E),fail))).
catch_nowarn(G):- catch(G,E,(always_rethrow(E)->(throw(E)),fail)).


% less Macro-ey Functions

%Metta
as_nop([]).
%mettalog
%as_nop('Empty').

as_nop(G,NoResult):-  G\=[_|_], rtrace_on_failure(G),!,
  as_nop(NoResult).

as_tf(G,TF):-  G\=[_|_], catch_warn((call(G)*->TF='True';TF='False')).
as_tf_nowarn(G,TF):-  G\=[_|_], catch_nowarn((call(G)*->TF='True';TF='False')).
as_tf_traceable(G,TF):-  G\=[_|_], ((catch(G,E,((maybe_trace(unknown),writeln(E),rtrace(G),!,throw(E))))*->TF='True';TF='False')).
as_tf_tracabe(G,TF):-  G\=[_|_], ((call(G)*->TF='True';TF='False')).
%eval_selfless_1(['==',X,Y],TF):- as_tf(X=:=Y,TF),!.
%eval_selfless_1(['==',X,Y],TF):- as_tf(X=@=Y,TF),!.

is_assignment(V):- \+ atom(V),!, fail.
is_assignment('is'). is_assignment('is!').
%is_assignment('=').
%is_assignment('==').
%is_assignment('=:=').  is_assignment(':=').

eval_selfless(_Eq,_RetType,_Depth,_Self,E,R):-  eval_selfless_0(E,R).
eval_selfless(E,R):-  eval_selfless_0(E,R).

eval_selfless_0([F|_],_):- var(F),!,fail.
eval_selfless_0([F,X,XY],TF):- is_assignment(F),  fake_notrace(args_to_mathlib([X,XY],Lib)),!,eval_selfless3(Lib,['=',X,XY],TF).
eval_selfless_0([F|XY],TF):- eval_selfless_1([F|XY],TF),!.
eval_selfless_0(E,R):- eval_selfless_2(E,R).

allow_clp:- false_flag.

eval_selfless_1([F|XY],TF):- allow_clp, \+ ground(XY),!,fake_notrace(args_to_mathlib(XY,Lib)),!,eval_selfless3(Lib,[F|XY],TF).
eval_selfless_1(['>',X,Y],TF):-!,as_tf_nowarn(X>Y,TF).
eval_selfless_1(['<',X,Y],TF):-!,as_tf_nowarn(X<Y,TF).
eval_selfless_1(['=>',X,Y],TF):-!,as_tf_nowarn(X>=Y,TF).
eval_selfless_1(['<=',X,Y],TF):-!,as_tf_nowarn(X=<Y,TF).
eval_selfless_1(['\\=',X,Y],TF):-!,as_tf(dif(X,Y),TF).

eval_selfless_2([F|_],_):- var(F),!,fail.
eval_selfless_2(['%',X,Y],TF):-!,eval_selfless_2(['mod',X,Y],TF).
eval_selfless_2(LIS,Y):-  fake_notrace(( ground(LIS),
   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   LIS\=[_], s2ps(LIS,IS))), fake_notrace(catch((Y is IS),_,fail)),!.


% 'State'(4,'Number')
% ['State',4,'Number']
% 'State'(8,'Number')


eval_selfless3(Lib,FArgs,TF):- maplist(s2ps,FArgs,Next),!,
   compare_selfless0(Lib,Next,TF).

:- use_module(library(clpfd)).
:- clpq:use_module(library(clpq)).
:- clpr:use_module(library(clpr)).

compare_selfless0(_,[F|_],_TF):- \+ atom(F),!,fail.
%compare_selfless0(clpfd,['=',X,Y],TF):-!,as_tf(X#=Y,TF).
compare_selfless0(clpfd,['\\=',X,Y],TF):-!,as_tf(X #\=Y,TF).
%compare_selfless0(clpfd,['>',X,Y],TF):-!,as_tf(X#>Y,TF).
%compare_selfless0(clpfd,['<',X,Y],TF):-!,as_tf(X#<Y,TF).
compare_selfless0(clpfd,['=>',X,Y],TF):-!,as_tf(X#>=Y,TF).
compare_selfless0(clpfd,['<=',X,Y],TF):-!,as_tf(X#=<Y,TF).
%compare_selfless0(clpfd,[F|Stuff],TF):- atom_concat('#',F,SharpF),P=..[SharpF|Stuff],!,as_tf(P,TF).
compare_selfless0(Lib,_,_):- Lib == clpfd,!,fail.
compare_selfless0(Lib,['\\=',X,Y],TF):-!,as_tf(Lib:{X \=Y}, TF).
compare_selfless0(Lib,['=',X,Y],TF):-!,as_tf(Lib:{X =Y}, TF).
compare_selfless0(Lib,['>',X,Y],TF):-!,as_tf(Lib:{X>Y},TF).
compare_selfless0(Lib,['<',X,Y],TF):-!,as_tf(Lib:{X<Y},TF).
compare_selfless0(Lib,['=>',X,Y],TF):-!,as_tf(Lib:{X>=Y},TF).
compare_selfless0(Lib,['<=',X,Y],TF):-!,as_tf(Lib:{X=<Y},TF).
compare_selfless0(Lib,[F|Stuff],TF):- P=..[F|Stuff],!,as_tf(Lib:{P},TF).

args_to_mathlib(XY,Lib):- sub_term_safely(T,XY), var(T),get_attrs(T,XX),get_attrlib(XX,Lib),!.
args_to_mathlib(XY,clpq):- once((sub_term_safely(Rat,XY),compound(Rat),Rat='/'(_,_))),!.
args_to_mathlib(XY,clpr):- once((sub_term_safely(T,XY), float(T))),!. % Reals
args_to_mathlib(_,clpfd).


get_attrlib(XX,clpfd):- sub_var_safely(clpfd,XX),!.
get_attrlib(XX,clpq):- sub_var_safely(clpq,XX),!.
get_attrlib(XX,clpr):- sub_var_safely(clpr,XX),!.

% =================================================================
% =================================================================
% =================================================================
%  USER DEFINED FUNCTIONS
% =================================================================
% =================================================================
% =================================================================

call_ndet(Goal,DET):- call(Goal),deterministic(DET),(DET==true->!;true).



:- dynamic(is_metta_type_constructor/3).

curried_arity(X,_,_):- var(X),!,fail.
curried_arity([F|T],F,A):-var(F),!,fail,len_or_unbound(T,A).
curried_arity([[F|T1]|T2],F,A):- nonvar(F),!,len_or_unbound(T1,A1),
  (var(A1)->A=A1;(len_or_unbound(T2,A2),(var(A2)->A=A2;A is A1+A2))).
curried_arity([F|T],F,A):-len_or_unbound(T,A).

%curried_arity(_,_,_).


len_or_unbound(T,A):- is_list(T),!,length(T,A).
len_or_unbound(T,A):- integer(A),!,length(T,A).
len_or_unbound(_,_).



eval_constructor(Eq,RetType,Depth,Self,X,Res):-
   maybe_eval_subst(Eq,RetType,Depth,Self,X,Res).


multiple_typesigs(TypesSet):- is_list(TypesSet),
   length(TypesSet,Len),Len>1,maplist(is_list,TypesSet),!.

/*
eval_defn_bodies(Eq,RetType,Depth,Self,X,Res,[]):- !,
   maybe_eval_subst(Eq,RetType,Depth,Self,X,Res).
eval_defn_bodies(Eq,RetType,Depth,Self,X,Res,[]):- !,
   \+ \+ ignore((curried_arity(X,F,A),assert(is_metta_type_constructor(Self,F,A)))),!,
   if_trace(e,color_g_mesg('#773700',indentq2(Depth,defs_none(X)))),!,
   \+ fail_on_constructor,
   eval_constructor(Eq,RetType,Depth,Self,X,Res).
*/

get_defn_expansions_guarded(Eq,RetType,Depth,Self,ParamTypes,FRetType,HArgs,RW,Y):-
  if_or_else(get_defn_expansions_guardedA(Eq,RetType,Depth,Self,ParamTypes,FRetType,HArgs,RW,Y),
  if_or_else(get_defn_expansions_guardedB(Eq,RetType,Depth,Self,ParamTypes,FRetType,HArgs,RW,Y),
             get_defn_expansions_guardedC(Eq,RetType,Depth,Self,ParamTypes,FRetType,HArgs,RW,Y))).

% type signatiure claims we should curry
get_defn_expansions_guardedA(Eq,RetType,Depth,Self,ParamTypes,FRetType,[[H|Start]|T1],[RW|T2],[['feval',Y]|T2]):-
  is_list(Start),
  get_defn_expansions_guarded_low(Eq,RetType,Depth,Self,ParamTypes,FRetType,[H|Start],RW,Y),
  if_trace((defn;metta_defn;eval_args;e),indentq_d(Depth,'explicit curry  ', [[[H|Start]|T1] ,'----->', RW])).


% type signatiure claims we should curry
get_defn_expansions_guardedB(Eq,RetType,Depth,Self,ParamTypes,FRetType,[[H|Start]|T1],RW,Y):-
  is_list(Start), append(Start,T1,Args),
  get_defn_expansions_guarded_low(Eq,RetType,Depth,Self,ParamTypes,FRetType,[H|Args],RW,Y),
  if_trace((defn;metta_defn;eval_args;e),indentq_d(Depth,'explicit curry  ', [[[H|Start]|T1] ,'----->', RW])).

% implicit curry (we "guessed" this)
get_defn_expansions_guardedC(Eq,RetType,Depth,Self,ParamTypes,FRetType,[[H|Start]|T1],[[H|NewStart]|NewT1],[Y|T1]):- is_list(Start),
    same_len_copy(Start,NewStart),
    X = [H|NewStart],
    findall(guarded_defn(XX,ParamTypes,FRetType,B0),get_defn_expansions_guarded_low(Eq,RetType,Depth,Self,ParamTypes,FRetType,X,XX,B0),XXB0L),
    XXB0L\=[], if_trace((defn;metta_defn;eval_args;e),maplist(print_templates(Depth,'implicit curry '),XXB0L)),!,
    member(guarded_defn(XX,ParamTypes,FRetType,B0),XXB0L), X=XX, Y=B0, X\=@=B0,
    light_eval(Eq,RetType,Depth,Self,B0,Y),
    same_len_copy(T1,NewT1).

% no curry
get_defn_expansions_guardedD(Eq,RetType,Depth,Self,ParamTypes,FRetType,HArgs,RW,Y):-
  get_defn_expansions_guarded_low(Eq,RetType,Depth,Self,ParamTypes,FRetType,HArgs,RW,Y).

get_defn_expansions_guarded_low(_Eq,_RetType,_Depth,Self,ParamTypes,FRetType,[H|Args],[H|NewArgs],Body):-
   same_len_copy(Args,NewArgs),same_len_copy(Args,NewParamTypes),
   length(Args,Len),
   function_declaration(Self, H, Len, NewArgs, NewParamTypes, FRetType, CBody, _WrappedBody , _ReturnVal), % trace,
   ParamTypes+CBody = NewParamTypes+Body.




% get a guarded definition
eval_30(Eq,RetType,Depth,Self,X,Y):-  can_be_ok(get_defn_expansions_guarded,X),
    quietly((if_trace(defn, (curried_arity(X,F,A),finfo(F,A,X))),
    findall(guarded_defn(XX,ParamTypes,FRetType,B0),
           get_defn_expansions_guarded(Eq,RetType,Depth,Self,ParamTypes,FRetType,X,XX,B0),XXB0L))),
    XXB0L \==[], % trace,
    \+ sub_var_safely('Any', XXB0L),!,

    (XXB0L==[] ->  eval_constructor(Eq,RetType,Depth,Self,X,Y);  % no definition therefore treat it like a data constructor
         catch(eval_defn_bodies_guarded(Eq,RetType,Depth,Self,X,Y,XXB0L),metta_NotReducible,X=Y)).

eval_defn_bodies_guarded(Eq,RetType,Depth,Self,X,Y,XXB0L):-
  if_trace((defn;metta_defn;eval_args;e),show_bodies('GUARDS ', Depth, XXB0L)),
  if_or_else((member(guarded_defn(XX,ParamTypes,FRetType,B0),XXB0L), copy_term(guarded_defn(XX,ParamTypes,FRetType,B0),USED),
              eval_defn_success_guarded(Eq,RetType,Depth,Self,ParamTypes,FRetType,X,Y,XX,B0,USED)),
            eval_defn_failure_guarded(Eq,RetType,Depth,Self,ParamTypes,X,Y)).



true_or_log_fail(Depth,Goal,LogFail):- (call(Goal)
          -> true ; ((if_trace(e,color_g_mesg('#713700',indentq2(Depth,failure(LogFail)))),!),!,fail)).


eval_defn_success_guarded(Eq,RetType,Depth,Self,ParamTypes,FRetType,X,Y,XX,B0,USED):-
  true_or_log_fail(Depth,X=XX,unify_head(X=XX)),
  XX = [_|Args], %ignore(FRetType=RetType),
  true_or_log_fail(Depth, \+ \+ (maplist(non_arg_violation(Self),ParamTypes,Args)), non_arg_violation(ParamTypes,Args)),
  Y=B0, X\=@=B0,
  if_trace(e,color_g_mesg('#773700',indentq2(Depth,defs_used(XX-->B0,def(USED))))),
  maybe_trace(unknown),
  light_eval(Eq,RetType,Depth,Self,B0,Y),
  nop(non_arg_violation(Self,FRetType,Y)).

%eval_defn_failure_guarded(_Eq,_RetType,_Depth,_Self,_ParamTypes,X,X):- !.
%eval_defn_failure_guarded(_Eq,_RetType,_Depth,_Self,_ParamTypes,_X,Res):- !, Res='Empty',!,fail.
eval_defn_failure_guarded(_Eq,_RetType,Depth,_Self,_ParamTypes,X,Res):-
  if_trace(e,color_g_mesg('#773701',indentq2(Depth,defs_failed(X)))),
  !, \+ fail_missed_defn, X=Res.



:- nodebug(metta('defn')).

%eval_40(Eq,RetType,Depth,Self,['If2',Cond,Then,_],Res):- trace,fail.

eval_30(Eq,RetType,Depth,Self,X,Y):-  can_be_ok(maybe_eval_defn,X),
       quietly( findall((rule(XX,B0,Nth,typs)),call_nth(get_defn_expansions(Eq,RetType,Depth,Self,X,XX,B0),Nth),XXB0L) ),
        XXB0L \==[], !, %trace,
        % maybe_trace(unknown),
        catch(eval_defn_bodies(Eq,RetType,Depth,Self,X,Y,XXB0L),metta_NotReducible,X=Y).
% eval_40(Eq,RetType,Depth,Self,['If2',Cond,Then,_],Res):- trace,fail.

eval_30(Eq,RetType,Depth,Self,H,BO):- can_be_ok(metta_eq_def,H),
  copy_term(H,HC),
  findall(H->B0, ((woc(metta_eq_def(Eq,Self,H,B0)),HC=@=H)), BL),
  BL\==[],!,
  member(H->B0,BL),nl,
  print_templates(Depth,HC,rule(H,B0,_Nth,_Types)),
  eval_args(Eq,RetType,Depth,Self,B0,BO).




show_bodies(Why, Depth, XXB0L):-
    length(XXB0L,Len),
    if_t(Len==0,
      (sformat(WhyEquals,'~w = ',[Why]),
       color_g_mesg('#A7370A',indentq_d(Depth,WhyEquals,Len)))),
    forall(nth1(Nth,XXB0L,Item),
      (sformat(WhyNth,'~w   ~w/~w   ',[Why,Nth,Len]),
       color_g_mesg('#A7370A',once(print_templates(Depth,WhyNth,Item))))),!.


eval_defn_bodies(Eq,RetType,Depth,Self,X,Y,XXB0LU):-
  must_det_lls((%subst_varnames(XXB0LU,XXB0L),
    =(XXB0LU,XXB0L),
  if_trace((defn;metta_defn;eval_args;e),show_bodies('RULE ', Depth, XXB0L)))),
  if_or_else(
     (member(rule(XX,B0,Nth,Typs),XXB0L), copy_term(rule(XX,B0,Nth,Typs),USED),
      eval_defn_success(Eq,RetType,Depth,Self,X,Y,XX,B0,USED)),
  eval_defn_failure(Eq,RetType,Depth,Self,X,Y)).

eval_defn_success(_Eq,_RetType,Depth,_Self,X,Y,XX,B0,USED):-
  true_or_log_fail(Depth, X=XX,unify_head(X=XX)),
  X\=@=B0,Y=B0,
  if_trace(e,color_g_mesg('#773700',indentq2(Depth,defs_used(XX-->B0,def(USED))))),
  maybe_trace(unknown).
/*
  nop(light_eval(Eq,RetType,Depth,Self,B0,Y)),
  if_t(between(401,420,Depth),writeq(a(B0))),
  if_t(between(397,400,Depth),(writeq(b(B0)),trace)),
  if_t(between(392,397,Depth),(writeq(c(B0)),maybe_trace(unknown),fail)),
  if_t(Depth<380,(writeq(f(B0)),fail)).
*/

%eval_defn_failure(_Eq,_RetType,_Depth,_Self,X,X):- !.
%eval_defn_failure(_Eq,_RetType,_Depth,_Self,_X,Res):- !, Res='Empty',!,fail.
eval_defn_failure(_Eq,_RetType,Depth,_Self,X,Res):- % trace,
  if_trace(e,color_g_mesg('#773701',indentq2(Depth,defs_failed(X)))),
  !, \+ fail_missed_defn, X=Res.


pl_clause_num(Head,Body,Ref,Index):-
    clause(Head,Body,Ref),
    nth_clause(Head,Index,Ref).

same_len_copy(Args,NewArgs):- length(Args,N),length(NewArgs,N).


get_defn_expansions(Eq,RetType,Depth,Self,X,XX,B0):-
    if_or_else(get_defn_expansionsA(Eq,RetType,Depth,Self,X,XX,B0),
    if_or_else(get_defn_expansionsB(Eq,RetType,Depth,Self,X,XX,B0),
    if_or_else(get_defn_expansionsC(Eq,RetType,Depth,Self,X,XX,B0),
               get_defn_expansionsD(Eq,RetType,Depth,Self,X,XX,B0)))).

get_defn_expansionsA(_Eq,_RetType,_Depth,Self,[[H|HArgs]|Args],XX,B0):- symbol(H),
   is_list(HArgs),is_list(Args),
   same_len_copy(Args,NewArgs), same_len_copy(HArgs,NewHArgs),
   quietly((metta_atom(Self,[=,[[HH|NewHArgs]|NewArgs],B0]),H==HH)),
   [[H|NewHArgs]|NewArgs]=XX,
   sanity_check_eval(curry0,B0).

get_defn_expansionsB(_Eq,_RetType,_Depth,Self,[H|Args],[H|NewArgs],B0):- same_len_copy(Args,NewArgs),
    quietly((metta_atom(Self,[=,[HH|NewArgs],B0]),H==HH)),
    metta_defn(Self,[H|NewArgs],B0).

get_defn_expansionsC(Eq,RetType,Depth,Self,[[H|Start]|T1],[[H|NewStart]|NewT1],[Y|T1]):- is_list(Start),
    same_len_copy(Start,NewStart),
    X = [H|NewStart],
    findall((rule(XX,B0,Nth,typs)),call_nth(get_defn_expansions(Eq,RetType,Depth,Self,X,XX,B0),Nth),XXB0L),
    XXB0L\=[], if_trace((defn;metta_defn;eval_args;e),maplist(print_templates(Depth,'curry 1'),XXB0L)),!,
    member(rule(XX,B0,Nth,_Typs),XXB0L), X=XX, Y=B0, X\=@=B0,
    light_eval(Eq,RetType,Depth,Self,B0,Y),
    same_len_copy(T1,NewT1).

get_defn_expansionsD(Eq,RetType,Depth,Self,[[H|Start]|T1],RW,Y):- is_list(Start), append(Start,T1,Args),
  get_defn_expansions(Eq,RetType,Depth,Self,[H|Args],RW,Y),
  if_trace((defn;metta_defn;eval_args;e),indentq_d(Depth,'curry 2 ', [[[H|Start]|T1] ,'----->', RW])).


print_templates(Depth,T,XXB0):- \+ \+ (numbervars(T+XXB0,0,_,[attvar(skip)]), print_templates0(Depth,T,XXB0)).
print_templates0(Depth,T,guarded_defn(Types,XX,B0)):-!,
   Depth2 is Depth+2,
    if_t(is_list(Types),indentq_d(Depth,'guarded',['->'|Types])),
    indentq_d(Depth,T,[]),
    indentq_d(Depth2,'(=',XX),
    indentq_d(Depth2,' ',ste('',B0,')')).
print_templates0(Depth,T,rule(XX,B0,Nth,Types)):-!,
    indentq_d(Depth,T,Nth:Types),
    indentq_d(Depth,'(=',XX),
    indentq_d(Depth,'',ste('',B0,')')).
print_templates0(Depth,_T,guarded_defn(XX,ParamTypes,FRetType,B0)):- !,
    append(ParamTypes,[FRetType],Types),
    if_t(is_list(Types),indentq_d(Depth,'guarded',['->'|Types])),
    indentq_d(Depth,'(=',XX),
    indentq_d(Depth,'',ste('',B0,')')).
print_templates0(Depth,T,XXB0):- ignore(indentq_d(Depth,'<<>>'(T),template(XXB0))),!.

light_eval(Depth,Self,X,B):-
  light_eval(_Eq,_RetType,Depth,Self,X,B).
light_eval(_Eq,_RetType,_Depth,_Self,B,B).

not_template_arg(TArg):- var(TArg), !, \+ attvar(TArg).
not_template_arg(TArg):- atomic(TArg),!.
%not_template_arg(TArg):- is_list(TArg),!,fail.


% =================================================================
% =================================================================
% =================================================================
%  AGREGATES
% =================================================================
% =================================================================
% =================================================================

cwdl(DL,Eval, Res):- call_with_depth_limit(eval(Eval,Res),DL, R), ignore(R==depth_limit_exceeded->Res=R;true).
%cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).

%cwtl(DL,Goal):- catch(call_with_time_limit(DL,Goal),time_limit_exceeded,fail).

cwtl(Time, Goal) :-
    Time>0,
    !,
    setup_call_cleanup(alarm(Time,
                             throw(time_limit_exceeded),
                             Id,
                             [install(false)]),
                       cwtl_goal(Id, Goal),
                       time:remove_alarm_notrace(Id)).

cwtl_goal(AlarmID, Goal) :-
    install_alarm(AlarmID),
    call(Goal).

eval_10(Eq,RetType,Depth,Self,X,Y):-
    as_prolog_x(Depth,Self,X,XX),
    eval_20(Eq,RetType,Depth,Self,XX,Y),
    notrace(if_t( \+ sub_var_safely(Y,X), sanity_check_eval(eval_20_last(XX),Y))).

eval_20(Eq,RetType,Depth,Self,AEMore,ResOut):-
  eval_adjust_args(Eq,RetType,ResIn,ResOut,Depth,Self,AEMore,AEAdjusted),
  (AEMore\==AEAdjusted
     -> if_trace((e;argtypes),color_g_mesg('#773733',indentq2(Depth,AEMore -> AEAdjusted)))
     ;  if_trace((  argtypes),color_g_mesg('#773733',indentq2(Depth,same(AEMore))))),
  woc(eval_30(Eq,RetType,Depth,Self,AEAdjusted,ResIn)),
  \+ \+ check_returnval(Eq,RetType,ResOut).

eval_30(Eq,RetType,Depth,Self,[Op|X],Y):- nonvar(Op), !, eval_40(Eq,RetType,Depth,Self,[Op|X],Y).

eval_each_arg(Eq,_RetType,Depth,Self,X,Y):- is_list(X),!, maplist(eval_ret_5(Eq,Depth,Self),X,YY),YY=Y.
eval_each_arg(_Eq,_RetType,_Depth,_Self,X,X).

eval_ret_5(Eq,Depth,Self,X,Y):- eval_ret(Eq,_,Depth,Self,X,Y).

eval_30(Eq,RetType,Depth,Self,X,Y):- \+ old_sys, !, eval_each_arg(Eq,RetType,Depth,Self,X,Y).
eval_30(Eq,RetType,Depth,Self,X,Y):-
  subst_args_here(Eq,RetType,Depth,Self,X,Y).


% functs_to_preds([Eq, H, B], Preds)
eval_40_disabled(_Eq,_RetType,_Dpth,_Slf,[H|PredDecl],Res):- fail,
      is_rust_operation([H|PredDecl]),!, % run
      must_det_ll((rust_metta_run(exec([H|PredDecl]),Res),
      nop(write_src(res(Res))))).

eval_40(Eq,RetType,Depth,Self,X,Y):- \+ old_sys, !, eval_each_arg(Eq,RetType,Depth,Self,X,Y).

eval_40(Eq,RetType,Depth,Self,[H|PredDecl],Res):-
   eval_args(Eq,_,Depth,Self,H,HH),
   % maybe_eval_subst ?
   subst_args_here(Eq,RetType,Depth,Self,[HH|PredDecl],Res).


%findall_eval(Eq,RetType,Depth,Self,X,L):- findall_eval(Eq,RetType,_RT,Depth,Self,X,L).
%findall_eval(Eq,RetType,Depth,Self,X,S):- findall(E,eval_ne(Eq,RetType,Depth,Self,X,E),S)*->true;S=[].
findall_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- self_eval(X),!,(is_returned(X)-> L=[X] ; L=[]).
findall_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- typed_list(X,_Type,L),!.
findall_eval(Eq,RetType,Depth,Self,Funcall,L):-
   findall_ne(E,eval(Eq,RetType,Depth,Self,Funcall,E),L).

%bagof_eval(Eq,RetType,Depth,Self,X,L):- bagof_eval(Eq,RetType,_RT,Depth,Self,X,L).
%bagof_eval(Eq,RetType,Depth,Self,X,S):- bagof(E,eval_ne(Eq,RetType,Depth,Self,X,E),S)*->true;S=[].
bagof_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- self_eval(X),!,L=[X].
bagof_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- typed_list(X,_Type,L),!.
bagof_eval(Eq,RetType,Depth,Self,Funcall,L):-
    bagof_ne(E,eval(Eq,RetType,Depth,Self,Funcall,E),L).

setof_eval(Depth,Self,Funcall,L):- setof_eval('=',_RT,Depth,Self,Funcall,L).
setof_eval(Eq,RetType,Depth,Self,Funcall,S):- findall_eval(Eq,RetType,Depth,Self,Funcall,L),
   list_to_set(L,S).

bagof_ne(E,Call,L):-
   bagof(E,(rtrace_on_error(Call), is_returned(E)),L).

findall_ne(E,Call,L):-
   findall(E,(rtrace_on_error(Call), is_returned(E)),L).

eval_ne(Eq,RetType,Depth,Self,Funcall,E):-
  ((eval_args(Eq,RetType,Depth,Self,Funcall,E))
    *-> is_returned(E);(fail,E=Funcall)).

is_returned(E):- notrace( \+ is_empty(E)).
is_empty(E):- E=='Empty'.
%is_empty(E):- notrace(( nonvar(E), sub_var_safely('Empty',E))),!.


:- ensure_loaded(metta_subst).

solve_quadratic(A, B, I, J, K) :-
    %X in -1000..1000,  % Define a domain for X
     (X + A) * (X + B) #= I*X*X + J*X + K.  % Define the quadratic equation
    %label([X]).  % Find solutions for X


as_type(B,_Type,B):- var(B),!.
as_type(B,_Type,B):- \+ compound(B),!.

as_type([OP|B],Type,Res):- var(Type),
   len_or_unbound(B,Len),
   get_operator_typedef(_Self,OP,Len,_ParamTypes,RetType),
   Type=RetType,
   eval_for(RetType,[OP|B],Res),!.

as_type(B,RetType,Res):- is_pro_eval_kind(RetType),!,
   eval_for(RetType,B,Res).

as_type(B,_Type,B).

same_types(A,C,_Type,A1,C1):-
  A1=A,C1=C,!.
same_types(A,C,Type,A1,C1):-
  freeze(A,guess_type(A,Type)),
  freeze(C,guess_type(C,Type)),
  A1=A,C1=C.

guess_type(A,Type):-
   current_self(Self),
   get_type_or_guess(20,Self,A,Was),
   can_assign(Was,Type).

eval_for(RetType,X,Y):-
  current_self(Self),
  eval_args('=',RetType,20,Self,X,Y).

%if_debugging(G):- ignore(call(G)).
if_debugging(_).
bcc:- maybe_trace(unknown),
  bc_fn([:,Prf,[in_tad_with,[sequence_variant,rs15],[gene,d]]],
     ['S',['S',['S',['S','Z']]]],
     OUT),
    write_src(prf=Prf), write_src(OUT).


bci:- maybe_trace(unknown),
  bc_impl([:,Prf,[in_tad_with,[sequence_variant,rs15],[gene,d]]],
     ['S',['S',['S',['S','Z']]]],
     OUT),
    write_src(prf=Prf), write_src(OUT).



bcm:- % maybe_trace(unknown),
  bc_impl([:,Prf,[member,_A,_B,_C]],
     ['S',['S',['S','Z']]],
     OUT),
    write_src(prf=Prf), write_src(OUT).


bc_fn(A,B,C):- %maybe_trace(unknown),
  same_types(A,C,_,A1,C1),
  as_type(B,'Nat',B1),
  bc_impl(A1,B1,C1).

bc_impl([:, _prf, _ccln], _, [:, _prf, _ccln]) :-
    if_debugging(println_impl(['bc-base', [:, _prf, _ccln]])),
    metta_atom('&kb', [:, _prf, _ccln]),
    if_debugging(println_impl(['bc-base-ground', [:, _prf, _ccln]])),
    nocut.

bc_impl([:, [_prfabs, _prfarg], _ccln], ['S', _k], [:, [_prfabs, _prfarg], _ccln]) :-
    if_debugging(println_impl(['bc-rec', [:, [_prfabs, _prfarg], _ccln], ['S', _k]])),
    bc_impl([:, _prfabs, ['->', _prms, _ccln]], _k, [:, _prfabs, [->, _prms, _ccln]]),
    bc_impl([:, _prfarg, _prms], _k, [:, _prfarg, _prms]).







% Unification hook for peer_objects (NewParamType should get our peer_objects)
peer_objects:attr_unify_hook(ObjList, NewParamType) :-
 if_t(var(NewParamType),
   ((get_attr(NewParamType, peer_objects, NewParamTypeObjList) -> true ; NewParamTypeObjList = []),
    maplist(not_violate_type_simularity(_Space, NewParamType, NewParamTypeObjList), ObjList))).

% Unification hook for peer_interfaces (NewParamType should get our peer_interfaces)
peer_interfaces:attr_unify_hook(CachedInterfaces, NewParamType) :-
 if_t(var(NewParamType),
    update_peer(peer_interfaces,NewParamType, CachedInterfaces)).

% Update ParamType with new interfaces (NewParamType should get our Attribute(s))
update_peer(Attribute, ParamType, NewInterfaces) :-
    (get_attr(ParamType, Attribute, CachedInterfaces) -> true ; CachedInterfaces = []),
    ord_union(CachedInterfaces, NewInterfaces, UpdatedInterfaces),
    put_attr(ParamType, Attribute, UpdatedInterfaces).

% Handling type similarity checks for peer_objects
not_violate_type_simularity(Space, ParamType, ObjList, NewObj) :-
    ObjList == [], !,
    get_dtypes(Space, NewObj, NewInterfaces),
    dont_put_attr(ParamType, peer_objects, [NewObj-NewInterfaces]),
    update_peer(peer_interfaces, ParamType, NewInterfaces).

not_violate_type_simularity(_Space,_ParamType, ObjList, NewObj) :-
    member(PrevObj-_, ObjList),
    (PrevObj =@= NewObj ; PrevObj == NewObj), !.

not_violate_type_simularity(Space, ParamType, ObjList, NewObj) :-
    get_dtypes(Space, NewObj, NewInterfaces),
    member(PrevObj-PrevInterfaces, ObjList),
    nonvar(PrevObj),
    once((
        can_be_same_types(PrevInterfaces, NewInterfaces)
    )), !,
    dont_put_attr(ParamType, peer_objects, [NewObj-NewInterfaces | ObjList]),
    update_peer(peer_interfaces, ParamType, NewInterfaces).

% Check type compatibility
can_be_same_types(PrevInterfaces, NewInterfaces) :-
    member(PrevType, PrevInterfaces), member(NewType, NewInterfaces), some_overlap(NewType,PrevType), !.

some_overlap(NewType,PrevType):- NewType =@= PrevType.

actual_violation(ParamType, List):- member(ActualType,List),
    (assignable_to_param_type(_Arg, ActualType, ParamType);assignable_to_param_type(_Arg, ParamType, ActualType)),!,fail.
actual_violation(ParamType, List):- member(ActualType,List), type_violation(ParamType,ActualType),!.
actual_violation(_ParamType,_List):- ! , fail.

% Handling parameter type compatibility
non_arg_violation_each(_Space,ParamType, Arg):- ParamType=='Atom', !, Arg\=='Empty'.
non_arg_violation_each(_Space,ParamType, Arg):- ParamType=='Any', !, Arg\=='Empty'.
non_arg_violation_each(_Space,ParamType,_Arg):- ParamType=='AnyRet', !.
%non_arg_violation_each(_Space,ParamType,_Arg):- ParamType=='%Undefined%',!.
non_arg_violation_each(Space, ParamType, Arg) :-
    (check_non_arg_violation(Space, ParamType, Arg)
     -> nop((writeq(good(ParamType, Arg)), nl))
     ;  (nop((writeq(bad(ParamType, Arg)), nl)), !, fail)).


% if was set to a variable should we continue to enforce the fact it is a variable?
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Variable',!,is_ftVar(Arg).
% Ensure non-argument violations are checked on Vars
check_non_arg_violation(_Space,ParamType,Arg):-  attvar(Arg), get_attr(Arg, cns, S = [L|IST] ), is_list(IST),
  ((member(ActualType,[L|IST]), ActualType == ParamType) -> true ;
  (actual_violation(ParamType,[L|IST])-> (!,fail);
   dont_put_attr(Arg, cns, S = [ParamType,L|IST] ))),!.
check_non_arg_violation(Space,ParamType,Arg):-  var(Arg), dont_put_attr(Arg, cns, Space = [ParamType]),!.
%check_non_arg_violation(Space, ParamType, Var) :- var(Var), !, freeze(Var, non_arg_violation_each(Space, ParamType, Var)).

% Ensure non-argument violations are checked
check_non_arg_violation(Space, ParamType, NewObj) :- var(ParamType), !,
    (get_attr(ParamType, peer_objects, ObjList) -> true ; ObjList = []), !,
    not_violate_type_simularity(Space, ParamType, ObjList, NewObj).

%check_non_arg_violation(_Space,['TypeOf',Target], Arg):- Arg=Target.
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Empty',!,Arg==ParamType.
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'EmptyType',!,Arg=='Empty'.
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Grounded','get-metatype'(Arg,Type),!,Type==ParamType.
%check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'ErrorType',!,Arg=='Empty'.
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Expression',!, is_list(Arg).
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Symbol',!,symbol(Arg).
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'String',!,string(Arg).
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Type', is_type(Arg),!.
check_non_arg_violation(_Space,ParamType,Arg):- ParamType == 'Type', is_type(Arg),!.

check_non_arg_violation(Space,ParamType,Arg):- ParamType == 'Undeclared',!,get_type_or_guess(30,Space,Arg,ActualType),ActualType=='%Undefined%',!.
check_non_arg_violation(Space,ParamType,Arg):- ParamType = [Ar|ParamTypes],Ar=='->', !,
    get_type_or_guess(30,Space,Arg,ActualType),
    ActualType=[Ar2|ActualParamTypes],Ar2==Ar,
    maplist(assignable_to_param_type(_),ActualParamTypes,ParamTypes),!.

%check_non_arg_violation(_Space,ParamType,_Arg):- nonvar(ParamType), is_nonspecific_type(ParamType),!.
check_non_arg_violation(Space,ParamType,Arg):-
  get_type_or_guess(30,Space,Arg,ActualType),
  assignable_to_param_type(Arg, ActualType, ParamType).


get_type_or_guess(Depth,Space,Arg,ActualType):-
  no_repeats(ActualType,get_type_or_guess1(Depth,Space,Arg,ActualType)).

get_type_or_guess1(Depth,Space,Arg,ActualType):-
  get_type(Depth,Space,Arg,ActualType), nocut.
get_type_or_guess1(_Dpth,_Spce,Arg,ActualType):-
  get_type(Arg,ActualType).

ok_when_undefined(Arg,_ParamType):- var(Arg),!.
ok_when_undefined(_Arg,'String'):-!.

maybe_type_check(_Eq,RetType,_ResIn,_ResOut,_Depth,Self,AE,More,Adjusted):-
   ignore((
   if_t(is_list(Adjusted),length(Adjusted,LenX)),
   if_t(var(LenX),if_t(is_list(More),length(More,LenX))),
   type_checks_out(Self,AE,LenX,Adjusted,RetType))).

return_type_compat(_RetType,_XType).

type_checks_out(Self,AE,LenX,Adjusted,RetType):-
   get_operator_typedef_R(Self,AE,LenX,XParamTypes,XType),
   return_type_compat(RetType,XType),
   trace_if_debug(AE,LenX),
   maplist(non_arg_violation_each(Self),XParamTypes,Adjusted),!.
type_checks_out(Self,AE,LenX,Adjusted,RetType):-
  get_operator_typedef_R(Self,AE,LenX,XParamTypes,XType),
  return_type_compat(RetType,XType),
  trace_if_debug(AE,LenX),
  maplist(throw_violation_each(Self),XParamTypes,Adjusted),!.
type_checks_out(Self,AE,LenX,Adjusted,RetType):-
  nop(type_checks_out(Self,AE,LenX,Adjusted,RetType)),!.
throw_violation_each(Self,ParamType,Arg):-
  (non_arg_violation_each(Self,ParamType,Arg)-> true ; throw_type_error(Arg,ParamType)).

throw_type_error(Arg,ParamType):-
   if_tracemsg(unknown,throw_type_error(Arg,ParamType)),
   maybe_trace(unknown),throw_metta_error(['Error',Arg,'BadType']).

assignable_to_param_type(_Arg, ActualType, ParamType):- ActualType==ParamType,!.
assignable_to_param_type(Arg, ActualType, ParamType):- assignable_to_param_type1(Arg, ActualType, ParamType).
assignable_to_param_type1(_Arg, ActualType, ParamType):- ActualType=ParamType,!.
assignable_to_param_type1(_Arg, ActualType, ParamType):- paramtype_assignable_to(ActualType, ParamType),!.

% Check the argument type and verify it conforms to the expected type.
paramtype_assignable_to(ActualType,_ParamType):- ActualType = '%Undefined%',!.
paramtype_assignable_to(ActualType, ParamType):- assignable_to(ActualType, ParamType),!.


'mc__1_0+_make-var'(Types, Var):-
   %freeze(Var, non_arg_violation_each(_, Type, Var)),
   put_attr(Var,cns, _ = Types),
   % name it something recognisable/debuggable
   push_typename(Var,Types).

'mc__1_1+_bless-var'(Var, Types, Var):-
   %freeze(Var, non_arg_violation_each(_, Type, Var)),
   maplist(prevent_type_violations(Var),Types),
   % _maybe_ name it something recognisable/debuggable
   if_t( \+ get_attr(Var,vn,_), push_typename(Var,Types)).

push_typename(Var,Types):-
   nameify(Types,SVar), atomic_list_concat(['_typed_var_',SVar,'_'],'',SymG),
   gensym(SymG,SymVar), put_attr(Var,vn,SymVar).

nameify(Type, SVar):- attvar(Type),get_attr(Type,vn,SVar),!.
nameify(Types,SVar):- is_list(Types),!,maplist(nameify,Types,STypes),atomic_list_concat(STypes,SVar).
nameify(Type, SVar):- sformat(TypeStr,'~w',[Type]), svar_fixvarname(TypeStr,SVar).







:- find_missing_cuts.

end_of_file.



        eval_20(Eq,RetType,Depth,Self,X,Y):- fail,
          once(type_fit_childs(Eq,Depth,Self,RetType,X,XX)),
            X\=@=XX, fbug(type_fit_childs(X,XX)),fail,
          eval_evals(Eq,RetType,Depth,Self,XX,Y).


        into_arg_code([],true):-!.
        into_arg_code(H,TT):- \+ iz_conz(H), TT = H.
        into_arg_code([H,T],TT):- H==true,!,into_arg_code(T,TT).
        into_arg_code([T,H],TT):- H==true,!,into_arg_code(T,TT).
        into_arg_code([H,T],','(HH,TT)):- !, into_arg_code(H,HH),into_arg_code(T,TT).
        into_arg_code([H|T],TT):- H==true,!,into_arg_code(T,TT).
        into_arg_code([H|T],','(HH,TT)):- !, into_arg_code(H,HH),into_arg_code(T,TT).
        into_arg_code(TT,TT).
        into_arg_code([H|T],next(H,TT)):- into_arg_code(T,TT).


        % reduce args to match types even inside atoms
        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,X,Y):- is_ftVar(X),!,Y=X.
        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,X,Y):- symbolic(X),!,Y=X.
        type_fit_childs(Eq,Depth,Self,RetType,CodeForArg,X,Y):- compound_non_cons(X),!,
           into_list_args(X,XX),!,type_fit_childs(Eq,Depth,Self,RetType,CodeForArg,XX,Y).
        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,X,Y):- \+ is_list(X),iz_conz(X), maybe_trace(unknown), !,Y=X.
        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,X,Y):- self_eval(X),!,Y=X.

        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,[H|Args],[H|Args]):- (H=='eval_args';H=='eval_args-for'),!.

        type_fit_childs(Eq,Depth,Self,RetType,CodeForArg,['let*',Lets,Body],RetVal):- !,
            expand_let_star(Lets,Body,NewLet),!,
                type_fit_childs(Eq,Depth,Self,RetType,CodeForArg,NewLet,RetVal).

        /*                                e,CodeForCond,['If',Cond,Then,Else],
            pe_fit_childs(Eq,Depth,Self,RetType,CodeForCond,['If',Cond,Then,Else],
                    ['If',ConVal,(CodeForThen),CodeForElse]):-
            type_fit_childs(Eq,Depth,Self,'Bool',CodeForCond,Cond,ConVal).
            type_fit_childs(Eq,Depth,Self,RetType,CodeForThen,Then,ThenVal).
            type_fit_childs(Eq,Depth,Self,RetType,CodeForElse,Else,ElseVal).
        */

        type_fit_childs(Eq,Depth,Self,RetType,FullCodeForArgs,[H|Args],Y):- H\==':',
           ignore(get_operator_typedef1(Self,H,ParamTypes,RType)),
           ignore(eager_for_type(RType,RetType)),!,
           must_det_ll((maplist(type_fit_childs(Eq,Depth,Self),ParamTypes,CodeForArgs,Args,NewArgs),
           into_arg_code(CodeForArgs,MCodeForArgs),
           into_arg_code([MCodeForArgs,'eval_args'(XX,Y)],FullCodeForArgs),

           XX = [H|NewArgs],
           Y = _)).
           %eval_args(Eq,RetType,CodeForArg,Depth,Self,XX,Y).

        type_fit_childs(Eq,Depth,Self,RetType,FullCodeForArgs,[H|Args],Y):-
           must_det_ll((ignore(get_operator_typedef1(Self,H,ParamTypes,RetType)),
           maplist(type_fit_childs(Eq,Depth,Self),ParamTypes,CodeForArgs,Args,NewArgs),
           into_arg_code(CodeForArgs,FullCodeForArgs),
           Y = [H|NewArgs])).
        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,X,Y):-!,must_det_ll((X=Y)).

        eager_for_type(_RType,'Atom'):- !, fail.
        eager_for_type(_RType,'Type'):- !, fail.
        eager_for_type(RType,RetType):- RType==RetType,!.
        eager_for_type(RType,'Expression'):- !, RType=='Expression'.
        eager_for_type('Atom','Expression'):- !, fail.
        eager_for_type('Symbol','Expression'):- !, fail.
        eager_for_type(RType,Var):- var(Var),!,RType=Var.
        eager_for_type(_RType,_):-!.
        %eager_for_type(_RType,'Any'):- !.
        %eager_for_type(_RType,'Number').
        %eager_for_type(_RType,'Nat').


        eval_evals(_Eq,_Depth,_Self,_RetType,X,Y):-self_eval(X),!,Y=X.
        eval_evals(_Eq,_Depth,_Self,_RetType,X,Y):- \+ is_list(X),!,Y=X.
        eval_evals(Eq,Depth,Self,RetType,[Eval,X],Y):- Eval == 'eval_args',!,
          eval_evals(Eq,Depth,Self,RetType,X,XX),
          eval_args(Eq,RetType,Depth,Self,XX,Y).
        eval_evals(Eq,Depth,Self,RetType,[Eval,SomeType,X],Y):- Eval == 'eval_args-for',!,
          eval_evals(Eq,Depth,Self,RetType,X,XX),
          eval_args(Eq,SomeType,Depth,Self,XX,Y).
        eval_evals(Eq,Depth,Self,RetType,[H|Args],Y):-
           ignore(get_operator_typedef1(Self,H,ParamTypes,RetType)),
           maplist(eval_evals(Eq,Depth,Self),ParamTypes,Args,NewArgs),
           XX = [H|NewArgs],Y=XX.
        eval_evals(_Eq,_Depth,_Self,_RetType,X,X):-!.









