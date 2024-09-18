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

%
% post match modew
%:- style_check(-singleton).
:- multifile(fake_notrace/1).
:- meta_predicate(fake_notrace(0)).
:- meta_predicate(color_g_mesg(+,0)).
:- multifile(color_g_mesg/2).

self_eval0(X):- \+ callable(X),!.
self_eval0(X):- py_is_py(X),!.
%self_eval0(X):- py_type(X,List), List\==list,!.
self_eval0(X):- is_valid_nb_state(X),!.
%self_eval0(X):- string(X),!.
%self_eval0(X):- number(X),!.
%self_eval0([]).
self_eval0(X):- is_metta_declaration(X),!.
self_eval0([_,Ar,_]):- (Ar=='-->';Ar=='<->';Ar=='<--'),!.
self_eval0([F|X]):- !, is_list(X),length(X,Len),!,nonvar(F), is_self_eval_l_fa(F,Len),!.
self_eval0(X):- typed_list(X,_,_),!.
%self_eval0(X):- compound(X),!.
%self_eval0(X):- is_ref(X),!,fail.
self_eval0('True'). self_eval0('False'). % self_eval0('F').
self_eval0('Empty').
self_eval0([]).
self_eval0('%Undefined%').
self_eval0(X):- atom(X),!, \+ nb_bound(X,_),!.

nb_bound(Name,X):- atom(Name), atom_concat('&', _, Name),
  nb_current(Name, X).


coerce(Type,Value,Result):- nonvar(Value),Value=[Echo|EValue], Echo == echo, EValue = [RValue],!,coerce(Type,RValue,Result).
coerce(Type,Value,Result):- var(Type), !, Value=Result, freeze(Type,coerce(Type,Value,Result)).
coerce('Atom',Value,Result):- !, Value=Result.
coerce('Bool',Value,Result):- var(Value), !, Value=Result, freeze(Value,coerce('Bool',Value,Result)).
coerce('Bool',Value,Result):- is_list(Value),!,as_tf(call_true(Value),Result),
set_list_value(Value,Result).

set_list_value(Value,Result):- nb_setarg(1,Value,echo),nb_setarg(1,Value,[Result]).

%is_self_eval_l_fa('S',1). % cheat to comment

% these should get uncomented with a flag
%is_self_eval_l_fa(':',2).
% is_self_eval_l_fa('=',2).
% eval_20(Eq,RetType,Depth,Self,['quote',Eval],RetVal):- !, Eval = RetVal, check_returnval(Eq,RetType,RetVal).
is_self_eval_l_fa('quote',_).
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
:- nb_setval(self_space, '&self').

%! eval_to(+X,+Y) is semidet.
% checks if X evals to Y
evals_to(XX,Y):- Y=@=XX,!.
evals_to(XX,Y):- Y=='True',!, is_True(XX),!.

%current_self(Space):- nb_current(self_space,Space).

do_expander('=',_,X,X):-!.
do_expander(':',_,X,Y):- !, get_type(X,Y)*->X=Y.

get_type(Arg,Type):- eval_H(['get-type',Arg],Type).


%! eval_true(+X) is semidet.
% Evaluates the given term X and succeeds if X is not a constraint (i.e. \+ iz_conz(X)) and is callable, and calling X succeeds.
%
% If X is not callable, this predicate will attempt to evaluate the arguments of X (using eval_args/2) and succeed if the result is not False.
eval_true(X):- \+ iz_conz(X), callable(X), call(X).
eval_true(X):- eval_args(X,Y), once(var(Y) ; \+ is_False(Y)).

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
eval_args(X,Y):- current_self(Self), eval_args(500,Self,X,Y).
%eval_args(Eq,RetType,Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval_args(Eq,RetType,X)),fail.
eval_args(Depth,Self,X,Y):- eval_args('=',_RetType,Depth,Self,X,Y).

eval_args(_Eq,_RetType,_Dpth,_Slf,X,Y):- var(X),nonvar(Y),!,X=Y.
eval_args(_Eq,_RetType,_Dpth,_Slf,X,Y):- notrace(self_eval(X)),!,Y=X.
eval_args(Eq,RetType,Depth,Self,X,Y):-
    notrace(nonvar(Y)), var(RetType),
    get_type(Depth,Self,Y,WasType),
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


:- nodebug(metta(overflow)).
eval_00(_Eq,_RetType,_Depth,_Slf,X,Y):- self_eval(X),!,X=Y.
eval_00(Eq,RetType,Depth,Self,X,YO):-
   eval_01(Eq,RetType,Depth,Self,X,YO).
eval_01(Eq,RetType,Depth,Self,X,YO):-
    if_t((Depth<1, trace_on_overflow),
      debug(metta(eval_args))),
   notrace((Depth2 is Depth-1,    copy_term(X, XX))),
    trace_eval(eval_20(Eq,RetType),e,Depth2,Self,X,M),
   (self_eval(M)-> YO=M ;
   (((M=@=XX)-> Y=M
      ;eval_01(Eq,RetType,Depth2,Self,M,Y)),
   eval_02(Eq,RetType,Depth2,Self,Y,YO))).

eval_02(Eq,RetType,Depth2,Self,Y,YO):-
  once(if_or_else((subst_args_here(Eq,RetType,Depth2,Self,Y,YO)),
    if_or_else((fail,finish_eval(Eq,RetType,Depth2,Self,Y,YO)),
        Y=YO))).


% subst_args_here(Eq,RetType,Depth2,Self,Y,YO):-
%   Y =@= [ house, _59198,_59204,==,fish,fish],!,break.

% %  this needs to test itself for when it can be skipped
% %  uncommented causes 7% failure but a 10x speedup
% subst_args_here(Eq,RetType,Depth2,Self,Y,YO):- Y=YO.
% %  this next one at least causes no failures and 5x speedup
subst_args_here(Eq,RetType,Depth2,Self,Y,YO):- wont_need_subst(Y),!, Y=YO.
subst_args_here(Eq,RetType,Depth2,Self,Y,YO):-
  subst_args(Eq,RetType,Depth2,Self,Y,YO),
  notrace(if_t_else((wont_need_subst(Y),Y\=@=YO),
     (wdmsg(red,needed_subst_args(Y,YO)),bt,sleep(1.0)),
  nop(wdmsg(unneeded_subst_args(Y))))).

wont_need_subst([_,A|_]):- number(A),!,fail.
wont_need_subst([F|_]):-atom(F), \+ need_subst_f(F).
need_subst_f('==').
% ['Mortal','Socrates'] -> 'T'
need_subst_f('Mortal').
need_subst_f('*'). need_subst_f('+').
need_subst_f('-'). need_subst_f('/').
need_subst_f('<'). need_subst_f('=<').

if_t_else(If,Then,Else):- If -> Then ; Else.

finish_eval_here(Eq,RetType,Depth2,Self,Y,YO):-
  finish_eval(Eq,RetType,Depth2,Self,Y,YO),
  notrace(if_t(Y\=@=YO,wdmsg(finish_eval(Y,YO)))).

:- nodebug(metta(e)).

:- discontiguous eval_20/6.
:- discontiguous eval_40/6.
:- discontiguous eval_70/6.
%:- discontiguous eval_30fz/5.
%:- discontiguous eval_31/5.
%:- discontiguous eval_maybe_defn/5.

eval_20(Eq,RetType,_Dpth,_Slf,Name,Y):-
    atom(Name), !,
      (nb_bound(Name,X)->do_expander(Eq,RetType,X,Y);
       Y = Name).


eval_20(Eq,RetType,_Dpth,_Slf,X,Y):- no_eval(X),!,do_expander(Eq,RetType,X,Y).

args_not_evaled(X):- ( \+ is_list(X); maplist(no_eval,X)),!.
no_eval(X):- self_eval(X),!.
no_eval([SL|X]):- atomic(SL), !, is_sl(SL), args_not_evaled(X).
no_eval([SL|X]):- ( \+ atom(SL), \+ is_list(SL)), !,
          args_not_evaled(X).
is_sl(N):- number(N).
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

eval_20(Eq,RetType,Depth,Self,[F,[Eval,V]|VI],VO):- Eval == eval,!,
  ((eval_args(Eq,_FRype,Depth,Self,V,VV), V\=@=VV)*-> true; VV = V),
  eval_args(Eq,RetType,Depth,Self,[F,VV|VI],VO).

eval_20(Eq,RetType,Depth,Self,[[Eval,V]|VI],VO):- Eval == eval,!,
  ((eval_args(Eq,_FRype,Depth,Self,V,VV), V\=@=VV)*-> true; VV = V),
  eval_args(Eq,RetType,Depth,Self,[VV|VI],VO).
% DMILES @ TODO make sure this isnt an implicit curry
eval_20(Eq,_RetType,Depth,Self,[V|VI],VO):-  \+ callable(V), is_list(VI),!,
  maplist(eval_ret(Eq,_ArgRetType,Depth,Self),[V|VI],VOO),VO=VOO.


eval_20(Eq,RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval_args(Eq,RetType,Depth,Self,VI,VM),
  ( VM\==VI -> eval_args(Eq,RetType,Depth,Self,[V|VM],VVO) ;
    (eval_args(Eq,RetType,Depth,Self,V,VV), (V\==VV -> eval_args(Eq,RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_20(Eq,RetType,_Dpth,_Slf,X,Y):- \+ is_list(X),!,do_expander(Eq,RetType,X,Y).

eval_20(Eq,_RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval_args(Eq,_ArgRetType,Depth,Self),VI,VO).

eval_20(_,_,_,_,['echo',Value],Value):- !.
eval_20(=,Type,_,_,['coerce',Type,Value],Result):- !, coerce(Type,Value,Result).

% =================================================================
% =================================================================
% =================================================================
%  LET*
% =================================================================
% =================================================================
% =================================================================

    %eval_20(Eq,RetType,Depth2,Self,[Qw,X,Y],YO):- Qw == ('=='),!,
    %  eval_args(X,XX),eval_args(Y,YY), !, as_tf(XX==YY,YO).


    eval_20(Eq,RetType,Depth,Self,['let*',Lets,Body],RetVal):-
        expand_let_star(Lets,Body,NewLet),!,
            eval_20(Eq,RetType,Depth,Self,NewLet,RetVal).



expand_let_star(Lets,Body,Body):- Lets==[],!.
expand_let_star([H|LetRest],Body,['let',V,E,NewBody]):-
    is_list(H), H = [V,E], !,
    expand_let_star(LetRest,Body,NewBody).

eval_20(Eq,RetType,Depth,Self,X,RetVal):-
    once(expand_eval(X,XX)),X\==XX,!,
        %fbug(expand_eval(X,XX)),
        eval_20(Eq,RetType,Depth,Self,XX,RetVal).

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

eval_20(Eq,RetType,Depth,Self,['eval',X],Res):- !,
   eval_args(Eq,RetType,Depth,Self,X, Res).


eval_20(Eq,RetType,Depth,Self,['eval-for',Type,X],Res):- !,
    ignore(Type=RetType),
    eval_args(Eq,Type,Depth,Self,X, Res).

eval_20(Eq,RetType,Depth,Self,['eval-for',_Why,Type,X],Res):- !,
    ignore(Type=RetType),
    eval_args(Eq,Type,Depth,Self,X, Res).



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
unified(X,Y):- eval(X,XX),X\=@=XX,unified(Y,XX).
unified(X,Y):- eval(Y,YY),Y\=@=YY,unified(YY,X).

eval_until_unify(_Eq,_RetType,_Dpth,_Slf,X,X):- !.
eval_until_unify(Eq,RetType,Depth,Self,X,Y):- eval_until_eq(Eq,RetType,Depth,Self,X,Y),!.

eval_until_eq(_Eq,_RetType,_Dpth,_Slf,X,Y):- catch_nowarn(X=:=Y),!.
eval_until_eq(_Eq,_RetType,_Dpth,_Slf,X,Y):- catch_nowarn('#='(X,Y)),!.
eval_until_eq(Eq,RetType,_Dpth,_Slf,X,Y):-  X=Y,check_returnval(Eq,RetType,Y).
%eval_until_eq(Eq,RetType,Depth,Self,X,Y):- var(Y),!,eval_in_steps_or_same(Eq,RetType,Depth,Self,X,XX),Y=XX.
%eval_until_eq(Eq,RetType,Depth,Self,Y,X):- var(Y),!,eval_in_steps_or_same(Eq,RetType,Depth,Self,X,XX),Y=XX.
eval_until_eq(Eq,RetType,Depth,Self,X,Y):- \+is_list(Y),!,eval_in_steps_some_change(Eq,RetType,Depth,Self,X,XX),Y=XX.
eval_until_eq(Eq,RetType,Depth,Self,Y,X):- \+is_list(Y),!,eval_in_steps_some_change(Eq,RetType,Depth,Self,X,XX),Y=XX.
eval_until_eq(Eq,RetType,Depth,Self,X,Y):- eval_in_steps_some_change(Eq,RetType,Depth,Self,X,XX),eval_until_eq(Eq,RetType,Depth,Self,Y,XX).
eval_until_eq(_Eq,_RetType,_Dpth,_Slf,X,Y):- length(X,Len), \+ length(Y,Len),!,fail.
eval_until_eq(Eq,RetType,Depth,Self,X,Y):-  nth1(N,X,EX,RX), nth1(N,Y,EY,RY),
  EX=EY,!, maplist(eval_until_eq(Eq,RetType,Depth,Self),RX,RY).
eval_until_eq(Eq,RetType,Depth,Self,X,Y):-  nth1(N,X,EX,RX), nth1(N,Y,EY,RY),
  ((var(EX);var(EY)),eval_until_eq(Eq,RetType,Depth,Self,EX,EY)),
  maplist(eval_until_eq(Eq,RetType,Depth,Self),RX,RY).
eval_until_eq(Eq,RetType,Depth,Self,X,Y):-  nth1(N,X,EX,RX), nth1(N,Y,EY,RY),
  h((is_list(EX);is_list(EY)),eval_until_eq(Eq,RetType,Depth,Self,EX,EY)),
  maplist(eval_until_eq(Eq,RetType,Depth,Self),RX,RY).

 eval_1change(Eq,RetType,Depth,Self,EX,EXX):-
    eval_20(Eq,RetType,Depth,Self,EX,EXX),  EX \=@= EXX.

eval_complete_change(Eq,RetType,Depth,Self,EX,EXX):-
   eval_args(Eq,RetType,Depth,Self,EX,EXX),  EX \=@= EXX.

eval_in_steps_some_change(_Eq,_RetType,_Dpth,_Slf,EX,_):- \+ is_list(EX),!,fail.
eval_in_steps_some_change(Eq,RetType,Depth,Self,EX,EXX):- eval_1change(Eq,RetType,Depth,Self,EX,EXX).
eval_in_steps_some_change(Eq,RetType,Depth,Self,X,Y):- append(L,[EX|R],X),is_list(EX),
    eval_in_steps_some_change(Eq,RetType,Depth,Self,EX,EXX), EX\=@=EXX,
    append(L,[EXX|R],XX),eval_in_steps_or_same(Eq,RetType,Depth,Self,XX,Y).

eval_in_steps_or_same(Eq,RetType,Depth,Self,X,Y):-eval_in_steps_some_change(Eq,RetType,Depth,Self,X,Y).
eval_in_steps_or_same(Eq,RetType,_Dpth,_Slf,X,Y):- X=Y,check_returnval(Eq,RetType,Y).

  % (fail,make_nop(RetType,[],Template))).


possible_type(_Self,_Var,_RetTypeV).

eval_20(Eq,RetType,Depth,Self,['let',E,V,Body],OO):- var(V), nonvar(E), !,
      %(var(V)->true;trace),
      possible_type(Self,V,RetTypeV),
      eval_args(Eq,RetTypeV,Depth,Self,E,ER), V=ER,
      eval_args(Eq,RetType,Depth,Self,Body,OO).

eval_20(Eq,RetType,Depth,Self,['let',V,E,Body],OO):- !, % var(V), nonvar(E), !,
        %(var(V)->true;trace),
        possible_type(Self,V,RetTypeV),
        eval_args(Eq,RetTypeV,Depth,Self,E,ER), V=ER,
        eval_args(Eq,RetType,Depth,Self,Body,OO).
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
%   eval_until_unify(Eq,_RetTypeV,Depth,Self,Val,Var),
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
eval_20(Eq,RetType,Depth,Self,['time!',Cond],Res):- !, time_eval(eval_args(Cond),eval_args(Eq,RetType,Depth,Self,Cond,Res)).
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

println_impl(X):- format("~N~@~N",[write_sln(X)]),!.
println_impl(X):- user_io((ansi_format(fg('#c7ea46'),"~N~@~N",[write_sln(X)]))).

princ_impl(X):- format("~@",[write_sln(X)]),!.

write_sln(X):- string(X), !, write(X).
write_sln(X):- write_src_woi(X).

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
         equal_enough_for_test(XX,YY), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertNotEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertNotEqual',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         findall_eval(Eq,_BRetType,Depth,Self,Y,YY)),
         ( \+ equal_enough(XX,YY)), RetVal).

eval_20(Eq,_RetType,Depth,Self,['assertEqualToResult',X,Y],RetVal):- !,
   loonit_assert_source_tf_empty(
        ['assertEqualToResult',X,Y],XX,YY,
        (findall_eval(Eq,_ARetType,Depth,Self,X,XX),
         =(Y,YY)),
         equal_enough_for_test(XX,YY), RetVal).

loonit_assert_source_tf_empty(Src,XX,YY,Goal,Check,RetVal):-
    loonit_assert_source_tf(Src,Goal,Check,TF),
    tf_to_empty(TF,['Error'(got(XX),expected(YY))],RetVal).

tf_to_empty(TF,Else,RetVal):-
  (TF=='True'->as_nop(RetVal);RetVal=Else).

val_sort(Y,YY):- is_list(Y),!,sort(Y,YY).
val_sort(Y,[Y]).

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
     with_debug((eval_args),time_eval('Trace',OrigGoal)))))).

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
is_blank(E):- is_empty(E),!.
is_blank([]).
is_blank([X]):-!,is_blank(X).
has_let_star(Y):- sub_var('let*',Y).

sort_univ(L,S):- cl_list_to_set(L,E),sort(E,S).
% !(pragma! unit-tests tollerant) ; tollerant or exact
is_tollerant:- \+ option_value('unit-tests','exact').

equal_enough_for_test(X,Y):- X==Y,!.
equal_enough_for_test(X,Y):- X=@=Y,!.
equal_enough_for_test(X,Y):- is_list(X),is_list(Y),sort(X,X0),sort(Y,Y0),
       Y0=[YY],X0=[XX],!,equal_enough_for_test(XX,YY).
equal_enough_for_test(X,Y):- is_list(X),is_list(Y),X=[ErrorX|_],Y=[ErrorY|_],ErrorX=='Error',
      ErrorY == ErrorX,!.
equal_enough_for_test(X,Y):- is_blank(X),!,is_blank(Y).
equal_enough_for_test(X,Y):- has_let_star(Y),!,\+ is_blank(X).
equal_enough_for_test(X,Y):- is_list(X),is_list(Y),
   Y=[YY],X=[XX],!,equal_enough_for_test(XX,YY).
   %length(XX,XL),length(YY,YL),

%equal_enough_for_test(X,Y):-!,fail.

equal_enough_for_test(X,Y):- must_det_ll((subst_vars(X,XX),subst_vars(Y,YY))),!,
  equal_enough_for_test2(XX,YY),!.
equal_enough_for_test2(X,Y):- equal_enough(X,Y).

equal_enough(R,V):- is_list(R),is_list(V),sort_univ(R,RR),sort_univ(V,VV),!,equal_enouf(RR,VV),!.
equal_enough(R,V):- copy_term(R,RR),copy_term(V,VV),equal_enouf(R,V),!,R=@=RR,V=@=VV.
equal_enouf(R,V):- is_ftVar(R), is_ftVar(V), R=V,!.
equal_enouf(X,Y):- is_blank(X),!,is_blank(Y).
equal_enouf(X,Y):- symbol(X),symbol(Y),atom_concat('&',_,X),atom_concat('Grounding',_,Y).
equal_enouf(R,V):- R=@=V, R=V, !.
equal_enouf(_,V):- V=@='...',!.

equal_enouf(L,C):- is_tollerant, is_list(L),is_list(C),
     maybe_remove_nils(C,CC),equal_enouf(L,CC).

equal_enouf(L,C):- is_list(L),into_list_args(C,CC),!,equal_enouf_l(CC,L).
equal_enouf(C,L):- is_list(L),into_list_args(C,CC),!,equal_enouf_l(CC,L).
%equal_enouf(R,V):- (var(R),var(V)),!, R=V.
equal_enouf(R,V):- (var(R);var(V)),!, R==V.
equal_enouf(R,V):- number(R),number(V),!, RV is abs(R-V), RV < 0.03 .
equal_enouf(R,V):- atom(R),!,atom(V), has_unicode(R),has_unicode(V).
equal_enouf(R,V):- (\+ compound(R) ; \+ compound(V)),!, R==V.
equal_enouf(L,C):- into_list_args(L,LL),into_list_args(C,CC),!,equal_enouf_l(CC,LL).

equal_enouf_l([S1,V1|_],[S2,V2|_]):- S1 == 'State',S2 == 'State',!, equal_enouf(V1,V2).
equal_enouf_l(C,L):- \+ compound(C),!,L=@=C.
equal_enouf_l(L,C):- \+ compound(C),!,L=@=C.
equal_enouf_l([C|CC],[L|LL]):- !, equal_enouf(L,C),!,equal_enouf_l(CC,LL).

maybe_remove_nils(I,O):- always_remove_nils(I,O),!,I\=@=O.
always_remove_nils(I,O):- \+ compound(I),!,I=O.
always_remove_nils([H|T], TT):- H==[],!, always_remove_nils(T,TT).
always_remove_nils([H|T], TT):- H=='Empty',!, always_remove_nils(T,TT).
always_remove_nils([H|T],[H|TT]):- always_remove_nils(T,TT).

has_unicode(A):- atom_codes(A,Cs),member(N,Cs),N>127,!.

set_last_error(_).


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
check_replace_with_local_var([], Expr, Result) :-
    Result = Expr.

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

eval_20(Eq,RetType,_Dpth,_Slf,['new-space'],Space):- !, 'new-space'(Space),check_returnval(Eq,RetType,Space).

eval_20(Eq,RetType,Depth,Self,[Op,Space|Args],Res):- is_space_op(Op),!,
  eval_space_start(Eq,RetType,Depth,Self,[Op,Space|Args],Res).
eval_20(Eq,RetType,Depth,Self,['unify',Space|Args],Res):- !,
  eval_space_start(Eq,RetType,Depth,Self,['match',Space|Args],Res).

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

% Match-ELSE
eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Template,Else],Template):- !,
  ((eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Template],Template),
       \+ make_nop(RetType,[],Template))*->true;Template=Else).
% Match-TEMPLATE
eval_space(Eq,_RetType,Depth,Self,['match',Other,Goal,Template],Template):-!,
  metta_atom_iter(Eq,Depth,Self,Other,Goal).
/*
eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Template],Res):- !,
   metta_atom_iter(Eq,Depth,Self,Other,Goal),
   eval_args(Eq,RetType,Depth,Self,Template,Res).
*/

%metta_atom_iter(Eq,_Depth,_Slf,Other,[Equal,[F|H],B]):- Eq == Equal,!,  % trace,
%   metta_eq_def(Eq,Other,[F|H],B).

/*
metta_atom_iter(Eq,Depth,Self,Other,[Equal,[F|H],B]):- Eq == Equal,!,  % trace,
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
can_be_ok(A,B):- cant_be_ok(A,B),!,fbug(cant_be_ok(A,B)),trace.
can_be_ok(_,_).

cant_be_ok(_,[Let|_]):- Let==let.
% =================================================================
% =================================================================
% =================================================================
%  CASE/SWITCH
% =================================================================
% =================================================================
% =================================================================
% Macro: case
:- nodebug(metta(case)).

eval_20(Eq,RetType,Depth,Self,['switch',A,CL|T],Res):- !,
  eval_20(Eq,RetType,Depth,Self,['case',A,CL|T],Res).

eval_20(Eq,RetType,Depth,Self,[P,X|More],YY):- is_list(X),X=[_,_,_],simple_math(X),
   eval_selfless_2(X,XX),X\=@=XX,!, eval_20(Eq,RetType,Depth,Self,[P,XX|More],YY).
% if there is only a void then always return nothing for each Case
eval_20(Eq,_RetType,Depth,Self,['case',A,[[Void,_]]],Res):-
   '%void%' == Void,
   eval_args(Eq,_UnkRetType,Depth,Self,A,_),!,Res =[].

% if there is nothing for case just treat like a collapse
eval_20(Eq,RetType,Depth,Self,['case',A,[]],NoResult):- !,
  %forall(eval_args(Eq,_RetType2,Depth,Self,Expr,_),true),
  once(eval_args(Eq,_RetType2,Depth,Self,A,_)),
  make_nop(RetType,[],NoResult).


into_case_keys(_,[],[]).
into_case_keys(Nth,[Case0|CASES],[Key-Value|KVs]):-
  Nth1 is Nth+1,
  is_case(Key,Case0,Value),
  if_trace((case),(format('~N'),writeqln(c(Nth,Key)=Value))),
  into_case_keys(Nth1,CASES,KVs).

% Macro: case
eval_20(Eq,RetType,Depth,Self,['case',A,CL|T],Res):- !,
   must_det_ll(T==[]),
   into_case_list(CL,CASES),
   into_case_keys(1,CASES,KVs),
   eval_case(Eq,RetType,Depth,Self,A,KVs,Res).

eval_case(Eq,CaseRetType,Depth,Self,A,KVs,Res):-
   if_trace((case),(writeqln('case'=A))),
   ((eval_args(Eq,_UnkRetType,Depth,Self,A,AA),
         if_trace((case),writeqln('switch'=AA)),
    (select_case(Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    eval_args(Eq,CaseRetType,Depth,Self,Value,Res).

  select_case(Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Depth,Self,Cases,CasES),
       (best_key(AA,CasES,Value) -> true ;
        (member(Void -Value,CasES),Void=='%void%')))).

  best_key(AA,Cases,Value):- member(Match-Value,Cases),AA = Match,!.
  best_key(AA,Cases,Value):-
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),AA=@=Match)->true;
        (member(Match-Value,Cases),AA = Match))).

    into_case_list(CASES,CASES):- is_list(CASES),!.
        is_case(AA,[AA,Value],Value):-!.
        is_case(AA,[AA|Value],Value).

   maybe_special_keys(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval_args(Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(Depth,Self,[_|KVI],KVO):-
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(_Depth,_Self,[],[]).


% =================================================================
% =================================================================
% =================================================================
%  COLLAPSE/SUPERPOSE
% =================================================================
% =================================================================
% =================================================================

%;; collapse-bind because `collapse` doesnt guarentee shared bindings
eval_20(Eq,RetType,Depth,Self,['collapse-bind',List],Res):-!,
 maplist_ok_fails(eval_ne(Eq,RetType,Depth,Self),List,Res).

maplist_ok_fails(Pred2,[A|AA],BBB):- !,
 (call(Pred2,A,B) -> (BBB=[B|BB], maplist_ok_fails(Pred2,AA,BB))
   ; maplist_ok_fails(Pred2,AA,BBB)).
maplist_ok_fails(_Pred2,[],[]).

%;; superpose-bind because `superpose` doesnt guarentee shared bindings
% @TODO  need to keep bindings
eval_20(Eq,RetType,Depth,Self,['superpose-bind',List],Res):- !,
       re_member(Res,E,List),
       eval_ret(Eq,RetType,Depth,Self,E,Res).

re_member(Res,E,List):- term_variables(Res+E+List,TV),copy_term(TV,Copy),
    member(E,List),TV=Copy.

%[collapse,[1,2,3]]
eval_20(Eq,RetType,Depth,Self,['collapse',List],Res):-!,
 findall_eval(Eq,RetType,Depth,Self,List,Res).


eval_20(Eq,RetType,Depth,Self,['superpose',List],Res):- !,
       member(E,List),
       eval_ret(Eq,RetType,Depth,Self,E,Res).

%[superpose,[1,2,3]]
old_eval_20(_Eq,RetType,_Depth,_Self,['superpose',List],Res):- List==[], !,
  make_empty(RetType,[],Res).
old_eval_20(Eq,RetType,Depth,Self,['superpose',List],Res):- !,
  (((
   is_user_defined_head(Eq,Self,List) ,eval_args(Eq,RetType,Depth,Self,List,UList),
   List\=@=UList)
    *->  eval_20(Eq,RetType,Depth,Self,['superpose',UList],Res)
       ; ((member(E,List),eval_args(Eq,RetType,Depth,Self,E,Res))*->true;make_nop(RetType,[],Res)))),
  \+ Res = 'Empty'.

%[sequential,[1,2,3]]
eval_20(Eq,RetType,Depth,Self,['sequential',List],Res):- !,
  (((fail,is_user_defined_head(Eq,Self,List) ,eval_args(Eq,RetType,Depth,Self,List,UList), List\=@=UList)
    *->  eval_20(Eq,RetType,Depth,Self,['sequential',UList],Res)
       ; ((member(E,List),eval_ne(Eq,RetType,Depth,Self,E,Res))*->true;make_nop(RetType,[],Res)))).


get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).
eval20_failed(Eq,RetType,Depth,Self, Term, Res):-
  notrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval_args(Eq,RetType,Depth,Self, Term, Res).


sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
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
eval_20(_Eq,RetType,_Depth,_Self,['empty'],                Empty ):- !,
   make_empty(RetType, Empty).
eval_20(_Eq,RetType,Depth,Self,['nop',Expr], NoResult ):- !,
   make_nop(RetType,[], NoResult),
  ignore(eval_args('=',_RetType2,Depth,Self,Expr,_)).


eval_20(Eq,RetType,Depth,Self,['do',Expr], NoResult):- !,
  forall(eval_args(Eq,_RetType2,Depth,Self,Expr,_),true),
  %eval_ne(Eq,_RetType2,Depth,Self,Expr,_),!,
  make_empty(RetType,[],NoResult).

eval_20(_Eq,_RetType1,_Depth,_Self,['call!',S], TF):- !, eval_call(S,TF).
eval_20(_Eq,_RetType1,_Depth,_Self,['call-fn!',S], R):- !, eval_call_fn(S,R).
eval_20(_Eq,_RetType1,_Depth,_Self,['call-fn-nth!',Nth,S], R):-
    length(Left,Nth),
    append(Left,Right,S),
    append(Left,[R|Right],NewS),!,
    eval_call(NewS,_).

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


eval_20(Eq,RetType,Depth,Self,['get-doc'|Args],Res):- !,with_all_spaces(eval_args(Eq,RetType,Depth,Self,['metta-get-doc'|Args],Res)).
eval_20(Eq,RetType,Depth,Self,['help!'|Args],Res):-!,with_all_spaces(eval_args(Eq,RetType,Depth,Self,['metta-help!'|Args],Res)).

with_all_spaces(Goal):-
 locally(nb_setval(with_all_spaces,t),Goal).
using_all_spaces:- nb_current(with_all_spaces,t).

% =================================================================
% =================================================================
% =================================================================
%  if/If
% =================================================================
% =================================================================
% =================================================================

eval_20(Eq,RetType,Depth,Self,['if-unify',X,Y,Then,Else],Res):- !,
   eval_args(Eq,'Bool',Depth,Self,['==',X,Y],TF),
   (is_True(TF)
     -> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).


eval_20(Eq,RetType,Depth,Self,['if-equal',X,Y,Then,Else],Res):- !,

   ( \+ \+ (eval_args(Eq,'Bool',Depth,Self,['==',X,Y],TF),is_True(TF))
     -> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).


eval_20(Eq,RetType,Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).

eval_20(Eq,RetType,Depth,Self,['If',Cond,Then,Else],Res):- !,
   eval_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> eval_args(Eq,RetType,Depth,Self,Then,Res)
     ;  eval_args(Eq,RetType,Depth,Self,Else,Res)).

eval_20(Eq,RetType,Depth,Self,['If',Cond,Then],Res):- !,
   eval_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).

eval_20(Eq,RetType,Depth,Self,['if',Cond,Then],Res):- !,
   eval_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).


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


eval_20(Eq,RetType,_Dpth,_Slf,['car-atom',Atom],CAR_Y):- !, Atom=[CAR|_],!,do_expander(Eq,RetType,CAR,CAR_Y).
eval_20(Eq,RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).

eval_20(Eq,RetType,Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval_args(Eq,RetType,Depth,Self,A,AA), eval_args(Eq,RetType,Depth,Self,B,BB).

%eval_20(_Eq,_RetType,Depth,Self,['::'|PL],Prolog):-  maplist(as_prolog(Depth,Self),PL,Prolog),!.
%eval_20(_Eq,_RetType,Depth,Self,['@'|PL],Prolog):- as_prolog(Depth,Self,['@'|PL],Prolog),!.

eval_20(Eq,RetType,Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
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

call_in_shared_space(G):- call_in_thread(main,G).

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
    ;   writeln('Error: Invalid input.')
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
type_accepted_from(Into,From):-wdmsg(type_accepted_from(Into,From)).


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


eval_20(_Eq,_RetType,Depth,Self,['get-types',Val],TypeO):- !,
    get_types(Depth,Self,Val,TypeO).

% use default self
eval_20(Eq,RetType,Depth,Self,['get-type',Val,Self],Type):- current_self(Self), !,
    eval_20(Eq,RetType,Depth,Self,['get-type',Val],Type).

% use other space
eval_20(Eq,RetType,Depth,Self,['get-type',Val,Other],Type):- !,
    into_space(Depth,Self,Other,Space),
    eval_20(Eq,RetType,Depth,Space,['get-type',Val],Type).

eval_20(_Eq,_RetType,Depth,Self,['get-type',Val],Type):- is_list(Val), !,
    catch_metta_return(get_type(Depth,Self,Val,Type),TypeM),
    var(TypeM), Type \== '%Undefined%'.

eval_20(Eq,RetType,Depth,Self,['get-type',Val],TypeO):- !,
    if_or_else(get_type(Depth,Self,Val,Type),Type='%Undefined%'),
    %term_singletons(Type,[]),
    %Type\==[], Type\==Val,!,
    do_expander(Eq,RetType,Type,TypeO).

eval_20(Eq,RetType,Depth,Self,['get-type-space',Other,Val],Type):- !,
   into_space(Depth,Self,Other,Space),
   eval_20(Eq,RetType,Depth,Space,['get-type',Val],Type).

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
get_symbol_metatype(_Vl,'Bool','Grounded').
get_symbol_metatype(Val,_Want,Type):- nb_current(Val,NewVal),'get-metatype'(NewVal,Type).
get_symbol_metatype(_Vl,'%Undefined%','Symbol').
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

format_args_write(Arg,_) :- string(Arg), !, write(Arg).
format_args_write('#\\'(Arg),_) :- !, write(Arg).
format_args_write(Arg,_) :- write_src_woi(Arg).

format_nth_args([], _, _).
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
    format_args_write(Arg,Format),
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
   \+ is_list(EArgs),!,throw_metta_return(['Error',Args,'BadType']).

eval_20(Eq,RetType,_Depth,_Self,['flip'],Bool):-
   ignore(RetType='Bool'), !, as_tf(random(0,2,0),Bool),
   check_returnval(Eq,RetType,Bool).

eval_20( Eq, RetType, Depth, Self, [ 'parse' , L ] , Exp ):- !,
    eval_args( Eq, RetType, Depth, Self, L, Str ),
    once(parse_sexpr_metta1( Str, Exp )).

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
nb_bind(Name,Value):- nb_current(Name,Was),same_term(Value,Was),!.
nb_bind(Name,Value):- call_in_shared_space(nb_setval(Name,Value)),!.
eval_20(_Eq,_RetType,_Dpth,_Slf,['extend-py!',Module],Res):-  !, 'extend-py!'(Module,Res).
eval_20(Eq,RetType,Depth,Self,['register-module!',Dir],RetVal):- !,
     eval_20(Eq,'Directory',Depth,Self,Dir,Folder),
     register_module(Self,Folder),!,
     %Folder = RetVal,
     ignore(make_nop(RetType,Self,RetVal)).
eval_20(Eq,RetType,Depth,Self,['register-module!',Name,Dir],RetVal):- !,
     eval_20(Eq,'Symbol',Depth,Self,Name,ModuleName),
     eval_20(Eq,'Directory',Depth,Self,Dir,Folder),
     register_module(Self,ModuleName,Folder),!,
     %Folder = RetVal,
     ignore(make_nop(RetType,Self,RetVal)).


eval_20(Eq,RetType,Depth,Self,['include!',Other,File],RetVal):- !,
     into_space(Depth,Self,Other,Space), include_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
% from metta in Rust
eval_20(Eq,RetType,_Depth,Self,['include',File],RetVal):- !,
     include_metta(Self,File),!,
     make_nr(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['load-ascii',Other,File],RetVal):- !,
     into_space(Depth,Self,Other,Space), include_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['import!',Other,File],RetVal):- !,
     into_space(Depth,Self,Other,Space), import_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['load-file!',Other,File],RetVal):- !,
     into_space(Depth,Self,Other,Space), load_metta(Space,File),!,
     make_nr(Eq,RetType,RetVal).

make_nr(_Eq,_RetType,RetVal):- as_nop(RetVal).




eval_20(Eq,RetType,_Depth,_Slf,['bind!',Other,['new-space']],RetVal):- atom(Other),!,
  assert(was_asserted_space(Other)),
  make_nop(RetType,[],RetVal), check_returnval(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['bind!',Other,Expr],RetVal):- !,
   must_det_ll((into_name(Self,Other,Name),!,eval_args(Eq,RetType,Depth,Self,Expr,Value),
    nb_bind(Name,Value),  make_nop(RetType,Value,RetVal))),
   check_returnval(Eq,RetType,RetVal).
eval_20(Eq,RetType,Depth,Self,['pragma!',Other,Expr],RetVal):- !,
   must_det_ll((into_name(Self,Other,Name),nd_ignore((eval_args(Eq,RetType,Depth,Self,Expr,Value),
   set_option_value_interp(Name,Value))),  make_nop(RetType,Value,RetVal),
    check_returnval(Eq,RetType,RetVal))).
eval_20(Eq,RetType,_Dpth,Self,['transfer!',File],RetVal):- !, must_det_ll((include_metta(Self,File),
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

is_True(T):- atomic(T), T\=='False', T\==0.

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

bool_xor(A,B) :- (A == 'True'; B == 'True'), \+ (A == B).

eval_20(Eq,RetType,Depth,Self,['and',X,Y],TF):- !,
    as_tf(( (eval_args_true(Eq,RetType,Depth,Self,X),
             eval_args_true(Eq,RetType,Depth,Self,Y))), TF).


eval_20(Eq,RetType,Depth,Self,['or',X,Y],TF):- !,
  as_tf(( (eval_args_true(Eq,RetType,Depth,Self,X);
           eval_args_true(Eq,RetType,Depth,Self,Y))), TF).

eval_20(Eq,RetType,Depth,Self,['xor',X,Y],TF):- !,
  as_tf(  (eval_args_true(Eq,RetType,Depth,Self,X)),  XTF),  % evaluate X
  as_tf(  (eval_args_true(Eq,RetType,Depth,Self,Y)),  YTF),  % evaluate Y
  as_tf(  (bool_xor(XTF,YTF))              ,   TF).


eval_20(Eq,RetType,Depth,Self,['not',X],TF):- !,
   as_tf(( \+ eval_args_true(Eq,RetType,Depth,Self,X)), TF).


% ================================================
% === function / return of minimal metta
eval_20(Eq,RetType,Depth,Self,['function',X],Res):- !, gensym(return_,RetF),
  RetUnit=..[RetF,Res],
  catch(locally(nb_setval('$rettag',RetF),
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

eval_20(Eq,RetType,Depth,Self,['findall!',Template,X],ResL):- !,
   findall(Template,eval_args(Eq,RetType,Depth,Self,X,_),ResL).



eval_20(Eq,RetType,Depth,Self,['limit!',N,E],R):- !, eval_20(Eq,RetType,Depth,Self,['limit',N,E],R).
eval_20(Eq,RetType,Depth,Self,['limit',NE,E],R):-  !,
   eval_args('=','Number',Depth,Self,NE,N),
   limit(N,eval_ne(Eq,RetType,Depth,Self,E,R)).

eval_20(Eq,RetType,Depth,Self,['offset!',N,E],R):- !, eval_20(Eq,RetType,Depth,Self,['offset',N,E],R).
eval_20(Eq,RetType,Depth,Self,['offset',NE,E],R):-  !,
   eval_args('=','Number',Depth,Self,NE,N),
   offset(N,eval_ne(Eq,RetType,Depth,Self,E,R)).

eval_20(Eq,RetType,Depth,Self,['max-time!',N,E],R):- !, eval_20(Eq,RetType,Depth,Self,['max-time',N,E],R).
eval_20(Eq,RetType,Depth,Self,['max-time',NE,E],R):-  !,
   eval_args('=','Number',Depth,Self,NE,N),
   cwtl(N,eval_ne(Eq,RetType,Depth,Self,E,R)).


eval_20(Eq,RetType,Depth,Self,['call-cleanup!',NE,E],R):-  !,
   call_cleanup(eval_args(Eq,RetType,Depth,Self,NE,R),
                eval_args(Eq,_U_,Depth,Self,E,_)).

eval_20(Eq,RetType,Depth,Self,['setup-call-cleanup!',S,NE,E],R):-  !,
   setup_call_cleanup(
         eval_args(Eq,_,Depth,Self,S,_),
         eval_args(Eq,RetType,Depth,Self,NE,R),
         eval_args(Eq,_,Depth,Self,E,_)).

eval_20(Eq,RetType,Depth,Self,['with-output-to!',S,NE],R):-  !,
   eval_args(Eq,'Sink',Depth,Self,S,OUT),
   with_output_to_stream(OUT,
      eval_args(Eq,RetType,Depth,Self,NE,R)).



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
%finish_eval(Eq,RetType,Depth,Self,[V|Nil],[O]):- Nil==[], once(eval_args(Eq,RetType,Depth,Self,V,O)),V\=@=O,!.
finish_eval(Eq,RetType,Depth,Self,[H|T],[HH|TT]):- !,
    eval_args(Depth,Self,H,HH),
    finish_eval(Eq,RetType,Depth,Self,T,TT).
finish_eval(_Eq,_RetType,Depth,Self,T,TT):- eval_args(Depth,Self,T,TT).

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
%  METTLOG COMPILER PREDEFS
% =================================================================
% =================================================================
% =================================================================


eval_20(_Eq,_RetType,_Dpth,_Slf,['predicate-arity',F],A):- !,
   eval_for('Symbol',F,FF),
   predicate_arity(FF,A).
eval_20(_Eq,_RetType,_Dpth,_Slf,['function-arity',F],A):- !,
   eval_for('Symbol',F,FF),
   function_arity(FF,A).



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
       (trace,compile_metta_defn(KB,X,Len,Args,BodyFn,_ClauseU))))),
    % pfcNoWatch,
    true,!,
     notrace(catch((wdmsg(?-listing(X)),listing(X)),E,
    (!,write_src(E),fail))),!.


empty('Empty').
','(A,B,(AA,BB)):- eval_args(A,AA),eval_args(B,BB).
':'(A,B,[':',A,B]).
'<'(A,B,TFO):- as_tf(A<B,TF),!,TF=TFO.
'>'(A,B,TFO):- as_tf(A<B,TF),!,TF=TFO.
minus(A,B,C):- plus(B,C,A).


eval_20(Eq,RetType,Depth,Self,[AE|More],Res):-
    metta_compiled_predicate(Self,AE,Len),
    len_or_unbound(More,Len), Pred = AE,
    current_predicate(AE/Arity),
    maplist(as_prolog, More , Adjusted),!,
    eval_201(Eq,RetType,Depth,Self,Pred,Adjusted,Arity,Len,Res),
    nonvar(Res),
    check_returnval(Eq,RetType,Res).


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
eval_20(Eq,_ListOfRetType,Depth,Self,['TupleConcat',A,B],OO):- fail, !,
    eval_args(Eq,RetType,Depth,Self,A,AA),
    eval_args(Eq,RetType,Depth,Self,B,BB),
    append(AA,BB,OO).

% Temporarily in this file
eval_20(Eq,OuterRetType,Depth,Self,['range',A,B],OO):- fail, (is_list(A);is_list(B)),
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

eval_20(Eq,RetType,Depth,Self,['unique',Eval],RetVal):- !,
   term_variables(Eval+RetVal,Vars),
   no_repeats_var(YY),
   eval_20(Eq,RetType,Depth,Self,Eval,RetVal),YY=Vars.

eval_20(Eq,RetType,Depth,Self,['pred-unique',P2,Eval],RetVal):- !,
   no_repeats_var(P2,YY),
   eval_20(Eq,RetType,Depth,Self,Eval,RetVal),YY=RetVal.


eval_20(Eq,RetType,Depth,Self,['subtraction',Eval1,Eval2],RetVal):- !,
    lazy_subtraction(variant_by_type,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['pred-subtraction',P2,Eval1,Eval2],RetVal):- !,
    lazy_subtraction(P2,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['union',Eval1,Eval2],RetVal):- !,
    lazy_union(variant_by_type,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['pred-union',P2,Eval1,Eval2],RetVal):- !,
    lazy_union(P2,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

%eval_20(Eq,RetType,_Dpth,_Slf,['py-list',Atom_list],CDR_Y):-
% !, Atom=[_|CDR],!,do_expander(Eq,RetType,Atom_list, CDR_Y ).

eval_20(Eq,RetType,Depth,Self,['intersection',Eval1,Eval2],RetVal):- !,
    lazy_intersection(variant_by_type,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
                  RetVal2^eval_args(Eq,RetType,Depth,Self,Eval2,RetVal2),
                  RetVal).

eval_20(Eq,RetType,Depth,Self,['pred-intersection',P2,Eval1,Eval2],RetVal):- !,
    lazy_intersection(P2,RetVal1^eval_args(Eq,RetType,Depth,Self,Eval1,RetVal1),
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
eval_21(Eq,_RetType,Depth,Self,['Tuple-Count',List],Len):- fail,!,
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
   ((((eval_selfless(Eq,RetType,Depth,Self,LESS,Res),fake_notrace(LESS\==Res))))),!.

eval_40(Eq,RetType,Depth,Self,['+',N1,N2],N):- number(N1),!,
   eval_args(Eq,RetType,Depth,Self,N2,N2Res), fake_notrace(catch_err(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
eval_40(Eq,RetType,Depth,Self,['-',N1,N2],N):- number(N1),!,
   eval_args(Eq,RetType,Depth,Self,N2,N2Res), fake_notrace(catch_err(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
eval_40(Eq,RetType,Depth,Self,['*',N1,N2],N):- number(N1),!,
   eval_args(Eq,RetType,Depth,Self,N2,N2Res), fake_notrace(catch_err(N is N1*N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).

eval_20(_Eq,_RetType,_Depth,_Self,['rust',Bang,PredDecl],Res):- Bang == '!', !,
    rust_metta_run(exec(PredDecl),Res), nop(write_src(res(Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['rust',PredDecl],Res):- !,
    rust_metta_run((PredDecl),Res), nop(write_src(res(Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['rust!',PredDecl],Res):- !,
    rust_metta_run(exec(PredDecl),Res), nop(write_src(res(Res))).

eval_20(_Eq,_RetType,_Depth,_Self,['py-list',Arg],Res):- !,
  must_det_ll((py_list(Arg,Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['py-dict',Arg],Res):- !,
  must_det_ll((py_dict(Arg,Res))).
eval_20(_Eq,_RetType,_Depth,_Self,['py-tuple',Arg],Res):- !,
  must_det_ll((py_tuple(Arg,Res))).
eval_40(_Eq,_RetType,_D7epth,_Self,['py-atom',Arg],Res):- !,
  must_det_ll((py_atom(Arg,Res))).
eval_40(_Eq,_RetType,_Depth,_Self,['py-atom',Arg,Type],Res):- !,
  must_det_ll((py_atom_type(Arg,Type,Res))).
eval_40(_Eq,_RetType,_Depth,_Self,['py-dot',Arg1,Arg2],Res):- !,
  must_det_ll((py_dot([Arg1,Arg2],Res))).
eval_70(_Eq,_RetType,_Depth,_Self,['py-eval',Arg],Res):- !,
  must_det_ll((py_eval(Arg,Res))).

eval_40(Eq,RetType,Depth,Self,['length',L],Res):- !, eval_args(Depth,Self,L,LL),
   (is_list(LL)->length(LL,Res);Res=1),
   check_returnval(Eq,RetType,Res).


/*
eval_40(Eq,RetType,Depth,Self,[P,A,X|More],YY):- is_list(X),X=[_,_,_],simple_math(X),
   eval_selfless_2(X,XX),X\=@=XX,!,
   eval_40(Eq,RetType,Depth,Self,[P,A,XX|More],YY).
*/
%eval_40(Eq,RetType,_Dpth,_Slf,['==',X,Y],Res):-  !, subst_args(Eq,RetType,_Dpth,_Slf,['==',X,Y],Res).

eval_40(Eq,RetType,Depth,Self,[EQ, X,Y],Res):- EQ=='==', using_all_spaces, !,
    suggest_type(RetType,'Bool'),
    as_tf(eval_until_unify(Eq,_SharedType,Depth,Self,X,Y),Res).

eval_40(Eq,RetType,_Dpth,_Slf,[EQ,X,Y],Res):- EQ=='==', !,
    suggest_type(RetType,'Bool'),
    eq_unify(Eq,_SharedType, X, Y, Res).

eq_unify(_Eq,_SharedType, X, Y, TF):- as_tf(X=:=Y,TF),!.
eq_unify(_Eq,_SharedType, X, Y, TF):- as_tf( '#='(X,Y),TF),!.
eq_unify( Eq,  SharedType, X, Y, TF):- as_tf(eval_until_unify(Eq,SharedType, X, Y), TF).


eval_20(_Eq,RetType,_Dpth,_Slf,[EQ,X,Y],TF):- EQ=='===', !,
    suggest_type(RetType,'Bool'),
    as_tf(X==Y,TF).

eval_20(_Eq,RetType,_Dpth,_Slf,[EQ,X,Y],TF):- EQ=='====', !,
    suggest_type(RetType,'Bool'),
    as_tf(same_terms(X,Y),TF).


suggest_type(_RetType,_Bool).

naive_eval_args:-
    false.

eval_41(Eq,RetType,Depth,Self,[AE|More],Res):- naive_eval_args,!,
  maplist(must_eval_args(Eq,_,Depth,Self),More,Adjusted),
  eval_70(Eq,RetType,Depth,Self,[AE|Adjusted],Res),
  check_returnval(Eq,RetType,Res).

eval_41(Eq,RetType,Depth,Self,AEMore,ResOut):- \+ naive_eval_args,!,
  eval_adjust_args(Eq,RetType,ResIn,ResOut,Depth,Self,AEMore,AEAdjusted),
  if_trace((e;args),
     (AEMore\==AEAdjusted -> color_g_mesg('#773733',indentq2(Depth,AEMore -> AEAdjusted))
       ; nop(indentq2(Depth,same(AEMore))))),
  eval_70(Eq,RetType,Depth,Self,AEAdjusted,ResIn),
  check_returnval(Eq,RetType,ResOut).


eval_20(Eq,RetType,Depth,Self,X,Y):-
  (eval_40(Eq,RetType,Depth,Self,X,M)*-> M=Y ;
     % finish_eval(Depth,Self,M,Y);
    (eval_failed(Depth,Self,X,Y)*->true;X=Y)).
eval_40(Eq,RetType,Depth,Self,AEMore,ResOut):- eval_41(Eq,RetType,Depth,Self,AEMore,ResOut).
eval_70(Eq,RetType,Depth,Self,PredDecl,Res):-
    if_or_else(eval_maybe_python(Eq,RetType,Depth,Self,PredDecl,Res),
    if_or_else(eval_maybe_host_predicate(Eq,RetType,Depth,Self,PredDecl,Res),
    if_or_else(eval_maybe_host_function(Eq,RetType,Depth,Self,PredDecl,Res),
    if_or_else(eval_maybe_defn(Eq,RetType,Depth,Self,PredDecl,Res),
               eval_maybe_subst(Eq,RetType,Depth,Self,PredDecl,Res))))).


eval_all_args:- true_flag.
fail_missed_defn:- true_flag.
fail_on_constructor:- true_flag.


eval_adjust_args(Eq,RetType,ResIn,ResOut,Depth,Self,X,Y):-
  if_or_else((eval_all_args,eval_adjust_args2(Eq,RetType,ResIn,ResOut,Depth,Self,X,Y)),
             eval_adjust_args1(Eq,RetType,ResIn,ResOut,Depth,Self,X,Y)).

eval_adjust_args1(Eq,RetType,ResIn,ResOut,Depth,Self,[AE|More],[AE|Adjusted]):-
 adjust_args_90(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted).
adjust_args_90(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted):- \+ is_debugging(eval_args),!,
    adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted).
adjust_args_90(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted):-
   if_or_else(adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted),
      if_or_else(with_debug(eval_args,adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted),
             if_or_else(More=Adjusted,
                if_or_else((trace, throw(adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted)))))))).



eval_adjust_args2(Eq,_RetType,ResIn,ResOut,Depth,Self,[AE|More],[AE|Adjusted]):-
   maplist(must_eval_args(Eq,_,Depth,Self),More,Adjusted),
   ResIn = ResOut.


must_eval_args(Eq,RetType,Depth,Self,More,Adjusted):- \+ is_debugging(eval_args),!, eval_args(Eq,RetType,Depth,Self,More,Adjusted).
must_eval_args(Eq,RetType,Depth,Self,More,Adjusted):-
   (eval_args(Eq,RetType,Depth,Self,More,Adjusted)*->true;
      (with_debug(eval_args,eval_args(Eq,RetType,Depth,Self,More,Adjusted))*-> true;
         (
           %nl,writeq(eval_args(Eq,RetType,Depth,Self,More,Adjusted)),writeln('.'),
             (More=Adjusted -> true ;
                (trace, throw(must_eval_args(Eq,RetType,Depth,Self,More,Adjusted))))))).


eval_maybe_subst(Eq,RetType,Depth,Self,PredDecl,Res):- !,
  subst_args_here(Eq,RetType,Depth,Self,PredDecl,Res).


eval_maybe_subst(_Eq,_RetType,_Dpth,_Slf,[H|PredDecl],Res):- fail,
  is_rust_operation([H|PredDecl]),!, % run
  must_det_ll((rust_metta_run(exec([H|PredDecl]),Res),
  nop(write_src(res(Res))))).

eval_maybe_subst(_Eq,_RetType,_Dpth,_Slf,Res,Res):- nb_current(eval_maybe_subst,false),!.
eval_maybe_subst(Eq,RetType,Depth,Self,PredDecl,Res):-
  locally(nb_setval(eval_maybe_subst,false),
   finish_eval(Eq,RetType,Depth,Self,PredDecl,Res)).

:- nb_setval(eval_maybe_subst,true).
/*
eval_maybe_subst(Eq,RetType,Depth,Self,PredDecl,Res):-
   if_or_else((finish_eval(Eq,RetType,Depth,Self,PredDecl,Res),
       PredDec\=@=Res),
       subst_args(Eq,RetType,Depth,Self,PredDecl,Res)).
*/

/*
eval_70(Eq,RetType,Depth,Self,PredDecl,Res):-
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
is_system_pred(S):- atom(S),atom_concat(_,'!',S).
is_system_pred(S):- atom(S),atom_concat(_,'-fn',S).
is_system_pred(S):- atom(S),atom_concat(_,'-p',S).
%is_system_pred(S):- atom(S),upcase_symbol(S,U),downcase_symbol(S,U).

% eval_80/6: Evaluates a Python function call within MeTTa.
% Parameters:
% - Eq: denotes get-type, match, or interpret call.
% - RetType: Expected return type of the MeTTa function.
% - Depth: Recursion depth or complexity control.
% - Self: Context or environment for the evaluation.
% - [MyFun|More]: List with MeTTa function and additional arguments.
% - RetVal: Variable to store the result of the Python function call.
eval_maybe_python(Eq, RetType, _Depth, Self, [MyFun|More], RetVal) :-
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

% predicate inherited by system
eval_maybe_host_predicate(Eq,RetType,_Depth,_Self,[AE|More],TF):- allow_host_functions,
  once((is_system_pred(AE),
  length(More,Len),
  is_syspred(AE,Len,Pred))),
  \+ (atom(AE),   atom_concat(_,'-fn',AE)),
  %current_predicate(Pred/Len),
  %fake_notrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  %adjust_args(Depth,Self,AE,More,Adjusted),
  maplist(as_prolog, More , Adjusted),
  if_trace(host;prolog,print_tree(apply(Pred,Adjusted))),
  catch_warn(efbug(show_call,eval_call(apply(Pred,Adjusted),TF))),
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

s2ps(S,P):- S=='Nil',!,P=[].
s2ps(S,P):- \+ is_list(S),!,P=S.
s2ps([F|S],P):- atom(F),maplist(s2ps,S,SS),join_s2ps(F,SS,P),!.
s2ps(S,S):-!.
join_s2ps('Cons',[H,T],[H|T]):-!.
join_s2ps(F,Args,P):-atom(F),P=..[F|Args].

eval_call(S,TF):-
  s2ps(S,P), !,
  fbug(eval_call(P,'$VAR'('TF'))),as_tf(P,TF).

eval_call_fn(S,R):-
  s2ps(S,P), !,
  fbug(eval_call_fn(P,'$VAR'('R'))),as_tf(call(P,R),TF),TF\=='False'.

% function inherited from system
eval_maybe_host_function(Eq,RetType,_Depth,_Self,[AE|More],Res):- allow_host_functions,
  is_system_pred(AE),
  length([AE|More],Len),
  is_syspred(AE,Len,Pred),
  \+ (symbol(AE), symbol_concat(_,'-p',AE)), % thus maybe -fn or !
  %fake_notrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  %adjust_args(Depth,Self,AE,More,Adjusted),!,
  %Len1 is Len+1,
  %current_predicate(Pred/Len1),
  maplist(as_prolog,More,Adjusted),
  append(Adjusted,[Res],Args),!,
  if_trace(host;prolog,print_tree(apply(Pred,Args))),
  efbug(show_call,catch_warn(apply(Pred,Args))),
  check_returnval(Eq,RetType,Res).

% user defined function
%eval_20(Eq,RetType,Depth,Self,[H|PredDecl],Res):-
 %  fake_notrace(is_user_defined_head(Self,H)),!,
 %  eval_defn(Eq,RetType,Depth,Self,[H|PredDecl],Res).

/*eval_maybe_defn(Eq,RetType,Depth,Self,PredDecl,Res):-
    eval_defn(Eq,RetType,Depth,Self,PredDecl,Res).

eval_maybe_subst(Eq,RetType,Depth,Self,PredDecl,Res):-
    subst_args_h(Eq,RetType,Depth,Self,PredDecl,Res).
*/



:- if( \+  current_predicate( check_returnval / 3 )).
check_returnval(_,_RetType,_TF).
:- endif.

:- if( \+  current_predicate( adjust_args / 5 )).
adjust_args(_Depth,_Self,_V,VI,VI).
:- endif.


last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.




catch_warn(G):- (catch_err(G,E,(fbug(catch_warn(G)-->E),fail))).
catch_nowarn(G):- (catch_err(G,error(_,_),fail)).


% less Macro-ey Functions

%Metta
as_nop([]).
%mettalog
%as_nop('Empty').

as_nop(G,NoResult):-  G\=[_|_], rtrace_on_failure(G),!,
  as_nop(NoResult).
as_tf(G,TF):-  G\=[_|_], catch_nowarn((call(G)*->TF='True';TF='False')).
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
eval_selfless_1(['>',X,Y],TF):-!,as_tf(X>Y,TF).
eval_selfless_1(['<',X,Y],TF):-!,as_tf(X<Y,TF).
eval_selfless_1(['=>',X,Y],TF):-!,as_tf(X>=Y,TF).
eval_selfless_1(['<=',X,Y],TF):-!,as_tf(X=<Y,TF).
eval_selfless_1(['\\=',X,Y],TF):-!,as_tf(dif(X,Y),TF).

eval_selfless_2([F|_],_):- var(F),!,fail.
eval_selfless_2(['%',X,Y],TF):-!,eval_selfless_2(['mod',X,Y],TF).
eval_selfless_2(LIS,Y):-  fake_notrace(( ground(LIS),
   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   LIS\=[_], s2ps(LIS,IS))), fake_notrace(catch((Y is IS),_,fail)),!.


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
compare_selfless0(Lib,['\\=',X,Y],TF):-!,as_tf(Lib:{X \=Y}, TF).
compare_selfless0(Lib,['=',X,Y],TF):-!,as_tf(Lib:{X =Y}, TF).
compare_selfless0(Lib,['>',X,Y],TF):-!,as_tf(Lib:{X>Y},TF).
compare_selfless0(Lib,['<',X,Y],TF):-!,as_tf(Lib:{X<Y},TF).
compare_selfless0(Lib,['=>',X,Y],TF):-!,as_tf(Lib:{X>=Y},TF).
compare_selfless0(Lib,['<=',X,Y],TF):-!,as_tf(Lib:{X=<Y},TF).
compare_selfless0(Lib,[F|Stuff],TF):- P=..[F|Stuff],!,as_tf(Lib:{P},TF).

args_to_mathlib(XY,Lib):- sub_term(T,XY), var(T),get_attrs(T,XX),get_attrlib(XX,Lib).
args_to_mathlib(XY,clpr):- once((sub_term(T,XY), float(T))). % Reals
args_to_mathlib(XY,clpq):- once((sub_term(Rat,XY),compound(Rat),Rat='/'(_,_))).
args_to_mathlib(_,clpfd).


get_attrlib(XX,clpfd):- sub_var(clpfd,XX),!.
get_attrlib(XX,clpq):- sub_var(clpq,XX),!.
get_attrlib(XX,clpr):- sub_var(clpr,XX),!.

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


:-if(true).
:- nodebug(metta('defn')).

eval_maybe_defn(Eq,RetType,Depth,Self,X,Res):-
   \+  \+ (curried_arity(X,F,A),
           is_metta_type_constructor(Self,F,AA),
           ( \+ AA\=A ),!,
           if_trace(e,color_g_mesg('#772000',
                 indentq2(Depth,defs_none_cached((F/A/AA)=X))))),!,
   \+ fail_on_constructor,
   eval_constructor(Eq,RetType,Depth,Self,X,Res).
eval_maybe_defn(Eq,RetType,Depth,Self,X,Y):- can_be_ok(eval_maybe_defn,X),!,
      trace_eval(eval_defn_choose_candidates(Eq,RetType),'defn',Depth,Self,X,Y).

eval_constructor(Eq,RetType,Depth,Self,X,Res):-
   eval_maybe_subst(Eq,RetType,Depth,Self,X,Res).


eval_defn_choose_candidates(Eq,RetType,Depth,Self,X,Y):-
    findall((XX->B0),get_defn_expansions(Eq,RetType,Depth,Self,X,XX,B0),XXB0L),!,
    eval_defn_bodies(Eq,RetType,Depth,Self,X,Y,XXB0L).
eval_defn_choose_candidates(Eq,RetType,Depth,Self,X,Y):-
    eval_defn_bodies(Eq,RetType,Depth,Self,X,Y,[]),!.

multiple_typesigs(TypesSet):- is_list(TypesSet),
   length(TypesSet,Len),Len>1,maplist(is_list,TypesSet),!.


eval_defn_bodies(Eq,RetType,Depth,Self,X,Res,[]):- !,
   \+ \+ ignore((curried_arity(X,F,A),assert(is_metta_type_constructor(Self,F,A)))),!,
   if_trace(e,color_g_mesg('#773700',indentq2(Depth,defs_none(X)))),!,
   \+ fail_on_constructor,
   eval_constructor(Eq,RetType,Depth,Self,X,Res).

eval_defn_bodies(Eq,RetType,Depth,Self,X,Y,XXB0L):-
  if_trace(e,maplist(print_templates(Depth,'   '),XXB0L)),!,
  if_or_else((member(XX->B0,XXB0L), copy_term(XX->B0,USED),
    eval_defn_success(Eq,RetType,Depth,Self,X,Y,XX,B0,USED)),
    eval_defn_failure(Eq,RetType,Depth,Self,X,Y)).


eval_defn_success(Eq,RetType,Depth,Self,X,Y,XX,B0,USED):-
  X=XX, Y=B0, X\=@=B0,
  if_trace(e,color_g_mesg('#773700',indentq2(Depth,defs_used(USED)))),
  light_eval(Eq,RetType,Depth,Self,B0,Y),!.
eval_defn_failure(_Eq,_RetType,Depth,_Self,X,Res):-
  if_trace(e,color_g_mesg('#773701',indentq2(Depth,defs_failed(X)))),
  !, \+ fail_missed_defn, X=Res.


:-else.
eval_maybe_defn(Eq,RetType,Depth,Self,X,Y):- can_be_ok(eval_maybe_defn,X),!,
      trace_eval(eval_defn_choose_candidates(Eq,RetType),'defn',Depth,Self,X,Y).

eval_defn_choose_candidates(Eq,RetType,Depth,Self,X,Y):-
    findall((XX->B0),get_defn_expansions(Eq,RetType,Depth,Self,X,XX,B0),XXB0L),
    XXB0L\=[],!,
        Depth2 is Depth-1,
    if_trace((defn;metta_defn),
        maplist(print_templates(Depth,'   '),XXB0L)),!,
    member(XX->B0,XXB0L), X=XX, Y=B0, X\=@=B0,
    %(X==B0 -> trace; eval_args(Eq,RetType,Depth,Self,B0,Y)).
     light_eval(Depth2,Self,B0,Y).
eval_defn_choose_candidates(_Eq,_RetType,_Depth,_Self,_X,_Y):- \+ is_debugging(metta_defn),!,fail.
eval_defn_choose_candidates(_Eq,_RetType,_Depth,_Self,X,_Y):-
   color_g_mesg('#773700',write(no_def(X))),!,fail.
:- endif.

pl_clause_num(Head,Body,Ref,Index):-
    clause(Head,Body,Ref),
    nth_clause(Head,Index,Ref).

same_len_copy(Args,NewArgs):- length(Args,N),length(NewArgs,N).

get_defn_expansions(Eq,_RetType,_Depth,Self,[H|Args],[H|NewArgs],B0):- same_len_copy(Args,NewArgs),
   metta_eq_def(Eq,Self,[H|NewArgs],B0).

get_defn_expansions(Eq,RetType,Depth,Self,[[H|Start]|T1],[[H|NewStart]|NewT1],[Y|T1]):- is_list(Start),
    same_len_copy(Start,NewStart),
    X = [H|NewStart],
    findall((XX->B0),get_defn_expansions(Eq,RetType,Depth,Self,X,XX,B0),XXB0L),
    XXB0L\=[], if_trace((defn;metta_defn;eval_args),maplist(print_templates(Depth,'curry 1'),XXB0L)),!,
    member(XX->B0,XXB0L), X=XX, Y=B0, X\=@=B0,
    light_eval(Eq,RetType,Depth,Self,B0,Y),
    same_len_copy(T1,NewT1).

get_defn_expansions(Eq,RetType,Depth,Self,[[H|Start]|T1],RW,Y):- is_list(Start), append(Start,T1,Args),
  get_defn_expansions(Eq,RetType,Depth,Self,[H|Args],RW,Y),
  if_trace((defn;metta_defn;eval_args),indentq_d(Depth,'curry 2 ', [[[H|Start]|T1] ,'----->', RW])).

print_templates(Depth,_T,guarded_defn(Types,XX,B0)):-!,
   Depth2 is Depth+2,
    if_t(is_list(Types),indentq_d(Depth,'guarded',['->'|Types])),
    indentq_d(Depth2,'(=',XX),
    indentq_d(Depth2,' ',ste('',B0,')')).
print_templates(Depth,_T,XX->B0):-!,
    indentq_d(Depth,'(=',XX),
    indentq_d(Depth,'',ste('',B0,')')).
print_templates(Depth,T,XXB0):- ignore(indentq_d(Depth,'<<>>'(T),template(XXB0))),!.

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

cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).

cwtl(DL,Goal):- catch(call_with_time_limit(DL,Goal),time_limit_exceeded(_),fail).


%findall_eval(Eq,RetType,Depth,Self,X,L):- findall_eval(Eq,RetType,_RT,Depth,Self,X,L).
%findall_eval(Eq,RetType,Depth,Self,X,S):- findall(E,eval_ne(Eq,RetType,Depth,Self,X,E),S)*->true;S=[].
findall_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- self_eval(X),!,L=[X].
findall_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- typed_list(X,_Type,L),!.
findall_eval(Eq,RetType,Depth,Self,Funcall,L):-
   findall_ne(E,
    catch_metta_return(eval_args(Eq,RetType,Depth,Self,Funcall,E),E),L).

%bagof_eval(Eq,RetType,Depth,Self,X,L):- bagof_eval(Eq,RetType,_RT,Depth,Self,X,L).
%bagof_eval(Eq,RetType,Depth,Self,X,S):- bagof(E,eval_ne(Eq,RetType,Depth,Self,X,E),S)*->true;S=[].
bagof_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- self_eval(X),!,L=[X].
bagof_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- typed_list(X,_Type,L),!.
bagof_eval(Eq,RetType,Depth,Self,Funcall,L):-
   bagof_ne(E,
    catch_metta_return(eval_args(Eq,RetType,Depth,Self,Funcall,E),E),L).

setof_eval(Depth,Self,Funcall,L):- setof_eval('=',_RT,Depth,Self,Funcall,L).
setof_eval(Eq,RetType,Depth,Self,Funcall,S):- findall_eval(Eq,RetType,Depth,Self,Funcall,L),
   sort(L,S).

bagof_ne(E,Call,L):-
   bagof(E,(rtrace_on_error(Call), is_returned(E)),L).

findall_ne(E,Call,L):-
   findall(E,(rtrace_on_error(Call), is_returned(E)),L).

eval_ne(Eq,RetType,Depth,Self,Funcall,E):-
  ((eval_args(Eq,RetType,Depth,Self,Funcall,E))
    *-> is_returned(E);(fail,E=Funcall)).

is_returned(E):- notrace( \+ is_empty(E)).
is_empty(E):- notrace(( nonvar(E), sub_var('Empty',E))),!.


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
   eval_for(RetType,[OP|B],Res).

as_type(B,RetType,Res):- is_pro_eval_kind(RetType),
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
   get_type(20,Self,A,Was),
   can_assign(Was,Type).

eval_for(RetType,X,Y):-
  current_self(Self),
  eval_args('=',RetType,20,Self,X,Y).

%if_debugging(G):- ignore(call(G)).
if_debugging(_).
bcc:- trace,
  bc_fn([:,Prf,[in_tad_with,[sequence_variant,rs15],[gene,d]]],
     ['S',['S',['S',['S','Z']]]],
     OUT),
    write_src(prf=Prf), write_src(OUT).


bci:- trace,
  bc_impl([:,Prf,[in_tad_with,[sequence_variant,rs15],[gene,d]]],
     ['S',['S',['S',['S','Z']]]],
     OUT),
    write_src(prf=Prf), write_src(OUT).



bcm:- % trace,
  bc_impl([:,Prf,[member,_A,_B,_C]],
     ['S',['S',['S','Z']]],
     OUT),
    write_src(prf=Prf), write_src(OUT).


bc_fn(A,B,C):- %trace,
  same_types(A,C,_,A1,C1),
  as_type(B,'Nat',B1),
  bc_impl(A1,B1,C1).

bc_impl([:, _prf, _ccln], _, [:, _prf, _ccln]) :-
    if_debugging(println_impl(['bc-base', [:, _prf, _ccln]])),
    metta_atom('&kb', [:, _prf, _ccln]),
    if_debugging(println_impl(['bc-base-ground', [:, _prf, _ccln]])),
    true.

bc_impl([:, [_prfabs, _prfarg], _ccln], ['S', _k], [:, [_prfabs, _prfarg], _ccln]) :-
    if_debugging(println_impl(['bc-rec', [:, [_prfabs, _prfarg], _ccln], ['S', _k]])),
    bc_impl([:, _prfabs, ['->', _prms, _ccln]], _k, [:, _prfabs, [->, _prms, _ccln]]),
    bc_impl([:, _prfarg, _prms], _k, [:, _prfarg, _prms]).
















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
        type_fit_childs(_Eq,_Depth,_Self,_RetType,true,X,Y):- \+ is_list(X),iz_conz(X), trace, !,Y=X.
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

