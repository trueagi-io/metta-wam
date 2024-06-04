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



:- dynamic(is_cached_call/3).

% cached_call(ForSeconds, Call) - Attempts to use cached results for Call, or executes Call if no valid cache is present.
cached_call(ForSeconds, Call) :-
    get_time(CurrentTime), % Get the current time for cache validation.
    copy_term(Call,CallCopied),numbervars(CallCopied,0,_,[attvar(bind)]),
    NewerThan is CurrentTime - ForSeconds,
    ((  % Check if a valid cache entry exists.
        is_cached_call(CallCopied, CachedTime, Result),
        NewerThan > CachedTime)
    ->  % Use cached result if valid.
        true
    ;   % Otherwise, execute Call and update cache.
       (retractall(is_cached_call(CallCopied, _, _)), % Remove any existing cache for Call.
        call_ndet(Call,IsLast),
    nop(assertion(IsLast)),
        assertz(is_cached_call(CallCopied, CurrentTime, Result))) % Cache the new result.
    ),
    Call = Result. % Return the result.




debugging_metta(G):- notrace((is_debugging((eval))->ignore(G);true)).


:- nodebug(metta(eval)).

depth_to_use(InDepth, UseThis) :-
    Depth is abs(InDepth),
    UseThis is Depth mod 50,!.
depth_to_use(_InDepth, 5).





w_indent(Depth,Goal):- %must_be(integer,Depth),
  must_det_ll((
    depth_to_use(Depth, UseThis),
    format('~N'), setup_call_cleanup(i_this(UseThis),Goal, format('~N')))).
i_this(UseThis):-
  ignore(catch(forall(between(1,UseThis,_),write('  ')),_,true)),write(';;').


indentq2(Depth,Term):- w_indent(Depth,format('~q',[Term])),!.
indentq2(_Depth,Term):- format('~q',[Term]).

print_padded(_DR,_EX,_AR):- is_fast_mode,!.
print_padded(EX, DR, AR ):- integer(EX),integer(DR), EX>0,DR>0,
   nb_current('$print_padded',print_padded(EX, DR, _)),!,
   format("~|          |", []),
   DRA is abs(round(DR) mod 24),
   forall(between(2,DRA,_),write('   |')),write('    '),write(' '),write(AR).
print_padded(EX, DR, AR):-
   format("~|~` t~d~5+:~d~5+|", [EX, DR]),
   nb_setval('$print_padded',print_padded(EX, DR, AR)),
   DRA is abs(round(DR) mod 24),
   forall(between(1,DRA,_),write('   |')),write('-'),write(AR).

indentq_d(_DR,_EX,_AR):- is_fast_mode,!.
indentq_d(Depth,Prefix4, Message):-
    flag(eval_num,EX0,EX0),
    EX is EX0 mod 500,
    DR is 99 - (Depth mod 100),
    indentq(DR,EX,Prefix4,Message).

indentq(_DR,_EX,_AR,_Term):- is_fast_mode,!.
indentq(DR,EX,AR,retval(Term)):-nonvar(Term),!,indentq(DR,EX,AR,Term).
indentq(DR,EX,AR,[E,Term]):- E==e,!,indentq(DR,EX,AR,Term).
%indentq(_DR,_EX,_AR,_Term):- flag(trace_output_len,X,X+1), XX is (X mod 1000), XX>100,!.

indentq(DR,EX,AR,ste(S,Term,E)):- !, indentq(DR,EX,AR,S,Term,E).
indentq(DR,EX,AR,Term):- indentq(DR,EX,AR,'',Term,'').

indentq(DR,EX,AR,S,Term,E):-
        setup_call_cleanup(
             notrace(format('~N;')),
   (wots(Str,indentq0(DR,EX,AR,S,Term,E)),
    newlines_to_spaces(Str,SStr),write(SStr)),
   notrace(format('~N'))).
newlines_to_spaces(Str,SStr):- atomics_to_string(L,'\n',Str),atomics_to_string(L,' ',SStr).

indentq0(DR,EX,AR,S,Term,E):-
             as_trace((
               print_padded(EX, DR, AR),format(S,[]),with_indents(false,write_src(Term)),
               format(E,[]))).


reset_eval_num:- flag(eval_num,_,0),flag(trace_output_len,_,0).
reset_only_eval_num:- flag(eval_num,_,0).

is_fast_mode:- fail, \+ is_debugging(eval),!.

%ignore_trace_once(Goal):- !, call(Goal).
ignore_trace_once(Goal):- ignore(notrace(catch( ignore( Goal), _, fail))),!.
%ignore_trace_once(Goal):- must_det_ll(Goal).

as_trace(Goal):-
  ignore_trace_once( \+ with_no_screen_wrap(color_g_mesg('#2f2f2f', Goal))).

with_no_screen_wrap(Goal) :-!,call(Goal).
with_no_screen_wrap(Goal) :- with_no_wrap(6000, Goal).

with_no_wrap(Cols, Goal) :-
    % Setup: Save current terminal settings and disable line wrapping
    setup_call_cleanup(
        begin_no_wrap(Cols, OriginalCols, OriginalRows),
        Goal,
        % Cleanup: Restore original terminal settings
        end_no_wrap(OriginalCols, OriginalRows)
    ).

begin_no_wrap(Cols, OriginalCols, OriginalRows) :-
    cached_call(30.0, get_current_terminal_settings(OriginalCols, OriginalRows)), % Get current settings
    set_terminal_size(Cols, OriginalRows), % Attempt to set new width; rows remain unchanged
    format('~s', ["\e[?7l"]). % Disable line wrapping

end_no_wrap(OriginalCols, OriginalRows) :-
    set_terminal_size(OriginalCols, OriginalRows), % Restore original size
    format('~s', ["\e[?7h"]). % Re-enable line wrapping

get_current_terminal_settings(Cols, Rows) :-
    % Use 'stty size' to get the current dimensions of the terminal
    process_create(path(stty), ['size'], [stdout(pipe(Stream))]),
    read_line_to_string(Stream, SizeStr),
    close(Stream),
    split_string(SizeStr, " ", "", [RowsStr, ColsStr]),
    number_string(Rows, RowsStr),
    number_string(Cols, ColsStr),!.
get_current_terminal_settings(_, _).

set_terminal_size(Cols, Rows) :-
    % Conceptual; actual resizing may not work in all terminals
    if_t(integer(Cols),
      if_t(integer(Rows),format('~s~w;~w~s', ["\e[8;", Rows, Cols, "t"]))).


with_debug(Flag,Goal):- is_debugging(Flag),!, call(Goal).
with_debug(Flag,Goal):- reset_only_eval_num,
  setup_call_cleanup(set_debug(Flag,true),call(Goal),set_debug(Flag,false)).

flag_to_var(Flag,Var):- atom(Flag), \+ atom_concat('trace-on-',_,Flag),!,atom_concat('trace-on-',Flag,Var).
flag_to_var(metta(Flag),Var):- !, nonvar(Flag), flag_to_var(Flag,Var).
flag_to_var(Flag,Var):- Flag=Var.

set_debug(metta(Flag),TF):- nonvar(Flag),!,set_debug(Flag,TF).
%set_debug(Flag,Val):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,set_debug(Var,Val).


set_debug(Flag,TF):- TF == 'True',!,set_debug(Flag,true).
set_debug(Flag,TF):- TF == 'False',!,set_debug(Flag,false).
set_debug(Flag,true):- !, debug(metta(Flag)). %,flag_to_var(Flag,Var),set_fast_option_value(Var,true).
set_debug(Flag,false):- nodebug(metta(Flag)). %,flag_to_var(Flag,Var),set_fast_option_value(Var,false).

if_trace(Flag,Goal):-
   notrace(real_notrace((catch_err(ignore((is_debugging(Flag),Goal)),E,
         fbug(E-->if_trace(Flag,Goal)))))).


is_showing(Flag):- fast_option_value(Flag,'silent'),!,fail.
is_showing(Flag):- is_verbose(Flag),!.
is_showing(Flag):- fast_option_value(Flag,'show'),!.

if_show(Flag,Goal):- real_notrace((catch_err(ignore((is_showing(Flag),Goal)),E,
                        fbug(E-->if_show(Flag,Goal))))).


fast_option_value(N,V):- atom(N),current_prolog_flag(N,V).

is_verbose(Flag):- fast_option_value(Flag,'silent'),!,fail.
is_verbose(Flag):- fast_option_value(Flag,'verbose'),!.
is_verbose(Flag):- is_debugging(Flag),!.

if_verbose(Flag,Goal):- real_notrace((catch_err(ignore((is_verbose(Flag),Goal)),E,
                            fbug(E-->if_verbose(Flag,Goal))))).



%maybe_efbug(SS,G):- efbug(SS,G)*-> if_trace(eval,fbug(SS=G)) ; fail.
maybe_efbug(_,G):- call(G).
%efbug(P1,G):- call(P1,G).
efbug(_,G):- call(G).



%is_debugging(Flag):- var(Flag),!,fail.
%is_debugging(Flag):- !, fail.

is_debugging_always(_Flag):-!.

is_debugging(Flag):- var(Flag),!,fail.
is_debugging((A;B)):- !, (is_debugging(A) ; is_debugging(B) ).
is_debugging((A,B)):- !, (is_debugging(A) , is_debugging(B) ).
is_debugging(not(Flag)):- !,  \+ is_debugging(Flag).
is_debugging(Flag):- Flag== false,!,fail.
is_debugging(Flag):- Flag== true,!.
%is_debugging(e):- is_testing, \+ fast_option_value(compile,'full'),!.
%is_debugging(e):- is_testing,!.
%is_debugging(eval):- is_testing,!.
%is_debugging(_):-!,fail.
is_debugging(Flag):- fast_option_value(Flag,'debug'),!.
is_debugging(Flag):- fast_option_value(Flag,'trace'),!.
is_debugging(Flag):- debugging(metta(Flag),TF),!,TF==true.
%is_debugging(Flag):- debugging(Flag,TF),!,TF==true.
%is_debugging(Flag):- once(flag_to_var(Flag,Var)),
%  (fast_option_value(Var,true)->true;(Flag\==Var -> is_debugging(Var))).

% overflow = trace
% overflow = fail
% overflow = continue
% overflow = debug

%trace_eval(P4,_TN,D1,Self,X,Y):- is_fast_mode,!, call(P4,D1,Self,X,Y).
%trace_eval(P4,TN,D1,Self,X,Y):- \+ is_debugging(TN),!, call(P4,D1,Self,X,Y).
trace_eval(P4,TNT,D1,Self,X,Y):-
   must_det_ll((
   notrace((
   flag(eval_num,EX0,EX0+1),
   EX is EX0 mod 500,
   DR is 99 - (D1 mod 100),
   PrintRet = _,
   option_else('trace-length',Max,500),
   option_else('trace-depth',DMax,30))),
   quietly((if_t((nop(stop_rtrace),EX>Max), (set_debug(eval,false),MaxP1 is Max+1,
         %set_debug(overflow,false),
         nop(format('; Switched off tracing. For a longer trace: !(pragma! trace-length ~w)',[MaxP1])),
         nop((start_rtrace,rtrace)))))),
   nop(notrace(no_repeats_var(NoRepeats))))),

   ((sub_term(TN,TNT),TNT\=TN)-> true ; TNT=TN),
   %if_t(DR<DMax, )
   ( \+ \+ if_trace((eval;TNT), (PrintRet=1,
      indentq(DR,EX, '-->',[TN,X]))) ),

   Ret=retval(fail),!,

   (Display= ( \+ \+ (flag(eval_num,EX1,EX1+1),
                ((Ret\=@=retval(fail),nonvar(Y))
                -> indentq(DR,EX1,'<--',[TN,Y])
                 ; indentq(DR,EX1,'<--',[TN,Ret]))))),

   call_cleanup((
      (call(P4,D1,Self,X,Y)*->nb_setarg(1,Ret,Y);
        (fail,trace,(call(P4,D1,Self,X,Y)))),
      ignore((notrace(( \+ (Y\=NoRepeats), nb_setarg(1,Ret,Y)))))),
    % cleanup
      ignore((PrintRet==1 -> ignore(Display) ;
       (notrace(ignore((( % Y\=@=X,
         if_t(DR<DMax,if_trace((eval;TN),ignore(Display))))))))))),
   Ret\=@=retval(fail).

%  (Ret\=@=retval(fail)->true;(fail,trace,(call(P4,D1,Self,X,Y)),fail)).



:- set_prolog_flag(expect_pfc_file,unknown).

% =======================================================
/*
%
%= predicates to examine the state of pfc
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/pfc_list_triggers.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
pfc_listing_module:- nop( module(pfc_listing,
          [ draw_line/0,
            loop_check_just/1,
            pinfo/1,
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            pfc_list_triggers_types/1,
            pfc_list_triggers_nlc/1,
            pfc_list_triggers_1/1,
            pfc_list_triggers_0/1,
            pfc_list_triggers/1,
            pfc_contains_term/2,
            pfc_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %pfcWhyBrouse/2,
            %pfcWhy1/1,
            %pfcWhy/1,
            %pfcWhy/0,
            pp_rules/0,
            pfcPrintSupports/0,
             pfcPrintTriggers/0,
            print_db_items/1,
            print_db_items/2,
            print_db_items/3,
            print_db_items/4,
            print_db_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            pfc_listing_file/0
          ])).

%:- include('pfc_header.pi').

:- endif.


    :- op(500,fx,'~').
    :- op(1050,xfx,('==>')).
    :- op(1050,xfx,'<==>').
    :- op(1050,xfx,('<-')).
    :- op(1100,fx,('==>')).
    :- op(1150,xfx,('::::')).

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).



:- multifile((
              user:portray/1,
    user:prolog_list_goal/1,
    user:prolog_predicate_name/2,
    user:prolog_clause_name/2)).

:- dynamic
    user:portray/1.

% :- dynamic(whybuffer/2).



%=

% %  lqu is semidet.
%
% Lqu.
%
lqu :- listing(que/2).


:- ensure_loaded(metta_pfc_base).


%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author : Dave Matuszek, dave@prc.unisys.com
%   Author : Douglas R. Miles, dmiles@teknowledge.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh
%   for Pfc.

:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic   pfcWarnings/1.

:- pfcDefault(pfcWarnings(_), pfcWarnings(true)).

% %  predicates to examine the state of pfc

pfcQueue :- listing(pfcQueue/1).

pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports,!.

printLine:- ansi_format([underline],"~N=========================================~n",[]).

% % pfcPrintFacts ...

pfcPrintFacts :- pfcPrintFacts(_,true).


pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  printLine,
  pfcPrintf("User added facts:~n",[]),
  pfcPrintitems(User),
  printLine,
  pfcPrintf("MettaLog-Pfc added facts:~n",[]),
  pfcPrintitems(Pfc),
  printLine,!.


% %  printitems clobbers it''s arguments - beware!

pfcPrintitems([]).
pfcPrintitems([H|T]) :-
  % numbervars(H,0,_),
  %format('~N ~p.',[H]),
  \+ \+ ( pretty_numbervars(H,H1),format(" ",[]),portray_clause_w_vars(H1)),
  pfcPrintitems(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  matches_why_UU(UU),
  pfcGetSupport(H,UU),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcPrintRules :-
  printLine,
  pfcPrintf("Rules:...~n",[]),
  bagof_or_nil((P==>Q),clause((P==>Q),true),R1),
  pfcPrintitems(R1),
  bagof_or_nil((P<==>Q),clause((P<==>Q),true),R2),
  pfcPrintitems(R2),
  bagof_or_nil((P<-Q),clause((P<-Q),true),R3),
  pfcPrintitems(R3),
  printLine.

pfcGetTrigger(Trigger):- pfc_call(Trigger).


% %   pfcPrintTriggers is semidet.
%
% Pretty Print Triggers.
%
pfcPrintTriggers :-
     print_db_items("Positive triggers", '$pt$'(_,_)),
     print_db_items("Negative triggers", '$nt$'(_,_,_)),
     print_db_items("Goal triggers",'$bt$'(_,_)).

pp_triggers:-pfcPrintTriggers.
%=

% %  pfcPrintSupports is semidet.
%
% Pretty Print Supports.
%
pfcPrintSupports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n",[]),
  setof_or_nil((P =< S), (pfcGetSupport(P,S), \+ pp_filtered(P)),L),
  pp_items('Support',L),
  draw_line,!.
pp_supports:- pfcPrintSupports.

pp_filtered(P):-var(P),!,fail.
pp_filtered(_:P):- !, pp_filtered(P).
pp_filtered(P):- safe_functor(P,F,A),F\==(/),!,pp_filtered(F/A).
pp_filtered(F/_):-F==pfc_prop.



pfcFact(P) :- pfcFact(P,true).

% %  pfcFact(P,C) is true if fact P was asserted into the database via
% %  pfcAdd and contdition C is satisfied.  For example, we might do:
% %
% %   pfcFact(X,pfcUserFact(X))
% %

pfcFact(F,C):-
  filter_to_pattern_call(F,P,Call),
  pfcFact1(P,C),pfcCallSystem(Call).

pfcFact1(P,C):-
  pfcGetSupport(P,_),
  pfcType(P,fact(_)),
  pfcCallSystem(C).


% %  pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

% %  pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof_or_nil(P,pfcFact(P,C),L).

brake(X) :-  pfcCallSystem(X), ibreak.

% % 
% %
% %  predicates providing a simple tracing facility
% %

pfcTraceAdd(P) :-
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd('$pt$'(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd('$nt$'(_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).


pfcTraceAddPrint(P,S) :-
  pfcIsTraced(P),
  !,
  \+ \+
     (pretty_numbervars(P,Pcopy),
      % numbervars(Pcopy,0,_),
      matches_why_UU(UU),
      (S=UU
           -> pfcPrintf("Adding (u) ~@",[fmt_cl(Pcopy)])
            ; pfcPrintf("Adding ~@",[fmt_cl(Pcopy)]))).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  pfcSpied(P,+) ->
   (pretty_numbervars(P,Pcopy),
    % numbervars(Pcopy,0,_),
    pfcPrintf("Breaking on pfcAdd(~p)",[Pcopy]),
    ibreak)
   ; true.

pfcTraceRem('$pt$'(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem('$nt$'(_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (pfcIsTraced(P)
     -> pfcPrintf("Removing: ~p.",[P])
      ; true),
  (pfcSpied(P,-)
   -> (pfcPrintf("Breaking on pfcRem(~p)",[P]),
       ibreak)
   ; true).

pfcIsTraced(P):- pfcIsNotTraced(P),!,fail.
pfcIsTraced(P):- compound_eles(1,P,Arg), pfcTraced(Arg).
pfcIsNotTraced(P):- compound_eles(1,P,Arg), pfcIgnored(Arg).

:- dynamic(pfcIgnored/1).

compound_eles(Lvl,P,Arg):- var(P),!,get_attr(P,A,AV),compound_eles(Lvl,attvar(A,AV),Arg).
compound_eles(Lvl,P,Arg):- (\+ compound(P);Lvl<1),!, Arg=P.
compound_eles(Lvl,P,Arg):- LvlM1 is Lvl-1, compound_eles(P,E), compound_eles(LvlM1,E,Arg).

compound_eles(P,E):-is_list(P),!,member(E,P).
compound_eles(P,E):-compound(P),compound_name_arguments(P,F,Args),!,member(E,[F|Args]).

mpred_trace_exec:- pfcWatch,pfcTrace.
mpred_notrace_exec:- pfcNoTrace,pfcNoWatch.

pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  assert(pfcTraced(Form)).

pfcTrace(Form,Condition) :-
  assert((pfcTraced(Form) :- Condition)).

pfcSpy(Form) :- pfcSpy(Form,[+,-],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[H|T],Condition) :-
  !,
  pfcSpy1(Form,H,Condition),
  pfcSpy(Form,T,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  assert((pfcSpied(Form,Mode) :- Condition)).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :-
  clause(pfcSpied(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

% needed:  pfcTraceRule(Name)  ...


% if the correct flag is set, trace exection of Pfc

pfcTraceMsg(Msg):- pfcTraceMsg('~p',[Msg]).
pfcTraceMsg(Msg,Args) :-
    pfcTraceExecution,
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(Msg,Args) :-
    member(P,Args),pfcIsTraced(P),
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(_Msg,_Args).


pfcPrintf(Msg,Args) :-
  pfcPrintf(user_output, Msg,Args).

pfcPrintf(Where,Msg,Args) :-
  format(Where,'~N',[]),
  with_output_to(Where,
    color_g_mesg_ok(blue,format(Msg,Args))).


pfcWatch :- clause(pfcTraceExecution,true),!.
pfcWatch :- assert(pfcTraceExecution).

pfcNoWatch :-  retractall(pfcTraceExecution).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :-
  format("~N~nERROR/Pfc: ",[]),
  format(Msg,Args).

% %
% %  These control whether or not warnings are printed at all.
% %    pfcWarn.
% %    nopfcWarn.
% %
% %  These print a warning message if the flag pfcWarnings is set.
% %    pfcWarn(+Message)
% %    pfcWarn(+Message,+ListOfArguments)
% %

pfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

nopfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(false)).

pfcWarn(Msg) :-  pfcWarn('~p',[Msg]).

pfcWarn(Msg,Args) :-
  pfcWarnings(true),
  !,
  ansi_format([underline,fg(red)],"~N==============WARNING/Pfc================~n",[]),
  ansi_format([fg(yellow)],Msg,Args),
  printLine.
pfcWarn(_,_).

% %
% %  pfcWarnings/0 sets flag to cause pfc warning messages to print.
% %  pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
% %

pfcWarnings :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

pfcNoWarnings :-
  retractall(pfcWarnings(_)).


%=

% %  pp_facts is semidet.
%
% Pretty Print Facts.
%
pp_facts :- pp_facts(_,true).


%=

% %  pp_facts( ?Pattern) is semidet.
%
% Pretty Print Facts.
%
pp_facts(Pattern) :- pp_facts(Pattern,true).


%=

% %  pp_facts( ?P, ?C) is semidet.
%
% Pretty Print Facts.
%
pp_facts(P,C) :-
  pfcFacts(P,C,L),
  pfc_classify_facts(L,User,Pfc,_Rule),
  draw_line,
  fmt("User added facts:",[]),
  pp_items(user,User),
  draw_line,
  draw_line,
  fmt("MettaLog-Pfc added facts:",[]),
  pp_items(system,Pfc),
  draw_line.



%=

% %  pp_deds is semidet.
%
% Pretty Print Deds.
%
pp_deds :- pp_deds(_,true).


%=

% %  pp_deds( ?Pattern) is semidet.
%
% Pretty Print Deds.
%
pp_deds(Pattern) :- pp_deds(Pattern,true).


%=

% %  pp_deds( ?P, ?C) is semidet.
%
% Pretty Print Deds.
%
pp_deds(P,C) :-
  pfcFacts(P,C,L),
  pfc_classify_facts(L,_User,Pfc,_Rule),
  draw_line,
  fmt("MettaLog-Pfc added facts:",[]),
  pp_items(system,Pfc),
  draw_line.


show_deds_w(F):- pp_deds(F).


show_info(F) :-
    pfcFacts(_,true,L),
    include(sub_functor(F),L,FL),
    pfc_classify_facts(FL,User,Pfc,_Rule),
    draw_line,
    fmt("User added facts with ~q:",[F]),
    pp_items(user,User),
    draw_line,
    draw_line,
    fmt("MettaLog-Pfc added facts with ~q:",[F]),
    pp_items(system,Pfc),
    draw_line.


%maybe_filter_to_pattern_call(F,P,true):- var(F),!,var(P),P=F.
maybe_filter_to_pattern_call(F,_,true):- var(F),!,fail.
maybe_filter_to_pattern_call(F,P,true):- atom(F),!, (P=F ; freeze(P,(P\==F,sub_functor(F,P)))).
maybe_filter_to_pattern_call(F,P,true):- \+ compound(F),!, P=_ ; freeze(P,(P\==F,sub_functor(F,P))).
maybe_filter_to_pattern_call(F/A,P,true):-!, freeze(P,(P\==F,sub_functor(F/A,P))).
%maybe_filter_to_pattern_call(F,P,true):-P=F.

filter_to_pattern_call(F,P,Call):-
   maybe_filter_to_pattern_call(F,P,Call)*->true;alt_filter_to_pattern_call(F,P,Call).
alt_filter_to_pattern_call(P,P,true).

sub_functor(F-UnF,Term):- !, sub_functor(F,Term), \+ sub_functor(UnF,Term).
sub_functor(F,Term):- var(F),!,sub_var(F,Term),!.
sub_functor(F/A,Term):- !,sub_term(E,Term),compound(E),compound_name_arity(E,F,A).
sub_functor(F,Term):- sub_term(E,Term),E=@=F,!.
sub_functor(F,Term):- sub_term(E,Term),compound(E),compound_name_arity(E,FF,AA),(AA==F;FF==F).

%=

% %  pp_items( ?Type, :TermH) is semidet.
%
% Pretty Print Items.
%
pp_items(_Type,[]):-!.
pp_items(Type,[H|T]) :-
  ignore(pp_item(Type,H)),!,
  pp_items(Type,T).
pp_items(Type,H) :- ignore(pp_item(Type,H)).

:- thread_local(t_l:print_mode/1).

%=

% %  pp_item( ?MM, :TermH) is semidet.
%
% Pretty Print Item.
%
pp_item(_M,H):-pp_filtered(H),!.
pp_item(MM,(H:-B)):- B ==true,pp_item(MM,H).
pp_item(MM,H):- flag(show_asserions_offered,X,X+1),find_and_call(get_print_mode(html)), ( \+ \+ if_defined(pp_item_html(MM,H))),!.


pp_item(MM,'$spft$'(W0,U,ax)):- W = (_KB:W0),!,pp_item(MM,U:W).
pp_item(MM,'$spft$'(W0,F,U)):- W = (_KB:W0),atom(U),!,    fmt('~N%~n',[]),pp_item(MM,U:W), fmt('rule: ~p~n~n', [F]),!.
pp_item(MM,'$spft$'(W0,F,U)):- W = (_KB:W0),         !,   fmt('~w~nd:       ~p~nformat:    ~p~n', [MM,W,F]),pp_item(MM,U).
pp_item(MM,'$nt$'(Trigger0,Test,Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger(-): ~p~ntest: ~p~nbody: ~p~n', [MM,Trigger,Test,Body]).
pp_item(MM,'$pt$'(F0,Body)):- F = (_KB:F0),             !,fmt('~w p-trigger(+):~n', [MM]), pp_item('',(F:-Body)).
pp_item(MM,'$bt$'(F0,Body)):- F = (_KB:F0),             !,fmt('~w b-trigger(?):~n', [MM]), pp_item('',(F:-Body)).


pp_item(MM,U:W):- !,format(string(S),'~w  ~w:',[MM,U]),!, pp_item(S,W).
pp_item(MM,H):- \+ \+ (( get_clause_vars_for_print(H,HH),fmt("~w ~p~N",[MM,HH]))).


%=

% %  get_clause_vars_for_print( ?HB, ?HB) is semidet.
%
% Get Clause Variables For Print.
%
get_clause_vars_for_print(HB,HB):- ground(HB),!.
get_clause_vars_for_print(I,I):- is_listing_hidden(skipVarnames),fail.
get_clause_vars_for_print(H0,MHB):- get_clause_vars_copy(H0,MHB),H0\=@=MHB,!.
get_clause_vars_for_print(HB,HB):- numbervars(HB,0,_,[singletons(true),attvars(skip)]),!.

%=

% %  pfc_classify_facts( :TermH, ?User, :TermPfc, ?H) is semidet.
%
% Managed Predicate Classify Facts.
%
pfc_classify_facts([],[],[],[]).

pfc_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(mfl4(_VarNameZ,_,_,_),ax)),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],User,[H|Pfc],Rule) :-
  pfc_classify_facts(T,User,Pfc,Rule).



%=

% %  print_db_items( ?T, ?I) is semidet.
%
% Print Database Items.
%
print_db_items(T, I):-
    draw_line,
    fmt("~N~w ...~n",[T]),
    print_db_items(I),
    draw_line,!.


%=

% %  print_db_items( ?I) is semidet.
%
% Print Database Items.
%
print_db_items(F/A):-number(A),!,safe_functor(P,F,A),!,print_db_items(P).
print_db_items(H):- bagof(H,clause(H,true),R1),pp_items((:),R1),R1\==[],!.
print_db_items(H):- \+ current_predicate(_,H),!.
print_db_items(H):- catch( ('$find_predicate'(H,_),call_u(listing(H))),_,true),!,nl,nl.


%=

% %  pp_rules is semidet.
%
% Pretty Print Rules.
%
pp_rules :-
   print_db_items("Forward Rules",(_ ==> _)),
   print_db_items("Bidirectional Rules",(_ <==> _)),
   print_db_items("Implication Rules",=>(_ , _)),
   print_db_items("Bi-conditional Rules",<=>(_ , _)),
   print_db_items("Backchaining Rules",(_ <- _)),
   print_db_items("Positive Facts",(==>(_))),
   print_db_items("Negative Facts",(~(_))).


%=


% %  draw_line is semidet.
%
% Draw Line.
%
draw_line:- \+ thread_self_main,!.
draw_line:- printLine,!.
draw_line:- (t_l:print_mode(H)->true;H=unknown),fmt("~N% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %~n",[]),H=H.

 :- meta_predicate loop_check_just(0).

%=

% %  loop_check_just( :GoalG) is semidet.
%
% Loop Check Justification.
%
loop_check_just(G):- loop_check(G,ignore(arg(1,G,[]))).


%=

% %  show_pred_info( ?F) is semidet.
%
% Show Predicate Info.
%
/*
show_pred_info(PI):-
   ((
       pi_to_head_l(PI,Head),
       % doall(show_call(why,call_u(isa(Head,_)))),
        safe_functor(Head,F,_),
        doall(show_call(why,call_u(isa(F,_)))),
       ((current_predicate(_,M:Head), (\+ predicate_property(M:Head,imported_from(_))))
          -> show_pred_info_0(M:Head);
             wdmsg_pretty(cannot_show_pred_info(Head))))),!.
*/

%=

% %  show_pred_info_0( ?Head) is semidet.
%
% show Predicate info  Primary Helper.
%
show_pred_info_0(Head):-
        doall(show_call(why,predicate_property(Head,_))),
        (has_cl(Head)->doall((show_call(why,clause(Head,_))));quietly((listing(Head)))),!.


% ===================================================
% Pretty Print Formula
% ===================================================



%=

% %  print_db_items( ?Title, ?Mask, ?What) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,What):-print_db_items(Title,Mask,Mask,What).

%=

% %  print_db_items( ?Title, ?Mask, ?SHOW, ?What0) is semidet.
%
% Print Database Items.
%
print_db_items(Title,Mask,SHOW,What0):-
     get_pi(Mask,H),get_pi(What0,What),
     format(atom(Showing),'~p for ~p...',[Title,What]),
     statistics(cputime,Now),Max is Now + 2,!,
       gripe_time(1.0,
         doall((once(statistics(cputime,NewNow)),NewNow<Max,clause_or_call(H,B),
             quietly(pfc_contains_term(What,(H:-B))),
             flag(print_db_items,LI,LI+1),
             ignore(quietly(pp_item(Showing,SHOW)))))),
     ignore(pp_item(Showing,done)),!.


%=

% %  pfc_contains_term( ?What, ?VALUE2) is semidet.
%
% Managed Predicate Contains Term.
%
pfc_contains_term(What,_):-is_ftVar(What),!.
pfc_contains_term(What,Inside):- compound(What),!,(\+ \+ ((copy_term_nat(Inside,Inside0),snumbervars(Inside0),occurs:contains_term(What,Inside0)))),!.
pfc_contains_term(What,Inside):- (\+ \+ once((subst(Inside,What,foundZadooksy,Diff),Diff \=@= Inside ))),!.



%=

% %  hook_pfc_listing( ?What) is semidet.
%
% Hook To [baseKB:hook_pfc_listing/1] For Module Mpred_listing.
% Hook Managed Predicate Listing.
%
:- current_prolog_flag(pfc_shared_module,BaseKB),
 assert_if_new((BaseKB:hook_pfc_listing(What):- on_x_debug(pfc_list_triggers(What)))).

:- thread_local t_l:pfc_list_triggers_disabled/0.
% listing(L):-locally(t_l:pfc_list_triggers_disabled,listing(L)).


%=

% %  pfc_list_triggers( ?What) is semidet.
%
% Managed Predicate List Triggers.
%
pfc_list_triggers(_):-t_l:pfc_list_triggers_disabled,!.
pfc_list_triggers(What):-loop_check(pfc_list_triggers_nlc(What)).

:- meta_predicate(pfc_list_triggers_nlc(?)).


%=

% %  pfc_list_triggers_nlc( ?What) is semidet.
%
% Managed Predicate List Triggers Nlc.
%
pfc_list_triggers_nlc(MM:What):-atom(MM),!,MM:pfc_list_triggers(What).
pfc_list_triggers_nlc(What):-loop_check(pfc_list_triggers_0(What),true).


%=

% %  pfc_list_triggers_0( ?What) is semidet.
%
% Managed Predicate list triggers  Primary Helper.
%
pfc_list_triggers_0(What):-get_pi(What,PI),PI\=@=What,pfc_list_triggers(PI).
pfc_list_triggers_0(What):-nonvar(What),What= ~(Then),!, \+ \+ pfc_list_triggers_1(Then), \+ \+ pfc_list_triggers_1(What).
pfc_list_triggers_0(What):- \+ \+  pfc_list_triggers_1(~(What)), \+ \+ pfc_list_triggers_1(What).


%=

% %  pfc_list_triggers_types( ?VALUE1) is semidet.
%
% Managed Predicate list triggers  Types.
%
pfc_list_triggers_types('Triggers').
pfc_list_triggers_types('Instances').
pfc_list_triggers_types('Subclasses').
pfc_list_triggers_types('ArgTypes').
pfc_list_triggers_types('Arity').
pfc_list_triggers_types('Forward').
pfc_list_triggers_types('Bidirectional').
pfc_list_triggers_types('Backchaining').
pfc_list_triggers_types('Negative').
pfc_list_triggers_types('Sources').
pfc_list_triggers_types('Supports').
pfc_list_triggers_types('Edits').

% print_db_items_and_neg(Title,Fact,What):-nonvar(Fact),Fact= ~(_),!,fail.

%=

% %  print_db_items_and_neg( ?Title, ?Fact, ?What) is semidet.
%
% Print Database Items And Negated.
%
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,Fact,What).
print_db_items_and_neg(Title,Fact,What):-print_db_items(Title,~(Fact),What).


%=

% %  pfc_list_triggers_1( ?What) is semidet.
%
% Managed Predicate list triggers  Secondary Helper.
%
pfc_list_triggers_1(What):-var(What),!.
pfc_list_triggers_1(~(What)):- var(What),!.
pfc_list_triggers_1(~(_What)):-!.
pfc_list_triggers_1(What):-
   print_db_items('Supports User',spft_precanonical(P,mfl4(VarNameZ,_,_,_),ax),'$spft$'(P,mfl4(VarNameZ,_,_,_),ax),What),
   print_db_items('Forward Facts',(nesc(F)),F,What),
   print_db_items('Forward Rules',(_==>_),What),
 ignore((What\= ~(_),safe_functor(What,IWhat,_),
   print_db_items_and_neg('Instance Of',isa(IWhat,_),IWhat),
   print_db_items_and_neg('Instances: ',isa(_,IWhat),IWhat),
   print_db_items_and_neg('Subclass Of',genls(IWhat,_),IWhat),
   print_db_items_and_neg('Subclasses: ',genls(_,IWhat),IWhat))),
   forall(suggest_m(M),print_db_items('PFC Watches', pfc_prop(M,_,_,_),What)),
   print_db_items('Triggers Negative', '$nt$'(_,_,_,_),What),
   print_db_items('Triggers Goal','$bt$'(_,_,_),What),
   print_db_items('Triggers Positive','$pt$'(_,_,_),What),
   print_db_items('Bidirectional Rules',(_<==>_),What),
   dif(A,B),print_db_items('Supports Deduced',spft_precanonical(P,A,B),'$spft$'(P,A,B),What),
   dif(G,ax),print_db_items('Supports Nonuser',spft_precanonical(P,G,G),'$spft$'(P,G,G),What),
   print_db_items('Backchaining Rules',(_<-_),What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits',is_edited_clause(_,_,_),What),
   print_db_items('Instances',isa(_,_),What),
   print_db_items('Subclasses',genls(_,_),What),
   print_db_items('Negative Facts',~(_),What),

   print_db_items('ArgTypes',argGenls(_,_,_),What),
   print_db_items('ArgTypes',argIsa(_,_,_),What),
   print_db_items('ArgTypes',argQuotedIsa(_,_,_),What),
   print_db_items('ArgTypes',meta_argtypes(_),What),
   print_db_items('ArgTypes',predicate_property(G,meta_predicate(G)),What),
   print_db_items('ArgTypes',resultGenls(_,_),What),
   print_db_items('ArgTypes',resultIsa(_,_),What),
   print_db_items('Arity',arity(_,_),What),
   print_db_items('Arity',current_predicate(_),What),
   print_db_items('MetaFacts Predicate',predicate_property(_,_),What),
   print_db_items('Sources',module_property(_,_),What),
   print_db_items('Sources',predicateConventionMt(_,_),What),
   print_db_items('Sources',source_file(_,_),What),
   print_db_items('Sources',_:man_index(_,_,_,_,_),What),
   print_db_items('Sources',_:'$pldoc'(_,_,_,_),What),
   print_db_items('Sources',_:'$pred_option'(_,_,_,_),What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.


pinfo(F/A):- listing(F/A),safe_functor(P,F,A),findall(Prop,predicate_property(P,Prop),List),wdmsg_pretty(pinfo(F/A)==List),!.



% %  pp_DB is semidet.
%
% Pretty Print All.
%
%pp_DB:- defaultAssertMt(M),clause_b(mtHybrid(M)),!,pp_DB(M).
%pp_DB:- forall(clause_b(mtHybrid(M)),pp_DB(M)).

pp_DB:- prolog_load_context(module,M),pp_DB(M).

with_exact_kb(M,G):- M:call(G).

pp_DB(M):-
 with_exact_kb(M,
 M:must_det_l((
  pp_db_facts,
  pp_db_rules,
  pp_db_triggers,
  pp_db_supports))).

pp_db_facts:- context_module(M), pp_db_facts(M).
pp_db_rules:- context_module(M), pp_db_rules(M).
pp_db_triggers:- context_module(M), pp_db_triggers(M).
pp_db_supports:- context_module(M), pp_db_supports(M).


:- system:import(pp_DB/0).
:- system:export(pp_DB/0).

%  pp_db_facts ...

pp_db_facts(MM):- ignore(pp_db_facts(MM,_,true)).

pp_db_facts(MM,Pattern):- pp_db_facts(MM,Pattern,true).

pp_db_facts(MM,P,C):-
  pfc_facts_in_kb(MM,P,C,L),
  pfc_classifyFacts(L,User,Pfc,_ZRule),
  length(User,UserSize),length(Pfc,PfcSize),
  format("~N~nUser added facts in [~w]: ~w",[MM,UserSize]),
  pp_db_items(User),
  format("~N~nSystem added facts in [~w]: ~w",[MM,PfcSize]),
  pp_db_items(Pfc).

%  printitems clobbers it''s arguments - beware!


pp_db_items(Var):-var(Var),!,format("~N  ~p",[Var]).
pp_db_items([]):-!.
pp_db_items([H|T]):- !,
  % numbervars(H,0,_),
  format("~N  ~p",[H]),
  nonvar(T),pp_db_items(T).

pp_db_items((P >= FT)):- is_hidden_pft(P,FT),!.

pp_db_items(Var):-
  format("~N  ~p",[Var]).


is_hidden_pft(_,(mfl4(_VarNameZ,BaseKB,_,_),ax)):- current_prolog_flag(pfc_shared_module,BaseKB),!.
is_hidden_pft(_,(why_marked(_),ax)).


pp_mask(Type,MM,Mask):-
  bagof_or_nil(Mask,lookup_kb(MM,Mask),Nts),
  list_to_set_variant(Nts,NtsSet),!,
  pp_mask_list(Type,MM,NtsSet).

pp_mask_list(Type,MM,[]):- !,
  format("~N~nNo ~ws in [~w]...~n",[Type,MM]).
pp_mask_list(Type,MM,NtsSet):- length(NtsSet,Size), !,
  format("~N~n~ws (~w) in [~w]...~n",[Type,Size,MM]),
  pp_db_items(NtsSet).

pfc_classifyFacts([],[],[],[]).

pfc_classifyFacts([H|T],User,Pfc,[H|Rule]):-
  pfcType(H,rule(_)),
  !,
  pfc_classifyFacts(T,User,Pfc,Rule).

pfc_classifyFacts([H|T],[H|User],Pfc,Rule):-
  % get_source_uu(UU),
  get_first_user_reason(H,_UU),
  !,
  pfc_classifyFacts(T,User,Pfc,Rule).

pfc_classifyFacts([H|T],User,[H|Pfc],Rule):-
  pfc_classifyFacts(T,User,Pfc,Rule).


pp_db_rules(MM):-
   pp_mask("Forward Rule",MM,==>(_,_)),
   pp_mask("Bidirectional Rule",MM,<==>(_,_)),
   pp_mask("Backchaining Rule",MM,<-(_,_)),
   pp_mask("Implication Rule",MM,=>(_,_)),
   pp_mask("Bi-conditional Rule",MM,<=>(_,_)),
   pp_mask("Negative Fact",MM,(~(_))),
  % pp_mask("Material-impl Rule",MM,<=(_,_)),
 % pp_mask("Prolog Rule",MM,:-(_,_)),
 !.


pp_db_triggers(MM):-
 pp_mask("Positive trigger(+)",MM,'$pt$'(_,_)),
 pp_mask("Negative trigger(-)",MM,'$nt$'(_,_,_)),
 pp_mask("Goal trigger(?)",MM,'$bt$'(_,_)),!.

pp_db_supports(MM):-
  % temporary hack.
  format("~N~nSupports in [~w]...~n",[MM]),
  with_exact_kb(MM, bagof_or_nil((P >= S), pfcGetSupport(P,S),L)),
  list_to_set_variant(L,LS),
  pp_db_items(LS),!.


list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique),!.

list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far),!,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).


% %     memberchk_variant(+Val, +List)
%
%   Deterministic check of membership using =@= rather than
%   unification.

memberchk_variant(X, [Y|Ys]) :-
   (   X =@= Y
   ->  true
   ;   memberchk_variant(X, Ys)
   ).

lookup_kb(MM,MHB):- strip_module(MHB,M,HB),
     expand_to_hb(HB,H,B),
      (MM:clause(M:H,B,Ref)*->true; M:clause(MM:H,B,Ref)),
      %clause_ref_module(Ref),
      clause_property(Ref,module(MM)).


% %  has_cl( +H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).



% %  clause_or_call( +H, ?B) is semidet.
%
% Clause Or Call.
%

% PFC2.0 clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
% PFC2.0 clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
% PFC2.0 clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause(H,B)*->!;fail) ; clause(H,B)).
% PFC2.0 clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).

  /*



% as opposed to simply using clause(H,true).

% %  should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

% %  should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(pfc_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(pfc_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.

       */

% %  no_side_effects( +P) is semidet.
%
% No Side Effects.
%
%no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).

pfc_facts_in_kb(MM,P,C,L):- with_exact_kb(MM,setof_or_nil(P,pfcFact(P,C),L)).

lookup_spft(P,F,T):-pfcGetSupport(P,(F,T)).
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

u_to_uu(U,(U,ax)):- var(U),!.
u_to_uu(U,U):- nonvar(U),U=(_,_),!.
u_to_uu([U|More],UU):-list_to_conjuncts([U|More],C),!,u_to_uu(C,UU).
u_to_uu(U,(U,ax)):-!.

% %  get_source_uu( :TermU) is det.
%
% Get Source Ref (Current file or User)
%
:- module_transparent((get_source_uu)/1).
get_source_uu(UU):- must_ex(((get_source_ref1(U),u_to_uu(U,UU)))),!.

get_source_ref1(U):- quietly_ex(((current_why(U),nonvar(U)));ground(U)),!.
get_source_ref1(U):- quietly_ex(((get_source_mfl(U)))),!.


:- module_transparent((get_why_uu)/1).
get_why_uu(UU):- findall(U,current_why(U),Whys),Whys\==[],!,u_to_uu(Whys,UU).
get_why_uu(UU):- get_source_uu(UU),!.


get_startup_uu(UU):-
  prolog_load_context(module,CM),
  u_to_uu((isRuntime,mfl4(VarNameZ,CM, user_input, _)),UU),varnames_load_context(VarNameZ).

is_user_reason((_,U)):-atomic(U).
only_is_user_reason((U1,U2)):- freeze(U2,is_user_reason((U1,U2))).

is_user_fact(P):-get_first_user_reason(P,UU),is_user_reason(UU).


get_first_real_user_reason(P,UU):- nonvar(P), UU=(F,T),
  quietly_ex((  ((((lookup_spft(P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ; fail)))).

get_first_user_reason(P,(F,T)):-
  UU=(F,T),
  ((((lookup_spft(P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ;
       (clause_asserted(P),get_source_uu(UU),is_user_reason(UU)))),!.
get_first_user_reason(_,UU):- get_why_uu(UU),is_user_reason(UU),!.
get_first_user_reason(_,UU):- get_why_uu(UU),!.
get_first_user_reason(P,UU):- must_ex(ignore(((get_first_user_reason0(P,UU))))),!.
get_first_user_reason0(_,(M,ax)):-get_source_mfl(M).

%get_first_user_reason(_,UU):- get_source_uu(UU),\+is_user_reason(UU). % ignore(get_source_uu(UU)).

%:- export(pfc_at_box:defaultAssertMt/1).
%:- system:import(defaultAssertMt/1).
%:- pfc_lib:import(pfc_at_box:defaultAssertMt/1).

:- module_transparent((get_source_mfl)/1).
get_source_mfl(M):- current_why(M), nonvar(M) , M =mfl4(_VarNameZ,_,_,_).
get_source_mfl(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_location(F,L),varnames_load_context(VarNameZ).

get_source_mfl(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_file(F:L),varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ,M,F,_L)):- defaultAssertMt(M), current_source_file(F),varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ,M,_F,_L)):- defaultAssertMt(M), varnames_load_context(VarNameZ).
%get_source_mfl(M):- (defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_mfl(M):- fail,dtrace,
 ((defaultAssertMt(M) -> !;
 (atom(M)->(module_property(M,class(_)),!);
    pfcError(no_source_ref(M))))).

is_source_ref1(_).

defaultAssertMt(M):- prolog_load_context(module, M).



pfc_pp_db_justifications(P,Js):-
 show_current_source_location,
 must_ex(quietly_ex(( format("~NJustifications for ~p:",[P]),
  pfc_pp_db_justification1('',Js,1)))).

pfc_pp_db_justification1(_Prefix,[],_).

pfc_pp_db_justification1(Prefix,[J|Js],N):-
  % show one justification and recurse.
  nl,
  pfc_pp_db_justifications2(Prefix,J,N,1),
  %reset_shown_justs,
  N2 is N+1,
  pfc_pp_db_justification1(Prefix,Js,N2).

pfc_pp_db_justifications2(_Prefix,[],_,_).

pfc_pp_db_justifications2(Prefix,[C|Rest],JustNo,StepNo):-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
(
 (StepNo==1->fmt('~N~n',[]);true),
  backward_compatibility:sformat(LP,' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(pfcWhy_sub_sub(C))),
  StepNext is 1+StepNo,
  pfc_pp_db_justifications2(Prefix,Rest,JustNo,StepNext))).


pfcWhy_sub_sub(P):-
  justifications(P,Js),
  clear_proofs,
  % retractall_u(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed',P)-> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P,Js)),
   nb_getval('$last_printed',LP),
   ((pfc_pp_db_justification1(LP,Js,1),fmt('~N~n',[])))))).

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

:- dynamic(t_l:whybuffer/2).



pfcWhy :-
  t_l:whybuffer(P,_),
  pfcWhy(P).

pfcTF(P):- pfc_call(P)*->foreach(pfcTF1(P),true);pfcTF1(P).
pfcTF1(P):-
   ansi_format([underline],"~N=========================================",[]),
   (ignore(pfcWhy(P))), ignore(pfcWhy(~P)),
   printLine.


pfcWhy(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  justifications(P,Js),
  retractall(t_l:whybuffer(_,_)),
  assert(t_l:whybuffer(P,Js)),
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  justifications(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhy2(P,N) :-
  justifications(P,Js), pfcShowJustification1(Js,N).

pfcWhyBrouse(P,Js) :-
  % rtrace(pfc_pp_db_justifications(P,Js)),
  pfcShowJustifications(P,Js),
  nop((pfcAsk(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js))).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :-
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

pfcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand(u,_,_) :-
  % u=up
  !.

pfcCommand(N,_,_) :-
  integer(N),
  !,
  pfcPrintf("~p is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 pfcPrintf("~p is an unrecognized command, enter h. for help.",[X]),
 fail.

pfcShowJustifications(P,Js) :-
  show_current_source_location,
  reset_shown_justs,
  %color_line(yellow,1),
  format("~N~nJustifications for ",[]),
  ansi_format([fg(green)],'~@',[pp(P)]),
  format(" :~n",[]),
  pfcShowJustification1(Js,1),!,
  printLine.

pfcShowJustification1([J|Js],N) :- !,
  % show one justification and recurse.
  %reset_shown_justs,
  pfcShowSingleJustStep(N,J),!,
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustification1(J,N) :-
  %reset_shown_justs, % nl,
  pfcShowSingleJustStep(N,J),!.


pfcShowSingleJustStep(N,J):-
  pfcShowSingleJust(N,step(1),J),!.
pfcShowSingleJustStep(N,J):-
  pp(pfcShowSingleJustStep(N,J)),!.

incrStep(StepNo,Step):- compound(StepNo),arg(1,StepNo,Step),X is Step+1,nb_setarg(1,StepNo,X).

pfcShowSingleJust(JustNo,StepNo,C):- is_ftVar(C),!,incrStep(StepNo,Step),
  ansi_format([fg(cyan)],"~N    ~w.~w ~w ",[JustNo,Step,C]),!, maybe_more_c(C).
pfcShowSingleJust(_JustNo,_StepNo,[]):-!.
pfcShowSingleJust(JustNo,StepNo,(P,T)):-!,
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P,F,T)):-!,
  pfcShowSingleJust1(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,F),
  pfcShowSingleJust1(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P*->T)):-!,
  pfcShowSingleJust1(JustNo,StepNo,P),format('      *-> ',[]),
  pfcShowSingleJust1(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P:-T)):-!,
  pfcShowSingleJust1(JustNo,StepNo,P),format(':- ~p.',[T]).

pfcShowSingleJust(JustNo,StepNo,(P : -T)):-!,
  pfcShowSingleJust1(JustNo,StepNo,P),format('      :- ',[]),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P :- T) ):- !,
  pfcShowSingleJust1(JustNo,StepNo,call(T)),
  pfcShowSingleJust1(JustNo,StepNo,P).


pfcShowSingleJust(JustNo,StepNo,[P|T]):-!,
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,'$pt$'(P,Body)):- !,
  pfcShowSingleJust1(JustNo,StepNo,'$pt$'(P)),
  pfcShowSingleJust(JustNo,StepNo,Body).

pfcShowSingleJust(JustNo,StepNo,C):-
 pfcShowSingleJust1(JustNo,StepNo,C).

fmt_cl(P):- \+ \+ (numbervars(P,666,_,[attvars(skip),singletons(true)]),write_src(P)),!.
fmt_cl(P):- \+ \+ (pretty_numbervars(P,PP),numbervars(PP,126,_,[attvar(skip),singletons(true)]),
   write_term(PP,[portray(true),portray_goal(fmt_cl)])),write('.').
fmt_cl(S,_):- term_is_ansi(S), !, write_keeping_ansi(S).
fmt_cl(G,_):- is_grid(G),write('"'),user:print_grid(G),write('"'),!.
% fmt_cl(P,_):- catch(arc_portray(P),_,fail),!.
fmt_cl(P,_):- is_list(P),catch(p_p_t_no_nl(P),_,fail),!.
%ptg(PP,Opts):- is_list(PP),select(portray_goal(ptg),Opts,Never),write_term(PP,Never).

unwrap_litr(C,CCC+VS):- copy_term(C,CC,VS),
  numbervars(CC+VS,0,_),
  unwrap_litr0(CC,CCC),!.
unwrap_litr0(call(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0('$pt$'(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(body(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(head(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(C,C).

:- thread_local t_l:shown_why/1.

pfcShowSingleJust1(JustNo,_,MFL):- is_mfl(MFL), JustNo \== 1,!.
pfcShowSingleJust1(JustNo,StepNo,C):- unwrap_litr(C,CC),!,pfcShowSingleJust4(JustNo,StepNo,C,CC).

pfcShowSingleJust4(_,_,_,CC):- t_l:shown_why(C),C=@=CC,!.
pfcShowSingleJust4(_,_,_,MFL):- is_mfl(MFL),!.
pfcShowSingleJust4(JustNo,StepNo,C,CC):- assert(t_l:shown_why(CC)),!,
   incrStep(StepNo,Step),
   ansi_format([fg(cyan)],"~N    ~w.~w ~@ ",[JustNo,Step,user:fmt_cl(C)]),
   %write('<'),
   pfcShowSingleJust_C(C),!,%write('>'),
   format('~N'),
   ignore((maybe_more_c(C))),
   assert(t_l:shown_why(C)),
   format('~N'),!.

is_mfl(MFL):- compound(MFL), MFL = mfl4(_,_,_,_).

maybe_more_c(MFL):- is_mfl(MFL),!.
maybe_more_c(_):- t_l:shown_why(no_recurse).
maybe_more_c(C):- t_l:shown_why(more(C)),!.
maybe_more_c(C):- t_l:shown_why((C)),!.
maybe_more_c(C):- assert(t_l:shown_why(more(C))),assert(t_l:shown_why((C))),
 locally(t_l:shown_why(no_recurse),
  locally(t_l:shown_why((C)),locally(t_l:shown_why(more(C)),
   ignore(catch(pfcWhy2(C,1.1),E,fbugio(E)))))),!.

pfcShowSingleJust_C(C):-is_file_ref(C),!.
pfcShowSingleJust_C(C):-find_mfl(C,MFL),assert(t_l:shown_why(MFL)),!,pfcShowSingleJust_MFL(MFL).
pfcShowSingleJust_C(_):-ansi_format([hfg(black)]," % [no_mfl] ",[]),!.

short_filename(F,FN):- symbolic_list_concat([_,FN],'/pack/',F),!.
short_filename(F,FN):- symbolic_list_concat([_,FN],swipl,F),!.
short_filename(F,FN):- F=FN,!.

pfcShowSingleJust_MFL(MFL):- MFL=mfl4(VarNameZ,_M,F,L),atom(F),short_filename(F,FN),!,varnames_load_context(VarNameZ),
   ansi_format([hfg(black)]," % [~w:~w] ",[FN,L]).

pfcShowSingleJust_MFL(MFL):- MFL=mfl4(V,M,F,L),my_maplist(var,[V,M,F,L]),!.
pfcShowSingleJust_MFL(MFL):- ansi_format([hfg(black)]," % [~w] ",[MFL]),!.

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).



