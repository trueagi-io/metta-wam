


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




debugging_metta(G):- fake_notrace((is_debugging((eval))->ignore(G);true)).


:- nodebug(metta(eval)).

depth_to_use(InDepth, UseThis) :-
    Depth is abs(InDepth),
    % Check if the depth is over 100, if so, subtract 100
    (Depth > 100 -> 
        TempDepth is Depth - 100
    ; 
        TempDepth = Depth
    ),
    % If the result (or original depth if not over 100) is over 50, just use 50
    (TempDepth > 50 -> 
        UseThis = 50
    ; 
        UseThis = TempDepth
    ).


w_indent(Depth,Goal):- must_be(integer,Depth), 
  as_trace((
    depth_to_use(Depth, UseThis),
    format('~N;;'),
    setup_call_cleanup(forall(between(1,UseThis,_),write('  ')),Goal, format('~N')))).

indentq2(Depth,Term):- w_indent(Depth,format('~q',[Term])).

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
indentq(_DR,_EX,_AR,_Term):- flag(trace_output_len,X,X+1), XX is (X mod 1000), XX<100,!.
indentq(DR,EX,AR,Term):- 

    setup_call_cleanup(
         notrace(format('~N;')),
         as_trace((
           print_padded(EX, DR, AR),with_indents(false,write_src(Term)))),
	notrace(format('~N'))).

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

set_debug(Flag,Val):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,set_debug(Var,Val).
set_debug(Flag,true):- !, debug(metta(Flag)),flag_to_var(Flag,Var),set_option_value(Var,true).
set_debug(Flag,false):- nodebug(metta(Flag)),flag_to_var(Flag,Var),set_option_value(Var,false).

if_trace(Flag,Goal):- real_notrace((catch_err(ignore((is_debugging(Flag),Goal)),E,
												fbug(E-->if_trace(Flag,Goal))))).


is_showing(Flag):- option_value(Flag,'silent'),!,fail.
is_showing(Flag):- is_verbose(Flag),!.
is_showing(Flag):- option_value(Flag,'show'),!.

if_show(Flag,Goal):- real_notrace((catch_err(ignore((is_showing(Flag),Goal)),E,
						fbug(E-->if_show(Flag,Goal))))).



is_verbose(Flag):- option_value(Flag,'silent'),!,fail.
is_verbose(Flag):- option_value(Flag,'verbose'),!.
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
is_debugging(Flag):- debugging(metta(Flag),TF),!,TF==true.
is_debugging(Flag):- debugging(Flag,TF),!,TF==true.
is_debugging(Flag):- flag_to_var(Flag,Var),
   (option_value(Var,true)->true;(Flag\==Var -> is_debugging(Var))).

% overflow = trace
% overflow = fail
% overflow = continue
% overflow = debug

%trace_eval(P4,_TN,D1,Self,X,Y):- is_fast_mode,!, call(P4,D1,Self,X,Y).
trace_eval(P4,TN,D1,Self,X,Y):- \+ is_debugging(TN),!, call(P4,D1,Self,X,Y).
trace_eval(P4,TN,D1,Self,X,Y):-
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
   nop(notrace(no_repeats_var(YY))))),

   if_t(DR<DMax, if_trace((eval;TN), (PrintRet=1, indentq(DR,EX, '-->',[TN,X])))),

   Ret=retval(fail),!,

   (Display= (flag(eval_num,EX1,EX1+1),
			    ((Ret\=@=retval(fail),nonvar(Y)) 
			    -> indentq(DR,EX1,'<--',[TN,Y])
			     ; indentq(DR,EX1,'<--',[TN,Ret])))),

   call_cleanup((
      (call(P4,D1,Self,X,Y)*->nb_setarg(1,Ret,Y);
        (fail,trace,(call(P4,D1,Self,X,Y)))),
      ignore((fake_notrace(( \+ (Y\=YY), nb_setarg(1,Ret,Y)))))),
    % cleanup 
      ignore((PrintRet==1 -> ignore(Display) ;
       (fake_notrace(ignore((( % Y\=@=X,
         if_t(DR<DMax,if_trace((eval),ignore(Display))))))))))),
   Ret\=@=retval(fail).

%  (Ret\=@=retval(fail)->true;(fail,trace,(call(P4,D1,Self,X,Y)),fail)).

