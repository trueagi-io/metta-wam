:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).
:- nb_setval(cmt_override,lse('; ',' !(" ',' ") ')).
:- ensure_loaded(swi_support).
:- set_prolog_flag(history, 10).
:- set_prolog_flag(save_history, true).
:- set_prolog_flag(backtrace,true).
:- set_prolog_flag(backtrace_depth,100).
:- set_prolog_flag(backtrace_goal_dept,100).
:- set_prolog_flag(backtrace_show_lines,true).
:- set_prolog_flag(write_attributes,portray).
:- set_prolog_flag(debug_on_interrupt,true).
:- set_prolog_flag(debug_on_error,true).
%:- set_prolog_flag(compile_meta_arguments,control).
:- prolog_load_context(directory, Value), absolute_file_name('../packs/',Dir,[relative_to(Value)]),
    symbol_concat(Dir,'predicate_streams',PS), 
    symbol_concat(Dir,'logicmoo_utils',LU),
    pack_attach(PS,[duplicate(replace),search(first)]),
    pack_attach(LU,[duplicate(replace),search(first)]).
%	:- attach_packs.
%	:- initialization(attach_packs).
:- nodebug(metta(eval)).
:- nodebug(metta(exec)).
:- nodebug(metta(load)).
:- nodebug(metta(prolog)).


	:-multifile(user:metta_file/3).
	:-dynamic(user:metta_file/3).


:-multifile(user:asserted_metta_pred/2).
:-dynamic(user:asserted_metta_pred/2).
:-multifile(user:loaded_into_kb/2).
:-dynamic(user:loaded_into_kb/2).
:- dynamic(user:is_metta_dir/1).
:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   retractall(user:is_metta_dir(_)),asserta(user:is_metta_dir(Dir)).

user:file_search_path(mettalog,Dir):- metta_dir(Dir).


metta_dir(Dir):- user:is_metta_dir(Dir),!.
metta_dir(Dir):- getenv('METTA_DIR',Dir),!.

:- ensure_loaded(metta_debug).

is_metta_flag(What):- notrace(is_flag0(What)).

is_tRuE(TF):- TF=='True',!.
is_tRuE(TF):- TF=='true',!.
is_flag0(What):- nb_current(What,TF),TF\==[],!,is_tRuE(TF).
is_flag0(What):- current_prolog_flag(What,TF),TF\==[],!,is_tRuE(TF).
is_flag0(What):- 
 symbol_concat('--',What,FWhat),symbol_concat(FWhat,'=true',FWhatTrue),
 symbol_concat('--no-',What,NoWhat),symbol_concat(FWhat,'=false',FWhatFalse),
 is_flag0(What,[FWhat,FWhatTrue],[NoWhat,FWhatFalse]).
 
is_flag0(What,_FWhatTrue,FWhatFalse):-
   current_prolog_flag(os_argv,ArgV),
   member(FWhat,FWhatFalse),member(FWhat,ArgV),!,   
   set_option_value(What,'False'),!,fail.
is_flag0(What,FWhatTrue,_FWhatFalse):-
   current_prolog_flag(os_argv,ArgV),
   member(FWhat,FWhatTrue),member(FWhat,ArgV),!,   
	set_option_value(What,'True'),!.
is_flag0(What,_FWhatTrue,_FWhatFalse):-
  current_prolog_flag(os_argv,ArgV),
  symbolic_list_concat(['--',What,'='],Starts),
  member(FWhat,ArgV),symbol_concat(Starts,Rest,FWhat),
  set_option_value_interp(What,Rest),!.

is_compiling:- current_prolog_flag(os_argv,ArgV),member(E,ArgV),   (E==qcompile_mettalog;E==qsave_program),!.
is_compiled:- current_prolog_flag(os_argv,ArgV), member('-x',ArgV),!.
is_compiled:- current_prolog_flag(os_argv,ArgV),\+ member('swipl',ArgV),!.

is_converting:- is_metta_flag('convert').

is_compat:- is_metta_flag('compat').

is_mettalog:- is_metta_flag('log').

is_synthing_unit_tests:- notrace(is_synthing_unit_tests0).
is_synthing_unit_tests0:- is_testing.
%is_synthing_unit_tests0:- is_html.
% is_synthing_unit_tests0:- is_compatio,!,fail.

is_testing:- is_metta_flag('test'),!.
%is_testing.
is_html:- is_metta_flag('html').

:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_loader).


:- nodebug(metta('trace-on-eval')).

% is_compatio:- !,fail.
is_compatio:- notrace(is_compatio0).
is_compatio0:- is_testing,!,fail.
is_compatio0:- is_flag0('compatio').
is_compatio0:- is_mettalog,!,fail.
%is_compatio0:- is_html,!,fail.
is_compatio0:- !.

keep_output:- !.
keep_output:- is_mettalog,!.
keep_output:- is_testing,!.
keep_output:- is_compatio,!,fail.


:- volatile(original_user_output/1).
:- dynamic(original_user_output/1).
original_user_output(X):- stream_property(X,file_no(1)).
:- original_user_output(_)->true;current_output(Out),asserta(original_user_output(Out)).
unnullify_output:- current_output(MFS),  original_user_output(OUT), MFS==OUT, !.
unnullify_output:- original_user_output(MFS), set_prolog_IO(user_input,MFS,user_error).

null_output(MFS):- use_module(library(memfile)),
  new_memory_file(MF),open_memory_file(MF,append,MFS).
:- dynamic(null_user_output/1).
:- null_user_output(_)->true;(null_output(MFS),
   asserta(null_user_output(MFS))).


nullify_output:- keep_output,!.
nullify_output:- nullify_output_really.
nullify_output_really:- current_output(MFS), null_user_output(OUT),  MFS==OUT, !.
nullify_output_really:- null_user_output(MFS), set_prolog_IO(user_input,MFS,MFS).

set_output_stream :- !.
set_output_stream :- keep_output -> nullify_output;  unnullify_output.
:- set_output_stream.
% :- nullify_output.

switch_to_mettalog:- 
  unnullify_output,  
  set_option_value('compatio',false),
  set_option_value('compat',false),
  set_option_value('load',show),
  set_option_value('load',verbose),
  set_option_value('log',true),
  %set_option_value('test',true),
  set_output_stream.
  
switch_to_mettarust:- 
  nullify_output,  
  set_option_value('compatio',true),
  set_option_value('compat',true),
  set_option_value('log',false),
  set_option_value('test',false),
  set_output_stream.
  


show_os_argv:- is_compatio,!.
show_os_argv:- current_prolog_flag(os_argv,ArgV),write('; libswipl: '),writeln(ArgV).
is_pyswip:- current_prolog_flag(os_argv,ArgV),member( './',ArgV).
:- multifile(is_metta_data_functor/1).
:- dynamic(is_metta_data_functor/1).
:- multifile(is_nb_space/1).
:- dynamic(is_nb_space/1).
%:- '$set_source_module'('user').
:- use_module(library(filesex)).
:- use_module(library(system)).
:- use_module(library(shell)).
%:- use_module(library(tabling)).

:- nb_setval(self_space, '&self').
current_self(Self):- ((nb_current(self_space,Self),Self\==[])->true;Self='&self').
:- nb_setval(repl_mode, '+').

%:- set_stream(user_input,tty(true)).
:- if(exists_source(library(readline))).
:- use_module(library(readline)).
:- endif.
%:- use_module(library(editline)).
:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(encoding,utf8).
%:- set_output(user_error).
%:- set_prolog_flag(encoding,octet).


:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- include(metta_data).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_eval).
/*
Now PASSING NARS.TEC:\opt\logicmoo_workspace\packs_sys\logicmoo_opencog\MeTTa\hyperon-wam\src\pyswip\metta_interp.pl
C:\opt\logicmoo_workspace\packs_sys\logicmoo_opencog\MeTTa\hyperon-wam\src\pyswip1\metta_interp.pl
STS1.01)
Now PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.08)
Now PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.14)
Now PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.15)
Now PASSING TEST-SCRIPTS.C1-GROUNDED-BASIC.15)
Now PASSING TEST-SCRIPTS.E2-STATES.08)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.02)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.07)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.09)
PASSING TEST-SCRIPTS.B5-TYPES-PRELIM.11)
PASSING TEST-SCRIPTS.C1-GROUNDED-BASIC.14)
PASSING TEST-SCRIPTS.E2-STATES.07)
-----------------------------------------
FAILING TEST-SCRIPTS.D5-AUTO-TYPES.01)
Now FAILING TEST-SCRIPTS.00-LANG-CASE.03)
Now FAILING TEST-SCRIPTS.B5-TYPES-PRELIM.19)
Now FAILING TEST-SCRIPTS.C1-GROUNDED-BASIC.20)

*/


%option_value_def('repl',auto).
option_value_def('prolog',false).
option_value_def('compat',auto).
option_value_def('compatio',true).
%option_value_def('compatio',false).
option_value_def('compile',false).
%option_value_def('compile',true).
%option_value_def('compile',full).
option_value_def('tabling',true).
option_value_def('optimize',true).
option_value_def(no_repeats,false).
%option_value_def('time',false).
option_value_def('test',false).
option_value_def('html',false).
option_value_def('python',true).
%option_value_def('halt',false).
option_value_def('doing_repl',false).
option_value_def('test-retval',false).
option_value_def('exeout','./Sav.gitlab.MeTTaLog').

option_value_def('synth_unit_tests',false).

option_value_def('trace-length',500).
option_value_def('stack-max',500).
option_value_def('trace-on-overtime',4.0).
option_value_def('trace-on-overflow',false).
option_value_def('trace-on-error',true).
option_value_def('trace-on-exec',false).
option_value_def('trace-on-fail',false).
option_value_def('trace-on-pass',false).


option_value_def('exec',true). % vs skip

option_value_def('trace-on-load',false).
option_value_def('load','silent').

option_value_def('trace-on-eval',false).
option_value_def('eval',silent).

option_value_def('transpiler',silent).
option_value_def('result',show).

option_value_def('maximum-result-count',inf). % infinate answers

% MeTTaLog --log mode only
% if print the first 10 answers without stopping
option_value_def('initial-result-count',10). 
  




fbugio(_,_):- is_compatio,!.
fbugio(TF,P):-!, ignore(( TF,!,fbug(P))).
fbugio(IO):-fbugio(true,IO).

different_from(N,V):- \+ \+ option_value_def(N,V),!,fail.
different_from(N,V):- \+ \+ nb_current(N,V),!,fail.
different_from(_,_).

set_option_value_interp(N,V):- symbol(N), symbolic_list_concat(List,',',N),List\=[_],!,
  forall(member(E,List),set_option_value_interp(E,V)).
set_option_value_interp(N,V):-
  (different_from(N,V)->Note=true;Note=false),
  fbugio(Note,set_option_value(N,V)),set_option_value(N,V),
  ignore(forall(on_set_value(Note,N,V),true)).

on_set_value(Note,N,'True'):- on_set_value(Note,N,true).   
on_set_value(Note,N,'False'):- on_set_value(Note,N,false).
on_set_value(_Note,log,true):- switch_to_mettalog.
on_set_value(_Note,compatio,true):- switch_to_mettarust.
on_set_value(Note,N,V):- symbol(N), symbol_concat('trace-on-',F,N),fbugio(Note,set_debug(F,V)),set_debug(F,V).
on_set_value(Note,N,V):- symbol(N), is_debug_like(V,TF),fbugio(Note,set_debug(N,TF)),set_debug(N,TF).

is_debug_like(trace, true).
is_debug_like(notrace, false).
is_debug_like(debug, true).
is_debug_like(nodebug, false).
is_debug_like(silent, false).
%is_debug_like(false, false).

%:- (is_mettalog->switch_to_mettalog;switch_to_mettarust).

set_is_unit_test(TF):-
  forall(option_value_def(A,B),set_option_value_interp(A,B)),
  set_option_value_interp('trace-on-pass',false),
  set_option_value_interp('trace-on-fail',false),
  set_option_value_interp('load',show),
  set_option_value_interp('test',TF),
	%set_option_value_interp('trace-on-load',TF),
/*  if_t(TF,set_option_value_interp('exec',debug)),
  if_t(TF,set_option_value_interp('eval',debug)),
  set_option_value_interp('trace-on-exec',TF),
  set_option_value_interp('trace-on-eval',TF),*/
 % if_t( \+ TF , set_prolog_flag(debug_on_interrupt,true)),
  !.

fake_notrace(G):- tracing,!,notrace(G).
fake_notrace(G):- !,once(G).
fake_notrace(G):- quietly(G),!.
real_notrace(G):- notrace(G).


null_io(G):- null_user_output(Out), !, with_output_to(Out,G).
user_io(G):- original_user_output(Out), !, with_output_to(Out,G).

with_output_to_s(Out,G):- current_output(COut), 
  redo_call_cleanup(set_prolog_IO(user_input, Out,user_error), G, 
					 set_prolog_IO(user_input,COut,user_error)).


    only_compatio(G):- if_t((is_compatio, \+ is_mettalog),user_io(G)).
  if_compatio(G):- if_t(is_compatio,user_io(G)).
 not_compatio(G):- if_t(once(is_mettalog;is_testing),user_io(G)).

 if_compat_io(G):- if_compatio(G).
not_compat_io(G):- not_compatio(G).
non_compat_io(G):- not_compatio(G).

:- set_is_unit_test(false).

trace_on_fail:-     option_value('trace-on-fail',true).
trace_on_overflow:- option_value('trace-on-overflow',true).
trace_on_pass:-     option_value('trace-on-pass',true).
doing_repl:-     option_value('doing_repl',true).
if_repl(Goal):- doing_repl->call(Goal);true.

any_floats(S):- member(E,S),float(E),!.

show_options_values:-
   forall((nb_current(N,V), \+((symbol(N),symbol_concat('$',_,N)))),write_src_nl(['pragma!',N,V])).

:- prolog_load_context(source,File), assert(interpreter_source_file(File)).


% ============================
% %%%% Arithmetic Operations
% ============================

% Addition
%'+'(A, B, Sum):- \+ any_floats([A, B, Sum]),!,Sum #= A+B .
%'+'(A, B, Sum):- notrace(catch_err(plus(A, B, Sum),_,fail)),!.
'+'(A, B, Sum):- eval_H([+,A,B],Sum).
% Subtraction
'-'( A, B, Sum):- eval_H([-,A,B],Sum).
% Multiplication
'*'(A, B, Product):- eval_H([*,A,B],Product).
% Division
'/'(Dividend, Divisor, Quotient):- eval_H(['/',Dividend, Divisor], Quotient).   %{Dividend = Quotient * Divisor}.
% Modulus
'mod'(Dividend, Divisor, Remainder):- eval_H(['mod',Dividend, Divisor], Remainder).
'%'(Dividend, Divisor, Remainder):- eval_H(['mod',Dividend, Divisor], Remainder).
% Exponentiation
'exp'(Base, Exponent, Result):- eval_H(['exp', Base, Exponent], Result).
% Square Root
'sqrt'(Number, Root):- eval_H(['sqrt', Number], Root).

% ============================
% %%%% List Operations
% ============================
% Retrieve Head of the List
'car-atom'(List, Head):- eval_H(['car-atom', List], Head).
% Retrieve Tail of the List
'cdr-atom'(List, Tail):- eval_H(['cdr-atom', List], Tail).
% Construct a List
'Cons'(Element, List, 'Cons'(Element, List)):- !.
% Collapse List
'collapse'(List, CollapsedList):- eval_H(['collapse', List], CollapsedList).
% Count Elements in List
'CountElement'(List, Count):- eval_H(['CountElement', List], Count).
% Find Length of List
%'length'(List, Length):- eval_H(['length', List], Length).

% ============================
% %%%% Nondet Opteration
% ============================
% Superpose a List
'superpose'(List, SuperposedList):- eval_H(['superpose', List], SuperposedList).

% ============================
% %%%% Testing
% ============================

% `assertEqual` Predicate
% This predicate is used for asserting that the Expected value is equal to the Actual value.
% Expected: The value that is expected.
% Actual: The value that is being checked against the Expected value.
% Result: The result of the evaluation of the equality.
% Example: `assertEqual(5, 5, Result).` would succeed, setting Result to true (or some success indicator).
%'assertEqual'(Expected, Actual, Result):- use_metta_compiler,!,as_tf((Expected=Actual),Result).
'assertEqual'(Expected, Actual, Result):- ignore(Expected=Actual), eval_H(['assertEqual', Expected, Actual], Result).

% `assertEqualToResult` Predicate
% This predicate asserts that the Expected value is equal to the Result of evaluating Actual.
% Expected: The value that is expected.
% Actual: The expression whose evaluation is being checked against the Expected value.
% Result: The result of the evaluation of the equality.
% Example: If Actual evaluates to the Expected value, this would succeed, setting Result to true (or some success indicator).
'assertEqualToResult'(Expected, Actual, Result):- eval_H(['assertEqualToResult', Expected, Actual], Result).

% `assertFalse` Predicate
% This predicate is used to assert that the evaluation of EvalThis is false.
% EvalThis: The expression that is being evaluated and checked for falsehood.
% Result: The result of the evaluation.
% Example: `assertFalse((1 > 2), Result).` would succeed, setting Result to true (or some success indicator), as 1 > 2 is false.
'assertFalse'(EvalThis, Result):- eval_H(['assertFalse', EvalThis], Result).

% `assertNotEqual` Predicate
% This predicate asserts that the Expected value is not equal to the Actual value.
% Expected: The value that is expected not to match the Actual value.
% Actual: The value that is being checked against the Expected value.
% Result: The result of the evaluation of the inequality.
% Example: `assertNotEqual(5, 6, Result).` would succeed, setting Result to true (or some success indicator).
'assertNotEqual'(Expected, Actual, Result):- eval_H(['assertNotEqual', Expected, Actual], Result).

% `assertTrue` Predicate
% This predicate is used to assert that the evaluation of EvalThis is true.
% EvalThis: The expression that is being evaluated and checked for truth.
% Result: The result of the evaluation.
% Example: `assertTrue((2 > 1), Result).` would succeed, setting Result to true (or some success indicator), as 2 > 1 is true.
'assertTrue'(EvalThis, Result):- eval_H(['assertTrue', EvalThis], Result).

% `rtrace` Predicate
% This predicate is likely used for debugging; possibly for tracing the evaluation of Condition.
% Condition: The condition/expression being traced.
% EvalResult: The result of the evaluation of Condition.
% Example: `rtrace((2 + 2), EvalResult).` would trace the evaluation of 2 + 2 and store its result in EvalResult.
'rtrace!'(Condition, EvalResult):- eval_H(['rtrace', Condition], EvalResult).

% `time` Predicate
% This predicate is used to measure the time taken to evaluate EvalThis.
% EvalThis: The expression whose evaluation time is being measured.
% EvalResult: The result of the evaluation of EvalThis.
% Example: `time((factorial(5)), EvalResult).` would measure the time taken to evaluate factorial(5) and store its result in EvalResult.
'time!'(EvalThis, EvalResult):- eval_H(['time', EvalThis], EvalResult).

% ============================
% %%%% Debugging, Printing and Utility Operations
% ============================
% REPL Evaluation
'repl!'(EvalResult):- eval_H(['repl!'], EvalResult).
% Condition Evaluation
'!'(Condition, EvalResult):- eval_H(['!', Condition], EvalResult).
% Import File into Environment
'import!'(Environment, Filename, Namespace):- eval_H(['import!', Environment, Filename], Namespace).
% Evaluate Expression with Pragma
'pragma!'(Environment, Expression, EvalValue):- eval_H(['pragma!', Environment, Expression], EvalValue).
% Print Message to Console
'print'(Message, EvalResult):- eval_H(['print', Message], EvalResult).
% No Operation, Returns EvalResult unchanged
'nop'(Expression, EvalResult):- eval_H(['nop', Expression], EvalResult).

% ============================
% %%%% Variable Bindings
% ============================
% Bind Variables
'bind!'(Environment, Variable, Value):- eval_H(['bind!', Environment, Variable], Value).
% Let binding for single variable
'let'(Variable, Expression, Body, Result):- eval_H(['let', Variable, Expression, Body], Result).
% Sequential let binding
'let*'(Bindings, Body, Result):- eval_H(['let*', Bindings, Body], Result).

% ============================
% %%%% Reflection
% ============================
% Get Type of Value
'get-type'(Value, Type):- eval_H(['get-type', Value], Type).


metta_argv(Args):- current_prolog_flag(metta_argv, Args),!.
metta_argv(Before):- current_prolog_flag(os_argv,OSArgv), append(_,['--args'|AArgs],OSArgv),
    before_arfer_dash_dash(AArgs,Before,_),!,set_metta_argv(Before).
argv_metta(Nth,Value):- metta_argv(Args),nth1(Nth,Args,Value).

set_metta_argv(Before):-  maplist(read_argv,Before,Args),set_prolog_flag(metta_argv, Args),!.
read_argv(AArg,Arg):- \+ symbol(AArg),!,AArg=Arg.
read_argv(AArg,Arg):- atom_string(AArg,S),read_metta(S,Arg),!.

metta_cmd_args(Rest):- current_prolog_flag(late_metta_opts,Rest),!.
metta_cmd_args(Rest):- current_prolog_flag(os_argv,P),append(_,['--'|Rest],P),!.
metta_cmd_args(Rest):- current_prolog_flag(argv,P),append(_,['--'|Rest],P),!.
metta_cmd_args(Rest):- current_prolog_flag(argv,Rest).

:- dynamic(has_run_cmd_args/0).
:- volatile(has_run_cmd_args/0).
run_cmd_args_prescan:- has_run_cmd_args, !.
run_cmd_args_prescan:- assert(has_run_cmd_args), do_cmdline_load_metta(prescan).

run_cmd_args:-  
  run_cmd_args_prescan,
  set_prolog_flag(debug_on_interrupt,true),
  do_cmdline_load_metta(execute).


metta_make_hook:-  loonit_reset, option_value(not_a_reload,true),!.
metta_make_hook:-
  metta_cmd_args(Rest), into_reload_options(Rest,Reload), do_cmdline_load_metta(reload,'&self',Reload).

:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).
prolog:make_hook(after, _Some):- nop( metta_make_hook).

into_reload_options(Reload,Reload).

is_cmd_option(Opt,M, TF):- symbol(M),
   symbol_concat('-',Opt,Flag),
   atom_contains(M,Flag),!,
   get_flag_value(M,FV),
   TF=FV.

get_flag_value(M,V):- symbolic_list_concat([_,V],'=',M),!.
get_flag_value(M,false):- atom_contains(M,'-no'),!.
get_flag_value(_,true).


:- ignore(((
   \+ prolog_load_context(reloading,true),
   nop((forall(option_value_def(Opt,Default),set_option_value_interp(Opt,Default))))))).

%process_option_value_def:- \+ option_value('python',false), skip(ensure_loaded(metta_python)).
process_option_value_def:- \+ option_value('python',false), ensure_loaded(mettalog(metta_python)), 
  ensure_mettalog_py.
process_option_value_def.


process_late_opts:- forall(process_option_value_def,true).
process_late_opts:- once(option_value('html',true)), set_is_unit_test(true).
%process_late_opts:- current_prolog_flag(os_argv,[_]),!,ignore(repl).
%process_late_opts:- halt(7).
process_late_opts.


do_cmdline_load_metta(Phase):- metta_cmd_args(Rest), !,  do_cmdline_load_metta(Phase,'&self',Rest).

%do_cmdline_load_metta(Phase,_Slf,Rest):- select('--prolog',Rest,RRest),!,
%  set_option_value_interp('prolog',true),
%  set_prolog_flag(late_metta_opts,RRest).
do_cmdline_load_metta(Phase,Self,Rest):-
  set_prolog_flag(late_metta_opts,Rest),
  forall(process_option_value_def,true),
  cmdline_load_metta(Phase,Self,Rest),!,
  forall(process_late_opts,true).

load_metta_file(Self,Filemask):- symbol_concat(_,'.metta',Filemask),!, load_metta(Self,Filemask).
load_metta_file(_Slf,Filemask):- load_flybase(Filemask).

catch_abort(From,Goal):-
   catch_abort(From,Goal,Goal).
catch_abort(From,TermV,Goal):-
   catch(Goal,'$aborted',fbug(aborted(From,TermV))).
% done

before_arfer_dash_dash(Rest,Args,NewRest):- 
  append(Args,['--'|NewRest],Rest)->true;([]=NewRest,Args=Rest).

cmdline_load_metta(_,_,Nil):- Nil==[],!.

cmdline_load_metta(Phase,Self,['--'|Rest]):- !,
  cmdline_load_metta(Phase,Self,Rest).

cmdline_load_metta(Phase,Self,['--args'|Rest]):- !,
  before_arfer_dash_dash(Rest,Before,NewRest),!,
  set_metta_argv(Before),
  cmdline_load_metta(Phase,Self,NewRest).
    
  cmdline_load_metta(Phase,Self,['--repl'|Rest]):- !, 
  if_phase(Phase,execute,repl),
  cmdline_load_metta(Phase,Self,Rest).
cmdline_load_metta(Phase,Self,['--log'|Rest]):- !, 
  if_phase(Phase,execute,switch_to_mettalog),
  cmdline_load_metta(Phase,Self,Rest).
cmdline_load_metta(Phase,Self,[Filemask|Rest]):- symbol(Filemask), \+ symbol_concat('-',_,Filemask),
  if_phase(Phase,execute,cmdline_load_file(Self,Filemask)),
  cmdline_load_metta(Phase,Self,Rest).        

cmdline_load_metta(Phase,Self,['-g',M|Rest]):- !,
  if_phase(Phase,execute,catch_abort(['-g',M],((read_term_from_atom(M, Term, []),ignore(call(Term)))))),
  cmdline_load_metta(Phase,Self,Rest).

cmdline_load_metta(Phase,Self,['-G',Str|Rest]):- !,
  current_self(Self),
  if_phase(Phase,execute,catch_abort(['-G',Str],ignore(call_sexpr('!',Self,Str,_S,_Out)))),
  cmdline_load_metta(Phase,Self,Rest).

cmdline_load_metta(Phase,Self,[M|Rest]):-
  m_opt(M,Opt),
  is_cmd_option(Opt,M,TF),
  fbug(is_cmd_option(Phase,Opt,M,TF)), 
  set_option_value_interp(Opt,TF), !,
  %set_tty_color_term(true),
  cmdline_load_metta(Phase,Self,Rest).

cmdline_load_metta(Phase,Self,[M|Rest]):-
  format('~N'), fbug(unused_cmdline_option(Phase,M)), !,
  cmdline_load_metta(Phase,Self,Rest).



%cmdline_load_file(Self,Filemask):- is_converting,!,

cmdline_load_file(Self,Filemask):-
    Src=(user:load_metta_file(Self,Filemask)),
    catch_abort(Src,
    (must_det_ll((
          not_compatio((nl,write('; '),write_src(Src),nl)),
          catch_red(Src),!,flush_output)))),!.

if_phase(Current,Phase,Goal):- ignore((sub_var(Current,Phase),!, Goal)).

set_tty_color_term(TF):-
  current_output(X),set_stream(X,tty(TF)),
  set_stream(current_output,tty(TF)),
  set_prolog_flag(color_term ,TF).

m_opt(M,Opt):-
  m_opt0(M,Opt1),
  m_opt1(Opt1,Opt).

m_opt1(Opt1,Opt):- symbolic_list_concat([Opt|_],'=',Opt1).

m_opt0(M,Opt):- symbol_concat('--no-',Opt,M),!.
m_opt0(M,Opt):- symbol_concat('--',Opt,M),!.
m_opt0(M,Opt):- symbol_concat('-',Opt,M),!.

:- set_prolog_flag(occurs_check,true).

start_html_of(_Filename):- \+ tee_file(_TEE_FILE),!.
start_html_of(_Filename):-!.
start_html_of(_Filename):-
 must_det_ll((
  S = _,
  %retractall(metta_defn(Eq,S,_,_)),
  nop(retractall(metta_type(S,_,_))),
  %retractall(get_metta_atom(Eq,S,_,_,_)),
  loonit_reset,
  tee_file(TEE_FILE),
  sformat(S,'cat /dev/null > "~w"',[TEE_FILE]),

  writeln(doing(S)),
  ignore(shell(S)))).

save_html_of(_Filename):- \+ tee_file(_TEE_FILE),!.
save_html_of(_):- \+ has_loonit_results, \+ option_value('html',true).
save_html_of(_):- !, writeln('<br/><a href="https://github.com/logicmoo/hyperon-wam/blob/main/MeTTaLog.md">Return to Summaries</a><br/>').
save_html_of(_Filename):-!.
save_html_of(Filename):-
 must_det_ll((
  file_name_extension(Base,_,Filename),
  file_name_extension(Base,'metta.html',HtmlFilename),
  loonit_reset,
  tee_file(TEE_FILE),
  writeln('<br/><a href="https://github.com/logicmoo/hyperon-wam/blob/main/MeTTaLog.md">Return to Summaries</a><br/>'),
  sformat(S,'ansi2html -u < "~w" > "~w" ',[TEE_FILE,HtmlFilename]),
  writeln(doing(S)),
  ignore(shell(S)))).

tee_file(TEE_FILE):- getenv('TEE_FILE',TEE_FILE),!.
tee_file(TEE_FILE):- metta_dir(Dir),directory_file_path(Dir,'TEE.ansi',TEE_FILE),!.


clear_spaces:- clear_space(_).
clear_space(S):-
   retractall(user:loaded_into_kb(S,_)),
   %retractall(metta_defn(_,S,_,_)),
   nop(retractall(metta_type(S,_,_))),
   retractall(metta_atom_asserted(S,_)).

dcall(G):- call(G).

lsm:- lsm(_).
lsm(S):-
   listing(metta_file(S,_,_)),
   %listing(mdyn_type(S,_,_,_)),
   forall(mdyn_type(S,_,_,Src),color_g_mesg('#22a5ff',write_f_src(Src))),
   nl,nl,nl,
   forall(mdyn_defn(S,_,_,Src),color_g_mesg('#00ffa5',write_f_src(Src))),
   %listing(mdyn_defn(S,_,_,_)),
   !.

write_f_src(H,B):- H=@=B,!,write_f_src(H).
write_f_src(H,B):- write_f_src(['=',H,B]).

hb_f(HB,ST):- sub_term(ST,HB),(symbol(ST),ST\==(=),ST\==(:)),!.
write_f_src(HB):-
  hb_f(HB,ST),
  option_else(current_def,CST,[]),!,
  (CST == ST -> true ; (nl,nl,nl,set_option_value_interp(current_def,ST))),
  write_src(HB).



debug_only(G):- notrace(ignore(catch_warn(G))).
debug_only(_What,G):- ignore((fail,notrace(catch_warn(G)))).


'True':- true.
'False':- fail.


'mettalog::vspace-main':- repl.

into_underscores(D,U):- symbol(D),!,symbolic_list_concat(L,'-',D),symbolic_list_concat(L,'_',U).
into_underscores(D,U):- descend_and_transform(into_underscores,D,U),!.


descend_and_transform(P2, Input, Transformed) :-
    (   var(Input)
    ->  Transformed = Input  % Keep variables as they are
    ;   compound(Input)
    -> (compound_name_arguments(Input, Functor, Args),
        maplist(descend_and_transform(P2), Args, TransformedArgs),
        compound_name_arguments(Transformed, Functor, TransformedArgs))
    ;   (symbol(Input),call(P2,Input,Transformed))
    ->  true % Transform atoms using xform_atom/2
    ;   Transformed = Input  % Keep other non-compound terms as they are
    ).

/*
is_syspred(H,Len,Pred):- notrace(is_syspred0(H,Len,Pred)).
is_syspred0(H,_Ln,_Prd):- \+ symbol(H),!,fail.
is_syspred0(H,_Ln,_Prd):- upcase_atom(H,U),downcase_atom(H,U),!,fail.
is_syspred0(H,Len,Pred):- current_predicate(H/Len),!,Pred=H.
is_syspred0(H,Len,Pred):- symbol_concat(Mid,'!',H), H\==Mid, is_syspred0(Mid,Len,Pred),!.
is_syspred0(H,Len,Pred):- into_underscores(H,Mid), H\==Mid, is_syspred0(Mid,Len,Pred),!.

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).





is_metta_data_functor(Eq,F):-
  current_self(Self),is_metta_data_functor(Eq,Self,F).

is_metta_data_functor(Eq,Other,H):-
  metta_type(Other,H,_),
  \+ get_metta_atom(Eq,Other,[H|_]),
  \+ metta_defn(Eq,Other,[H|_],_).
*/
is_function(F):- symbol(F).

is_False(X):- X\=='True', (is_False1(X)-> true ; (eval_H(X,Y),is_False1(Y))).
is_False1(Y):- (Y==0;Y==[];Y=='False').

is_conz(Self):- compound(Self), Self=[_|_].

%dont_x(eval_H(Depth,Self,metta_if(A<B,L1,L2),R)).
dont_x(eval_H(_<_,_)).

into_fp(D,D):- \+ \+ dont_x(D),!.
into_fp(ListX,CallAB):-
  sub_term(STerm,ListX),needs_expanded(STerm,Term),
  %copy_term(Term,CTerm),
  =(Term,CTerm),
  substM(ListX,CTerm,Var,CallB), fn_append1(Term,Var,CallA),
  into_fp((CallA,CallB),CallAB).
into_fp(A,A).

needs_expand(Expand):- compound(Expand),functor(Expand,F,N),N>=1,symbol_concat(metta_,_,F).
needs_expanded(eval_H(Term,_),Expand):- !,sub_term(Expand,Term),compound(Expand),Expand\=@=Term,
   compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).
needs_expanded([A|B],Expand):- sub_term(Expand,[A|B]), compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).

fn_append1(eval_H(Term,X),X,eval_H(Term,X)):-!.
fn_append1(Term,X,eval_H(Term,X)).


% Check if parentheses are balanced in a list of characters
balanced_parentheses(Str):- string(Str), string_chars(Str,Chars),!,balanced_parentheses(Chars, 0).
balanced_parentheses(Chars) :- balanced_parentheses(Chars, 0).
balanced_parentheses([], 0).
balanced_parentheses(['('|T], N) :- N1 is N + 1, !, balanced_parentheses(T, N1).
balanced_parentheses([')'|T], N) :- N > 0, N1 is N - 1, !, balanced_parentheses(T, N1).
balanced_parentheses([H|T], N) :- H \= '(', H \= ')', !, balanced_parentheses(T, N).
% Recursive function to read lines until parentheses are balanced.

repl_read(NewAccumulated, Expr):-
    symbol_concat(Atom, '.', NewAccumulated),
    catch_err((read_term_from_atom(Atom, Term, []), Expr=call(Term)), E,
       (write('Syntax error: '), writeq(E), nl, repl_read(Expr))),!.


%repl_read(Str, Expr):- ((clause(t_l:s_reader_info(Expr),_,Ref),erase(Ref))).
repl_read("!", '!'):-!.
repl_read("+", '+'):-!.
repl_read(Str,Atom):- atom_string(Atom,Str),metta_interp_mode(Atom,_),!.

repl_read(Str, Expr):- symbol_concat('@',_,Str),!,atom_string(Expr,Str).
repl_read(Str, _Expr):- symbol_concat(')',_,Str),!,fbug(repl_read_syntax(Str)),throw(restart_reading).
repl_read(NewAccumulated, Expr):-
    normalize_space(string(Renew),NewAccumulated),
    Renew \== NewAccumulated, !,
    repl_read(Renew, Expr).
%repl_read(Str, 'add-atom'('&self',Expr)):- symbol_concat('+',W,Str),!,repl_read(W,Expr).
%repl_read(NewAccumulated,exec(Expr)):- string_concat("!",Renew,NewAccumulated), !, repl_read(Renew, Expr).
repl_read(NewAccumulated, Expr):- string_chars(NewAccumulated, Chars),
    balanced_parentheses(Chars), length(Chars, Len), Len > 0,
	read_metta(NewAccumulated,Expr),
	normalize_space(string(Renew),NewAccumulated), 
    add_history_string(Renew).
repl_read(Accumulated, Expr) :- read_line_to_string(current_input, Line), repl_read(Accumulated, Line, Expr).

repl_read(_, end_of_file, end_of_file):- throw(end_of_input).

repl_read(Accumulated, "", Expr):- !, repl_read(Accumulated, Expr).
repl_read(_Accumulated, Line, Expr):- Line == end_of_file, !, Expr = Line.
repl_read(Accumulated, Line, Expr) :- symbolics_to_string([Accumulated," ",Line], NewAccumulated), !,
    repl_read(NewAccumulated, Expr).

repl_read(O2):- clause(t_l:s_reader_info(O2),_,Ref),erase(Ref).
repl_read(Expr) :- repeat,
  remove_pending_buffer_codes(_,Was),text_to_string(Was,Str),
      repl_read(Str, Expr),
        % once(((symbol(Expr1),symbol_concat('@',_,Expr1), \+ atom_contains(Expr1,"="), repl_read(Expr2)) -> Expr=[Expr1,Expr2] ; Expr1 = Expr)),
        % this cutrs the repeat/0
        ((peek_pending_codes(_,Peek),Peek==[])->!;true).

add_history_string(Str):- notrace(ignore(add_history01(Str))),!.

add_history_src(Exec):- notrace(ignore((Exec\=[],with_output_to(string(H),with_indents(false,write_src(Exec))),add_history_string(H)))).

add_history_pl(Exec):- var(Exec), !.
add_history_pl(eval(_,catch_red(PL),_)):- !,add_history_pl(PL).
add_history_pl(show_failure(PL)):-!,add_history_pl(PL).
add_history_pl(as_tf(PL,_OUT)):-!,add_history_pl(PL).
add_history_pl(Exec):- notrace(ignore((Exec\=[],with_output_to(string(H),with_indents(false,(writeq(Exec),writeln('.')))),add_history_string(H)))).


:- nb_setval(variable_names,[]).

assert_preds(Self,Load,List):- is_list(List),!,maplist(assert_preds(Self,Load),List).
%assert_preds(_Self,_Load,_Preds):- \+ show_transpiler,!.
assert_preds(Self,Load,Preds):-
  expand_to_hb(Preds,H,_B),functor(H,F,A),
  if_t((show_transpiler),
    color_g_mesg_ok('#005288',(    
   ignore((
      % \+ predicate_property(H,defined),
      %if_t(is_transpiling,catch_i(dynamic(F,A))),
      if_t( \+ predicate_property(H,defined),
           not_compatio(format('  :- ~q.~n',[dynamic(F/A)]))),
      if_t(option_value('tabling','True'), 
           not_compatio(format('  :- ~q.~n',[table(F/A)]))))),
      not_compatio(format('~N~n  ~@',[portray_clause(Preds)]))))),
   
   
  if_t(is_transpiling,
   if_t( \+ predicate_property(H,static),
   add_assertion(Self,Preds))),
   nop(metta_anew1(Load,Preds)).


%load_hook(_Load,_Hooked):- !.
load_hook(Load,Hooked):-
   ignore(( \+ ((forall(load_hook0(Load,Hooked),true))))),!.


rtrace_on_error(G):- catch(G,_,fail),!.
rtrace_on_error(G):-rtrace(G),!.
assertion_hb(metta_defn(=,Self,H,B),Self,H,B).
assertion_hb(metta_atom_asserted(Self,[=,H,B]),Self,H,B).
assertion_hb(metta_atom_asserted(Self,[=,H,B]),Self,H,B).
assertion_hb(metta_atom(Self,[=,H,B]),Self,H,B).

load_hook0(_,_):- \+ show_transpiler, \+ is_transpiling, !.
load_hook0(Load,Assertion):-
      assertion_hb(Assertion,Self,H,B),
       functs_to_preds([=,H,B],Preds),
       assert_preds(Self,Load,Preds).
load_hook0(_,_):- \+ current_prolog_flag(metta_interp,ready),!.
/*
load_hook0(Load,get_metta_atom(Eq,Self,H)):- B = 'True',
       H\=[':'|_], functs_to_preds([=,H,B],Preds),
       assert_preds(Self,Load,Preds).
*/
is_transpiling:- use_metta_compiler.
use_metta_compiler:- notrace(option_value('compile','full')), !.
preview_compiler:- \+ option_value('compile',false), !.
%preview_compiler:- use_metta_compiler,!.
show_transpiler:- option_value('code',Something), Something\==silent,!.
show_transpiler:- preview_compiler.
option_switch_pred(F):- 
  current_predicate(F/0),interpreter_source_file(File),
  source_file(F, File), \+ \+ (member(Prefix,[is_,show_,trace_on_]), symbol_concat(Prefix,_,F)).

do_show_option_switches :- 
  forall(option_switch_pred(F),(call(F)-> writeln(yes(F)); writeln(not(F)))). 
do_show_options_values:-
  forall((nb_current(N,V), \+((symbol(N),symbol_concat('$',_,N)))),write_src_nl(['pragma!',N,V])),
  do_show_option_switches. 

op_decl('pragma!', [ 'Atom', 'Atom'], 'EmptyType').

op_decl('match', [ 'hyperon::space::DynSpace', 'Atom', 'Atom'], '%Undefined%').
op_decl('remove-atom', [ 'hyperon::space::DynSpace', 'Atom'], 'EmptyType').
op_decl('add-atom', [ 'hyperon::space::DynSpace', 'Atom'], 'EmptyType').
op_decl('get-atoms', [ 'hyperon::space::DynSpace' ], 'Atom').

op_decl('get-state', [[ 'StateMonad', Type]],Type).
op_decl('change-state!', [[ 'StateMonad',Type],Type],'EmptyType').
op_decl('new-state', [Type], ['StateMonad',Type ]).

op_decl('car-atom', [ 'Expression' ], 'Atom').
op_decl('cdr-atom', [ 'Expression' ], 'Expression').

op_decl(let, [ 'Atom', '%Undefined%', 'Atom' ], 'Atom').
op_decl('let*', [ 'Expression', 'Atom' ], 'Atom').

op_decl(and, [ 'Bool', 'Bool' ], 'Bool').
op_decl(or, [ 'Bool', 'Bool' ], 'Bool').
op_decl(case, [ 'Expression', 'Atom' ], 'Atom').
/*
op_decl(apply, [ 'Atom', 'Variable', 'Atom' ], 'Atom').
op_decl(chain, [ 'Atom', 'Variable', 'Atom' ], 'Atom').
op_decl('filter-atom', [ 'Expression', 'Variable', 'Atom' ], 'Expression').
op_decl('foldl-atom', [ 'Expression', 'Atom', 'Variable', 'Variable', 'Atom' ], 'Atom').
op_decl('map-atom', [ 'Expression', 'Variable', 'Atom' ], 'Expression').
op_decl(quote, [ 'Atom' ], 'Atom').
op_decl('if-decons', [ 'Atom', 'Variable', 'Variable', 'Atom', 'Atom' ], 'Atom').
op_decl('if-empty', [ 'Atom', 'Atom', 'Atom' ], 'Atom').
op_decl('if-error', [ 'Atom', 'Atom', 'Atom' ], 'Atom').
op_decl('if-non-empty-expression', [ 'Atom', 'Atom', 'Atom' ], 'Atom').
op_decl('if-not-reducible', [ 'Atom', 'Atom', 'Atom' ], 'Atom').
op_decl(return, [ 'Atom' ], 'ReturnType').
op_decl('return-on-error', [ 'Atom', 'Atom'], 'Atom').
op_decl(unquote, [ '%Undefined%'], '%Undefined%').
op_decl(cons, [ 'Atom', 'Atom' ], 'Atom').
op_decl(decons, [ 'Atom' ], 'Atom').
op_decl(empty, [], '%Undefined%').
op_decl('Error', [ 'Atom', 'Atom' ], 'ErrorType').
op_decl(function, [ 'Atom' ], 'Atom').
op_decl(id, [ 'Atom' ], 'Atom').
op_decl(unify, [ 'Atom', 'Atom', 'Atom', 'Atom' ], 'Atom').
*/
op_decl(eval, [ 'Atom' ], 'Atom').
op_decl(unify, [ 'Atom', 'Atom', 'Atom', 'Atom'], '%Undefined%').
op_decl(if, [ 'Bool', 'Atom', 'Atom'], _T).
op_decl('%', [ 'Number', 'Number' ], 'Number').
op_decl('*', [ 'Number', 'Number' ], 'Number').
op_decl('-', [ 'Number', 'Number' ], 'Number').
op_decl('+', [ 'Number', 'Number' ], 'Number').
op_decl(combine, [ X, X], X).

op_decl('bind!', ['Symbol','%Undefined%'], 'EmptyType').
op_decl('import!', ['hyperon::space::DynSpace','Atom'], 'EmptyType').
op_decl('get-type', ['Atom'], 'Type').

type_decl('Any').
type_decl('Atom').
type_decl('Bool').
type_decl('ErrorType').
type_decl('Expression').
type_decl('Number').
type_decl('ReturnType').
type_decl('hyperon::space::DynSpace').
type_decl('Symbol').
type_decl('StateMonad').
type_decl('Type').
type_decl('%Undefined%').
type_decl('Variable').

:- dynamic(get_metta_atom/2).
:- dynamic(metta_atom_asserted/2).
:- multifile(asserted_metta/4).
:- dynamic(asserted_metta/4).
% metta_atom_stdlib(_):-!,fail.
metta_atom_stdlib(X):- metta_atom_stdlib_types(X).
metta_atom_stdlib_types([':', Type, 'Type']):- type_decl(Type).
metta_atom_stdlib_types([':', Op, [->|List]]):- 
	 atom(Op), op_decl(Op,Params,ReturnType),
	 append(Params,[ReturnType],List).

%get_metta_atom(Eq,KB, [F|List]):- KB='&flybase',fb_pred(F, Len), length(List,Len),apply(F,List).


get_metta_atom(Eq,Space, Atom):- get_metta_atom_from(Space, Atom), \+ (Atom =[EQ,_,_], EQ==Eq).

get_metta_atom_from(KB, [F, A| List]):- KB='&flybase',fb_pred_nr(F, Len),current_predicate(F/Len), length([A|List],Len),apply(F,[A|List]).
get_metta_atom_from([Superpose,ListOf], Atom):- Superpose == 'superpose',is_list(ListOf),!,member(KB,ListOf),get_metta_atom_from(KB,Atom).
get_metta_atom_from(Space, Atom):- typed_list(Space,_,L),!, member(Atom,L).
get_metta_atom_from(KB,Atom):- (KB=='&self'; KB='&stdlib'), metta_atom_stdlib(Atom).
get_metta_atom_from(KB,Atom):- if_or_else(metta_atom_asserted( KB,Atom),metta_atom_asserted_fallback( KB,Atom)).

metta_atom_asserted_fallback( KB,Atom):- fail, is_list(KB),!, member(Atom,KB).
%metta_atom_asserted_fallback( KB,Atom):- get_metta_atom_from(KB,Atom)

%metta_atom(KB,[F,A|List]):- metta_atom(KB,F,A,List), F \== '=',!.
metta_atom(KB,Atom):- get_metta_atom_from(KB,Atom).
metta_defn(KB,Head,Body):- metta_defn(_Eq,KB,Head,Body).
metta_defn(Eq,KB,Head,Body):- ignore(Eq = '='), get_metta_atom_from(KB,[Eq,Head,Body]).
metta_type(S,H,B):- get_metta_atom_from(S,[':',H,B]).
metta_type(_,H,B):- metta_atom_stdlib_types([':',H,B]).
%typed_list(Cmpd,Type,List):-  compound(Cmpd), Cmpd\=[_|_], compound_name_arguments(Cmpd,Type,[List|_]),is_list(List).

:- if( \+ current_predicate(pfcAdd/1 )).
pfcAdd(P):- assert(P).
:- endif.

%maybe_xform(metta_atom(KB,[F,A|List]),metta_atom(KB,F,A,List)):- is_list(List),!.
maybe_xform(metta_defn(Eq,KB,Head,Body),metta_atom(KB,[Eq,Head,Body])).
maybe_xform(metta_type(KB,Head,Body),metta_atom(KB,[':',Head,Body])).
maybe_xform(metta_atom(KB,HeadBody),metta_atom_asserted(KB,HeadBody)).
maybe_xform(_OBO,_XForm):- !, fail.

metta_atom_asserted(KB,HeadBody):- asserted_metta(KB,HeadBody,_,_).

metta_anew1(Load,_OBO):- var(Load),trace,!.
metta_anew1(Ch,OBO):-  metta_interp_mode(Ch,Mode), !, metta_anew1(Mode,OBO).
metta_anew1(Load,OBO):- maybe_xform(OBO,XForm),!,metta_anew1(Load,XForm).
metta_anew1(load,OBO):- OBO= metta_atom(Space,Atom),!,'add-atom'(Space, Atom).
metta_anew1(unload,OBO):- OBO= metta_atom(Space,Atom),!,'remove-atom'(Space, Atom).

metta_anew1(load,OBO):- !, 
  must_det_ll((load_hook(load,OBO),
   subst_vars(OBO,Cl),
   pfcAdd_Now(Cl))). %to_metta(Cl).
metta_anew1(load,OBO):- !, 
  must_det_ll((load_hook(load,OBO), 
  subst_vars(OBO,Cl),
  show_failure(pfcAdd_Now(Cl)))).
metta_anew1(unload,OBO):- subst_vars(OBO,Cl),load_hook(unload,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  ignore((clause(Head,Body,Ref),clause(Head2,Body2,Ref),(Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(Cl))).

% TODO uncomment this next line but it is breaking the curried chainer
% pfcAdd_Now(P):- pfcAdd(P),!.
pfcAdd_Now(P):- assertz(P),!.

metta_anew2(Load,_OBO):- var(Load),trace,!.
metta_anew2(Load,OBO):- maybe_xform(OBO,XForm),!,metta_anew2(Load,XForm).
metta_anew2(Ch,OBO):-  metta_interp_mode(Ch,Mode), !, metta_anew2(Mode,OBO).
metta_anew2(load,OBO):- must_det_ll((load_hook(load,OBO),subst_vars_not_last(OBO,Cl),assertz_if_new(Cl))). %to_metta(Cl).
metta_anew2(unload,OBO):- subst_vars_not_last(OBO,Cl),load_hook(unload,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  ignore((clause(Head,Body,Ref),clause(Head2,Body2,Ref),(Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(Cl))).


metta_anew(Load,Src,OBO):- maybe_xform(OBO,XForm),!,metta_anew(Load,Src,XForm).
metta_anew(Ch, Src, OBO):-  metta_interp_mode(Ch,Mode), !, metta_anew(Mode,Src,OBO).
metta_anew(Load,_Src,OBO):- silent_loading,!,metta_anew1(Load,OBO).
metta_anew(Load,Src,OBO):- 
    not_compat_io((
	if_show(load,color_g_mesg('#ffa500', ((format('~N '), write_src(Src))))),
	% format('~N'),
	if_verbose(load,color_g_mesg('#0f0f0f',(write('  ; Action: '),writeq(Load=OBO),nl))))),
   metta_anew1(Load,OBO),not_compat_io((format('~N'))).

subst_vars_not_last(A,B):-
  functor(A,_F,N),arg(N,A,E),
  subst_vars(A,B),
  nb_setarg(N,B,E),!.

con_write(W):-check_silent_loading, not_compat_io((write(W))).
con_writeq(W):-check_silent_loading, not_compat_io((writeq(W))).
writeqln(Q):- check_silent_loading,not_compat_io((write(' '),con_writeq(Q),connl)).


into_space(Self,'&self',Self):-!.
into_space(_,Other,Other):-!.


into_space(Self,Myself,SelfO):- into_space(30,Self,Myself,SelfO).

into_space(_Dpth,Self,Myself,Self):-Myself=='&self',!.
into_space(_Dpth,Self,None,Self):- 'None' == None,!.
into_space(Depth,Self,Other,Result):- eval_H(Depth,Self,Other,Result).
into_name(_,Other,Other).

%eval_f_args(Depth,Self,F,ARGS,[F|EARGS]):- maplist(eval_H(Depth,Self),ARGS,EARGS).


combine_result(TF,R2,R2):- TF == [], !.
combine_result(TF,_,TF):-!.


do_metta1_e(_Self,_,exec(Exec)):- !,write_exec(Exec),!.
do_metta1_e(_Self,_,[=,A,B]):- !, with_concepts(false,
  (con_write('(= '), with_indents(false,write_src(A)), 
    (is_list(B) -> connl ; true),
    con_write(' '),with_indents(true,write_src(B)),con_write(')'))),connl.
do_metta1_e(_Self,_LoadExec,Term):- write_src(Term),connl.

write_exec(Exec):- notrace(write_exec0(Exec)).
%write_exec0(Exec):- symbol(Exec),!,write_exec0([Exec]).

write_exec0(Exec):-
  wots(S,write_src(exec(Exec))),
  nb_setval(exec_src,Exec),
  format('~N'),
  ignore((notrace((color_g_mesg('#0D6328',writeln(S)))))).



asserted_do_metta(Space,Ch,Src):- metta_interp_mode(Ch,Mode), !, asserted_do_metta(Space,Mode,Src).

asserted_do_metta(Space,Load,Src):- Load==exec,!,do_metta_exec(python,Space,Src,_Out).
asserted_do_metta(Space,Load,Src):- asserted_do_metta2(Space,Load,Src,Src).

asserted_do_metta2(Space,Ch,Info,Src):- metta_interp_mode(Ch,Mode), !, asserted_do_metta2(Space,Mode,Info,Src).
asserted_do_metta2(Self,Load,[TypeOp,Fn,Type], Src):- 
 TypeOp == ':',  \+ is_list(Type),!,
 must_det_ll((
  color_g_mesg_ok('#ffa500',
  metta_anew(Load,Src,metta_atom(Self,[':',Fn,Type]))))),!.

asserted_do_metta2(Self,Load,[TypeOp,Fn,TypeDecL], Src):- 
 TypeOp == ':',!,
 must_det_ll((
  decl_length(TypeDecL,Len),LenM1 is Len - 1, last_element(TypeDecL,LE),
  color_g_mesg_ok('#ffa500',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecL]))),
  metta_anew1(Load,metta_arity(Self,Fn,LenM1)),
  arg_types(TypeDecL,[],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),!,
  metta_anew1(Load,metta_last(Self,Fn,LE)))).
/*
asserted_do_metta2(Self,Load,[TypeOp,Fn,TypeDecL,RetType], Src):- 
 TypeOp == ':',!,
 must_det_ll((
  decl_length(TypeDecL,Len),
  append(TypeDecL,[RetType],TypeDecLRet),
  color_g_mesg_ok('#ffa500',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecLRet]))),
  metta_anew1(Load,metta_arity(Self,Fn,Len)),
  arg_types(TypeDecL,[RetType],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),
  metta_anew1(Load,metta_return(Self,Fn,RetType)))),!.
*/
/*do_metta(File,Self,Load,PredDecl, Src):-fail,
   metta_anew(Load,Src,metta_atom(Self,PredDecl)),
   ignore((PredDecl=['=',Head,Body], metta_anew(Load,Src,metta_defn(Eq,Self,Head,Body)))),
   ignore((Body == 'True',!,do_metta(File,Self,Load,Head))),
   nop((fn_append(Head,X,Head), fn_append(PredDecl,X,Body),
   metta_anew((Head:- Body)))),!.*/

asserted_do_metta2(Self,Load,[EQ,Head,Result], Src):- EQ=='=', !,
 must_det_ll((
    discover_head(Self,Load,Head),
    color_g_mesg_ok('#ffa500',metta_anew(Load,Src,metta_defn(EQ,Self,Head,Result))),
    discover_body(Self,Load,Result))).

asserted_do_metta2(Self,Load,PredDecl, Src):-
   ignore(discover_head(Self,Load,PredDecl)),
   color_g_mesg_ok('#ffa500',metta_anew(Load,Src,metta_atom(Self,PredDecl))).


always_exec(exec(W)):- !, is_list(W), always_exec(W).
always_exec(Comp):- compound(Comp),compound_name_arity(Comp,Name,N),symbol_concat('eval',_,Name),Nm1 is N-1, arg(Nm1,Comp,TA),!,always_exec(TA).
always_exec(List):- \+ is_list(List),!,fail.
always_exec([Var|_]):- \+ symbol(Var),!,fail.
always_exec(['extend-py!'|_]):- !, fail.
always_exec([H|_]):- symbol_concat(_,'!',H),!. %pragma!/print!/transfer!/include! etc
always_exec(['assertEqualToResult'|_]):-!,fail.
always_exec(['assertEqual'|_]):-!,fail.
always_exec(_):-!,fail. % everything else

file_hides_results([W|_]):- W== 'pragma!'.

if_t(A,B,C):- trace,if_t((A,B),C).


check_answers_for(TermV,Ans):- (string(TermV);var(Ans);var(TermV)),!,fail.
check_answers_for(TermV,_):-  sformat(S,'~q',[TermV]),atom_contains(S,"[assert"),!,fail.
check_answers_for(_,Ans):- contains_var('BadType',Ans),!,fail.
check_answers_for(TermV,_):-  inside_assert(TermV,BaseEval), always_exec(BaseEval),!,fail.

%check_answers_for([TermV],Ans):- !, check_answers_for(TermV,Ans).
%check_answers_for(TermV,[Ans]):- !, check_answers_for(TermV,Ans).
check_answers_for(_,_).

	/*
got_exec_result2(Val,Nth,Ans):- is_list(Ans), exclude(==(','),Ans,Ans2), Ans\==Ans2,!,
  got_exec_result2(Val,Nth,Ans2).
got_exec_result2(Val,Nth,Ans):-
 must_det_ll((
  Nth100 is Nth+100,
  get_test_name(Nth100,TestName),
  nb_current(exec_src,Exec),
  if_t( ( \+ is_unit_test_exec(Exec)),
  ((equal_enough(Val,Ans)
     -> write_pass_fail_result_now(TestName,exec,Exec,'PASS',Ans,Val)
      ; write_pass_fail_result_now(TestName,exec,Exec,'FAIL',Ans,Val)))))).

write_pass_fail_result_now(TestName,exec,Exec,PASS_FAIL,Ans,Val):-
   (PASS_FAIL=='PASS'->flag(loonit_success, X, X+1);flag(loonit_failure, X, X+1)),
   (PASS_FAIL=='PASS'->Color=cyan;Color=red),
   color_g_mesg(Color,write_pass_fail_result_c(TestName,exec,Exec,PASS_FAIL,Ans,Val)),!,nl,
   nl,writeln('--------------------------------------------------------------------------'),!.

write_pass_fail_result_c(TestName,exec,Exec,PASS_FAIL,Ans,Val):-
  nl,write_mobj(exec,[(['assertEqualToResult',Exec,Ans])]),
  nl,write_src('!'(['assertEqual',Val,Ans])),
  write_pass_fail_result(TestName,exec,Exec,PASS_FAIL,Ans,Val).
*/

is_unit_test_exec(Exec):- sformat(S,'~w',[Exec]),sub_atom(S,_,_,_,'assert').
is_unit_test_exec(Exec):- sformat(S,'~q',[Exec]),sub_atom(S,_,_,_,"!',").

make_empty('Empty').
make_empty(_,Empty):- make_empty(Empty).
make_empty(_RetType,_,Empty):- make_empty(Empty).


convert_tax(_How,Self,Tax,Expr,NewHow):-
  metta_interp_mode(Ch,Mode),
  string_concat(Ch,TaxM,Tax),!,
  normalize_space(string(NewTax),TaxM),
  convert_tax(Mode,Self,NewTax,Expr,NewHow).
convert_tax(How,_Self,Tax,Expr,How):-
  %parse_sexpr_metta(Tax,Expr).
  normalize_space(string(NewTax),Tax),
  parse_sexpr_metta1(NewTax,Expr).

:- if( \+ current_predicate(notrace/1) ).
  notrace(G):- once(G).
:- endif.

metta_interp_mode('+',load).
metta_interp_mode('-',unload).
metta_interp_mode('!',exec).
metta_interp_mode('?',call).
metta_interp_mode('^',load_like_file).


call_sexpr(How,Self,Tax,_S,Out):-
  (symbol(Tax);string(Tax)),
    normalize_space(string(TaxM),Tax),
    convert_tax(How,Self,TaxM,Expr,NewHow),!,
    show_call(do_metta(python,NewHow,Self,Expr,Out)).

do_metta(File,Load,Self,Cmt,Out):-
  if_trace(do_metta, fbug(do_metta(File,Load,Self,Cmt,Out))),fail.

do_metta(_File,_Load,_Self,In,Out):- var(In),!,In=Out.
do_metta(_From,_Mode,_Self,end_of_file,'Empty'):- !. %, halt(7), writeln('\n\n% To restart, use: ?- repl.').
do_metta(_File,_Load,_Self,Cmt,Out):- Cmt==[],!, Out=[].

do_metta(From,Load,Self,'$COMMENT'(Expr,_,_),Out):- !, do_metta(From,comment(Load),Self,Expr,Out).
do_metta(From,Load,Self,'$STRING'(Expr),Out):- !, do_metta(From,comment(Load),Self,Expr,Out).
do_metta(From,comment(Load),Self,[Expr],Out):-  !, do_metta(From,comment(Load),Self,Expr,Out).
do_metta(From,comment(Load),Self,Cmt,Out):- write_comment(Cmt),  !,
   ignore(( symbolic(Cmt),symbolic_list_concat([_,Src],'MeTTaLog only: ',Cmt),!,atom_string(Src,SrcCode),do_metta(mettalog_only(From),Load,Self,SrcCode,Out))),
   ignore(( symbolic(Cmt),symbolic_list_concat([_,Src],'MeTTaLog: ',Cmt),!,atom_string(Src,SrcCode),do_metta(mettalog_only(From),Load,Self,SrcCode,Out))),!.

do_metta(From,How,Self,Src,Out):- string(Src),!,
    normalize_space(string(TaxM),Src),
    convert_tax(How,Self,TaxM,Expr,NewHow),!,
    do_metta(From,NewHow,Self,Expr,Out).

do_metta(From,_,Self,exec(Expr),Out):- !, do_metta(From,exec,Self,Expr,Out).
do_metta(From,_,Self,  call(Expr),Out):- !, do_metta(From,call,Self,Expr,Out).
do_metta(From,_,Self,     ':-'(Expr),Out):- !, do_metta(From,call,Self,Expr,Out).
do_metta(From,call,Self,TermV,FOut):- !,
   call_for_term_variables(TermV,Term,NamedVarsList,X), must_be(nonvar,Term),
   copy_term(NamedVarsList,Was),
   Output = NamedVarsList,
   user:interactively_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut).
do_metta(_File,Load,Self,Src,Out):- Load\==exec, !, 
	 as_tf(asserted_do_metta(Self,Load,Src),Out).

do_metta(file(Filename),exec,Self,TermV,Out):-
   must_det_ll((inc_exec_num(Filename),
     get_exec_num(Filename,Nth),
     Nth>0)),
	((
	 is_synthing_unit_tests,
     file_answers(Filename, Nth, Ans),
     check_answers_for(TermV,Ans))),!,
     must_det_ll((
      color_g_mesg_ok('#ffa500',
       (writeln(';; In file as:  '),
        color_g_mesg([bold,fg('#FFEE58')], write_src(exec(TermV))),
        write(';; To unit test case:'))),!,
        do_metta_exec(file(Filename),Self,['assertEqualToResult',TermV,Ans],Out))).

do_metta(From,exec,Self,TermV,Out):- !, do_metta_exec(From,Self,TermV,Out).

do_metta_exec(From,Self,TermV,FOut):-
  Output = X,
  not_compatio(write_exec(TermV)),
   notrace(into_metta_callable(Self,TermV,Term,X,NamedVarsList,Was)),!,
   user:interactively_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),!.


call_for_term_variables(TermV,catch_red(show_failure(Term)),NamedVarsList,X):-
 term_variables(TermV, AllVars), call_for_term_variables4v(TermV,AllVars,Term,NamedVarsList,X),!,
 must_be(callable,Term).
call_for_term_variables(TermV,catch_red(show_failure(Term)),NamedVarsList,X):-
  get_term_variables(TermV, DCAllVars, Singletons, NonSingletons),
  call_for_term_variables5(TermV, DCAllVars, Singletons, NonSingletons, Term,NamedVarsList,X),!,
  must_be(callable,Term).

into_metta_callable(_Self,TermV,Term,X,NamedVarsList,Was):- 
 is_transpiling, !,
 must_det_ll((((

 % ignore(Res = '$VAR'('ExecRes')),
  RealRes = Res,
  compile_for_exec(Res,TermV,ExecGoal),!,
  subst_vars(Res+ExecGoal,Res+Term,NamedVarsList),
  copy_term(NamedVarsList,Was),
  term_variables(Term,Vars),
  notrace((color_g_mesg('#114411',print_pl_source(answer(Res):-ExecGoal)))),
  %nl,writeq(Term),nl,
  ((\+ \+
  ((numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(bind)]),
  %nb_current(variable_names,NamedVarsList),
  %nl,print(subst_vars(Term,NamedVarsList,Vars)),
  nop(nl))))),
  nop(maplist(verbose_unify,Vars)),
  %NamedVarsList=[_=RealRealRes|_],
  var(RealRes), X = RealRes)))),!.


into_metta_callable(Self,TermV,CALL,X,NamedVarsList,Was):-!,
 option_else('stack-max',StackMax,100),
 CALL = eval_H(StackMax,Self,Term,X),
 notrace(( must_det_ll((
 if_t(show_transpiler,write_compiled_exec(TermV,_Goal)),
  subst_vars(TermV,Term,NamedVarsList),
  copy_term(NamedVarsList,Was)
  %term_variables(Term,Vars),
  %nl,writeq(Term),nl,
  %skip((\+ \+
  %((numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(bind)]),  %nb_current(variable_names,NamedVarsList),
  %nl,print(subst_vars(TermV,Term,NamedVarsList,Vars)),nl)))),
  %nop(maplist(verbose_unify,Vars)))))),!.
  )))),!.


  
eval_S(Self,Form):- nonvar(Form),
  current_self(SelfS),SelfS==Self,!,
  do_metta(true,exec,Self,Form,_Out).
eval_H(Term,X):- eval_args(Term,X).
eval_H(StackMax,Self,Term,X):-  eval_args('=',_,StackMax,Self,Term,X).
/*
eval_H(StackMax,Self,Term,X).

eval_H(StackMax,Self,Term,X):-
  Time = 90.0,
 ((always_exec(Term)) ->
    if_or_else(t1('=',_,StackMax,Self,Term,X),
                      (t2('=',_,StackMax,Self,Term,X)));
    call_max_time(t1('=',_,StackMax,Self,Term,X),   Time,
                              (t2('=',_,StackMax,Self,Term,X)))).

eval_H(Term,X):-
    current_self(Self), StackMax = 100,
    if_or_else((t1('=',_,StackMax,Self,Term,X),X\==Term),(t2('=',_,StackMax,Self,Term,X),nop(X\==Term))).


t1('=',_,StackMax,Self,Term,X):- eval_args('=',_,StackMax,Self,Term,X).
t2('=',_,StackMax,Self,Term,X):- fail, subst_args('=',_,StackMax,Self,Term,X).
*/

%eval_H(Term,X):- if_or_else((subst_args(Term,X),X\==Term),(eval_args(Term,Y),Y\==Term)).

print_goals(TermV):- write_src(TermV).


if_or_else(Goal,Else):- call(Goal)*->true;call(Else).

interacting:- tracing,!.
interacting:- current_prolog_flag(debug,true),!.
interacting:- option_value(interactive,true),!.
interacting:- option_value(prolog,true),!.

% call_max_time(+Goal, +MaxTime, +Else)
call_max_time(Goal,_MaxTime, Else) :- interacting,!, if_or_else(Goal,Else).
call_max_time(Goal,_MaxTime, Else) :- !, if_or_else(Goal,Else).
call_max_time(Goal, MaxTime, Else) :-
    catch(if_or_else(call_with_time_limit(MaxTime, Goal),Else), time_limit_exceeded, Else).


catch_err(G,E,C):- catch(G,E,(notrace(if_t(symbol(E),throw(E))),C)).

load_and_trim_history:- 
 notrace((
  current_input(In), %catch(load_history,_,true),
  ignore(install_readline(In)) )).

%repl:- option_value('repl',prolog),!,prolog.
%:- ensure_loaded(metta_toplevel).

%:- discontiguous do_metta_exec/3.

%repl:- setup_call_cleanup(flag(repl_level,Was,Was+1),repl0,
 % (flag(repl_level,_,Was),(Was==0 -> maybe_halt(7) ; true))).

repl:-  catch(repl2,end_of_input,true).

repl1:-   
   with_option('doing_repl',true,
     with_option(repl,true,repl2)). %catch((repeat, repl2, fail)'$aborted',true).
repl2:-
   load_and_trim_history,
   repeat,
     %set_prolog_flag(gc,true),
     garbage_collect,
     %set_prolog_flag(gc,false),
     %with_option(not_a_reload,true,make),
      ignore(catch(once(repl3),restart_reading,true)),
      %set_prolog_flag(gc,true),
      fail.
repl3:-
    notrace(( reset_eval_num,
     current_self(Self),
     current_read_mode(repl,Mode),
     %ignore(shell('stty sane ; stty echo')),
     %current_input(In),
      'format'(atom(P),'metta ~w ~w> ',[Self, Mode]))), 
      setup_call_cleanup(
         notrace(prompt(Was,P)),
         notrace((ttyflush,repl_read(Expr),ttyflush)),
         notrace(prompt(_,Was))),
      if_trace(replt,fbug(repl_read(Mode,Expr))),
      %fbug(repl_read(Expr)),
      notrace(if_t(Expr==end_of_file,throw(end_of_input))),
      %ignore(shell('stty sane ; stty echo')),
      notrace(ignore(check_has_directive(Expr))),
      once(do_metta(repl_true,Mode,Self,Expr,_)).



check_has_directive(V):- var(V),!,fail.
check_has_directive('log.'):- switch_to_mettalog,!.
check_has_directive('rust.'):- switch_to_mettarust,!.
check_has_directive(Atom):- symbol(Atom),symbol_concat(_,'.',Atom),!.
check_has_directive(call(N=V)):- nonvar(N),!, set_directive(N,V).
check_has_directive(call(Rtrace)):- rtrace == Rtrace,!, rtrace,notrace(throw(restart_reading)).
check_has_directive(NEV):- symbol(NEV), symbolic_list_concat([N,V],'=',NEV), set_directive(N,V).
check_has_directive([AtEq,Value]):-symbol(AtEq),symbol_concat('@',Name,AtEq), set_directive(Name,Value).
check_has_directive(ModeChar):- symbol(ModeChar),metta_interp_mode(ModeChar,_Mode),!,set_directive(repl_mode,ModeChar).
check_has_directive('@'):- do_show_options_values,!,notrace(throw(restart_reading)).
check_has_directive(AtEq):-symbol(AtEq),symbol_concat('@',NEV,AtEq),option_value(NEV,Foo),fbug(NEV=Foo),!,notrace(throw(restart_reading)).
check_has_directive(_).

set_directive(N,V):- symbol_concat('@',NN,N),!,set_directive(NN,V).
set_directive(N,V):- N=='mode',!,set_directive((repl_mode),V).
set_directive(N,V):- show_call(set_option_value_interp(N,V)),!,notrace(throw(restart_reading)).

read_pending_white_codes(In):-
  read_pending_codes(In,[10],[]),!.
read_pending_white_codes(_).

call_for_term_variables4v(Term,[]  ,as_tf(Term,TF),NamedVarsList,TF):- get_global_varnames(NamedVarsList),!.
call_for_term_variables4v(Term,[X]  ,       Term,      NamedVarsList,X):- get_global_varnames(NamedVarsList).


not_in_eq(List, Element) :-
    member(V, List), V == Element.

get_term_variables(Term, DontCaresN, CSingletonsN, CNonSingletonsN) :-
    term_variables(Term, AllVars),
    get_global_varnames(VNs),
    writeqln(term_variables(Term, AllVars)=VNs),
    term_singletons(Term, Singletons),
    term_dont_cares(Term, DontCares),
    include(not_in_eq(Singletons), AllVars, NonSingletons),
    include(not_in_eq(DontCares), NonSingletons, CNonSingletons),
    include(not_in_eq(DontCares), Singletons, CSingletons),
    maplist(into_named_vars,[DontCares, CSingletons,  CNonSingletons],
                           [DontCaresN, CSingletonsN, CNonSingletonsN]),
    writeqln([DontCaresN, CSingletonsN, CNonSingletonsN]).

term_dont_cares(Term, DontCares):-
  term_variables(Term, AllVars),
  get_global_varnames(VNs),
  include(has_sub_var(AllVars),VNs,HVNs),
  include(underscore_vars,HVNs,DontCareNs),
  maplist(arg(2),DontCareNs,DontCares).

into_named_vars(Vars,L):- is_list(Vars), !, maplist(name_for_var_vn,Vars,L).
into_named_vars(Vars,L):- term_variables(Vars,VVs),!,into_named_vars(VVs,L).

has_sub_var(AllVars,_=V):- sub_var(V,AllVars).
underscore_vars(V):- var(V),!,name_for_var(V,N),!,underscore_vars(N).
underscore_vars(N=_):- !, symbolic(N),!,underscore_vars(N).
underscore_vars(N):- symbolic(N),!,symbol_concat('_',_,N).

get_global_varnames(VNs):- nb_current('variable_names',VNs),VNs\==[],!.
get_global_varnames(VNs):- prolog_load_context(variable_names,VNs),!.
maybe_set_var_names(List):- List==[],!.
maybe_set_var_names(List):- % fbug(maybe_set_var_names(List)),
   is_list(List),!,nb_linkval(variable_names,List).
maybe_set_var_names(_).

name_for_var_vn(V,N=V):- name_for_var(V,N).

name_for_var(V,N):- var(V),!,get_global_varnames(VNs),member(N=VV,VNs),VV==V,!.
name_for_var(N=_,N):- !.
name_for_var(V,N):- term_to_atom(V,N),!.


  %call_for_term_variables5(Term,[],as_tf(Term,TF),[],TF):- symbol(Term),!.
call_for_term_variables5(Term,[],[],[],as_tf(Term,TF),[],TF):- ground(Term),!.
call_for_term_variables5(Term,DC,[],[],call_nth(Term,TF),DC,TF):- ground(Term),!.
call_for_term_variables5(Term,_,[],[_=Var],call_nth(Term,Count),['Count'=Count],Var).
call_for_term_variables5(Term,_,[_=Var],[],call_nth(Term,Count),['Count'=Count],Var).
call_for_term_variables5(Term,_,Vars,[_=Var],Term,Vars,Var).
call_for_term_variables5(Term,_,[_=Var],Vars,Term,Vars,Var).
call_for_term_variables5(Term,_,SVars,Vars,call_nth(Term,Count),[Vars,SVars],Count).



is_interactive(From):- notrace(is_interactive0(From)).
is_interactive0(From):- From==false,!,fail.
is_interactive0(From):- symbolic(From),is_stream(From),!, \+ stream_property(From,filename(_)).
is_interactive0(From):- From = repl_true,!.
is_interactive0(From):- From = true,!.


:- set_prolog_flag(history, 3).

inside_assert(Var,Var):- \+ compound(Var),!.
inside_assert([H,IA,_],IA):- symbol(H),symbol_concat('assert',_,H),!.
inside_assert(Conz,Conz):- is_conz(Conz),!.
inside_assert(exec(I),O):- !, inside_assert(I,O).
inside_assert(Eval,O):- functor(Eval,eval_H,A), A1 is A-1, arg(A1,Eval,I),!, inside_assert(I,O).
%inside_assert(eval_H(I,C),eval_H(O,C)):- !, inside_assert(I,O).
%inside_assert(eval_H(A,B,I,C),eval_H(A,B,O,C)):- !, inside_assert(I,O).
inside_assert(call(I),O):- !, inside_assert(I,O).
inside_assert( ?-(I), O):- !, inside_assert(I,O).
inside_assert( :-(I), O):- !, inside_assert(I,O).
inside_assert(Var,Var).

current_read_mode(repl,Mode):- ((nb_current(repl_mode,Mode),Mode\==[])->true;Mode='+'),!.
current_read_mode(file,Mode):- ((nb_current(file_mode,Mode),Mode\==[])->true;Mode='+').



eval(all(Form)):- nonvar(Form), !, forall(eval(Form,_),true).
eval(Form):-   current_self(Self),   do_metta(true,exec,Self,Form,_Out).
eval(Form,Out):-current_self(Self),eval(Self,Form,Out). 
eval(Self,Form,Out):- eval_H(100,Self,Form,Out).

name_vars(P):- ignore(name_vars0(P)).
name_vars0(X=Y):- X==Y,!.
name_vars0(X='$VAR'(X)).

interactively_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut):-
  catch(interactively_do_metta_exec00(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),
		 Error,write_src(error(Error,From,TermV))).


interactively_do_metta_exec00(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut):-
  catch(interactively_do_metta_exec01(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),
		 '$aborted',fbug(aborted(From,TermV))).

% Interactively executes a mettalog command if certain conditions are met and hides results based on file settings.
interactively_do_metta_exec01(file(_), Self, _TermV, Term, X, _NamedVarsList, _Was, _Output, _FOut) :-
    file_hides_results(Term), !,
    eval_args(Self, Term, X).

interactively_do_metta_exec01(From,Self,_TermV,Term,X,NamedVarsList,Was,Output,FOut):-
  notrace((
  	reset_eval_num,
    Result = res(FOut),
	Prev = prev_result('Empty'),
    inside_assert(Term,BaseEval),
    (is_compatio
       -> option_else(answer,Leap,leap)
         ;   option_else(answer,Leap,each)),
    option_else('maximum-result-count',MaxResults,inf), % infinate answers
    option_else('initial-result-count',LeashResults,10), % if print the first 10 answers without stopping
    Control = contrl(MaxResults,Leap),
    Skipping = _,
    % Initialize Control as a compound term with 'each' as its argument.
    %GG = interact(['Result'=X|NamedVarsList],Term,trace_off),
    (((From = file(_Filename), option_value('exec',skip),  \+ always_exec(BaseEval)))
     -> (GG = (skip(Term),deterministic(Complete)),
               %Output =
                %FOut = "Skipped",
                Skipping = 1,!,
                %color_g_mesg('#da70d6', (write('% SKIPPING: '), writeq(eval_H(100,Self,BaseEval,X)),writeln('.'))),
                % color_g_mesg('#fa90f6', (writeln('; SKIPPING'), with_indents(true,write_src(exec(BaseEval))))),
               %  if_t(is_list(BaseEval),add_history_src(exec(TermV))),
                 true
             )
        ; GG =      %$ locally(set_prolog_flag(gc,false),
           (
                             ((  (Term),deterministic(Complete), nb_setarg(1,Result,Output)))),
    !, % metta_toplevel
   flag(result_num,_,0),
   PL=eval(Self,BaseEval,X),
 ( % with_indents(true,
  \+ \+ (
   user:maplist(name_vars,NamedVarsList),
   user:name_vars('OUT'=X),
    % add_history_src(exec(BaseEval)),
      if_t(Skipping==1,writeln(' ; SKIPPING')),
     %if_t(TermV\=BaseEval,color_g_mesg('#fa90f6', (write('; '), with_indents(false,write_src(exec(BaseEval)))))),
      if_t((is_interactive(From);Skipping==1),
          (
            if_t( \+ option_value(doing_repl,true),
              if_t( \+ option_value(repl,true),
                if_t(   option_value(prolog,true), add_history_pl(PL)))),
            if_t(option_value(repl,true), add_history_src(exec(BaseEval))))),

      prolog_only((color_g_mesg('#da70d6', (write('% DEBUG:   '), writeq(PL),writeln('.'))))),
      true))))),
   
   (forall_interactive(
    From, WasInteractive,Complete, %may_rtrace
     (timed_call(GG,Seconds)),
     ((Complete==true->!;true),
       %repeat,
       set_option_value(interactive,WasInteractive),
       Control = contrl(Max,DoLeap),
       nb_setarg(1,Result,Output),
       current_input(CI),
       read_pending_codes(CI,_,[]),
       flag(result_num,R,R+1),
       flag(result_num,ResNum,ResNum),
	   reset_eval_num,
     if_t(ResNum=<Max,
         ((((ResNum==1,Complete==true)->(not_compatio(format('~NDeterministic: ',  [])), !);          %or Nondet
           ( Complete==true -> (not_compatio(format('~NLast Result(~w): ',[ResNum])),! );
                               not_compatio(format('~NNDet Result(~w): ',[ResNum]))))),
      ignore(((		 
            not_compatio(if_t( \+ symbolic(Output), nl)), 
			if_t(ResNum==1,only_compatio(format('~N['))),
			only_compatio(if_t((Prev\=@=prev_result('Empty')),write(', '))),
			nb_setarg(1,Prev,Output),
			user_io(with_indents(is_mettalog,
             color_g_mesg_ok(yellow,
              \+ \+ ( maplist(maybe_assign,NamedVarsList),
                write_asrc(Output)))))  ))),

      not_compatio(with_output_to(user_error,give_time('Execution',Seconds))),
      %not_compatio(give_time('Execution',Seconds),
       color_g_mesg(green,
           ignore((NamedVarsList \=@= Was ->(not_compatio(( maplist(print_var,NamedVarsList), nl))) ; true))))),
       (
         (Complete \== true, WasInteractive, DoLeap \== leap, 
                LeashResults > ResNum, ResNum < Max) ->
         (write("press ';' for more solutions "),get_single_char_key(C), 
           not_compatio((writeq(key=C),nl)),
         (C=='b' -> (once(repl),fail) ;
         (C=='m' -> make ;
         (C=='t' -> (nop(set_debug(eval,true)),rtrace) ;
         (C=='T' -> (set_debug(eval,true));
         (C==';' -> true ;
         (C==esc('[A',[27,91,65]) -> nb_setarg(2, Control, leap) ;
         (C=='L' -> nb_setarg(1, Control, ResNum) ;
         (C=='l' -> nb_setarg(2, Control, leap) ;
         (((C=='\n');(C=='\r')) -> (!,fail);
         (!,fail))))))))))));
       (Complete\==true, \+ WasInteractive, Control = contrl(Max,leap)) -> true ;
        (((Complete==true ->! ; true)))))
                    *-> (ignore(Result = res(FOut)),ignore(Output = (FOut)))
                    ; (flag(result_num,ResNum,ResNum),(ResNum==0->(not_compatio(format('~N<no-results>~n~n')),!,true);true))),
                    only_compatio(write(']')),user_io(nl),
   ignore(Result = res(FOut)).

maybe_assign(N=V):- ignore(V='$VAR'(N)).

mqd:-
  forall(metta_atom(_KB,['query-info',E,T,Q]),
     (writeln(E),
      term_variables(T,TVs),
      term_variables(Q,QVs),
      intersection(TVs,QVs,_,_,SVs),
      notrace(eval(['match','&flybase',Q,T],SVs)))).


get_single_char_key(O):- get_single_char(C),get_single_char_key(C,O).
get_single_char_key(27,esc(A,[27|O])):- !,read_pending_codes(user_input,O,[]),name(A,O).
get_single_char_key(C,A):- name(A,[C]).

forall_interactive(file(_),false,Complete,Goal,After):- !,   Goal, (Complete==true ->  ( After,!)  ;  (  \+  After )).
forall_interactive(prolog,false,Complete,Goal,After):- !,  Goal, (Complete == true -> ! ; true), quietly(After).
forall_interactive(From,WasInteractive,Complete,Goal,After):-
   (is_interactive(From) -> WasInteractive = true ; WasInteractive = false),!,
    Goal, (Complete==true ->  ( quietly(After),!)  ;  (  quietly( \+ After) )).



print_var(Name=Var) :- print_var(Name,Var).
write_var(V):- var(V), !, write_dvar(V),!. 
write_var('$VAR'(S)):-  !, write_dvar(S),!. 
write_var(V):- write_dvar(V),!.
%print_var(Name,_Var) :- symbol_concat('Num',Rest,Name),atom_number(Rest,_),!.
print_var(Name,Var):- write_var(Name), write(' = '), write_bsrc(Var), nl.

write_asrc(Var):- Var=='Empty',is_compatio,!.
write_asrc(Var):- write_bsrc(Var),!.

write_bsrc(Var):- Var=='Empty',!,write(Var).
write_bsrc(Var):- ground(Var),!,write_src(Var).
write_bsrc(Var):- copy_term(Var,Copy,Goals),Var=Copy,write_bsrc(Var,Goals).
write_bsrc(Var,[]):- write_src(Var).
write_bsrc(Var,[G|Goals]):- write_src(Var), write(' { '),write_src(G),maplist(write_src_space,Goals),writeln(' } ').

write_src_space(Goal):- write(' '),write_src(Goal).

% Entry point for the user to call with tracing enabled
toplevel_goal(Goal) :-
   term_variables(Goal,Vars),
   interact(Vars, Goal, trace_off).

% Entry point for the user to call with tracing enabled
trace_goal(Goal) :-
    trace_goal(Goal, trace_on).

% Handle tracing
trace_goal(Goal, Tracing) :-
    (Tracing == trace_on -> writeln('Entering goal:'), writeln(Goal) ; true),
    term_variables(Goal, Variables),
    ( call(Goal) ->
        (Tracing == trace_on -> writeln('Goal succeeded with:'), writeln(Variables) ; true),
        interact(Variables, Goal, Tracing)
    ;   (Tracing == trace_on -> writeln('Goal failed.') ; true),
        false
    ).

% Interaction with the user
interact(Variables, Goal, Tracing) :-
    call(Goal),write('Solution: '), write_src(Variables),
    write(' [;next]?'),
    get_single_char(Code),
    (command(Code, Command) ->
        handle_command(Command, Variables, Goal, Tracing)
    ;   writeln('Unknown command.'), interact(Variables, Goal, Tracing) % handle unknown commands
    ).


:- dynamic(is_installed_readline_editline/1).
:- volatile(is_installed_readline_editline/1).
install_readline_editline:- current_input(Input), install_readline(Input),!.

install_readline(Input):- is_installed_readline_editline(Input),!.
install_readline(_):- is_compatio,!.
install_readline(_):-!.
install_readline(Input):-
   assert(is_installed_readline_editline(Input)),   
   install_readline_editline1,
   use_module(library(readline)),
   use_module(library(editline)),
   nop(catch(load_history,_,true)),
    add_history_string("!(pfb3)"),
    add_history_string("!(load-flybase-full)"),
    add_history_string("!(obo-alt-id $X BS:00063)"),
    add_history_string("!(and (total-rows $T TR$) (unique-values $T2 $Col $TR))"),
    nop(ignore(editline:el_wrap)),
    nop(ignore(editline:add_prolog_commands(Input))).


:- dynamic  setup_done/0.
:- volatile setup_done/0.

install_readline_editline1 :-
   setup_done,
   !.
install_readline_editline1 :-
   asserta(setup_done),
  '$toplevel':(
   '$clean_history',
   apple_setup_app,
   '$run_initialization',
   '$load_system_init_file',
   set_toplevel,
   '$set_file_search_paths',
   init_debug_flags,
   start_pldoc,
   opt_attach_packs,
   load_init_file,
   catch(setup_backtrace, E1, print_message(warning, E1)),
   catch(setup_readline,  E2, print_message(warning, E2)),
   catch(setup_history,   E3, print_message(warning, E3)),
   catch(setup_colors, E4, print_message(warning, E4))),
   install_readline(user_input).


% Command descriptions
command(59, retry).    % ';' to retry
command(115, skip).    % 's' to skip to the next solution
command(108, leap).    % 'l' to leap (end the debugging session)
command(103, goals).   % 'g' to show current goals
command(102, fail).    % 'f' to force fail
command(116, trace).   % 't' to toggle tracing
command(117, up).      % 'u' to continue without interruption
command(101, exit).    % 'e' to exit the debugger
command(97, abort).    % 'a' to abort
command(98, break).    % 'b' to set a breakpoint
command(99, creep).    % 'c' to proceed step by step
command(104, help).    % 'h' for help
command(65, alternatives).    % 'A' for alternatives
command(109, make).       % 'm' for make (recompile)
command(67, compile).     % 'C' for Compile (compile new executable)

:- style_check(-singleton).

% Command implementations
handle_command(make, Variables, Goal, Tracing) :-
    writeln('Recompiling...'),
    % Insert the logic to recompile the code.
    % This might involve calling `make/0` or similar.
    make,  % This is assuming your Prolog environment has a `make` predicate.
    fail. % interact(Variables, Goal, Tracing).

handle_command(compile, Variables, Goal, Tracing) :-
    writeln('Compiling new executable...'),
    % Insert the logic to compile a new executable.
    % This will depend on how you compile Prolog programs in your environment.
    % For example, you might use `qsave_program/2` to create an executable.
    % Pseudocode: compile_executable(ExecutableName)
    fail. % interact(Variables, Goal, Tracing).
handle_command(alternatives, Variables, Goal, Tracing) :-
    writeln('Showing alternatives...'),
    % Here you would include the logic for displaying the alternatives.
    % For example, showing other clauses that could be tried for the current goal.
    writeln('Alternatives for current goal:'),
    writeln(Goal),
    % Pseudocode: find_alternatives(Goal, Alternatives)
    % Pseudocode: print_alternatives(Alternatives)
    fail. % interact(Variables, Goal, Tracing).
% Extend the command handling with the 'help' command implementation
handle_command(help, Variables, Goal, Tracing) :-
    print_help,
    fail. % interact(Variables, Goal, Tracing).
handle_command(abort, _, _, _) :-
    writeln('Aborting...'), abort.
handle_command(break, Variables, Goal, Tracing) :-
    writeln('Breakpoint set.'), % Here you should define what 'setting a breakpoint' means in your context
    fail. % interact(Variables, Goal, Tracing).
handle_command(creep, Variables, Goal, Tracing) :-
    writeln('Creeping...'), % Here you should define how to 'creep' (step by step execution) through the code
    trace. % interact(Variables, Goal, Tracing).
handle_command(retry, Variables, Goal, Tracing) :-
    writeln('Continuing...'),!.
    %trace_goal(Goal, Tracing).
handle_command(skip, Variables, Goal, Tracing) :-
    writeln('Skipping...').
handle_command(leap, _, _, _) :-
    writeln('Leaping...'), nontrace. % Cut to ensure we stop the debugger
handle_command(goals, Variables, Goal, Tracing) :-
    writeln('Current goal:'), writeln(Goal),
    writeln('Current variables:'), writeln(Variables),
    bt,fail. % interact(Variables, Goal, Tracing).
handle_command(fail, _, _, _) :-
    writeln('Forcing failure...'), fail.
handle_command(trace, Variables, Goal, Tracing) :-
    (Tracing == trace_on ->
        NewTracing = trace_off, writeln('Tracing disabled.')
    ;   NewTracing = trace_on, writeln('Tracing enabled.')
    ),
    interact(Variables, Goal, NewTracing).
handle_command(up, Variables, Goal, Tracing) :-
    writeln('Continuing up...'),
    repeat,
    ( trace_goal(Goal, Tracing) -> true ; !, fail ).
handle_command(exit, _, _, _) :-
    writeln('Exiting debugger...'), !. % Cut to ensure we exit the debugger

:- style_check(+singleton).


% Help description
print_help :-
    writeln('Debugger commands:'),
    writeln('(;)  next             - Retry with next solution.'),
    writeln('(g)  goal             - Show the current goal.'),
    writeln('(u)  up               - Finish this goal without interruption.'),
    writeln('(s)  skip             - Skip to the next solution.'),
    writeln('(c)  creep or <space> - Proceed step by step.'),
    writeln('(l)  leap             - Leap over (the debugging).'),
    writeln('(f)  fail             - Force the current goal to fail.'),
    writeln('(B)  back             - Go back to the previous step.'),
    writeln('(t)  trace            - Toggle tracing on or off.'),
    writeln('(e)  exit             - Exit the debugger.'),
    writeln('(a)  abort            - Abort the current operation.'),
    writeln('(b)  break            - Break to a new sub-REPL.'),
    writeln('(h)  help             - Display this help message.'),
    writeln('(A)  alternatives     - Show alternative solutions.'),
    writeln('(m)  make             - Recompile/Update the current running code.'),
    writeln('(C)  compile          - Compile a fresh executable (based on the running state).'),
    writeln('(E)  error msg        - Show the latest error messages.'),
    writeln('(r)  retry            - Retry the previous command.'),
    writeln('(I)  info             - Show information about the current state.'),
    !.




really_trace:- once(option_value('exec',rtrace);option_value('eval',rtrace);is_debugging((exec));is_debugging((eval))).
% !(pragma! exec rtrace)
may_rtrace(Goal):- really_trace,!,  really_rtrace(Goal).
may_rtrace(Goal):- Goal*->true;( \+ tracing, trace,really_rtrace(Goal)).
really_rtrace(Goal):- is_transpiling,!,rtrace(call(Goal)).
really_rtrace(Goal):- with_debug((eval),with_debug((exec),Goal)).

rtrace_on_existence_error(G):- !, catch_err(G,E, (fbug(E=G),  \+ tracing, trace, rtrace(G))).
%rtrace_on_existence_error(G):- catch(G,error(existence_error(procedure,W),Where),rtrace(G)).

%prolog_only(Goal):- !,Goal.
prolog_only(Goal):- if_trace(prolog,Goal).

write_compiled_exec(Exec,Goal):-
%  ignore(Res = '$VAR'('ExecRes')),
  compile_for_exec(Res,Exec,Goal),
  notrace((color_g_mesg('#114411',print_pl_source(answer2(Res):-Goal)))).

verbose_unify(Term):- verbose_unify(trace,Term).
verbose_unify(What,Term):- term_variables(Term,Vars),maplist(verbose_unify0(What),Vars),!.
verbose_unify0(What,Var):- put_attr(Var,verbose_unify,What).
verbose_unify:attr_unify_hook(Attr, Value) :-
    format('~N~q~n',[verbose_unify:attr_unify_hook(Attr, Value)]),
    vu(Attr,Value).
vu(_Attr,Value):- is_ftVar(Value),!.
vu(fail,_Value):- !, fail.
vu(true,_Value):- !.
vu(trace,_Value):- trace.
:- nodebug(metta(eval)).
:- nodebug(metta(exec)).
:- nodebug(metta(load)).
:- nodebug(metta(prolog)).
% Measures the execution time of a Prolog goal and displays the duration in seconds,
% milliseconds, or microseconds, depending on the execution time.
%
% Args:
%   - Goal: The Prolog goal to be executed and timed.
%
% The predicate uses the `statistics/2` predicate to measure the CPU time before
% and after executing the provided goal. It calculates the elapsed time in seconds
% and converts it to milliseconds and microseconds. The output is formatted to
% provide clear timing information:
%
% - If the execution takes more than 2 seconds, it displays the time in seconds.
% - If the execution takes between 1 millisecond and 2 seconds, it displays the time
%   in milliseconds.
% - If the execution takes less than 1 millisecond, it displays the time in microseconds.
%
% Example usage:
%   ?- time_eval(my_goal(X)).
%
%   ?- time_eval(sleep(0.95)).
%
% Output examples:
%   ; Evaluation took 2.34 seconds.
%   ; Evaluation took 123.45 ms.
%   ; Evaluation took 0.012 ms. (12.33 microseconds)
%
time_eval(Goal):-
  time_eval('Evaluation',Goal).
time_eval(What,Goal) :-
    timed_call(Goal,Seconds),
    give_time(What,Seconds).

give_time(_What,_Seconds):- is_compatio,!.
give_time(What,Seconds):-
    Milliseconds is Seconds * 1_000,
    (Seconds > 2
        -> format('~N; ~w took ~2f seconds.~n~n', [What, Seconds])
        ; (Milliseconds >= 1
            -> format('~N; ~w took ~3f secs. (~2f milliseconds) ~n~n', [What, Seconds, Milliseconds])
            ;( Micro is Milliseconds * 1_000,
              format('~N; ~w took ~6f secs. (~2f microseconds) ~n~n', [What, Seconds, Micro])))).

timed_call(Goal,Seconds):-
    statistics(cputime, Start),
    call(Goal),
    statistics(cputime, End),
    Seconds is End - Start.

%:- nb_setval(cmt_override,lse('; ',' !(" ',' ") ')).

:- abolish(fbug/1).
fbug(_):- is_compatio,!.
fbug(Info):- notrace(in_cmt(color_g_mesg('#2f2f2f',write_src(Info)))).
example0(_):- fail.
example1(a). example1(_):- fail.
example2(a). example2(b). example2(_):- fail.
example3(a). example3(b). example3(c). example3(_):- fail.
%eval_H(100,'&self',['change-state!','&var',[+,1,['get-state','&var']]],OUT)
%dcall(X):- (call(X),deterministic(YN)),trace,((YN==true)->!;true).
chkdet_call(XX):- !, call(XX).
chkdet_call0(XX):- !, call(XX).

dcall0000000000(XX):-
   USol = sol(dead),
   copy_term(XX,X),
   call_nth(USol,X,Nth,Det,Prev),
   %fbug(call_nth(USol,X,Nth,Det,Prev)),
   XX=Prev,
   (Det==yes -> (!, (XX=Prev;XX=X)) ;
   (((var(Nth) -> ( ! , Prev\==dead) ;
      true),
   (Nth==1 -> ! ; true)))).

call_nth(USol,XX,Nth,Det,Prev):-
  repeat,
   ((call_nth(XX,Nth),deterministic(Det),arg(1,USol,Prev))*->
         ( nb_setarg(1,USol,XX))
         ; (!, arg(1,USol,Prev))).

catch_red(Term):- catch_err(Term,E,pp_m(red,in(Term,E))).
%catch_red(Term):- call(Term).

s2p(I,O):- sexpr_s2p(I,O),!.

discover_head(Self,Load,Head):-
 ignore(([Fn|PredDecl]=Head,
 nop(( arg_types(PredDecl,[],EachArg),
  metta_anew1(Load,metta_head(Self,Fn,EachArg)))))).

discover_body(Self,Load,Body):-
  nop(( [Fn|PredDecl] = Body, arg_types(PredDecl,[],EachArg),
  metta_anew1(Load,metta_body(Self,Fn,EachArg)))).

decl_length(TypeDecL,Len):- is_list(TypeDecL),!,length(TypeDecL,Len).
decl_length(_TypeDecL,1).

arg_types([Ar|L],R,LR):- Ar == '->', !, arg_types(L,R,LR).
arg_types([[Ar|L]],R,LR):- Ar == '->', !, arg_types(L,R,LR).
arg_types(L,R,LR):- append(L,R,LR).

%:- ensure_loaded('../../examples/factorial').
%:- ensure_loaded('../../examples/fibonacci').

%print_preds_to_functs:-preds_to_functs_src(factorial_tail_basic)
ggtrace(G):- call(G).
ggtrace0(G):- ggtrace,
    leash(-all),
  visible(-all),
    % debug, 
  %visible(+redo),
  visible(+call),
  visible(+exception),
    maybe_leash(+exception),
   setup_call_cleanup(trace,G,notrace).
:- dynamic(began_loon/1).
loon:- loon(typein).


catch_red_ignore(G):- catch_red(G)*->true;true.

:- export(loon/1).
:- public(loon/1).


%loon(Why):- began_loon(Why),!,fbugio(begun_loon(Why)).
loon(Why):- is_compiling,!,fbug(compiling_loon(Why)),!.
%loon( _Y):- current_prolog_flag(os_argv,ArgV),member('-s',ArgV),!.
% Why\==toplevel,Why\==default, Why\==program,!
loon(Why):- is_compiled, Why\==toplevel,!,fbugio(compiled_loon(Why)),!.
loon(Why):- began_loon(_),!,fbugio(skip_loon(Why)).
loon(Why):- fbugio(began_loon(Why)), assert(began_loon(Why)),
  do_loon.

do_loon:-
 ignore((
  \+ prolog_load_context(reloading,true),
  maplist(catch_red_ignore,[

   %if_t(is_compiled,ensure_mettalog_py),
          install_readline_editline,
	% nts,
   metta_final,
   nop(load_history),
   set_prolog_flag(history, 3),
   (set_output_stream),
   if_t(is_compiled,update_changed_files),
   run_cmd_args,
   maybe_halt(7)]))),!.


need_interaction:- \+ option_value('had_interaction',true),
   \+ is_converting,  \+ is_compiling, \+ is_pyswip,!,
    option_value('prolog',false), option_value('repl',false),  \+ metta_file(_Self,_Filename,_Directory).

pre_halt1:- is_compiling,!,fail.
pre_halt1:- loonit_report,fail.
pre_halt2:- is_compiling,!,fail.
pre_halt2:-  option_value('prolog',true),!,set_option_value('prolog',started),call_cleanup(prolog,pre_halt2).
pre_halt2:-  option_value('repl',true),!,set_option_value('repl',started),call_cleanup(repl,pre_halt2).
pre_halt2:-  need_interaction, set_option_value('had_interaction',true),call_cleanup(repl,pre_halt2).

%loon:- time(loon_metta('./examples/compat/test_scripts/*.metta')),fail.
%loon:- repl, (option_value('halt',false)->true;halt(7)).
%maybe_halt(Seven):- option_value('prolog',true),!,call_cleanup(prolog,(set_option_value_interp('prolog',false),maybe_halt(Seven))).
%maybe_halt(Seven):- option_value('repl',true),!,call_cleanup(repl,(set_option_value_interp('repl',false),maybe_halt(Seven))).
%maybe_halt(Seven):- option_value('repl',true),!,halt(Seven).
maybe_halt(_):- once(pre_halt1), fail.
maybe_halt(Seven):- option_value('repl',false),!,halt(Seven).
maybe_halt(Seven):- option_value('halt',true),!,halt(Seven).
maybe_halt(_):- once(pre_halt2), fail.
maybe_halt(Seven):- fbugio(maybe_halt(Seven)), fail.
maybe_halt(H):- halt(H). 


:- initialization(nb_setval(cmt_override,lse('; ',' !(" ',' ") ')),restore).


%needs_repl:- \+ is_converting, \+ is_pyswip, \+ is_compiling, \+ has_file_arg.
%  libswipl: ['./','-q',--home=/usr/local/lib/swipl]

:- initialization(show_os_argv).

:- initialization(loon(program),program).
:- initialization(loon(default)).

ensure_mettalog_system_compilable:- 
    %ensure_loaded(library(metta_python)),
    ensure_mettalog_system.
ensure_mettalog_system:-    
    abolish(began_loon/1),
    dynamic(began_loon/1),
    system:use_module(library(quasi_quotations)),
    system:use_module(library(hashtable)),
    system:use_module(library(gensym)),
    system:use_module(library(sort)),
    system:use_module(library(writef)),
    system:use_module(library(rbtrees)),
    system:use_module(library(dicts)),
    system:use_module(library(shell)),
    system:use_module(library(edinburgh)),
  %  system:use_module(library(lists)),
    system:use_module(library(statistics)),
    system:use_module(library(nb_set)),
    system:use_module(library(assoc)),
    system:use_module(library(pairs)),
    user:use_module(library(swi_ide)),
    user:use_module(library(prolog_profile)),
    %metta_python,
    %ensure_loaded('./src/main/flybase_convert'),
    %ensure_loaded('./src/main/flybase_main'),    
    ensure_loaded(library(flybase_convert)),
    ensure_loaded(library(flybase_main)),
    autoload_all,
    make,
    autoload_all,
    %pack_install(predicate_streams, [upgrade(true),global(true)]),
    %pack_install(logicmoo_utils, [upgrade(true),global(true)]),
    %pack_install(dictoo, [upgrade(true),global(true)]),
    !.

file_save_name(E,_):- \+ symbol(E),!,fail.
file_save_name(E,Name):- file_base_name(E,BN),BN\==E,!,file_save_name(BN,Name).
file_save_name(E,E):- symbol_concat('Sav.',_,E),!.
file_save_name(E,E):- symbol_concat('Bin.',_,E),!.
before_underscore(E,N):-symbolic_list_concat([N|_],'_',E),!.
save_name(Name):- current_prolog_flag(os_argv,ArgV),member(E,ArgV),file_save_name(E,Name),!.
next_save_name(Name):- save_name(E),
  before_underscore(E,N),
  symbol_concat(N,'_',Stem),
  gensym(Stem,Name),
  \+ exists_file(Name),
  Name\==E,!.
next_save_name(SavMeTTaLog):- option_value(exeout,SavMeTTaLog),
  symbolic(SavMeTTaLog),atom_length(SavMeTTaLog,Len),Len>1,!.
next_save_name('Sav.MeTTaLog').
qcompile_mettalog:-
    ensure_mettalog_system,    
    option_value(exeout,Named),
    catch_err(qsave_program(Named,
        [class(development),autoload(true),goal(loon(goal)), 
         toplevel(loon(toplevel)), stand_alone(true)]),E,writeln(E)),
    halt(0).
qsave_program:-  ensure_mettalog_system, next_save_name(Name),
    catch_err(qsave_program(Name,
        [class(development),autoload(true),goal(loon(goal)), toplevel(loon(toplevel)), stand_alone(false)]),E,writeln(E)),
    !.


:- ensure_loaded(flybase_main).
:- ensure_loaded(metta_server).
:- initialization(update_changed_files,restore).
:- set_prolog_flag(history, 3).

nts:- !.
nts:-  redefine_system_predicate(system:notrace/1), 
  abolish(system:notrace/1),
  meta_predicate(system:notrace(0)),
  asserta((system:notrace(G):- (!,once(G)))).

override_portray:- 
    forall(
        clause(user:portray(List), Where:Body, Cl),
    (assert(user:portray_prev(List):- Where:Body),
    erase(Cl))),
    asserta((user:portray(List) :- metta_portray(List))).

metta_message_hook(A, B, C) :-
      user:
      (   B==error,
          fbug(metta_message_hook(A, B, C)),
          fail
      ).

override_message_hook:- 
      forall(
          clause(user:message_hook(A,B,C), Where:Body, Cl),
      (assert(user:message_hook(A,B,C):- Where:Body), erase(Cl))),
      asserta((user:message_hook(A,B,C) :- metta_message_hook(A,B,C))).
    
fix_message_hook:-
      clause(message_hook(A, B, C),
        user:
        (   B==error,
            fbug(user:message_hook(A, B, C)),
            fail
        ), Cl),erase(Cl).

:- unnullify_output.  

%:- ensure_loaded(metta_python).


:- ignore(((
   (is_testing -> UNIT_TEST=true; UNIT_TEST=false),
   set_is_unit_test(UNIT_TEST),
   \+ prolog_load_context(reloading,true),
    initialization(loon(restore),restore),
   % nts,
   metta_final
	))).
	
:- set_prolog_flag(metta_interp,ready).

:- use_module(library(clpr)). % Import the CLP(R) library

% Define a predicate to relate the likelihoods of three events
complex_relationship3_ex(Likelihood1, Likelihood2, Likelihood3) :-
    { Likelihood1 = 0.3 * Likelihood2 },
    { Likelihood2 = 0.5 * Likelihood3 },
    { Likelihood3 < 1.0 },
    { Likelihood3 > 0.0 }.

% Example query to find the likelihoods that satisfy the constraints
%?- complex_relationship(L1, L2, L3).
