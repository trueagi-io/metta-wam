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
% Set the encoding for the Prolog system to UTF-8 to ensure proper handling of characters.
% The previously commented out line was for iso_latin_1 encoding.
% UTF-8 is more universal and can handle a wider range of characters.
:- encoding(utf8).

% Set the 'RUST_BACKTRACE' environment variable to 'full'.
% This likely enables detailed error backtraces when using Rust-based components.
% Rust will now output full stack traces when errors occur, which aids in debugging.
:- setenv('RUST_BACKTRACE', full).

% Set the Prolog flag for encoding to UTF-8 (overrides other default encodings).
% This ensures that the Prolog interpreter treats all input/output as UTF-8 encoded.
:- set_prolog_flag(encoding, utf8).

% Set a global non-backtrackable variable 'cmt_override' with a specific string pattern.
% This could be used for customizing the way comments or other formatting behaviors are handled.
:- nb_setval(cmt_override, lse('; ', ' !(" ', ' ") ')).

% Set the flag to make the source search relative to the working directory.
% This helps locate Prolog files from the current working directory.
:- set_prolog_flag(source_search_working_directory, true).

% Enable backtracing, which allows tracking the sequence of goals that led to an error.
% This is useful for debugging as it shows the call stack at the time of an error.
:- set_prolog_flag(backtrace, true).

% Set the maximum depth for the backtrace to 100, meaning up to 100 frames of the call stack will be shown.
:- set_prolog_flag(backtrace_depth, 100).

% Set the maximum goal depth for backtraces, limiting the display of deeply nested goal calls.
:- set_prolog_flag(backtrace_goal_dept, 100).

% Enable showing line numbers in the backtrace, which can help pinpoint where in the source code an error occurred.
:- set_prolog_flag(backtrace_show_lines, true).

% Configure the flag to customize how Prolog writes out attributes (such as variables and terms).
% Using 'portray' ensures that the output is human-readable and properly formatted.
:- set_prolog_flag(write_attributes, portray).

% Enable debugging on errors.
% When an error occurs, this setting will automatically start the Prolog debugger, providing detailed information about the error.
:- set_prolog_flag(debug_on_error, true).

% Load additional Prolog support functions from the 'swi_support' file.
% This could include helper predicates or extensions for SWI-Prolog.
:- ensure_loaded(swi_support).

% Load the Prolog documentation library (pldoc).
% This library provides tools for generating and interacting with Prolog documentation.
:- ensure_loaded(library(pldoc)).

% Ensure that the `user_input` stream is treated as a terminal (TTY).
% This is useful for interactive input, ensuring the system knows it's receiving input from a terminal.
:- set_stream(user_input, tty(true)).

% Set the encoding of the `current_input` stream to UTF-8.
% This ensures that any input read from `current_input` (which is typically `user_input`) is interpreted as UTF-8.
:- set_stream(current_input, encoding(utf8)).

% Treat `current_input` as a terminal (TTY).
% Even though `current_input` is usually linked to `user_input`, this ensures that it's explicitly treated as terminal input.
:- set_stream(current_input, tty(true)).

% Set the encoding for the `user_input` stream to UTF-8.
% This makes sure that all input read from `user_input` is correctly handled as UTF-8 encoded text.
:- set_stream(user_input, encoding(utf8)).

% Treat `user_output` as a terminal (TTY).
% This ensures that output to the terminal behaves properly, recognizing that it's interacting with a terminal (e.g., for handling special characters).
:- set_stream(user_output, tty(true)).

% Set the encoding for the `user_output` stream to UTF-8.
% This ensures that all output sent to `user_output` is encoded in UTF-8, allowing the display of Unicode characters.
:- set_stream(user_output, encoding(utf8)).

% Treat `user_error` as a terminal (TTY).
% This ensures that error messages are handled as terminal output, allowing for proper interaction when the user sees error messages.
:- set_stream(user_error, tty(true)).

% Set the encoding for the `user_error` stream to UTF-8.
% This ensures that error messages and other output sent to `user_error` are encoded in UTF-8, preventing issues with special characters in error messages.
:- set_stream(user_error, encoding(utf8)).

% Flush any pending output to ensure that anything waiting to be written to output is immediately written.
% Useful to make sure output is synchronized and nothing is left in the buffer.
:- flush_output.

%:- set_prolog_flag(debug_on_interrupt,true).
%:- set_prolog_flag(compile_meta_arguments,control).
:- (prolog_load_context(directory, Value);Value='.'), absolute_file_name('../packs/',Dir,[relative_to(Value)]),
    atom_concat(Dir,'predicate_streams',PS),
    atom_concat(Dir,'logicmoo_utils',LU),
    attach_packs(Dir,[duplicate(replace),search(first)]),
    pack_attach(PS,[duplicate(replace),search(first)]),
    pack_attach(LU,[duplicate(replace),search(first)]).
%   :- attach_packs.
%:- ensure_loaded(metta_interp).
is_win64:- current_prolog_flag(windows,_).
is_win64_ui:- is_win64,current_prolog_flag(hwnd,_).

dont_change_streams:- true.

%!  lazy_load_python is det.
%
%   This predicate represents a placeholder or a stub for lazily loading the Python
%   integration. Currently, it does not contain any implementation logic.
%   Presumably, it would attempt to load Python-related resources or interfaces
%   when needed, avoiding unnecessary overhead if Python is not required.
%
%   The implementation should be added to perform the actual lazy loading of
%   the Python environment or integration.
%
:- dynamic(lazy_load_python/0).
lazy_load_python.

:- dynamic(user:is_metta_src_dir/1).
:- prolog_load_context(directory,Dir),
  retractall(user:is_metta_src_dir(_)),
  asserta(user:is_metta_src_dir(Dir)).

metta_root_dir(Dir):- is_metta_src_dir(Value), absolute_file_name('../../',Dir,[relative_to(Value)]).
metta_root_dir(Dir):- getenv('METTA_DIR',Dir),!.

metta_library_dir(Dir):- metta_root_dir(Value), absolute_file_name('./library/',Dir,[relative_to(Value)]).

metta_dir(Dir):- metta_library_dir(Value), absolute_file_name('./genome/',Dir,[relative_to(Value)]).
metta_dir(Dir):- is_metta_src_dir(Dir).
metta_dir(Dir):- metta_library_dir(Dir).
metta_dir(Dir):- metta_root_dir(Dir).
metta_dir(Dir):- is_metta_src_dir(Value), absolute_file_name('../flybase/',Dir,[relative_to(Value)]).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
user:file_search_path(library,Dir):- metta_dir(Dir).
user:file_search_path(mettalog,Dir):- metta_dir(Dir).


:- is_win64 -> ensure_loaded(library(logicmoo_utils)) ; true.

%   :- initialization(attach_packs).
:- nodebug(metta(eval)).
:- nodebug(metta(exec)).
:- nodebug(metta(load)).
:- nodebug(metta(prolog)).

:- dynamic(function_arity/2).
:- dynamic(predicate_arity/2).


:-multifile(user:metta_file/3).
:-dynamic(user:metta_file/3).

:- multifile(reset_cache/0).

    :-multifile(metta_type/3).
    :-dynamic(metta_type/3).

    :-multifile(metta_defn/3).
    :-dynamic(metta_defn/3).


:-multifile(user:asserted_metta_pred/2).
:-dynamic(user:asserted_metta_pred/2).
:-multifile(user:loaded_into_kb/2).
:-dynamic(user:loaded_into_kb/2).
:- dynamic(user:is_metta_dir/1).

once_writeq_nl(_):- \+ clause(pfcTraceExecution,true),!.
once_writeq_nl(P):- nb_current('$once_writeq_ln',W),W=@=P,!.
once_writeq_nl(P):-
 \+ \+ (numbervars(P,444,_,[attvar(skip),singletons(true)]),
 ansi_format([fg(cyan)],'~N~q.~n',[P])),nb_setval('$once_writeq_ln',P),!.
% TODO uncomment this next line but it is breaking the curried chainer
% pfcAdd_Now(P):- pfcAdd(P),!.
pfcAdd_Now(P):- current_predicate(pfcAdd/1),!, once_writeq_nl(pfcAdd(P)),pfcAdd(P).
pfcAdd_Now(P):- once_writeq_nl(asssert(P)),assert(P).
%:- endif.

system:copy_term_g(I,O):- ground(I),!,I=O.
system:copy_term_g(I,O):- copy_term(I,O).

:- ensure_loaded(metta_debug).

is_metta_flag(What):- notrace(is_flag0(What)).

true_flag.
false_flag:- fail.

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
   notrace(catch(set_prolog_flag(What,false),_,true)),
   set_option_value(What,'False'),!,fail.
is_flag0(What,FWhatTrue,_FWhatFalse):-
   current_prolog_flag(os_argv,ArgV),
   member(FWhat,FWhatTrue),member(FWhat,ArgV),!,
   notrace(catch(set_prolog_flag(What,true),_,true)),
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

is_mettalog:- is_win64,!.
is_mettalog:- is_metta_flag('log').

is_synthing_unit_tests:- notrace(is_synthing_unit_tests0).
is_synthing_unit_tests0:- is_testing.
%is_synthing_unit_tests0:- is_html.
% is_synthing_unit_tests0:- is_compatio,!,fail.

is_testing:- is_metta_flag('test').
is_html:- is_metta_flag('html').

:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_loader).


:- nodebug(metta('trace-on-eval')).

is_compatio:- notrace(is_compatio0).
is_compatio0:- is_win64,!,fail.
is_compatio0:- is_testing,!,fail.
is_compatio0:- is_flag0('compatio').
is_compatio0:- is_mettalog,!,fail.
%is_compatio0:- is_html,!,fail.
is_compatio0:- !.

keep_output:- !.
keep_output:- dont_change_streams,!.
keep_output:- is_win64,!.
keep_output:- is_mettalog,!.
keep_output:- is_testing,!.
keep_output:- is_compatio,!,fail.


:- volatile(original_user_output/1).
:- dynamic(original_user_output/1).
original_user_output(X):- stream_property(X,file_no(1)).
original_user_error(X):- stream_property(X,file_no(2)).
:- original_user_output(_)->true;current_output(Out),asserta(original_user_output(Out)).
unnullify_output:- current_output(MFS),  original_user_output(OUT), MFS==OUT, !.
unnullify_output:- original_user_output(MFS), set_prolog_IO(user_input,MFS,user_error).

null_output(MFS):- dont_change_streams,!, original_user_output(MFS),!.
null_output(MFS):- use_module(library(memfile)),
  new_memory_file(MF),open_memory_file(MF,append,MFS).
:- volatile(null_user_output/1).
:- dynamic(null_user_output/1).
:- null_user_output(_)->true;(null_output(MFS),
   asserta(null_user_output(MFS))).


nullify_output:- keep_output,!.
nullify_output:- dont_change_streams,!.
nullify_output:- nullify_output_really.
nullify_output_really:- current_output(MFS), null_user_output(OUT),  MFS==OUT, !.
nullify_output_really:- null_user_output(MFS), set_prolog_IO(user_input,MFS,MFS).

set_output_stream :- dont_change_streams,!.
set_output_stream :- \+ keep_output -> nullify_output;  unnullify_output.
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
%:- use_module(library(editline)).
%:- set_output(user_error).
%:- set_prolog_flag(encoding,octet).



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




%fbugio(TF,P):-!, ignore(( TF,!,wdmsg(fbug(P)))).
%fbugio(_,_):- is_compatio,!.
fbugio(TF,P):-!, ignore(( TF,!,fbug(P))).
fbugio(IO):-fbugio(true,IO).

different_from(N,V):- \+ \+ option_value_def(N,V),!,fail.
different_from(N,V):- \+ \+ nb_current(N,V),!,fail.
different_from(_,_).

set_option_value_interp(N,V):- symbol(N), symbolic_list_concat(List,',',N),List\=[_],!,
  forall(member(E,List),set_option_value_interp(E,V)).
set_option_value_interp(N,V):-
  %(different_from(N,V)->Note=true;Note=false),
  Note = true,
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

'is-symbol'(X):- symbol(X).
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

:- meta_predicate fake_notrace(0).
fake_notrace(G):- tracing,!,real_notrace(G).
fake_notrace(G):- !,notrace(G).
fake_notrace(G):- !,once(G).
% `quietly/1` allows breaking in and inspection (real `no_trace/1` does not)
fake_notrace(G):- quietly(G),!.
:- meta_predicate real_notrace(0).
real_notrace(Goal) :-
    setup_call_cleanup('$notrace'(Flags, SkipLevel),
                       once(Goal),
                       '$restore_trace'(Flags, SkipLevel)).


:- dynamic(is_answer_output_stream/2).
answer_output(Stream):- is_testing,original_user_output(Stream),!.
answer_output(Stream):- !,original_user_output(Stream),!. % yes, the cut is on purpose
answer_output(Stream):- is_answer_output_stream(_,Stream),!.
answer_output(Stream):- tmp_file('answers',File),
   open(File,write,Stream,[encoding(utf8)]),
   asserta(is_answer_output_stream(File,Stream)).

write_answer_output:-
  retract(is_answer_output_stream(File,Stream)),!,
  ignore(catch_log(close(Stream))),
  sformat(S,'cat ~w',[File]),
  catch_log(ignore(shell(S))),nl.
write_answer_output.


null_io(G):- null_user_output(Out), !, with_output_to(Out,G).
user_io(G):- original_user_output(Out), ttyflush, !, with_output_to(Out,G), flush_output(Out), ttyflush.
user_err(G):- original_user_error(Out), !, with_output_to(Out,G).
with_output_to_s(Out,G):- current_output(COut),
  redo_call_cleanup(set_prolog_IO(user_input, Out,user_error), G,
                     set_prolog_IO(user_input,COut,user_error)).

 in_answer_io(_):- nb_current(suspend_answers,true),!.
 in_answer_io(G):- answer_output(Out), !, with_output_to(Out,G).
 not_compatio(G):- if_t(once(is_mettalog;is_testing),user_err(G)).

%if_compatio(G):- if_t(is_compatio,user_io(G)).
% if_compat_io(G):- if_compatio(G).
not_compat_io(G):- not_compatio(G).
non_compat_io(G):- not_compatio(G).


trace_on_fail:-     option_value('trace-on-fail',true).
trace_on_overflow:- option_value('trace-on-overflow',true).
trace_on_pass:-     option_value('trace-on-pass',true).
doing_repl:-     option_value('doing_repl',true).
if_repl(Goal):- doing_repl->call(Goal);true.

any_floats(S):- member(E,S),float(E),!.

show_options_values:-
   forall((nb_current(N,V), \+((symbol(N),symbol_concat('$',_,N)))),write_src_nl(['pragma!',N,V])).

:- prolog_load_context(source,File), assert(interpreter_source_file(File)).


:- ensure_loaded(metta_utils).
%:- ensure_loaded(mettalog('metta_ontology.pfc.pl')).
:- ensure_loaded(metta_pfc_base).
:- ensure_loaded(metta_pfc_support).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_eval).

:- set_is_unit_test(false).
extract_prolog_arity([Arrow|ParamTypes],PrologArity):-
    Arrow == ('->'),!,
    len_or_unbound(ParamTypes,PrologArity).

add_prolog_code(_KB,AssertZIfNew):-
  fbug(writeln(AssertZIfNew)),
  assertz_if_new(AssertZIfNew).
gen_interp_stubs(KB,Symb,Def):-
  ignore((is_list(Def),
 must_det_ll((
     extract_prolog_arity(Def,PrologArity),
       symbol(Symb),
       symbol_concat('i_',Symb,Tramp),
       length(PrologArgs,PrologArity),
       append(MeTTaArgs,[RetVal],PrologArgs),
       TrampH =.. [Tramp|PrologArgs],
       add_prolog_code(KB,
           (TrampH :- eval_H([Symb|MeTTaArgs], RetVal))))))).

% 'int_fa_format-args'(FormatArgs, Result):- eval_H(['format-args'|FormatArgs], Result).
% 'ext_fa_format-args'([EFormat, EArgs], Result):- int_format-args'(EFormat, EArgs, Result)
/*

'ext_format-args'(Shared,Format, Args, EResult):-
    pred_in('format-args',Shared,3),
      argn_in(1,Shared,Format,EFormat),
      argn_in(2,Shared,Args,EArgs),
      argn_in(3,Shared,EResult,Result),
    int_format-args'(Shared,EFormat, EArgs, Result),
      arg_out(1,Shared,EFormat,Format),
      arg_out(2,Shared,EArgs,Args),
      arg_out(3,Shared,Result,EResult).

 you are goign to create the clause based on the first 2 args

?-  gen_form_body('format-args',3, HrnClause).

HrnClause =
   ('ext_format-args'(Shared, Arg1, Arg2, EResult):-
    pred_in('format-args',Shared,3),
      argn_in(1,Shared,Arg1,EArg1),
      argn_in(2,Shared,Arg2,EArg2),
      argn_in(3,Shared,EResult,Result),
    'int_format-args'(Shared,EArg1, EArg2, Result),
      arg_out(1,Shared,EArg1,Arg1),
      arg_out(2,Shared,EArg2,Arg2),
      arg_out(3,Shared,Result,EResult)).

*/



% Helper to generate head of the clause
generate_head(Shared,Arity, FormName, Args, Head) :-
    atom_concat('ext_', FormName, ExtFormName),
    number_string(Arity, ArityStr),
    atom_concat(ExtFormName, ArityStr, FinalFormName), % Append arity to form name for uniqueness
    append([FinalFormName, Shared | Args], HeadArgs),
    Head =.. HeadArgs.

% Helper to generate body of the clause, swapping arguments
generate_body(Shared,Arity, FormName, Args, EArgs, Body) :-
    atom_concat('int_', FormName, IntFormName),
    number_string(Arity, ArityStr),
    atom_concat(IntFormName, ArityStr, FinalIntFormName), % Append arity to internal form name for uniqueness
    reverse(EArgs, ReversedEArgs), % Reverse the order of evaluated arguments for internal processing
    % Generate predicates for input handling
    findall(argn_in(Index, Shared, Arg, EArg),
            (nth1(Index, Args, Arg), nth1(Index, EArgs, EArg)), ArgIns),
    % Internal processing call with reversed arguments
    append([Shared | ReversedEArgs], IntArgs),
    InternalCall =.. [FinalIntFormName | IntArgs],
    % Generate predicates for output handling
    findall(arg_out(Index, Shared, EArg, Arg),
            (nth1(Index, EArgs, EArg), nth1(Index, Args, Arg)), ArgOuts),
    % Combine predicates
    PredIn = pred_in(FormName, Shared, Arity),
    append([PredIn | ArgIns], [InternalCall | ArgOuts], BodyParts),
    list_to_conjunction(BodyParts, Body).

% Main predicate to generate form body clause
gen_form_body(FormName, Arity, Clause) :-
    length(Args,Arity),
    length(EArgs,Arity),
    generate_head(Shared,Arity, FormName, Args, Head),
    generate_body(Shared,Arity, FormName, Args, EArgs, Body),
    Clause = (Head :- Body).


% Helper to format atoms
format_atom(Format, N, Atom) :- format(atom(Atom), Format, [N]).


% 'int_format-args'(Shared,Format, Args, Result):-
%    .... actual impl ....


% ============================
% %%%% Missing Arithmetic Operations
% ============================
'%'(Dividend, Divisor, Remainder):- eval_H(['mod',Dividend, Divisor], Remainder).


mettalog_rt_args(Args):- current_prolog_flag(mettalog_rt_args, Args),!.
mettalog_rt_args(['--repl=false']).

metta_argv(Args):- current_prolog_flag(metta_argv, Args),!.
metta_argv(Args):- current_prolog_flag(mettalog_rt, true),!,mettalog_rt_args(Args).
metta_argv(Before):- current_prolog_flag(os_argv,OSArgv), append(_,['--args'|AArgs],OSArgv),
    before_arfer_dash_dash(AArgs,Before,_),!,set_metta_argv(Before).
argv_metta(Nth,Value):- metta_argv(Args),nth1(Nth,Args,Value).

set_metta_argv(Before):-  maplist(read_argv,Before,Args),set_prolog_flag(metta_argv, Args),!.
read_argv(AArg,Arg):- \+ symbol(AArg),!,AArg=Arg.
read_argv(AArg,Arg):- atom_string(AArg,S),read_metta(S,Arg),!.


metta_cmd_args(Args):- current_prolog_flag(mettalog_rt, true),!,mettalog_rt_args(Args).
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
process_option_value_def:- fail, \+ option_value('python',false), ensure_loaded(mettalog(metta_python)),
  real_notrace((ensure_mettalog_py)).
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

:- if( \+ current_predicate(load_metta_file/2)).
load_metta_file(Self,Filemask):- symbol_concat(_,'.metta',Filemask),!, load_metta(Self,Filemask).
load_metta_file(_Slf,Filemask):- load_flybase(Filemask).
:- endif.

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


install_ontology:- ensure_corelib_types.
load_ontology:- option_value(compile,false),!.
load_ontology.

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
                    set_stream(X, encoding(utf8)),
  set_stream(current_output,tty(TF)),
  set_stream(current_output, encoding(utf8)),
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
  %retractall(metta_eq_def(Eq,S,_,_)),
  nop(retractall(metta_type(S,_,_))),
  %retractall(get_metta_atom(Eq,S,_,_,_)),
  loonit_reset,
  tee_file(TEE_FILE),
  sformat(S,'cat /dev/null > "~w"',[TEE_FILE]),

  writeln(doing(S)),
  ignore(shell(S)))).

save_html_of(_Filename):- \+ tee_file(_TEE_FILE),!.
save_html_of(_):- \+ has_loonit_results, \+ option_value('html',true).
save_html_of(_):- loonit_report, !, writeln('<br/> <a href="#" onclick="window.history.back(); return false;">Return to summaries</a><br/>').
save_html_of(_Filename):-!.
save_html_of(Filename):-
 must_det_ll((
  file_name_extension(Base,_,Filename),
  file_name_extension(Base,'metta.html',HtmlFilename),
  loonit_reset,
  tee_file(TEE_FILE),
  writeln('<br/> <a href="#" onclick="window.history.back(); return false;">Return to summaries</a><br/>'),
  sformat(S,'ansi2html -u < "~w" > "~w" ',[TEE_FILE,HtmlFilename]),
  writeln(doing(S)),
  ignore(shell(S)))).

tee_file(TEE_FILE):- getenv('TEE_FILE',TEE_FILE),!.
tee_file(TEE_FILE):- metta_dir(Dir),directory_file_path(Dir,'TEE.ansi',TEE_FILE),!.


clear_spaces:- clear_space(_).
clear_space(S):-
   retractall(user:loaded_into_kb(S,_)),
   %retractall(metta_eq_def(_,S,_,_)),
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
  \+ metta_eq_def(Eq,Other,[H|_],_).
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
  %copy_term_g(Term,CTerm),
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





assert_preds(Self,Load,List):- is_list(List),!,maplist(assert_preds(Self,Load),List).
%assert_preds(_Self,_Load,_Preds):- \+ show_transpiler,!.
assert_preds(_Self,Load,Preds):-
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
   %add_assertion(Self,Preds)
   true)),
   nop(metta_anew1(Load,Preds)).


%load_hook(_Load,_Hooked):- !.
load_hook(Load,Hooked):-
   ignore(( \+ ((forall(load_hook0(Load,Hooked),true))))),!.


%rtrace_on_error(G):- catch(G,_,fail).
rtrace_on_error(G):-
  catch_err(G,E,
   (%notrace,
    write_src_uo(E=G),
    %catch(rtrace(G),E,throw(E)),
    catch(rtrace(G),E,throw(give_up(E=G))),
    throw(E))).

rtrace_on_failure(G):- tracing,!,call(G).
rtrace_on_failure(G):-
  catch_err((G*->true;(write_src_uo(rtrace_on_failure(G)),
                       ignore(rtrace(G)),
                       write_src_uo(rtrace_on_failure(G)),
                       !,fail)),E,
   (%notrace,
    write_src_uo(E=G),
    %catch(rtrace(G),E,throw(E)),
    catch(rtrace(G),E,throw(give_up(E=G))),
    throw(E))).

rtrace_on_failure_and_break(G):- tracing,!,call(G).
rtrace_on_failure_and_break(G):-
  catch_err((G*->true;(write_src(rtrace_on_failure(G)),
                       ignore(rtrace(G)),
                       write_src(rtrace_on_failure(G)),
                       !,break,fail)),E,
   (%notrace,
    write_src_uo(E=G),
    %catch(rtrace(G),E,throw(E)),
    catch(rtrace(G),E,throw(give_up(E=G))),
    throw(E))).

assertion_hb(metta_eq_def(Eq,Self,H,B),Self,Eq,H,B):-!.
assertion_hb(metta_defn(Self,H,B),Self,'=',H,B):-!.
assertion_hb(metta_atom_asserted(KB,HB),Self,Eq,H,B):- !, assertion_hb(metta_atom(KB,HB),Self,Eq,H,B).
assertion_hb(metta_atom(Self,[Eq,H,B]),Self,Eq,H,B):- assert_type_cl(Eq),!.
assertion_hb(metta_atom(Self,[Eq,H|B]),Self,Eq,H,B):- assert_type_cl(Eq),!.

assert_type_cl(Eq):- \+ symbol(Eq),!,fail.
assert_type_cl('=').
assert_type_cl(':-').


load_hook0(_,_):- \+ show_transpiler, \+ is_transpiling, !.
load_hook0(Load,Assertion):- fail,
       assertion_hb(Assertion,Self,H,B),
       functs_to_preds([=,H,B],Preds),
       assert_preds(Self,Load,Preds).
% old compiler hook       
load_hook0(Load,Assertion):-
     assertion_hb(Assertion,Self, Eq, H,B),
     rtrace_on_error(compile_for_assert_eq(Eq, H, B, Preds)),!,
     rtrace_on_error(assert_preds(Self,Load,Preds)).
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

:- dynamic(metta_atom_asserted/2).
:- multifile(metta_atom_asserted/2).
:- dynamic(metta_atom_asserted_deduced/2).
:- multifile(metta_atom_asserted_deduced/2).
metta_atom_asserted(X,Y):-
    metta_atom_asserted_deduced(X,Y),
    \+ clause(metta_atom_asserted(X,Y),true).


%get_metta_atom(Eq,KB, [F|List]):- KB='&flybase',fb_pred(F, Len), length(List,Len),apply(F,List).


get_metta_atom_from(KB,Atom):- metta_atom(KB,Atom).

get_metta_atom(Eq,Space, Atom):- metta_atom(Space, Atom), \+ (Atom =[EQ,_,_], EQ==Eq).

metta_atom(Atom):- current_self(KB),metta_atom(KB,Atom).
%metta_atom([Superpose,ListOf], Atom):- Superpose == 'superpose',is_list(ListOf),!,member(KB,ListOf),get_metta_atom_from(KB,Atom).
metta_atom(Space, Atom):- typed_list(Space,_,L),!, member(Atom,L).
metta_atom(KB, [F, A| List]):- KB=='&flybase',fb_pred_nr(F, Len),current_predicate(F/Len), length([A|List],Len),apply(F,[A|List]).
%metta_atom(KB,Atom):- KB=='&corelib',!, metta_atom_corelib(Atom).
metta_atom(KB,Atom):- metta_atom_in_file( KB,Atom).
metta_atom(KB,Atom):- metta_atom_asserted( KB,Atom).

%metta_atom(KB,Atom):- KB == '&corelib', !, metta_atom_asserted('&self',Atom).
metta_atom(KB,Atom):- KB \== '&corelib', using_all_spaces,!, metta_atom('&corelib',Atom).
metta_atom(KB,Atom):- KB \== '&corelib', !,
   \+ \+ (metta_atom_asserted(KB,'&corelib'),
          should_inherit_from_corelib(Atom)), !,
   metta_atom('&corelib',Atom).
should_inherit_from_corelib(_):- using_all_spaces,!.
should_inherit_from_corelib([H,A|_]):- H == ':',!,nonvar(A).
should_inherit_from_corelib([H|_]):- H == '@doc', !.
should_inherit_from_corelib([H,A|T]):- fail,
  H == '=',wdmsg(try([H,A|T])),!,is_list(A),
  A=[F|_],nonvar(F), F \==':',
  \+ metta_atom_asserted('&self',[:,F|_]),
  % \+ \+ metta_atom_asserted('&corelib',[=,[F|_]|_]),
  wdmsg([H,A|T]).

/*
should_inherit_op_from_corelib('=').
should_inherit_op_from_corelib(':').
should_inherit_op_from_corelib('@doc').
%should_inherit_op_from_corelib(_).
*/
metta_atom_asserted('&self','&corelib').
metta_atom_asserted('&self','&stdlib').
metta_atom_asserted('&stdlib','&corelib').
metta_atom_asserted('&flybase','&corelib').
metta_atom_asserted('&catalog','&corelib').
metta_atom_asserted('&catalog','&stdlib').

/*
'mod-space'(top,'&self').
'mod-space'(catalog,'&catalog').
'mod-space'(corelib,'&corelib').
'mod-space'(stdlib,'&stdlib').
'mod-space'(Top,'&self'):- Top == self.
*/
not_metta_atom_corelib(A,N):-  A \== '&corelib' , metta_atom('&corelib',N).

%metta_atom_asserted_fallback( KB,Atom):- metta_atom_stdlib(KB,Atom)


%metta_atom(KB,[F,A|List]):- metta_atom(KB,F,A,List), F \== '=',!.
is_metta_space(Space):- \+ \+ is_space_type(Space,_Test).

%metta_eq_def(Eq,KB,H,B):- ignore(Eq = '='),if_or_else(metta_atom(KB,[Eq,H,B]), metta_atom_corelib(KB,[Eq,H,B])).
metta_eq_def(Eq,KB,H,B):-  ignore(Eq = '='),metta_atom(KB,[Eq,H,B]).
%metta_eq_def(Eq,KB,H,B):-  ignore(Eq = '='), if_or_else(metta_atom(KB,[Eq,H,B]),not_metta_atom_corelib(KB,[Eq,H,B])).

%metta_defn(KB,Head,Body):- metta_eq_def(_Eq,KB,Head,Body).
%metta_defn(KB,H,B):- if_or_else(metta_atom(KB,['=',H,B]),not_metta_atom_corelib(KB,['=',H,B])).
metta_defn(KB,H,B):- metta_eq_def('=',KB,H,B).
%metta_type(KB,H,B):- if_or_else(metta_atom(KB,[':',H,B]),not_metta_atom_corelib(KB,[':',H,B])).
metta_type(KB,H,B):- metta_eq_def(':',KB,H,B).
%metta_type(S,H,B):- S == '&corelib', metta_atom_stdlib_types([':',H,B]).
%typed_list(Cmpd,Type,List):-  compound(Cmpd), Cmpd\=[_|_], compound_name_arguments(Cmpd,Type,[List|_]),is_list(List).

%metta_atom_corelib(KB,Atom):- KB\='&corelib',!,metta_atom('&corelib',Atom).

%maybe_xform(metta_atom(KB,[F,A|List]),metta_atom(KB,F,A,List)):- is_list(List),!.
maybe_xform(metta_eq_def(Eq,KB,Head,Body),metta_atom(KB,[Eq,Head,Body])).
maybe_xform(metta_defn(KB,Head,Body),metta_atom(KB,['=',Head,Body])).
maybe_xform(metta_type(KB,Head,Body),metta_atom(KB,[':',Head,Body])).
maybe_xform(metta_atom(KB,HeadBody),metta_atom_asserted(KB,HeadBody)).
maybe_xform(_OBO,_XForm):- !, fail.

metta_anew1(Load,_OBO):- var(Load),trace,!.
metta_anew1(Ch,OBO):-  metta_interp_mode(Ch,Mode), !, metta_anew1(Mode,OBO).
metta_anew1(Load,OBO):- maybe_xform(OBO,XForm),!,metta_anew1(Load,XForm).
metta_anew1(load,OBO):- OBO= metta_atom(Space,Atom),!,'add-atom'(Space, Atom).
metta_anew1(unload,OBO):- OBO= metta_atom(Space,Atom),!,'remove-atom'(Space, Atom).
metta_anew1(unload_all,OBO):- OBO= forall(metta_atom(Space,Atom),ignore('remove-atom'(Space, Atom))).

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
  ignore((clause(Head,Body,Ref),clause(Head2,Body2,Ref),
    (Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(unload(Cl)))).
metta_anew1(unload_all,OBO):- subst_vars(OBO,Cl),load_hook(unload_all,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  forall(
    (clause(Head,Body,Ref),clause(Head2,Body2,Ref)),
    must_det_ll((((Head+Body)=@=(Head2+Body2))
               ->(erase(Ref),nop(pp_m(unload_all(Ref,Cl))))
               ;(pp_m(unload_all_diff(Cl,(Head+Body)\=@=(Head2+Body2))))))).


/*
metta_anew2(Load,_OBO):- var(Load),trace,!.
metta_anew2(Load,OBO):- maybe_xform(OBO,XForm),!,metta_anew2(Load,XForm).
metta_anew2(Ch,OBO):-  metta_interp_mode(Ch,Mode), !, metta_anew2(Mode,OBO).
metta_anew2(load,OBO):- must_det_ll((load_hook(load,OBO),subst_vars_not_last(OBO,Cl),assertz_if_new(Cl))). %to_metta(Cl).
metta_anew2(unload,OBO):- subst_vars_not_last(OBO,Cl),load_hook(unload,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  ignore((clause(Head,Body,Ref),clause(Head2,Body2,Ref),(Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(Cl))).
metta_anew2(unload_all,OBO):- subst_vars_not_last(OBO,Cl),load_hook(unload_all,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  forall((clause(Head,Body,Ref),clause(Head2,Body2,Ref),(Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(Cl)),true).
*/

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

write_exec(Exec):- real_notrace(write_exec0(Exec)).
%write_exec0(Exec):- symbol(Exec),!,write_exec0([Exec]).

write_exec0(Exec):-
  wots(S,write_src(exec(Exec))),
  nb_setval(exec_src,Exec),
  format('~N'),
  ignore((notrace((color_g_mesg('#0D6328',writeln(S)))))).

%!(let* (( ($a $b) (collapse (get-atoms &self)))) ((bind! &stdlib $a) (bind! &corelib $b)))

asserted_do_metta(Space,Ch,Src):- metta_interp_mode(Ch,Mode), !, asserted_do_metta(Space,Mode,Src).

asserted_do_metta(Space,Load,Src):- Load==exec,!,do_metta_exec(python,Space,Src,_Out).
asserted_do_metta(Space,Load,Src):- asserted_do_metta2(Space,Load,Src,Src).

asserted_do_metta2(Space,Ch,Info,Src):- nonvar(Ch), metta_interp_mode(Ch,Mode), !, asserted_do_metta2(Space,Mode,Info,Src).
/*
asserted_do_metta2(Self,Load,[TypeOp,Fn,Type], Src):- TypeOp == ':',  \+ is_list(Type),!,
 must_det_ll((
  color_g_mesg_ok('#ffa501',metta_anew(Load,Src,metta_atom(Self,[':',Fn,Type]))))),!.

asserted_do_metta2(Self,Load,[TypeOp,Fn,TypeDecL], Src):- TypeOp == ':',!,
 must_det_ll((
  decl_length(TypeDecL,Len),LenM1 is Len - 1, last_element(TypeDecL,LE),
  color_g_mesg_ok('#ffa502',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecL]))),
  metta_anew1(Load,metta_arity(Self,Fn,LenM1)),
  arg_types(TypeDecL,[],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),!,
  metta_anew1(Load,metta_last(Self,Fn,LE)))).
*/
/*
asserted_do_metta2(Self,Load,[TypeOp,Fn,TypeDecL,RetType], Src):- TypeOp == ':',!,
 must_det_ll((
  decl_length(TypeDecL,Len),
  append(TypeDecL,[RetType],TypeDecLRet),
  color_g_mesg_ok('#ffa503',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecLRet]))),
  metta_anew1(Load,metta_arity(Self,Fn,Len)),
  arg_types(TypeDecL,[RetType],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),
  metta_anew1(Load,metta_return(Self,Fn,RetType)))),!.
*/
/*do_metta(File,Self,Load,PredDecl, Src):-fail,
   metta_anew(Load,Src,metta_atom(Self,PredDecl)),
   ignore((PredDecl=['=',Head,Body], metta_anew(Load,Src,metta_eq_def(Eq,Self,Head,Body)))),
   ignore((Body == 'True',!,do_metta(File,Self,Load,Head))),
   nop((fn_append(Head,X,Head), fn_append(PredDecl,X,Body),
   metta_anew((Head:- Body)))),!.*/
/*
asserted_do_metta2(Self,Load,[EQ,Head,Result], Src):- EQ=='=', !,
 color_g_mesg_ok('#ffa504',must_det_ll((
    discover_head(Self,Load,Head),
    metta_anew(Load,Src,metta_eq_def(EQ,Self,Head,Result)),
    discover_body(Self,Load,Result)))).
*/
asserted_do_metta2(Self,Load,PredDecl, Src):-
   %ignore(discover_head(Self,Load,PredDecl)),
   color_g_mesg_ok('#ffa505',metta_anew(Load,Src,metta_atom(Self,PredDecl))).

never_compile(X):- always_exec(X).

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

check_answers_for(_,_):- nb_current(suspend_answers,true),!,fail.
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

make_empty(Empty):- 'Empty'=Empty.
make_empty(_,Empty):- make_empty(Empty).
make_empty(_RetType,_,Empty):- make_empty(Empty).


make_nop(Nop):- []=Nop.
make_nop(_,Nop):- make_nop(Nop).
make_nop(_RetType,_,Nop):- make_nop(Nop).


convert_tax(_How,Self,Tax,Expr,NewHow):-
  metta_interp_mode(Ch,Mode),
  string_concat(Ch,TaxM,Tax),!,
  normalize_space(string(NewTax),TaxM),
  convert_tax(Mode,Self,NewTax,Expr,NewHow).
convert_tax(How,_Self,Tax,Expr,How):-
  %parse_sexpr_metta(Tax,Expr).
  normalize_space(string(NewTax),Tax),
  parse_sexpr_metta1(NewTax,Expr).

%:- if( \+ current_predicate(notrace/1) ).
%  notrace(G):- once(G).
%:- endif.

metta_interp_mode('+',load).
metta_interp_mode('-',unload).
metta_interp_mode('--',unload_all).
metta_interp_mode('!',exec).
metta_interp_mode('?',call).
metta_interp_mode('^',load_like_file).


call_sexpr(How,Self,Tax,_S,Out):-
  (symbol(Tax);string(Tax)),
    normalize_space(string(TaxM),Tax),
    convert_tax(How,Self,TaxM,Expr,NewHow),!,
    show_call(do_metta(python,NewHow,Self,Expr,Out)).

/*
do_metta(File,Load,Self,Cmt,Out):-
  fail,
  if_trace(do_metta, fbug(do_metta(File,Load,Self,Cmt,Out))),fail.
*/

do_metta(_File,_Load,_Self,In,Out):- var(In),!,In=Out.
do_metta(_From,_Mode,_Self,end_of_file,'Empty'):- !. %, halt(7), writeln('\n\n% To restart, use: ?- repl.').
do_metta(_File,Load,_Self,Cmt,Out):- Load \==exec, Cmt==[],!, ignore(Out=[]).

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
   if_t(into_simple_op(call,TermV,OP),pfcAdd_Now('next-operation'(OP))),
   call_for_term_variables(TermV,Term,NamedVarsList,X), must_be(nonvar,Term),
   copy_term(NamedVarsList,Was),
   Output = NamedVarsList,
   user:interactively_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut).

do_metta(_File,Load,Self,Src,Out):- Load\==exec, !,
   if_t(into_simple_op(Load,Src,OP),pfcAdd_Now('next-operation'(OP))),
   dont_give_up(as_tf(asserted_do_metta(Self,Load,Src),Out)).

do_metta(file(Filename),exec,Self,TermV,Out):-
   must_det_ll((inc_exec_num(Filename),
     get_exec_num(Filename,Nth),
     Nth>0)),
    ((
     is_synthing_unit_tests,
     file_answers(Filename, Nth, Ans),
     check_answers_for(TermV,Ans))),!,
     if_t(into_simple_op(exec,TermV,OP),pfcAdd_Now('next-operation'(OP))),
     must_det_ll((
      ensure_increments((color_g_mesg_ok('#ffa509',
       (writeln(';; In file as:  '),
        color_g_mesg([bold,fg('#FFEE58')], write_src(exec(TermV))),
        write(';; To unit test case:'))),!,
        call(do_metta_exec(file(Filename),Self,['assertEqualToResult',TermV,Ans],Out)))))).

do_metta(From,exec,Self,TermV,Out):- !,
    if_t(into_simple_op(exec,TermV,OP),pfcAdd_Now('next-operation'(OP))),
    dont_give_up(do_metta_exec(From,Self,TermV,Out)).

do_metta_exec(From,Self,TermV,FOut):-
  Output = X,
 ignore(catch(((not_compatio(write_exec(TermV)),
   notrace(into_metta_callable(Self,TermV,Term,X,NamedVarsList,Was)),!,
   user:interactively_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut))),
   give_up(Why),pp_m(red,gave_up(Why)))),!.


o_s(['assertEqual'|O],S):- nonvar(O), o_s(O,S).
o_s(['assertEqualToResult'|O],S):- nonvar(O), o_s(O,S).
o_s([O|_],S):- nonvar(O), !, o_s(O,S).
o_s(S,S).
into_simple_op(Load,[Op|O],op(Load,Op,S)):- o_s(O,S),!.

call_for_term_variables(TermV,catch_red(show_failure(Term)),NamedVarsList,X):-
 term_variables(TermV, AllVars), call_for_term_variables4v(TermV,AllVars,Term,NamedVarsList,X),!,
 must_be(callable,Term).
call_for_term_variables(TermV,catch_red(show_failure(Term)),NamedVarsList,X):-
  get_term_variables(TermV, DCAllVars, Singletons, NonSingletons),
  call_for_term_variables5(TermV, DCAllVars, Singletons, NonSingletons, Term,NamedVarsList,X),!,
  must_be(callable,Term).

into_metta_callable(_Self,TermV,Term,X,NamedVarsList,Was):- 
 \+ never_compile(TermV),
 is_transpiling, !,
 must_det_ll((((

 % ignore(Res = '$VAR'('ExecRes')),
  RealRes = Res,
  compile_for_exec(Res,TermV,ExecGoal),!,
  subst_vars(Res+ExecGoal,Res+Term,NamedVarsList),
  copy_term_g(NamedVarsList,Was),
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
  copy_term_g(NamedVarsList,Was)
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
eval_H(Term,X):- catch_metta_return(eval_args(Term,X),X).
eval_H(StackMax,Self,Term,X):-  catch_metta_return(eval_args('=',_,StackMax,Self,Term,X),X).
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


catch_err(G,E,C):- catch(G,E,(always_rethrow(E)->(throw(E));C)).
dont_give_up(G):- catch(G,give_up(E),write_src_uo(dont_give_up(E))).

not_in_eq(List, Element) :-
    member(V, List), V == Element.

:- ensure_loaded(metta_repl).


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
time_eval(Goal):- time_eval('Evaluation',Goal).
time_eval(What,Goal) :-
    timed_call(Goal,Seconds),
    give_time(What,Seconds).

ctime_eval(Goal):- ctime_eval('Evaluation',Goal).
ctime_eval(What,Goal) :-
    ctimed_call(Goal,Seconds),
    give_time(What,Seconds).

wtime_eval(Goal):- wtime_eval('Evaluation',Goal).
wtime_eval(What,Goal) :-
    wtimed_call(Goal,Seconds),
    give_time(What,Seconds).

%give_time(_What,_Seconds):- is_compatio,!.
give_time(What,Seconds):-
    Milliseconds is Seconds * 1_000,
    (Seconds > 2
        -> format('~N; ~w took ~2f seconds.~n~n', [What, Seconds])
        ; (Milliseconds >= 1
            -> format('~N; ~w took ~3f secs. (~2f milliseconds) ~n~n', [What, Seconds, Milliseconds])
            ;( Micro is Milliseconds * 1_000,
              format('~N; ~w took ~6f secs. (~2f microseconds) ~n~n', [What, Seconds, Micro])))).

timed_call(Goal,Seconds):- ctimed_call(Goal,Seconds).

ctimed_call(Goal,Seconds):-
    statistics(cputime, Start),
    ( \+ rtrace_this(Goal)->rtrace_on_error(Goal);rtrace(Goal)),
    statistics(cputime, End),
    Seconds is End - Start.

wtimed_call(Goal,Seconds):-
    statistics(walltime, [Start,_]),
    ( \+ rtrace_this(Goal)->rtrace_on_error(Goal);rtrace(Goal)),
    statistics(walltime, [End,_]),
    Seconds is (End - Start)/1000.


rtrace_this(eval_H(_, _, P , _)):- compound(P), !, rtrace_this(P).
rtrace_this([P|_]):- P == 'pragma!',!,fail.
rtrace_this([P|_]):- P == 'import!',!,fail.
rtrace_this([P|_]):- P == 'rtrace!',!.
rtrace_this(_Call):- option_value(rtrace,true),!.
rtrace_this(_Call):- is_debugging(rtrace),!.

%:- nb_setval(cmt_override,lse('; ',' !(" ',' ") ')).

:- abolish(fbug/1).
fbug(_):- is_compatio,!.
fbug(Info):- real_notrace(in_cmt(color_g_mesg('#2f2f2f',write_src(Info)))).
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
   copy_term_g(XX,X),
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


catch_red_ignore(G):- if_or_else(catch_red(G),true).

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
   %nts1,
   %install_ontology,
   metta_final,
   % ensure_corelib_types,
   set_output_stream,
   if_t(is_compiled,update_changed_files),
   test_alarm,
   run_cmd_args,
   write_answer_output,
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
maybe_halt(_):- current_prolog_flag(mettalog_rt,true),!.
maybe_halt(H):- halt(H).


:- initialization(nb_setval(cmt_override,lse('; ',' !(" ',' ") ')),restore).


%needs_repl:- \+ is_converting, \+ is_pyswip, \+ is_compiling, \+ has_file_arg.
%  libswipl: ['./','-q',--home=/usr/local/lib/swipl]

:- initialization(show_os_argv).


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
    if_t(exists_source(library(swi_ide)),user:use_module(library(swi_ide))),
    user:use_module(library(prolog_profile)),
    %metta_python,
    %ensure_loaded('./src/main/flybase_convert'),
    %ensure_loaded('./src/main/flybase_main'),
    %ensure_loaded(library(flybase_convert)),
    %ensure_loaded(library(flybase_main)),
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


:- ensure_loaded(library(flybase_main)).
:- ensure_loaded(metta_server).
:- initialization(update_changed_files,restore).

nts :- nts1.
nts1:- !. % disable redefinition
nts1:-  redefine_system_predicate(system:notrace/1),
  %listing(system:notrace/1),
  abolish(system:notrace/1),
  dynamic(system:notrace/1),
  meta_predicate(system:notrace(0)),
  asserta((system:notrace(G):- (!,once(G)))).
nts1:- !.

:- nts1.

nts0:-  redefine_system_predicate(system:notrace/0),
  abolish(system:notrace/0),
  asserta((system:notrace:- wdmsg(notrace))).
%:- nts0.

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

%:- ensure_loaded('../../library/genome/flybase_loader').

:- ensure_loaded(metta_python).
:- ensure_loaded(metta_corelib).
:- initialization(use_corelib_file).

:- ignore(((
   %wdmsg(init_prog),
   use_corelib_file,
   (is_testing -> UNIT_TEST=true; UNIT_TEST=false),
   set_is_unit_test(UNIT_TEST),
   %trace,
   \+ prolog_load_context(reloading,true),
   initialization(loon(restore),restore),
   % should fail (if tested from here https://swi-prolog.discourse.group/t/call-with-time-limit-2-not-enforcing-time-limit-as-expected/7755)
   %test_alarm,
   % nts1,
   metta_final,
   true))).

:- initialization(use_corelib_file).
%:- initialization(loon(program),program).
%:- initialization(loon(default)).
:- set_prolog_flag(metta_interp,ready).
%:- set_prolog_flag(gc,false).

:- use_module(library(clpr)). % Import the CLP(R) library
%:- ensure_loaded('metta_ontology.pfc.pl').
%:- initialization(loon_main, main).
:- initialization(loon(main), main).

%:- ensure_loaded(mettalog('metta_ontology.pfc.pl')).


% Define a predicate to relate the likelihoods of three events
complex_relationship3_ex(Likelihood1, Likelihood2, Likelihood3) :-
    { Likelihood1 = 0.3 * Likelihood2 },
    { Likelihood2 = 0.5 * Likelihood3 },
    { Likelihood3 < 1.0 },
    { Likelihood3 > 0.0 }.

% Example query to find the likelihoods that satisfy the constraints
%?- complex_relationship(L1, L2, L3).
