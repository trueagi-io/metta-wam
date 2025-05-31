%% Generated from /home/deb12user/metta-wam/prolog/master/corelib.metta at 2025-05-30T01:59:12-07:00
:- set_prolog_flag(mettalog_rt,true).
%:- set_prolog_flag(mettalog_rt_args, ['--repl=false']).
%:- set_prolog_flag(mettalog_rt_args, ['--repl']).
:- include(library(metta_lang/metta_transpiled_header)).
%:- ensure_loaded(library(metta_lang/metta_interp)).
:- ensure_loaded(library(metta_rt)). % avoids starting the REPL
:- setup_library_calls.
:- style_check(-discontiguous).
:- style_check(-singleton).



top_call_1:- eval_H(['import!','stdlib_mettalog.metta'],ExecRes).




:- time(top_call_1).


