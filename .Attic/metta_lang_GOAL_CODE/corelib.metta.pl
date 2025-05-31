%% Generated from /home/deb12user/metta-wam/prolog/metta_lang/corelib.metta at 2025-05-24T14:42:08-07:00
:- set_prolog_flag(mettalog_rt,true).
%:- set_prolog_flag(mettalog_rt_args, ['--repl=false']).
%:- set_prolog_flag(mettalog_rt_args, ['--repl']).
:- include(library(metta_lang/metta_transpiled_header)).
%:- ensure_loaded(library(metta_lang/metta_interp)).
:- ensure_loaded(library(metta_rt)). % avoids starting the REPL
:- style_check(-discontiguous).
:- style_check(-singleton).

:-eval_H(['import!','stdlib_mettalog.metta'],ExecRes).
