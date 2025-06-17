%% Generated from /home/deb12user/metta-wam/.Attic/vnamed/corelib.metta at 2025-06-17T11:44:24-07:00
:- style_check(-discontiguous).
:- style_check(-singleton).
:- include(library(metta_lang/metta_transpiled_header)).



top_call_1:- eval_H(['import!','&self','stdlib_mettalog.metta'],ExecRes).




top_call :-
    time(top_call_1).


%% Finished generating /home/deb12user/metta-wam/.Attic/vnamed/corelib.metta at 2025-06-17T11:44:24-07:00

:- normal_IO.
:- initialization(transpiled_main, program).
