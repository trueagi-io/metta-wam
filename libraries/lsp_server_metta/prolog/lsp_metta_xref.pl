:- module(lsp_metta_xref,[ensure_METTA_DIR/0,load_mettalog_xref/0,skip_metta/0,metta_home/1]).
:- user:use_module(library(logicmoo_common)).
:- user:use_module(library(logicmoo_utils)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD METTA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

metta_home(LTH):- getenv('METTALOG_DIR',LTH),!.
metta_home(LTH):- absolute_directory(pack(metta), LTH),
  % exists_directory(LTH),
  setenv('METTALOG_DIR',LTH),!.

skip_metta:- app_argv('--nometta'),!.
%skip_metta:- \+ app_argv1('--metta'),!.
skip_metta:- \+ metta_home(_), !.
skip_metta:- metta_home(LTH), \+ exists_directory(LTH),!.

had_METTA_DIR :- getenv('METTA_DIR', _Location),!.

ensure_METTA_DIR:- had_METTA_DIR,!.
ensure_METTA_DIR:- skip_metta, !, dmsg("Skipping metta").
% ensure_METTA_DIR:- user:use_module(library(metta)).
ensure_METTA_DIR:- metta_home(LTH),
   setenv('METTA_DIR', LTH),!.

:- multifile(metta:'$lgt_current_engine_'/4).
:- volatile(metta:'$lgt_current_engine_'/4).
%load_mettalog_xref(system):- !, load_mettalog_xref('/usr/share/metta').
%load_mettalog_xref(LTH):- atom_concat(LTH,'/integration/metta_swi',Init),
%  exists_source(Init),!,metta:ensure_loaded(Init),!,listing(metta:'$lgt_default_flag'/2).
load_mettalog_xref(LTH):- \+ exists_directory(LTH),!,dmsg("Skipping metta: not exists_directory").
load_mettalog_xref(LTH):- absolute_file_name('prolog/metta_lang/metta_interp.pl',File,[relative_to(LTH)]),!,
   notrace(load_mettalog_xref_file(File)).
load_mettalog_xref(LTH):- dmsg("Skipping metta="+LTH).

load_mettalog_xref_file(File):-
   locally(set_prolog_flag(argv,[]),
     locally(set_prolog_flag(os_argv,[swipl]),
       notrace(load_mettalog_xref_file_now(File)))).
    
load_mettalog_xref_file_now(File):-   
   set_prolog_flag(mettalog_rt, true),
   set_prolog_flag(mettalog_rt_args, []),
   set_prolog_flag(metta_argv, []),
   user:ensure_loaded(File),!,
   user:loon.



load_mettalog_xref:- current_predicate(write_src/1),!.
load_mettalog_xref:- skip_metta, !, dmsg("Skipping metta").
load_mettalog_xref:- metta_home(LTH), \+ exists_directory(LTH),!,dmsg("Skipping metta: not exists_directory").
load_mettalog_xref:- metta_home(LTH), exists_directory(LTH),!,load_mettalog_xref(LTH).

load_mettalog_xref:- dmsg("Skipping metta: no home dir").




%:- dmsg("Loading metta").
%:- before_boot(ensure_METTA_DIR).
:- before_boot(load_mettalog_xref).

% :- if( (( \+ prolog_load_context(reload,true) ))).

:- module_transparent(metta:'::'/1).
:- metta:export(metta:'::'/1).
:- user:import(metta:'::'/1).

:- module_transparent(metta:'::'/2).
:- metta:export(metta:'::'/2).
:- user:import(metta:'::'/2).
:- baseKB:import(metta:'::'/2).

%user:'::'(X,Y):- metta:'::'(X,Y).
%user:'::'(X):- metta:'::'(X).

:-op(200,fy,user:'--').
:-op(600,fy,user:'::').
:-op(600,xfy,user:'::').
:-op(200,fy,user:'++').
:-op(600,fy,user:'^^').


% :- endif.
:- fixup_exports.



