% This file initializes and configures the runtime environment for MeTTaLog .. it is for non RELP user such as
% From the LSP server and transpiled MeTTa files.
% It sets the required directories, attaches necessary packages, configures Prolog flags for mettalog and
% arguments, and ensures that essential libraries are loaded. The environment is prepared for execution
% without starting the REPL, and includes detailed runtime logging and session tracking functionalities.

% Configure arguments specific to SWI-Prolog (this is so the LSP Server when it uses this library doesn't trick MeTTa into processing its args)
:- set_prolog_flag(argv, []),
   set_prolog_flag(os_argv, [swipl]).

% Set the directory context for loading packages; use the default directory if context retrieval fails
:- (prolog_load_context(directory, Value);Value='.'), absolute_file_name('../../packs/',Dir,[relative_to(Value)]),
   atom_concat(Dir, 'predicate_streams', PS),
   atom_concat(Dir, 'logicmoo_utils', LU),
   attach_packs(Dir, [duplicate(replace), search(first)]),
   pack_attach(PS, [duplicate(replace), search(first)]),
   pack_attach(LU, [duplicate(replace), search(first)]).

% Set Prolog flags for mettalog runtime and arguments configuration
:- set_prolog_flag(mettalog_rt, true),
   set_prolog_flag(mettalog_rt_args, []),
   set_prolog_flag(metta_argv, []).

% Ensure essential libraries are loaded and initialize the main application
% Uncomment the next line if 'metta_compiler_lib' needs to be loaded as well
% :- user:ensure_loaded(metta_compiler_lib),!,

:- user:ensure_loaded(metta_interp),!.

% Start interpreter code
:- user:loon.


