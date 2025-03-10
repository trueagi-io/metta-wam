/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2021, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
/*
:- module('metta_toplevel',
          [ '$initialise'/0,            % start Prolog
            'metta_toplevel'/0,              % Prolog top-level (re-entrant)
            '$compile'/0,               % `-c' toplevel
            '$config'/0,                % --dump-runtime-variables toplevel
            initialize/0,               % Run program '$initialization'
            version/0,                  % Write initial banner
            version/1,                  % Add message to the banner
            prolog/0,                   % user toplevel predicate
            'metta_query_loop'/0,            % toplevel predicate
            'metta_execute_query'/3,         % +Query, +Bindings, -Truth
            residual_goals/1,           % +Callable
            ('$initialization')/1,         % '$initialization' goal (directive)
            '$thread_init'/0,           % initialise thread
            (thread_initialization)/1   % thread '$initialization' goal
            ]).
*/

                 /*******************************
                 *         VERSION BANNER       *
                 *******************************/

:- dynamic
    prolog:version_msg/1.

%!  version is det.
%
%   Print the Prolog banner message and messages registered using
%   version/1.

version :-
    print_message(banner, welcome).

%!  version(+Message) is det.
%
%   Add message to version/0
/*
:- multifile
    system:term_expansion/2.

system:term_expansion((:- version(Message)),
                      prolog:version_msg(Message)).

version(Message) :-
    (   prolog:version_msg(Message)
    ->  true
    ;   assertz(prolog:version_msg(Message))
    ).
*/

                /********************************
                *         INITIALISATION        *
                *********************************/

%!  load_init_file is det.
%
%   Load the user customization file. This can  be done using ``swipl -f
%   file`` or simply using ``swipl``. In the   first  case we search the
%   file both directly and over  the   alias  `user_app_config`.  In the
%   latter case we only use the alias.

load_init_file :-
    '$cmd_option_val'(init_file, OsFile),
    !,
    prolog_to_os_filename(File, OsFile),
    load_init_file(File, explicit).
load_init_file :-
    load_init_file('init.pl', implicit).

%!  loaded_init_file(?Base, ?AbsFile)
%
%   Used by prolog_load_context/2 to confirm we are loading a script.

:- dynamic
    loaded_init_file/2.             % already loaded init files

load_init_file(none, _) :- !.
load_init_file(Base, _) :-
    loaded_init_file(Base, _),
    !.
load_init_file(InitFile, explicit) :-
    exists_file(InitFile),
    !,
    ensure_loaded(user:InitFile).
load_init_file(Base, _) :-
    absolute_file_name(user_app_config(Base), InitFile,
                       [ access(read),
                         file_errors(fail)
                       ]),
    asserta(loaded_init_file(Base, InitFile)),
    load_files(user:InitFile,
               [ scope_settings(false)
               ]).
load_init_file('init.pl', implicit) :-
    (   current_prolog_flag(windows, true),
        absolute_file_name(user_profile('swipl.ini'), InitFile,
                           [ access(read),
                             file_errors(fail)
                           ])
    ;   expand_file_name('~/.swiplrc', [InitFile]),
        exists_file(InitFile)
    ),
    !,
    print_message(warning, backcomp(init_file_moved(InitFile))).
load_init_file(_, _).

'$load_system_init_file' :-
    loaded_init_file(system, _),
    !.
'$load_system_init_file' :-
    '$cmd_option_val'(system_init_file, Base),
    Base \== none,
    current_prolog_flag(home, Home),
    file_name_extension(Base, rc, Name),
    atomic_list_concat([Home, '/', Name], File),
    absolute_file_name(File, Path,
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ]),
    asserta(loaded_init_file(system, Path)),
    load_files(user:Path,
               [ silent(true),
                 scope_settings(false)
               ]),
    !.
'$load_system_init_file'.

'$load_script_file' :-
    loaded_init_file(script, _),
    !.
'$load_script_file' :-
    '$cmd_option_val'(script_file, OsFiles),
    load_script_files(OsFiles).

load_script_files([]).
load_script_files([OsFile|More]) :-
    prolog_to_os_filename(File, OsFile),
    (   absolute_file_name(File, Path,
                           [ file_type(prolog),
                             access(read),
                             file_errors(fail)
                           ])
    ->  asserta(loaded_init_file(script, Path)),
        load_files(user:Path, []),
        load_files(More)
    ;   throw(error(existence_error(script_file, File), _))
    ).


                 /*******************************
                 *       AT_INITIALISATION      *
                 *******************************/

:- meta_predicate
    '$initialization'(0).

:- '$iso'(('$initialization')/1).

%!  '$initialization'(:Goal)
%
%   Runs Goal after loading the file in which this directive
%   appears as well as after restoring a saved state.
%
%   @see '$initialization'/2
/*
'$initialization'(Goal) :-
    Goal = _:G,
    prolog:initialize_now(G, Use),
    !,
    print_message(warning, initialize_now(G, Use)),
    initialization(Goal, now).
'$initialization'(Goal) :-
    initialization(Goal, after_load).

:- multifile
    prolog:initialize_now/2,
    prolog:message//1.

prolog:initialize_now(load_foreign_library(_),
                      'use :- use_foreign_library/1 instead').
prolog:initialize_now(load_foreign_library(_,_),
                      'use :- use_foreign_library/2 instead').

prolog:message(initialize_now(Goal, Use)) -->
    [ 'Initialization goal ~p will be executed'-[Goal],nl,
      'immediately for backward compatibility reasons', nl,
      '~w'-[Use]
    ].

'$run_initialization' :-
    '$run_initialization'(_, []),
    '$thread_init'.

%!  initialize
%
%   Run goals registered with `:-  '$initialization'(Goal, program).`. Stop
%   with an exception if a goal fails or raises an exception.

initialize :-
    forall('$init_goal'(when(program), Goal, Ctx),
           run_initialize(Goal, Ctx)).

run_initialize(Goal, Ctx) :-
    (   catch(Goal, E, true),
        (   var(E)
        ->  true
        ;   throw(error(initialization_error(E, Goal, Ctx), _))
        )
    ;   throw(error(initialization_error(failed, Goal, Ctx), _))
    ).

*/
                 /*******************************
                 *     THREAD INITIALIZATION    *
                 *******************************/
/*
:- meta_predicate
    thread_initialization(0).
:- dynamic
    '$at_thread_initialization'/1.

%!  thread_initialization(:Goal)
%
%   Run Goal now and everytime a new thread is created.

thread_initialization(Goal) :-
    assert('$at_thread_initialization'(Goal)),
    call(Goal),
    !.

'$thread_init' :-
    (   '$at_thread_initialization'(Goal),
        (   call(Goal)
        ->  fail
        ;   fail
        )
    ;   true
    ).
*/

                 /*******************************
                 *     FILE SEARCH PATH (-p)    *
                 *******************************/
/*
%!  '$set_file_search_paths' is det.
%
%   Process -p PathSpec options.

'$set_file_search_paths' :-
    '$cmd_option_val'(search_paths, Paths),
    (   '$member'(Path, Paths),
        atom_chars(Path, Chars),
        (   phrase('$search_path'(Name, Aliases), Chars)
        ->  '$reverse'(Aliases, Aliases1),
            forall('$member'(Alias, Aliases1),
                   asserta(user:file_search_path(Name, Alias)))
        ;   print_message(error, commandline_arg_type(p, Path))
        ),
        fail ; true
    ).

'$search_path'(Name, Aliases) -->
    '$string'(NameChars),
    [=],
    !,
    {atom_chars(Name, NameChars)},
    '$search_aliases'(Aliases).

'$search_aliases'([Alias|More]) -->
    '$string'(AliasChars),
    path_sep,
    !,
    { '$make_alias'(AliasChars, Alias) },
    '$search_aliases'(More).
'$search_aliases'([Alias]) -->
    '$string'(AliasChars),
    '$eos',
    !,
    { '$make_alias'(AliasChars, Alias) }.

path_sep -->
    { current_prolog_flag(windows, true)
    },
    !,
    [;].
path_sep -->
    [:].

'$string'([]) --> [].
'$string'([H|T]) --> [H], '$string'(T).

'$eos'([], []).

'$make_alias'(Chars, Alias) :-
    catch(term_to_atom(Alias, Chars), _, fail),
    (   atom(Alias)
    ;   functor(Alias, F, 1),
        F \== /
    ),
    !.
'$make_alias'(Chars, Alias) :-
    atom_chars(Alias, Chars).

*/
                 /*******************************
                 *   LOADING ASSIOCIATED FILES  *
                 *******************************/

%!  argv_files(-Files) is det.
%
%   Update the Prolog flag `argv`, extracting the leading script files.
/*
argv_files(Files) :-
    current_prolog_flag(argv, Argv),
    no_option_files(Argv, Argv1, Files, ScriptArgs),
    (   (   ScriptArgs == true
        ;   Argv1 == []
        )
    ->  (   Argv1 \== Argv
        ->  set_prolog_flag(argv, Argv1)
        ;   true
        )
    ;   '$usage',
        halt(1)
    ).

no_option_files([--|Argv], Argv, [], true) :- !.
no_option_files([Opt|_], _, _, ScriptArgs) :-
    ScriptArgs \== true,
    sub_atom(Opt, 0, _, _, '-'),
    !,
    '$usage',
    halt(1).
no_option_files([OsFile|Argv0], Argv, [File|T], ScriptArgs) :-
    file_name_extension(_, Ext, OsFile),
    user:prolog_file_type(Ext, prolog),
    !,
    ScriptArgs = true,
    prolog_to_os_filename(File, OsFile),
    no_option_files(Argv0, Argv, T, ScriptArgs).
no_option_files([OsScript|Argv], Argv, [Script], ScriptArgs) :-
    ScriptArgs \== true,
    !,
    prolog_to_os_filename(Script, OsScript),
    (   exists_file(Script)
    ->  true
    ;   '$existence_error'(file, Script)
    ),
    ScriptArgs = true.
no_option_files(Argv, Argv, [], _).

clean_argv :-
    (   current_prolog_flag(argv, [--|Argv])
    ->  set_prolog_flag(argv, Argv)
    ;   true
    ).

%!  associated_files(-Files)
%
%   If SWI-Prolog is started as <exe> <file>.<ext>, where <ext> is
%   the extension registered for associated files, set the Prolog
%   flag associated_file, switch to the directory holding the file
%   and -if possible- adjust the window title.

associated_files([]) :-
    current_prolog_flag(saved_program_class, runtime),
    !,
    clean_argv.
associated_files(Files) :-
    '$set_prolog_file_extension',
    argv_files(Files),
    (   Files = [File|_]
    ->  absolute_file_name(File, AbsFile),
        set_prolog_flag(associated_file, AbsFile),
        set_working_directory(File),
        set_window_title(Files)
    ;   true
    ).

%!  set_working_directory(+File)
%
%   When opening as a GUI application, e.g.,  by opening a file from
%   the Finder/Explorer/..., we typically  want   to  change working
%   directory to the location of  the   primary  file.  We currently
%   detect that we are a GUI app  by the Prolog flag =console_menu=,
%   which is set by swipl-win[.exe].

set_working_directory(File) :-
    current_prolog_flag(console_menu, true),
    access_file(File, read),
    !,
    file_directory_name(File, Dir),
    working_directory(_, Dir).
set_working_directory(_).

set_window_title([File|More]) :-
    current_predicate(system:window_title/2),
    !,
    (   More == []
    ->  Extra = []
    ;   Extra = ['...']
    ),
    atomic_list_concat(['SWI-Prolog --', File | Extra], ' ', Title),
    system:window_title(_, Title).
set_window_title(_).


%!  start_pldoc
%
%   If the option  =|--pldoc[=port]|=  is   given,  load  the  PlDoc
%   system.

start_pldoc :-
    '$cmd_option_val'(pldoc_server, Server),
    (   Server == ''
    ->  call((doc_server(_), doc_browser))
    ;   catch(atom_number(Server, Port), _, fail)
    ->  call(doc_server(Port))
    ;   print_message(error, option_usage(pldoc)),
        halt(1)
    ).
start_pldoc.


%!  load_associated_files(+Files)
%
%   Load Prolog files specified from the commandline.

load_associated_files(Files) :-
    (   '$member'(File, Files),
        load_files(user:File, [expand(false)]),
        fail
    ;   true
    ).

hkey('HKEY_CURRENT_USER/Software/SWI/Prolog').
hkey('HKEY_LOCAL_MACHINE/Software/SWI/Prolog').

'$set_prolog_file_extension' :-
    current_prolog_flag(windows, true),
    hkey(Key),
    catch(win_registry_get_value(Key, fileExtension, Ext0),
          _, fail),
    !,
    (   atom_concat('.', Ext, Ext0)
    ->  true
    ;   Ext = Ext0
    ),
    (   user:prolog_file_type(Ext, prolog)
    ->  true
    ;   asserta(user:prolog_file_type(Ext, prolog))
    ).
'$set_prolog_file_extension'.

*/
                /********************************
                *        TOPLEVEL GOALS         *
                *********************************/
/*
%!  '$initialise' is semidet.
%
%   Called from PL_initialise()  to  do  the   Prolog  part  of  the
%   '$initialization'. If an exception  occurs,   this  is  printed and
%   '$initialise' fails.

'$initialise' :-
    catch(initialise_prolog, E, initialise_error(E)).

initialise_error('$aborted') :- !.
initialise_error(E) :-
    print_message(error, initialization_exception(E)),
    fail.
*/
initialise_prolog :-
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
    catch(setup_colors, E, print_message(warning, E)),
    associated_files(Files),
    '$load_script_file',
    load_associated_files(Files),
    '$cmd_option_val'(goals, Goals),
    (   Goals == [],
        \+ '$init_goal'(when(_), _, _)
    ->  version                                 % default interactive run
    ;   run_init_goals(Goals),
        (   load_only
        ->  version
        ;   run_program_init,
            run_main_init
        )
    ).

:- if(current_prolog_flag(apple,true)).
apple_set_working_directory :-
    (   expand_file_name('~', [Dir]),
	exists_directory(Dir)
    ->  working_directory(_, Dir)
    ;   true
    ).

apple_set_locale :-
    (   getenv('LC_CTYPE', 'UTF-8'),
	apple_current_locale_identifier(LocaleID),
	atom_concat(LocaleID, '.UTF-8', Locale),
	catch(setlocale(ctype, _Old, Locale), _, fail)
    ->  setenv('LANG', Locale),
        unsetenv('LC_CTYPE')
    ;   true
    ).

apple_setup_app :-
    current_prolog_flag(apple, true),
    current_prolog_flag(console_menu, true),	% SWI-Prolog.app on MacOS
    apple_set_working_directory,
    apple_set_locale.
:- endif.
apple_setup_app.

opt_attach_packs :-
    current_prolog_flag(packs, true),
    !,
    attach_packs.
opt_attach_packs.

set_toplevel :-
    '$cmd_option_val'(toplevel, TopLevelAtom),
    catch(term_to_atom(TopLevel, TopLevelAtom), E,
          (print_message(error, E),
           halt(1))),
    create_prolog_flag(toplevel_goal, TopLevel, [type(term)]).

load_only :-
    current_prolog_flag(os_argv, OSArgv),
    memberchk('-l', OSArgv),
    current_prolog_flag(argv, Argv),
    \+ memberchk('-l', Argv).

%!  run_init_goals(+Goals) is det.
%
%   Run registered '$initialization' goals  on  order.   If  a  goal fails,
%   execution is halted.

run_init_goals([]).
run_init_goals([H|T]) :-
    run_init_goal(H),
    run_init_goals(T).

run_init_goal(Text) :-
    catch(term_to_atom(Goal, Text), E,
          (   print_message(error, init_goal_syntax(E, Text)),
              halt(2)
          )),
    run_init_goal(Goal, Text).

%!  run_program_init is det.
%
%   Run goals registered using

run_program_init :-
    forall('$init_goal'(when(program), Goal, Ctx),
           run_init_goal(Goal, @(Goal,Ctx))).

run_main_init :-
    findall(Goal-Ctx, '$init_goal'(when(main), Goal, Ctx), Pairs),
    '$last'(Pairs, Goal-Ctx),
    !,
    (   current_prolog_flag(toplevel_goal, default)
    ->  set_prolog_flag(toplevel_goal, halt)
    ;   true
    ),
    run_init_goal(Goal, @(Goal,Ctx)).
run_main_init.

run_init_goal(Goal, Ctx) :-
    (   catch_with_backtrace(user:Goal, E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(error, init_goal_failed(E, Ctx)),
            halt(2)
        )
    ;   (   current_prolog_flag(verbose, silent)
        ->  Level = silent
        ;   Level = error
        ),
        print_message(Level, init_goal_failed(failed, Ctx)),
        halt(1)
    ).

%!  init_debug_flags is det.
%
%   Initialize the various Prolog flags that   control  the debugger and
%   toplevel.

init_debug_flags :-
    once(print_predicate(_, [print], PrintOptions)),
    Keep = [keep(true)],
    create_prolog_flag(answer_write_options, PrintOptions, Keep),
    create_prolog_flag(prompt_alternatives_on, determinism, Keep),
    create_prolog_flag(toplevel_extra_white_line, true, Keep),
    create_prolog_flag(toplevel_print_factorized, false, Keep),
    create_prolog_flag(print_write_options,
                       [ portray(true), quoted(true), numbervars(true) ],
                       Keep),
    create_prolog_flag(toplevel_residue_vars, false, Keep),
    create_prolog_flag(toplevel_list_wfs_residual_program, true, Keep),
    '$set_debugger_write_options'(print).

%!  setup_backtrace
%
%   Initialise printing a backtrace.

setup_backtrace :-
    (   \+ current_prolog_flag(backtrace, false),
        load_setup_file(library(prolog_stack))
    ->  true
    ;   true
    ).

%!  setup_colors is det.
%
%   Setup  interactive  usage  by  enabling    colored   output.

setup_colors :-
    (   \+ current_prolog_flag(color_term, false),
        stream_property(user_input, tty(true)),
        stream_property(user_error, tty(true)),
        stream_property(user_output, tty(true)),
        \+ getenv('TERM', dumb),
        load_setup_file(user:library(ansi_term))
    ->  true
    ;   true
    ).

%!  setup_history
%
%   Enable per-directory persistent history.

setup_history :-
    (   \+ current_prolog_flag(save_history, false),
        stream_property(user_input, tty(true)),
        \+ current_prolog_flag(readline, false),
        load_setup_file(library(prolog_history))
    ->  prolog_history(enable)
    ;   true
    ),
    set_default_history,
    '$load_history'.

%!  setup_readline
%
%   Setup line editing.

setup_readline :-
    (   current_prolog_flag(readline, swipl_win)
    ->  true
    ;   stream_property(user_input, tty(true)),
        current_prolog_flag(tty_control, true),
        \+ getenv('TERM', dumb),
        (   current_prolog_flag(readline, ReadLine)
        ->  true
        ;   ReadLine = true
        ),
        readline_library(ReadLine, Library),
        load_setup_file(library(Library))
    ->  set_prolog_flag(readline, Library)
    ;   set_prolog_flag(readline, false)
    ).

readline_library(true, Library) :-
    !,
    preferred_readline(Library).
readline_library(false, _) :-
    !,
    fail.
readline_library(Library, Library).

preferred_readline(editline).
preferred_readline(readline).

%!  load_setup_file(+File) is semidet.
%
%   Load a file and fail silently if the file does not exist.

load_setup_file(File) :-
    catch(load_files(File,
                     [ silent(true),
                       if(not_loaded)
                     ]), _, fail).


:- '$hide'('metta_toplevel'/0).              % avoid in the GUI stacktrace

%!  'metta_toplevel'
%
%   Called from PL_toplevel()

'metta_toplevel' :-
    '$runtoplevel',
    print_message(informational, halt).

%!  '$runtoplevel'
%
%   Actually run the toplevel. The values   `default`  and `prolog` both
%   start the interactive toplevel, where `prolog` implies the user gave
%   =|-t prolog|=.
%
%   @see prolog/0 is the default interactive toplevel

'$runtoplevel' :-
    current_prolog_flag(toplevel_goal, TopLevel0),
    toplevel_goal(TopLevel0, TopLevel),
    user:TopLevel.

:- dynamic  setup_done/0.
:- volatile setup_done/0.

toplevel_goal(default, 'metta_query_loop') :-
    !,
    setup_interactive.
toplevel_goal(prolog, 'metta_query_loop') :-
    !,
    setup_interactive.
toplevel_goal(Goal, Goal).

setup_interactive :-
    setup_done,
    !.
setup_interactive :-
    asserta(setup_done),
    catch(setup_backtrace, E, print_message(warning, E)),
    catch(setup_readline,  E, print_message(warning, E)),
    catch(setup_history,   E, print_message(warning, E)).

%!  '$compile'
%
%   Toplevel called when invoked with -c option.

'$compile' :-
    (   catch('$compile_', E, (print_message(error, E), halt(1)))
    ->  true
    ;   print_message(error, error(goal_failed('$compile'), _)),
        halt(1)
    ),
    halt.                               % set exit code

'$compile_' :-
    '$load_system_init_file',
    catch(setup_colors, _, true),
    '$set_file_search_paths',
    init_debug_flags,
    '$run_initialization',
    opt_attach_packs,
    use_module(library(qsave)),
    qsave:qsave_toplevel.

%!  '$config'
%
%   Toplevel when invoked with --dump-runtime-variables
/*
'$config' :-
    '$load_system_init_file',
    '$set_file_search_paths',
    init_debug_flags,
    '$run_initialization',
    load_files(library(prolog_config)),
    (   catch(prolog_dump_runtime_variables, E,
              (print_message(error, E), halt(1)))
    ->  true
    ;   print_message(error, error(goal_failed(prolog_dump_runtime_variables),_))
    ).

*/
                /********************************
                *    USER INTERACTIVE LOOP      *
                *********************************/

%!  prolog
%
%   Run the Prolog toplevel. This is now  the same as break/0, which
%   pretends  to  be  in  a  break-level    if  there  is  a  parent
%   environment.

%prolog :-  break.

:- create_prolog_flag(toplevel_mode, backtracking, []).

%!  'metta_query_loop'
%
%   Run the normal Prolog query loop.  Note   that  the query is not
%   protected by catch/3. Dealing with  unhandled exceptions is done
%   by the C-function query_loop().  This   ensures  that  unhandled
%   exceptions are really unhandled (in Prolog).

'metta_query_loop' :-
    current_prolog_flag(toplevel_mode, recursive),
    !,
    break_level(Level),
    read_expanded_query(Level, Query, Bindings),
    (   Query == end_of_file
    ->  print_message(query, query(eof))
    ;   '$call_no_catch'('metta_execute_query'(Query, Bindings, _)),
        (   current_prolog_flag(toplevel_mode, recursive)
        ->  'metta_query_loop'
        ;   '$switch_toplevel_mode'(backtracking),
            'metta_query_loop'           % Maybe throw('$switch_toplevel_mode')?
        )
    ).
'metta_query_loop' :-
    break_level(BreakLev),
    repeat,
        read_expanded_query(BreakLev, Query, Bindings),
        (   Query == end_of_file
        ->  !, print_message(query, query(eof))
        ;   'metta_execute_query'(Query, Bindings, _),
            (   current_prolog_flag(toplevel_mode, recursive)
            ->  !,
                '$switch_toplevel_mode'(recursive),
                'metta_query_loop'
            ;   fail
            )
        ).

break_level(BreakLev) :-
    (   current_prolog_flag(break_level, BreakLev)
    ->  true
    ;   BreakLev = -1
    ).

read_expanded_query(BreakLev, ExpandedQuery, ExpandedBindings) :-
    '$current_typein_module'(TypeIn),
    (   stream_property(user_input, tty(true))
    ->  '$system_prompt'(TypeIn, BreakLev, Prompt),
        prompt(Old, '|    ')
    ;   Prompt = '',
        prompt(Old, '')
    ),
    trim_stacks,
    trim_heap,
    repeat,
      read_query(Prompt, Query, Bindings),
      prompt(_, Old),
      catch(call_expand_query(Query, ExpandedQuery,
                              Bindings, ExpandedBindings),
            Error,
            (print_message(error, Error), fail)),
    !.

%!  read_s_term_with_history(-Term, +Options)
%
%   Read a term guide by Options and  maintain a history similar to most
%   Unix shells.
%
%   When read_history reads a term of   the  form $silent(Goal), it will
%   call Goal and pretend it has not seen anything. This hook is used by
%   the GNU-Emacs interface to for   communication between GNU-EMACS and
%   SWI-Prolog.

read_s_term_with_history(Term, Options) :-
    '$option'(prompt(Prompt), Options, '~! metta>'),
    '$option'(input(Input), Options, user_input),
    repeat,
        prompt_history(Prompt),
        read_query_line(Input, Raw),
        read_history_(Raw, Term, Options),
    !.


%!  read_query(+Prompt, -Goal, -Bindings) is det.
%
%   Read the next query. The first  clause   deals  with  the case where
%   !-based history is enabled. The second is   used  if we have command
%   line editing.


:- if(current_prolog_flag(emscripten, true)).
read_query(_Prompt, Goal, Bindings) :-
    '$can_yield',
    !,
    await(goal, GoalString),
    term_string(Goal, GoalString, [variable_names(Bindings)]).
:- endif.
read_query(Prompt, Goal, Bindings) :-
    current_prolog_flag(history, N),
    integer(N), N > 0,
    !,
    read_s_term_with_history(
        Goal,
        [ show(h),
          help('!h'),
          no_save([trace, end_of_file]),
          prompt(Prompt),
          variable_names(Bindings)
        ]).
read_query(Prompt, Goal, Bindings) :-
    remove_history_prompt(Prompt, Prompt1),
    repeat,                                 % over syntax errors
    prompt1(Prompt1),
    read_query_line(user_input, Line),
    '$save_history_line'(Line),             % save raw line (edit syntax errors)
    '$current_typein_module'(TypeIn),
    catch(read_s_term_as_atom(Line, Goal,
                              [ variable_names(Bindings),
                                module(TypeIn)
                              ]), E,
          (   print_message(error, E),
              fail
          )),
    !,
    '$save_history_event'(Line).            % save event (no syntax errors)

%!  read_query_line(+Input, -Line) is det.

read_query_line(Input, Line) :-
    stream_property(Input, error(true)),
    !,
    Line = end_of_file.
read_query_line(Input, Line) :-
    catch(read_s_term_as_atom(Input, Line), Error, true),
    save_debug_after_read,
    (   var(Error)
    ->  true
    ;   catch(print_message(error, Error), _, true),
        (   Error = error(syntax_error(_),_)
        ->  fail
        ;   throw(Error)
        )
    ).

%!  read_s_term_as_atom(+Input, -Line)
%
%   Read the next term as an  atom  and   skip  to  the newline or a
%   non-space character.

read_s_term_as_atom(In, Line) :-
  read_metta(In,Line),
      (   Line == end_of_file
    ->  true
    ;   skip_to_nl(In)
    ).
/*
read_s_term_as_atom(In, Line) :-
    '$raw_read'(In, Line),
    (   Line == end_of_file
    ->  true
    ;   skip_to_nl(In)
    ).
*/

%!  skip_to_nl(+Input) is det.
%
%   Read input after the term. Skips   white  space and %... comment
%   until the end of the line or a non-blank character.

skip_to_nl(In) :-
    repeat,
    peek_char(In, C),
    (   C == '%'
    ->  skip(In, '\n')
    ;   char_type(C, space)
    ->  get_char(In, _),
        C == '\n'
    ;   true
    ),
    !.

remove_history_prompt('', '') :- !.
remove_history_prompt(Prompt0, Prompt) :-
    atom_chars(Prompt0, Chars0),
    clean_history_prompt_chars(Chars0, Chars1),
    delete_leading_blanks(Chars1, Chars),
    atom_chars(Prompt, Chars).

clean_history_prompt_chars([], []).
clean_history_prompt_chars(['~', !|T], T) :- !.
clean_history_prompt_chars([H|T0], [H|T]) :-
    clean_history_prompt_chars(T0, T).

delete_leading_blanks([' '|T0], T) :-
    !,
    delete_leading_blanks(T0, T).
delete_leading_blanks(L, L).


%!  set_default_history
%
%   Enable !-based numbered command history. This  is enabled by default
%   if we are not running under GNU-emacs  and   we  do not have our own
%   line editing.

set_default_history :-
    current_prolog_flag(history, _),
    !.
set_default_history :-
    (   (   \+ current_prolog_flag(readline, false)
        ;   current_prolog_flag(emacs_inferior_process, true)
        )
    ->  create_prolog_flag(history, 0, [])
    ;   create_prolog_flag(history, 25, [])
    ).


                 /*******************************
                 *        TOPLEVEL DEBUG        *
                 *******************************/

%!  save_debug_after_read
%
%   Called right after the toplevel read to save the debug status if
%   it was modified from the GUI thread using e.g.
%
%     ==
%     thread_signal(main, gdebug)
%     ==
%
%   @bug Ideally, the prompt would change if debug mode is enabled.
%        That is hard to realise with all the different console
%        interfaces supported by SWI-Prolog.

save_debug_after_read :-
    current_prolog_flag(debug, true),
    !,
    save_debug.
save_debug_after_read.

save_debug :-
    (   tracing,
        notrace
    ->  Tracing = true
    ;   Tracing = false
    ),
    current_prolog_flag(debug, Debugging),
    set_prolog_flag(debug, false),
    create_prolog_flag(query_debug_settings,
                       debug(Debugging, Tracing), []).

restore_debug :-
    current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
    set_prolog_flag(debug, Debugging),
    (   Tracing == true
    ->  trace
    ;   true
    ).

%:- '$initialization'(create_prolog_flag(query_debug_settings, debug(false, false), [])).


                /********************************
                *            PROMPTING          *
                ********************************/

'$system_prompt'(Module, BrekLev, Prompt) :-
    current_prolog_flag(toplevel_prompt, PAtom),
    atom_codes(PAtom, P0),
    (    Module \== user
    ->   '$substitute'('~m', [Module, ': '], P0, P1)
    ;    '$substitute'('~m', [], P0, P1)
    ),
    (    BrekLev > 0
    ->   '$substitute'('~l', ['[', BrekLev, '] '], P1, P2)
    ;    '$substitute'('~l', [], P1, P2)
    ),
    current_prolog_flag(query_debug_settings, debug(Debugging, Tracing)),
    (    Tracing == true
    ->   '$substitute'('~d', ['[trace] '], P2, P3)
    ;    Debugging == true
    ->   '$substitute'('~d', ['[debug] '], P2, P3)
    ;    '$substitute'('~d', [], P2, P3)
    ),
    atom_chars(Prompt, P3).

'$substitute'(From, T, Old, New) :-
    atom_codes(From, FromCodes),
    phrase(subst_chars(T), T0),
    '$append'(Pre, S0, Old),
    '$append'(FromCodes, Post, S0) ->
    '$append'(Pre, T0, S1),
    '$append'(S1, Post, New),
    !.
'$substitute'(_, _, Old, Old).

subst_chars([]) -->
    [].
subst_chars([H|T]) -->
    { atomic(H),
      !,
      atom_codes(H, Codes)
    },
    Codes,
    subst_chars(T).
subst_chars([H|T]) -->
    H,
    subst_chars(T).


                /********************************
                *           EXECUTION           *
                ********************************/

%!  'metta_execute_query'(Goal, Bindings, -Truth) is det.
%
%   Execute Goal using Bindings.

'metta_execute_query'(Var, _, true) :-
    var(Var),
    !,
    print_message(informational, var_query(Var)).
'metta_execute_query'(Goal, Bindings, Truth) :-
    '$current_typein_module'(TypeIn),
    '$dwim_correct_goal'(TypeIn:Goal, Bindings, Corrected),
    !,
    setup_call_cleanup(
        '$set_source_module'(M0, TypeIn),
        expand_goal(Corrected, Expanded),
        '$set_source_module'(M0)),
    print_message(silent, toplevel_goal(Expanded, Bindings)),
    '$execute_goal2'(Expanded, Bindings, Truth).
'metta_execute_query'(_, _, false) :-
    notrace,
    print_message(query, query(no)).

'$execute_goal2'(Goal, Bindings, true) :-
    restore_debug,
    '$current_typein_module'(TypeIn),
    residue_vars(TypeIn:Goal, Vars, TypeIn:Delays, Chp),
    deterministic(Det),
    (   save_debug
    ;   restore_debug, fail
    ),
    flush_output(user_output),
    (   Det == true
    ->  DetOrChp = true
    ;   DetOrChp = Chp
    ),
    call_expand_answer(Bindings, NewBindings),
    (    \+ \+ write_bindings(NewBindings, Vars, Delays, DetOrChp)
    ->   !
    ).
'$execute_goal2'(_, _, false) :-
    save_debug,
    print_message(query, query(no)).

residue_vars(Goal, Vars, Delays, Chp) :-
    current_prolog_flag(toplevel_residue_vars, true),
    !,
    '$wfs_call'(call_residue_vars(stop_backtrace(Goal, Chp), Vars), Delays).
residue_vars(Goal, [], Delays, Chp) :-
    '$wfs_call'(stop_backtrace(Goal, Chp), Delays).

stop_backtrace(Goal, Chp) :-
    toplevel_call(Goal),
    prolog_current_choice(Chp).

toplevel_call(Goal) :-
    call(Goal),
    no_lco.

no_lco.

%!  write_bindings(+Bindings, +ResidueVars, +Delays, +DetOrChp)
%!	is semidet.
%
%   Write   bindings   resulting   from   a     query.    The   flag
%   prompt_alternatives_on determines whether the   user is prompted
%   for alternatives. =groundness= gives   the  classical behaviour,
%   =determinism= is considered more adequate and informative.
%
%   Succeeds if the user accepts the answer and fails otherwise.
%
%   @arg ResidueVars are the residual constraints and provided if
%        the prolog flag `toplevel_residue_vars` is set to
%        `project`.

write_bindings(Bindings, ResidueVars, Delays, DetOrChp) :-
    '$current_typein_module'(TypeIn),
    translate_bindings(Bindings, Bindings1, ResidueVars, TypeIn:Residuals),
    omit_qualifier(Delays, TypeIn, Delays1),
    name_vars(Bindings1, Residuals, Delays1),
    write_bindings2(Bindings1, Residuals, Delays1, DetOrChp).

write_bindings2([], Residuals, Delays, _) :-
    current_prolog_flag(prompt_alternatives_on, groundness),
    !,
    print_message(query, query(yes(Delays, Residuals))).
write_bindings2(Bindings, Residuals, Delays, true) :-
    current_prolog_flag(prompt_alternatives_on, determinism),
    !,
    print_message(query, query(yes(Bindings, Delays, Residuals))).
write_bindings2(Bindings, Residuals, Delays, Chp) :-
    repeat,
        print_message(query, query(more(Bindings, Delays, Residuals))),
        get_respons(Action, Chp),
    (   Action == redo
    ->  !, fail
    ;   Action == show_again
    ->  fail
    ;   !,
        print_message(query, query(done))
    ).

name_vars(Bindings, Residuals, Delays) :-
    current_prolog_flag(toplevel_name_variables, true),
    !,
    '$term_multitons'(t(Bindings,Residuals,Delays), Vars),
    name_vars_(Vars, Bindings, 0),
    term_variables(t(Bindings,Residuals,Delays), SVars),
    anon_vars(SVars).
name_vars(_Bindings, _Residuals, _Delays).

name_vars_([], _, _).
name_vars_([H|T], Bindings, N) :-
    name_var(Bindings, Name, N, N1),
    H = '$VAR'(Name),
    name_vars_(T, Bindings, N1).

anon_vars([]).
anon_vars(['$VAR'('_')|T]) :-
    anon_vars(T).

name_var(Bindings, Name, N0, N) :-
    between(N0, infinite, N1),
    I is N1//26,
    J is 0'A + N1 mod 26, %'
    (   I == 0
    ->  format(atom(Name), '_~c', [J])
    ;   format(atom(Name), '_~c~d', [J, I])
    ),
    (   current_prolog_flag(toplevel_print_anon, false)
    ->  true
    ;   \+ is_bound(Bindings, Name)
    ),
    !,
    N is N1+1.

is_bound([Vars=_|T], Name) :-
    (   in_vars(Vars, Name)
    ->  true
    ;   is_bound(T, Name)
    ).

in_vars(Name, Name) :- !.
in_vars(Names, Name) :-
    '$member'(Name, Names).

%!  residual_goals(:NonTerminal)
%
%   Directive that registers NonTerminal as a collector for residual
%   goals.
/*
:- multifile
    residual_goal_collector/1.

:- meta_predicate
    residual_goals(2).

residual_goals(NonTerminal) :-
    throw(error(context_error(nodirective, residual_goals(NonTerminal)), _)).

system:term_expansion((:- residual_goals(NonTerminal)),
                      'metta_toplevel':residual_goal_collector(M2:Head)) :-
    \+ current_prolog_flag(xref, true),
    prolog_load_context(module, M),
    strip_module(M:NonTerminal, M2, Head),
    '$must_be'(callable, Head).

%!  prolog:residual_goals// is det.
%
%   DCG that collects residual goals that   are  not associated with
%   the answer through attributed variables.

:- public prolog:residual_goals//0.

prolog:residual_goals -->
    { findall(NT, residual_goal_collector(NT), NTL) },
    collect_residual_goals(NTL).

collect_residual_goals([]) --> [].
collect_residual_goals([H|T]) -->
    ( call(H) -> [] ; [] ),
    collect_residual_goals(T).
*/


%!  prolog:translate_bindings(+Bindings0, -Bindings, +ResidueVars,
%!                            +ResidualGoals, -Residuals) is det.
%
%   Translate the raw variable bindings  resulting from successfully
%   completing a query into a  binding   list  and  list of residual
%   goals suitable for human consumption.
%
%   @arg    Bindings is a list of binding(Vars,Value,Substitutions),
%           where Vars is a list of variable names. E.g.
%           binding(['A','B'],42,[])` means that both the variable
%           A and B have the value 42. Values may contain terms
%           '$VAR'(Name) to indicate sharing with a given variable.
%           Value is always an acyclic term. If cycles appear in the
%           answer, Substitutions contains a list of substitutions
%           that restore the original term.
%
%   @arg    Residuals is a pair of two lists representing residual
%           goals. The first element of the pair are residuals
%           related to the query variables and the second are
%           related that are disconnected from the query.
/*
:- public
    prolog:translate_bindings/5.
:- meta_predicate
    prolog:translate_bindings(+, -, +, +, :).

prolog:translate_bindings(Bindings0, Bindings, ResVars, ResGoals, Residuals) :-
    translate_bindings(Bindings0, Bindings, ResVars, ResGoals, Residuals).
*/
translate_bindings(Bindings0, Bindings, ResidueVars, Residuals) :-
    prolog:residual_goals(ResidueGoals, []),
    translate_bindings(Bindings0, Bindings, ResidueVars, ResidueGoals,
                       Residuals).

translate_bindings(Bindings0, Bindings, [], [], _:[]-[]) :-
    term_attvars(Bindings0, []),
    !,
    join_same_bindings(Bindings0, Bindings1),
    factorize_bindings(Bindings1, Bindings2),
    bind_vars(Bindings2, Bindings3),
    filter_bindings(Bindings3, Bindings).
translate_bindings(Bindings0, Bindings, ResidueVars, ResGoals0,
                   TypeIn:Residuals-HiddenResiduals) :-
    project_constraints(Bindings0, ResidueVars),
    hidden_residuals(ResidueVars, Bindings0, HiddenResiduals0),
    omit_qualifiers(HiddenResiduals0, TypeIn, HiddenResiduals),
    copy_term(Bindings0+ResGoals0, Bindings1+ResGoals1, Residuals0),
    '$append'(ResGoals1, Residuals0, Residuals1),
    omit_qualifiers(Residuals1, TypeIn, Residuals),
    join_same_bindings(Bindings1, Bindings2),
    factorize_bindings(Bindings2, Bindings3),
    bind_vars(Bindings3, Bindings4),
    filter_bindings(Bindings4, Bindings).

hidden_residuals(ResidueVars, Bindings, Goal) :-
    term_attvars(ResidueVars, Remaining),
    term_attvars(Bindings, QueryVars),
    subtract_vars(Remaining, QueryVars, HiddenVars),
    copy_term(HiddenVars, _, Goal).

subtract_vars(All, Subtract, Remaining) :-
    sort(All, AllSorted),
    sort(Subtract, SubtractSorted),
    ord_subtract(AllSorted, SubtractSorted, Remaining).

ord_subtract([], _Not, []).
ord_subtract([H1|T1], L2, Diff) :-
    diff21(L2, H1, T1, Diff).

diff21([], H1, T1, [H1|T1]).
diff21([H2|T2], H1, T1, Diff) :-
    compare(Order, H1, H2),
    diff3(Order, H1, T1, H2, T2, Diff).

diff12([], _H2, _T2, []).
diff12([H1|T1], H2, T2, Diff) :-
    compare(Order, H1, H2),
    diff3(Order, H1, T1, H2, T2, Diff).

diff3(<,  H1, T1,  H2, T2, [H1|Diff]) :-
    diff12(T1, H2, T2, Diff).
diff3(=, _H1, T1, _H2, T2, Diff) :-
    ord_subtract(T1, T2, Diff).
diff3(>,  H1, T1, _H2, T2, Diff) :-
    diff21(T2, H1, T1, Diff).


%!  project_constraints(+Bindings, +ResidueVars) is det.
%
%   Call   <module>:project_attributes/2   if   the    Prolog   flag
%   `toplevel_residue_vars` is set to `project`.

project_constraints(Bindings, ResidueVars) :-
    !,
    term_attvars(Bindings, AttVars),
    phrase(attribute_modules(AttVars), Modules0),
    sort(Modules0, Modules),
    term_variables(Bindings, QueryVars),
    project_attributes(Modules, QueryVars, ResidueVars).
project_constraints(_, _).

project_attributes([], _, _).
project_attributes([M|T], QueryVars, ResidueVars) :-
    (   current_predicate(M:project_attributes/2),
        catch(M:project_attributes(QueryVars, ResidueVars), E,
              print_message(error, E))
    ->  true
    ;   true
    ),
    project_attributes(T, QueryVars, ResidueVars).

attribute_modules([]) --> [].
attribute_modules([H|T]) -->
    { get_attrs(H, Attrs) },
    attrs_modules(Attrs),
    attribute_modules(T).

attrs_modules([]) --> [].
attrs_modules(att(Module, _, More)) -->
    [Module],
    attrs_modules(More).


%!  join_same_bindings(Bindings0, Bindings)
%
%   Join variables that are bound to the   same  value. Note that we
%   return the _last_ value. This is   because the factorization may
%   be different and ultimately the names will   be  printed as V1 =
%   V2, ... VN = Value. Using the  last, Value has the factorization
%   of VN.

join_same_bindings([], []).
join_same_bindings([Name=V0|T0], [[Name|Names]=V|T]) :-
    take_same_bindings(T0, V0, V, Names, T1),
    join_same_bindings(T1, T).

take_same_bindings([], Val, Val, [], []).
take_same_bindings([Name=V1|T0], V0, V, [Name|Names], T) :-
    V0 == V1,
    !,
    take_same_bindings(T0, V1, V, Names, T).
take_same_bindings([Pair|T0], V0, V, Names, [Pair|T]) :-
    take_same_bindings(T0, V0, V, Names, T).


%!  omit_qualifiers(+QGoals, +TypeIn, -Goals) is det.
%
%   Omit unneeded module qualifiers  from   QGoals  relative  to the
%   given module TypeIn.


omit_qualifiers([], _, []).
omit_qualifiers([Goal0|Goals0], TypeIn, [Goal|Goals]) :-
    omit_qualifier(Goal0, TypeIn, Goal),
    omit_qualifiers(Goals0, TypeIn, Goals).

omit_qualifier(M:G0, TypeIn, G) :-
    M == TypeIn,
    !,
    omit_meta_qualifiers(G0, TypeIn, G).
omit_qualifier(M:G0, TypeIn, G) :-
    predicate_property(TypeIn:G0, imported_from(M)),
    \+ predicate_property(G0, transparent),
    !,
    G0 = G.
omit_qualifier(_:G0, _, G) :-
    predicate_property(G0, built_in),
    \+ predicate_property(G0, transparent),
    !,
    G0 = G.
omit_qualifier(M:G0, _, M:G) :-
    atom(M),
    !,
    omit_meta_qualifiers(G0, M, G).
omit_qualifier(G0, TypeIn, G) :-
    omit_meta_qualifiers(G0, TypeIn, G).

omit_meta_qualifiers(V, _, V) :-
    var(V),
    !.
omit_meta_qualifiers((QA,QB), TypeIn, (A,B)) :-
    !,
    omit_qualifier(QA, TypeIn, A),
    omit_qualifier(QB, TypeIn, B).
omit_meta_qualifiers(tnot(QA), TypeIn, tnot(A)) :-
    !,
    omit_qualifier(QA, TypeIn, A).
omit_meta_qualifiers(freeze(V, QGoal), TypeIn, freeze(V, Goal)) :-
    callable(QGoal),
    !,
    omit_qualifier(QGoal, TypeIn, Goal).
omit_meta_qualifiers(when(Cond, QGoal), TypeIn, when(Cond, Goal)) :-
    callable(QGoal),
    !,
    omit_qualifier(QGoal, TypeIn, Goal).
omit_meta_qualifiers(G, _, G).


%!  bind_vars(+BindingsIn, -Bindings)
%
%   Bind variables to '$VAR'(Name), so they are printed by the names
%   used in the query. Note that by   binding  in the reverse order,
%   variables bound to one another come out in the natural order.

bind_vars(Bindings0, Bindings) :-
    bind_query_vars(Bindings0, Bindings, SNames),
    bind_skel_vars(Bindings, Bindings, SNames, 1, _).

bind_query_vars([], [], []).
bind_query_vars([binding(Names,Var,[Var2=Cycle])|T0],
                [binding(Names,Cycle,[])|T], [Name|SNames]) :-
    Var == Var2,                   % also implies var(Var)
    !,
    '$last'(Names, Name),
    Var = '$VAR'(Name),
    bind_query_vars(T0, T, SNames).
bind_query_vars([B|T0], [B|T], AllNames) :-
    B = binding(Names,Var,Skel),
    bind_query_vars(T0, T, SNames),
    (   var(Var), \+ attvar(Var), Skel == []
    ->  AllNames = [Name|SNames],
        '$last'(Names, Name),
        Var = '$VAR'(Name)
    ;   AllNames = SNames
    ).



bind_skel_vars([], _, _, N, N).
bind_skel_vars([binding(_,_,Skel)|T], Bindings, SNames, N0, N) :-
    bind_one_skel_vars(Skel, Bindings, SNames, N0, N1),
    bind_skel_vars(T, Bindings, SNames, N1, N).

%!  bind_one_skel_vars(+Subst, +Bindings, +VarName, +N0, -N)
%
%   Give names to the factorized variables that   do not have a name
%   yet. This introduces names  _S<N>,   avoiding  duplicates.  If a
%   factorized variable shares with another binding, use the name of
%   that variable.
%
%   @tbd    Consider the call below. We could remove either of the
%           A = x(1).  Which is best?
%
%           ==
%           ?- A = x(1), B = a(A,A).
%           A = x(1),
%           B = a(A, A), % where
%               A = x(1).
%           ==

bind_one_skel_vars([], _, _, N, N).
bind_one_skel_vars([Var=Value|T], Bindings, Names, N0, N) :-
    (   var(Var)
    ->  (   '$member'(binding(Names, VVal, []), Bindings),
            same_term(Value, VVal)
        ->  '$last'(Names, VName),
            Var = '$VAR'(VName),
            N2 = N0
        ;   between(N0, infinite, N1),
            atom_concat('_S', N1, Name),
            \+ memberchk(Name, Names),
            !,
            Var = '$VAR'(Name),
            N2 is N1 + 1
        )
    ;   N2 = N0
    ),
    bind_one_skel_vars(T, Bindings, Names, N2, N).


%!  factorize_bindings(+Bindings0, -Factorized)
%
%   Factorize cycles and sharing in the bindings.

factorize_bindings([], []).
factorize_bindings([Name=Value|T0], [binding(Name, Skel, Subst)|T]) :-
    '$factorize_term'(Value, Skel, Subst0),
    (   current_prolog_flag(toplevel_print_factorized, true)
    ->  Subst = Subst0
    ;   only_cycles(Subst0, Subst)
    ),
    factorize_bindings(T0, T).


only_cycles([], []).
only_cycles([B|T0], List) :-
    (   B = (Var=Value),
        Var = Value,
        acyclic_term(Var)
    ->  only_cycles(T0, List)
    ;   List = [B|T],
        only_cycles(T0, T)
    ).


%!  filter_bindings(+Bindings0, -Bindings)
%
%   Remove bindings that must not be printed. There are two of them:
%   Variables whose name start with '_'  and variables that are only
%   bound to themselves (or, unbound).

filter_bindings([], []).
filter_bindings([H0|T0], T) :-
    hide_vars(H0, H),
    (   (   arg(1, H, [])
        ;   self_bounded(H)
        )
    ->  filter_bindings(T0, T)
    ;   T = [H|T1],
        filter_bindings(T0, T1)
    ).

hide_vars(binding(Names0, Skel, Subst), binding(Names, Skel, Subst)) :-
    hide_names(Names0, Skel, Subst, Names).

hide_names([], _, _, []).
hide_names([Name|T0], Skel, Subst, T) :-
    (   sub_atom(Name, 0, _, _, '_'),
        current_prolog_flag(toplevel_print_anon, false),
        sub_atom(Name, 1, 1, _, Next),
        char_type(Next, prolog_var_start)
    ->  true
    ;   Subst == [],
        Skel == '$VAR'(Name)
    ),
    !,
    hide_names(T0, Skel, Subst, T).
hide_names([Name|T0], Skel, Subst, [Name|T]) :-
    hide_names(T0, Skel, Subst, T).

self_bounded(binding([Name], Value, [])) :-
    Value == '$VAR'(Name).

%!  get_respons(-Action, +Chp)
%
%   Read the continuation entered by the user.

:- if(current_prolog_flag(emscripten, true)).
get_respons(Action, _Chp) :-
    '$can_yield',
    !,
    await(more, ActionS),
    atom_string(Action, ActionS).
:- endif.
get_respons(Action, Chp) :-
    repeat,
        flush_output(user_output),
        get_single_char(Char),
        answer_respons(Char, Chp, Action),
        (   Action == again
        ->  print_message(query, query(action)),
            fail
        ;   !
        ).

answer_respons(Char, _, again) :-
    '$in_reply'(Char, '?h'),
    !,
    print_message(help, query(help)).
answer_respons(Char, _, redo) :-
    '$in_reply'(Char, ';nrNR \t'),
    !,
    print_message(query, if_tty([ansi(bold, ';', [])])).
answer_respons(Char, _, redo) :-
    '$in_reply'(Char, 'tT'),
    !,
    trace,
    save_debug,
    print_message(query, if_tty([ansi(bold, '; [trace]', [])])).
answer_respons(Char, _, continue) :-
    '$in_reply'(Char, 'ca\n\ryY.'),
    !,
    print_message(query, if_tty([ansi(bold, '.', [])])).
answer_respons(0'b, _, show_again) :- %'
    !,
    break.
answer_respons(0'*, Chp, show_again) :- %'
    !,
    print_last_chpoint(Chp).
answer_respons(Char, _, show_again) :-
    print_predicate(Char, Pred, Options),
    !,
    print_message(query, if_tty(['~w'-[Pred]])),
    set_prolog_flag(answer_write_options, Options).
answer_respons(-1, _, show_again) :-
    !,
    print_message(query, halt('EOF')),
    halt(0).
answer_respons(Char, _, again) :-
    print_message(query, no_action(Char)).

print_predicate(0'w, [write], [ quoted(true),
                                spacing(next_argument)
                              ]).
print_predicate(0'p, [print], [ quoted(true),
                                portray(true),
                                max_depth(10),
                                spacing(next_argument)
                              ]).


print_last_chpoint(Chp) :-
    current_predicate(print_last_choice_point/0),
    !,
    print_last_chpoint_(Chp).
print_last_chpoint(Chp) :-
    use_module(library(prolog_stack), [print_last_choicepoint/2]),
    print_last_chpoint_(Chp).

print_last_chpoint_(Chp) :-
    print_last_choicepoint(Chp, [message_level(information)]).


                 /*******************************
                 *          EXPANSION           *
                 *******************************/

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

call_expand_query(Goal, Expanded, Bindings, ExpandedBindings) :-
    user:expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    !.
call_expand_query(Goal, Expanded, Bindings, ExpandedBindings) :-
    toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    !.
call_expand_query(Goal, Goal, Bindings, Bindings).


:- user:dynamic(expand_answer/2).
:- user:multifile(expand_answer/2).

call_expand_answer(Goal, Expanded) :-
    user:expand_answer(Goal, Expanded),
    !.
call_expand_answer(Goal, Expanded) :-
    toplevel_variables:expand_answer(Goal, Expanded),
    !.
call_expand_answer(Goal, Goal).



/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
/*
:- module('$history',
          [ read_term_with_history/2,           % -Term, +Line
            '$save_history_line'/1,             % +Line
            '$clean_history'/0,
            '$load_history'/0,
            '$save_history_event'/1
          ]).

%!  read_term_with_history(-Term, +Options)
%
%   Read a term guide by Options and  maintain a history similar to most
%   Unix shells.
%
%   When read_history reads a term of   the  form $silent(Goal), it will
%   call Goal and pretend it has not seen anything. This hook is used by
%   the GNU-Emacs interface to for   communication between GNU-EMACS and
%   SWI-Prolog.

read_term_with_history(Term, Options) :-
    '$option'(prompt(Prompt), Options, '~! ?> '),
    '$option'(input(Input), Options, user_input),
    repeat,
        prompt_history(Prompt),
        read_query_line(Input, Raw),
        read_history_(Raw, Term, Options),
    !.
*/
read_history_(Raw, _Term, Options) :-
    '$option'(show(Raw), Options, history),
    list_history,
    !,
    fail.
read_history_(Raw, _Term, Options) :-
    '$option'(help(Raw), Options, '!help'),
    '$option'(show(Show), Options, '!history'),
    print_message(help, history(help(Show, Raw))),
    !,
    fail.
read_history_(Raw, Term, Options) :-
    expand_history(Raw, Expanded, Changed),
    '$save_history_line'(Expanded),
    '$option'(module(Module), Options, Var),
    (   Module == Var
    ->  '$current_typein_module'(Module)
    ;   true
    ),
    (   '$select'(variable_names(Bindings), Options, Options1)
    ->  true
    ;   Options1 = Options,
        i(Bindings)                     % ignore
    ),
    catch(read_term_from_atom(Expanded, Term0,
                              [ module(Module),
                                variable_names(Bindings0)
                              | Options1
                              ]),
          E,
          (   print_message(error, E),
              fail
          )),
    (   var(Term0)
    ->  Term = Term0,
        Bindings = Bindings0
    ;   Term0 = '$silent'(Goal)
    ->  user:ignore(Goal),
        read_term_with_history(Term, Options)
    ;   save_event(Expanded, Options),
        (   Changed == true
        ->  print_message(query, history(expanded(Expanded)))
        ;   true
        ),
        Term = Term0,
        Bindings = Bindings0
    ).

i(_).

%   list_history
%   Write history events to the current output stream.

list_history :-
    (   '$history'(Last, _)
    ->  true
    ;   Last = 0
    ),
    history_depth_(Depth),
    plus(First, Depth, Last),
    findall(Nr/Event,
            (   between(First, Last, Nr),
                '$history'(Nr, Event)
            ),
            Events),
    print_message(query, history(history(Events))).

'$clean_history' :-
    retractall('$history'(_,_)).

%!  '$load_history' is det.
%
%   Load persistent history using a hook

'$load_history' :-
    '$clean_history',
    current_prolog_flag(history, Depth),
    Depth > 0,
    catch(prolog:history(current_input, load), _, true), !.
'$load_history'.


%%   prompt_history(+Prompt)
%
%    Give prompt, substituting '~!' by the event number.

prompt_history('') :-
    !,
    ttyflush.
prompt_history(Prompt) :-
    (   '$history'(Last, _)
    ->  This is Last + 1
    ;   This = 1
    ),
    atom_codes(Prompt, SP),
    atom_codes(This, ST),
    (   atom_codes('~!', Repl),
        substitute(Repl, ST, SP, String)
    ->  prompt1(String)
    ;   prompt1(Prompt)
    ),
    ttyflush.

%   substitute(+Old, +New, +String, -Substituted)
%   substitute first occurence of Old in String by New

substitute(Old, New, String, Substituted) :-
    '$append'(Head, OldAndTail, String),
    '$append'(Old, Tail, OldAndTail),
    !,
    '$append'(Head, New, HeadAndNew),
    '$append'(HeadAndNew, Tail, Substituted),
    !.

%!  '$save_history_line'(+Line)
%
%   Add Line to the command line editing history.

:- multifile
    prolog:history_line/2.

'$save_history_line'(end_of_file) :- !.
'$save_history_line'(Line) :-
    format(string(CompleteLine), '~W~W',
           [ Line, [partial(true)],
             '.',  [partial(true)]
           ]),
    catch(prolog:history(user_input, add(CompleteLine)), _, fail),
    !.
'$save_history_line'(_).

%!  save_event(+Event, +Options)
%
%   Save Event into the  history  system unless it appears in the
%   option `no_save`.

save_event(Event, Options) :-
    '$option'(no_save(Dont), Options),
    memberchk(Event, Dont),
    !.
save_event(Event, _) :-
    '$save_history_event'(Event).

%!  '$save_history_event'(+Event) is det.
%
%   Save an input line as text into the !- based history. Event is one
%   of
%
%     * a *string*.  The event is added with a next number at the end.
%     * a *pair*.  The event is added with the given sequence number.

:- thread_local
    '$history'/2.

'$save_history_event'(Num-String) :-
    integer(Num), string(String),
    !,
    asserta('$history'(Num, String)),
    truncate_history(Num).
'$save_history_event'(Event) :-
    to_string(Event, Event1),
    !,
    last_event(Num, String),
    (   Event1 == String
    ->  true
    ;   New is Num + 1,
        asserta('$history'(New, Event1)),
        truncate_history(New)
    ).
'$save_history_event'(Event) :-
    '$type_error'(history_event, Event).

last_event(Num, String) :-
    '$history'(Num, String),
    !.
last_event(0, "").

to_string(String, String) :-
    string(String),
    !.
to_string(Atom, String) :-
    atom_string(Atom, String).

truncate_history(New) :-
    history_depth_(Depth),
    remove_history(New, Depth).

remove_history(New, Depth) :-
    New - Depth =< 0,
    !.
remove_history(New, Depth) :-
    Remove is New - Depth,
    retract('$history'(Remove, _)),
    !.
remove_history(_, _).

%    history_depth_(-Depth)
%    Define the depth to which to keep the history.

history_depth_(N) :-
    current_prolog_flag(history, N),
    integer(N),
    N > 0,
    !.
history_depth_(25).

%    expand_history(+Raw, -Expanded)
%    Expand Raw using the available history list. Expandations performed
%    are:
%
%       !match          % Last event starting <match>
%       !n              % Event nr. <n>
%       !!              % last event
%
%    Note: the first character after a '!' should be a letter or number to
%    avoid problems with the cut.

expand_history(Raw, Expanded, Changed) :-
    atom_chars(Raw, RawString),
    expand_history2(RawString, ExpandedString, Changed),
    atom_chars(Expanded, ExpandedString),
    !.

expand_history2([!], [!], false) :- !.
expand_history2([!, C|Rest], [!|Expanded], Changed) :-
    not_event_char(C),
    !,
    expand_history2([C|Rest], Expanded, Changed).
expand_history2([!|Rest], Expanded, true) :-
    !,
    match_event(Rest, Event, NewRest),
    '$append'(Event, RestExpanded, Expanded),
    !,
    expand_history2(NewRest, RestExpanded, _).
expand_history2(['\''|In], ['\''|Out], Changed) :-
    !,
    skip_quoted(In, '\'', Out, Tin, Tout),
    expand_history2(Tin, Tout, Changed).
expand_history2(['"'|In], ['"'|Out], Changed) :-
    !,
    skip_quoted(In, '"', Out, Tin, Tout),
    expand_history2(Tin, Tout, Changed).
expand_history2([H|T], [H|R], Changed) :-
    !,
    expand_history2(T, R, Changed).
expand_history2([], [], false).

skip_quoted([Q|T],Q,[Q|R], T, R) :- !.
skip_quoted([\,Q|T0],Q,[\,Q|T], In, Out) :-
    !,
    skip_quoted(T0, Q, T, In, Out).
skip_quoted([Q,Q|T0],Q,[Q,Q|T], In, Out) :-
    !,
    skip_quoted(T0, Q, T, In, Out).
skip_quoted([C|T0],Q,[C|T], In, Out) :-
    !,
    skip_quoted(T0, Q, T, In, Out).
skip_quoted([], _, [], [], []).

%   get_last_event(-String)
%   return last event typed as a string

get_last_event(Event) :-
    '$history'(_, Atom),
    atom_chars(Atom, Event),
    !.
get_last_event(_) :-
    print_message(query, history(no_event)),
    fail.

%   match_event(+Spec, -Event, -Rest)
%   Use Spec as a specification of and event and return the event as Event
%   and what is left of Spec as Rest.

match_event(Spec, Event, Rest) :-
    find_event(Spec, Event, Rest),
    !.
match_event(_, _, _) :-
    print_message(query, history(no_event)),
    fail.

not_event_char(C) :- code_type(C, csym), !, fail.
not_event_char(!) :- !, fail.
not_event_char(_).

find_event([!|Left], Event, Left) :-
    !,
    get_last_event(Event).
find_event([N|Rest], Event, Left) :-
    code_type(N, digit),
    !,
    take_number([N|Rest], String, Left),
    number_codes(Number, String),
    '$history'(Number, Atom),
    atom_chars(Atom, Event).
find_event(Spec, Event, Left) :-
    take_string(Spec, String, Left),
    matching_event(String, Event).

take_string([C|Rest], [C|String], Left) :-
    code_type(C, csym),
    !,
    take_string(Rest, String, Left).
take_string([C|Rest], [], [C|Rest]) :- !.
take_string([], [], []).

take_number([C|Rest], [C|String], Left) :-
    code_type(C, digit),
    !,
    take_string(Rest, String, Left).
take_number([C|Rest], [], [C|Rest]) :- !.
take_number([], [], []).

%   matching_event(+String, -Event)
%
%   Return first event with prefix String as a Prolog string.

matching_event(String, Event) :-
    '$history'(_, AtomEvent),
    atom_chars(AtomEvent, Event),
    '$append'(String, _, Event),
    !.


