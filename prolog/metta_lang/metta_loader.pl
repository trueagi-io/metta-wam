/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *     MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *     optimizing and transforming function/logic programs. It handles different
 *     logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *    https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *      file in the repository.
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


%*********************************************************************************************
% PROGRAM FUNCTION: handles loading, parsing, and processing MeTTa files, including functions
% for reading s-expressions, managing file buffers, and converting between different data representations.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

%!  when_tracing(+Goal) is det.
%
%   Executes the given Goal if tracing is currently enabled.
%
%   This predicate checks if tracing is enabled. If tracing is active, it
%   temporarily disables tracing to execute the Goal, ensuring that the Goal
%   runs without generating trace output. If tracing is not active, the predicate
%   simply succeeds without executing the Goal.
%
%   @arg Goal The Prolog goal to be executed conditionally based on the tracing status.
%
%   @example
%     % Assume tracing is active and we want to run a goal without trace output.
%     ?- trace, when_tracing(writeln('This runs without trace output')).
%     % Trace is turned off temporarily, executes the goal, then restores tracing.
%
when_tracing(Goal) :-
    % Check if tracing is active
    tracing,
    % Cut to prevent further execution if tracing is not active
    !,
    % Temporarily disable tracing, run the Goal without tracing output
    notrace(Goal),
    % Cut to avoid backtracking into the next clause
    !.
% If tracing is not active, do nothing and succeed without executing Goal.
when_tracing(_).

% The 'multifile' predicate allows other files to add clauses
:- multifile(user:asserted_metta_pred/2).
%  The 'dynamic' predicate allows the predicate to be added, removed, or modified during execution
:- dynamic(user:asserted_metta_pred/2).

%!  exists_virtually(+Library) is det.
%
%   Declares the virtual existence of a specified library.
%
%   @arg Library The name of the library that is considered to exist virtually.
%
exists_virtually(corelib).
exists_virtually(stdlib).

%!  path_chars(+A, -C) is det.
%
%   Maps the symbolic characters of `A` to `C`.
%
%   This predicate relates a symbolic representation in `A` to its character form
%   in `C`. It delegates this functionality to `symbol_chars/2`, which should
%   perform the actual conversion.
%
%   @arg A The symbolic representation to be converted.
%   @arg C The resulting list of characters.
%
path_chars(A, C) :- symbol_chars(A, C).

%!  with_wild_path(+Fnicate, +Dir) is det.
%
%   Sets up a wild card path environment in the given directory `Dir`.
%
%   This predicate sets up the current directory with wild card processing by
%   referring to `Fnicate` and the directory `Dir`. It retrieves the current
%   working directory `PWD` and then invokes `wwp/2` with `Fnicate` and `Dir` as
%   arguments. `wwp/2` is assumed to handle the wild card path processing.
%
%   @arg Fnicate A function or object associated with wild path setup.
%   @arg Dir     The directory where the wild path setup is to be applied.
%
%   @example
%     % Apply wild path setup for a specific directory.
%     ?- with_wild_path(my_fnicate, '/home/user/docs').
%
with_wild_path(Fnicate, Dir) :-
    % Retrieve the current working directory.
    working_directory(PWD, PWD),
    % Apply the wild path setup.
    wwp(Fnicate, Dir).

%!  inner_compound(+Compound, -Outer, -Inner) is det.
%
%   Traverses the innermost compound term, producing `Inner` as the deepest nested term.
%
%   This predicate recursively navigates the structure of a compound term `Compound`
%   until reaching an atomic term. It constructs `Outer` by maintaining the
%   structure around `Inner`. If `Inner` is not compound, it is returned directly.
%
%   @arg Compound The compound term to traverse.
%   @arg Outer    The structure containing `Inner`.
%   @arg Inner    The innermost atomic or non-compound term.
%
%   @example
%     % Decompose nested compound structures.
%     ?- inner_compound(f(g(h, i)), Outer, Inner).
%     Outer = f(g(Midder)),
%     Inner = h.
%
inner_compound(Inner, '.', Inner) :-
    % If Inner is not compound, return it as Inner.
    \+ compound(Inner), !.
inner_compound(Cmpd, Outter, Inner) :-
    % Decompose Cmpd into functor F and arguments [X|Args]
    compound_name_arguments(Cmpd, F, [X|Args]),
    % Recompose Outer with the functor F and [Midder|Args] as arguments
    compound_name_arguments(Outter, F, [Midder|Args]),
    % Recursively find the innermost term.
    inner_compound(X, Midder, Inner).

%!  afn(+A, -B) is det.
%
%   Resolves the absolute file name for `A`, yielding `B` as the resolved path.
%
%   This predicate quietly resolves the absolute file name of `A`, unifying the
%   result with `B`.
%
%   @arg A The file name or path to resolve.
%   @arg B The resolved absolute path.
%
%   @example
%     % Resolve the absolute path of a relative file name.
%     ?- afn('file.txt', AbsPath).
%     AbsPath = '/home/user/file.txt'.
%
afn(A, B) :- quietly(absolute_file_name(A, B)).

%!  afn(+A, -B, +Options) is det.
%
%   Resolves the absolute file name for `A` with specific options, unifying with `B`.
%
%   This variant of `afn/2` allows additional options for file name resolution,
%   which are passed to `absolute_file_name/3`.
%
%   @arg A       The file name or path to resolve.
%   @arg B       The resolved absolute path.
%   @arg Options The options list for customizing the resolution.
%
%   @example
%     % Resolve a file path with specific options.
%     ?- afn('file.txt', AbsPath, [access(read)]).
%     AbsPath = '/home/user/file.txt'.
%
afn(A, B, C) :- quietly(absolute_file_name(A, B, C)).

%!  wwp(+Fnicate, +Path) is det.
%
%   Processes a file or directory path with a given predicate `Fnicate`.
%
%   The `wwp/2` predicate is a versatile file and directory handler that operates
%   on paths of various types, including files, directories, symbolic paths, and
%   compound terms. It allows processing of virtual paths, lists of paths, and
%   compound structures. The predicate applies the function `Fnicate` on each
%   processed file or directory found.
%
%   @arg Fnicate The predicate to apply to each processed path or file.
%   @arg Path    The file, directory, symbolic path, or compound structure to process.
%
%   @example
%     % Apply a function to each file in a directory or file list.
%     ?- wwp(my_process_fnicate, '/path/to/directory').
%
wwp(Fnicate,Dir):-
    extreme_debug(fbug(wwp(Fnicate,Dir))),
    fail.
wwp(Fnicate, File) :- atom(File),atom_concat('file://',Path,File),!, wwp(Fnicate, Path).
wwp(_Fnicate, []) :-
    % If the path is an empty list, succeed without further processing.
    !.
wwp(_Fnicate, Virtual) :-
    % If the path exists virtually, succeed immediately.
    exists_virtually(Virtual), !.
wwp(Fnicate, Virtual) :-
    % If the path is unbound, throw an error to handle uninitialized input.
    var(Virtual), !, throw(var_wwp(Fnicate, Virtual)).
wwp(Fnicate, Dir) :-
    % If running on Scryer Prolog and the path is a symbol, convert it to a character list
    % and reapply wwp on the resulting list.
    is_scryer, symbol(Dir), !, must_det_ll((path_chars(Dir, Chars), wwp(Fnicate, Chars))).
wwp(Fnicate, Chars) :-
    % If the path is a character list or code list, convert it to a file name and reapply wwp.
    is_list(Chars), catch(name(File, Chars), _, fail), Chars \== File, !, wwp(Fnicate, File).
wwp(Fnicate, File) :-
    % If the path is a list of files, apply wwp to each element in the list.
    is_list(File), !, must_det_ll((maplist(wwp(Fnicate), File))).
wwp(Fnicate, Cmpd) :-
    % If the path is a compound term, find the innermost term and process it within the outer structure.
    compound(Cmpd), inner_compound(Cmpd, Outter, Inner), !,
    % Find absolute path of the outer compound and apply wwp with Inner inside that directory.
    afn(Outter, Dir, [solutions(all), access(read), file_errors(fail)]),
    with_cwd(Dir, wwp(Fnicate, Inner)), !.
wwp(Fnicate, Chars) :-
    % If running on SWI-Prolog, convert character list to an atom and process it.
    \+ is_scryer, \+ symbol(Chars), !, must_det_ll((name(Atom, Chars), wwp(Fnicate, Atom))).
wwp(Fnicate, File) :-
    % If the path exists as a file, directly apply Fnicate to it.
    exists_file(File), !, must_det_ll((call(Fnicate, File))).
wwp(Fnicate, ColonS) :-
    % Handle symbolic paths containing ':' separators, treating them as modular paths.
    fail, symbolic(ColonS), symbol_contains(ColonS, ':'), !,
    % Split the symbolic path into top-level directory and the remaining path.
    symbolic_list_concat([Top|Rest], ':', ColonS),
    symbolic_list_concat(Rest, ':', FileNext),
    % Log if tracing and attempt to find the directory.
    when_tracing(listing(is_metta_module_path)),
    find_top_dirs(Top, Dir),
    (
        % If FileNext is empty, apply wwp only on Dir, otherwise process within the directory.
        (fail, symbol_length(FileNext, 0))
        -> wwp(Fnicate, Dir)
        ; (exists_directory(Dir)
            -> with_cwd(Dir, wwp(Fnicate, FileNext))
            ; fail)
    ),
    !.
wwp(Fnicate, ColonS) :-
    % If path contains ':' separator, split it for directory processing.
    symbolic(ColonS), symbol_contains(ColonS, ':'), !,
    symbolic_list_concat([Top|Rest], ':', ColonS),
    symbolic_list_concat(Rest, ':', FileNext), !,
    when_tracing(listing(is_metta_module_path)),
    must_det_ll((call((
        quietly(find_top_dirs(Top, Dir)),
        % If Dir exists, process the remaining path within Dir.
        exists_directory(Dir),
        with_cwd(Dir, wwp(Fnicate, FileNext)))))), !.
wwp(Fnicate, File) :-
    % If the path contains '*', expand it to match multiple files.
    symbol_contains(File, '*'),
    expand_file_name(File, List),
    maplist(wwp(Fnicate), List), !.
wwp(Fnicate, Dir) :-
    % If Dir is a directory, check for `__init__.py` file and process if it exists.
    exists_directory(Dir),
    quietly(afn_from('__init__.py', PyFile, [access(read), file_errors(fail), relative_to(Dir)])),
    wwp(Fnicate, PyFile).
wwp(Fnicate, File) :-
    % If File doesnâ€™t exist as file or directory, search for it with predefined extensions.
    \+ exists_directory(File), \+ exists_file(File),
    extension_search_order(Ext),
    symbolic_list_concat([File|Ext], MeTTafile),
    exists_file(MeTTafile),
    call(Fnicate, MeTTafile).
wwp(Fnicate, File) :-
    % For files containing '..', search with alternative extensions and process if found.
    \+ exists_directory(File), \+ exists_file(File), symbol_contains(File, '..'),
    extension_search_order(Ext),
    symbolic_list_concat([File|Ext], MeTTafile0),
    afn_from(MeTTafile0, MeTTafile, [access(read), file_errors(fail)]),
    exists_file(MeTTafile),
    call(Fnicate, MeTTafile).
wwp(Fnicate, File) :-
    % If File is a directory, process all files matching '*.*sv' in that directory.
    exists_directory(File),
    directory_file_path(File, '*.*sv', Wildcard),
    expand_file_name(Wildcard, List), !,
    maplist(Fnicate, List).
wwp(Fnicate, Dir) :-
    % If Dir is a directory, retrieve all files within and apply Fnicate to each.
    exists_directory(Dir), !,
    must_det_ll((directory_files(Dir, Files),
        maplist(directory_file_path(Dir, Files), Paths),
        maplist(path_chars, Paths, CharPaths),
        maplist(wwp(Fnicate), CharPaths))), !.
wwp(Fnicate, File) :-
    % Fallback case: directly apply Fnicate on the file.
    must_det_ll((call(Fnicate, File))).

%!  extension_search_order(-ExtensionList) is det.
%
%   Defines the order in which file extensions are searched.
%
%   Specifies the order of file extensions to use when searching for files.
%
%   @arg ExtensionList A list of file extensions in the preferred search order.
%
%   @example
%     % Retrieve the preferred search order for extensions.
%     ?- extension_search_order(Order).
%     Order = ['.metta'] ;
%     Order = ['.py'] ;
%     Order = [''].
%
extension_search_order(['.metta']).
extension_search_order(['.py']).
extension_search_order(['']).

:- if(\+ current_predicate(load_metta_file/2)).

%!  load_metta_file(+Self, +Filemask) is det.
%
%   Loads a `.metta` file or other supported files based on the file mask.
%
%   Attempts to load the specified `Filemask`. If the `Filemask` has a `.metta`
%   extension, `load_metta/2` is used. Otherwise, `load_flybase/1` is called.
%
%   @arg Self The calling module or context.
%   @arg Filemask The file name or pattern to load.
%
%   @example
%     % Load a file with .metta extension.
%     ?- load_metta_file(module, 'example.metta').
%
load_metta_file(Self, Filemask) :-
    symbol_concat(_, '.metta', Filemask), !,
    % Load the file if it has a .metta extension
    load_metta(Self, Filemask).
load_metta_file(_Slf, Filemask) :-
    % Otherwise, use the flybase loader for the file mask
    load_flybase(Filemask).

:- endif.

%!  afn_from(+RelFilename, -Filename) is det.
%
%   Resolves the absolute filename from a relative filename.
%
%   Finds the absolute path for `RelFilename`, applying defaults as needed.
%
%   @arg RelFilename The relative filename to resolve.
%   @arg Filename The resulting absolute filename.
%
%   @example
%     % Resolve absolute path of a relative file name.
%     ?- afn_from('docs/example.txt', Path).
%
afn_from(RelFilename, Filename) :-
    afn_from(RelFilename, Filename, []).

%!  afn_from(+RelFilename, -Filename, +Opts) is det.
%
%   Resolves an absolute filename from a relative filename with options.
%
%   Supports an option `relative_to/1` to specify a base directory.
%
%   @arg RelFilename The relative filename to resolve.
%   @arg Filename The resulting absolute filename.
%   @arg Opts A list of options for the resolution process.
%
%   @example
%     % Resolve relative to a specified directory.
%     ?- afn_from('docs/example.txt', Path, [relative_to('/home/user')]).
%
afn_from(RelFilename, Filename, Opts) :-
    select(relative_to(RelFrom), Opts, NewOpts),
    afn_from(RelFrom, RelFromNew, NewOpts),
    % Resolve Filename relative to RelFromNew directory
    quietly(afn(RelFilename, Filename, [relative_to(RelFromNew) | NewOpts])).
afn_from(RelFilename, Filename, Opts) :-
    is_metta_module_path(ModPath),
    % Attempt to resolve Filename with ModPath as reference
    quietly(afn(RelFilename, Filename, [relative_to(ModPath) | Opts])).

%!  register_module(+Dir) is det.
%
%   Registers the current module within a directory.
%
%   Registers `Dir` with the current module space.
%
%   @arg Dir The directory to register.
%
%   @example
%     % Register a module in a specific directory.
%     ?- register_module('/path/to/module').
%
register_module(Dir) :-
    current_self(Space),
    % Register the module under the current Space
    register_module(Space, Dir).

%!  register_module(+Space, +Path) is det.
%
%   Registers a module in the specified path.
%
%   Registers `Space` in `Path`, calculating `Dir` and `ModuleName` from the path.
%
%   @arg Space The module space to register.
%   @arg Path The directory or file path of the module.
%
%   @example
%     % Register a specific module space and path.
%     ?- register_module(space, '/path/to/module').
%
register_module(Space, Path) :-
    % Register the top-level path within the Space
    register_module(Space, '%top%', Path),
    file_directory_name(Path, Dir),
    file_base_name(Path, ModuleName),
    % Register the specific ModuleName in the derived Dir
    register_module(Space, ModuleName, Dir).

%!  register_module(+Space, +ModuleName, +Dir) is det.
%
%   Registers a module by name within a space and directory.
%
%   Registers `ModuleName` under `Space` in the specified `Dir`.
%
%   @arg Space The module space.
%   @arg ModuleName The name of the module to register.
%   @arg Dir The directory of the module.
%
%   @example
%     % Register a module by name in a directory.
%     ?- register_module(space, 'mod_name', '/path/to/module').
%
register_module(Space, ModuleName, Dir) :-
    space_name(Space, SpaceName),
    % Find the absolute path of Dir before asserting
    absolute_dir(Dir, AbsDir),
    asserta(is_metta_module_path(SpaceName, ModuleName, AbsDir)).

%!  find_top_dirs(+Top, -Dir) is det.
%
%   Finds the top-level directory for a given identifier.
%
%   @arg Top The top-level identifier.
%   @arg Dir The directory corresponding to the identifier.
%
%   @example
%     % Find the directory for a top-level identifier.
%     ?- find_top_dirs('top_id', Dir).
%
find_top_dirs(Top, Dir) :-
    current_self(Self),
    space_name(Self, SpaceName),
    % Use the space name to locate the directory
    find_top_dirs(SpaceName, Top, Dir).

%!  find_top_dirs(+SpaceName, +Top, -Dir) is det.
%
%   Finds or asserts the directory associated with `Top` within a `SpaceName`.
%
%   @arg SpaceName The name of the space.
%   @arg Top The top-level identifier.
%   @arg Dir The corresponding directory.
%
%   @example
%     % Find or assert directory in a specific space.
%     ?- find_top_dirs('space', 'top_id', Dir).
%
find_top_dirs(SpaceName, Top, Abs) :-
    % Try to locate the absolute path for Top in SpaceName
    is_metta_module_path(SpaceName, Top, Abs).
find_top_dirs(SpaceName,Top,Dir):-
     % Search within the top level of SpaceName's root
    is_metta_module_path(SpaceName,'%top%',Root),absolute_dir(Top,Root,Dir).
find_top_dirs(SpaceName,Top,Dir):-
    % If not found, derive the parent directory
    working_directory(PWD,PWD),
    parent_dir_of(PWD,Top,Dir), assert(is_metta_module_path(SpaceName,Top,Dir)).

%!  parent_dir_of(+PWD, +Top, -Dir) is det.
%
%   Finds the parent directory matching `Top`.
%
%   @arg PWD The current working directory.
%   @arg Top The top-level directory identifier.
%   @arg Dir The matching parent directory.
%
%   @example
%     % Find the parent directory containing the identifier `Top`.
%     ?- parent_dir_of('/current/dir', 'top', Dir).
%
parent_dir_of(PWD, _Top, _Dir) :- PWD == '/',!, fail.
parent_dir_of(PWD, Top, Dir) :-
    directory_file_path(Parent, TTop, PWD),
    % Check if the current top matches
    (TTop == Top -> Dir = PWD ; parent_dir_of(Parent, Top, Dir)).

%!  space_name(+Space, -SpaceName) is det.
%
%   Retrieves the name associated with a space.
%
%   Resolves `SpaceName` from `Space` using symbolic representations or existing names.
%
%   @arg Space The initial space identifier.
%   @arg SpaceName The resolved name for the space.
%
%   @example
%     % Find the name of a space.
%     ?- space_name('space_identifier', Name).
%
space_name(Space, SpaceName) :-
    symbol(Space), !,
    % If Space is symbolic, use it directly as SpaceName
    SpaceName = Space, !.
space_name(Space, SpaceName) :-
    % Check if an existing space name matches
    is_space_name(SpaceName), same_space(SpaceName, Space), !.
space_name(Space, SpaceName) :-
    % Retrieve or create a space-symbol if needed
    'get-atoms'(Space, ['space-symbol', SpaceName]), !.

%!  same_space(+Space1, +Space2) is nondet.
%
%   Checks if `Space1` and `Space2` represent the same space.
%
%   Uses symbolic equality or evaluation to determine if the spaces match.
%
%   @arg Space1 The first space to compare.
%   @arg Space2 The second space to compare.
%
%   @example
%     % Check if two spaces are the same.
%     ?- same_space('space1', 'space2').
%
same_space(Space1, Space2) :-
    % Direct equality check for spaces
    Space1 = Space2.
same_space(SpaceName1, Space2) :-
    % Evaluate symbolic name and compare
    symbol(SpaceName1),
    eval(SpaceName1, Space1),
    !,
    same_space(Space2, Space1).

%!  absolute_dir(+Dir, -AbsDir) is det.
%
%   Resolves the absolute path of a directory.
%
%   Converts `Dir` to an absolute directory path.
%
%   @arg Dir The directory to resolve.
%   @arg AbsDir The absolute path of the directory.
%
%   @example
%     % Resolve the absolute path of a directory.
%     ?- absolute_dir('my_dir', AbsPath).
%
absolute_dir(Dir, AbsDir) :-
    % Generate the absolute path with access and error options
    afn(Dir, AbsDir, [access(read), file_errors(fail), file_type(directory)]).

%!  absolute_dir(+Dir, +From, -AbsDir) is det.
%
%   Resolves the absolute path of a directory relative to a base directory.
%
%   Finds `AbsDir` from `Dir` with reference to `From`.
%
%   @arg Dir The target directory to resolve.
%   @arg From The base directory.
%   @arg AbsDir The resulting absolute directory path.
%
%   @example
%     % Find absolute path of `Dir` relative to a base directory `From`.
%     ?- absolute_dir('my_dir', '/base/dir', AbsPath).
%
absolute_dir(Dir, From, AbsDir) :-
    % Specify From as the base for the absolute path resolution
    afn(Dir, AbsDir, [relative_to(From), access(read), file_errors(fail), file_type(directory)]), !.

:- dynamic(is_metta_module_path/3).
:- dynamic(is_metta_module_path/1).

%!  is_metta_module_path(-Path) is det.
%
%   Represents paths associated with Metta modules.
%
%   Declares a fact to indicate a path is part of a Metta module.
%
%   @arg Path The module path.
%
%   @example
%     % Assert or check if a path is a metta module path.
%     ?- is_metta_module_path(Path).
%
is_metta_module_path('.').

%!  when_circular(+Key, :Goal, +Item, :DoThis) is nondet.
%
%   Executes a goal while preventing circular dependencies by tracking processed items.
%   If the current item (e.g., a file or goal) is already in the list of currently processed
%   items (tracked by Key), it executes the provided DoThis action and fails. Otherwise, it
%   proceeds with the goal, ensuring that the item is properly tracked. The item is automatically
%   removed from the tracking list after execution, using `setup_call_cleanup/3` for cleanup.
%
%   @arg Key   The name of the non-backtrackable global variable to track circular dependencies.
%   @arg Goal  The goal to execute if no circular dependencies are detected.
%   @arg Item  The item being processed (e.g., a file name or goal).
%   @arg DoThis The action to take when a circular dependency is detected (e.g., throwing an error).
%
when_circular(Key, Goal, Item, DoThis) :-
    % Retrieve the current list of items being processed from the global variable (if it exists).
    (nb_current(Key, CurrentItems) -> true; CurrentItems = []),
    % Check if the current item is already in the list of processed items, indicating a circular dependency.
    (   member(Item, CurrentItems)
    ->  % If a circular dependency is detected, execute the DoThis action (e.g., throw an error).
        call(DoThis)
    ;   % Otherwise, proceed with setup_call_cleanup to track and cleanup the processing list.
        setup_call_cleanup(
            % Setup: Add the current item to the list of processed items.
            nb_setval(Key, [Item | CurrentItems]),
            % Call the main goal to be executed.
            call(Goal),
            % Cleanup: Remove the current item from the processed list after the goal completes.
            (nb_current(Key, UpdatedItems),
    select(Item, UpdatedItems, RemainingItems),
    nb_setval(Key, RemainingItems))
        )
    ).

%!  without_circular_error(:Goal, +Error) is det.
%
%   Executes a goal while avoiding circular dependencies. If a circular dependency
%   is detected, an error is thrown.
%
%   This predicate provides a safety check when executing a goal, ensuring that if
%   the goal leads to a circular dependency, the specified error will be thrown instead
%   of entering an infinite loop or recursion.
%
%   @arg Goal  The goal to execute if no circular dependencies are detected.
%   @arg Error The error term to throw in case of circular dependency.
%
%   @example
%     % Attempt to execute a goal that might have circular dependencies.
%     ?- without_circular_error(my_goal, circular_dependency_detected).
%     % If `my_goal` involves a circular dependency, an error will be raised.
%
without_circular_error(Goal, Error) :-
    % Use when_circular/4 to check for circular dependencies and throw an error when detected.
    when_circular('$circular_goals', Goal, Goal, throw(error(Error, _))).

%!  load_metta(+Filename) is det.
%
%   Loads a MeTTa file with the specified filename in the current context.
%
%   This predicate loads a MeTTa file specified by the Filename parameter into the
%   current context, here represented by `&self`. It is assumed that the loaded file
%   contains constructs or expressions in the MeTTa language, which this program
%   interprets or transpiles into Prolog.
%
%   @arg Filename The name of the MeTTa file to load, specified as an atom or string.
%
%   @example
%     % Load a MeTTa language file named "example.metta".
%     ?- load_metta('example.metta').
%
load_metta(Filename):-
    % Call load_metta with the context `&self` and the provided Filename.
    load_metta('&self', Filename).

%!  load_metta(+Self, +Filename) is det.
%
%   Loads a Metta file and handles circular dependencies.
%   The predicate checks if the Filename is already in the list of currently
%   loaded files (to avoid circular loads). If it is, an error is thrown.
%   If not, it adds the Filename to the list, proceeds with the load, and
%   finally removes the Filename after the load is complete.
%
%   @arg Self The current module or context performing the load.
%   @arg Filename The name of the file to be loaded.
%
%   @throws An error if the file is already in the list of currently loaded files.
%
load_metta(Self, Module) :- maybe_into_top_self(Self, TopSelf), !, load_metta(TopSelf, Module).
load_metta(_Self, Filename):-
    % Special case: if the Filename is '--repl', start the REPL instead of loading a file.
    Filename == '--repl', !, repl.
load_metta(Self, Filename):-
    % Call without_circular_error/2 to prevent circular dependencies when loading files.
    without_circular_error(load_metta1(Self, Filename),
        missing_exception(load_metta(Self, Filename))).

%!  load_metta1(+Self, +Filename) is det.
%
%   Helper predicate that performs the actual MeTTa file loading process.
%
%   This predicate checks if the `Filename` is a valid symbol and if the file exists.
%   If either condition fails, it attempts to handle the `Filename` as a wildcard path,
%   which could match multiple files. Once a valid file is identified, it loads the file
%   in the specified context and tracks its loading status.
%
%   @arg Self The module or context performing the load.
%   @arg Filename The name or path of the file to load, potentially including wildcards.
%
%   @example
%     % Attempt to load a file named "example.metta".
%     ?- load_metta1('&self', 'example.metta').
%
load_metta1(Self, Module) :- maybe_into_top_self(Self, TopSelf), !, load_metta1(TopSelf, Module).
load_metta1(Self, Filename):-
    % Check if the Filename is not a valid symbol or does not exist as a file.
    (\+ symbol(Filename); \+ exists_file(Filename)),!,
    % Use with_wild_path to handle wildcard paths and load the file if matched.
    with_wild_path(load_metta(Self), Filename), !,
    % Call loonit_report for logging or tracking purposes.
    loonit_report.
load_metta1(Self, RelFilename):-
    % Ensure that RelFilename is valid and exists as a file.
    must_det_ll((symbol(RelFilename),  % @TODO: Check if it can also be a string.
    exists_file(RelFilename),!,
    % Convert the relative filename to an absolute filename.
    afn_from(RelFilename, Filename),
    % Use a local flag for garbage collection and track file loading.
    %  locally(set_prolog_flag(gc_never, true),...)
    track_load_into_file(Filename,
        % Include the file in the current context/module.
        include_metta(Self, RelFilename)))).

%!  import_metta(+Self, +Filename) is det.
%
%   Imports a Metta file and handles circular dependencies.
%   The predicate checks if the Filename is already in the list of currently
%   imported files (to avoid circular imports). If it is, an error is thrown.
%   If not, it adds the Filename to the list, proceeds with the import, and
%   finally removes the Filename after the import is complete.
%
%   @arg Self The current module or context performing the import.
%   @arg Filename The name of the file to be imported.
%
%   @throws An error if the file is already in the list of currently imported files.
%
import_metta(Self, Module) :- maybe_into_top_self(Self, TopSelf), !, import_metta(TopSelf, Module).
import_metta(Self, Filename):-
    % Define the goal for importing the Metta file.
    About = import_metta1(Self, Filename),
    % Use when_circular/4 to handle circular dependencies during imports.
    when_circular('$circular_goals', About, About, complain_if_missing(Filename, About)).

%!  complain_if_missing(+Filename, +About) is det.
%
%   Reports an error if a specified file is either missing or involved in a circular dependency.
%
%   This predicate checks if the `Filename` exists. If it does not, it writes an error message
%   indicating that the file is missing. If the file exists but a circular dependency is detected,
%   it writes a circular dependency error message.
%
%   @arg Filename The name of the file being checked for existence and circular dependency.
%   @arg About    A term describing the context or operation related to the file (e.g., which
%        part of the program or module requires this file).
%
%   @example
%     % Attempt to load a file and report if it is missing or circular.
%     ?- complain_if_missing('example.metta', load_metta).
%
complain_if_missing(Filename, About):-
    % If the file does not exist, print a missing exception message.
    \+ exists_file(Filename), !, write_src_nl(missing_exception(About)).
complain_if_missing(_, About):-
    % If a circular dependency is found, print a circular exception message.
    write_src_nl(circular_exception(About)).

%!  import_metta1(+Self, +Module) is det.
%
%   Imports a MeTTa file or module into the current Prolog context.
%   This predicate performs the actual import process for a MeTTa file or module, extending
%   the current Prolog environment with MeTTa constructs or, in some cases, Python modules.
%
%   @arg Self   The identifier for the current Prolog context or module performing the import.
%   @arg Module The name of the MeTTa file or Python module to import, either as a file path
%      or a module name.
%
%   @example
%     % Import a MeTTa file named "example.metta" within the `&self` context.
%     ?- import_metta1('&self', 'example.metta').
%
%     % Import a Python module named "example_py_module" into the Prolog environment.
%     ?- import_metta1('&self', 'example_py_module').
%
import_metta1(Self, Module) :- maybe_into_top_self(Self, TopSelf), !, import_metta1(TopSelf, Module).
import_metta1(Self, Module):-
    % If the Module is a valid Python module, extend the current Prolog context with Python.
    current_predicate(py_is_module/1), py_is_module(Module),!,
    must_det_ll(self_extend_py(Self, Module)),!.
import_metta1(Self, Filename):-
    % If Filename is not a valid symbol or file does not exist, use wildcards for import.
    (\+ symbol(Filename); \+ exists_file(Filename)),!,
    must_det_ll(with_wild_path(import_metta(Self), Filename)),!.
import_metta1(Self, RelFilename):-
    % Ensure that RelFilename is a symbol and exists as a file.
    must_det_ll((
    symbol(RelFilename),
    exists_file(RelFilename),
    % Convert the relative filename to an absolute path.
    absolute_file_name(RelFilename, Filename),
    % Extract the directory path from the filename.
    directory_file_path(Directory, _, Filename),
    % Register the file in Prolog as being part of the MeTTa context.
    pfcAdd_Now(metta_file(Self, Filename, Directory)),
    % Suspend Prolog answers during the inclusion of the MeTTa file.
    locally(nb_setval(suspend_answers, true),
    % Include the file and load its content into the specified directory.
    include_metta_directory_file(Self, Directory, Filename)))).

% Ensure Metta persistency and parsing functionalities are loaded.
:- ensure_loaded(metta_persists).
:- ensure_loaded(metta_parser).

%!  include_metta(+Self, +Filename) is det.
%
%   Includes a Metta file and handles circular dependencies.
%   The predicate checks if the Filename is already in the list of currently
%   included files (to avoid circular includes). If it is, an error is thrown.
%   If not, it adds the Filename to the list, proceeds with the include, and
%   finally removes the Filename after the include is complete.
%
%   @arg Self The current module or context performing the include.
%   @arg Filename The name of the file to be included.
%
%   @throws An error if the file is already in the list of currently included files.
%
include_metta(Self, Filename) :- maybe_into_top_self(Self, TopSelf), !, include_metta(TopSelf, Filename).
include_metta(Self, Filename):-
    % Use without_circular_error/2 to handle circular dependencies for including files.
    without_circular_error(include_metta1(Self, Filename),
        missing_exception(include_metta(Self, Filename))).

%!  include_metta1(+Self, +Filename) is det.
%
%   Helper predicate that performs the actual inclusion of a MeTTa file.
%
%   This predicate checks if `Filename` is a valid symbol and if the file exists.
%   If not, it handles the filename as a wildcard path for potential matching files.
%   Once validated, it converts `RelFilename` to an absolute path, generates a
%   temporary file if needed, extracts the directory path, and registers the file
%   in the Prolog knowledge base as part of the current context.
%
%   @arg Self The module or context in which the file is being included.
%   @arg Filename The name or path of the file to include.
%
%   @example
%     % Include a valid file "example.metta" into the current knowledge base context.
%     ?- include_metta1('&self', 'example.metta').
%
include_metta1(Self, Filename) :- maybe_into_top_self(Self, TopSelf), !, include_metta1(TopSelf, Filename).
include_metta1(Self, Filename):-
    % If Filename is not a valid symbol or file does not exist, handle wildcards for includes.
    (\+ symbol(Filename); \+ exists_file(Filename)),!,
    must_det_ll(with_wild_path(include_metta(Self), Filename)),!.

include_metta1(Self, RelFilename):-
    % Ensure RelFilename is a valid symbol and exists as a file.
    must_det_ll((
    symbol(RelFilename),
    exists_file(RelFilename),!,
    % Convert the relative filename to an absolute path.
    afn_from(RelFilename, Filename),
    % Generate a temporary file if necessary, based on the absolute filename.
    gen_tmp_file(false, Filename),
    % Extract the directory path from the filename.
    directory_file_path(Directory, _, Filename),
    % Register the file in Prolog knowledge base as part of the MeTTa context.
    pfcAdd_Now(metta_file(Self, Filename, Directory)),
    % Mark the file as loaded into the current knowledge base.
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    % Include the file's directory content into the current module context.
    include_metta_directory_file(Self, Directory, Filename))),
    % Register the file status in the knowledge base and optionally list it.
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    nop(listing(user:loaded_into_kb/2)).

%!  count_lines_up_to(+TwoK, +Filename, -Count) is det.
%
%   Counts lines in a file up to a specified limit.
%   This predicate opens the specified file `Filename`, counts lines up to the
%   specified limit `TwoK`, and then closes the file. If the file has fewer than
%   `TwoK` lines, the total line count is returned.
%
%   @arg TwoK     The maximum number of lines to count.
%   @arg Filename The name of the file whose lines are being counted.
%   @arg Count    The resulting line count, which will be either the total number of
%        lines in the file or `TwoK`, whichever is smaller.
%
%   @example
%     % Count up to 2000 lines in "example.txt".
%     ?- count_lines_up_to(2000, 'example.txt', Count).
%
count_lines_up_to(TwoK, Filename, Count) :-
  open(Filename, read, Stream, [encoding(utf8)]),
  count_lines_in_stream(TwoK, Stream, 0, Count),
  close(Stream).

%!  count_lines_in_stream(+TwoK, +Stream, +CurrentCount, -FinalCount) is det.
%
%   Counts lines from an open stream up to a specified limit.
%   This helper predicate recursively reads lines from `Stream` and increments the
%   count until it reaches `TwoK` or the end of the file. `FinalCount` is unified
%   with the line count reached.
%
%   @arg TwoK          The maximum number of lines to count.
%   @arg Stream        The open file stream to read from.
%   @arg CurrentCount  The current line count, used for recursion.
%   @arg FinalCount    The resulting line count, limited by `TwoK` or end of file.
%
count_lines_in_stream(TwoK, Stream, CurrentCount, FinalCount) :-
  ( CurrentCount >= TwoK
  -> FinalCount = TwoK
  ;  read_line_to_codes(Stream, Codes),
    ( Codes == end_of_file
    -> FinalCount = CurrentCount
    ;  NewCount is CurrentCount + 1,
        count_lines_in_stream(TwoK, Stream, NewCount, FinalCount)
    )
  ).

%!  include_metta_directory_file_prebuilt(+Self, +Directory, +Filename) is nondet.
%
%   Loads a prebuilt `.qlf` or `.datalog` file if it is available and up-to-date.
%
%   This predicate checks if there is an existing precompiled `.qlf` or `.datalog` file
%   associated with a `.metta` file (`Filename`). If the `.qlf` or `.datalog` file exists
%   and is more recent than the `.metta` file, it is loaded to avoid recompiling. The `.qlf`
%   and `.datalog` files must be newer than the `.metta` file and meet additional conditions
%   before they are loaded.
%
%   @arg Self      The context or module into which the file is being loaded.
%   @arg Directory The directory containing the file.
%   @arg Filename  The name of the original `.metta` file.
%
include_metta_directory_file_prebuilt(Self, _Directory, Filename):-
    % Attempt to load a prebuilt `.qlf` file if it is newer than the `.metta` file.
    symbol_concat(_, '.metta', Filename),
    symbol_concat(Filename, '.qlf', QlfFile),
    exists_file(QlfFile),
    time_file(Filename, MettaTime),
    time_file(QlfFile, QLFTime),
    \+ always_rebuild_temp,
    QLFTime > MettaTime,!, % Ensure QLF file is newer than the METTA file
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    ensure_loaded(QlfFile),!.
include_metta_directory_file_prebuilt(Self, _Directory, Filename):-
    % Attempt to load a `.datalog` file if it is newer, large enough, and `just_load_datalog` is true.
    just_load_datalog,
    symbol_concat(_, '.metta', Filename),
    symbol_concat(Filename, '.datalog', DatalogFile),
    exists_file(DatalogFile),
    time_file(Filename, MettaTime),
    time_file(DatalogFile, DatalogTime),
    DatalogTime > MettaTime,
    \+ always_rebuild_temp, !,
    size_file(Filename, MettaSize),
    size_file(DatalogFile, DatalogSize),
    % Ensure the Datalog file is at least 25% the size of the METTA file.
    DatalogSize >= 0.25 * MettaSize,
    % always rebuild
    delete_file(DatalogFile), fail,
    !, % Cut to prevent backtracking
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    ensure_loaded(DatalogFile), !.
include_metta_directory_file_prebuilt(Self, _Directory, Filename):-
    % Convert a `.datalog` file to `.qlf` if the size requirement is met and load it.
    symbol_concat(_, '.metta', Filename),
    symbol_concat(Filename, '.datalog', DatalogFile),
    exists_file(DatalogFile),!,
    size_file(Filename, MettaSize),
    size_file(DatalogFile, DatalogSize),
    % Ensure the size of the Datalog file is at least 25% of the METTA file
    DatalogSize >= 0.25 * MettaSize,
    % always rebuild
    delete_file(DatalogFile), fail,
    !, % Cut to prevent backtracking
    convert_datalog_to_loadable(DatalogFile, QlfFile),!,
    exists_file(QlfFile),!,
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    ensure_loaded(QlfFile), !.

%!  include_metta_directory_file(+Self, +Directory, +Filename) is det.
%
%   Includes a MeTTa file into the current context, handling large files and prebuilt options.
%
%   This predicate attempts to include a `.metta` file by first checking if a prebuilt version
%   (either `.qlf` or `.datalog`) exists and is up-to-date. If a prebuilt version is unavailable,
%   it assesses the fileâ€™s size and uses an alternative loading method if the file is large.
%
%   @arg Self      The context or module in which the file is being included.
%   @arg Directory The directory containing the file.
%   @arg Filename  The name of the `.metta` file to include.
%
%   @example
%     % Include a file named "example.metta" from a specific directory.
%     ?- include_metta_directory_file('&self', '/path/to/directory', 'example.metta').
%
include_metta_directory_file(Self, Directory, Filename) :- maybe_into_top_self(Self, TopSelf), !, include_metta_directory_file(TopSelf, Directory, Filename).
include_metta_directory_file(Self, Directory, Filename):-
    % Attempt to include the file via a prebuilt version if it exists.
    include_metta_directory_file_prebuilt(Self, Directory, Filename), !.
include_metta_directory_file(Self, Directory, Filename):-
    % If file has more than 1980 lines, use optimized loading for large files.
    count_lines_up_to(2000, Filename, Count),
    Count > 1980, % \+ use_fast_buffer,
    include_large_metta_directory_file(Self, Directory, Filename), !.
include_metta_directory_file(Self, Directory, Filename):-
    % Default inclusion by reading from the file stream in the specified directory.
    with_cwd(Directory, must_det_ll(setup_call_cleanup(
        open(Filename, read, In, [encoding(utf8)]),
        must_det_ll(load_metta_file_stream(Filename, Self, In)),
        close(In)))).

% include_large_metta_directory_file(Self, Directory, Filename):- \+ use_fast_buffer, !, locally(nb_setval(may_use_fast_buffer,t), include_metta_directory_file(Self,Directory, Filename)).

%!  include_large_metta_directory_file(+Self, +Directory, +Filename) is nondet.
%
%   Handles the inclusion of large `.metta` files by converting to a loadable format.
%
%   This predicate manages large `.metta` files by converting them to a `.qlf` format (a Prolog
%   quick-load file format) if possible, then loading the resulting file. This approach is more
%   efficient for large files by reducing processing overhead.
%
%   @arg Self      The context or module in which the file is being included.
%   @arg Directory The directory containing the file.
%   @arg Filename  The name of the `.metta` file being converted and included.
%
%   @example
%     % Handle a large file named "example_large.metta" by converting to `.qlf` and loading.
%     ?- include_large_metta_directory_file('&self', '/path/to/directory', 'example_large.metta').
%
include_large_metta_directory_file(Self, _Directory, Filename):-
    % \+ use_fast_buffer, !, locally(nb_setval(may_use_fast_buffer,t), include_metta_directory_file(Self, Directory, Filename)).
    once(convert_metta_to_loadable(Filename, QlfFile)),
    exists_file(QlfFile), !,
    % Register and load the `.qlf` file.
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    ensure_loaded(QlfFile).

%!  convert_metta_to_datalog(+Filename, -DatalogFile) is det.
%
%   Converts a `.metta` file to a `.datalog` file.
%
%   This predicate reads a `.metta` file specified by `Filename`, converts its contents
%   to Datalog format, and writes the output to a `.datalog` file (`DatalogFile`). After
%   conversion, it verifies that the generated `.datalog` file is at least 50% of the size
%   of the original `.metta` file. If this size condition is not met, the `.datalog` file
%   is deleted and the conversion fails.
%
%   @arg Filename     The name of the `.metta` file to be converted.
%   @arg DatalogFile  The name of the resulting `.datalog` file.
%
%   @example
%     % Convert a `.metta` file named "example.metta" to "example.metta.datalog".
%     ?- convert_metta_to_datalog('example.metta', DatalogFile).
%
convert_metta_to_datalog(Filename, DatalogFile):-
    % Generate the Datalog file name
    ignore(symbol_concat(Filename, '.datalog', DatalogFile)),
    % Open the METTA file for reading
    setup_call_cleanup(
        open(Filename, read, Input, [encoding(utf8)]),
        % Open the Datalog file for writing
        setup_call_cleanup(
           open(DatalogFile, write, Output, [encoding(utf8)]),
            % Perform the conversion
    must_det_ll(translate_metta_file_to_datalog_io(Filename, Input, Output)),
            % Cleanup: Close the Datalog file
           close(Output)
        ),
        % Cleanup: Close the METTA file
        close(Input)
    ),
    % Ensure the generated Datalog file is at least 50% the size of the METTA file
    must_det_ll((
       (size_file(Filename, MettaSize),
        size_file(DatalogFile, DatalogSize)),
        (
            DatalogSize >= 0.5 * MettaSize
        ->  true  % If the size condition is met, succeed
        ;   delete_file(DatalogFile), fail  % If not, delete the Datalog file and fail
        )
    )),
    !.  % Prevent backtracking

%!  atom_subst(+Source, +Replacements, -Result) is det.
%
%   Substitutes multiple search-replace pairs within an atom.
%
%   This predicate replaces occurrences of terms in `Source` based on a list of
%   search-replace pairs specified by `Replacements`. Each pair is applied in
%   sequence to generate the final `Result`.
%
%   @arg Source       The original atom in which substitutions will be applied.
%   @arg Replacements A list of `Search-Replace` pairs, each defining a substitution.
%   @arg Result       The atom resulting from all substitutions.
%
%   @example
%     % Substitute "apple" with "orange" and "pear" with "peach" in "apple and pear".
%     ?- atom_subst('apple and pear', ['apple'-'orange', 'pear'-'peach'], Result).
%     Result = 'orange and peach'.
%
atom_subst(Source, Replacements, Result) :-
    % Apply each replacement in the list to Source
    foldl(replace_in_symbol, Replacements, Source, Result).

%!  replace_in_symbol(+SearchReplace, +CurrentSource, -NewSource) is det.
%
%   Applies a single search-replace operation to an atom.
%
%   This helper predicate replaces all occurrences of `Search` with `Replace` in
%   `CurrentSource`, producing `NewSource`.
%
%   @arg SearchReplace  A pair of atoms, where `Search` is the term to replace and `Replace`
%              is the term to substitute in its place.
%   @arg CurrentSource  The atom in which the replacement is applied.
%   @arg NewSource      The atom resulting from the replacement operation.
%
%   @example
%     % Replace "apple" with "orange" in "apple pie".
%     ?- replace_in_symbol('apple'-'orange', 'apple pie', NewSource).
%     NewSource = 'orange pie'.
%
replace_in_symbol(Search-Replace, CurrentSource, NewSource) :-
    symbolic_list_concat(Split, Search, CurrentSource),
    symbolic_list_concat(Split, Replace, NewSource).

%!  filename_to_mangled_pred(+Filename, -MangleP) is det.
%
%   Generates a mangled predicate name from a filename.
%
%   This predicate creates a unique, mangled predicate name (`MangleP`) based on the given
%   `Filename`, current timestamp, and a series of replacements to ensure it conforms to a
%   specific format. Unwanted characters such as slashes, dots, and hyphens are replaced
%   with underscores. The result is trimmed to the last 24 characters.
%
%   @arg Filename The original filename used to generate the mangled predicate name.
%   @arg MangleP  The resulting mangled predicate name.
%
%   @example
%     % Generate a mangled predicate name from "example.metta".
%     ?- filename_to_mangled_pred('example.metta', MangleP).
%     MangleP = 'data_example_metta_<timestamp>'.
%
filename_to_mangled_pred(Filename, MangleP) :-
    % Get the current time as a unique component.
    get_time(Time),
    % Concatenate 'data', Filename, and Time to form the initial mangled name.
    symbolic_list_concat(['data', Filename, Time], '_', GS),
    % Define replacements to sanitize and format the predicate name.
    Replacements = [ '.metta_'- '_',
            '_1710'-'_',
            '/'- '_',
            '/'- '_', '.'- '_', '-'- '_', '__'- '_'],
    % Apply the replacements to the generated name.
    atom_subst(GS, Replacements, IntermediateResult),
    % Trim the result to the last 24 characters for compactness.
    trim_to_last_nchars(24, IntermediateResult, MangleP).

%!  trim_to_last_nchars(+Len, +Atom, -TrimmedAtom) is det.
%
%   Trims an atom to its last `Len` characters.
%
%   This predicate shortens the given `Atom` to the specified `Len` number of characters,
%   producing `TrimmedAtom`. If `Atom` is shorter than or equal to `Len`, it is returned
%   unaltered.
%
%   @arg Len         The maximum number of characters for the resulting atom.
%   @arg Atom        The original atom to be trimmed.
%   @arg TrimmedAtom The resulting atom, trimmed to the specified length if necessary.
%
%   @example
%     % Trim "very_long_predicate_name" to its last 10 characters.
%     ?- trim_to_last_nchars(10, 'very_long_predicate_name', TrimmedAtom).
%     TrimmedAtom = 'ate_name'.
%
trim_to_last_nchars(Len, Atom, TrimmedAtom) :-
    atom_length(Atom, Length),
    (   Length =< Len
    ->  TrimmedAtom = Atom
    ;   Before is Length - 32,
        sub_atom(Atom, Before, 32, _, TrimmedAtom)
    ).

%!  translate_metta_file_to_datalog_io(+Filename, +Input, +Output) is det.
%
%   Translates a `.metta` file to Datalog format, outputting to a specified file.
%
%   This predicate reads the contents of a `.metta` file from `Input` and translates it
%   to Datalog format, writing the result to `Output`. It makes the resulting predicates
%   dynamic/multifile and tracks translation progress, reporting it periodically. If the
%   Datalog conversion is complete, it displays the total number of translated forms.
%
%   @arg Filename The name of the `.metta` file being translated.
%   @arg Input    The input stream for reading the `.metta` file.
%   @arg Output   The output stream for writing the Datalog format.
%
%   @example
%     % Translate a `.metta` file to Datalog format.
%     ?- translate_metta_file_to_datalog_io('example.metta', Input, Output).
%
translate_metta_file_to_datalog_io(Filename,Input,Output):- may_use_datalog,
  must_det_ll((
  %write header
 notrace((
  write(Output,'/* '),write(Output,Filename),writeln(Output,' */'),
  % write the translation time and date
  get_time(Time),stamp_date_time(Time,Date,'UTC'),
  format_time(string(DateStr),'%FT%T%z',Date),
  write(Output,'/* '),write(Output,DateStr),writeln(Output,' */'))),
  % make the predicate dynamic/multifile
  filename_to_mangled_pred(Filename,MangleP2),
    mangle_iz(MangleP2,MangleIZ),
 notrace((
  format(Output,':- style_check(-singleton). ~n',[]),
  format(Output,':- style_check(-discontiguous). ~n',[]),
  format(Output,':- dynamic((~q)/2). ~n',[MangleP2]),
  format(Output,':- dynamic((~q)/3). ~n',[MangleP2]),
  format(Output,':- dynamic((~q)/4). ~n',[MangleP2]),
  format(Output,':- dynamic((~q)/5). ~n',[MangleP2]),
  format(Output,':- dynamic((~q)/6). ~n',[MangleP2]),
  format(Output,':- dynamic((~q)/7). ~n',[MangleP2]),

  format(Output,':- dynamic((~q)/4). ~n',[MangleIZ]),
  format(Output,':- dynamic((~q)/5). ~n',[MangleIZ]),
  format(Output,':- dynamic((~q)/6). ~n',[MangleIZ]),
  format(Output,':- dynamic((~q)/7). ~n',[MangleIZ]),
  format(Output,':- dynamic((~q)/8). ~n',[MangleIZ]),
  writeln(Output,':- dynamic(user:asserted_metta_pred/2).'),
  writeln(Output,':- multifile(user:asserted_metta_pred/2).'),
  format(Output,'user:asserted_metta_pred(~q,~q). ~n',[MangleP2,Filename]),
  with_output_to(Output,produce_iz(MangleP2)))),
  %format(Output,':- user:register_asserted_metta_pred(~q,~q). ~n',[MangleP2,Filename]),
  flag(translated_forms,_,0),
  LastTime = t(Time),
  % translate the file
  once(call((
  repeat,
  (at_end_of_stream(Input)->!;
  ( must_det_ll((
    line_count(Input,Lineno),
    read_file_sexpr(Input,Term))),
    (Term==end_of_file->!;
    (once(((
      % if_t((0 is (Lineno mod 10000)),writeln(Term:Lineno)),
      /*non_compat_io*/
      (
          if_t((
    get_time(NTime),arg(1,LastTime,Last),
       Elapsed is (NTime-Last), Elapsed > 4),
            (nb_setarg(1,LastTime,NTime),
      move_cursor_to_first_column,
      format(user_error,'; ~@ ; line: ~w ',[write_src_woi(Term),Lineno]),
      write(user_error,'\033[K'),
      move_cursor_to_first_column))),
      flag(translated_forms,X,X+1),
      write_metta_datalog_term(Output,Term,MangleP2,Lineno))))),fail)))))),
  flush_output(Output),
  % tell the user we are done
  flag(translated_forms,TF,TF),
  format(user_error,'~N; Done translating ~w forms: ~q.',
         [TF,asserted_metta_pred(MangleP2,Filename)]))).

%!  write_metta_datalog_term(+Output, +Term, +MangleP2, +Lineno) is det.
%
%   Writes a translated MeTTa term to Datalog format in the specified output stream.
%
%   This predicate formats and writes a given term (`Term`) to `Output`, adapting its
%   structure based on its type (e.g., comments, executable terms, or asserted terms).
%   Each case is handled separately to ensure correct syntax for Datalog.
%
%   @arg Output   The output stream to which the term is written.
%   @arg Term     The term to be written in Datalog format.
%   @arg MangleP2 The predicate name or identifier associated with this translation.
%   @arg Lineno   The line number of the term in the source file.
%
%   @example
%     % Write a term "exec(some_action)" to Datalog format.
%     ?- write_metta_datalog_term(Output, exec(some_action), my_pred, 42).
%
%   @see relistify/2
%
write_metta_datalog_term(Output, '$COMMENT'(Term, _, _), _MangleP2, _Lineno) :-
    % Write comments in Datalog format
    format(Output, "/* ~w */~n", [Term]).
write_metta_datalog_term(Output, exec(Term), MangleP2, Lineno) :-
    % Write executed terms in Datalog format
    format(Output, ":-eval_Line(~q,~q,~q).~n", [Term, MangleP2, Lineno]).
write_metta_datalog_term(Output, STerm, MangleP2, Lineno) :-
    % Write asserted terms in Datalog format
    s2t_iz(MangleP2, P, STerm, Term),
    relistify(Term, TermL),
    Data =.. [P, Lineno | TermL],
    format(Output, "~q.~n", [Data]).

%!  relistify(+Term, -TermL) is det.
%
%   Ensures a term is in list form.
%
%   This helper predicate converts a term into list form (`TermL`). If `Term` is
%   already a list, it is returned unaltered. If `Term` is a compound term, it
%   is flattened into a list.
%
%   @arg Term  The term to be converted to list form.
%   @arg TermL The resulting list-form of the term.
%
relistify(Term, TermL) :- is_list(Term), !, TermL = Term.
relistify([H|T], TermL) :- flatten([H|T], TermL), !.
relistify(Term, [Term]).

%!  eval_Line(+A, +B, +C) is det.
%
%   Evaluates a term `A` within the Datalog context, displaying results.
%
%   This predicate evaluates `A` and optionally outputs the result (`R`),
%   useful for debugging or tracking evaluation results.
%
%   @arg A The term to evaluate.
%   @arg B Unused parameter for possible extensions.
%   @arg C Unused parameter for possible extensions.
%
eval_Line(A, _B, _C) :-
    test_alarm,
    format('~N'), nl, % write_src(eval_Line(A,B,C)),nl,
    eval(A, R), nl, write_src_uo(R).

%!  translate_metta_datalog(+Input, +Output) is det.
%
%   Translates MeTTa terms from input to Datalog format in the output stream.
%
%   This predicate reads characters from `Input` and translates them to a format
%   compatible with Datalog, writing the results to `Output`. Special handling is
%   provided for parentheses, quotes, whitespace, and comments.
%
%   @arg Input  The input stream containing MeTTa terms.
%   @arg Output The output stream for writing Datalog-translated terms.
%
translate_metta_datalog(Input, Output) :-
    translate_metta_datalog('', Input, Output), !.
translate_metta_datalog(_, Input, _) :-
    at_end_of_stream(Input), !.
translate_metta_datalog(Ch, Input, Output) :-
    peek_char(Input, Char),
    translate_metta_datalog(Ch, Input, Output, Char).
translate_metta_datalog(_, Input, Output, ')') :-
    % Handle closing parentheses
    !, get_char(Input, _),writeq(Output, ']'),translate_metta_datalog(',', Input, Output).
translate_metta_datalog(Ch, Input, Output, '(') :-
    % Handle opening parentheses
    !, get_char(Input, _),write(Output, Ch),writeq(Output, '['),translate_metta_datalog('', Input, Output).
translate_metta_datalog(Ch, Input, Output, Space) :-
    % Skip whitespace
    char_type(Space, space), !,get_char(Input, Char),write(Output, Char),translate_metta_datalog(Ch, Input, Output).
translate_metta_datalog(Ch, Input, Output, ';') :-
    % Handle comments
    !, read_line_to_string(Input, Comment),format(Output, '/* ~w */', [Comment]),translate_metta_datalog(Ch, Input, Output).
translate_metta_datalog(Ch, Input, Output, '"') :-
    % Handle quoted terms
    !, read_term(Input, Term, []),write(Output, Ch),writeq(Output, Term),translate_metta_datalog(',', Input, Output).
translate_metta_datalog(Ch, Input, Output, '`') :-
    % Handle backquoted terms
    !, read_term(Input, Term, []),write(Output, Ch),writeq(Output, Term),translate_metta_datalog(',', Input, Output).
translate_metta_datalog(Ch, Input, Output, '\'') :-
    % Handle single-quoted terms
    !, read_term(Input, Term, []),write(Output, Ch),writeq(Output, Term),translate_metta_datalog(',', Input, Output).
translate_metta_datalog(Ch, Input, Output, '$') :-
    % Handle dollar-prefixed terms
    !, read_chars_until([type(space), ')'], Input, Codes),name(Term, Codes),write(Output, Ch),writeq(Output, Term),
    translate_metta_datalog(',', Input, Output).
translate_metta_datalog(Ch, Input, Output, Peek) :-
    % Handle general characters
    !, read_chars_until([type(space), ')'], Peek, Input, Codes),name(Term, Codes),write(Output, Ch),
    writeq(Output, Term),translate_metta_datalog(',', Input, Output).

%!  read_chars_until(+StopsBefore, +Input, -Codes) is det.
%
%   Reads characters from an input stream until encountering a specified stopping
%   character or character type.
%
%   This predicate reads characters from `Input`, collecting them into `Codes`, until it
%   encounters a character that matches any element in `StopsBefore`. `StopsBefore` may
%   contain specific characters or types (e.g., spaces, punctuation).
%
%   This function is useful for parsing input streams up to a specific point, based on
%   defined character types or individual characters.
%
%   @arg StopsBefore A list of characters or character types (e.g., `type(space)`)
%           that indicate where to stop reading.
%   @arg Input       The input stream from which characters are read.
%   @arg Codes       The list of character codes read up to the stop character.
%
%   @example
%     % Read characters until encountering a space or period.
%     ?- read_chars_until([type(space), '.'], Input, Codes).
%
read_chars_until(_StopsBefore, Input, []) :-
    % Stop reading if the end of stream is reached.
    at_end_of_stream(Input), !.
read_chars_until(StopsBefore, Input, Codes) :-
    % Peek at the next character to determine if it matches any stop criteria.
    peek_char(Input, Char),
    % Delegate to the next clause based on the character peeked.
    read_chars_until(StopsBefore, Char, Input, Codes).

%!  stops_before(+StopsBefore, +Char) is nondet.
%
%   Checks if a character matches any condition in a list of stop criteria.
%
%   This predicate succeeds if `Char` matches any element in `StopsBefore`. Each element
%   in `StopsBefore` can be a specific character or a type of character (e.g., `type(space)`).
%   This is useful for determining if a character should halt reading in a stream.
%
%   @arg StopsBefore A list of stopping conditions, either character types or specific characters.
%   @arg Char        The character to check against the stop conditions.
%
%   @example
%     % Check if a character is a space or equals the stop character `;`.
%     ?- stops_before([type(space), ';'], ' ').
%
stops_before([type(Type) | StopsBefore], Char) :-
    % Check if the character type matches
    char_type(Char, Type); stops_before(StopsBefore, Char).
stops_before([Ch | StopsBefore], Char) :-
    % Check if the character itself is a stop character
    Ch == Char; stops_before(StopsBefore, Char).

%!  read_chars_until(+StopsBefore, +Char, +Input, -Codes) is det.
%
%   Reads characters from an input stream until a stop condition is met.
%
%   This predicate reads characters from the `Input` stream, collecting them in `Codes`,
%   until it encounters a character `Char` that matches any condition in `StopsBefore`.
%   If an escape character `\` is encountered, the next character is read and added to
%   `Codes` before continuing to read.
%
%   @arg StopsBefore A list of stop conditions for characters or character types.
%   @arg Char        The current character being checked against the stop conditions.
%   @arg Input       The input stream to read from.
%   @arg Codes       The list of character codes read up to the stopping character.
%
%   @example
%     % Read characters from input until a space or period is encountered.
%     ?- read_chars_until([type(space), '.'], Char, Input, Codes).
%
read_chars_until(StopsBefore, Char, _, []) :-
    % Stop reading if the current character matches a stop condition.
    stops_before(StopsBefore, Char), !.
read_chars_until(StopsBefore, '\\', Input, [Code | Codes]) :-
    % Handle escape characters by reading the escaped character.
    get_char(Input, Code),
    read_chars_until(StopsBefore, Input, Codes).
read_chars_until(StopsBefore, Char, Input, [Char | Codes]) :-
    % Read character and continue until stop condition is met.
    get_char(Input, _),
    read_chars_until(StopsBefore, Input, Codes).

%!  just_load_datalog is det.
%
%   Placeholder predicate indicating that Datalog loading is enabled.
%
%   This predicate always succeeds with `true`, indicating that Datalog loading is allowed.
just_load_datalog :- !, true.

%!  may_use_datalog is det.
%
%   Placeholder predicate indicating that Datalog usage is allowed.
%
%   This predicate always succeeds with `true`, indicating that Datalog usage is permitted.
may_use_datalog :- !, true.

%!  convert_datalog_to_loadable(+DatalogFile, -QlfFile) is det.
%
%   Converts a Datalog file to a loadable `.qlf` file format, if required.
%
%   If `just_load_datalog` is enabled, no conversion is done, and `QlfFile` is identical
%   to `DatalogFile`. Otherwise, a shell command is used to compile the Datalog file to `.qlf`.
%   This can improve performance during loading by creating a quick-load format.
%
%   @arg DatalogFile The source Datalog file.
%   @arg QlfFile     The resulting loadable `.qlf` file, or identical to `DatalogFile` if
%           conversion is skipped.
%
%   @example
%     % Convert "example.datalog" to "example.qlf" for quick-loading.
%     ?- convert_datalog_to_loadable('example.datalog', QlfFile).
%
convert_datalog_to_loadable(DatalogFile, DatalogFile) :-
    % Skip conversion if only Datalog loading is allowed
    just_load_datalog, !.
convert_datalog_to_loadable(DatalogFile, QlfFile) :-
    % Perform `.qlf` conversion using a shell command.
    sformat(S, 'swipl -g "qcompile(~q)" -t halt', [DatalogFile]),
    shell(S, _),
    file_name_extension(Base, _, DatalogFile),
    % Update the file extension to `.qlf`
    file_name_extension(Base, 'qlf', QlfFile).

%!  convert_metta_to_loadable(+Filename, -QlfFile) is det.
%
%   Converts a `.metta` file to a loadable format if Datalog usage is allowed.
%
%   This predicate converts a `.metta` file to `.qlf` format if `may_use_datalog` is true.
%   If conditions are met, the file is first converted to Datalog and then to `.qlf`. This
%   two-step process optimizes loading by creating a quick-load format.
%
%   @arg Filename The original `.metta` file to convert.
%   @arg QlfFile  The resulting `.qlf` file, if conversion succeeds.
%
%   @example
%     % Convert "example.metta" to a `.qlf` format.
%     ?- convert_metta_to_loadable('example.metta', QlfFile).
%
convert_metta_to_loadable(_Filename, _QlfFile) :- !, fail.
convert_metta_to_loadable(_Filename, _QlfFile) :-
    % Use fast buffer, so skip Datalog conversion
    use_fast_buffer, !, fail.
convert_metta_to_loadable(_Filename, _QlfFile) :-
    % Datalog usage not allowed, skip conversion
    \+ may_use_datalog, !.
convert_metta_to_loadable(Filename, QlfFile) :-
    % Convert to Datalog, then to `.qlf` if permitted
    must_det_ll((
        convert_metta_to_datalog(Filename, DatalogFile),
        convert_datalog_to_loadable(DatalogFile, QlfFile)
    )), !.
convert_metta_to_loadable(Filename, _) :-
    % Alternative conversion using a shell script
    metta_dir(Dir),
    sformat(S, '~w/cheap_convert.sh --verbose=1 ~w', [Dir, Filename]),
    shell(S, Ret), !, Ret == 0.

%!  accept_line(+Self, +I) is det.
%
%   Processes a line from input, normalizing it and evaluating if appropriate.
%
%   This predicate takes a line `I`, normalizes whitespace, and either interprets it as
%   a command or forwards it for further processing. This function is typically used
%   for reading and processing single lines from an input source.
%
%   @arg Self The context or module in which the line is accepted.
%   @arg I    The line content to process.
%
accept_line(_Self, end_of_file) :- !.
accept_line(Self, I) :-
    % Normalize whitespace in the input line
    normalize_space(string(Str), I),
    !, accept_line2(Self, Str),!.

%!  accept_line2(+Self, +S) is det.
%
%   Processes a normalized line as a command or assertion.
%
%   This helper predicate determines whether `S` is a comment or a statement, and handles
%   it accordingly by either printing or asserting it to the knowledge base. This function
%   assists `accept_line/2` in handling lines that may be commands or statements.
%
%   @arg Self The context or module in which the line is processed.
%   @arg S    The normalized line content to process.
%
%   @example
%     % Accept a line formatted as a Prolog assertion.
%     ?- accept_line2('&self', "(assert fact)").
%
accept_line2(_Self, S) :-
    % Process comments that start with a semicolon
    string_concat(";", _, S),
    !, writeln(S).
accept_line2(Self, S) :-
    % Process statements enclosed in parentheses
    string_concat('(', RS, S),
    string_concat(M, ')', RS),
    !,
    % Split the statement by spaces to extract the function and arguments
    symbolic_list_concat([F | LL], ' ', M),
    % Construct a Prolog term from the function and arguments
    PL =.. [F, Self | LL],
    % Assert the term to the knowledge base
    pfcAdd_Now(PL),
    !,
    % Update assertion counter and log progress periodically
    flag(next_assert, X, X + 1),
    if_t((0 is X mod 10_000_000), (writeln(X = PL), statistics)).
accept_line2(Self, S) :-
    % Fallback case for unexpected content
    fbug(accept_line2(Self, S)), !.

%!  load_metta_file_stream(+Filename, +Self, +In) is det.
%
%   Loads a MeTTa file from an input stream and processes its contents.
%
%   This predicate reads a `.metta` file from `In` (the input stream) and determines
%   the appropriate method (`P2`) to read its contents based on file size. If the file
%   size is large, the predicate adjusts the reading process to optimize performance.
%   It then processes the file using `load_metta_file_stream_fast/5`.
%
%   @arg Filename The name of the `.metta` file being loaded, used for file context.
%   @arg Self     The context or module in which the file content is processed.
%   @arg In       The input stream from which the file content is read.
%
%   @example
%     % Load and process a `.metta` file stream in the context of `&self`.
%     ?- load_metta_file_stream('example.metta', '&self', In).
%
load_metta_file_stream(Filename, Self, In) :- maybe_into_top_self(Self, TopSelf), !, load_metta_file_stream(Filename, TopSelf, In).
load_metta_file_stream(Filename, Self, In) :-
    % Check if the filename is atomic and exists, then get file size.
    if_t((atomic(Filename), exists_file(Filename)), size_file(Filename, Size)),
    % If the size is still unbound, determine it from the stream.
    if_t(var(Size), is_file_stream_and_size(In, Size)),
    % Choose reading method based on file size:
    % once((is_file_stream_and_size(In, Size), Size > 102400) -> P2 = read_file_sexpr;
    P2 = read_file_sexpr,  % Default reading method.
    % Load file with specific options and execution settings.
    with_option(loading_file, Filename,
        % current_exec_file(Filename),  % Optionally set current execution file.
        must_det_ll((
            % Set initial execution number, load answer file, and reset execution.
            must_det_ll((
       set_exec_num(Filename, 1),
       load_answer_file(Filename),
       set_exec_num(Filename, 0)
            )),
            % Process the file using optimized loading.
            load_metta_file_stream_fast(Size, P2, Filename, Self, In)))).

% use_fast_buffer makes tmp .buffer files that get around long load times
use_fast_buffer:- nb_current(may_use_fast_buffer,t).

%:- nb_setval(may_use_fast_buffer,t).
%:- use_fast_buffer.

:- dynamic(user:metta_file_buffer/7).
:- multifile(user:metta_file_buffer/7).

%!  use_cache_file(+Filename, +BufferFile) is nondet.
%
%   Determines if a temporary buffer file should be preferred over the original file.
%
%   This predicate checks if `BufferFile` exists and is newer and sufficiently large
%   compared to `Filename`. If the original `Filename` does not exist, it defaults to
%   using `BufferFile`. This check ensures that a buffer file is not used if it might be
%   outdated or truncated.
%
%   @arg Filename   The name of the original `.metta` file.
%   @arg BufferFile The name of the buffer file.
%
%   @example
%     % Check if a buffer file should be preferred over "example.metta".
%     ?- use_cache_file('example.metta', 'example.metta.buffer~').
%
use_cache_file(Filename, BufferFile) :-
    \+ exists_file(Filename), !, exists_file(BufferFile).
use_cache_file(Filename, BufferFile) :-
    % Get modification times for both files
    time_file(Filename, FileTime),
    time_file(BufferFile, BufferFileTime),
    BufferFileTime > FileTime,
    % Check file sizes
    size_file(Filename, MettaSize),
    size_file(BufferFile, BufferSize),
    % Ensure buffer file is not truncated
    BufferSize >= 0.25 * MettaSize,
    \+ older_than_impl(BufferFileTime).

% checks to see if the impl has been updated since a particular time
older_than_impl(BufferFileTime):-
    is_metta_src_dir(Directory),
    newest_file_time(Directory, '{*.pl,*.metta}', NewestTime),
    NewestTime>BufferFileTime.

%% newest_file_time(+Directory, +Mask, -NewestTime) is det.
%
% Retrieves the modification time of the newest file in a specified directory
% that matches a given file mask.
%
% @param Directory The directory to scan.
% @param Mask The file mask for filtering files, e.g., '*.txt'.
% @param NewestTime The Unix timestamp of the newest file found that matches the mask.
%
% @example
%    % To find the newest .txt file modification time in the directory '/my/files':
%    newest_file_time('/my/files', '*.txt', Time).
%
% @example
%    % To find the newest .jpg file modification time in the directory 'C:\Pictures':
%    newest_file_time('C:\\Pictures', '*.jpg', Time).
%
newest_file_time(Directory, Mask, NewestTime) :-
    directory_files(Directory, Files),
    include(wildcard_match(Mask), Files, FilteredFiles),
    find_newest_file_time(Directory, FilteredFiles, 0, NewestTime).


%% find_newest_file_time(+Directory, +Files, +CurrentNewest, -NewestTime) is det.
%
% Helper predicate to find the newest file time from a list of files.
%
% @param Directory The directory containing the files.
% @param Files List of file names to check.
% @param CurrentNewest The current newest time found.
% @param NewestTime The updated newest time.

find_newest_file_time(_, [], Newest, Newest).
find_newest_file_time(Directory, [File | Rest], CurrentNewest, NewestTime) :-
    directory_file_path(Directory, File, FilePath),
    (   exists_file(FilePath)
    ->  time_file(FilePath, Time),
        (   Time > CurrentNewest
        ->  UpdatedNewest = Time
        ;   UpdatedNewest = CurrentNewest
        )
    ;   UpdatedNewest = CurrentNewest
    ),
    find_newest_file_time(Directory, Rest, UpdatedNewest, NewestTime).


%!  load_metta_file_stream_fast(+Size, +P2, +Filename, +Self, +In) is det.
%
%   Loads a MeTTa file quickly using a buffer if available.
%
%   This predicate attempts to load a `.metta` file directly from a buffer file if the
%   buffer exists and is preferred over the original file. If `BufferFile` is valid and
%   newer than `Filename`, it uses `load_metta_buffer/2` to load the file contents.
%
%   Additionally, alternative methods for loading `.metta` content are provided as
%   commented-out options for handling specific scenarios.
%
%   @arg Size     The size of the `.metta` file.
%   @arg P2       The reading method to use (not currently modified in this code).
%   @arg Filename The name of the `.metta` file to be loaded.
%   @arg Self     The context or module in which the file is processed.
%   @arg In       The input stream for reading file content.
%
%   @example
%     % Load a `.metta` file using a buffer if possible.
%     ?- load_metta_file_stream_fast(10000, read_file_sexpr, 'example.metta', '&self', In).
%

load_metta_file_stream_fast(Size, P2, Filename, Self, In) :- maybe_into_top_self(Self, TopSelf), !, load_metta_file_stream_fast(Size, P2, Filename, TopSelf, In).
load_metta_file_stream_fast(_Size, _P2, Filename, Self, _In) :-
    % Generate buffer file name and check existence
    atomic(Filename), cache_file(Filename, BufferFile),
    exists_file(BufferFile),
    (   % Prefer the buffer file if it is newer and large enough
        use_cache_file(Filename, BufferFile)
    ->  (use_fast_buffer, fbugio(using(BufferFile)),
         ensure_loaded(BufferFile), !, load_metta_buffer(Self, Filename))
    ;   % Delete outdated buffer file
        (fbugio(deleting(BufferFile)), delete_file(BufferFile), fail)
    ).
load_metta_file_stream_fast(_Size, _P2, Filename, Self, S) :-
    fail,
    % Alternative loading method for HTML files with symbol concatenation (disabled)
    symbolic_list_concat([_, _, _ | _], '.', Filename),
    \+ option_value(html, true),
    atomic(S), is_stream(S), stream_property(S, input), !,
    % Read and process each line until end of file
    repeat,
    read_line_to_string(S, I),
    accept_line(Self, I),
    I == end_of_file, !.
load_metta_file_stream_fast(_Size, _P2, Filename, Self, In) :-
    % Create a buffer file if `use_fast_buffer` is enabled and load the buffer
    make_metta_file_buffer(use_fast_buffer, Filename, In),
    load_metta_buffer(Self, Filename).

%!  make_metta_file_buffer(+TFMakeFile, +FileName, +InStream) is det.
%
%   Creates a buffer file for a MeTTa file if `TFMakeFile` is true.
%
%   This predicate generates a buffer file (`BufferFile`) with a `.buffer~` extension
%   based on `FileName`. It processes each expression from `InStream` using
%   `maybe_write_bf/3`, which writes expressions to the buffer file if `TFMakeFile`
%   is true.
%
%   @arg TFMakeFile  A flag indicating whether to create a buffer file.
%   @arg FileName    The base file name for the `.metta` file.
%   @arg InStream    The input stream for reading file content.
%
%   @example
%     % Create a buffer file for "example.metta" if the flag is true.
%     ?- make_metta_file_buffer(true, 'example.metta', InStream).
%
make_metta_file_buffer(TFMakeFile, FileName, InStream) :-
    % Generate buffer file name with `.buffer~` extension.
    cache_file(FileName, BufferFile),
    % Process expressions from the input stream with optional buffering.
    process_expressions(FileName, InStream, maybe_write_bf(TFMakeFile, BufferFile)).


:- use_module(library(system)).   % for absolute_file_name/3
:- use_module(library(filesex)).  % For make_directory_path/1, etc.
:- use_module(library(lists)).

/** cache_file(+Original, -CachedFile) is det.

Construct a cache file path for `Original` under `<temp>/metta_cache/`.

Steps:
  1. Determine a system temp dir (`TMPDIR`/`TMP`/`TEMP`/`TEMPDIR`)
     or fall back to `C:/Windows/Temp` (on Windows) or `/tmp`.
  2. Append `/metta_cache/`.
  3. Create that directory if it doesn't exist.
  4. Apply multiple find-replace pairs to `Original`:
       - `:` -> `~`
       - `\\` -> `/`
       - `/`  -> `~~`
       - `' '` (space) -> `~~~`
  5. Append `.buffer~` extension.

Example:

==
?- cache_file('C:/some path/file:01', Path).
Path = 'C:/Windows/Temp/metta_cache/C~~some~~~path~~file~01.buffer~'.
==
*/
cache_file(Original, CachedFile):-  cache_file(Original, '.buffer~', CachedFile).
cache_file(Original, Ending, CachedFile) :-
    metta_cache_dir(Dir),
    make_directory_path(Dir),  % Ensure the directory exists
    Replacements = [
       ':'-'~~',
       '\\'-'/',
       '/'-'~',
       ' '-'~~~'
    ],
    fr_slashes(Replacements, Original, RelPath),
    atomic_list_concat([RelPath, Ending], BufferFile),
    directory_file_path(Dir, BufferFile, CachedFile).


/** clean_cache_files is det.

Delete **all files** in `<temp>/metta_cache/`, but do *not* remove
subdirectories or the directory itself.

If the directory does not exist, this predicate simply succeeds.

Example:

==
?- clean_cache_files.
true.
==
*/
clean_cache_files:- clean_cache_files('.buffer~').
clean_cache_files(Ending) :-
    metta_cache_dir(Dir),
    (   exists_directory(Dir)
    ->  directory_files(Dir, Items),
        forall(member(Item, Items),
          ignore(( Item \= '.', Item \= '..',
            directory_file_path(Dir, Item, FilePath),
            ( (exists_file(FilePath), atom_concat(_,Ending, FilePath))
            -> delete_file(FilePath)
            ; true
            )
          )))
    ;   true
    ).

/* ---------------------------------------------------------------------
   2) HELPER PREDICATES
   --------------------------------------------------------------------- */

%! metta_cache_dir(-Dir) is det.
%
%  Return `<temp>/metta_cache/` (ensuring forward slashes).
%  Priority environment variables: TMPDIR, TMP, TEMP, TEMPDIR.
%  Fallback: `C:/Windows/Temp` on Windows, `/tmp` otherwise.
%
metta_cache_dir(Dir) :-
    (   getenv('TMPDIR', TempDir)
    ;   getenv('TMP', TempDir)
    ;   getenv('TEMP', TempDir)
    ;   getenv('TEMPDIR', TempDir)
    ;  (current_prolog_flag(windows, true)
    ->  TempDir = 'C:/Windows/Temp'
    ;   TempDir = '/tmp'
    )),
    absolute_file_name(TempDir,TDir,[access(write),file_type(directory)]),!,
    directory_file_path(TDir,'metta_cache',Dir).

%! fr_slashes(+Pairs, +In, -Out) is det.
%
%  Applies each `Find-Replace` pair in `Pairs` to the atom/string `In`,
%  producing `Out`. For each pair, *all* occurrences of `Find` get replaced
%  with `Replace`.
%
%  Example:
%  ==
%  ?- fr_slashes([':'='~',' '='~_'], "File: name", R).
%  R = "File~ name".
%  ==
%
fr_slashes([], In, In).
fr_slashes([Find-Replace | Rest], In, Out) :-
    atomic_list_concat(Pieces, Find, In),
    atomic_list_concat(Pieces, Replace, Next),
    fr_slashes(Rest, Next, Out).


%!  maybe_write_bf(+TFMakeFile, +BufferFile, +Item) is det.
%
%   Conditionally writes an item to the buffer file if `TFMakeFile` is true.
%
%   This predicate checks `TFMakeFile` and, if true, writes `Item` to `BufferFile`
%   using `write_bf/2`.
%
%   @arg TFMakeFile  A flag indicating whether to write to the buffer.
%   @arg BufferFile  The buffer file where the item might be written.
%   @arg Item        The item to potentially write to the buffer.
%
maybe_write_bf(TFMakeFile, BufferFile, Item) :-
    % Write to the buffer file if `TFMakeFile` is true.
    if_t(TFMakeFile, write_bf(BufferFile, Item)).

%!  pos_line_char(+Position, -LineChar) is det.
%
%   Extracts line and column information from a stream position.
%
%   Given a `Position` in a stream, this predicate provides the corresponding line
%   and column data as `line_char(LineM1, Col)`, where `LineM1` is zero-based.
%
%   @arg Position  The stream position containing line and column information.
%   @arg LineChar  The result structure with line and column information.
%
%   @example
%     % Extract line and column from a stream position.
%     ?- pos_line_char(Position, LineChar).
%
pos_line_char(Position, line_char(LineM1, Col)) :-
    % Extract the line number.
    stream_position_data(line_count, Position, Line),
    LineM1 is Line - 1,
    % Extract the column number.
    stream_position_data(line_position, Position, Col).

%!  always_rebuild_temp is det.
%
%   Placeholder predicate indicating that temporary files should always be rebuilt.
%
%   This predicate is always true, signaling that any temporary files should be rebuilt.
always_rebuild_temp :- true.

%!  write_bf(+BufferFile, +BufferTerm) is det.
%
%   Appends a term to the buffer file.
%
%   This predicate opens `BufferFile` in append mode, writes `BufferTerm` to it in a
%   Prolog-readable format, and closes the file afterward. It ensures safe file handling
%   through `setup_call_cleanup/3`.
%
%   @arg BufferFile  The file to which `BufferTerm` is appended.
%   @arg BufferTerm  The term to write to the buffer file.
%
%   @example
%     % Write a term to the buffer file.
%     ?- write_bf('example.metta.buffer~', some_term).
%
write_bf(BufferFile, BufferTerm) :-
    setup_call_cleanup(
        open(BufferFile, append, Out),
        format(Out, '~q.~n', [BufferTerm]),
        close(Out)
    ).

%!  my_line_count(+In, -Pos) is det.
%
%   Retrieves the current position in the stream or performs an action if repositioning is supported.
%
%   This predicate provides a method for checking the current stream position in `In`.
%   It can perform an action to determine the position if the stream supports repositioning.
%
%   @arg In   The input stream.
%   @arg Pos  The position in the stream or a specific action indicator.
%
%   @example
%     % Get the current position or perform specific checks.
%     ?- my_line_count(In, Pos).
%
my_line_count(In, seek($, 0, current, CC)) :-
    % Check if the stream allows repositioning and get the current position.
    stream_property(In, reposition(true)),
    seek(In, 0, current, CC), fail.
my_line_count(In, /*position*/(Pos)) :-
    % Obtain the current position property.
    stream_property(In, position(Pos)).

%!  metta_file_buffer(+Flag, +Expr, +NamedVarsList, +Filename, +LineCount) is det.
%
%   Legacy predicate for compatibility with older code.
%
%   This predicate is retained for compatibility with code that uses `metta_file_buffer/5`.
%   It forwards calls to `metta_file_buffer/6` with default parameters.
%
%   @arg Flag           A flag for controlling the buffer behavior.
%   @arg Expr           The expression to be processed.
%   @arg NamedVarsList  A list of named variables.
%   @arg Filename       The name of the file being processed.
%   @arg LineCount      The current line count in the file.
%
%   @example
%     % Use legacy compatibility with a buffer processing function.
%     ?- metta_file_buffer(+, Expr, NamedVarsList, 'example.metta', 42).
%
metta_file_buffer(+, Expr, NamedVarsList, Filename, LineCount) :-
    user:metta_file_buffer(0, _Ord, _Kind, Expr, NamedVarsList, Filename, LineCount).

%!  load_metta_buffer(+Self, +Filename) is det.
%
%   Loads a `.metta` file into the knowledge base from a buffer.
%
%   This predicate processes buffered expressions from a `.metta` file, adding each
%   expression to the knowledge base. It sets an execution number for tracking, loads
%   answer files, and processes expressions through `do_metta/5`. Any unhandled expressions
%   trigger a warning.
%
%   @arg Self     The context or module in which the file is loaded.
%   @arg Filename The name of the `.metta` file being loaded from the buffer.
%
%   @example
%     % Load the buffered contents of "example.metta" into the knowledge base.
%     ?- load_metta_buffer('&self', 'example.metta').
%
load_metta_buffer(Self, Filename) :- maybe_into_top_self(Self, TopSelf), !, load_metta_buffer(TopSelf, Filename).
load_metta_buffer(Self, Filename) :-
    % Set execution number, load answer file, and reset execution.
    set_exec_num(Filename, 1),
    load_answer_file(Filename),
    set_exec_num(Filename, 0),
    Mode = '+',
    % Register the file as loaded in the knowledge base.
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    % Process each buffered expression.
    (forall(
        user:metta_file_buffer(0, _Ord, _Kind, Expr, NamedVarsList, Filename, _LineCount),
         (must_det_lls(maybe_name_vars(NamedVarsList)),
          (must_det_lls(do_metta(file(Filename), Mode, Self, Expr, _O)) -> true
        ;  (ignore(rtrace(do_metta(file(Filename), Mode, Self, Expr, _O2))),
                   trace, epp_m(unknown_do_metta(file(Filename), Mode, Self, Expr))))))).


%!  is_file_stream_and_size(+Stream, -Size) is nondet.
%
%   Checks if the given stream is a file stream and retrieves its file size.
%
%   This predicate verifies whether the provided Stream is associated with an actual
%   file. If so, it attempts to retrieve the file's size. It is particularly useful for
%   identifying file-based streams and obtaining metadata about the file without
%   directly manipulating it.
%
%   @arg Stream The stream to check; it should be an open Prolog stream.
%   @arg Size   Unifies with the size of the file in bytes if the stream is a file stream.
%
%   @example
%     % Check if a stream is a file stream and get its size.
%     ?- open('example.txt', read, Stream),
%        is_file_stream_and_size(Stream, Size).
%     Size = 1024.
%
is_file_stream_and_size(Stream, Size) :-
    % Check if the stream is associated with a file.
    stream_property(Stream, file_name(FileName)),
    % Check if the file is accessible and get its size.
    exists_file(FileName),
    size_file(FileName, Size).

%!  maybe_read_pl(+In, -Expr) is nondet.
%
%   Attempts to read a Prolog term from the input stream if specific conditions are met.
%
%   This predicate peeks at the next line of input from the stream `In` to determine if it contains
%   a valid Prolog term. If the line is non-empty and includes both a period ('.') and ':-', it assumes
%   the presence of a potential Prolog term and reads it, unifying the result with `Expr`.
%
%   @arg In   The input stream to read from.
%   @arg Expr The resulting Prolog term expression, wrapped in `call/1` if successfully read.
%
%   @example
%     % Attempt to read a Prolog term from a stream.
%     ?- open('example.pl', read, In),
%        maybe_read_pl(In, Expr).
%     Expr = call((:- initialization(main))).
%
maybe_read_pl(In, Expr) :-
    % Peek at the next line from the input stream.
    peek_line(In, Line1),
    % Check if the line is non-empty and contains '.' and ':-' (indicating potential Prolog term).
    Line1 \== '', atom_contains(Line1, '.'), atom_contains(Line1, ':-'),
    % Attempt to read the term if the line matches the conditions.
    notrace(((catch_err((read_term_from_atom(Line1, Term, []), Term \== end_of_file, Expr = call(Term)), _, fail), !,
    read_term(In, Term, [])))).

%!  peek_line(-Line) is det.
%
%   Peeks at the current input line without advancing the stream.
%
%   This predicate reads the next line from the current input stream without moving the
%   stream position forward, allowing subsequent reads to access the same line again.
%
%   @arg Line The string content of the current input line.
%
peek_line(Line) :-
    % Use the current input stream for peeking the line.
    current_input(Stream),
    peek_line(Stream, Line).

%!  peek_line(+Stream, -Line) is det.
%
%   Peeks at the next line from the specified stream without moving its position.
%
%   This predicate saves the current position of the stream, reads the next line as a
%   string, and then restores the stream position, effectively "peeking" at the line.
%
%   @arg Stream The stream to read from.
%   @arg Line   The content of the line as a string, without advancing the stream position.
%
%   @example
%     % Peek at the next line of a stream.
%     ?- open('example.txt', read, Stream),
%        peek_line(Stream, Line).
%     Line = "This is the next line in the file.".
%
peek_line(Stream, Line) :-
    % Remember the current stream position.
    stream_property(Stream, position(Pos)),
    % Read the next line.
    read_line_to_string(Stream, Line),
    % Set the stream back to the remembered position.
    set_stream_position(Stream, Pos).



%!  in2_stream(+N1, -S1) is nondet.
%
%   Converts an integer, atom, or stream reference `N1` to a valid stream `S1`.
%
%   This predicate accepts multiple forms of stream references (`N1`), such as an integer
%   file descriptor, an alias atom, or a stream identifier, and attempts to unify `S1`
%   with the corresponding open stream.
%
%   @arg N1  The input representing the stream (integer, atom, or stream).
%   @arg S1  The resulting Prolog stream if `N1` matches an open stream.
%
in2_stream(N1, S1):- integer(N1), !, stream_property(S1, file_no(N1)), !.
in2_stream(N1, S1):- atom(N1), stream_property(S1, alias(N1)), !.
in2_stream(N1, S1):- is_stream(N1), S1 = N1, !.
in2_stream(N1, S1):- atom(N1), stream_property(S1, file_name(N1)), !.

%!  is_same_streams(+N1, +N2) is nondet.
%
%   Checks if two references `N1` and `N2` refer to the same stream.
%
%   This predicate uses `in2_stream/2` to convert both `N1` and `N2` to streams and then
%   checks if they refer to the same stream.
%
%   @arg N1  The first stream reference (integer, atom, or stream).
%   @arg N2  The second stream reference (integer, atom, or stream).
%
is_same_streams(N1, N2):- in2_stream(N1, S1), in2_stream(N2, S2), !, S1 == S2.



%!  move_cursor_to_first_column is det.
%
%   Moves the cursor to the first column in the user error output stream.
%
move_cursor_to_first_column:- write(user_error, '\033[1G').

%!  move_cursor_to_first_column_out is det.
%
%   Moves the cursor to the first column in the user output stream.
%
move_cursor_to_first_column_out:- write(user_output, '\033[1G').



%!  test_parse_sexpr_metta1 is det.
%
%   A test predicate to evaluate `read_metta/2` with a complex Metta expression.
%
%   This predicate tests the parsing function with a nested S-expression, printing the
%   result in quoted form. It uses `ignore/1` to handle potential errors and `break/0`
%   to allow debugging if needed.
%
test_parse_sexpr_metta1:-
  ignore((read_metta(
"(: synonyms-gene-ENSG00000085491 (synonyms (gene ENSG00000085491) (ATP-Mg/P\\(i\\)_co-transporter_1 calcium-binding_mitochondrial_carrier_protein_SCaMC-1 HGNC:20662 mitochondrial_ATP-Mg/Pi_carrier_protein_1 small_calcium-binding_mitochondrial_carrier_protein_1 mitochondrial_Ca\\(2+\\)-dependent_solute_carrier_protein_1 mitochondrial_adenyl_nucleotide_antiporter_SLC25A24 solute_carrier_family_25_member_24 calcium-binding_transporter APC1 short_calcium-binding_mitochondrial_carrier_1 solute_carrier_family_25_\\(mitochondrial_carrier;_phosphate_carrier\\),_member_24 SCAMC1 SLC25A24 short_calcium-binding_mitochondrial_carrier_protein_1 SCAMC-1)))",O),
  writeq(read_metta(O)))),break.

%!  writeqln(+W, +Q) is det.
%
%   Writes the quoted term `Q` to the stream `W`, followed by a newline.
%
%   This predicate formats the term `Q` in quoted form and appends a newline.
%   It is a no-operation (`nop`) if formatting is disabled.
%
%   @arg W  The output stream to write to.
%   @arg Q  The term to format and write.
%
writeqln(W, Q):- nop(format(W, '; ~q~n', [Q])).

%!  write_comment(+Cmt) is det.
%
%   Writes a comment `Cmt` to the output if conditions allow.
%
%   This predicate writes comments depending on compatibility and silent loading settings.
%   It uses `connlf/0` to prepare the connection for output and formats the comment.
%
%   @arg Cmt The comment to write, if permitted by conditions.
%
write_comment(_):- is_compatio, !.
write_comment(_):- silent_loading, !.
write_comment(Cmt):- connlf, format(';;~w~n', [Cmt]).

%!  do_metta_cmt(+Self, +Cmt) is det.
%
%   Processes and writes comments based on their format type.
%
%   This predicate handles different types of comments (`$COMMENT`, `$STRING`, or list format),
%   invoking `write_comment/1` to output the comment appropriately.
%
%   @arg Self The current context or reference for processing.
%   @arg Cmt  The comment term, which could be a structured comment or list.
%
do_metta_cmt(_, '$COMMENT'(Cmt, _, _)):-
      % Handle `$COMMENT` term and write the comment.
      write_comment(Cmt), !.
do_metta_cmt(_, '$STRING'(Cmt)):-
      % Handle `$STRING` term and write the comment.
      write_comment(Cmt), !.
do_metta_cmt(Self, [Cmt]):-
      % Handle list format and recursively process as a comment.
      !, do_metta_cmt(Self, Cmt), !.

%!  metta_atom_in_file(+Self, +Term) is det.
%
%   Checks if a given `Term` is associated with `Self` within a file, with file
%   and line number details omitted.
%
%   @arg Self  The context or source associated with the term.
%   @arg Term  The term to verify within the file context.
%
metta_atom_in_file(Self, Term):-
     metta_atom_in_file(Self, Term, _, _).

%!  metta_atom_in_file(+Self, +STerm, -Filename, -Lineno) is det.
%
%   Finds and verifies an S-expression term `STerm` in a specific file associated
%   with `Self`, extracting the filename and line number.
%
%   This predicate checks if a term was loaded into a file and locates its predicate
%   structure, confirming its presence by calling it with the associated data.
%
%   @arg Self     The context or source in which to search for the term.
%   @arg STerm    The S-expression term to find in the file.
%   @arg Filename The name of the file where the term is located.
%   @arg Lineno   The line number where the term is found.
%
metta_atom_in_file(Self, STerm, Filename, Lineno):-
     % Confirm the file was loaded into the knowledge base.
     user:loaded_into_kb(Self, Filename),
     once(user:asserted_metta_pred(Mangle, Filename)),
     % s2t_iz(Mangle, P, CTerm, Term),
     % CTerm = Term, Mangle = P,
     current_predicate(Mangle/Arity),
     notrace((length(STerm, Arity),
              term_variables(STerm, SVs),
              copy_term(STerm+SVs, CTerm+CVs),
              Data =.. [Mangle, Lineno | CTerm])),
     % write_src_woi(Data),
     current_predicate(_, Data),
     call(Data),
     maplist(mapvar, CVs, SVs).


%!  mapvar(+CV, -SV) is det.
%
%   Maps a variable or term `CV` to `SV`, converting `CV` to a simpler form if needed.
%
%   This predicate applies `t2s/2` to transform `CV` to `CCV` if `CV` is not a variable,
%   then unifies `SV` with the resulting term.
%
%   @arg CV  The term or variable to map.
%   @arg SV  The simplified or mapped term.
%

%mapvar(CV, SV):- var(CV), !, SV = CV.
mapvar(CV, SV):- t2s(CV, CCV), !, SV = CCV.

%!  constrain_sterm(+NV) is nondet.
%
%   Constrains `NV` to be a non-variable or a specific list structure.
%
%   This predicate enforces constraints on `NV`, succeeding if `NV` is non-variable,
%   or if it matches certain list patterns. Used to ensure `NV` meets expected structural forms.
%
%   @arg NV  The term to constrain.
%
%   @example
%     % Example usage of `constrain_sterm` with a list.
%     ?- constrain_sterm([1, 2, 3]).
%     true.
%

%constrain_sterm(STerm):- var(STerm), !, between(1, 5, Len), length(STerm, Len).
%constrain_sterm(STerm):- is_list(STerm), !.
constrain_sterm(NV):- nonvar(NV), !.
constrain_sterm([_, _, _]).
constrain_sterm([_, _, _, _]).
constrain_sterm([_, _, _, _, _]).
constrain_sterm([_, _]).

%!  s2t_iz(+Mangle, -Iz, +Input, -Output) is det.
%
%   Converts a structured term `[Name | InfoL]` to a form `[Name | InfoL]`, using
%   the `Mangle` name and specific mappings for `Iz` (mangled form).
%
%   @arg Mangle The base name for mangling.
%   @arg Iz     The mangled name based on `Mangle`.
%   @arg Input  The input list to convert.
%   @arg Output The resulting converted list.
%
%   @example
%     % Convert a structured term with mangling.
%     ?- s2t_iz(my_mangle, Iz, [:, term, [a, b, c]], Result).
%     Iz = my_mangle_iz,
%     Result = [term, a, b, c].
%
s2t_iz(Mangle, Iz, [Colon, Name, Info], [Name | InfoL]):-
     % Convert `Info` to `InfoL` and set `Iz` as the mangled form of `Mangle`.
     Colon == ':', is_list(Info), mangle_iz(Mangle, Iz),
     maplist(s2t, Info, InfoL).
s2t_iz(Mangle, Mangle, Info, InfoL):-
     % Direct conversion without mangling when `Mangle` remains unchanged.
     s2tl(Info, InfoL).

%!  mangle_iz(+Mangle, -Iz) is det.
%
%   Produces a mangled name by appending `_iz` to `Mangle`.
%
%   @arg Mangle The base name to mangle.
%   @arg Iz     The resulting mangled name.
%
mangle_iz(Mangle, Iz):-
     % Concatenate `_iz` suffix to form the mangled name.
     symbol_concat(Mangle, '_iz', Iz).

%!  produce_iz(+Mangle) is det.
%
%   Generates clauses with the mangled name `Iz` for varying argument lengths.
%
%   @arg Mangle The base name for producing clauses.
%
produce_iz(Mangle):-
     % Iterate over lengths 1 to 5, generating clauses for each.
     mangle_iz(Mangle, Iz),
     forall(between(1, 5, Len),
            once((length(Args, Len),
         produce_iz_hb([Mangle, Lineno, [:, Name, [Pred | Args]]],
              [Iz, Lineno, Name, Pred | Args])))).

%!  produce_iz_hb(+HList, +BList) is det.
%
%   Creates a clause with head `HList` and body `BList`, printing the clause.
%
%   @arg HList The list representing the clause head.
%   @arg BList The list representing the clause body.
%
produce_iz_hb(HList, BList):-
     % Unify `H` and `B` with `HList` and `BList`, then output the clause.
     H =.. HList, B =.. BList, HB = (H :- B),
     numbervars(HB, 0, _),
     writeq(HB), writeln('.').

%!  t2s(+SList, -List) is det.
%
%   Converts a structured term `SList` to a simplified term `List`.
%
%   @arg SList The structured term to convert.
%   @arg List  The simplified result.
%
%   @example
%     % Convert a compound term to a simpler structure.
%     ?- t2s([a, b, [c]], Result).
%     Result = [a, b, c].
%
t2s(SList, List):-
     % Return if `SList` is not compound.
     \+ compound(SList), !, SList = List.
t2s([H | SList], [HH | List]):-
     % Recursively transform each element in `SList`.
     !, t2s(H, HH), !, t2s(SList, List).
t2s(X, XX):-
     % Convert compound terms to arguments with functor `t`.
     compound(X), compound_name_arguments(X, t, Args), !,
     maplist(t2s, Args, XX).
t2s(X, X):- !.

%!  s2tl(+SList, -List) is det.
%
%   Simplifies a structured list `SList` by recursively transforming its elements.
%
%   @arg SList The input structured list.
%   @arg List  The resulting transformed list.
%
s2tl(SList, List):-
     % Directly assign if `SList` is non-compound.
     \+ compound(SList), !, SList = List.
s2tl([H | SList], [HH | List]):-
     % Recursively apply `s2t` transformation on list items.
     !, s2t(H, HH), !, s2tl(SList, List).
s2tl(List, List).
% s2tl(SList, List):- is_list(SList), maplist(s2t, SList, List), !.

%!  s2t(+SList, -Term) is det.
%
%   Converts a structured term `SList` into a simplified term `Term`, handling
%   specific operators and structures.
%
%   @arg SList The structured term to convert.
%   @arg Term  The simplified resulting term.
%
%   @example
%     % Convert a term with special operators.
%     ?- s2t([->, a, b], Result).
%     Result = (->, [a, b]).
%
s2t(SList, List):-
     % Directly assign if `SList` is non-compound.
     \+ compound(SList), !, SList = List.
s2t([A | SList], Term):-
     % Handle '->' as a special operator term.
     A == '->', !, s2tl(SList, List), Term =.. [A, List].
s2t([A | SList], Term):-
     % Handle 'Cons' as a special operator term.
     A == 'Cons', !, s2tl(SList, List), Term =.. [A | List].
s2t([A | SList], Term):-
     % Handle '=' as a special operator term.
     A == '=', !, s2tl(SList, List), Term =.. [A | List].
s2t(List, Term):-
     % Recursively transform list items to `t` compound.
     is_list(List), !, maplist(s2t, List, TermList),
     compound_name_arguments(Term, t, TermList), !.
s2t(STerm, Term):-
     % Default transformation using `s2tl`.
     s2tl(STerm, Term), !.

%!  mlog_sym(@Sym) is nondet.
%
%   Defines a Metta logic symbol.
%
%   @arg Sym The symbol to define in Metta logic.
%
mlog_sym('@').

%!  untyped_to_metta(+I, -O) is det.
%
%   Converts an untyped term `I` to a Metta-compatible form `O`, handling special
%   constructs such as `exec/1`.
%
%   This predicate processes the input term `I`, applying transformations through
%   helper predicates like `mfix_vars1/2`, `cons_to_c/2`, and `cons_to_l/2` to
%   ensure compatibility with Metta's expected format.
%
%   @arg I  The input untyped term to convert.
%   @arg O  The resulting Metta-compatible form.
%
%   @example
%     % Convert an untyped exec term to Metta format.
%     ?- untyped_to_metta(exec(foo), Result).
%     Result = exec(foo).
%

%untyped_to_metta(I, exec(O)):- compound(I), I = exec(M), !, untyped_to_metta(M, O).
untyped_to_metta(I, O):-
     must_det_ll((trly(mfix_vars1, I, M),
         trly(cons_to_c, M, OM),
         trly(cons_to_l, OM, O))).

%!  trly(+P2, +A, -B) is det.
%
%   Applies predicate `P2` iteratively to `A`, resulting in `B` once convergence is reached.
%
%   This predicate repeatedly applies `P2` to `A` until the result no longer changes,
%   allowing recursive transformations to reach a stable form.
%
%   @arg P2  The predicate to apply iteratively.
%   @arg A   The initial term to transform.
%   @arg B   The resulting transformed term.
%
trly(P2, A, B):-
     % Apply P2 and continue if A and M differ.
     once(call(P2, A, M)), A \=@= M, !, trly(P2, M, B).
trly(_, A, A).

%!  mfix_vars1(+I, -O) is det.
%
%   Transforms various terms in `I` into normalized or Metta-compatible forms in `O`.
%
%   This predicate applies specific mappings for variable-like symbols, booleans,
%   strings, bracketed expressions, and lists to ensure compatibility with Metta logic.
%
%   @arg I  The input term to transform.
%   @arg O  The resulting transformed term.
%
%   @example
%     % Transform a string representation of true to Metta format.
%     ?- mfix_vars1(true, Result).
%     Result = 'True'.
%
mfix_vars1(I, O):-
     % If `I` is a variable, unify `I` with `O`.
     var(I), !, I = O.
mfix_vars1('$_', '$VAR'('_')).
mfix_vars1('$', '$VAR'('__')).
mfix_vars1(I, '$VAR'(O)):-
     % Handle atoms prefixed with `$`, converting to `'$VAR'` notation.
     atom(I), symbol_concat('$', N, I), symbol_concat('_', N, O).
%mfix_vars1('$t','$VAR'('T')):-!.
%mfix_vars1('$T','$VAR'('T')):-!.
%mfix_vars1(I,O):- I=='T',!,O='True'.
%mfix_vars1(I,O):- I=='F',!,O='False'.
%mfix_vars1(I,O):- is_i_nil(I),!,O=[].
mfix_vars1(I, O):-
     % Convert 'true' and 'false' to capitalized forms.
     I == 'true', !, O = 'True'.
mfix_vars1(I, O):- I == 'false', !, O = 'False'.
mfix_vars1('$STRING'(I), O):-
     % Handle `$STRING` term, convert directly if `I = O`.
     I = O, !.
mfix_vars1('$STRING'(I), O):-
     % Convert `$STRING` to a string if symbols are disabled.
     \+ string_to_syms, mfix_vars1(I, OO), text_to_string(OO, O), !.
%mfix_vars1('$STRING'(I),O):- \+ string_to_syms, text_to_string(I,O),!.
mfix_vars1('$STRING'(I), O):-
     % Convert `$STRING` to list if symbols are enabled.
     !, mfix_vars1(I, M), atom_chars(O, M), !.
%mfix_vars1('$STRING'(I),O):- !, mfix_vars1(I,M),name(O,M),!.
mfix_vars1([H | T], O):-
     % Handle list format with square brackets.
     H == '[', is_list(T), last(T, L), L == ']', append(List, [L], T), !, O = ['[...]', List].
mfix_vars1([H | T], O):-
     % Handle list format with curly braces.
     H == '{', is_list(T), last(T, L), L == '}', append(List, [L], T), !, O = ['{...}', List].
mfix_vars1([H | T], O):-
     % Handle nested lists with curly braces.
     is_list(T), last(T, L), L == '}', append(List, [L], T),
     append(Left, ['{' | R], List), append([H | Left], [['{}', R]], NewList), mfix_vars1(NewList, O).
mfix_vars1('$OBJ'(claz_bracket_vector, List), O):-
     % Convert `claz_bracket_vector` objects to list form.
     is_list(List), !, O = ['[...]', List].
mfix_vars1(I, O):-
     % Transform square bracket terms.
     I = ['[', X, ']'], nonvar(X), !, O = ['[...]', X].
mfix_vars1(I, O):-
     % Transform curly bracket terms.
     I = ['{', X, '}'], nonvar(X), !, O = ['{...}', X].
mfix_vars1('$OBJ'(claz_bracket_vector, List), Res):-
     % Append brackets to `claz_bracket_vector`.
     is_list(List), !, append(['[' | List], [']'], Res), !.
mfix_vars1(I, O):-
     % Special handling for quoted terms.
     I == [Quote, S], Quote == quote, S == s, !, O = is.
mfix_vars1([K, H | T], Cmpd):-
     % Attempt to form a compound term (currently fails).
     fail, atom(K), mlog_sym(K), is_list(T),
     mfix_vars1([H | T], [HH | TT]), atom(HH), is_list(TT), !,
     compound_name_arguments(Cmpd, HH, TT).
%mfix_vars1([H|T],[HH|TT]):- !, mfix_vars1(H,HH),mfix_vars1(T,TT).
mfix_vars1(List, ListO):-
     % Recursively apply transformation to lists.
     is_list(List), !, maplist(mfix_vars1, List, ListO).
mfix_vars1(I, O):-
     % Convert strings to atoms if symbol representation is enabled.
     string(I), string_to_syms, !, atom_string(O, I).
mfix_vars1(I, O):-
     % Recursively transform compound terms.
     compound(I), !, compound_name_arguments(I, F, II), F \== '$VAR', maplist(mfix_vars1, II, OO), !, compound_name_arguments(O, F, OO).
mfix_vars1(I, O):-
     % Return `I` if it's neither a variable nor symbol.
     \+ symbol(I), !, I = O.
mfix_vars1(I, I).

%!  string_to_syms is nondet.
%
%   Indicates whether strings should be converted to symbols.
%
%   This predicate currently fails by default, disabling symbol conversion for strings.
%
string_to_syms :- fail.

%!  no_cons_reduce is det.
%
%   Defines a flag or predicate placeholder indicating no reduction for cons terms.
%
%   This predicate is typically used as a control flag for terms that should avoid
%   reduction when handled in certain contexts.
%
no_cons_reduce.

%!  svar_fixvarname_dont_capitalize(+M, -O) is det.
%
%   Fixes variable names, avoiding capitalization adjustments.
%
%   This predicate serves as an override for `svar_fixvarname/2`, ensuring that
%   `O` is assigned directly from `M` without capitalization changes.
%
%   @arg M  The input term or variable name to fix.
%   @arg O  The resulting fixed variable name.
%
svar_fixvarname_dont_capitalize(O, O) :- !.
svar_fixvarname_dont_capitalize(M, O):-
     % Use `svar_fixvarname/2` if direct assignment fails.
     svar_fixvarname(M, O), !.


%!  dvar_name(+N, -O) is det.
%
%   Converts a variable name `N` into a standardized format `O`, handling underscores,
%   integers, and capitalization.
%
%   This predicate performs various transformations on `N` to produce `O`, which is suitable
%   for Metta logic compatibility. Handles integers, symbols with underscores, and capitalization.
%
%   @arg N  The input variable name to transform.
%   @arg O  The resulting standardized variable name.
%

%dvar_name(t,'T'):- !.
dvar_name(N, O):-
     % If `N` starts with an underscore, retain `O = N`.
     symbol_concat('_', _, N), !, O = N.
dvar_name(N, O):-
     % Handle integer `N` by appending an underscore prefix.
     integer(N), symbol_concat('_', N, O).
dvar_name(N, O):-
     % If `N` is an atom representing a number, convert it.
     atom(N), atom_number(N, Num), dvar_name(Num, O), !.
dvar_name(N, O):-
     % Convert non-symbol `N` to a formatted string.
     \+ symbol(N), !, format(atom(A), '~w', [N]), dvar_name(A, O).
dvar_name(N, O):-
     % Default conversion by prefixing underscore.
     !, format(atom(A), '_~w', [N]), dvar_name(A, O).
%dvar_name('',''):-!. % $
%dvar_name('_','__'):-!. % $_
dvar_name(N, O):-
     % Retain names starting with underscores.
     symbol_concat('_', _, N), !, symbol_concat('_', N, O).
dvar_name(N, O):-
     % Apply capitalization rules if applicable.
     svar_fixvarname_dont_capitalize(N, O), !.
dvar_name(N, O):-
     % Convert to variable name format by mapping characters.
     must_det_ll((atom_chars(N, Lst), maplist(c2vn, Lst, NList), symbolic_list_concat(NList, S), svar_fixvarname_dont_capitalize(S, O))), !.

%!  c2vn(+A, -AA) is det.
%
%   Maps a character `A` to a valid Prolog variable name format in `AA`.
%
%   @arg A   The character to convert.
%   @arg AA  The resulting character or symbol in Prolog variable format.
%
c2vn(A, A):-
     % Retain identifier characters and starting characters for variables.
     char_type(A, prolog_identifier_continue), !.
c2vn(A, A):- char_type(A, prolog_var_start), !.
c2vn(A, AA):-
     % Convert non-identifier characters to a prefixed numeric format.
     char_code(A, C), symbolic_list_concat(['_C', C, '_'], AA).

%!  cons_to_l(+I, -O) is det.
%
%   Converts terms containing cons cells to a list format, respecting `no_cons_reduce/0`.
%
%   @arg I  The input term to convert.
%   @arg O  The resulting list or transformed term.
%
cons_to_l(I, I):-
     % Skip transformation if `no_cons_reduce/0` is set.
     no_cons_reduce, !.
cons_to_l(I, O):-
     % Retain variables without modification.
     var(I), !, O = I.
cons_to_l(I, O):-
     % Convert `nil` or empty lists.
     is_i_nil(I), !, O = [].
cons_to_l(I, O):- I == 'nil', !, O = [].
cons_to_l(C, O):-
     % Retain non-compound terms.
     \+ compound(C), !, O = C.
cons_to_l([Cons, H, T | List], [HH | TT]):-
     % Handle `Cons` cells by recursively transforming `H` and `T`.
     List == [], atom(Cons), is_cons_f(Cons), t_is_ttable(T), cons_to_l(H, HH), !, cons_to_l(T, TT).
cons_to_l(List, ListO):-
     % Apply transformation recursively to list elements.
     is_list(List), !, maplist(cons_to_l, List, ListO).
cons_to_l(I, I).

%!  cons_to_c(+I, -O) is det.
%
%   Converts terms containing cons cells to a compound format, respecting `no_cons_reduce/0`.
%
%   @arg I  The input term to convert.
%   @arg O  The resulting compound or transformed term.
%
cons_to_c(I, I):-
     % Skip transformation if `no_cons_reduce/0` is set.
     no_cons_reduce, !.
cons_to_c(I, O):-
     % Retain variables without modification.
     var(I), !, O = I.
cons_to_c(I, O):-
     % Convert `nil` or empty lists.
     is_i_nil(I), !, O = [].
cons_to_c(I, O):- I == 'nil', !, O = [].
cons_to_c(C, O):-
     % Retain non-compound terms.
     \+ compound(C), !, O = C.
cons_to_c([Cons, H, T | List], [HH | TT]):-
     % Handle `Cons` cells by recursively transforming `H` and `T`.
     List == [], atom(Cons), is_cons_f(Cons), t_is_ttable(T), cons_to_c(H, HH), !, cons_to_c(T, TT).
cons_to_c(I, O):-
     % Recursively transform compound terms.
     \+ is_list(I), compound_name_arguments(I, F, II), maplist(cons_to_c, II, OO), !, compound_name_arguments(O, F, OO).
cons_to_c(I, I).

%!  t_is_ttable(+T) is nondet.
%
%   Checks if `T` is a valid table or list structure for use in Metta terms.
%
%   This predicate ensures that `T` follows specific formats like `Cons` lists or other
%   structures that conform to Metta's table handling rules.
%
%   @arg T  The term to check.
%
t_is_ttable(T):-
     % Accept if `T` is a variable.
     var(T), !.
t_is_ttable(T):-
     % Accept if `T` represents an empty list or nil.
     is_i_nil(T), !.
t_is_ttable(T):-
     % Accept if `T` is a functionally treated variable.
     is_ftVar(T), !.
t_is_ttable([F | Args]):-
     % Accept if `F` is 'Cons' with a list of arguments.
     F == 'Cons', !, is_list(Args).
t_is_ttable([_ | Args]):-
     % Accept if `Args` is not a list.
     !, \+ is_list(Args).
t_is_ttable(_).

%!  is_cons_f(+Cons) is nondet.
%
%   Determines if `Cons` represents a valid cons term.
%
%   @arg Cons  The term to check.
%
is_cons_f(Cons):- is_cf_nil(Cons, _).

%!  is_cf_nil(+Cons, -Nil) is nondet.
%
%   Matches `Cons` terms with their nil representations.
%
%   @arg Cons The cons-like term.
%   @arg Nil  The nil term associated with `Cons`.
%
is_cf_nil('Cons', 'NNNil').
%is_cf_nil('::', 'nil').

%!  is_i_nil(+I) is nondet.
%
%   Checks if `I` represents an empty or nil structure in Metta.
%
%   @arg I  The term to check for nil.
%
is_i_nil(I):- is_cf_nil('Cons', Nil), I == Nil.


%!  connlf is det.
%
%   Outputs a line feed unless silent loading or compatibility settings prevent it.
%
%   This predicate checks if silent loading is enabled and calls `not_compat_io/1`
%   to output a newline if permitted.
%
connlf:- check_silent_loading, not_compat_io((format('~N'))).

%!  connl is det.
%
%   Outputs a newline unless silent loading or compatibility settings prevent it.
%
connl:- check_silent_loading, not_compat_io((nl)).

%!  check_silent_loading is det.
%
%   Checks if silent loading is enabled. This predicate can trigger debugging behavior
%   when uncommented.
%

% check_silent_loading:- silent_loading,!,trace,break.
check_silent_loading.

%!  silent_loading is nondet.
%
%   Succeeds if the current loading mode is silent, based on options and environment.
%
%   This predicate checks various conditions, such as conversion mode or trace settings,
%   to determine if silent loading is enabled.
%
silent_loading:- option_value('load', 'silent'), !.
silent_loading:- is_converting, !.
silent_loading:- option_value('html', 'True'), !, fail.
silent_loading:- option_value('trace-on-load', 'False'), !.

%!  uncompound(+OBO, -Src) is det.
%
%   Recursively converts compound terms in `OBO` to a flattened source list `Src`.
%
%   This predicate simplifies compound terms by extracting their name and arguments
%   and converting them into a list form.
%
%   @arg OBO The original compound term or variable.
%   @arg Src The resulting list or source term.
%
uncompound(OBO, Src):-
     % Handle non-compound terms.
     \+ compound(OBO), !, Src = OBO.
uncompound('$VAR'(OBO), '$VAR'(OBO)):- !.
uncompound(IsList, Src):-
     % Recursively process list elements.
     is_list(IsList), !, maplist(uncompound, IsList, Src).
uncompound([Is | NotList], [SrcH | SrcT]):-
     % Convert head and tail of lists.
     !, uncompound(Is, SrcH), uncompound(NotList, SrcT).
uncompound(Compound, Src):-
     % Convert compound terms to list format.
     compound_name_arguments(Compound, Name, Args), maplist(uncompound, [Name | Args], Src).

%!  assert_to_metta(+OBO) is det.
%
%   Asserts a term `OBO` into the Metta knowledge base, processing it into a datum structure.
%
%   This predicate handles term processing to meet Metta's requirements, including
%   creating a datum structure, declaring the predicate, and performing the assertion.
%
%   @arg OBO The term to assert in the Metta knowledge base.
%
assert_to_metta(_):-
    % Exit if file limit is reached.
     reached_file_max,!.
assert_to_metta(OBO):-
    % Process `OBO` into a datum and assert.
     must_det_ll((OBO =.. [Fn | DataLL],
         maplist(better_arg, DataLL, DataL),
         into_datum(Fn, DataL, Data),
         functor(Data, Fn, A), decl_fb_pred(Fn, A),
         real_assert(Data), !,
         incr_file_count(_))).
assert_to_metta(OBO):-
 % Alternative processing method for assertions with additional validation
 ignore(( A>=2,A<700,
  OBO=..[Fn|Cols],
 must_det_ll((
  make_assertion4(Fn,Cols,Data,OldData),
  functor(Data,FF,AA),
  decl_fb_pred(FF,AA),
  ((fail,call(Data))->true;(
   must_det_ll((
     real_assert(Data),
     incr_file_count(_),
     ignore((((should_show_data(X),
       ignore((fail,OldData\==Data,write('; oldData '),write_src(OldData),format('  ; ~w ~n',[X]))),
       write_src(Data),format('  ; ~w ~n',[X]))))),
     ignore((
       fail, option_value(output_stream,OutputStream),
       is_stream(OutputStream),
       should_show_data(X1),X1<1000,must_det_ll((display(OutputStream,Data),writeln(OutputStream,'.'))))))))))))),!.

%!  assert_MeTTa(+OBO) is det.
%
%   Asserts a term `OBO` into the Metta knowledge base, using `assert_to_metta/1`.
%
%   This predicate directly calls `assert_to_metta/1` to process and assert the term.
%   Additional behavior (e.g., heartbeat) is currently commented out but can be re-enabled
%   if needed.
%
%   @arg OBO  The term to assert in the Metta knowledge base.
%
assert_MeTTa(OBO):- !, assert_to_metta(OBO).
%assert_MeTTa(OBO):- !, assert_to_metta(OBO),!,heartbeat.

/*
assert_MeTTa(Data):- !, heartbeat, functor(Data,F,A), A>=2,
   decl_fb_pred(F,A),
   incr_file_count(_),
   ignore((((should_show_data(X),
       write(newData(X)),write(=),write_src(Data))))),
   assert(Data),!.
*/

%:- dynamic((metta_type/3,metta_defn/3,get_metta_atom/2)).

:- dynamic(progress_bar_position/1).

%!  init_progress_bar(+Width) is det.
%
%   Initializes a progress bar with a specified `Width` and records its starting position.
%
%   This predicate sets up a visual progress bar in the output stream with a width of
%   empty spaces to be filled as the process progresses. The position is saved to allow
%   for efficient updating.
%
%   @arg Width  The width of the progress bar in characters.
%
init_progress_bar(Width) :-
    current_output(Stream),
    stream_property(Stream, position(Pos)),
    % Record the starting position of the progress bar.
    asserta(progress_bar_position(Pos)),
    write('['),
    forall(between(1, Width, _), write(' ')),
    write(']'),
    flush_output.

%!  update_progress_bar(+Current, +Total, +Width) is det.
%
%   Updates the progress bar based on the `Current` progress out of `Total` with
%   a specified `Width`.
%
%   This predicate checks if the progress barâ€™s position has changed and redraws it if necessary.
%   It then calculates the filled and remaining sections and displays the progress visually.
%
%   @arg Current The current progress value.
%   @arg Total   The total value representing completion.
%   @arg Width   The width of the progress bar in characters.
%
update_progress_bar(Current, Total, Width) :-
    current_output(Stream),
    % Get the current position.
    stream_property(Stream, position(CurrentPos)),
    % Get the remembered starting position.
    progress_bar_position(SavedPos),
    % If positions differ, redraw the entire progress bar.
    (   SavedPos \= CurrentPos
    ->  redraw_progress_bar(Width)
    ;   true
    ),
    % Calculate filled portion based on progress percentage.
    Percentage is Current / Total,
    Filled is round(Percentage * Width),
    write('\r['),
    forall(between(1, Filled, _), write('#')),
    Remaining is Width - Filled,
    forall(between(1, Remaining, _), write(' ')),
    write(']'),
    flush_output.

%!  redraw_progress_bar(+Width) is det.
%
%   Redraws the progress bar if its position has changed, reinitializing it.
%
%   This predicate clears the current line by adding a newline and reinitializes
%   the progress bar at the new position with the specified `Width`.
%
%   @arg Width The width of the progress bar in characters.
%
redraw_progress_bar(Width) :- nl,init_progress_bar(Width).


% Adjusted example predicate for 1 million steps
progress_bar_example :-
    TotalSteps = 1000000,  % Adjust the total steps to 1 million
    ProgressBarWidth = 30,
    init_progress_bar(ProgressBarWidth),
    between(1, TotalSteps, Step),
    update_progress_bar(Step, TotalSteps, ProgressBarWidth),
    % Simulate work
    sleep(0.00001),  % Adjust sleep time as needed for demonstration
    fail. % Continue looping until between/3 fails
progress_bar_example.

:- dynamic(using_corelib_file/0).
:- dynamic(really_using_corelib_file/0).

%!  use_corelib_file is det.
%
%   Ensures the core library file is loaded and interpreted if not already in use.
%
%   This predicate checks if the core library is in use and, if not, attempts to load it by
%   calling `really_use_corelib_file/0`. It uses a dynamic flag `using_corelib_file` to track
%   whether the core library has been loaded.
%
use_corelib_file :- using_corelib_file, !.
use_corelib_file :-
     % Mark core library as in use and attempt to load it.
     asserta(using_corelib_file), fail.
use_corelib_file :- really_use_corelib_file, !.
use_corelib_file :- !.
%use_corelib_file :- really_use_corelib_file, !.

%!  really_use_corelib_file is det.
%
%   Loads the core library file and generates interpreter stubs as needed.
%
%   This predicate loads the core library using `load_corelib_file/0` and generates
%   stubs for interpreting core library symbols via `generate_interpreter_stubs/0`.
%
really_use_corelib_file :- load_corelib_file, generate_interpreter_stubs.

% Dynamic predicate to track if interpreter stubs have been generated.
:- dynamic(did_generate_interpreter_stubs/0).

%!  generate_interpreter_stubs is det.
%
%   Generates interpreter stubs for core library symbols if they have not been created yet.
%
%   This predicate checks if stubs have already been generated, and if not, it iterates over
%   symbols defined in the core library to create interpreter stubs for each.
%
generate_interpreter_stubs :-
     % Avoid generating stubs multiple times.
     did_generate_interpreter_stubs, !.
generate_interpreter_stubs :-
     % Generate stubs for each core library symbol.
     asserta(did_generate_interpreter_stubs),
     forall(metta_type('&corelib', Symb, Def),
            gen_interp_stubs('&corelib', Symb, Def)).

% Dynamic and multifile declaration for metta_atom_deduced/2.
:- dynamic(metta_atom_deduced/2).
:- multifile(metta_atom_deduced/2).

%!  metta_atom_deduced(+Source, +Term) is nondet.
%
%   Determines if a `Term` is part of the core library, logging the term if so.
%
%   This predicate checks if the `Term` originates from the `&corelib` source and
%   meets the core library type requirements.
%
%   @arg Source  The source of the term, expected to be `&corelib`.
%   @arg Term    The term to verify.
%
metta_atom_deduced('&corelib', Term) :- fail,
     % Log terms matching core library types.
     %\+ did_generate_interpreter_stubs,
     metta_atom_corelib_types(Term),
     write_src_uo(metta_atom_corelib_types(Term)).

%!  load_corelib_file is det.
%
%   Loads the core library file if it hasn't already been loaded.
%
%   This predicate first checks if the core library is already in use (`really_using_corelib_file`).
%   If not, it attempts to load the file from the Metta source directory. Currently, it defaults to
%   `stdlib_mettalog.metta`, with `corelib.metta` as a commented alternative.
%
%   @example
%     % Load the core library if it's not already loaded.
%     ?- load_corelib_file.
%
load_corelib_file :- really_using_corelib_file, !.
%load_corelib_file :- is_metta_src_dir(Dir), really_use_corelib_file(Dir, 'corelib.metta'), !.
load_corelib_file :-
     % Load the standard Metta logic file from the source directory.
     must_det_lls((is_metta_src_dir(Dir), really_use_corelib_file(Dir, 'stdlib_mettalog.metta'),
     metta_atom('&corelib', [':', 'Any', 'Type']),
     really_use_corelib_file(Dir, 'corelib.metta'))).
% !(import! &corelib "src/canary/stdlib_mettalog.metta")

%!  really_use_corelib_file(+Dir, +File) is det.
%
%   Loads a specified core library `File` from a directory `Dir` and initializes it.
%
%   This predicate constructs the absolute path of `File` relative to `Dir` and, if the file exists,
%   includes it in the Metta knowledge base. It sets up specific environment flags (e.g., fast buffer)
%   and then asserts that the core library is in use.
%
%   @arg Dir   The directory containing the core library file.
%   @arg File  The core library file to load.
%
really_use_corelib_file(Dir, File) :-
    must_det_ll((absolute_file_name(File, Filename, [relative_to(Dir)]),
     exists_file(Filename),
     debug(lsp(main), "~q", [start_really_use_corelib_file(Dir, File)]),
     locally(nb_setval(may_use_fast_buffer, t),
        locally(nb_setval(debug_context, stdlib),
          locally(nb_setval(compiler_context, builtin),
             locally(nb_setval(suspend_answers, true),
            without_output(include_metta_directory_file('&corelib', Dir, Filename)))))),
     asserta(really_using_corelib_file),
     debug(lsp(main), "~q", [end_really_use_corelib_file(Dir, File)]))).

without_output(G):- is_devel,!,call(G).
without_output(G):- with_output_to(string(_), G).
:- nb_setval(debug_context, 'run').

