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



when_tracing(Goal):- tracing,!,notrace(Goal),!.
when_tracing(_).

:- multifile(user:asserted_metta_pred/2).
:- dynamic(user:asserted_metta_pred/2).

exists_virtually(corelib).
exists_virtually(stdlib).

path_chars(A,C):- symbol_chars(A,C).

with_wild_path(Fnicate, Dir):-
  working_directory(PWD,PWD),
  wwp(Fnicate, Dir).

inner_compound(Inner,'.',Inner):- \+ compound(Inner),!.
inner_compound(Cmpd,Outter,Inner):-
    compound_name_arguments(Cmpd,F,[X|Args]),
    compound_name_arguments(Outter,F,[Midder|Args]),
    inner_compound(X,Midder,Inner).

afn(A,B):- quietly(absolute_file_name(A,B)).
afn(A,B,C):- quietly(absolute_file_name(A,B,C)).

% Process a file or directory path with a given predicate.
wwp(Fnicate, Dir) :- extreme_debug(fbug(wwp(Fnicate, Dir))),fail.
wwp(_Fnicate, []) :- !.
wwp(_Fnicate, Virtual) :- exists_virtually(Virtual),!.
wwp(Fnicate, Virtual) :- var(Virtual),!,throw(var_wwp(Fnicate, Virtual)).
wwp(Fnicate, Dir) :-  is_scryer, symbol(Dir), !, must_det_ll((path_chars(Dir,Chars), wwp(Fnicate, Chars))).

% catches charlist and codelist filenames
wwp(Fnicate, Chars) :- is_list(Chars), catch(name(File,Chars),_,fail), Chars\==File,!, wwp(Fnicate, File).

wwp(Fnicate, File) :- is_list(File), !,
   must_det_ll((maplist(wwp(Fnicate), File))).

wwp(Fnicate, Cmpd):- compound(Cmpd),
  inner_compound(Cmpd,Outter,Inner),!,
  afn(Outter, Dir,[solutions(all), access(read), file_errors(fail)]),
  with_cwd(Dir,wwp(Fnicate, Inner)),!.

% this is what captures string in SWI-Prolog
wwp(Fnicate, Chars) :-  \+ is_scryer, \+ symbol(Chars), !, must_det_ll((name(Atom,Chars), wwp(Fnicate, Atom))).

wwp(Fnicate, File) :- exists_file(File), !, must_det_ll(( call(Fnicate, File))).

wwp(Fnicate, ColonS) :- fail, symbolic(ColonS), symbol_contains(ColonS, ':'),!,
  symbolic_list_concat([Top|Rest],':',ColonS),
  symbolic_list_concat(Rest,':',FileNext),
  when_tracing(listing(is_metta_module_path)),
  find_top_dirs(Top,Dir),
  ((fail,symbol_length(FileNext,0))
   -> wwp(Fnicate, Dir)
   ; (exists_directory(Dir)
       -> with_cwd(Dir,wwp(Fnicate, FileNext))
       ; fail)),!.

wwp(Fnicate, ColonS) :- symbolic(ColonS), symbol_contains(ColonS, ':'),!,
  symbolic_list_concat([Top|Rest],':',ColonS),
  symbolic_list_concat(Rest,':',FileNext),!,
  when_tracing(listing(is_metta_module_path)),
  must_det_ll((call((
  quietly(find_top_dirs(Top,Dir)),
  exists_directory(Dir),
  with_cwd(Dir,wwp(Fnicate, FileNext)))))),!.

wwp(Fnicate, File) :-
  symbol_contains(File, '*'),
  expand_file_name(File, List),
  maplist(wwp(Fnicate), List),!.

wwp(Fnicate, Dir) :-  exists_directory(Dir),
  quietly(afn_from('__init__.py', PyFile, [access(read), file_errors(fail), relative_to(Dir)])),
  wwp(Fnicate, PyFile).


wwp(Fnicate, File) :-
  \+ exists_directory(File), \+ exists_file(File), %\+ symbol_contains(File,'.'),
  extension_search_order(Ext),
  symbolic_list_concat([File|Ext],MeTTafile),
  exists_file(MeTTafile),
  call(Fnicate, MeTTafile).

wwp(Fnicate, File) :-
  \+ exists_directory(File), \+ exists_file(File), symbol_contains(File,'..'),
  extension_search_order(Ext),
  symbolic_list_concat([File|Ext],MeTTafile0),
  afn_from(MeTTafile0, MeTTafile, [access(read), file_errors(fail)]),
  exists_file(MeTTafile),
  call(Fnicate, MeTTafile).

wwp(Fnicate, File) :-
  exists_directory(File),
  directory_file_path(File, '*.*sv', Wildcard),
  expand_file_name(Wildcard, List), !,
  maplist(Fnicate, List).

wwp(Fnicate, Dir) :-  exists_directory(Dir), !,
  must_det_ll((directory_files(Dir, Files),
  maplist(directory_file_path(Dir,Files),Paths),
  maplist(path_chars,Paths,CharPaths),
  maplist(wwp(Fnicate), CharPaths))), !.

wwp(Fnicate, File) :- must_det_ll((call(Fnicate, File))).

extension_search_order(['.metta']).
extension_search_order(['.py']).
extension_search_order(['']).

:- if( \+ current_predicate(load_metta_file/2)).
load_metta_file(Self,Filemask):- symbol_concat(_,'.metta',Filemask),!, load_metta(Self,Filemask).
load_metta_file(_Slf,Filemask):- load_flybase(Filemask).
:- endif.

afn_from(RelFilename,Filename):-
  afn_from(RelFilename,Filename,[]).

afn_from(RelFilename,Filename,Opts):-
   select(relative_to(RelFrom),Opts,NewOpts),
   afn_from(RelFrom,RelFromNew,NewOpts),
   quietly(afn(RelFilename,Filename,[relative_to(RelFromNew)|NewOpts])).
afn_from(RelFilename,Filename,Opts):-
   is_metta_module_path(ModPath),
   quietly(afn(RelFilename,Filename,[relative_to(ModPath)|Opts])).

register_module(Dir):- current_self(Space), register_module(Space,Dir).

register_module(Space,Path):-
    register_module(Space,'%top%',Path),
    file_directory_name(Path,Dir),
    file_base_name(Path, ModuleName),
    register_module(Space,ModuleName,Dir).

register_module(Space,ModuleName,Dir):-
    space_name(Space,SpaceName),
    absolute_dir(Dir,AbsDir),
    asserta(is_metta_module_path(SpaceName,ModuleName,AbsDir)).


find_top_dirs(Top,Dir):- current_self(Self),space_name(Self,SpaceName), find_top_dirs(SpaceName,Top,Dir).

find_top_dirs(SpaceName,Top,Abs):- is_metta_module_path(SpaceName,Top,Abs).
find_top_dirs(SpaceName,Top,Dir):- is_metta_module_path(SpaceName,'%top%',Root),absolute_dir(Top,Root,Dir).
find_top_dirs(SpaceName,Top,Dir):- working_directory(PWD,PWD),
   parent_dir_of(PWD,Top,Dir), assert(is_metta_module_path(SpaceName,Top,Dir)).

parent_dir_of(PWD,Top,Dir):- directory_file_path(Parent,TTop,PWD),
   (TTop==Top->Dir=PWD;parent_dir_of(Parent,Top,Dir)).


space_name(Space,SpaceName):- symbol(Space),!,SpaceName = Space,!.
space_name(Space,SpaceName):- is_space_name(SpaceName), same_space(SpaceName,Space),!.
space_name(Space,SpaceName):- 'get-atoms'(Space,['space-symbol',SpaceName]),!.

same_space(Space1,Space2):- Space1=Space2.
same_space(SpaceName1,Space2):- symbol(SpaceName1),eval(SpaceName1,Space1),!,same_space(Space2,Space1).

absolute_dir(Dir,AbsDir):- afn(Dir, AbsDir, [access(read), file_errors(fail), file_type(directory)]).
absolute_dir(Dir,From,AbsDir):- afn(Dir, AbsDir, [relative_to(From),access(read), file_errors(fail), file_type(directory)]),!.

:- dynamic(is_metta_module_path/3).
:- dynamic(is_metta_module_path/1).
is_metta_module_path('.').




%!  when_circular(+Key, :Goal, +Item, :DoThis) is semidet.
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
%   @arg Goal  The goal to execute if no circular dependencies are detected.
%   @arg Error The error term to throw in case of circular dependency.
%
without_circular_error(Goal, Error) :-
    % Use when_circular/4 to check for circular dependencies and throw an error when detected.
    when_circular('$circular_goals', Goal, Goal, throw(error(Error, _))).

% Predicate to load a Metta file.
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
load_metta(_Self, Filename):-
    % Special case: if the Filename is '--repl', start the REPL instead of loading a file.
    Filename == '--repl', !, repl.
load_metta(Self, Filename):-
    % Call without_circular_error/2 to prevent circular dependencies when loading files.
    without_circular_error(load_metta1(Self, Filename),
        missing_exception(load_metta(Self, Filename))).

% Helper predicate for actually loading the Metta file.
load_metta1(Self, Filename):-
    % Check if the Filename is not a valid symbol or the file does not exist.
    (\+ symbol(Filename); \+ exists_file(Filename)),!,
    % Use with_wild_path to handle wildcard paths and load the file if it matches.
    with_wild_path(load_metta(Self), Filename), !,
    % Call loonit_report (likely for logging or reporting purposes).
    loonit_report.
load_metta1(Self, RelFilename):-
    % Ensure that the relative filename is a path and exists as a valid file.
    must_det_ll((symbol(RelFilename), % @TODO or a string?
    exists_file(RelFilename),!,
    % Convert the relative filename to an absolute filename.
    afn_from(RelFilename, Filename),
    % Set a local flag for garbage collection and track the file loading process.
    locally(set_prolog_flag(gc, true),
    track_load_into_file(Filename,
        % Include the file into the current module.
        include_metta(Self, RelFilename))))).

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
import_metta(Self, Filename):-
    % Define the goal for importing the Metta file.
    About = import_metta1(Self, Filename),
    % Use when_circular/4 to handle circular dependencies during imports.
    when_circular('$circular_goals', About, About, complain_if_missing(Filename, About)).

% Predicate to complain if a file is missing or circular dependency is found.
complain_if_missing(Filename, About):-
    % If the file does not exist, print a missing exception message.
    \+ exists_file(Filename), !, write_src_nl(missing_exception(About)).
complain_if_missing(_, About):-
    % If a circular dependency is found, print a circular exception message.
    write_src_nl(circular_exception(About)).

% Helper predicate for actually importing the Metta file.
import_metta1(Self, Module):-
    % If the Module is a valid Python module, extend the current Prolog context with Python.
    current_predicate(py_is_module/1), py_is_module(Module),!,
    must_det_ll(self_extend_py(Self, Module)),!.
import_metta1(Self, Filename):-
    % If the Filename is not a valid symbol or the file does not exist, use wildcards for import.
    (\+ symbol(Filename); \+ exists_file(Filename)),!,
    must_det_ll(with_wild_path(import_metta(Self), Filename)),!.
import_metta1(Self, RelFilename):-
    % Ensure that the relative filename is a symbol and the file exists.
    must_det_ll((
    symbol(RelFilename),
    exists_file(RelFilename),
    % Convert the relative filename to an absolute path.
    absolute_file_name(RelFilename, Filename),
    % Extract the directory path from the filename.
    directory_file_path(Directory, _, Filename),
    % Register the file in the Prolog knowledge base as being part of the Metta context.
    pfcAdd_Now(metta_file(Self, Filename, Directory)),
    % Suspend Prolog answers during the inclusion of the Metta file.
    locally(nb_setval(suspend_answers, true),
    % Include the file and load its content into the directory.
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
include_metta(Self, Filename):-
    % Use without_circular_error/2 to handle circular dependencies for including files.
    without_circular_error(include_metta1(Self, Filename),
        missing_exception(include_metta(Self, Filename))).

% Helper predicate for actually including the Metta file.
include_metta1(Self, Filename):-
    % If the filename is not a valid symbol or the file does not exist, handle wildcards for includes.
    (\+ symbol(Filename); \+ exists_file(Filename)),!,
    must_det_ll(with_wild_path(include_metta(Self), Filename)),!.
include_metta1(Self, RelFilename):-
    % Ensure that the relative filename is a valid symbol and exists.
    must_det_ll((
    symbol(RelFilename),
    exists_file(RelFilename),!,
    % Convert the relative filename to an absolute path.
    afn_from(RelFilename, Filename),
    % Generate a temporary file (if necessary) based on the absolute filename.
    gen_tmp_file(false, Filename),
    % Extract the directory path from the filename.
    directory_file_path(Directory, _, Filename),
    % Register the file in the Prolog knowledge base as being loaded into the current module.
    pfcAdd_Now(metta_file(Self, Filename, Directory)),
    % Register the file as being loaded into the knowledge base.
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    % Include the file's directory content into the current module.
    include_metta_directory_file(Self, Directory, Filename))),
    % Mark the file as loaded into the knowledge base and optionally list its status.
    pfcAdd_Now(user:loaded_into_kb(Self, Filename)),
    nop(listing(user:loaded_into_kb/2)).



% count_lines_up_to(TwoK,Filename, Count).
count_lines_up_to(TwoK,Filename, Count) :-
  open(Filename, read, Stream,[encoding(utf8)]),
  count_lines_in_stream(TwoK,Stream, 0, Count),
  close(Stream).

% count_lines_in_stream(Stream, CurrentCount, FinalCount).
count_lines_in_stream(TwoK,Stream, CurrentCount, FinalCount) :-
  ( CurrentCount >= TwoK
  -> FinalCount = TwoK
  ;  read_line_to_codes(Stream, Codes),
    ( Codes == end_of_file
    -> FinalCount = CurrentCount
    ;  NewCount is CurrentCount + 1,
        count_lines_in_stream(TwoK, Stream, NewCount, FinalCount)
    )
  ).


include_metta_directory_file_prebuilt(Self, _Directory, Filename):-
    symbol_concat(_, '.metta', Filename),
    symbol_concat(Filename, '.qlf', QlfFile),
    exists_file(QlfFile),
    time_file(Filename, MettaTime),
    time_file(QlfFile, QLFTime),
    \+ always_rebuild_temp,
    QLFTime > MettaTime,!, % Ensure QLF file is newer than the METTA file
    pfcAdd_Now(user:loaded_into_kb(Self,Filename)),
    ensure_loaded(QlfFile),!.


include_metta_directory_file_prebuilt(Self,_Directory, Filename):- just_load_datalog,
  symbol_concat(_,'.metta',Filename),
  symbol_concat(Filename,'.datalog',DatalogFile),
  exists_file(DatalogFile),
  time_file(Filename, MettaTime),
  time_file(DatalogFile, DatalogTime),
  DatalogTime > MettaTime, \+ always_rebuild_temp, !, % Ensure Datalog file is newer than the METTA file
    size_file(Filename, MettaSize),
    size_file(DatalogFile, DatalogSize),
    % Ensure the size of the Datalog file is at least 25% of the METTA file
    DatalogSize >= 0.25 * MettaSize,
 % always rebuild
  delete_file(DatalogFile),fail,
    !, % Cut to prevent backtracking
  pfcAdd_Now(user:loaded_into_kb(Self,Filename)),
  ensure_loaded(DatalogFile),!.

include_metta_directory_file_prebuilt(Self,_Directory, Filename):-
  symbol_concat(_,'.metta',Filename),
  symbol_concat(Filename,'.datalog',DatalogFile),
  exists_file(DatalogFile),!,
    size_file(Filename, MettaSize),
    size_file(DatalogFile, DatalogSize),
    % Ensure the size of the Datalog file is at least 25% of the METTA file
    DatalogSize >= 0.25 * MettaSize,
% always rebuild
  delete_file(DatalogFile),fail,
    !, % Cut to prevent backtracking
  convert_datalog_to_loadable(DatalogFile,QlfFile),!,
  exists_file(QlfFile),!,
  pfcAdd_Now(user:loaded_into_kb(Self,Filename)),
  ensure_loaded(QlfFile),!.



include_metta_directory_file(Self,Directory, Filename):-
  include_metta_directory_file_prebuilt(Self,Directory, Filename),!.
include_metta_directory_file(Self, Directory, Filename):-
  count_lines_up_to(2000,Filename, Count), Count > 1980, % \+ use_fast_buffer,
  include_large_metta_directory_file(Self, Directory, Filename), !.
include_metta_directory_file(Self,Directory,Filename):-
  with_cwd(Directory,must_det_ll(setup_call_cleanup(open(Filename,read,In, [encoding(utf8)]),
    must_det_ll( load_metta_file_stream(Filename,Self,In)),
    close(In)))).


% include_large_metta_directory_file(Self, Directory, Filename):- \+ use_fast_buffer, !, locally(nb_setval(may_use_fast_buffer,t), include_metta_directory_file(Self,Directory, Filename)).
include_large_metta_directory_file(Self,_Directory, Filename):-
  once(convert_metta_to_loadable(Filename,QlfFile)),
  exists_file(QlfFile),!,
  pfcAdd_Now(user:loaded_into_kb(Self,Filename)),
  ensure_loaded(QlfFile).


convert_metta_to_datalog(Filename,DatalogFile):-
    % Generate the Datalog file name
  ignore(symbol_concat(Filename,'.datalog',DatalogFile)),
    % Open the METTA file for reading
    setup_call_cleanup(
        open(Filename, read, Input, [encoding(utf8)]),
        % Open the Datalog file for writing
        setup_call_cleanup(
           open(DatalogFile, write, Output, [encoding(utf8)]),
            % Perform the conversion
             must_det_ll(translate_metta_file_to_datalog_io(Filename,Input,Output)),
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
    ))),
    !.  % Prevent backtracking

% atom_subst(+Source, +Replacements, -Result)
% Replacements is a list of Search-Replace pairs.
atom_subst(Source, Replacements, Result) :-
    foldl(replace_in_symbol, Replacements, Source, Result).

% replace_in_symbol(+Search-Replace, +CurrentSource, -NewSource)
% Helper predicate to apply a single search-replace operation.
replace_in_symbol(Search-Replace, CurrentSource, NewSource) :-
    symbolic_list_concat(Split, Search, CurrentSource),
    symbolic_list_concat(Split, Replace, NewSource).


% filename_to_mangled_pred(+Filename, -MangleP)
filename_to_mangled_pred(Filename, MangleP) :-
    get_time(Time),
    symbolic_list_concat(['data', Filename, Time], '_', GS),
    Replacements = [ '.metta_'- '_',
                     '_1710'-'_',
                     '/'- '_',
                 '/'- '_', '.'- '_', '-'- '_', '__'- '_'],
    atom_subst(GS, Replacements, IntermediateResult),
    trim_to_last_nchars(24, IntermediateResult, MangleP).


% trim_to_last_32(+Atom, -TrimmedAtom)
% Trims the given Atom to its last 32 characters, producing TrimmedAtom.
trim_to_last_nchars(Len, Atom, TrimmedAtom) :-
    atom_length(Atom, Length),
    (   Length =< Len
    ->  TrimmedAtom = Atom  % Atom is shorter than or exactly 32 characters, no trimming needed
    ;   Before is Length - 32,
        sub_atom(Atom, Before, 32, _, TrimmedAtom)
    ).


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
    read_sform(Input,Term))),
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
  % teell the user we are done
  flag(translated_forms,TF,TF),
  format(user_error,'~N; Done translating ~w forms: ~q.',
                           [TF,asserted_metta_pred(MangleP2,Filename)]))).

% write comments
write_metta_datalog_term(Output,'$COMMENT'(Term,_,_),_MangleP2,_Lineno):-
  format(Output,"/* ~w */~n",[Term]).
% write executed terms
write_metta_datalog_term(Output,exec(Term),MangleP2,Lineno):-
  format(Output,":-eval_Line(~q,~q,~q).~n",[Term,MangleP2,Lineno]).
% write asserted terms
write_metta_datalog_term(Output,STerm,MangleP2,Lineno):-
  s2t_iz(MangleP2,P,STerm,Term),
  relistify(Term,TermL),
  Data =..[P,Lineno|TermL],
  format(Output,"~q.~n",[Data]).

relistify(Term,TermL):- is_list(Term),!,TermL=Term.
relistify([H|T],TermL):- flatten([H|T],TermL),!.
relistify(Term,[Term]).

eval_Line(A,_B,_C):-
  test_alarm,
  format('~N'), nl, % write_src(eval_Line(A,B,C)),nl,
  eval(A,R),nl,wdmsg(R).

translate_metta_datalog(Input,Output):- translate_metta_datalog('',Input,Output),!.

translate_metta_datalog(_,Input,_):- at_end_of_stream(Input),!.
translate_metta_datalog(Ch,Input,Output):- peek_char(Input,Char),
  translate_metta_datalog(Ch,Input,Output,Char).

translate_metta_datalog(_,Input,Output,')'):- !, get_char(Input,_),
  writeq(Output,']'),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'('):- !,get_char(Input,_),
  write(Output,Ch),writeq(Output,'['),translate_metta_datalog('',Input,Output).
translate_metta_datalog(Ch,Input,Output,Space):-char_type(Space,space),!,
  get_char(Input,Char),  write(Output,Char),translate_metta_datalog(Ch,Input,Output).
translate_metta_datalog(Ch,Input,Output,';'):-!,read_line_to_string(Input, Comment),
  format(Output, '/* ~w */',[Comment]),translate_metta_datalog(Ch,Input,Output).
translate_metta_datalog(Ch,Input,Output,'"'):-!,read_term(Input,Term,[]),
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'`'):-!,read_term(Input,Term,[]),
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'\''):-!,read_term(Input,Term,[]),
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,'$'):-!,
  read_chars_until([type(space),')'],Input,Codes),name(Term,Codes),
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).
translate_metta_datalog(Ch,Input,Output,Peek):-!,
  read_chars_until([type(space),')'],Peek,Input,Codes),name(Term,Codes),
  write(Output,Ch),writeq(Output,Term),translate_metta_datalog(',',Input,Output).

read_chars_until(_StopsBefore,Input,[]):- at_end_of_stream(Input),!.
read_chars_until(StopsBefore,Input,Codes):- peek_char(Input,Char),
      read_chars_until(StopsBefore, Char, Input, Codes).

stops_before([type(Type)|StopsBefore],Char):- char_type(Char,Type); stops_before(StopsBefore,Char).
stops_before([Ch|StopsBefore],Char):-  Ch==Char; stops_before(StopsBefore,Char).

read_chars_until(StopsBefore,Char,_, []):- stops_before(StopsBefore,Char),!.
read_chars_until(StopsBefore, '\\', Input, [Code|Codes]):- get_char(Input,Code),
    read_chars_until(StopsBefore, Input, Codes).
read_chars_until(StopsBefore, Char, Input, [Char|Codes]):- get_char(Input,_),
  read_chars_until(StopsBefore, Input, Codes).

just_load_datalog:-!, true.
may_use_datalog:-!, true.

convert_datalog_to_loadable(DatalogFile,DatalogFile):-just_load_datalog,!.
convert_datalog_to_loadable(DatalogFile,QlfFile):-
  sformat(S,'swipl -g "qcompile(~q)" -t halt',[DatalogFile]),
  shell(S,_),
  file_name_extension(Base, _, DatalogFile),
  file_name_extension(Base,'qlf',QlfFile).

convert_metta_to_loadable(_Filename,_QlfFile):- use_fast_buffer,!, fail.
convert_metta_to_loadable(_Filename,_QlfFile):- \+ may_use_datalog, !.

convert_metta_to_loadable(Filename,QlfFile):-
  must_det_ll((
  convert_metta_to_datalog(Filename,DatalogFile),
  convert_datalog_to_loadable(DatalogFile,QlfFile))),!.

convert_metta_to_loadable(Filename,_):-
  metta_dir(Dir),
  sformat(S,'~w/cheap_convert.sh --verbose=1 ~w',[Dir,Filename]),
  shell(S,Ret),!,Ret==0.

accept_line(_Self,end_of_file):-!.
accept_line(Self,I):- normalize_space(string(Str),I),!,accept_line2(Self,Str),!.

accept_line2(_Self,S):- string_concat(";",_,S),!,writeln(S).
accept_line2(Self,S):- string_concat('(',RS,S),string_concat(M,')',RS),!,
  symbolic_list_concat([F|LL],' ',M),PL =..[F,Self|LL],pfcAdd_Now(PL),!,flag(next_assert,X,X+1),
  if_t((0 is X mod 10_000_000),(writeln(X=PL),statistics)).
accept_line2(Self,S):- fbug(accept_line2(Self,S)),!.


load_metta_file_stream(Filename,Self,In):-
  if_t((atomic(Filename),exists_file(Filename)), size_file(Filename, Size)),
  if_t(var(Size),is_file_stream_and_size(In, Size)),
  %once((is_file_stream_and_size(In, Size),Size>102400) -> P2 = read_sform2 ;
  P2 = read_metta2, %)
  with_option(loading_file,Filename,
  %current_exec_file(Filename),
  must_det_ll((must_det_ll((
      set_exec_num(Filename,1),
      load_answer_file(Filename),
      set_exec_num(Filename,0))),
  load_metta_file_stream_fast(Size,P2,Filename,Self,In)))).

% use_fast_buffer makes tmp .buffer files that get around long load times
use_fast_buffer:- nb_current(may_use_fast_buffer,t).

%:- nb_setval(may_use_fast_buffer,t).
%:- use_fast_buffer.

:- dynamic(metta_file_buffer/5).
:- multifile(metta_file_buffer/5).

prefer_temp(Filename,BufferFile):- \+ exists_file(Filename),!, exists_file(BufferFile).
prefer_temp(Filename,BufferFile):-
    time_file(Filename, FileTime),
    time_file(BufferFile, BufferFileTime),
    BufferFileTime > FileTime,
    size_file(Filename, MettaSize),
    size_file(BufferFile, BufferSize),
    % not truncated ?
    BufferSize >= 0.25 * MettaSize.



load_metta_file_stream_fast(_Size,_P2,Filename,Self,S):- fail,
 symbolic_list_concat([_,_,_|_],'.',Filename),
  \+ option_value(html,true),
  atomic(S),is_stream(S),stream_property(S,input),!,
  repeat,
  read_line_to_string(S,I),
  accept_line(Self,I),
  I==end_of_file,!.

load_metta_file_stream_fast(_Size, _P2, Filename, Self, _In) :-
    symbol_concat(Filename, '.buffer~', BufferFile),
    exists_file(BufferFile),
    (   prefer_temp(Filename,BufferFile)
    ->  (use_fast_buffer, fbugio(using(BufferFile)),ensure_loaded(BufferFile), !, load_metta_buffer(Self, Filename))
    ;   (fbugio(deleting(BufferFile)),delete_file(BufferFile), fail)
    ).

load_metta_file_stream_fast(_Size,_P2,Filename,Self,In):-
  make_metta_file_buffer(use_fast_buffer,Filename,In),
  load_metta_buffer(Self,Filename).



make_metta_file_buffer(TFMakeFile,Filename,In):-
  % maybe time this
  ((
      if_t(TFMakeFile,
         ((symbol_concat(Filename, '.buffer~', BufferFile),
          fbugio(creating(BufferFile)),
          write_bf(BufferFile, ( :- dynamic(metta_file_buffer/5))),
          write_bf(BufferFile, ( :- multifile(metta_file_buffer/5)))))),
      repeat,
            my_line_count(In, LineCount),
            current_read_mode(file,Mode),
            must_det_ll(call(read_metta2, In,Expr)), %write_src(read_metta=Expr),nl,
            subst_vars(Expr, Term, [], NamedVarsList),
            BufferTerm = metta_file_buffer(Mode,Term,NamedVarsList,Filename,LineCount),
            assertz(BufferTerm),
            if_t(TFMakeFile,write_bf(BufferFile,BufferTerm)),

      flush_output,
      at_end_of_stream(In),!)),!.
      %listing(metta_file_buffer/5),
      



always_rebuild_temp:- true.

write_bf(BufferFile,BufferTerm):-
  setup_call_cleanup(open(BufferFile,append,Out),
       format(Out,'~q.~n',[BufferTerm]),
       close(Out)).


my_line_count(In, seek($,0,current,CC)):-
   stream_property(In,reposition(true)),
   seek(In,0,current,CC),fail.
my_line_count(In,/*position*/(Pos)):-
   stream_property(In,position(Pos)).


load_metta_buffer(Self,Filename):-
   set_exec_num(Filename,1),
   load_answer_file(Filename),
   set_exec_num(Filename,0),
   pfcAdd_Now(user:loaded_into_kb(Self,Filename)),
   forall(metta_file_buffer(Mode,Expr,NamedVarsList,Filename,_LineCount),
       (maplist(maybe_assign,NamedVarsList),
        must_det_ll((((do_metta(file(Filename),Mode,Self,Expr,_O)))
             ->true
              ; (trace,pp_m(unknown_do_metta(file(Filename),Mode,Self,Expr))))))).



%read_metta(In,Expr):- current_input(CI), \+ is_same_streams(CI,In), !, read_sform(In,Expr).
read_metta(_,O):- clause(t_l:s_reader_info(O),_,Ref),erase(Ref).
read_metta(I,O):- string(I),normalize_space(string(M),I),!,parse_sexpr_metta1(M,O),!.
read_metta(In,Expr):- current_input(In0),In==In0,!, repl_read(Expr).
read_metta(In,Expr):- read_metta1(In,Expr).

read_metta1(S,F1):- use_new_parse_sexpr_metta_IO(S),!,new_parse_sexpr_metta_IO(S,F1).
read_metta1(In,Expr):- is_file_stream_and_size(In, Size) , Size>10240,!,read_sform1([],In,Expr).
read_metta1(In,Expr):- read_metta2(In,Expr).

read_metta2(_,O):- clause(t_l:s_reader_info(O),_,Ref),erase(Ref).
read_metta2(S,F1):- use_new_parse_sexpr_metta_IO(S),!,new_parse_sexpr_metta_IO(S,F1).
read_metta2(In,Expr):- peek_char(In,Char), read_metta2(In,Char,Expr).

read_metta2(S,_,F1):- use_new_parse_sexpr_metta_IO(S),!,new_parse_sexpr_metta_IO(S,F1).

read_metta2(In,Char,Expr):- char_type(Char,space),get_char(In,Char),not_compatio(put(Char)),!,read_metta2(In,Expr).
%read_metta2(In,'"',Expr):- read_sform2(In,Expr),!.
%read_metta2(In,'\'',Expr):- read_sform2(In,Expr),!.
read_metta2(In,'!',Expr):- get_char(In,_), !, read_metta2(In,Read1),!,Expr=exec(Read1).
read_metta2(In,';',Expr):- get_char(In,_), !, (maybe_read_pl(In,Expr)-> true ;
  (read_line_to_string(In,Str),Expr='$COMMENT'(Str,0,0))).
% write_comment(Str),!,read_metta2(In,Expr))),!.
% read_metta2(In,_,Expr):-  maybe_read_pl(In,Expr),!.
read_metta2(In,_,Read1):- parse_sexpr_metta(In,Expr),!,must_det_ll(Expr=Read1).


% Predicate to check if a stream is a file stream and get its size.
is_file_stream_and_size(Stream, Size) :-
    % Check if the stream is associated with a file.
    stream_property(Stream, file_name(FileName)),
    % Check if the file is accessible and get its size.
    exists_file(FileName),
    size_file(FileName, Size).


maybe_read_pl(In,Expr):-
  peek_line(In,Line1), Line1\=='', atom_contains(Line1, '.'),atom_contains(Line1, ':-'),
  notrace(((catch_err((read_term_from_atom(Line1, Term, []), Term\==end_of_file, Expr=call(Term)),_, fail),!,
  read_term(In, Term, [])))).


% Define the peek_line predicate.
% It uses a temporary string buffer to peek at the current line.
peek_line(Line) :-
    current_input(Stream),
    peek_line(Stream, Line).

% Helper predicate to peek the line from a specific stream.
peek_line(Stream, Line) :-
    % Remember the current stream position.
    stream_property(Stream, position(Pos)),
    % Read the next line.
    read_line_to_string(Stream, Line),
    % Set the stream back to the remembered position.
    set_stream_position(Stream, Pos).



maybe_read_sform_line(Stream, P2, Form) :- fail,
    % Check if the stream is repositionable
    % Get the current position in the stream
    stream_property(Stream, position(Pos)),
    % Read a line from the stream
    read_line_to_string(Stream, Line),
    maybe_read_sform_line_pos(Stream, Line, Pos, P2, Form).


maybe_read_sform_line_pos(Stream, Line, _Pos, P2, Form):- normalize_space(string(M),Line),M="",!,
  maybe_read_sform_line(Stream, P2, Form).

maybe_read_sform_line_pos(Stream, Line, Pos, P2, Form):-
    % Call P2 with the line. If P2 fails, reset the stream position
    (    call(P2,Line,Form)
    ->  true  % If P2 succeeds, do nothing more
    ;   set_stream_position(Stream, Pos), fail  % If P2 fails, reset position and fail
    ).



%read_line_to_sexpr(Stream,UnTyped),
read_sform(Str,F):- string(Str),open_string(Str,S),!,read_sform(S,F).
read_sform(S,F1):- use_new_parse_sexpr_metta_IO(S),!,new_parse_sexpr_metta_IO(S,F1).
read_sform(S,F):-
  read_sform1([],S,F1),
  ( F1\=='!' -> F=F1 ;
    (read_sform1([],S,F2), F = exec(F2))).


%read_sform2(S,F1):- !, read_metta2(S,F1).
read_sform2(S,F1):- use_new_parse_sexpr_metta_IO(S),!,new_parse_sexpr_metta_IO(S,F1).
read_sform2(S,F1):- read_sform1([],S,F1).

read_sform1(_,_,O):- clause(t_l:s_reader_info(O),_,Ref),erase(Ref).
read_sform1( AltEnd,Str,F):- string(Str),open_string(Str,S),!,read_sform1( AltEnd,S,F).
read_sform1(_AltEnd,S,F):- at_end_of_stream(S),!,F=end_of_file.
read_sform1( AltEnd,S,M):- get_char(S,C),read_sform3(s, AltEnd,C,S,F),
      untyped_to_metta(F,M).
%read_sform1( AltEnd,S,F):- profile(parse_sexpr_metta(S,F)).

read_sform3(_AoS,_AltEnd,C,_,F):- C == end_of_file,!,F=end_of_file.
read_sform3(       s, AltEnd,C,S,F):- char_type(C,space),!,read_sform1( AltEnd,S,F).
%read_sform3(AoS,_AltEnd,';',S,'$COMMENT'(F,0,0)):- !, read_line_to_string(S,F).
read_sform3(       s, AltEnd,';',S,F):- read_line_to_string(S,_),!,read_sform1( AltEnd,S,F).
read_sform3(       s, AltEnd,'!',S,exec(F)):- !,read_sform1( AltEnd,S,F).

read_sform3(s,_AltEnd,_,S,F1):- maybe_read_sform_line(S, parse_sexpr_metta1, F1),!.

read_sform3(_AoS,_AltEnd,'"',S,Text):- !,must_det_ll(atom_until(S,[],'"',Text)).
read_sform3(_AoS,_AltEnd,'`',S,Text):- !,atom_until(S,[],'`',Text).
read_sform3(_AoS,_AltEnd,'\'',S,Text):- fail, !,atom_until(S,[],'\'',Text).
read_sform3(_AoS,_AltEnd,',',_,','):- fail, !.
read_sform3(     s , AltEnd,C,S,F):- read_sform4( AltEnd,C,S,F),!.
read_sform3(_AoS, AltEnd,P,S,Sym):- peek_char(S,Peek),!,read_symbol_or_number( AltEnd,Peek,S,[P],Expr),into_symbol_or_number(Expr,Sym).

into_symbol_or_number(Expr,Sym):- atom_number(Expr,Sym),!.
into_symbol_or_number(Sym,Sym).

read_sform4(_AltEnd,B,S,Out):-  read_sform5(s,B,S,List,E), c_list(E,List,Out).
c_list(')',List,List).  c_list('}',List,['{...}',List]). c_list(']',List,['[...]',List]).


read_sform5(AoS,'(',S,List,')'):- !,collect_list_until(AoS,S,')',List),!.
read_sform5(AoS,'{',S,List,'}'):- !,collect_list_until(AoS,S,'}',List),!.
read_sform5(AoS,'[',S,List,']'):- !,collect_list_until(AoS,S,']',List),!.


read_symbol_or_number( AltEnd,Peek,S,SoFar,Expr):- SoFar\==[], Peek=='\\', !,
    get_char(S,_),get_char(S,C),append(SoFar,[C],NSoFar),
    peek_char(S,NPeek), read_symbol_or_number(AltEnd,NPeek,S,NSoFar,Expr).

read_symbol_or_number(_AltEnd,Peek,_S,SoFar,Expr):- Peek==end_of_file,!,
    must_det_ll(( symbolic_list_concat(SoFar,Expr))).




read_symbol_or_number(_AltEnd,Peek,_S,SoFar,Expr):- char_type(Peek,space),!,
    must_det_ll(( symbolic_list_concat(SoFar,Expr))).

read_symbol_or_number( AltEnd,Peek,_S,SoFar,Expr):- member(Peek,AltEnd),!,
    must_det_ll(( do_symbolic_list_concat(Peek,SoFar,Expr))).
read_symbol_or_number(AltEnd,B,S,SoFar,Expr):- fail,read_sform5(AltEnd,B,S,List,E),
  flatten([List,E],F), append(SoFar,F,NSoFar),!,
   peek_char(S,NPeek), read_symbol_or_number(AltEnd,NPeek,S,NSoFar,Expr).
read_symbol_or_number( AltEnd,_Peek,S,SoFar,Expr):- get_char(S,C),append(SoFar,[C],NSoFar),
    peek_char(S,NPeek), read_symbol_or_number(AltEnd,NPeek,S,NSoFar,Expr).

atom_until(S,SoFar,End,Text):- get_char(S,C),atom_until(S,SoFar,C,End,Text).
atom_until(_,SoFar,C,End,Expr):- C ==End,!,must_det_ll((do_symbolic_list_concat(End,SoFar,Expr))).
atom_until(S,SoFar,'\\',End,Expr):-get_char(S,C),!,atom_until2(S,SoFar,C,End,Expr).
atom_until(S,SoFar,C,End,Expr):- atom_until2(S,SoFar,C,End,Expr).
atom_until2(S,SoFar,C,End,Expr):- append(SoFar,[C],NSoFar),get_char(S,NC),
    atom_until(S,NSoFar,NC,End,Expr).

do_symbolic_list_concat('"',SoFar,Expr):- \+ string_to_syms,!, atomics_to_string(SoFar,Expr),!.
do_symbolic_list_concat(_End,SoFar,Expr):- symbolic_list_concat(SoFar,Expr).

collect_list_until(AoS,S,End,List):- get_char(S,C), cont_list(AoS,C,End,S,List).

cont_list(_AoS,End,_End1,_,[]):- End==end_of_file, !.
cont_list(_AoS,End,End1,_,[]):- End==End1, !.
cont_list( AoS,C,End,S,[F|List]):- read_sform3(AoS,[End],C,S,F),!,collect_list_until(AoS,S,End,List).


use_new_parse_sexpr_metta_IO(S):- \+ string(S).

new_parse_sexpr_metta_IO1(S,F1):- at_end_of_stream(S),!,F1=end_of_file.
new_parse_sexpr_metta_IO1(S,F1):- peek_char(S,Char),char_type(Char,space),!,
  get_char(S,Char), parse_sexpr_metta_IO(S,F1).
new_parse_sexpr_metta_IO1(S,_F1):- S = InStream,
   once((
    read_position(InStream, Line, Col, CharPos_Item, Position),  % Retrieve line, column, and character position.
    read_sexpr(InStream, Item),  % Read an S-expression or comment from the input stream.
    assertz(metta_file_comment(Line, Col, CharPos_Item, Item, Position)))),
    fail.
new_parse_sexpr_metta_IO1(_S,F1):- retract(metta_file_comment(_Line, _Col, _CharPos, M, _Pos)), trly(untyped_to_metta,M,F1).

new_parse_sexpr_metta_IO(S,F1):- new_parse_sexpr_metta_IO1(S,F1), nop(wdmsg(new_parse_sexpr_metta_IO1(S,F1))).

in2_stream(N1,S1):- integer(N1),!,stream_property(S1,file_no(N1)),!.
in2_stream(N1,S1):- atom(N1),stream_property(S1,alias(N1)),!.
in2_stream(N1,S1):- is_stream(N1),S1=N1,!.
in2_stream(N1,S1):- atom(N1),stream_property(S1,file_name(N1)),!.
is_same_streams(N1,N2):- in2_stream(N1,S1),in2_stream(N2,S2),!,S1==S2.



parse_sexpr_metta(I,O):- (\+ atomic(I) ; \+ is_stream(I)),!,text_to_string(I,S),!,parse_sexpr_metta1(S,O),!.
parse_sexpr_metta(S,F1):- fail, %line_count(S, LineNumber),
                          maybe_read_sform_line(S, parse_sexpr_metta1, F1),!.
parse_sexpr_metta(S,F1):- parse_sexpr_metta_IO(S,F1),!.

parse_sexpr_metta_IO(S,F1):- at_end_of_stream(S),!,F1=end_of_file.
parse_sexpr_metta_IO(S,F1):- peek_char(S,Char),char_type(Char,space),!,
  get_char(S,Char), parse_sexpr_metta_IO(S,F1).
parse_sexpr_metta_IO(S,F1):- use_new_parse_sexpr_metta_IO(S),!,new_parse_sexpr_metta_IO(S,F1).

parse_sexpr_metta_IO(S,F1):-
    %line_count(S, LineNumber),
    % Get the character position within the current line
    %line_position(S, LinePos),
    nop((character_count(S, Offset),move_cursor_to_first_column,
      write(user_error,'File Offset: '),write(user_error,Offset))),
    parse_sexpr_untyped(S, M),!,
    nop((write(user_error,'.'),!,move_cursor_to_first_column)),
    trly(untyped_to_metta,M,F1),
    nop(writeqln(user_error,F1)),!.

move_cursor_to_first_column:- write(user_error,'\033[1G').
move_cursor_to_first_column_out:- write(user_output,'\033[1G').

parse_sexpr_metta1(I,O):- normalize_space(string(M),I),!,parse_sexpr_metta2(M,U),!,
  trly(untyped_to_metta,U,O).
parse_sexpr_metta2(M,exec(O)):- string_concat('!',I,M),!,parse_sexpr_metta2(I,O).
parse_sexpr_metta2(M,(O)):- string_concat('+',I,M),!,parse_sexpr_metta2(I,O).
parse_sexpr_metta2(I,U):- parse_sexpr_untyped(I,U),!,writeqln(user_error,U).

test_parse_sexpr_metta1:-
  ignore((parse_sexpr_metta1(
"(: synonyms-gene-ENSG00000085491 (synonyms (gene ENSG00000085491) (ATP-Mg/P\\(i\\)_co-transporter_1 calcium-binding_mitochondrial_carrier_protein_SCaMC-1 HGNC:20662 mitochondrial_ATP-Mg/Pi_carrier_protein_1 small_calcium-binding_mitochondrial_carrier_protein_1 mitochondrial_Ca\\(2+\\)-dependent_solute_carrier_protein_1 mitochondrial_adenyl_nucleotide_antiporter_SLC25A24 solute_carrier_family_25_member_24 calcium-binding_transporter APC1 short_calcium-binding_mitochondrial_carrier_1 solute_carrier_family_25_\\(mitochondrial_carrier;_phosphate_carrier\\),_member_24 SCAMC1 SLC25A24 short_calcium-binding_mitochondrial_carrier_protein_1 SCAMC-1)))",O),
  writeq(parse_sexpr_metta1(O)))),break.

writeqln(W,Q):- nop(format(W,'; ~q~n',[Q])).

write_comment(_):- is_compatio,!.
write_comment(_):- silent_loading,!.
write_comment(Cmt):- connlf,format(';;~w~n',[Cmt]).
do_metta_cmt(_,'$COMMENT'(Cmt,_,_)):- write_comment(Cmt),!.
do_metta_cmt(_,'$STRING'(Cmt)):- write_comment(Cmt),!.
do_metta_cmt(Self,[Cmt]):- !, do_metta_cmt(Self, Cmt),!.

metta_atom_in_file(Self,Term):-  metta_atom_in_file(Self,Term,_,_).
metta_atom_in_file(Self,STerm,Filename,Lineno):-
    user:loaded_into_kb(Self,Filename),
    once(user:asserted_metta_pred(Mangle,Filename)),
    %s2t_iz(Mangle,P,CTerm,Term),
    %CTerm=Term,Mangle=P,
    current_predicate(Mangle/Arity),

    notrace((length(STerm,Arity),
    term_variables(STerm,SVs),
    copy_term(STerm+SVs,CTerm+CVs),
    Data =..[Mangle,Lineno|CTerm])),
    %write_src_woi(Data),
    current_predicate(_,Data),
    call(Data),
    maplist(mapvar,CVs,SVs).

%mapvar(CV,SV):- var(CV),!,SV=CV.
mapvar(CV,SV):- t2s(CV,CCV),!,SV=CCV.

%constrain_sterm(STerm):- var(STerm),!,between(1,5,Len),length(STerm,Len).
%constrain_sterm(STerm):- is_list(STerm),!.
constrain_sterm(NV):- nonvar(NV),!.
constrain_sterm([_,_,_]).
constrain_sterm([_,_,_,_]).
constrain_sterm([_,_,_,_,_]).
constrain_sterm([_,_]).

s2t_iz(Mangle,Iz,[Colon,Name,Info],[Name|InfoL]):- Colon == ':',
   is_list(Info), mangle_iz(Mangle,Iz),
   maplist(s2t,Info,InfoL).
s2t_iz(Mangle,Mangle,Info,InfoL):- s2tl(Info,InfoL).

mangle_iz(Mangle,Iz):- symbol_concat(Mangle,'_iz',Iz).

produce_iz(Mangle):-
  mangle_iz(Mangle,Iz),
  forall(between(1,5,Len),
    once((length(Args,Len),
    produce_iz_hb([Mangle,Lineno,[:,Name,[Pred|Args]]],[Iz,Lineno,Name,Pred|Args])))).

produce_iz_hb(HList,BList):-
   H=..HList,B=..BList,  HB=(H:-B),
   numbervars(HB,0,_),
   writeq(HB),writeln('.').

t2s(SList,List):- \+ compound(SList),!,SList=List.
t2s([H|SList],[HH|List]):- !, t2s(H,HH),!,t2s(SList,List).
t2s(X,XX):- compound(X),compound_name_arguments(X,t,Args),!,
    maplist(t2s,Args,XX).
t2s(X,X):-!.

s2tl(SList,List):- \+ compound(SList),!,SList=List.
s2tl([H|SList],[HH|List]):- !, s2t(H,HH),!,s2tl(SList,List).
s2tl(List,List).
%s2tl(SList,List):- is_list(SList), maplist(s2t,SList,List),!.

s2t(SList,List):- \+ compound(SList), !, SList=List.
s2t([A|SList],Term):- A == '->',!, s2tl(SList,List),   Term =.. [A,List].
s2t([A|SList],Term):- A == 'Cons',!,s2tl(SList,List), Term =.. [A|List].
s2t([A|SList],Term):- A == '=',!, s2tl(SList,List),   Term =.. [A|List].
s2t(List,Term):- is_list(List),!,maplist(s2t,List,TermList),
  compound_name_arguments(Term,t,TermList),!.
s2t(STerm,Term):- s2tl(STerm,Term),!.

mlog_sym('@').

%untyped_to_metta(I,exec(O)):- compound(I),I=exec(M),!,untyped_to_metta(M,O).
untyped_to_metta(I,O):-
 must_det_ll((
  trly(mfix_vars1,I,M),
  trly(cons_to_c,M,OM),
  trly(cons_to_l,OM,O))).


trly(P2,A,B):- once(call(P2,A,M)),A\=@=M,!,trly(P2,M,B).
trly(_,A,A).

mfix_vars1(I,O):- var(I),!,I=O.
mfix_vars1('$_','$VAR'('_')).
mfix_vars1('$','$VAR'('__')).
mfix_vars1(I,'$VAR'(O)):- atom(I),symbol_concat('$',N,I),symbol_concat('_',N,O).
%mfix_vars1('$t','$VAR'('T')):-!.
%mfix_vars1('$T','$VAR'('T')):-!.
%mfix_vars1(I,O):- I=='T',!,O='True'.
%mfix_vars1(I,O):- I=='F',!,O='False'.
%mfix_vars1(I,O):- is_i_nil(I),!,O=[].
mfix_vars1(I,O):- I=='true',!,O='True'.
mfix_vars1(I,O):- I=='false',!,O='False'.
mfix_vars1('$STRING'(I),O):- I=O,!.
mfix_vars1('$STRING'(I),O):- \+ string_to_syms, mfix_vars1(I,OO),text_to_string(OO,O),!.
%mfix_vars1('$STRING'(I),O):- \+ string_to_syms, text_to_string(I,O),!.
mfix_vars1('$STRING'(I),O):- !, mfix_vars1(I,M),atom_chars(O,M),!.
%mfix_vars1('$STRING'(I),O):- !, mfix_vars1(I,M),name(O,M),!.
mfix_vars1([H|T],O):-   H=='[', is_list(T), last(T,L),L==']',append(List,[L],T), !, O = ['[...]',List].
mfix_vars1([H|T],O):-   H=='{', is_list(T), last(T,L),L=='}',append(List,[L],T), !, O = ['{...}',List].
mfix_vars1([H|T],O):-   is_list(T), last(T,L),L=='}',append(List,[L],T),
   append(Left,['{'|R],List),append([H|Left],[['{}',R]],NewList),mfix_vars1(NewList,O).
mfix_vars1('$OBJ'(claz_bracket_vector,List),O):- is_list(List),!, O = ['[...]',List].
mfix_vars1(I,O):-  I = ['[', X, ']'], nonvar(X), !, O = ['[...]',X].
mfix_vars1(I,O):-  I = ['{', X, '}'], nonvar(X), !, O = ['{...}',X].
mfix_vars1('$OBJ'(claz_bracket_vector,List),Res):- is_list(List),!, append(['['|List],[']'],Res),!.
mfix_vars1(I,O):- I==[Quote, S], Quote==quote,S==s,!, O=is.
mfix_vars1([K,H|T],Cmpd):- fail,
  atom(K),mlog_sym(K),is_list(T),
  mfix_vars1([H|T],[HH|TT]),atom(HH),is_list(TT),!,
  compound_name_arguments(Cmpd,HH,TT).
%mfix_vars1([H|T],[HH|TT]):- !, mfix_vars1(H,HH),mfix_vars1(T,TT).
mfix_vars1(List,ListO):- is_list(List),!,maplist(mfix_vars1,List,ListO).
mfix_vars1(I,O):- string(I),string_to_syms,!,atom_string(O,I).

mfix_vars1(I,O):- compound(I),!,compound_name_arguments(I,F,II),F\=='$VAR',maplist(mfix_vars1,II,OO),!,compound_name_arguments(O,F,OO).
mfix_vars1(I,O):- \+ symbol(I),!,I=O.
mfix_vars1(I,I).

string_to_syms:- fail.
no_cons_reduce.
svar_fixvarname_dont_capitalize(O,O):-!.
svar_fixvarname_dont_capitalize(M,O):- svar_fixvarname(M,O),!.


%dvar_name(t,'T'):- !.
dvar_name(N,O):- symbol_concat('_',_,N),!,O=N.
dvar_name(N,O):- integer(N),symbol_concat('_',N,O).
dvar_name(N,O):- atom(N),atom_number(N,Num),dvar_name(Num,O),!.
dvar_name(N,O):- \+ symbol(N),!,format(atom(A),'~w',[N]),dvar_name(A,O).
dvar_name(N,O):- !, format(atom(A),'_~w',[N]),dvar_name(A,O).
%dvar_name(  '',''):-!. % $
%dvar_name('_','__'):-!. % $_
dvar_name(N,O):- symbol_concat('_',_,N),!,symbol_concat('_',N,O).
dvar_name(N,O):- svar_fixvarname_dont_capitalize(N,O),!.
dvar_name(N,O):- must_det_ll((atom_chars(N,Lst),maplist(c2vn,Lst,NList),symbolic_list_concat(NList,S),svar_fixvarname_dont_capitalize(S,O))),!.
c2vn(A,A):- char_type(A,prolog_identifier_continue),!.
c2vn(A,A):- char_type(A,prolog_var_start),!.
c2vn(A,AA):- char_code(A,C),symbolic_list_concat(['_C',C,'_'],AA).

cons_to_l(I,I):- no_cons_reduce,!.
cons_to_l(I,O):- var(I),!,O=I.
cons_to_l(I,O):- is_i_nil(I),!,O=[].
cons_to_l(I,O):- I=='nil',!,O=[].
cons_to_l(C,O):- \+ compound(C),!,O=C.
cons_to_l([Cons,H,T|List],[HH|TT]):- List==[], atom(Cons),is_cons_f(Cons), t_is_ttable(T), cons_to_l(H,HH),!,cons_to_l(T,TT).
cons_to_l(List,ListO):- is_list(List),!,maplist(cons_to_l,List,ListO).
cons_to_l(I,I).

cons_to_c(I,I):- no_cons_reduce,!.
cons_to_c(I,O):- var(I),!,O=I.
cons_to_c(I,O):- is_i_nil(I),!,O=[].
cons_to_c(I,O):- I=='nil',!,O=[].
cons_to_c(C,O):- \+ compound(C),!,O=C.
cons_to_c([Cons,H,T|List],[HH|TT]):- List==[], atom(Cons),is_cons_f(Cons), t_is_ttable(T), cons_to_c(H,HH),!,cons_to_c(T,TT).
cons_to_c(I,O):- \+ is_list(I), compound_name_arguments(I,F,II),maplist(cons_to_c,II,OO),!,compound_name_arguments(O,F,OO).
cons_to_c(I,I).



t_is_ttable(T):- var(T),!.
t_is_ttable(T):- is_i_nil(T),!.
t_is_ttable(T):- is_ftVar(T),!.
t_is_ttable([F|Args]):- F=='Cons',!,is_list(Args).
t_is_ttable([_|Args]):- !, \+ is_list(Args).
t_is_ttable(_).

is_cons_f(Cons):- is_cf_nil(Cons,_).
is_cf_nil('Cons','NNNil').
%is_cf_nil('::','nil').

is_i_nil(I):-
  is_cf_nil('Cons',Nil), I == Nil.

subst_vars(TermWDV, NewTerm):-
   subst_vars(TermWDV, NewTerm, NamedVarsList),
   maybe_set_var_names(NamedVarsList).

subst_vars(TermWDV, NewTerm, NamedVarsList) :-
    subst_vars(TermWDV, NewTerm, [], NamedVarsList).

subst_vars(Term, Term, NamedVarsList, NamedVarsList) :- var(Term), !.
subst_vars([], [], NamedVarsList, NamedVarsList):- !.
subst_vars([TermWDV|RestWDV], [Term|Rest], Acc, NamedVarsList) :- !,
    subst_vars(TermWDV, Term, Acc, IntermediateNamedVarsList),
    subst_vars(RestWDV, Rest, IntermediateNamedVarsList, NamedVarsList).
subst_vars('$VAR'('_'), _, NamedVarsList, NamedVarsList) :- !.
subst_vars('$VAR'(VName), Var, Acc, NamedVarsList) :- nonvar(VName), svar_fixvarname_dont_capitalize(VName,Name), !,
    (memberchk(Name=Var, Acc) -> NamedVarsList = Acc ; ( !, Var = _, NamedVarsList = [Name=Var|Acc])).
subst_vars(Term, Var, Acc, NamedVarsList) :- atom(Term),symbol_concat('$',DName,Term),
   dvar_name(DName,Name),!,subst_vars('$VAR'(Name), Var, Acc, NamedVarsList).

subst_vars(TermWDV, NewTerm, Acc, NamedVarsList) :-
    compound(TermWDV), !,
    compound_name_arguments(TermWDV, Functor, ArgsWDV),
    subst_vars(ArgsWDV, Args, Acc, NamedVarsList),
    compound_name_arguments(NewTerm, Functor, Args).
subst_vars(Term, Term, NamedVarsList, NamedVarsList).


connlf:- check_silent_loading, not_compat_io((format('~N'))).
connl:- check_silent_loading,not_compat_io((nl)).
% check_silent_loading:- silent_loading,!,trace,break.
check_silent_loading.
silent_loading:- option_value('load','silent'), !.
silent_loading:- is_converting,!.
silent_loading:- option_value('html','True'), !,fail.
silent_loading:- option_value('trace-on-load','False'), !.




uncompound(OBO,Src):- \+ compound(OBO),!, Src = OBO.
uncompound('$VAR'(OBO),'$VAR'(OBO)):-!.
uncompound(IsList,Src):- is_list(IsList),!,maplist(uncompound,IsList,Src).
uncompound([Is|NotList],[SrcH|SrcT]):-!, uncompound(Is,SrcH),uncompound(NotList,SrcT).
uncompound(Compound,Src):- compound_name_arguments(Compound,Name,Args),maplist(uncompound,[Name|Args],Src).

assert_to_metta(_):- reached_file_max,!.
assert_to_metta(OBO):-
    must_det_ll((OBO=..[Fn|DataLL],
    maplist(better_arg,DataLL,DataL),
    into_datum(Fn, DataL, Data),
    functor(Data,Fn,A),decl_fb_pred(Fn,A),
    real_assert(Data),!,
   incr_file_count(_))).

assert_to_metta(OBO):-
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

% Initialize the progress bar and remember its starting position
init_progress_bar(Width) :-
    current_output(Stream),
    stream_property(Stream, position(Pos)),
    asserta(progress_bar_position(Pos)),
    write('['),
    forall(between(1, Width, _), write(' ')),
    write(']'),
    flush_output.

% Check if the progress bar needs to be redrawn and update it accordingly
update_progress_bar(Current, Total, Width) :-
    current_output(Stream),
    % Get the current position
    stream_property(Stream, position(CurrentPos)),
    % Get the remembered position
    progress_bar_position(SavedPos),
    % Compare positions; if they differ, redraw the entire progress bar
    (   SavedPos \= CurrentPos
    ->  redraw_progress_bar(Width)
    ;   true
    ),
    % Update the progress bar
    Percentage is Current / Total,
    Filled is round(Percentage * Width),
    write('\r['),
    forall(between(1, Filled, _), write('#')),
    Remaining is Width - Filled,
    forall(between(1, Remaining, _), write(' ')),
    write(']'),
    flush_output.

% Redraw the progress bar if the position has changed
redraw_progress_bar(Width) :-
    nl,
    init_progress_bar(Width).

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


use_corelib_file:- using_corelib_file,!.
use_corelib_file:- asserta(using_corelib_file), fail.
use_corelib_file:- really_use_corelib_file, !.
use_corelib_file:- !.
%use_corelib_file:- really_use_corelib_file,!.
really_use_corelib_file:- load_corelib_file, generate_interpreter_stubs.

:- dynamic(did_generate_interpreter_stubs/0).
generate_interpreter_stubs:- did_generate_interpreter_stubs,!.
generate_interpreter_stubs:-
   asserta(did_generate_interpreter_stubs),
   forall(metta_type('&corelib',Symb,Def),
        gen_interp_stubs('&corelib',Symb,Def)).
        
:- dynamic(metta_atom_asserted_deduced/2).
:- multifile(metta_atom_asserted_deduced/2).
metta_atom_asserted_deduced('&corelib', Term):- fail,
  %\+ did_generate_interpreter_stubs,
   metta_atom_corelib_types(Term),
   wdmsg(metta_atom_corelib_types(Term)).

load_corelib_file:- really_using_corelib_file,!.
load_corelib_file:- asserta(really_using_corelib_file), fail.
load_corelib_file:- is_metta_src_dir(Dir), really_use_corelib_file(Dir,'corelib.metta'),!.
load_corelib_file:- is_metta_src_dir(Dir), really_use_corelib_file(Dir,'stdlib_mettalog.metta'),!.
% !(import! &corelib "src/canary/stdlib_mettalog.metta")
really_use_corelib_file(Dir,File):- absolute_file_name(File,Filename,[relative_to(Dir)]),
 locally(nb_setval(may_use_fast_buffer,t),
   locally(nb_setval(suspend_answers,true),
     with_output_to(string(_),include_metta_directory_file('&corelib',Dir,Filename)))).


