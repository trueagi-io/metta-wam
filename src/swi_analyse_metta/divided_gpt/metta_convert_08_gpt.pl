      with_file_lists(Rel,P1, Dir).

% Case 1: File contains a wildcard, so we expand it and process the resulting files.
with_file_lists(Rel, P1, File) :-
    % Check if the file contains a wildcard symbol '*'
    symbol_contains(File, '*'),
    % Expand the file name (with wildcard) to get the list of matching files
    expand_file_name(File, List),
    List \== [],  % Ensure the list is not empty
    !,  % Commit to this clause if conditions are met
    % Map over the file list and apply the with_wild_path/1 predicate to each file
    maplist(with_wild_path(Fnicate), List).

% Case 2: File is a directory, find all files matching wildcard in that directory.
with_file_lists(Rel, P1, File) :-
    % Check if File is a directory
    exists_directory(File),
    % Construct a wildcard to match files in the directory
    directory_file_path(File, '*.*sv', Wildcard),
    % Expand the wildcard into a list of files
    expand_file_name(Wildcard, List),
    !,  % Commit to this clause
    % Apply the predicate to each file in the list
    maplist(Fnicate, List).
*/



/* 
   This file contains predicates for handling file lists and directories.
   It uses predicates to expand wildcards and to recursively find and process files in directories.

   The following predicates are heavily commented to improve clarity, including examples where appropriate.
*/

% PLDoc header for with_file_lists/3
/** 
 * with_file_lists(+Rel, +P1, +FileOrDir) 
 * 
 * Processes files or directories, expanding wildcards and recursively processing directories if needed.
 * 
 * @param Rel The base directory or context for relative file paths.
 * @param P1  A predicate to apply to each discovered file.
 * @param FileOrDir The file or directory to process, possibly containing wildcards.
 * 
 * @example
 * ?- with_file_lists('.', writeln, '*.pl').
 * This will print all Prolog files in the current directory.
 */


/* previously:
   This clause was removed to avoid redundancy.
   It used a slightly different wildcard pattern ('*.pl') but the logic
   is covered in the active clauses.
*/

% Case 3: Wildcard handling when the wildcard is an atom and file does not exist.
with_file_lists(Rel, P1, Wildcard) :-
    atom(Wildcard),
    % Ensure that the file does not exist but may contain a wildcard pattern
    \+ exists_file(Wildcard),
    % Check if the wildcard contains special characters like '*', '?', or '|'
    once(atom_contains(Wildcard, '*') ; atom_contains(Wildcard, '?') ; atom_contains(Wildcard, '|')),
    % Expand the wildcard into a list of matching files
    expand_file_name(Wildcard, Files),
    Files \== [],  % Ensure the list is not empty
    !,  % Commit to this clause
    % Apply the predicate to each file in the list
    ignore(maplist(with_file_lists(Rel, P1), Files)).

% Case 4: Wildcard handling when expanding relative file paths.
with_file_lists(Rel, P1, Wildcard) :-
    atom(Wildcard),
    % Check if the wildcard contains special characters
    once(atom_contains(Wildcard, '*') ; atom_contains(Wildcard, '?') ; atom_contains(Wildcard, '|')),
    \+ exists_file(Wildcard),  % Ensure that the file does not exist yet
    % Convert relative wildcard to an absolute path
    absolute_file_name(Wildcard, AbsWildcard, [relative_to(Rel)]),
    \+ exists_file(AbsWildcard),  % Ensure that the absolute wildcard doesn't point to an existing file
    % Expand the wildcard into a list of matching files
    expand_file_name(AbsWildcard, Files),
    Files \== [],  % Ensure the list is not empty
    !,  % Commit to this clause
    % Apply the predicate to each file in the list
    ignore(maplist(with_file_lists(Rel, P1), Files)).

/* previously:
   There was another directory expansion clause that used '.' and '*.pl'.
   It has been commented out as the logic is now better covered by the wildcard handling above.
   The commented out code was not removed to preserve older functionality, which might be useful in specific contexts.
*/

% Case 5: Recursively process all files in directories when '**' is specified.
with_file_lists(Rel, P1, Local) :-
    (Local == '**' ; Local == '**.pl'),  % Check if the input is '**' or '**.pl'
    % Must-det predicate ensures that the directory exists
    must_det_ll((
        absolute_file_name(Directory, AbsDirectory, [file_type(directory)]),
        exists_directory(AbsDirectory))),
    % Find all source files in the directory, including recursively
    findall(File, directory_source_files(AbsDirectory, File, [recursive(true), if(true)]), Files),
    !,  % Commit to this clause
    % Apply the predicate to each file
    ignore(maplist(with_file_lists(Rel, P1), Files)).

% Case 6: Handle filenames with symbolic lists and subdirectories
with_file_lists(Rel, P1, Filename) :-
    % Decompose the symbolic list '**/S/More' into components
    symbolic_list_concat(['**', S | More], '/', Filename),
    symbolic_list_concat([S | More], '/', Rest),
    % Get all subdirectories
    list_all_subdirectories(Rel, AllSubdirectories),
    !,  % Commit to this clause
    % For each subdirectory, apply the predicate recursively
    forall(member(SubDir, AllSubdirectories), with_file_lists(SubDir, P1, Rest)).

% Case 7: Handle symbolic lists with wild directories
with_file_lists(Rel, P1, Filename) :-
    % Decompose symbolic list with wildcard directory
    symbolic_list_concat([WildDir, S | More], '/', Filename),
    symbolic_list_concat([Rel, WildDir, ''], '/', WildMaskDir),
    % Expand the wildcard into all matching subdirectories
    expand_file_name(WildMaskDir, AllSubdirectories),
    symbolic_list_concat([S | More], '/', Rest),
    !,  % Commit to this clause
    % For each subdirectory, apply the predicate recursively
    forall(member(SubDir, AllSubdirectories), with_file_lists(SubDir, P1, Rest)).

% Case 8: Process individual files, ensuring file existence.
with_file_lists(Rel, P1, FileSpec) :-
    atomic(FileSpec),  % Ensure the file specification is atomic
    % Convert to an absolute file name and ensure it exists
    absolute_file_name(FileSpec, AbsFile, [relative_to(Rel), access(read), file_errors(fail)]),
    exists_file(AbsFile),  % Check if the file exists
    !,  % Commit to this clause
    % Apply the predicate P1 to the file
    ignore(call(P1, AbsFile)).

% Case 9: Process directories recursively.
with_file_lists(Rel, P1, Directory) :-
    atomic(Directory),  % Ensure the directory is atomic
    % Convert to an absolute directory path and ensure it exists
    absolute_file_name(Directory, AbsDirectory, [relative_to(Rel), access(read), file_errors(fail), file_type(directory)]),
    exists_directory(AbsDirectory),  % Check if the directory exists
    !,  % Commit to this clause
    % Find all source files in the directory, including recursively
    findall(File, directory_source_files(AbsDirectory, File, [recursive(true), if(true)]), Files),
    !,  % Commit to this clause
    % Apply the predicate to each file
    ignore(maplist(with_file_lists(Rel, P1), Files)).