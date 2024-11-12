
is_list(List), % Converts a list of conjunctions into a sequential representation in MeTTa
    into_sequential(OC, List, SP), !.

% Base case for when the input is an empty list; this results in 'True'.
into_sequential([progn|_], Nothing, 'True') :- Nothing == [], !.

% Another base case for when the input is an empty list, results in 'Nil'.
into_sequential(_OC, Nothing, 'Nil') :- Nothing == [], !.

% If there is only one element in the list, convert it directly using 'prolog_to_metta'.
into_sequential(_, [SP], O) :- prolog_to_metta(SP, O).

% When the list has more than one element, convert each item using 'prolog_to_metta' and wrap it with AND.
into_sequential([progn|_], List, SPList) :-
    maplist(prolog_to_metta, List, SPList), !.

% If the AND operator is compiled, use it and convert all elements in the list to MeTTa.
into_sequential(_CA, List, [AND|SPList]) :-
    is_compiled_and(AND),
    maplist(prolog_to_metta, List, SPList), !.


/** list_direct_subdirectories(+Directory, -DirectSubdirectories)
    Get a list of immediate subdirectories in a given directory.

    @param Directory The directory to search in.
    @param DirectSubdirectories The result will be a list of subdirectories directly within the specified directory.

    @example
    ?- list_direct_subdirectories('/home/user', Subdirectories).
    Subdirectories = ['/home/user/Documents', '/home/user/Downloads'].
*/
% List all files in the directory.
list_direct_subdirectories(Directory, DirectSubdirectories) :-
    directory_files(Directory, Entries), % Retrieve the directory entries
    findall(Path,
            (
                member(Entry, Entries), % Iterate over each entry
                \+ member(Entry, ['.', '..']), % Skip '.' and '..'
                symbolic_list_concat([Directory, '/', Entry], Path), % Construct the full path
                is_directory(Path) % Ensure the entry is a directory
            ),
            DirectSubdirectories).

/** list_all_subdirectories(+Directory, -AllSubdirectories)
    Recursively list all subdirectories of a given directory.

    @param Directory The directory to start from.
    @param AllSubdirectories The result will be a list of all subdirectories, including nested ones.

    @example
    ?- list_all_subdirectories('/home/user', Subdirectories).
    Subdirectories = ['/home/user/Documents', '/home/user/Documents/Work'].
*/
% Recursively list all subdirectories in a directory.
list_all_subdirectories(Directory, AllSubdirectories) :-
    list_direct_subdirectories(Directory, DirectSubdirectories), % Get the immediate subdirectories
    findall(Sub,
            (
                member(SubDir, DirectSubdirectories), % For each direct subdirectory
                list_all_subdirectories(SubDir, Subs), % Recursively find subdirectories
                member(Sub, Subs) % Collect the nested subdirectories
            ),
            NestedSubdirectories),
    append(DirectSubdirectories, NestedSubdirectories, AllSubdirectories). % Combine direct and nested subdirectories

/** with_file_lists(+Rel, +P1, +FileSpec)
    Process a list of filenames by applying the predicate P1 to each file. It handles various types of file specifications, such as lists or atomic filenames.

    @param Rel The base directory.
    @param P1 The predicate to apply to each file.
    @param FileSpec The file or list of files to process.

    @example
    ?- with_file_lists('.', print_file, 'example.pl').
*/
% Base case: if the file specification is '.pl', succeed.
with_file_lists(_Rel, _P1, FileSpec) :- FileSpec == '.pl', !.

% If the file specification is a list, apply the predicate to each file.
with_file_lists(Rel, P1, FileSpec) :-
    is_list(FileSpec), !,
    ignore(maplist(with_file_lists(Rel, P1), FileSpec)).

% If the filename is atomic and the file exists, apply the predicate to the file.
with_file_lists(_Rel, P1, Filename) :-
    atomic(Filename), exists_file(Filename), !,
    ignore(call(P1, Filename)).

% Handle the case where the directory is relative and exists.
with_file_lists(Rel, P1, Filename) :-
    absolute_file_name(Rel, Dir, [access(read), file_errors(fail), file_type(directory)]),
    Rel \=@= Dir, !,
    with_file_lists(Dir, P1, Filename).

% If the relative directory doesn't exist, try using the current directory.
with_file_lists(Rel, P1, Filename) :-
    \+ exists_directory(Rel), !,
    with_file_lists('.', P1, Filename).

% Handle compound file paths and directories, attempting to resolve the file specification.
with_file_lists(Rel, P1, File) :-
    compound(File),
    absolute_file_name(File, Dir, [access(read), relative_to(Rel), file_errors(fail), extensions(['pl', 'prolog', 'pfc'])]),
    '\\=@='(Dir, File), !,
    with_file_lists(Rel, P1, Dir).

% Handle compound file paths when they represent directories.
with_file_lists(Rel, P1, File) :-
    compound(File),
    absolute_file_name(File, Dir, [access(read), file_errors(fail), relative_to(Rel), file_type(directory)]),
    '\\=@='(Dir, File), !,
    with_file_lists(Rel, P1, Dir).

/* previously: This block had redundant code for directory handling and file type handling. The earlier approach repeated the handling of compound files unnecessarily, so we have consolidated the logic above.
with_file_lists(Rel, P1, File) :-
    compound(File),
    absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(directory)]),
    '\\=@='(Dir, File), !,
    with_file_lists(Rel, P1, Dir).

with_file_lists(Rel, P1, File) :-
    compound(File), !,
    absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(['csv', 'tsv', ''])]),
    '\\=@='(Dir, File), !,
