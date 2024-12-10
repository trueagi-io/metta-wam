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

%********************************************************************************************* 
% PROGRAM FUNCTION: loads, parses, and converts OBO (Open Biomedical and Biomedical Ontologies) files 
% into a structured format, extracting key ontological information like term IDs, names, definitions, 
% and relationships.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ===========================================
% BEGIN OBO Loader
%   - Douglas R. Miles 2023
% ===========================================

% requires:  assert_MeTTa/1, track_load_into_file/2

% The `use_module/1` directive automatically loads the module if it isn't already loaded.
:- use_module(library(logicmoo_utils)).
    % Guarantee the metta_interp library is loaded into the program.
    :- ensure_loaded(library(metta_interp)).

%!  assert_OBO(+P, +X, +Y) is det.
%
%   Asserts a fact into the Prolog database in the form of `ontology_info/3`.
%   This predicate takes three arguments, wraps them into the structure 
%   `ontology_info/3`, and asserts the resulting fact into the database.
%   Using `assert_OBO/3` allows for a simplified interface when adding 
%   ontology-related information to the knowledge base.
%
%   @arg P The predicate or category representing the ontology relationship.
%   @arg X The subject or entity in the ontology.
%   @arg Y The object or related entity in the ontology.
%
%   @example
%     % Assert a new ontology fact:
%     ?- assert_OBO('is_a', 'Dog', 'Animal').
%     % This results in the fact `ontology_info('is_a', 'Dog', 'Animal')` being added 
%     % to the Prolog database.
%
assert_OBO(P, X, Y) :-
    % Wrap the input arguments into the ontology_info/3 structure and assert it.
    assert_OBO(ontology_info(P, X, Y)).

%!  load_obo(+Filename) is det.
%
%   Main entry point for loading OBO files or directories containing OBO files.
%   This predicate processes files or directories to handle ontology data in 
%   OBO format. The input can be a single filename, a directory, or a wildcard pattern.
%   The predicate resolves paths, checks for existence, and processes each file
%   or directory accordingly.
%
%   @arg Filename The name of the file or directory to process. Can also be a wildcard pattern.
%
%   @example
%     % Load a specific OBO file:
%     ?- load_obo('example.obo').
%
%     % Load all OBO files in a directory:
%     ?- load_obo('/path/to/directory').
%
load_obo(Filename) :-
    % Handle the case where the input is not atomic (e.g., a variable or complex term).
    \+ atomic(Filename),
    % Resolve the absolute path to a directory or file that exists.
    absolute_file_name(Filename, X, [
        read(exists),           % Check that the file or directory exists.
        extension(['']),        % Allow any file extension.
        file_type(directory),   % Ensure the target is a directory.
        file_errors(fail),      % Fail if the file cannot be accessed.
        solutions(first)        % Use the first valid solution found.
        ]),
    !, 
    % Retry loading with the resolved absolute path.
    load_obo(X).
load_obo(Filename) :-
    % Retry if the first clause failed and the input is still not atomic.
    \+ atomic(Filename), !,
    absolute_file_name(Filename, X, 
        [read(exists),
        extension(['']),
        file_errors(fail),
        solutions(first)]), 
    !, load_obo(X).
load_obo(Filename) :-
    % Handle the case where the input is atomic but the file does not exist.
    atomic(Filename), 
    \+ exists_file(Filename),
    % Expand wildcard patterns into a list of filenames.
    expand_file_name(Filename, List),
    List \== [],  % Ensure the list is not empty.
    List \== [Filename],  % Ensure the list is not the same as the original input.
    % Recursively process each file in the expanded list.
    maplist(load_obo, List).
load_obo(Directory) :-
    % Handle the case where the input is an existing directory.
    atomic(Directory), 
    exists_directory(Directory),
    % Construct a wildcard pattern to match all OBO files in the directory.
    directory_file_path(Directory, "*.obo", Filename),
    % Expand the wildcard pattern into a list of matching filenames.
    expand_file_name(Filename, List), 
    !, 
    % Recursively process each OBO file in the directory.
    maplist(load_obo, List).
load_obo(Filename) :-
    % Attempt to process the file using process_obo_file/1.
    fail,  % This clause is currently set to fail, likely for debugging or placeholder purposes.
    process_obo_file(Filename),!.
load_obo(Filename) :-
    % If process_obo_file/1 fails, attempt to process the file using process_obo_direct/1.
    process_obo_direct(Filename),!.

%!  process_obo_file(+File) is det.
%
%   Processes an OBO file, generating a corresponding `.metta_x` file.
%   This predicate is an entry point for processing OBO files. If the corresponding
%   `.metta_x` file already exists, it will be processed directly. Otherwise, the 
%   OBO file is processed to generate a new `.metta_x` file, which is then handled.
%
%   @arg File The path to the OBO file to be processed.
%
process_obo_file(File) :-
    % Construct the name of the corresponding `.metta_x` file.
    atom_concat(File, '.metta_x', MXFile),
    % Delegate to the two-argument version of the predicate.
    process_obo_file(File, MXFile).

%!  process_obo_file(+File, +MXFile) is det.
%
%   Processes an OBO file and its corresponding `.metta_x` file.
%   If the `.metta_x` file already exists, it is processed directly. If not, the OBO file
%   is read, a `.metta_x` file is created, duplicates are removed, and the `.metta_x`
%   file is processed.
%
%   @arg File The path to the OBO file.
%   @arg MXFile The path to the corresponding `.metta_x` file.
%
process_obo_file(_File, MXFile) :-
    % If the `.metta_x` file already exists, process it directly.
    exists_file(MXFile),!,process_metta_x_file(MXFile).
process_obo_file(File, MXFile) :-
    % If the OBO file exists, process it to generate a `.metta_x` file.
    exists_file(File),!,
    % Ensure resources are properly managed using `setup_call_cleanup/3`.
    setup_call_cleanup(
        % Open the `.metta_x` file for writing with UTF-8 encoding.
        open(MXFile, write, Strm, [encoding(utf8)]),
        setup_call_cleanup(
            % Temporarily set the output stream alias to `metta_x_output`.
            set_stream(Strm, alias(metta_x_output)),
            % Process the OBO file with the option to generate `.metta_x` content disabled.
            with_option(make_metta_x, 'False', process_obo_direct(File)),
            % Reset the output stream to its original alias.
            set_stream(current_output, alias(metta_x_output))
        ),
        % Close the output stream after processing.
        close(Strm)
    ),
    % Remove any duplicate entries from the `.metta_x` file.
    remove_duplicates(MXFile),
    % Process the `.metta_x` file.
    process_metta_x_file(MXFile).
process_obo_file(File, MXFile) :-
    % If none of the above clauses succeed, throw an exception to indicate an error.
    throw(process_obo_file(File, MXFile)).

%!  remove_duplicates(+InputFile) is det.
%
%   Removes duplicate lines from a file in place.
%   This predicate uses system shell commands to sort the lines in the file, 
%   remove duplicates, and then overwrite the original file with the deduplicated content.
%
%   @arg InputFile The path to the file from which duplicate lines are to be removed.
%
%   @example
%     % Remove duplicate lines from a file named 'data.txt'.
%     ?- remove_duplicates('data.txt').
%
remove_duplicates(InputFile) :-
    % Create a temporary output file
    format(atom(OutputFile), '~w.tmp', [InputFile]),
    % Build the command to remove duplicates and output to the temporary file
    format(atom(Command), 'sort ~w | uniq > ~w', [InputFile, OutputFile]),
    % Execute the command
    shell(Command, ExitStatus),
    % Check the exit status
    ExitStatus =:= 0,
    % Move the temporary output file to the original input file
    format(atom(Command2), 'mv ~w ~w', [OutputFile, InputFile]),
    shell(Command2, ExitStatus2),
    ExitStatus2 =:= 0,!.

% rename_file(+Source, +Destination) - renames or moves a file.
%rename_file(Source, Destination) :-
 %   rename(Source, Destination).

%!  process_obo_direct(+Filename) is det.
%
%   Processes an OBO file directly by loading its content, extracting relevant information, 
%   and asserting it into the Prolog knowledge base.
%   This predicate generates a `.metta` file for the OBO file and tracks its loading progress.
%
%   @arg Filename The path to the OBO file to be processed.
%
process_obo_direct(Filename) :-
    % Extract the directory and base name from the file path.
    directory_file_path(Directory, BaseName, Filename),
    % Extract the identifier (Id) from the base name, ignoring the file extension.
    file_name_extension(Id, _, BaseName),
    % Generate the output file name with the `.metta` extension.
    symbol_concat(Id, '.metta', OutputFile),
    % Optionally prepare to write to the `.metta` file.
    nop(tell(OutputFile)),
    % Track and process the OBO file while asserting relevant information.
    track_load_into_file(Filename,
        must_det_ll((
            Type = 'OntologyFile',
            % Assert the type of the file into the knowledge base.
            assert_OBO(id_type, Id, Type),
            % Store the current ontology ID and type in backtrackable variables.
            nb_setval(obo_id, Id), nb_setval(obo_type, Type),
            % Assert various properties of the file.
            assert_OBO('pathname', Id, Filename), !,
            assert_OBO('basename', Id, BaseName), !,
            assert_OBO('directory', Id, Directory), !,
            % Process the file stream.
            setup_call_cleanup(
                open(Filename, read, Stream, [encoding(utf8)]),
                process_obo_stream_repeat(Stream),
                close(Stream))))),
    % Optionally stop writing to the `.metta` file.
    nop(told).

%!  process_obo_stream_repeat(+Stream) is nondet.
%
%   Repeatedly processes lines from the OBO file stream until the end of the file or 
%   a maximum limit is reached.
%
%   @arg Stream The input stream for the OBO file being processed.
%
process_obo_stream_repeat(Stream) :-
    repeat,
        % Retrieve the current ontology type and ID from backtrackable variables.
        nb_current(obo_type, Type),
        nb_current(obo_id, Id),        
        % Read a line from the file and normalize it to remove leading/trailing spaces.
        once((
            read_line_to_string(Stream, Line),
            % Optionally display data
            ((should_show_data(_),fail) -> writeln(Line) ; true),
            normalize_space(chars(Chars), Line)
        )),
        % If the line is not empty, process its content.
        Chars \== [],
        once(process_obo_chars(Type, Chars, Id)),
        % Stop processing if the end of the file or a maximum limit is reached.
        ((at_end_of_stream(Stream) ; reached_file_max) -> ! ; fail).

%!  process_obo_stream(+Stream, +Type, +Id) is det.
%
%   Processes a single line from the OBO file stream for the given ontology type and ID.
%
%   @arg Stream The input stream for the OBO file being processed.
%   @arg Type   The ontology type (e.g., 'OntologyFile').
%   @arg Id     The unique identifier for the ontology being processed.
%
process_obo_stream(Stream, _Type, _Id) :-
    % Stop processing if the end of the stream or a maximum limit is reached.
    (at_end_of_stream(Stream) ; reached_file_max), !.
process_obo_stream(Stream, Type, Id) :-
    must_det_ll((
        % Read a line from the stream and normalize its content.
        read_line_to_string(Stream, Line),
        % Uncomment the line below to print the line
        % writeln(Line),
        normalize_space(chars(Chars), Line),
        % Process the normalized characters.
        process_obo_chars(Type, Chars, Id))).

%!  into_rest(+Rest, -RestChars, -RestStr) is det.
%
%   Processes the remaining part of a line in an OBO file.
%   This predicate converts the remaining characters into a normalized form
%   (stripped of extra spaces) and extracts both a character list and a string.
%
%   @arg Rest      The input list of characters (rest of the line).
%   @arg RestChars The normalized list of characters.
%   @arg RestStr   The corresponding string form of the normalized characters.
%
into_rest(Rest, RestChars, RestStr) :-
    % Convert the character list into a string.
    obo_string(Str, Rest),
    % Normalize the string by removing excess spaces and converting back to characters.
    normalize_space(chars(RestChars), Str),
    % Convert the normalized characters back into a string.
    obo_string(RestStr, RestChars).

%!  obo_string(?String, +Chars) is det.
%
%   Converts a list of characters to a string or vice versa, handling spaces.
%
%   @arg String The resulting string or input string.
%   @arg Chars  The input or output list of characters.
%
obo_string(String, [C|Chars]) :-
    % Skip spaces and continue processing the character list.
    var(String), C == ' ', !, obo_string(String, Chars).
obo_string(String, Chars) :-
    % Convert between string and character list.
    string_chars(String, Chars).

%!  process_obo_chars(+Type, +Chars, +Id) is det.
%
%   Processes a line of characters from an OBO file.
%   Handles various line formats, including type declarations, IDs, and key-value pairs.
%
%   @arg Type  The current ontology type (e.g., section type).
%   @arg Chars The list of characters representing the line.
%   @arg Id    The current ontology ID being processed.
%
%   
process_obo_chars(_, [e, n, d, '_', o, f, '_', f, i, l, e], _) :- % Special case: End of file marker.
    !.
% Special case: Empty line.
process_obo_chars(_, [], _) :- !.
% Special case: Section header line (e.g., [Term]).
process_obo_chars(_, ['[' | Chars], _) :-
    % Extract the section type by isolating the content between square brackets.
    append(Left, [']'], Chars), !,
    % Determine the section type and store it in a backtrackable variable.
    must_det_ll((
        symbol_chars(Type, Left), 
        !, 
        nb_setval(obo_type, Type)
    )).
% Case: Line starting with 'id', defining a new ontology ID.
process_obo_chars(Type, Chars, _) :-
    % Extract the key and rest of the line.
    get_key(Key, Chars, Rest),
    Key == id,  % Ensure the key is 'id'.
    % Process the rest of the line into normalized forms.
    into_rest(Rest, RestChars, _RestStr),
    % Convert the characters into a symbolic identifier.
    symbol_chars(Id, RestChars),
    % Assert the ID and its type into the ontology.
    assert_OBO(id_type, Id, Type),
    % Update the current ontology ID and type in backtrackable variables.
    nb_setval(obo_id, Id), 
    nb_setval(obo_type, Type).
% Case: Generic key-value line.
process_obo_chars(Type, Chars, Id) :-
    must_det_ll((
        % Extract the key and rest of the line.
        get_key(Key, Chars, Rest),
        % Normalize the rest of the line.
        into_rest(Rest, RestChars, RestStr),
        % Process the line using the extracted key and value.
        process_obo_rest_line(Type, Id, Key, RestChars, RestStr)
    )),
    !.

%!  process_obo_rest_line(+Type, +Id, +Reln, +Rest, +Str) is det.
%
%   Processes a single line from an OBO file based on the given relationship (Reln). 
%   Depending on the type of relationship, this predicate extracts information, 
%   asserts facts, and handles errors where necessary.
%
%   @arg Type  The type of the current OBO entity (e.g., 'term', 'typedef').
%   @arg Id    The identifier for the current OBO entity.
%   @arg Reln  The relationship or key being processed (e.g., `id`, `name`, `is_a`).
%   @arg Rest  The remaining characters or items in the current line.
%   @arg Str   A string representation of the remaining content.
%
%   @example
%     % Example usage for processing a 'name' relationship:
%     ?- process_obo_rest_line(term, 'GO:0008150', name, [], "biological_process").

% Handle 'id' relationship.
process_obo_rest_line(Type, Id, Reln, Rest, _) :-
    Reln = id,
    % Extract 'item(Id)' from Rest and ensure no remaining unprocessed items.
    get_some_items([item(Id)], Rest, []), !,
    % Assert the type of the identifier (e.g., id_type(term, 'GO:0008150')).
    assert_OBO(id_type, Id, Type), !.
% Handle 'name' or 'comment' relationships.
process_obo_rest_line(_Type, Id, Ref, _Chars, S) :-
    % Check if the reference is either 'name' or 'comment'.
    member(Ref, [name, comment]),
    % Assert the relationship directly (e.g., name(term, 'biological_process')).
    assert_OBO(Ref, Id, S), !.
% Handle 'relationship' type and recursively process the rest of the line.
process_obo_rest_line(Type, Id, Reln, Chars, _) :-
    Reln = relationship, !,
    must_det_ll((
        % Parse the key-like string and split the rest into chars and string.
        key_like_string(KeyLike, Chars, Rest),
        symbol_chars(Key, KeyLike),
        into_rest(Rest, RestChars, RestStr),
        % Recursively process the rest of the line.
        process_obo_rest_line(Type, Id, Key, RestChars, RestStr))).
% Handle lines where the reference is a symbol or string.
process_obo_rest_line(_Type, Id, Ref, Chars, _) :-
    % Ensure there are no conflicting characters like `!`, `[`, or `"`.
    \+ (member(C, Chars), member(C, ['!', '[', '"'])),
    % Convert Chars into a symbol or string, depending on its format.
    ( \+ member(' ', Chars) -> symbol_chars(S, Chars) ; obo_string(S, Chars) ),
    % Assert the relationship (e.g., synonym(term, 'example')).
    assert_OBO(Ref, Id, S), !.
% Handle 'is_a' relationships with additional parsing.
process_obo_rest_line(_Type, Id, is_a, Chars, Str) :-
    % Split the line into a left-hand and right-hand part around `!`.
    member('!', Chars),
    symbolic_list_concat([L, R], ' ! ', Str),
    % Normalize spaces and extract both terms.
    normalize_space(atom(T), L),
    normalize_space(string(N), R),
    % Assert the relationships (e.g., is_a(term, parent_term)).
    assert_OBO(is_a, Id, T),
    assert_OBO(name, T, N), !.
% Handle other relationships by extracting arguments and asserting.
process_obo_rest_line(_Type, Id, Reln, Chars, _) :-
    %  member(Reln,[synonym]), 
    % Extract items from Chars and convert them to arguments.
    get_some_items(List, Chars, []),
    maplist(fix_obo_arg, List, Args),
    % Build a compound term and assert it (e.g., synonym(term, 'syn1', 'syn2')).
    Assert =.. [Reln, Id | Args],
    assert_OBO(Assert), !.
% process_obo_rest_line(_Type, Id, Reln, Chars, _) :-
%     get_some_items(List, Chars, []),
%     maplist(arg(1), List, Args),
%     assert_OBO(Reln, Id, Args).
% Catch-all for lines that fail processing, logging the error.
process_obo_rest_line(Type, Id, Miss, Rest, Str) :-
    % Log an error for debugging or further investigation.
    pp_fb('ERROR'(process_obo_rest_line(Type, Id, Miss, Rest, Str))), !.

%!  fix_obo_arg(+Input, -Output) is det.
%
%   Normalizes various types of arguments extracted from OBO files.
%   Ensures that variables, strings, atoms, and compound terms are handled
%   consistently for further processing.
%
%   @arg Input  The input argument to normalize (may be a variable, string, atom, or compound term).
%   @arg Output The normalized result after processing.
%
%   @example
%     % Normalize a string to remove unnecessary spaces:
%     ?- fix_obo_arg("  some_value  ", Normalized).
%     Normalized = "some_value".
%
%     % Convert a compound term to its argument:
%     ?- fix_obo_arg(compound(arg), Normalized).
%     Normalized = arg.
%
fix_obo_arg(Var, Var) :-
    % Leave variables unchanged.
    var(Var), !.
fix_obo_arg("[]", []) :-
    % Convert the string "[]" to an empty list.
    !.
fix_obo_arg('[]', []) :-
    % Convert the atom '[]' to an empty list.
    !.
fix_obo_arg(X, Y) :-
    % Normalize a string by trimming whitespace.
    string(X), !,
    normalize_space(string(Y), X).
fix_obo_arg(X, Y) :-
    % Normalize an atom by trimming whitespace.
    atom(X), !,
    normalize_space(atom(Y), X).
fix_obo_arg(X, Y) :-
    % Handle compound terms by extracting their first argument and normalizing it.
    compound(X),
    arg(1, X, XX), !,
    fix_obo_arg(XX, Y).
fix_obo_arg(X, X). % Leave the argument unchanged if no other case applies.
    

/*
Given the DCG rules we've defined, the input

``` OBO

[Term]
id: FBcv:0000391
name: bang sensitive
namespace: phenotypic_class
def: "A phenotype exhibited following mechanical shock and consisting of a brief period of intense, uncoordinated motor activity (legs and wings flailing, abdomen coiling) followed by a prolonged period of paralysis." [FlyBase:FBrf0022877]
synonym: "easily shocked" RELATED [FlyBase:FBrf0022877]
is_a: FBcv:0000389 ! paralytic

```
Would be parsed into the following Prolog terms:
```
[
    bracketed(['Term']),
    key('id'), item('FBcv:0000391'),
    key('name'), item('bang sensitive'),
    key('namespace'), item('phenotypic_class'),
    key('def'), quoted("A phenotype exhibited following mechanical shock and consisting of a brief period of intense, uncoordinated motor activity (legs and wings flailing, abdomen coiling) followed by a prolonged period of paralysis."), bracketed(['FlyBase:FBrf0022877']),
    key('synonym'), quoted("easily shocked"), keyword('RELATED'), bracketed(['FlyBase:FBrf0022877']),
    key('is_a'), item('FBcv:0000389'), named('paralytic')
]
```

*/

%!  get_key(-Key)// is det.
%
%   Parses a key-like string followed by a colon (`:`) and converts it into a Prolog atom.
%
%   This DCG rule is used to extract structured keys from an input list, ensuring they
%   are represented as valid Prolog atoms.
%
%   @arg Key The parsed key as a Prolog atom.
%
%   @example
%     % Parse a key from an input list:
%     ?- phrase(get_key(Key), [e, x, a, m, p, l, e, ':']).
%     Key = 'example'.
%
%     % This matches the characters `example:` and converts `example` into a Prolog atom.
%
get_key(Key) -->
    key_like_string(Chars),         % Parse a key-like string into a character list.
    [':'],                          % Match the colon following the key.
    { symbol_chars(Key, Chars) },   % Convert the character list into a Prolog atom.
    !.                              % Commit to this rule once matched.

%!  get_some_items(-Items)// is det.
%
%   Parses a list of items, skipping spaces and handling different types
%   of items recursively. The parsed items are returned as a list.
%
%   @arg Items A list of parsed items, where each item is processed by `get_one_item/2`.
%
%   @example
%     % Parse multiple items from a list:
%     ?- phrase(get_some_items(Items), [x, s, d, ':', t, y, p, e, ' ', h, t, t, p, ':', '/', '/']).
%     Items = [quoted('xsd:type'), quoted('http://')].
%
%     % This parses the items "xsd:type" and "http://".
%
get_some_items(I) -->
    % Skip leading spaces and continue parsing.
    [' '], !, get_some_items(I).
get_some_items(_, [], []) :-
    % End parsing when input and output are empty.
    !.
get_some_items([H|T]) -->
    % Parse one item and recursively parse the rest.
    get_one_item(H), get_some_items(T).
get_some_items([]) -->
    % End parsing when no more items are present.
    [].

%!  get_one_item(-Item)// is det.
%
%   Parses a single item, which can be quoted, named, bracketed, or other types,
%   based on its structure in the input list. Specific prefixes (e.g., `xsd:`, `http`) 
%   or delimiters are used to identify item types.
%
%   @arg Item The parsed item, represented in various forms (e.g., quoted/1, named/1).
%
%   @example
%     % Parse a single item starting with `xsd:`:
%     ?- phrase(get_one_item(Item), [x, s, d, ':', t, y, p, e]).
%     Item = quoted('xsd:type').
%
%     % Parse an item enclosed in square brackets:
%     ?- phrase(get_one_item(Item), ['[', x, ',', y, ']']).
%     Item = bracketed([x, y]).
%
get_one_item(I) -->
    % Skip leading spaces and continue parsing.
    [' '], !, get_one_item(I).
get_one_item(quoted(Item)) -->
    % Match prefix `xsd:` and parse a symbol or URL.
    [x, s, d, ':'], symbol_or_url(Chars),
    { symbol_chars(Item, [x, s, d, ':' | Chars]) }.
get_one_item(quoted(Item)) -->
    % Match prefix `http` and parse a symbol or URL.
    [h, t, t, p], symbol_or_url(Chars),
    { obo_string(Item, [h, t, t, p | Chars]) }.
get_one_item(quoted(Item)) -->
    % Match prefix `ftp` and parse a symbol or URL.
    [f, t, p], symbol_or_url(Chars),
    { obo_string(Item, [f, t, p | Chars]) }.
get_one_item(quoted(Item)) -->
    % Match a quoted string enclosed in double quotes.
    ['"'], string_until_end_quote(Chars),
    { obo_string(Item, Chars) }.
get_one_item(named(Item)) -->
    % Match prefix `!` and parse a named-like string.
    ['!'], whs, named_like_string(Chars),
    { symbol_chars(Item, Chars) }.
get_one_item(bracketed(Items)) -->
    % Parse items enclosed in square brackets.
    ['['], whs, items(Items), whs, [']'].
get_one_item(bracketed(Items)) -->
    % Parse items enclosed in curly braces.
    ['{'], whs, items(Items), whs, ['}'].
% get_one_item(item(Item)) -->
%     % Parse key-like items (commented out for alternative behavior).
%     whs, key_like_string(Chars), whs,
%     { Chars \== [], symbol_chars(Item, Chars) }.
get_one_item(keyword(Keyword)) -->
    % Parse keywords using id-like strings.
    whs, id_like_string(Chars), { Chars \== [] }, whs,
    { symbol_chars(Keyword, Chars) }.
get_one_item(text(Text)) -->
    % Parse a named-like string as text.
    named_like_string(Chars),
    { obo_string(Text, Chars) }.
get_one_item(text(Text), [H|T], []) :-
    % Parse ground input directly as text.
    ground([H|T]),
    obo_string(Text, [H|T]), !.

%!  items(-Items)// is det.
%
%   Parses a list of items separated by commas, handling optional whitespace around
%   the commas. The parsed items are returned as a Prolog list.
%
%   @arg Items A list of parsed items, where each item is processed by `item/2`.
%
%   @example
%     % Parse a list of items separated by commas:
%     ?- phrase(items(Items), [x, ',', y, ',', z]).
%     Items = [x, y, z].
%
%     % Parse a single item:
%     ?- phrase(items(Items), [x]).
%     Items = [x].
%
items([Item | Rest]) -->
    % Parse the first item, followed by a comma and the rest of the items.
    item(Item), whs, [','], whs, items(Rest).
items([Item]) -->
    % Parse the last single item without a trailing comma.
    item(Item), !.

%!  item(-Item)// is det.
%
%   Parses a single item, which is a symbol or URL, and converts it into a Prolog atom.
%
%   @arg Item The parsed item as a Prolog atom.
%
%   @example
%     % Parse a single symbol:
%     ?- phrase(item(Item), [x, s, d]).
%     Item = 'xsd'.
%
item(Item) -->
    % Parse a symbol or URL and convert it into an atom.
    symbol_or_url(Chars), { Chars \== [], symbol_chars(Item, Chars) }.

%!  key_like_string(-Chars)// is det.
%
%   Parses a key-like string, consisting of characters not including `:`, whitespace,
%   or newline. The parsed characters are returned as a list.
%
%   @arg Chars The list of parsed characters.
%
%   @example
%     % Parse a key-like string:
%     ?- phrase(key_like_string(Chars), [k, e, y, 1]).
%     Chars = [k, e, y, 1].
%
key_like_string([H | T]) -->
    % Parse a single valid character, followed by the rest of the key.
    [H], { \+ member(H, [':', ' ', '\t', '\n']) }, key_like_string(T).
key_like_string([]) -->
    % End parsing when no more valid characters are present.
    [].

%!  id_like_string(-Chars)// is det.
%
%   Parses an ID-like string, consisting of characters not including delimiters,
%   whitespace, or special characters. The parsed characters are returned as a list.
%
%   @arg Chars The list of parsed characters.
%
%   @example
%     % Parse an ID-like string:
%     ?- phrase(id_like_string(Chars), [i, d, '_', 1]).
%     Chars = [i, d, '_', 1].
%
id_like_string([H | T]) -->
    % Parse a single valid character, followed by the rest of the ID.
    [H], { \+ member(H, ['!', ' ', '\t', '\n', ',', '[', ']', '{', '}', '"']) }, id_like_string(T).
id_like_string([]) -->
    % End parsing when no more valid characters are present.
    [].

%!  symbol_or_url(-Chars)// is det.
%
%   Parses a symbol or URL, consisting of characters not including commas,
%   brackets, quotes, or spaces. The parsed characters are returned as a list.
%
%   @arg Chars The list of parsed characters.
%
%   @example
%     % Parse a URL-like string:
%     ?- phrase(symbol_or_url(Chars), [h, t, t, p, ':', '/', '/']).
%     Chars = [h, t, t, p, ':', '/', '/'].
%
symbol_or_url([H | T]) -->
    % Parse a single valid character, followed by the rest of the symbol or URL.
    [H], { \+ member(H, [',', '[', ']', '"', ' ']) }, symbol_or_url(T).
symbol_or_url([]) -->
    % End parsing when no more valid characters are present.
    [].

%!  string_until_end_quote(-Chars)// is det.
%
%   Parses a string enclosed in double quotes, handling escaped characters.
%
%   @arg Chars The list of characters inside the quotes.
%
%   @example
%     % Parse a quoted string:
%     ?- phrase(string_until_end_quote(Chars), ['"', h, e, l, l, o, '"']).
%     Chars = [h, e, l, l, o].
%
string_until_end_quote([]) -->
    % End parsing when the closing double quote is encountered.
    ['"'], !.
string_until_end_quote([H | T]) -->
    % Parse an escaped character or a normal character.
    (['\\', H]; [H]), !, string_until_end_quote(T).

%!  named_like_string(-Chars)// is det.
%
%   Parses a string of characters that does not include a newline.
%
%   @arg Chars The list of parsed characters.
%
%   @example
%     % Parse a named-like string:
%     ?- phrase(named_like_string(Chars), [n, a, m, e]).
%     Chars = [n, a, m, e].
%
named_like_string([H | T]) -->
    % Parse a single valid character, followed by the rest of the string.
    [H], { \+ member(H, ['\n']) }, named_like_string(T).
named_like_string([]) -->
    % End parsing when no more valid characters are present.
    [].

%!  whs// is det.
%
%   Parses optional whitespace characters.
%
%   @example
%     % Parse whitespace:
%     ?- phrase(whs, [' ', '\t', '\n']).
%     true.
%
whs -->
    % Match an empty character as whitespace and continue parsing.
    [''], !, whs.
whs -->
    % End parsing if no more whitespace is present.
    [].


% ===========================================
% END OBO Loader
% ===========================================

%!  assert_OBO(+Fact) is det.
%
%   Asserts an OBO (Ontology-Based Ontology) fact into the knowledge base.
%   Handles different forms of property values, synonyms, ontology info, and lists
%   while simplifying arguments or converting them for Metta representation.
%
%   @arg Fact The OBO fact to assert. It can represent properties, synonyms,
%             ontology information, or other structured terms.
%
%   @example
%     % Assert a property value for a term with a simplified URI:
%     ?- assert_OBO(property_value('GO:0008150', 'http://example.org/has_name', 'Process Name')).
%     % This will simplify the URI and assert it as:
%     % property_value('GO:0008150', has_name, 'Process Name').
%
assert_OBO(property_value(Term, URI, V, 'xsd:string')) :-
    % Handle property values explicitly typed as xsd:string.
    assert_OBO(property_value(Term, URI, V)).
assert_OBO(property_value(Term, URI, V)) :-
    % Simplify the URI and re-assert as property_value/3.
    simplify_obo_arg(URI, Pred), !,
    assert_OBO(property_value(Term, Pred, V)).
assert_OBO(property_value(Term, Pred, V)) :-
    % Simplify the value argument and re-assert as property_value/3.
    simplify_obo_arg(V, VV), !,
    assert_OBO(property_value(Term, Pred, VV)).
assert_OBO(property_value(Term, Pred, V)) :-
    % Convert property_value into a Prolog fact if Pred is an atom.
    atom(Pred), !,
    OBO =.. [Pred, Term, V],
    assert_OBO(OBO).
assert_OBO(synonym(Pred, A, Term, V)) :-
    % Simplify the value argument in synonym/4 and re-assert.
    simplify_obo_arg(V, VV), !,
    assert_OBO(synonym(Pred, A, Term, VV)).
assert_OBO(ontology_info(Pred, Term, V)) :-
    % Convert ontology_info/3 into a property_value/3 fact and re-assert.
    assert_OBO(property_value(Term, Pred, V)).
assert_OBO([F|List]) :-
    % Handle lists where the first element is an atom, converting into a fact.
    is_list([F|List]), atom(F),
    OBO =.. [F | List], !,
    assert_OBO(OBO).
assert_OBO(OBO) :-
    % General case for asserting an OBO fact.
    must_det_ll((
        OBO =.. [Fn | Cols],
        into_obofn(Fn, OboFn),
        assert_OB1([OboFn | Cols])
    )).

%!  assert_OB1(+List) is det.
%
%   Handles the assertion of OBO facts depending on the Metta export state.
%   If the `make_metta_x` flag is true, it writes to the Metta output.
%   Otherwise, it asserts the fact in its Prolog representation.
%
%   @arg List The list representation of the OBO fact.
%
%   @example
%     % Assert a fact in the form of a list:
%     ?- assert_OB1(['has_part', 'GO:0008150', 'GO:0009987']).
%     % This will convert and assert as:
%     % has_part('GO:0008150', 'GO:0009987').
%
assert_OB1(List) :-
    nb_current(make_metta_x, 'True'), !,
    assert_OB2(List).
assert_OB1([OboFn | Cols]) :-
    % Convert the list to a Prolog fact and assert.
    OBO1 =.. [OboFn | Cols],
    assert_to_metta(OBO1).

%!  assert_OB2(+List) is det.
%
%   Writes the given list of arguments to the Metta output file in tab-separated format.
%
%   @arg List The list of arguments to write to the Metta output.
%
%   @example
%     % Write a fact to the Metta output file:
%     ?- assert_OB2(['has_part', 'GO:0008150', 'GO:0009987']).
%     % This will produce the output:
%     % has_part    GO:0008150    GO:0009987
%
assert_OB2(List) :-
    maplist(to_metta_x_args, List, ListO),
    atomics_to_string(ListO, '\t', Str),
    writeln(metta_x_output, Str).

%!  to_metta_x_args(+Arg, -FormattedArg) is det.
%
%   Converts arguments into a format suitable for Metta output.
%
%   @arg Arg           The argument to convert.
%   @arg FormattedArg  The formatted argument for Metta output.
%
%   @example
%     % Convert an atomic argument to a Metta-compatible format:
%     ?- to_metta_x_args('GO:0008150', Formatted).
%     Formatted = 'GO:0008150'.
%
to_metta_x_args(X, O) :-
    X == [], !, O = '[]'.
to_metta_x_args(X, O) :-
    atomic(X), !, O = X.
to_metta_x_args(X, O) :-
    term_to_atom(X, O).

%!  args_x_metta(+Arg, -ConvertedArg) is det.
%
%   Converts arguments from Metta format back to Prolog representations.
%
%   @arg Arg           The argument to convert.
%   @arg ConvertedArg  The converted Prolog argument.
%
%   @example
%     % Convert a Metta-style argument back to a Prolog term:
%     ?- args_x_metta('?- parent(child)', Converted).
%     Converted = parent(child).
%
args_x_metta(X, O) :-
    X == '[]', !, O = [].
args_x_metta(X, O) :-
    atomic(X), atom_concat('?-', Read, X), !,
    atom_to_term(Read, O, _).
args_x_metta(X, O) :-
    X = O.

/*
  OBOW=..[OboFn|Cols],
  (is_converting -> (format('~N'), write_src(OBOW));(OBO1=..OBOW,assert_MeTTa(OBO1))))),
  !.
*/

%!  into_obofn(+Fn, -OboFn) is det.
%
%   Converts a function name into an OBO-style function name, ensuring consistency.
%   If the function already has the 'obo-' prefix, it is retained.
%   Otherwise, the 'obo-' prefix is added, and underscores are converted to dashes.
%
%   @arg Fn     The original function name (atom).
%   @arg OboFn  The converted OBO-style function name (atom).
%
%   @example
%     % Example: Function with 'obo-' prefix remains unchanged:
%     ?- into_obofn('obo-term', OboFn).
%     OboFn = 'obo-term'.
%
%     % Example: Function name gets 'obo-' prefix added:
%     ?- into_obofn('term', OboFn).
%     OboFn = 'obo-term'.
%
into_obofn(Fn, OboFn) :-
    atom_concat('obo-', _, Fn), !,
    Fn = OboFn, !.
into_obofn(Fn, OboFn) :-
    atom_concat('obo-', Fn, OboF_), !,
    use_dashes(OboF_, OboFn).

%!  use_dashes(+OboF_, -OboFn) is det.
%
%   Converts underscores in a string to dashes.
%
%   @arg OboF_  The input string with underscores.
%   @arg OboFn  The resulting string with underscores replaced by dashes.
%
%   @example
%     % Convert underscores to dashes:
%     ?- use_dashes('obo_some_term', OboFn).
%     OboFn = 'obo-some-term'.
%
use_dashes(OboF_, OboFn) :-
    symbolic_list_concat(List, '_', OboF_),
    symbolic_list_concat(List, '-', OboFn), !.

%!  simplify_obo_arg(+Input, -Output) is nondet.
%
%   Simplifies OBO arguments by processing strings or atoms to extract or convert
%   meaningful content. Handles special cases such as empty lists, certain prefixes,
%   and numeric conversions.
%
%   @arg Input   The original argument (string or atom).
%   @arg Output  The simplified result.
%
%   @example
%     % Simplify an OBO URI to a specific term:
%     ?- simplify_obo_arg('http://purl.obolibrary.org/obo/chebi/12345', Term).
%     Term = '12345'.
%
%     % Convert a string into a numeric value:
%     ?- simplify_obo_arg('42', Value).
%     Value = 42.
%
simplify_obo_arg(I, _O) :-
    % Fail if the input is neither a string nor an atom.
    \+ string(I), \+ atom(I), !, fail.
simplify_obo_arg([], _O) :-
    % Fail if the input is an empty list.
    !, fail.
simplify_obo_arg("[]", []) :-
    % Convert the string "[]" to an empty list.
    !.
simplify_obo_arg(I, O) :-
    % Extract a term from a CHEBI URI.
    atom_concat('http://purl.obolibrary.org/obo/chebi/', O, I), !.
simplify_obo_arg(I, O) :-
    % Simplify a string by removing leading spaces.
    atom_concat(' ', O, I), !.
simplify_obo_arg(I, O) :-
    % Simplify a string by removing trailing spaces.
    atom_concat(O, ' ', I), !.
simplify_obo_arg(I, O) :-
    % Convert a numeric string to a number.
    atom_number(I, O), !.
