:- module(lsp_metta_utils, [
%                       called_at/4,
%                       defined_at/3,
%                       name_callable/2,
%                       relative_ref_location/4,
%                       clause_variable_positions/3,
%                       seek_to_line/2,
%                       linechar_offset/3,
%                       clause_in_file_at_position/3,
                        help_at_position/4
                        ]).
:- use_module(library(debug), [debug/3]).
:- use_module(lsp_metta_xref).
:- use_module(metta_eval, [eval_args/6]).

% /** <module> LSP Utils
%
% Module with a bunch of helper predicates for looking through prolog
% source and stuff.
%
% @author James Cash
% */
%! help_at_position(+Path:atom, +Line:integer, +Char:integer, -Help:string) is det.
%
%  =Help= is the documentation for the term under the cursor at line
%  =Line=, character =Char= in the file =Path=.

%!  linechar_offset(+Stream:stream, +Position:line_char, -Offset:int) is det.
%
%   Converts a line and character position into a byte offset in the given stream.
%   This predicate seeks to the specified line and character within the stream.
%
%   @arg Stream is the input stream being read.
%   @arg Position is a term of the form line_char(Line, Char), representing the line and character to seek to.
%   @arg Offset is the resulting byte offset corresponding to the position.
%
%   @example Convert line and character position to byte offset:
%       ?- open('file.pl', read, Stream), linechar_offset(Stream, line_char(5, 10), Offset).
%       Offset = 65.
%
linechar_offset(Stream, line_char(Line1, Char0), Offset, PreChars) :-
    % Seek to the beginning of the stream (bof = beginning of file).
    seek(Stream, 0, bof, _),
    % Seek to the specified line number in the stream.
    seek_to_line(Stream, Line1),
    % Seek to the specified character position from the current line position.
    accumulating_pre_seek(Stream, Char0, [], PreChars),
    seek(Stream, 0, current, Offset).

accumulating_pre_seek(_Stream, 0, CharList, CharList) :- !.
accumulating_pre_seek(Stream, N, CharListIn, CharListOut) :- N>0,
    get_char(Stream, Char),
    ((char_type(Char,csym);Char='-';Char='$') -> append(CharListIn,[Char],Current) ; Current=[]),
    Nnext is N-1,
    accumulating_pre_seek(Stream, Nnext, Current, CharListOut).

accumulating_post_seek(Stream, CharListIn, CharListOut) :-
    get_char(Stream, Char),
    ((char_type(Char,csym);Char='-') ->
        (append(CharListIn,[Char],Current),
        accumulating_post_seek(Stream,Current,CharListOut))
    ; CharListOut=CharListIn).

%!  seek_to_line(+Stream:stream, +Line:int) is det.
%
%   Seeks to the specified line in the stream by skipping lines until the target line is reached.
%
%   @arg Stream is the input stream.
%   @arg Line is the line number to seek to.
%
%   @example Seek to line 5 in a file:
%       ?- open('file.pl', read, Stream), seek_to_line(Stream, 5).
%
seek_to_line(Stream, N) :-
    % If N is greater than 1, we need to skip lines.
    N > 1, !,
    % Skip the current line by searching for a newline character.
    skip(Stream, 0'\n),
    % Decrement the line counter.
    NN is N - 1,
    % Recursively seek to the remaining lines.
    seek_to_line(Stream, NN).
% Base case: If N is 1, we have reached the desired line.
seek_to_line(_, _).

% help_at_position(_Path, _Line1, _Char0, "blah blah blah") :- !. % clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)), !.
help_at_position(Path, Line1, Char0, S) :-
    debug(server,"help_at_position",[]),
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    S=Clause.
%     predicate_help(Path, Clause, S0),
%     format_help(S0, S).

%!  clause_in_file_at_position(-Clause, +Path, +Position) is det.
%
%   Reads the clause located at the specified position within the given file.
%
%   @arg Clause is the clause found at the specified position.
%   @arg Path is the path to the file being analyzed.
%   @arg Position is the position in the file (typically line/character position).
%
%   @example Example usage:
%       ?- clause_in_file_at_position(Clause, 'file.pl', line_char(5, 10)).
%       Clause = (some_prolog_fact :- some_prolog_goal).
%
clause_in_file_at_position(Clause, Path, Position) :-
    % Setup a stream to read the file and find the clause at the specified position.
    debug(server,"clause_in_file_at_position",[]),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        % Call clause_at_position to extract the clause at the given position.
        clause_at_position(Stream, Clause, Position),
        % Close the stream once the operation is done.
        close(Stream)
    ).

%!  clause_at_position(+Stream, -Clause, +Start) is det.
%
%   Extracts a clause from the stream at the specified start position.
%
%   @arg Stream is the input stream of the source file.
%   @arg Clause is the clause extracted from the stream.
%   @arg Start is the starting position to search for the clause.
%
%   @example Example usage:
%       ?- clause_at_position(Stream, Clause, line_char(5, 10)).
%       Clause = (some_clause).
%
clause_at_position(Stream, Clause, Start) :-
    debug(server,"clause_at_position1",[]),
    % Convert the line/character position into an offset within the stream.
    linechar_offset(Stream, Start, Offset, Prechars),!,
    accumulating_post_seek(Stream,Prechars,Chars),
    string_chars(String,Chars),
    Clause=String.
    % TODO - add this in when I can import eval_args
    %atom_string(Atom,String),
    %debug(server,"clause ~w",[Atom]),
    %eval_args(=,_RetType,499,'&self',['help!',Atom],['Error',_,Clause]).

    % Call clause_at_position with the computed offset.
    %clause_at_position(Stream, Clause, Start, Offset).

%!  clause_at_position(+Stream, -Clause, +Start, +Here) is det.
%
%   Retrieves a clause at the specified line and character position within the stream.
%
%   @arg Stream is the input stream of the source file.
%   @arg Clause is the clause found at the given position.
%   @arg Start represents the line and character position to start from.
%   @arg Here is the offset position calculated earlier.
%
%   @see read_source_term_at_location/3 for term reading.
%
clause_at_position(Stream, Clause, line_char(Line1, Char), Here) :-
    debug(server,"clause_at_position2 ~w",[Here]),
    peek_string(Stream,10,Clause).
%    % Read terms from the source file at the given position.
%    read_source_term_at_location(Stream, Terms, [line(Line1),
%                                                 subterm_positions(SubPos),
%                                                 operators(Ops),
%                                                 error(Error)]),
%    % Extract the clause at the specified position.
%    extract_clause_at_position(Stream, Ops, Terms, line_char(Line1, Char), Here,
%                               SubPos, Error, Clause).
