% Load necessary libraries
:- use_module(library(http/json)).

% Entry point: save_nodes_to_prolog(+InputJsonFile)
save_nodes_to_prolog(Directory,InputJsonFile) :-
    open(InputJsonFile, read, InStream, [encoding(utf8)]),
    skip_to_first_node(InStream),
    process_nodes_stream(Directory,InStream, 1, 0, _OutStream),
    close(InStream).

% Skip characters until reaching the "nodes":{ part
skip_to_first_node(Stream) :-
    get_char(Stream, Char),
    skip_to_first_node(Stream, Char, "\"nodes\":{").

skip_to_first_node(_, _, []) :- !.
skip_to_first_node(Stream, Char, [Char|Rest]) :-
    get_char(Stream, NextChar),
    skip_to_first_node(Stream, NextChar, Rest), !.
skip_to_first_node(Stream, _, Pattern) :-
    get_char(Stream, NextChar),
    skip_to_first_node(Stream, NextChar, Pattern).

% Process nodes incrementally
process_nodes_stream(Directory,InStream, FileIndex, TermCount, OutStream) :-
    read_node_term(InStream, NodeTerm),
    (   NodeTerm == end_of_nodes
    ->  (nonvar(OutStream) -> close(OutStream); true)
    ;   ( TermCount mod 1_000_000 =:= 0
        -> ( nonvar(OutStream) -> close(OutStream); true ),
           open_next_output(Directory,FileIndex, NextOutStream),
           NextFileIndex is FileIndex + 1,
           NextTermCount is TermCount + 1,
           write_json_predicate(NodeTerm, NextTermCount, NextOutStream),
           process_nodes_stream(Directory,InStream, NextFileIndex, NextTermCount, NextOutStream)
        ;  ( var(OutStream) -> open_next_output(Directory,FileIndex, OutStream); true ),
           NextTermCount is TermCount + 1,
           write_json_predicate(NodeTerm, NextTermCount, OutStream),
           process_nodes_stream(Directory,InStream, FileIndex, NextTermCount, OutStream)
        )
    ).

% Read single node term manually character by character
read_node_term(Stream, NodeTerm) :-
    read_term_chars(Stream, Chars),
    ( Chars = [] -> NodeTerm = end_of_nodes
    ; atom_chars(Atom, Chars),
      atom_json_dict(Atom, NodeTerm, [])
    ).

% Read characters for a single JSON node
read_term_chars(Stream, Chars) :-
    skip_whitespace(Stream, FirstChar),
    ( FirstChar == '}' -> Chars = []
    ; read_until_closing_brace(Stream, [FirstChar], Chars)
    ).

read_until_closing_brace(Stream, Acc, Chars) :-
    get_char(Stream, Char),
    ( Char == ',' -> reverse(['}'|Acc], Chars)
    ; Char == end_of_file -> reverse(Acc, Chars)
    ; read_until_closing_brace(Stream, [Char|Acc], Chars)
    ).

skip_whitespace(Stream, Char) :-
    get_char(Stream, Char),
    ( char_type(Char, space) -> skip_whitespace(Stream, _)
    ; true
    ).

% Open next output file
open_next_output(Directory, FileIndex, OutStream) :-
    format(atom(FileName), '~w/saved_~|~`0t~d~12+.pl', [Directory, FileIndex]),
    open(FileName, write, OutStream, [encoding(utf8)]).

% Write predicate to output stream
write_json_predicate(JsonTerm, TermCount, OutStream) :-
    format(OutStream, 'json_term(~w, ', [TermCount]),
    writeq(OutStream, JsonTerm),
    writeln(OutStream, ').').


save_nodes_to_prolog_csv:-
   SaveTo = './neo4j_out_v3_json/',
   make_directory(SaveTo),
   save_nodes_to_prolog(SaveTo,'/mnt/c/Users/logicmoo/Downloads/whole_flybase.json'),
   !.

