:- module(lsp_metta_formatter, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_metta_formater.pl
%
% Purpose:
%
% A Prolog implementation of a documentFormattingProvider for Lisp code.
%
% This module formats Lisp code by ensuring consistent indentation and removing
% unnecessary whitespace. The formatting rules account for Lisp's syntax, including
% proper indentation for nested expressions and balanced parentheses.
%
% How It Works:
% - Reads the document text from the provided URI.
% - Splits the document into lines.
% - Applies MeTTa-specific formatting rules, such as:
%   - Consistent indentation for nested parentheses.
%   - Removing unnecessary trailing whitespace.
%   - Ensuring consistent line breaks between nested forms.
% - Returns a list of text edits to apply to the document.
%
% Author: Douglas Miles
% Date: 10-21-2024
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    :- include(lsp_metta_include).

:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(lists)).

:- use_module(lsp_metta_workspace, [source_file_text/2, maybe_doc_path/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the 'textDocument/formatting' Request and Format the Lisp Document
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_msg_hook("textDocument/formatting", Msg, _{id: Id, result: Edits}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}} :< Params,
    format_lisp_document(Uri, Edits).  % Perform the document formatting

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Format the Lisp Document by Applying Formatting Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_lisp_document(Uri, Edits) :-
    source_file_text(Uri, Text),      % Retrieve the document text
    split_string(Text, "\n", "", Lines),
    string_codes(Text, Codes),
    phrase(metta_lines(ParsedLines), Codes),
    process_lines(0, ParsedLines, ProcessedLines),
    lines_to_strings(ProcessedLines, FormattedLines),
    create_edit_list(Lines, FormattedLines, Edits).  % Create a list of edits based on the differences

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

whitespace(N) -->
    [C], { nonvar(C), code_type(C, white) },
    whitespace(1, N).
whitespace(N0, N) -->
    [C],
    { nonvar(C), code_type(C, white), !,
      N1 is N0 + 1
    },
    whitespace(N1, N).
whitespace(N, N) --> [].

open_paren -->  "(".
close_paren --> ")".

comment(Comment) -->
    ";", string_without("\n", Comment).

metta_string(Content) -->
    [0'"],
    metta_string_content(Content, Content).

metta_string_content(Content, Tail0) -->
    [0'\\, C], !,
    { Tail0 = [0'\\, C|Tail1] },
    metta_string_content(Content, Tail1).
metta_string_content(_, []) --> [0'"], !.
metta_string_content(Content, Tail0) -->
    [C],
    { Tail0 = [C|Tail1] },
    metta_string_content(Content, Tail1).

text(Text) --> string_without(" \t\n()", Text), { Text \= [] }.

metta_line([]) --> "\n", !.
metta_line([white(N)|Rest]) --> whitespace(N), !, metta_line(Rest).
metta_line([comment(Cs)|Rest]) -->
    comment(C), !, { string_codes(Cs, C) },
    metta_line(Rest).
metta_line([string(S)|Rest]) -->
    metta_string(C), !, { string_codes(S, C) },
    metta_line(Rest).
metta_line([open|Rest]) --> open_paren, !, metta_line(Rest).
metta_line([close|Rest]) --> close_paren, !, metta_line(Rest).
metta_line([atom(Text)|Rest]) -->
    text(Codes), !, { string_codes(Text, Codes) },
    metta_line(Rest).

metta_lines([Line|Lines]) -->
    metta_line(Line), !, metta_lines(Lines).
metta_lines([]) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process parsed lines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% single-line cleanups
drop_leading_white([white(_)|Rest], Rest) :- !.
drop_leading_white(L, L).

drop_trailing_white([], []) :- !.
drop_trailing_white([white(_)], []) :- !.
drop_trailing_white([X|Rest], [X|Rest0]) :- drop_trailing_white(Rest, Rest0).

trim_whites(Line0, Line) :-
    drop_leading_white(Line0, Line1),
    drop_trailing_white(Line1, Line).

normalize_whitespaces([open, white(_), X|Rest], [open, X|Rest1]) :-
    \+ \+ ( X = atom(_) ; X = close ; X = open), !,
    normalize_whitespaces(Rest, Rest1).
normalize_whitespaces([white(_), close|Rest], [close|Rest1]) :- !,
    normalize_whitespaces(Rest, Rest1).
% Is this too aggressive?
normalize_whitespaces([atom(A), white(_), atom(B)|Rest], [atom(A), white(1), atom(B)|Rest1]) :-
    !, normalize_whitespaces(Rest, Rest1).
normalize_whitespaces([X|Rest0], [X|Rest1]) :- !,
    normalize_whitespaces(Rest0, Rest1).
normalize_whitespaces([], []).

line_net_parens(N, N, []) :- !.
line_net_parens(N0, N, [open|Rest]) :- !,
    N1 is N0 + 1,
    line_net_parens(N1, N, Rest).
line_net_parens(N0, N, [close|Rest]) :- !,
    N1 is N0 - 1,
    line_net_parens(N1, N, Rest).
line_net_parens(N0, N, [_|Rest]) :-
    line_net_parens(N0, N, Rest).

process_line(Parens0, Parens1, Line0, Line) :-
    trim_whites(Line0, Line1),
    normalize_whitespaces(Line1, Line2),
    line_net_parens(Parens0, Parens1, Line2),
    ( Parens0 > 0
    -> Indent is Parens0 * 2,
       Line = [white(Indent)|Line2]
    ;  Line = Line2).

% multi-line cleanups

line_ending_comment(Line, Tail, Rest) :-
    append(Rest, Tail, Line),
    ( Tail = [white(_), comment(_)] ; Tail = [comment(_)] ),
    Rest = [_|_], !.

not_just_comment_line(Line) :-
    member(E, Line), E \= comment(_), E \= white(_), !.

no_orphaned_close_parens([Line1,Line2|Rest], OutRest) :-
    forall(member(E, Line2), once(( E = close ; E = white(_) ))),
    trim_whites(Line2, TrimLine2), TrimLine2 \= [],
    not_just_comment_line(Line1), !,
    ( line_ending_comment(Line1, Comment, Line1Rest)
    -> append(Line1Rest, TrimLine2, OutLine0),
       append(OutLine0, Comment, OutLine1)
    ;  append(Line1, TrimLine2, OutLine1)
    ),
    no_orphaned_close_parens([OutLine1|Rest], OutRest).
no_orphaned_close_parens([L|Rs], [L|ORs]) :-
    no_orphaned_close_parens(Rs, ORs).
no_orphaned_close_parens([], []).

process_individual_lines(_Indent, [], []) :- !.
process_individual_lines(Indent0, [Line0|OldLines], [Line1|NewLines]) :-
    process_line(Indent0, Indent1, Line0, Line1),
    process_individual_lines(Indent1, OldLines, NewLines).

process_lines(Indent, Lines, ProcessedLines) :-
    process_individual_lines(Indent, Lines, Lines1),
    no_orphaned_close_parens(Lines1, ProcessedLines).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Emitting parsed lines back to strings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
emit_line(_To, []) => true. % format(To, "~n", []).
emit_line(To, [white(N)|Rest]) =>
    length(Whites, N),
    maplist(=(0' ), Whites),
    format(To, "~s", [Whites]),
    emit_line(To, Rest).
emit_line(To, [comment(Comment)|Rest]) =>
    format(To, ";~s", [Comment]),
    emit_line(To, Rest).
emit_line(To, [string(String)|Rest]) =>
    format(To, "\"~s\"", [String]),
    emit_line(To, Rest).
emit_line(To, [open|Rest]) => format(To, "(", []), emit_line(To, Rest).
emit_line(To, [close|Rest]) => format(To, ")", []), emit_line(To, Rest).
emit_line(To, [atom(Atom)|Rest]) =>
    format(To, "~w", [Atom]),
    emit_line(To, Rest).

emit_lines(To, [Line|Lines]) :-
    emit_line(To, Line),
    format(To, "~n", []),
    emit_lines(To, Lines).
emit_lines(_, []).

lines_to_strings([Line|Lines], [FormattedLine|FormattedLines]) :-
    with_output_to(string(FormattedLine), emit_line(current_output, Line)),
    lines_to_strings(Lines, FormattedLines).
lines_to_strings([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a List of Edits from the Original and Formatted Lines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_edit_list(Orig, Formatted, Edits) :-
    create_edit_list(0, Orig, Formatted, Edits).

create_edit_list(_, [], [], []) :- !.
create_edit_list(LineNum, [Line|Lines], [], [Edit|Edits]) :- !,
    string_length(Line, LenLen),
    Edit = _{range: _{start: _{line: LineNum, character: 0},
                      end: _{line: LineNum, character: LenLen}},
            newText: ""},
    succ(LineNum, LineNum1),
    create_edit_list(LineNum1, Lines, [], Edits).
create_edit_list(LineNum, [], [NewLine|NewLines], [Edit|Edits]) :- !,
    string_length(NewLine, LenLen),
    Edit = _{range: _{start: _{line: LineNum, character: 0},
                      end: _{line: LineNum, character: LenLen}},
            newText: NewLine},
    succ(LineNum, LineNum1),
    create_edit_list(LineNum1, [], NewLines, Edits).
create_edit_list(LineNum, [OrigLine | OrigRest], [FormattedLine | FormattedRest], Edits) :-
    (   OrigLine \= FormattedLine  % Only create an edit if the line has changed
    -> string_length(OrigLine, LineLen), %TODO: what should this be?
       Edit = _{
                  range: _{
                             start: _{line: LineNum, character: 0},
                             end: _{line: LineNum, character: LineLen}
                         },
                  newText: FormattedLine
              },
       Edits = [Edit|EditRest]
    ; EditRest = Edits
    ),
    succ(LineNum, LineNum1),
    create_edit_list(LineNum1, OrigRest, FormattedRest, EditRest).
