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

:- use_module(lsp_metta_workspace, [source_file_text/2]).

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
    split_string(Text, "\n", "", Lines),  % Split the text into lines
    format_lisp_lines(Lines, 0, 0, FormattedLines),  % Format each line, starting with indent level 0 and open parentheses 0
    create_edit_list(Lines, FormattedLines, Edits).  % Create a list of edits based on the differences

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Format Each Line According to Lisp Syntax and Indentation Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format_lisp_lines([], _, _, []).  % Base case: no more lines
format_lisp_lines([Line | Rest], IndentLevel, OpenParens, [FormattedLine | FormattedRest]) :-
    trim_line(Line, TrimmedLine),  % Remove trailing whitespace from the current line
    calculate_new_indent(TrimmedLine, IndentLevel, OpenParens, NewIndentLevel, NewOpenParens),  % Determine the new indent level and open parentheses count
    indent_line(TrimmedLine, IndentLevel, FormattedLine),  % Indent this line based on the old indent level
    format_lisp_lines(Rest, NewIndentLevel, NewOpenParens, FormattedRest).  % Format the rest of the lines

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Trim Leading and Trailing Whitespace from a Line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trim_line(Line, TrimmedLine) :-
    split_string(Line, "", "\t\s\n", [TrimmedLine]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate New Indentation Level and Open Parentheses Count
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_new_indent(Line, IndentLevel, OpenParens, NewIndentLevel, NewOpenParens) :-
    count_open_close_parens(Line, Opens, Closes),  % Count open and close parentheses
    NewOpenParens is OpenParens + Opens - Closes,  % Update the number of open parentheses
    (NewOpenParens > OpenParens -> NewIndentLevel is IndentLevel + 2 ; NewIndentLevel is IndentLevel).

% Helper to count open and close parentheses in a line
count_open_close_parens(Line, Opens, Closes) :-
    sub_atom_count(Line, '(', Opens),
    sub_atom_count(Line, ')', Closes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Indent a Line Based on the Current Indentation Level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
indent_line(Line, IndentLevel, IndentedLine) :-
    IndentSpaces is IndentLevel,  % Indentation level determines the number of spaces
    generate_spaces(IndentSpaces, Spaces),  % Create the indent string
    string_concat(Spaces, Line, IndentedLine).  % Prepend the spaces to the line

generate_spaces(0, "") :- !.
generate_spaces(N, Spaces) :-
    N > 0,
    N1 is N - 1,
    generate_spaces(N1, SubSpaces),
    string_concat(' ', SubSpaces, Spaces).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a List of Edits from the Original and Formatted Lines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_edit_list(Orig, Formatted, Edits) :-
    create_edit_list(0, Orig, Formatted, Edits).

create_edit_list(_, [], [], []).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper to count the number of specific characters (e.g., '(' or ')')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sub_atom_count(Line, Char, Count) :-
    atom_chars(Line, Chars),
    include(=(Char), Chars, CharList),
    length(CharList, Count).
