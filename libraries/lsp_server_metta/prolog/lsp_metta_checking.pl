:- module(lsp_metta_checking, [metta_check_errors/2]).
/** <module> LSP Checking
 Module for checking Prolog source files for errors and warnings.
 @author Roy Ward
*/
% :- use_module(library(apply_macros)).
% :- use_module(library(assoc), [list_to_assoc/2,
%                                get_assoc/3]).
% :- use_module(library(apply), [maplist/3]).
% :- use_module(library(debug), [debug/3]).
% :- use_module(library(lists), [member/2]).
% :- use_module(library(prolog_xref), [xref_clean/1, xref_source/1]).
% :- use_module(lsp_metta_utils, [clause_variable_positions/3]).
%
% :- dynamic message_hook/3.
% :- multifile message_hook/3.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_metta_did_save_diagnostics.pl
%
% Purpose:
%
% A Prolog implementation of the `didSave` handler for the LSP server, which
% triggers diagnostics upon file save and replies with a `textDocument/publishDiagnostics`
% notification. This notification allows the editor or IDE to display errors, warnings,
% or other diagnostic information related to the saved document.
%
% How It Works:
% - Upon receiving a `didSave` notification, the system analyzes the document.
% - If issues are found (e.g., syntax errors, undefined symbols), a diagnostics list is created.
% - A `textDocument/publishDiagnostics` notification is sent back to the client with
%   the list of diagnostic issues found in the document.
%
% Author: [Your Name]
% Date: [Date]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the 'textDocument/didSave' Request and Publish Diagnostics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_msg_hook("textDocument/didSave", Msg, Result) :- fail, % FAIL for now (allow other code to do their things)
    _{params: _{textDocument: _{uri: Uri}}} :< Msg,
    source_file_text(Uri, Text),
    analyze_document_for_diagnostics(Text, Diagnostics),  % Analyze the document
    publish_diagnostics(Uri, Diagnostics, Result), !.  % Publish the diagnostics


%! metta_check_errors(+Path:atom, -Errors:List) is det.
%
%  =Errors= is a list of the errors in the file given by =Path=.
%  This predicate checks the =metta_file_buffer/7= cache for saved errors.

% will do some real error checking later
metta_check_errors(Uri, Diagnostics):-
    source_file_text(Uri, Text),  % Retrieve the saved document text
    analyze_document_for_diagnostics(Text, Diagnostics),!.  % Analyze the document
    % publish_diagnostics(Uri, Diagnostics), !.  % Publish the diagnostics
metta_check_errors(_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Analyze the Document for Issues and Generate Diagnostics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_document_for_diagnostics(Text, ValidDiagnostics) :-
    split_string(Text, "\n", "", Lines),  % Split the text into lines
    analyze_lines(Lines, 0, Diagnostics),!,  % Analyze each line for issues
    exclude(=([]), Diagnostics, ValidDiagnostics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Analyze Each Line and Collect Diagnostic Information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_lines([], _, []).  % Base case: no more lines to analyze
analyze_lines([Line | Rest], LineNum, [Diagnostic | DiagnosticsRest]) :-
    (   find_issue_in_line(Line, LineNum, Diagnostic)  % If an issue is found, create a diagnostic
    ->  true
    ;   Diagnostic = []  % No issue found on this line, so return an empty list
    ),
    NextLineNum is LineNum + 1,  % Move to the next line
    analyze_lines(Rest, NextLineNum, DiagnosticsRest).  % Analyze the remaining lines

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Identify Issues in a Line and Create a Diagnostic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_issue_in_line(Line, LineNum, Diagnostic) :-
    (   (has_unmatched_parens(Line), string_length(Line, Len))
    ->  Diagnostic = _{
            range: _{
                start: _{line: LineNum, character: 0},
                end: _{line: LineNum, character: Len}
            },
            severity: 1,  % Error severity
            message: "Unmatched parentheses found."
        }
    ;   Diagnostic = []
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example Diagnostic: Check for Unmatched Parentheses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_unmatched_parens(Line) :- fail,
    count_open_close_parens(Line, Opens, Closes),
    Opens \= Closes.

count_open_close_parens(Line, Opens, Closes) :-
    sub_atom_count(Line, '(', Opens),
    sub_atom_count(Line, ')', Closes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Publish Diagnostics to the Client
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
publish_diagnostics(Uri, Diagnostics, Message) :-
    % Filter out any empty diagnostics
    exclude(=([]), Diagnostics, ValidDiagnostics),
    % Create the diagnostics notification message
    Message = _{
        jsonrpc: "2.0",
        method: "textDocument/publishDiagnostics",
        params: _{
            uri: Uri,
            diagnostics: ValidDiagnostics
        }
    },!.
    % lsp_server_metta:send_message(Message).  % Send the diagnostics back to the client

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper to count occurrences of specific characters in a line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sub_atom_count(Line, Char, Count) :-
    atom_chars(Line, Chars),
    include(=(Char), Chars, CharList),
    length(CharList, Count).



