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

help_at_position(_Path, _Line1, _Char0, "blah blah blah") :- !. % clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)), !.
% help_at_position(Path, Line1, Char0, S) :-
%     clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
%     predicate_help(Path, Clause, S0),
%     format_help(S0, S).
