:- module(lsp_metta_utils, [
%                       called_at/4,
%                       defined_at/3,
%                       name_callable/2,
%                       relative_ref_location/4,
%                       clause_variable_positions/3,
%                       seek_to_line/2,
%                       linechar_offset/3,
%                       clause_with_arity_in_file_at_position/3,
                        help_at_position/4
                        ]).
:- use_module(library(debug), [debug/3]).
:- use_module(lsp_metta_xref).
:- use_module(lsp_metta_changes, [handle_doc_changes/2]).
:- use_module(lsp_metta_parser, [annotated_read_sexpr_list/4]).
:- use_module(lsp_metta_split, [
        split_document_get_section_only/4
]).

:- dynamic lsp_metta_changes:doc_text/2.

% /** <module> LSP Utils
%
% Module with a bunch of helper predicates for looking through prolog
% source and stuff.
%
% @author James Cash
% */

%linechar_offset(_Stream, line_char(_Line1, _Char0), _Offset, _PreChars) :-
%    % needs to use the split-document model
%    debug(server,"~w",["lsp_metta_utils::linechar_offset not implemented yet"]).

% %!  linechar_offset(+Stream:stream, +Position:line_char, -Offset:int) is det.
% %
% %   Converts a line and character position into a byte offset in the given stream.
% %   This predicate seeks to the specified line and character within the stream.
% %
% %   @arg Stream is the input stream being read.
% %   @arg Position is a term of the form line_char(Line, Char), representing the line and character to seek to.
% %   @arg Offset is the resulting byte offset corresponding to the position.
% %
% %   @example Convert line and character position to byte offset:
% %       ?- open('file.pl', read, Stream), linechar_offset(Stream, line_char(5, 10), Offset).
% %       Offset = 65.
% %
% linechar_offset(Stream, line_char(Line1, Char0), Offset, PreChars) :-
%     % Seek to the beginning of the stream (bof = beginning of file).
%     seek(Stream, 0, bof, _),
%     % Seek to the specified line number in the stream.
%     seek_to_line(Stream, Line1),
%     % Seek to the specified character position from the current line position.
%     accumulating_pre_seek(Stream, Char0, [], PreChars),
%     seek(Stream, 0, current, Offset).
%
% %!  seek_to_line(+Stream:stream, +Line:int) is det.
% %
% %   Seeks to the specified line in the stream by skipping lines until the target line is reached.
% %
% %   @arg Stream is the input stream.
% %   @arg Line is the line number to seek to.
% %
% %   @example Seek to line 5 in a file:
% %       ?- open('file.pl', read, Stream), seek_to_line(Stream, 5).
% %
% seek_to_line(Stream, N) :-
%     % If N is greater than 1, we need to skip lines.
%     N > 1, !,
%     % Skip the current line by searching for a newline character.
%     skip(Stream, 0'\n),
%     % Decrement the line counter.
%     NN is N - 1,
%     % Recursively seek to the remaining lines.
%     seek_to_line(Stream, NN).
% % Base case: If N is 1, we have reached the desired line.
% seek_to_line(_, _).

%! help_at_position(+Path:atom, +Line:integer, +Char:integer, -Help:string) is det.
%
%  =Help= is the documentation for the term under the cursor at line
%  =Line=, character =Char= in the file =Path=.
help_at_position(Path, Line, Char0, S) :-
    %debug(server,"help_at_position",[]),
    clause_with_arity_in_file_at_position(Clause, Arity, Path, line_char(Line, Char0)),
    % TODO - add this in when I can import eval_args
    %debug(server,"Clause=~w",[Clause]),
    predicate_help(Path,Clause,Arity,S).

predicate_help(_,'',_,"") :- !.
predicate_help(_,var(Term),_,S) :- !,format(string(S),"Variable: ~w",[Term]).
predicate_help(_,Term,_,"") :- number(Term),!.
predicate_help(_,')',_,"") :- !.
predicate_help(_,']',_,"") :- !.
predicate_help(_,'}',_,"") :- !.
predicate_help(_,Term,Arity,S) :- metta_atom(_KB,['@doc',Term|Help]),!,
    %debug(server,"clause1 ~w",[Help]),
    format_metta_doc(Term,Arity,Help,S).
predicate_help(_,Term,_,S) :- format(string(S),"Unknown: ~w",[Term]).

format_metta_doc(Term,Arity,[['@desc',Description], ['@params', Params], ['@return', Return]],String) :-
    maplist(format_metta_Param,Params,Params_formatted),
    atomic_list_concat(Params_formatted,'\n',Params_formattednl),
    length(Params,LP),
    ((Arity=unknown;Arity=LP) -> Warning="" ; format(string(Warning),"\n Arity warning, found ~w, expected ~w",[Arity,LP])),
    format(string(String),"~w: ~w\n~w\nReturns: ~w~w",[Term,Description,Params_formattednl,Return,Warning]).

format_metta_Param(['@param',P],Pf) :- format(string(Pf),"Param: ~w",[P]).

%format_help(HelpFull, Help) :-
%    split_string(HelpFull, "\n", " ", Lines0),
%    exclude([Line]>>string_concat("Availability: ", _, Line),
%            Lines0, Lines1),
%    exclude([""]>>true, Lines1, Lines2),
%    Lines2 = [HelpShort|_],
%    split_string(HelpFull, "\n", "", HelpLines),
%    selectchk(HelpShort, HelpLines, "", HelpLines0),
%    append([HelpShort], HelpLines0, HelpLines1),
%    atomic_list_concat(HelpLines1, "\n", Help).

%    S=Clause.
%     predicate_help(Path, Clause, S0),
%     format_help(S0, S).

%!  clause_with_arity_in_file_at_position(-Clause, -Arity, +Path, +Position) is det.
%
%   Reads the clause located at the specified position within the given file.
%
%   @arg Clause is the clause found at the specified position.
%   @arg Path is the path to the file being analyzed.
%   @arg Position is the position in the file (typically line/character position).
%
%   @example Example usage:
%       ?- clause_with_arity_in_file_at_position(Clause, 'file.pl', line_char(5, 10)).
%       Clause = (some_prolog_fact :- some_prolog_goal).
%
clause_with_arity_in_file_at_position(Clause, Arity, Path, line_char(Line, Char)) :-
    % Setup a stream to read the file and find the clause at the specified position.
    lsp_metta_changes:doc_text(Path,SplitText),
    split_document_get_section_only(Line,LinesLeft,SplitText,d(_,Text,_EndPosition,_Meta)),
    %string_codes(Text,TextChars),
    %debug(server,"Input ~w",[TextChars]),
    setup_call_cleanup(
        open_string(Text,Stream),
        annotated_read_sexpr_list(p(0,0),_,Stream,ItemList),
        close(Stream)),
    %debug(server,"1 ~w ~w ~w",[ItemList,0,Char]),
    (find_term_in_annotated_stream(0,ItemList,LinesLeft,Char,Clause,Arity) -> true ; Clause='',Arity=0).
    %debug(server,"2 ~w ~w",[Clause,Arity]).

find_term_in_annotated_stream(_,a(Lpos,S,E,Term),Lpos,CPos,Term,-1) :- CPos=<E,!,S=<CPos.
find_term_in_annotated_stream(Depth,[a(Lpos,S,E,Term)|T],Lpos,CPos,Term,Arity) :- CPos=<E,!,S=<CPos,
    (Depth==0 -> Arity=unknown ; length(T,Arity)).
find_term_in_annotated_stream(_,exec(L),Lpos,CPos,Term,Arity) :- find_term_in_annotated_stream(1,L,Lpos,CPos,Term,Arity).
find_term_in_annotated_stream(Depth,[H|T],Lpos,CPos,Term,Arity) :-
    Depth1 is Depth+1,
    (find_term_in_annotated_stream(Depth1,H,Lpos,CPos,Term,Arity) -> true ; find_term_in_annotated_stream(Depth,T,Lpos,CPos,Term,Arity)).
