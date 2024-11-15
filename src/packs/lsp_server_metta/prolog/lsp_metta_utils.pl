not_mod_lsp_metta_utils
:- module(lsp_metta_utils, [
                        clause_with_arity_in_file_at_position/4
                        ]).
:- use_module(library(debug), [debug/3]).
:- use_module(lsp_metta_xref).
:- use_module(lsp_metta_changes, [handle_doc_changes_d4/2]).
:- use_module(lsp_metta_parser, [annotated_read_sexpr_list/4]).
:- use_module(lsp_metta_split, [
        split_document_get_section_only/4
]).

:- include(lsp_metta_include).

:- dynamic lsp_metta_changes:doc_text_d4/2.

% /** <module> LSP Utils
%
% Module with a bunch of helper predicates for looking through prolog
% source and stuff.
%
% @author Roy Ward
% @author James Cash
% */


%!  clause_with_arity_in_file_at_position(-Term, -Arity, +Path, +Position) is det.
%
%   Reads the Term (such as the Symbol) located at the specified position within the given file.
%
%   @arg Term is the symbol found at the specified position.
%   @arg Path is the path to the file being analyzed.
%   @arg Position is the position in the file (typically line/character position).
%
%   @example Example usage:
%       ?- clause_with_arity_in_file_at_position(Term, 'file.pl', line_char(5, 10)).
%       Clause = (some_prolog_fact :- some_prolog_goal).
%
clause_with_arity_in_file_at_position(Clause, Arity, Doc, Loc) :-
   maybe_doc_path(Doc,Path), !,
   clause_with_arity_in_file_at_position(Clause, Arity, Path, Loc).

clause_with_arity_in_file_at_position(Clause, _Arity, Path, Range):- !,
        get_code_at_range(symbol, Path, Range, Clause), !.

clause_with_arity_in_file_at_position(Clause, Arity, Path, line_char(Line1, Char)) :- fail,
    % Setup a stream to read the file and find the clause at the specified position.
    lsp_metta_changes:doc_text_d4(Path,SplitText),
    succl(Line0, Line1),
    split_document_get_section_only(Line0,LinesLeft,SplitText,d(_,Text,_EndPosition,_Meta)),
    %string_codes(Text,TextChars),
    %debug(lsp(low),"Input ~w",[TextChars]),
    setup_call_cleanup(
        open_string(Text,Stream),
        annotated_read_sexpr_list(p(0,0),_,Stream,ItemList),
        close(Stream)),
    %debug(lsp(low),"1 ~w ~w ~w",[ItemList,0,Char]),
    (find_term_in_annotated_stream(0,ItemList,LinesLeft,Char,Clause,Arity) -> true ; Clause='',Arity=0).
    %debug(lsp(low),"2 ~w ~w",[Clause,Arity]).



find_term_in_annotated_stream(_,a(Lpos,S,E,Term),Lpos,CPos,Term,-1) :- CPos=<E,!,S=<CPos.
find_term_in_annotated_stream(Depth,[a(Lpos,S,E,Term)|T],Lpos,CPos,Term,Arity) :- CPos=<E,!,S=<CPos,
    (Depth==0 -> Arity=unknown ; length(T,Arity)).
find_term_in_annotated_stream(_,exec(L),Lpos,CPos,Term,Arity) :- find_term_in_annotated_stream(1,L,Lpos,CPos,Term,Arity).
find_term_in_annotated_stream(Depth,[H|T],Lpos,CPos,Term,Arity) :-
    Depth1 is Depth+1,
    (find_term_in_annotated_stream(Depth1,H,Lpos,CPos,Term,Arity) -> true ; find_term_in_annotated_stream(Depth,T,Lpos,CPos,Term,Arity)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks if the first line_char comes before the second one.
before_line_char(line_char(Line1,_Char1),line_char(Line2,_Char2)):-
    Line1 > Line2, !, fail.  % If the first line is greater than the second, it's not before.
before_line_char(line_char(Line1,Char1),line_char(Line2,Char2)):-
    Line1 =:= Line2, Char1 > Char2, !, fail.  % If on the same line, compare characters.
before_line_char(_, _).  % Otherwise, the first one is before the second.

% Checks if the first range ends before the second range starts.
completely_before_range(range(_,LineChar1),range(LineChar2,_)):-
    before_line_char(LineChar1, LineChar2), !.
% Checks if the first range starts before the second range starts.
before_range(range(LineChar1,_),range(LineChar2,_)):-
    before_line_char(LineChar1, LineChar2), !.


% Checks if the first line_char comes after the second one.
after_line_char(line_char(Line1,_Char1),line_char(Line2,_Char2)):-
    Line1 < Line2, !, fail.  % If the first line is greater than the second, it's not after.
after_line_char(line_char(Line1,Char1),line_char(Line2,Char2)):-
    Line1 =:= Line2, Char1 < Char2, !, fail.  % If on the same line, compare characters.
after_line_char(_, _).  % Otherwise, the first one is after the second.


% Checks if the first range starts after the second range ends.
completely_after_range(range(LineChar1,_),range(_,LineChar2)):-
    after_line_char(LineChar1, LineChar2), !.
% Checks if the first range starts after the second range starts.
after_range(range(LineChar1,_),range(LineChar2,_)):-
    after_line_char(LineChar1, LineChar2), !.


% Enhanced starts_after with tolerance to determine if one position starts after another
starts_after(line_col(Line1, Col1), line_col(Line2, Col2), Tolerance) :-
   Line1 > Line2 ;
   (Col1Adjusted is Col1 + Tolerance,
    ((Line1 =:= Line2 + 1, Tolerance > 2, Col2 > 98, Col1Adjusted < 2) ;
     (Line1 =:= Line2, Col1Adjusted > Col2))).

% Check if a given line_col is within any ranges in the text_info database with a tolerance
text_at_position_blurred(Pos, Text, Tolerance) :-
    text_info(Text, range(Start, End)),
    range_blurred_contains(Start, End, Pos, Tolerance),
    !.

text_info(metta_file_buffer(N, Ord, Kind, Code, Vs, Path, Range), Range):- user:metta_file_buffer(N, Ord, Kind, Code, Vs, Path, Range).


% Check if a range is completely before another range with tolerance
completely_before_range(Range1, Range2, Tolerance) :-
    must_succeed1((Range1 = range(_, End1),
    Range2 = range(Start2, _))),
    starts_after(Start2, End1, Tolerance).

% Check if a range is completely after another range with tolerance
completely_after_range(Range1, Range2, Tolerance) :-
    must_succeed1((Range1 = range(End1, _),
    Range2 = range(_, Start2))),
    starts_after(End1, Start2, Tolerance).

% Helper predicate to determine if a range contains a specific line_col position with a given tolerance
range_blurred_contains(Start, End, Pos, Tolerance) :-
    \+ starts_after(Pos, End, -Tolerance),
    \+ starts_after(Start, Pos, Tolerance).

range_blurred_contains(range(Start, End), range(Pos, _), Tolerance) :-
   range_blurred_contains(Start, End, Pos, Tolerance).

% Find text exactly or nearest based on position
find_text_or_nearest(Pos, Text) :-
    text_at_position_blurred(Pos, Text, 2) ;
    find_nearest_text(Pos, Text).

% Find nearest text based on position
find_nearest_text(Pos, Text) :-
    findall(Difference-Text, (text_info(Text, Range), calculate_difference(Pos, Range, Difference)), Differences),
    sort(Differences, [_-NearestText | _]),
    Text = NearestText.

% calculate_difference(+MousePos, +Range, -Difference)
% Calculates the "distance" between a mouse position and a given range.
calculate_difference(line_col(MouseY, MouseX), range(line_col(SLine, SCol), line_col(ELine, ECol)), Difference) :- !,
    (MouseY < SLine ->               % Mouse is above the range
        Difference is (SLine - MouseY) * 100 + abs(SCol - MouseX)
    ; MouseY > ELine ->              % Mouse is below the range
        Difference is (MouseY - ELine) * 100 + abs(MouseX - ECol)
    ; MouseY = SLine ->              % Mouse is on the same line as the start of the range
        Difference is abs(SCol - MouseX)
    ; MouseY = ELine ->              % Mouse is on the same line as the end of the range
        Difference is abs(MouseX - ECol)
    ; Difference is 0).              % Mouse is within the range
% calculate_difference(+MouseRange, +TargetRange, -Difference)
% Calculates the "distance" between a mouse selection range and a target range.
calculate_difference(
    range(line_col(MouseStartLine, MouseStartCol), line_col(MouseEndLine, MouseEndCol)),
    range(line_col(TargetStartLine, TargetStartCol), line_col(TargetEndLine, TargetEndCol)), Difference) :-
    (MouseEndLine < TargetStartLine ->  % Mouse range is entirely before the target range
        Difference is (TargetStartLine - MouseEndLine) * 100 + abs(TargetStartCol - MouseEndCol)
    ; MouseStartLine > TargetEndLine -> % Mouse range is entirely after the target range
        Difference is (MouseStartLine - TargetEndLine) * 100 + abs(MouseStartCol - TargetEndCol)
    ; MouseEndLine = TargetStartLine -> % Mouse end line aligns with target start line (mouse before target)
        Difference is abs(TargetStartCol - MouseEndCol)
    ; MouseStartLine = TargetEndLine -> % Mouse start line aligns with target end line (mouse after target)
        Difference is abs(MouseStartCol - TargetEndCol)
    ; Difference is 0).                 % Ranges overlap or mouse range is within the target range




get_src_code_at_range(TargetType, Uri, Range, SrcCode):-
   get_code_at_range(TargetType, Uri, Range, Code),
   with_output_to(string(SrcCode),write_src_wi(Code)).



% Extract code at the specified range for different types of targets (symbol, expression, block, exact).
% For `symbol`, it looks for a clause/symbol in the file at the specified position.
get_code_at_range(Type, Uri, Range, Target):- maybe_doc_path(Uri, Path), !, get_code_at_range(Type, Path, Range, Target).

% get_code_at_range(+TargetType, +Uri, +line_char(L, C), -Target)
get_code_at_range(TargetType, Uri, line_char(L, C), Target) :-
    % Retrieve the last recorded range from the user data
    user:last_range(_Method, Range),
    % Correct use of :< to directly access start and end members of the Range
    _{start: Start, end: End} :< Range,
    _{character: SC, line: SL} :< Start,
    _{character: EC, line: EL} :< End,
    % Check if the position (L, C) is within the range (SL, SC) to (EL, EC)
    (
        (L = SL, C >= SC) ;    % On the start line, within start character
        (L = EL, C =< EC) ;    % On the end line, within end character
        (L > SL, L < EL)       % Between start and end lines
    ), !,
    % Call the function to get code based on the target range
    get_code_at_range(TargetType, Uri, Range, Target).

get_code_at_range(TargetType, Uri, line_char(_,_), Target):- fail,
   user:last_range(_Method,Range), !,
   get_code_at_range(TargetType, Uri, Range, Target).

get_code_at_range(TargetType, Uri, Range, Target):- \+ ((is_dict(Range), _{start: _RStart, end: _REnd} :< Range)),
   must_succeed1(into_json_range(Range, LspRange)), !,
   get_code_at_range(TargetType, Uri, LspRange, Target).

get_code_at_range(TargetType, Uri, Range, Target):-
   once(into_json_range(Range, LspRange)), Range\=@=LspRange, !,
   get_code_at_range(TargetType, Uri, LspRange, Target).

get_code_at_range(text, Uri, Range, Target):- !, get_code_at_range(symbol, Uri, Range, Target).


get_code_at_range(symbol, Path, Range, Code):-
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    user:metta_file_buffer(_Lvl, _Ord, _Kind, Code, Vs, Path, BRange),  % Get the buffer contents for the file.
    %sub_var(Target, Code),  % Check that the symbol (Target) appears within the buffer (Code).
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    \+ is_list(Code),!,maybe_name_vars(Vs), !.
    %\+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.

get_code_at_range(symbol_arity, Path, Range, Code):-
    get_code_at_range(symbol, Path, Range, Symbol),
    user:metta_file_buffer(_, _Ord, _Kind, [Head|Rest], _Vs, Path, _BRange),  Head == Symbol,
    Head == Symbol, is_list(Rest), length(Rest,Len),Code = Symbol/Len,!.
get_code_at_range(symbol_arity, Path, Range, Symbol/0):-
    get_code_at_range(symbol, Path, Range, Symbol), !.

get_code_at_range(nonsymbol, Path, Range, Code):-
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    user:metta_file_buffer(0, _Ord, _Kind, Code, Vs, Path, BRange),  % Get the buffer contents for the file.
    %sub_var(Target, Code),  % Check that the symbol (Target) appears within the buffer (Code).
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    is_list(Code),
    %\+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    maybe_name_vars(Vs), !.


get_code_at_range(symbol, Path, Range, Target):- fail, !,
    must_succeed((
        _{start: RStart, end: _REnd} :< Range,  % Extract the start and end from the LSP range.
        _{line: StartLine0, character: StartChar} :< RStart,  % Get line and character from the start position.
        Start = line_char(StartLine0, StartChar),  % Convert to a line_char pair.
        clause_with_arity_in_file_at_position(Target, _Arity, Path, Start)  % Get the clause at the specified position.
    )).

get_code_at_range(toplevel_form, Path, Range, Code) :-
    %get_code_at_range(symbol, Path, Range, Target),  % First, get the symbol at the range.
    %Target \== '',
    %path_doc(Path, Uri),  % Extract the file path from the URI.
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    user:metta_file_buffer(0, _Ord, _Kind, Code, Vs, Path, BRange),  % Get the buffer contents for the file.
    %sub_var(Target, Code),  % Check that the symbol (Target) appears within the buffer (Code).
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    %\+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    maybe_name_vars(Vs), !.
    % brange_to_dict(BRange,CodeRange), get_code_at_range(exact, Uri, BRange, Code).  % Refine the code extraction with exact range.
get_code_at_range(toplevel_form, Uri, Range, Target):- !, get_code_at_range(expression, Uri, Range, Target).

% For `term`, it first resolves the symbol and then looks for the code within the buffer at the range.
get_code_at_range(term, Path, Range, Code) :-
    %path_doc(Path, Uri),  % Extract the file path from the URI.
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    user:metta_file_buffer(_, _Ord, _Kind, Code, Vs, Path, BRange),  % Get the buffer contents for the file.
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    %\+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    maybe_name_vars(Vs), !.



% For `expression`, it first resolves the symbol and then looks for the code within the buffer at the range.
get_code_at_range(expression, Path, Range, Code) :-
    %get_code_at_range(symbol, Path, Range, Target),  % First, get the symbol at the range.
    %Target \== '',
    %path_doc(Path, Uri),  % Extract the file path from the URI.
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    user:metta_file_buffer(N, _Ord, _Kind, Code, Vs, Path, BRange),  % Get the buffer contents for the file.
    %sub_var(Target, Code),  % Check that the symbol (Target) appears within the buffer (Code).
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    %\+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    (N > 0 -> compound(Code) ; true),
    maybe_name_vars(Vs), !.

get_code_at_range(expression, Path, Range, Code) :-
    into_line_char_range(Range, LspLCRange),  % Convert the LSP range into line_char format.
    user:metta_file_buffer(_, _Ord, _Kind, Code, Vs, Path, BRange),  % Get the buffer contents for the file.
    %sub_var(Target, Code),  % Check that the symbol (Target) appears within the buffer (Code).
    \+ completely_before_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    %\+ completely_after_range(BRange, LspLCRange),  % Ensure the buffer range is relevant to the LSP range.
    %(N > 0 -> is_list(Code) ; true),
    maybe_name_vars(Vs), !.

get_code_at_range(expression, Uri, Range, Target):- get_code_at_range(term, Uri, Range, Target), Target \== '' , !.
get_code_at_range(expression, Uri, Range, Code):- get_code_at_range(exact, Uri, Range, Code).
% brange_to_dict(BRange,CodeRange), get_code_at_range(exact, Uri, BRange, Code).  % Refine the code extraction with exact range.

% For `block`, it acts similarly to expression but with larger code blocks.
get_code_at_range(block, Uri, Range, Code):-
    get_code_at_range(toplevel_form, Uri, Range, Code),!.


% Extracts the exact range of code specified by the Range (LSP-style start and end).
get_code_at_range(exact, Path, Range, Code) :- !,
    %path_doc(Path, Uri),  % Extract the file path from the URI.
    source_file_text(Path, FullText),  % Retrieve the full file text.
    split_string(FullText, "\n", "", Lines),  % Split the file into lines.
    _{start: Start, end: End} :< Range,  % Extract start and end positions from the range.
    _{line: StartLine0, character: StartChar} :< Start,  % Get line and character of the start position.
    _{line: EndLine0, character: EndChar} :< End,  % Get line and character of the end position.
    StartLine is StartLine0 + 0,  % Set the start line (Prolog indexing adjustment if needed).
    EndLine is EndLine0 + 1,  % Set the end line (since Prolog indexes from 1).
    extract_code(Lines, StartLine, StartChar, EndLine, EndChar, Code).  % Extract the exact code range.


% Helper to extract code from lines based on the start and end line/character positions.
extract_code(Lines, StartLine, StartChar, EndLine, EndChar, Code) :-
    findall(LineText, (
        between(StartLine, EndLine, LineNum),  % Iterate over the line numbers in the range.
        nth1(LineNum, Lines, Line),  % Get the line at the current number.
        (
            (LineNum =:= StartLine, LineNum =:= EndLine) ->  % Case where the start and end are on the same line.
            (EndCharStartChar is EndChar - StartChar,  % Get the substring within this line.
             sub_atom(Line, StartChar, EndCharStartChar, _, LineText))
        ;
            LineNum =:= StartLine ->  % Case where the current line is the start line.
            sub_atom(Line, StartChar, _, 0, LineText)  % Get the substring starting from StartChar.
        ;
            LineNum =:= EndLine ->  % Case where the current line is the end line.
            sub_atom(Line, 0, EndChar, _, LineText)  % Get the substring ending at EndChar.
        ;
            LineText = Line  % In-between lines are added fully.
        )
    ), CodeLines),
    atomic_list_concat(CodeLines, '\n', Code).  % Combine the extracted lines into a single code string.


