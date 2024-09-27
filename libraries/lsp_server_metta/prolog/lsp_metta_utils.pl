:- module(lsp_metta_utils, [
%                       called_at/4,
%                       defined_at/3,
%                       name_callable/2,
%                       relative_ref_location/4,
%                       clause_variable_positions/3,
%                       seek_to_line/2,
%                       linechar_offset/3,
%                       clause_with_arity_in_file_at_position/3,
                        help_at_position/4,
                        split_text_document/2,
                        split_document_get_multiple_sections/7,
                        coalesce_text/2
                        ]).
:- use_module(library(debug), [debug/3]).
:- use_module(lsp_metta_xref).
:- use_module(lsp_metta_changes, [handle_doc_changes/2]).

:- dynamic lsp_metta_changes:doc_text/2.

% /** <module> LSP Utils
%
% Module with a bunch of helper predicates for looking through prolog
% source and stuff.
%
% @author James Cash
% */

linechar_offset(_Stream, line_char(_Line1, _Char0), _Offset, _PreChars) :-
    % needs to use the split-document model
    debug(server,"~w",["lsp_metta_utils::linechar_offset not implemented yet"]).

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
    debug(server,"Clause=~w",[Clause]),
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

annotated_position_inc(p(L,C0),p(L,C1),N) :- C1 is C0+N.

annotated_post_newline(p(L,_),p(L1,0)) :-
    L1 is L+1.

annotated_read_sexpr_list(LC0,LC0,Stream,[]) :- at_end_of_stream(Stream),!.
annotated_read_sexpr_list(LC0,LC2,Stream,[Item|L]) :-
    annotated_read_sexpr(LC0,LC1,Stream,Item),
    %debug(server,"x ~w",[Item]),
    annotated_read_sexpr_list(LC1,LC2,Stream,L).

annotated_read_sexpr(LC0,LC1,I,O):- annotated_cont_sexpr(LC0,LC1,')',I,O).

%! annotated_cont_sexpr(+EndChar:atom, +Stream:stream, -Item) is det.
%
% Reads a single item (S-expression or comment) from the specified stream, handling different formats and encodings.
% Throws an error with stream position if the S-expression cannot be parsed.
% @arg EndChar Character that denotes the end of a symbol.
% @arg Stream Stream from which to read.
% @arg Item The item read from the stream.
annotated_cont_sexpr(LCi,LC1,EndChar, Stream, Item) :-
    annotated_skip_spaces(LCi,LCj,Stream),  % Ignore whitespace before reading the expression.
    annotated_position_inc(LCj,LC0,1),
    get_char(Stream, Char),
    (   Char = '(' -> (annotated_read_list(LC0,LC1,')', Stream, Item))  % If '(', read an S-expression list.
    ;   Char = '[' -> (annotated_read_list(LC0,LC1,']', Stream, It3m), Item = ['[...]',It3m])  % If '[', read an S-expression list.
    ;   Char = '{' -> (annotated_read_list(LC0,LC1,'}', Stream, It3m), Item = ['{...}',It3m])  % If '{', read an S-expression list.
    ;   Char = '"' -> (annotated_read_quoted_string(LC0,LC1,Stream, '"', Item))  % Read a quoted string.
    ;   Char = '!' -> (annotated_read_sexpr(LC0,LC1,Stream, Subr), Item = exec(Subr))  % Read called directive
    ;   Char = '\'' -> (annotated_read_quoted_symbol(LC0,LC1,Stream, '\'', Item))  % Read a quoted symbol.
    ;   Char = '`' -> (annotated_read_quoted_symbol(LC0,LC1,Stream, '`', Item))  % Read a backquoted symbol.
    ;   Char = end_of_file -> (LC1=LC0,Item = end_of_file)  % If EOF, set Item to 'end_of_file'.
    ;   annotated_read_symbolic(LC0, LC1, EndChar, Stream, Char, Item)
    ), !.

%! annotated_read_quoted_string(+Stream:stream, +EndChar:atom, -String:atom) is det.
%
% Reads a quoted string from the stream until the corresponding ending quote is found.
% Handles escape sequences within the string.
% Throws an error with stream position if the quoted string cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar Character that denotes the end of the quoted string.
% @arg String The string read from the stream.
annotated_read_quoted_string(LC0, LC1, Stream, EndChar, String) :-
    annotated_read_until_char(Stream, EndChar, Chars),  % Read characters until the ending quote.
    uft8_count_to_utf16_count(Chars,Total,1),
    string_chars(String,Chars),
    annotated_position_inc(LC0,LC1,Total).

%! annotated_read_quoted_symbol(+Stream:stream, +EndChar:atom, -Symbol:atom) is det.
%
% Reads a quoted symbol from the stream, handling escapes and storing the result as a symbol.
% Throws an error with stream position if the quoted symbol cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar Character that closes the quoted symbol.
% @arg Symbol The symbol read from the stream.
annotated_read_quoted_symbol(LC0, LC1, Stream, EndChar, Symbol) :-
    annotated_read_until_char(Stream, EndChar, Chars),
    ((EndChar == '\'', Chars = [Char])
             -> Symbol='#\\'(Char); atom_chars(Symbol, Chars)),
    uft8_count_to_utf16_count(Chars,Total,1),
   annotated_position_inc(LC0,LC1,Total).

%! annotated_read_until_char(+Stream:stream, +EndChar:atom, -Chars:list) is det.
%
% Reads characters from the stream until the specified end character is encountered.
% This function is used to help read quoted strings and symbols.
% Throws an error with stream position if the end character is not found.
% @arg Stream Stream from which to read.
% @arg EndChar Character that indicates the end of the reading.
% @arg Chars List of characters read until the end character.
annotated_read_until_char(Stream, EndChar, Chars) :-
    get_char(Stream, Char),
    (   Char = end_of_file -> throw_stream_error(Stream, unexpected_end_of_file(annotated_read_until_char(EndChar)))
    ;   Char = EndChar -> Chars = []
    ;   Char = '\\' -> get_char(Stream, NextChar),
                       annotated_read_until_char(Stream, EndChar, RestChars),
                       Chars = [NextChar | RestChars]
    ;   annotated_read_until_char(Stream, EndChar, RestChars),
        Chars = [Char | RestChars]
    ).

%! annotated_read_list(+EndChar:atom, +Stream:stream, -List:list) is det.
%
% Reads a list from the stream until the closing parenthesis is encountered.
% It skips comments while reading the list but asserts them with their positions.
% Throws an error with stream position if the list cannot be parsed correctly.
% @arg Stream Stream from which to read.
% @arg List The list read from the stream.
% @arg EndChar Character that denotes the end of the list.
annotated_read_list(LCi,LC2,EndChar, Stream, List) :-
    annotated_skip_spaces(LCi,LC0,Stream),  % Skip any leading spaces before reading.
    peek_char(Stream, Char), !,
    ( Char = EndChar ->  % Closing parenthesis signals the end of the list.
        annotated_position_inc(LC0,LC2,1),
        get_char(Stream, _),  % Consume the closing parenthesis.
        List = []
    ; Char = end_of_file ->  % Unexpected end of file inside the list.
        LC2=LC0,
        List = [incomplete]
    ; annotated_cont_sexpr(LC0, LC1, EndChar, Stream, Element),  % Read the next S-expression.
        annotated_read_list(LC1, LC2, EndChar, Stream, Rest),  % Continue reading the rest of the list.
        List = [Element | Rest]  % Add the element to the result list.
    ), !.

%! annotated_skip_spaces(+Stream:stream) is det.
%
% Skips whitespace characters in the input stream.
% If a comment is encountered, reads the comment and asserts it.
% @arg Stream Stream from which to skip spaces.
annotated_skip_spaces(LC0,LC1,Stream) :-
    peek_char(Stream, Char),
    (   Char = ';' ->
            (annotated_read_single_line_comment(Stream),
            annotated_post_newline(LC0,LC0a),
            annotated_skip_spaces(LC0a,LC1,Stream))  % If the character is ';', read a single-line comment.
    ;   char_type(Char,end_of_line) ->
            (get_char(Stream, _),
            annotated_post_newline(LC0,LC0a),
            annotated_skip_spaces(LC0a,LC1,Stream))
    ;   (char_type(Char,white);char_type(Char,space);char_type(Char,cntrl)) ->
            (get_char(Stream, Char2),
            uft8_count_to_utf16_count([Char2],Size,0),
            annotated_position_inc(LC0,LC0a,Size),
            annotated_skip_spaces(LC0a,LC1,Stream))  % Consume the space and continue.
    ;   LC1=LC0  % Non-space character found; stop skipping.
    ), !.

%! annotated_skip_spaces_until_eol(+Stream:stream) is det.
%
% Skips whitespace characters in the input stream.
% If a comment is encountered, reads the comment and asserts it.
% @arg Stream Stream from which to skip spaces.
%annotated_skip_spaces_until_eol(LC0,LC1,Stream,EolFound) :-
%    peek_char(Stream, Char),
%     (   Char = ';' ->
%             (annotated_read_single_line_comment(LC0,LC0a,Stream),
%             annotated_post_newline(LCa,LC1),
%             EolFound=true)
%     ;   char_type(Char,end_of_line) ->
%             (get_char(Stream, _),
%             annotated_post_newline(LC0,LC1),
%             EolFound=true)
%     ;   (char_type(Char,white);char_type(Char,space);char_type(Char,cntrl)) ->
%             (get_char(Stream, _),
%             annotated_skip_spaces_until_eol(LC0,LC1,Stream,EolFound))  % Consume the space and continue.
%     ;   Char=end_of_file -> LC1=LC0,EolFound=true
%     ;   (LC1=LC0,EolFound=false)  % Non-space character found; stop skipping.
%     ), !.

% annotated_get_blank_lines(LC0,LCblank,LCLeftover,StartOfIncompleteLinePos,Stream) :-
%     seek(Stream,0,current,CurrentPos),
%     annotated_skip_spaces_until_eol(LC0,LC1,Stream,EolFound),
%     (EolFound
%         -> (annotated_get_blank_lines(LC1,LCblank,LCLeftover,StartOfIncompleteLinePos,Stream))
%         ; (LCblank=LC0,LCLeftover=LC1,StartOfIncompleteLinePos=CurrentPos)).

%! annotated_read_single_line_comment(+Stream:stream) is det.
%
% Reads a single-line comment from the stream and asserts it with the position.
% A comment starts with ';' and continues to the end of the line.
% @arg Stream The input stream from which to read.
annotated_read_single_line_comment(Stream) :-
    read_line_to_string(Stream, _Comment).

uft8_count_to_utf16_count(Chars,Size,Additional) :-
    maplist(char_code,Chars,Codes),
    uft8_count_to_utf16_count_aux(Codes,Sum),
    Size is Sum+Additional.

uft8_count_to_utf16_count_aux([],0).
uft8_count_to_utf16_count_aux([C|T],Sum) :- C<128,!,uft8_count_to_utf16_count_aux(T,Sum0),Sum is Sum0+1.
uft8_count_to_utf16_count_aux([C,_|T],Sum) :- C<224,!,uft8_count_to_utf16_count_aux(T,Sum0),Sum is Sum0+1.
uft8_count_to_utf16_count_aux([C,_,_|T],Sum) :- C<240,!,uft8_count_to_utf16_count_aux(T,Sum0),Sum is Sum0+1.
uft8_count_to_utf16_count_aux([_,_,_,_|T],Sum) :- uft8_count_to_utf16_count_aux(T,Sum0),Sum is Sum0+2.

%! annotated_read_symbolic(+EndChar:atom, +Stream:stream, +FirstChar:atom, -Symbolic:atom) is det.
%
% Reads a symbolic expression starting with a specific character, possibly incorporating more complex syntaxes.
% Throws an error with stream position if the symbolic expression cannot be parsed.
% @arg EndChar Character that indicates the end of the reading unless escaped.
% @arg Stream Stream from which to read.
% @arg FirstChar The first character of the symbolic expression.
% @arg Symbolic The complete symbolic expression read.
annotated_read_symbolic(LC0, LC1, EndChar, Stream, FirstChar, Item) :-
    annotated_read_symbolic_cont(EndChar, Stream, RestChars),
    annotated_classify_and_convert_charseq_([FirstChar| RestChars], Symbolic), !,
    uft8_count_to_utf16_count([FirstChar| RestChars],Total,-1),
    annotated_position_inc(LC0,LC1,Total),
    LC0=p(L,C0a),LC1=p(_,C1),
    C0 is C0a-1,
    Item=a(L,C0,C1,Symbolic).

%! annotated_classify_and_convert_charseq_(+Chars:list, -Symbolic:term) is det.
%
% Helper predicate that attempts to classify the character sequence.
% Handles special cases such as Prolog variables and numbers.
%
% @param Chars    The input list of characters.
% @param Symbolic The resultant Prolog term or symbol.

% Case 1: If the character sequence starts with '$', treat it as a variable.
annotated_classify_and_convert_charseq_(['$'| RestChars], var(Symbolic)) :-
    !,atom_chars(Symbolic, RestChars).  % Convert the rest of the characters into a variable name.
% Case 2: Attempt to interpret the characters as a Prolog term using `read_from_chars/2`.
% This handles more complex syntaxes like numbers, dates, etc.
annotated_classify_and_convert_charseq_(Chars, Symbolic) :-
    notrace(catch(read_from_chars(Chars, Symbolic), _, fail)),  % Safely attempt to parse the characters.
    atomic(Symbolic),!.  % Ensure the result is atomic.
% Case 3: If no other case applies, convert the characters directly into an atom.
annotated_classify_and_convert_charseq_(Chars, Symbolic) :-
    atom_chars(Symbolic, Chars).  % Convert the character sequence into an atom.

%! annotated_read_symbolic_cont(+EndChar:atom, +Stream:stream, -Chars:list) is det.
%
% Continues reading symbolic characters from the stream until a delimiter is encountered.
% If a backslash is followed by a delimiter, the delimiter is added as a regular character.
% @arg EndChar Character that indicates the end of the reading unless escaped.
% @arg Stream Stream from which to read.
% @arg Chars List of characters read, forming part of a symbolic expression.
annotated_read_symbolic_cont(EndChar, Stream, Chars) :-
    peek_char(Stream, NextChar),
    (   annotated_is_delimiter(NextChar) -> Chars = []  % Stop when a delimiter is found.
    ;   EndChar == NextChar -> Chars = []  % Stop when an EndChar is found.
    ; ( get_char(Stream, NextChar),
        (   NextChar = '\\' ->  % If it's a backslash, read the next char.
          ( get_char(Stream, EscapedChar),
            annotated_read_symbolic_cont(EndChar, Stream, RestChars),
            Chars = [EscapedChar | RestChars] ) % Add the escaped char normally.
        ; ( annotated_read_symbolic_cont(EndChar, Stream, RestChars),
            Chars = [NextChar | RestChars] ) % Continue reading the symbolic characters.
        ))
    ), !.

%! annotated_is_delimiter(+Char:atom) is semidet.
%
% Determines if a character is a delimiter for reading symbolic expressions.
% @arg Char Character to check.
annotated_is_delimiter(Char) :-
    char_type(Char, space) ;  % Space is a delimiter.
    arg(_, v('(', ')', end_of_file), Char).  % Other delimiters include parentheses and end of file.

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

%!  split_document_get_section(+N, -M, +SplitText, -Pre, -This, -Post) is det.
%
%   Splits a document into three parts based on a starting line number N.
%
%   This predicate takes a list of document sections, represented as terms `d(L, Body , EndPosition, Meta)`,
%   where L is the number of lines, Body is the content, and Meta is additional metadata.
%   The goal is to split the document into a prefix (Pre), the section containing the Nth line (This),
%   and the remainder (Post).
%
%   @arg N        The line number at which to split the document.
%   @arg M        Unifies with the offset from N at which the splitting occurs.
%   @arg SplitText The input list of document sections to be split.
%   @arg Pre      The prefix sections before the Nth line.
%   @arg This     The section containing the Nth line.
%   @arg Post     The remaining sections after the Nth line.
%
split_document_get_section(N,N,[],[],d(0,"", 0, []),[]).
split_document_get_section(N,N,[d(L,Body,EndPosition,Meta)|SplitText],[],d(L,Body,EndPosition,Meta),SplitText) :- L>N,!.
split_document_get_section(N,M,[d(L,Body,EndPosition,Meta)|SplitText],[d(L,Body,EndPosition,Meta)|Pre],This,Post) :-
    N1 is N-L,
    split_document_get_section(N1,M,SplitText,Pre,This,Post).

split_document_get_section_only(N,N,[],d(0,"", 0, [])).
split_document_get_section_only(N,N,[d(L,Body,EndPosition,Meta)|_],d(L,Body,EndPosition,Meta)) :- L>N,!.
split_document_get_section_only(N,M,[d(L,_,_,_)|SplitText],This) :-
    N1 is N-L,
    split_document_get_section_only(N1,M,SplitText,This).

split_document_get_multiple_sections(N1,_N2,N1,[],  [],[],[]). % empty list
split_document_get_multiple_sections(_N1,N2,0,SplitText,  [],[],SplitText) :- 0>N2,!. % past the end
split_document_get_multiple_sections(N1,N2,M,[d(L,Body,EndPosition,Meta)|SplitText],  [d(L,Body,EndPosition,Meta)|Pre],This,Post) :- L=<N1,!, % in pre
    N1n is N1-L,
    N2n is N2-L,
    split_document_get_multiple_sections(N1n,N2n,M,SplitText,Pre,This,Post).
split_document_get_multiple_sections(N1,N2,N1,[d(L,Body,EndPosition,Meta)|SplitText],  Pre,[d(L,Body,EndPosition,Meta)|This],Post) :- % in list
    N1n is N1-L,
    N2n is N2-L,
    split_document_get_multiple_sections(N1n,N2n,_M1,SplitText,Pre,This,Post).

% Choose the split strategy
% Have only one of these commented out - any split strategy should work as long as lines are not broken up
%split_text_document(FullText,SplitText) :- split_text_single_lines(FullText,SplitText).
% should use the number of lines in the file, but that would need to be calculated
split_text_document(FullText,[d(Big,FullText,Big,[])]) :- current_prolog_flag(max_tagged_integer,Big).

split_text_document_by_clause(FullText,SplitText) :-
    setup_call_cleanup(
        open_string(FullText,Stream),
        split_text_document_by_clause_aux(Stream, SplitText),
        close(Stream)).

split_text_document_by_clause_aux(Stream,[]) :- at_end_of_stream(Stream),!.
%split_text_document_by_clause_aux(Stream,Out) :-

% get an empty line split (if one exists), followed by a clause (if one exists)
split_text_document_by_clause_get_empty_plus_clause(Stream,[]) :- at_end_of_stream(Stream),!.
split_text_document_by_clause_get_empty_plus_clause(Stream,[]) :-
    seek(Stream,0,current,CurrentPos),
    annotated_get_blank_lines(p(0,0),LCblank,LCLeftover,StartOfIncompleteLinePos,Stream).
    annotated_skip_spaces_until_eol(p(0,0),Pout,Stream,EolFound). %% TODO FIXME





%annotated_read_sexpr_list(LC0,LC0,Stream,[]) :- at_end_of_stream(Stream),!.
%annotated_read_sexpr_list(LC0,LC2,Stream,[Item|L]) :-
%    annotated_read_sexpr(LC0,LC1,Stream,Item),
%    %debug(server,"x ~w",[Item]),
%    annotated_read_sexpr_list(LC1,LC2,Stream,L).

create_line_entry(N,S,d(N,S,Size,false)) :- string_length(S,Size).

extract_line_entry(d(_,S,_,_),S).

concat_strings([],"").
concat_strings([S],S) :- !.
concat_strings([H|T], Result) :-
    concat_strings(T, TailResult),
    string_concat(H, "\n", H1),
    string_concat(H1, TailResult, Result).

split_text_single_lines(FullText,SplitText) :-
    split_string(FullText, "\n", "", SplitText0),
    maplist(create_line_entry(1),SplitText0,SplitText).

coalesce_text(SplitText,FullText) :-
    maplist(extract_line_entry,SplitText,Strings),
    concat_strings(Strings,FullText).
