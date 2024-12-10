:- module(lsp_metta_parser, [
    annotated_read_sexpr_list/4,
    annotated_get_blank_lines/3,
    annotated_read_sexpr/4,
    annotated_skip_spaces_until_eol/4
]).

annotated_position_inc(p(L,C0),p(L,C1),N) :- C1 is C0+N.

annotated_post_newline(p(L,_),p(L1,0)) :-
    L1 is L+1.

annotated_read_sexpr_list(LC0,LC0,Stream,[]) :- at_end_of_stream(Stream),!.
annotated_read_sexpr_list(LC0,LC2,Stream,[Item|L]) :-
    annotated_read_sexpr(LC0,LC1,Stream,Item),
    %debug(lsp(low),"x ~w",[Item]),
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
    annotated_read_until_char(LC0, LCa, Stream, EndChar, Chars),  % Read characters until the ending quote.
    string_chars(String,Chars),
    annotated_position_inc(LCa,LC1,1).

%! annotated_read_quoted_symbol(+Stream:stream, +EndChar:atom, -Symbol:atom) is det.
%
% Reads a quoted symbol from the stream, handling escapes and storing the result as a symbol.
% Throws an error with stream position if the quoted symbol cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar Character that closes the quoted symbol.
% @arg Symbol The symbol read from the stream.
annotated_read_quoted_symbol(LC0, LC1, Stream, EndChar, Symbol) :-
    annotated_read_until_char(LC0, LCa, Stream, EndChar, Chars),
    ((EndChar == '\'', Chars = [Char])
             -> Symbol='#\\'(Char); atom_chars(Symbol, Chars)),
    annotated_position_inc(LCa,LC1,1).

%! annotated_read_until_char(+Stream:stream, +EndChar:atom, -Chars:list) is det.
%
% Reads characters from the stream until the specified end character is encountered.
% This function is used to help read quoted strings and symbols.
% Throws an error with stream position if the end character is not found.
% @arg Stream Stream from which to read.
% @arg EndChar Character that indicates the end of the reading.
% @arg Chars List of characters read until the end character.
annotated_read_until_char(LC0, LC1, Stream, EndChar, Chars) :-
    get_char(Stream, Char),
    (   Char = end_of_file -> throw_stream_error(Stream, unexpected_end_of_file(annotated_read_until_char(EndChar)))
    ;   Char = EndChar -> Chars = [],
                        annotated_position_inc(LC0,LC1,1)
    ;   Char = '\n' ->
                        annotated_read_until_char(LC0,LCa,Stream,EndChar,RestChars),
                        Chars = [Char | RestChars],
                        annotated_post_newline(LCa,LC1)
    ;   Char = '\\' -> get_char(Stream, NextChar),
                        % need to advance for end of line even if it is escaped
                        (NextChar = '\n' ->
                            annotated_post_newline(LC0,LCa)
                        ;
                            uft8_count_to_utf16_count_single(NextChar,NextSize),
                            Total is NextSize+1,
                            annotated_position_inc(LC0,LCa,Total)),
                        annotated_read_until_char(LCa, LC1, Stream, EndChar, RestChars),
                        Chars = [NextChar | RestChars]
    ;   annotated_read_until_char(LC0,LCa,Stream, EndChar, RestChars),
        uft8_count_to_utf16_count_single(Char,Size),
        annotated_position_inc(LCa,LC1,Size),
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
    ;   Char = '\n' ->
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
annotated_skip_spaces_until_eol(LC0,LC1,Stream,EolFound) :-
   peek_char(Stream, Char),
    (   Char = ';' ->
            (annotated_read_single_line_comment(Stream),
            annotated_post_newline(LC0,LC1),
            EolFound=true)
    ;   Char = '\n' ->
            (get_char(Stream, _),
            annotated_post_newline(LC0,LC1),
            EolFound=true)
    ;   (char_type(Char,white);char_type(Char,space);char_type(Char,cntrl)) ->
            (get_char(Stream, _),
            annotated_skip_spaces_until_eol(LC0,LC1,Stream,EolFound))  % Consume the space and continue.
    ;   Char=end_of_file -> LC1=LC0,EolFound=true
    ;   (LC1=LC0,EolFound=false)  % Non-space character found; stop skipping.
    ), !.

annotated_get_blank_lines(LC0,LC0,Stream) :- at_end_of_stream(Stream),!.
annotated_get_blank_lines(LC0,LCStartOfBlank,Stream) :-
    seek(Stream,0,current,StartOfLinePos),
    annotated_skip_spaces_until_eol(LC0,LC1,Stream,EolFound),
    (EolFound
    -> (annotated_get_blank_lines(LC1,LCStartOfBlank,Stream))
    ; (LCStartOfBlank=LC0,
        seek(Stream,StartOfLinePos,bof,_))). % found something, so go back to the start of the line

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

uft8_count_to_utf16_count_single(Char,Count) :-
    char_code(Char,C),
    (C<128 -> Count=1; % normal ASCII characters
    C<192 -> Count=0; % x80-xBF do not count towards the total
    C<240 -> Count=1; % xC0-xDF,xE0-xEF are the 2-3 byte UTF-8 characters, which only count as 2 UTF-16
    Count=2). % xF0-xF7 are 4 byte UTF-8 which count as 2 UTF-16

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
annotated_classify_and_convert_charseq_(['$'| RestChars], '$VAR'(Symbolic)) :-
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


