/* Set the encoding for this file to ISO Latin-1 (often used for Western European languages) */
:- encoding(iso_latin_1).

/* Ensure that all output is immediately flushed to the output stream */
:- flush_output.

/* Set the environment variable 'RUST_BACKTRACE' to 'full' for debugging (in case this system interacts with Rust code) */
:- setenv('RUST_BACKTRACE', full).

% ===============================
%       PRINTERS
% ===============================

/**
 * ppc/2
 * Pretty-print original terms and convert them to 'MeTTa' (if applicable),
 * printing both the original and converted forms.
 *
 * @param Msg - A message or context label to describe the term
 * @param Term - The term to be printed
 * @examples
 * ?- ppc('Example', X = 5).
 */
ppc(Msg, Term) :-
    % Call ppc1 to print the original message and term
    ppc1(Msg, Term),
    % Convert the term to MeTTa format and bind to 'MeTTa'
    p2m(Term, MeTTa),
    % If the converted term 'MeTTa' is different from the original, print the converted version
    !, (MeTTa \== Term -> ppc1(p2m(Msg), MeTTa) ; true).

/**
 * ppc1/2
 * Pretty-print a term with a given message.
 *
 * @param Msg - A message or context label to describe the term
 * @param Term - The term to be printed
 * @examples
 * ?- ppc1('Original', X = 5).
 */
ppc1(Msg, Term) :-
    % Double negation (\+ \+) ensures non-backtracking execution
    % Call ppct/2 to handle printing and formatting of various term types
    \+ \+ (ppct(Msg, Term)),
    !.

/**
 * ppc1/2 (overloaded)
 * Alternative case for pretty-printing with more details like tree structures.
 *
 * @param Msg - A message or context label to describe the term
 * @param Term - The term to be printed
 */
ppc1(Msg, Term) :-
    % Double negation again to prevent backtracking
    \+ \+ (
        % Try to guess the format of the term for pretty printing
        ignore(guess_pretty(Term)),
        writeln('---------------------'),
        % Print message label with a prefix 'p'
        write(p(Msg)), write(':'), nl,
        % Use portray_clause/1 to pretty-print the Prolog term
        portray_clause(Term),
        writeln('---------------------'),
        % Attempt to print the term as a tree structure
        \+ \+ (print_tree(?-show_cvts(Term))), nl,
        writeln('---------------------'),
        % Print source form of the term, prefixed with 's'
        write(s(Msg)), write(':'), nl,
        write_src(Term), nl).

/**
 * ppct/2
 * Handles printing of different term types (lists, clauses, equalities).
 *
 * @param Msg - A message or context label to describe the term
 * @param Term - The term to be printed (list, clause, equality, or other)
 */
ppct(Msg, Term) :-
    % Check if the term is a list and pretty-print if so
    is_list(Term), !,
    writeln('---------------------'),
    % Number variables in the list (starting at 666)
    numbervars(Term, 666, _, [attvar(bind)]),
    % Print the message and the term's source code
    write(Msg), write(':'), nl,
    write_src(Term), nl.

ppct(Msg, Term) :-
    % Handle if the term is a Prolog clause (Head :- Body)
    Term = (_ :- _), !,
    writeln('---------------------'),
    write(Msg), write(':'), nl,
    portray_clause(Term), nl.

ppct(Msg, Term) :-
    % Handle if the term is an equality (A = B)
    Term = (_ = _), !,
    writeln('---------------------'),
    write(Msg), write(':'), nl,
    % Number variables in the equality (starting at 444)
    numbervars(Term, 444, _, [attvar(bind)]),
    write_src(Term), nl.

/* previously: Repeated clause for (_ :- _) case skipped here, likely to avoid redundant printing */

/**
 * pp_metta/1
 * Pretty-print a 'MeTTa' formatted term.
 *
 * @param P - The term to be printed
 * @examples
 * ?- pp_metta(X = 5).
 */
pp_metta(P) :-
    % Apply pretty-numbering to the variables in the term
    pretty_numbervars(P, PP),
    % Pretty-print the 'MeTTa' formatted version with an option to suppress concept printing
    with_option(concepts=false, pp_fb(PP)).

/**
 * string_height/2
 * Calculate the number of lines in a string, treating line breaks (\r\n) as delimiters.
 *
 * @param Pt1 - The input string
 * @param H1 - The resulting height (number of lines)
 */
string_height(Pt1, H1) :-
    % Split the input string by line breaks and whitespace, then count the lines
    split_string(Pt1, "\r\n", "\s\t\n\n", L),
    length(L, H1).

/* Declare just_printed/1 dynamic predicate to track the last printed term */
:- dynamic(just_printed/1).

/**
 * print_pl_source/1
 * Print the source code representation of a Prolog term.
 *
 * @param P - The term to be printed
 * @examples
 * ?- print_pl_source((X = 5)).
 */
print_pl_source(P) :-
    % Call helper predicate to run the source printing process
    run_pl_source(print_pl_source0(P)).

/**
 * run_pl_source/1
 * Helper to run source printing with error handling.
 *
 * @param G - The goal (printing action) to execute
 */
run_pl_source(G) :-
    % Attempt to run the goal and handle any exceptions
    catch(G, E, (fail, write_src_uo(G = E), rtrace(G))).

/**
 * print_pl_source0/1
 * Handles different cases for printing source code representations.
 *
 * @param P - The term to be printed
 */
print_pl_source0(_) :-
    % Skip if system is in 'compatio' mode
    notrace(is_compatio), !.

print_pl_source0(_) :-
    % Skip if system is silently loading
    notrace(silent_loading), !.

print_pl_source0(P) :-
    % Skip if this term has just been printed
    notrace((just_printed(PP), PP =@= P)), !.

print_pl_source0((A :- B)) :- !,
    % Pretty-print Prolog clauses (A :- B)
    portray_clause((A :- B)).

print_pl_source0((:- B)) :- !,
    % Pretty-print Prolog directives (:- B)
    portray_clause((:- B)).

print_pl_source0(P) :-
    % For any other terms, print it as a tree structure
    format('~N'), print_tree(P), format('~N'), !.

/**
 * print_pl_source0/1 (continued)
 * Selects from different formatting options (tree, clause, or other).
 *
 * @param P - The term to be printed
 */
print_pl_source0(P) :-
    % List of actions to try for printing
    Actions = [print_tree, portray_clause, pp_fb1_e],
    % Collect heights of different formats and select the best one to display
    findall(H-Pt,
        (member(Action, Actions),
         must_det_ll((
            run_pl_source(with_output_to(string(Pt), call(Action, P))),
            catch(string_height(Pt, H), _, H = 0)))),
        HeightsAndOutputs),
    sort(HeightsAndOutputs, Lst),
    last(Lst, _-Pt),
    writeln(Pt),
    % Update the last printed term
    retractall(just_printed(_)),
    assert(just_printed(P)),
    !.

/**
 * pp_fb1_a/1
 * Format the term with variable numbering and then pretty-print it.
 *
 * @param P - The term to be formatted and printed
 */
pp_fb1_a(P) :-
    format("~N "),
    \+ \+ (numbervars_w_singles(P), pp_fb1_e(P)),
    format("~N "), flush_output.

/**
 * pp_fb1_e/1
 * Apply various pretty-printing methods to the term.
 *
 * @param P - The term to be printed
 */
pp_fb1_e(P) :-
    pp_fb2(print_tree, P).

pp_fb1_e(P) :-
    pp_fb2(pp_ilp, P).

pp_fb1_e(P) :-
    pp_fb2(pp_as, P).

pp_fb1_e(P) :-
    pp_fb2(portray_clause, P).

pp_fb1_e(P) :-
    pp_fb2(print, P).




pp_fb1_e(P):- 
        pp_fb2(fbdebug1, P).

pp_fb1_e(P):- 
        pp_fb2(fmt0(P)).


pp_fb2(F,P):- 
        atom(F), current_predicate(F/1), call(F, P).


pp_sax(V) :- 
        is_final_write(V), !.

pp_sax(S) :- 
        \+ allow_concepts, !, write_src(S).

pp_sax(S) :- 
        is_englishy(S), !, print_concept("StringValue", S).

pp_sax(S) :- 
        symbol_length(S, 1), symbol_string(S, SS), !, print_concept("StringValue", SS).

pp_sax(S) :- 
        is_an_arg_type(S, T), !, print_concept("TypeNode", T).

pp_sax(S) :- 
        has_type(S, T), !, format('(~wValueNode "~w")', [T, S]).

pp_sax(S) :- 
        sub_atom(S, 0, 4, Aft, FB), flybase_identifier(FB, Type), !,
    (Aft > 0 -> format('(~wValueNode "~w")', [Type, S]); format('(TypeNode "~w")', [Type])).

pp_sax(S) :- 
        print_concept("ConceptNode", S).



print_concept(_CType, V):- ignore(write_src(V)).


write_val(V):- 
        is_final_write(V), !.

write_val(V):- 
        number(V), !, write_src(V).

write_val(V):- 
        compound(V), !, write_src(V).

write_val(V):- 
        write('"'), write(V), write('"').


is_final_write(V):- 
        var(V), !, write_dvar(V), !.

is_final_write('$VAR'(S)):- 
        !, write_dvar(S), !.

is_final_write('#\\'(S)):- 
        !, format("'~w'", [S]).

is_final_write(V):- 
        py_is_enabled, py_is_py(V), !, py_ppp(V), !.

is_final_write([VAR, V|T]):- 
        '$VAR' == VAR, T == [], !, write_dvar(V).

is_final_write('[|]'):- 
        write('Cons'), !.

is_final_write([]):- 
        !, write('()').




write_dvar(S):- 
        S == '_', !, write_dname(S).

write_dvar(S):- 
        S == '__', !, write('$').

write_dvar(S):- 
        var(S), get_var_name(S, N), write_dname(N), !.

write_dvar(S):- 
        var(S), !, format('$~p', [S]).

write_dvar(S):- 
        atom(S), symbol_concat('_', N, S), write_dname(N).

write_dvar(S):- 
        string(S), symbol_concat('_', N, S), write_dname(N).



write_dvar(S):- 
        write_dname(S).


write_dname(S):- 
        write('$'), write(S).


pp_as(V):- 
        \+ \+ pp_sex(V), flush_output.


pp_sex_nc(V):- 
        with_no_quoting_symbols(true, pp_sex(V)), !.


unlooped_fbug(Mesg):- 
        fbug_message_hook(fbug_message_hook, fbug(Mesg)).


into_hyphens(D, U):- 
        atom(D), !, always_dash_functor(D, U).

into_hyphens(D, U):- 
        descend_and_transform(into_hyphens, D, U), !.


unlooped_fbug(W, Mesg):- 
        nb_current(W, true), !, print(Mesg), nl, bt, break.

unlooped_fbug(W, Mesg):- 
        setup_call_cleanup(nb_setval(W, true), once(Mesg), nb_setval(W, false)), nb_setval(W, false).

:- dynamic(py_is_enabled/0). 

py_is_enabled:- 
        predicate_property(py_ppp(_), defined), asserta((py_is_enabled:- !)).


write_src(V):- 
        \+ \+ notrace((guess_metta_vars(V), pp_sex(V))), !.


write_src_woi(Term):- 
        notrace((with_indents(false, write_src(Term)))).


write_src_woi_nl(X):- 
        \+ \+ notrace((guess_metta_vars(X), format('~N'), write_src_woi(X), format('~N'))).


pp_sex(V):- 
        pp_sexi(V), !.


pp_sexi(V):- is_final_write(V),!.
pp_sexi(V):- is_dict(V),!,print(V).
pp_sexi((USER:Body)) :- USER==user,!, pp_sex(Body).
pp_sexi(V):- allow_concepts,!,with_concepts('False',pp_sex(V)),flush_output.
pp_sexi('Empty') :- !.
pp_sexi('') :- !, writeq('').
% Handling more cases for 'pp_sex', when the value is a number, a string, a symbol, or a compound.
%pp_sex('') :- format('(EmptyNode null)',[]).
pp_sexi(V):- number(V),!, writeq(V).
pp_sexi(V):- string(V),!, writeq(V).
pp_sexi(S):- string(S),!, print_concept('StringValue',S).
pp_sexi(V):- symbol(V), should_quote(V),!, symbol_string(V,S), write("'"),write(S),write("'").
% Base case: atoms are printed as-is.
%pp_sexi(S):- symbol(S), always_dash_functor(S,D), D \=@= S, pp_sax(D),!.
pp_sexi(V):- symbol(V),!,write(V).
pp_sexi(V) :- (number(V) ; is_dict(V)), !, print_concept('ValueAtom',V).
%pp_sex((Head:-Body)) :- !, print_metta_clause0(Head,Body).
%pp_sex(''):- !, write('()').

% Continuing with 'pp_sex', 'write_mobj', and related rules,
% handling different cases based on the valueï¿½s type and structure, and performing the appropriate writing action.
% Lists are printed with parentheses.
pp_sexi(V) :- \+ compound(V), !, format('~p',[V]).

%pp_sexi(V):-  is_list(V),!, pp_sex_l(V).
%pp_sex(V) :- (symbol(V),symbol_number(V,N)), !, print_concept('ValueAtom',N).
%pp_sex(V) :- V = '$VAR'(_), !, format('$~p',[V]).
pp_sexi(V) :- no_src_indents,!,pp_sex_c(V).
pp_sexi(V) :- w_proper_indent(2,w_in_p(pp_sex_c(V))).


write_mobj(H,_):- \+ symbol(H),!,fail.
write_mobj('$VAR',[S]):- write_dvar(S).
write_mobj(exec,[V]):- !, write('!'),write_src(V).
write_mobj('$OBJ',[_,S]):- write('['),write_src(S),write(' ]').
write_mobj('{}',[S]):- write('{'),write_src(S),write(' }').
write_mobj('{...}',[S]):- write('{'),write_src(S),write(' }').
write_mobj('[...]',[S]):- write('['),write_src(S),write(' ]').
write_mobj('$STRING',[S]):- !, writeq(S).
write_mobj(F,Args):- fail, mlog_sym(K),!,pp_sex_c([K,F|Args]).
%write_mobj(F,Args):- pp_sex_c([F|Args]).

print_items_list(X):- is_list(X),!,print_list_as_sexpression(X).
print_items_list(X):- write_src(X).

pp_sex_l(V):- pp_sexi_l(V),!.
pp_sexi_l(V) :- is_final_write(V),!.
%pp_sexi_l([F|V]):- integer(F), is_codelist([F|V]),!,format("|~s|",[[F|V]]).
pp_sexi_l([F|V]):- symbol(F), is_list(V),write_mobj(F,V),!.
pp_sexi_l([H|T]):-T ==[],!,write('('), pp_sex_nc(H),write(')').
pp_sexi_l([H,H2]):- write('('), pp_sex_nc(H), write(' '), with_indents(false,print_list_as_sexpression([H2])), write(')'),!.
pp_sexi_l([H|T]):- write('('),
  pp_sex_nc(H), write(' '), print_list_as_sexpression(T), write(')'),!.

pp_sexi_l([H,S]):-H=='[...]', write('['),print_items_list(S),write(' ]').
pp_sexi_l([H,S]):-H=='{...}', write('{'),print_items_list(S),write(' }').

%pp_sex_l(X):- \+ compound(X),!,write_src(X).

% File Directive: This code appears to be part of a pretty-printing tool that formats
% Prolog expressions as s-expressions, similar to how LISP represents its code.

% PLDoc header for pp_sex_l/1 predicate
/**
 * pp_sex_l(+Var)
 * 
 * Pretty prints the s-expression for a given variable. 
 * This predicate matches and processes specific structures of s-expressions.
 * 
 * @param Var The variable or term that needs to be pretty-printed.
 * 
 * @example
 * ?- pp_sex_l(['If',cond,let,[x,y]]).
 */
%pp_sex_l('$VAR'(S))):-


% pp_sexi_l processes an s-expression with head and body
pp_sexi_l([=, H, B]) :-
    % Calls helper predicate pp_sexi_hb to process the head and body.
    pp_sexi_hb(H, B),
    !.

% PLDoc header for pp_sexi_l/1 predicate
/**
 * pp_sexi_l(+Expr)
 * 
 * Pretty prints an s-expression. Handles certain symbols specially and ensures proper indentation.
 * 
 * @param Expr The list representing the s-expression to be printed.
 */
pp_sexi_l([H|T]) :- 
    % Checks if there should be no source indents and if the head is a recognized symbol.
    \+ no_src_indents, 
    symbol(H),
    member(H, ['If', 'cond', 'let', 'let*']),  % Special symbols for conditional or binding expressions.
    !,
    % If true, applies proper indentation and prints the s-expression recursively.
    with_indents(true, w_proper_indent(2, w_in_p(pp_sex([H|T])))).

% Another case of pp_sexi_l where it processes lists of specific lengths
pp_sexi_l([H|T]) :- 
    is_list(T), 
    length(T, Args), 
    Args =< 2, 
    fail,  % Fails intentionally for cases with list length <= 2, previously this was used.
    
    % Code that would be executed on success, uses wots to format output with proper indentation.
    wots(SS, (with_indents(false, (write('('), pp_sex_nc(H), write(' '), print_list_as_sexpression(T), write(')'))))),
    
    % Chooses how to output based on length of the symbol SS.
    ((symbol_length(SS, Len), Len < 20) -> write(SS);
    with_indents(true, w_proper_indent(2, w_in_p(pp_sex_c([H|T]))))), !.

/* previously: This block of code has been commented out to simplify symbol handling logic.
pp_sexi_l([H|T]) :- 
    is_list(T), 
    symbol(H),
    upcase_atom(H, U), 
    downcase_atom(H, U), 
    !,
    with_indents(false, (write('('), pp_sex_nc(H), write(' '), print_list_as_sexpression(T), write(')'))).
*/

% PLDoc header for pp_sexi_hb/2 predicate
/**
 * pp_sexi_hb(+Head, +Body)
 * 
 * Formats and pretty-prints an s-expression where Head is followed by a Body.
 * 
 * @param Head The head of the s-expression, often a symbol or variable.
 * @param Body The body of the s-expression, which may be a list or a single term.
 */
pp_sexi_hb(H, B) :-
    % Writes the beginning of an equality expression.
    write('(= '),
    
    % Prints the head term without indentation.
    with_indents(false, pp_sex(H)), 
    write('  '),
    
    % Checks if the body is a list of lists and maps each element to a helper predicate.
    ((is_list(B), maplist(is_list, B)) 
    -> with_indents(true, maplist(write_src_inl, B))  % Proper indentation for list body elements.
    ; with_indents(true, pp_sex(B))),  % Otherwise, just prints the body as is.
    write(')').  % Closes the expression.

% PLDoc header for write_src_inl/1 predicate
/**
 * write_src_inl(+Body)
 * 
 * Prints an s-expression body with new line and indentation for readability.
 * 
 * @param Body The s-expression to be formatted and printed.
 */
write_src_inl(B) :- 
    % Inserts a newline and indent, then prints the body term.
    nl, 
    write('    '), 
    pp_sex(B).

% PLDoc header for pp_sex_c/1 predicate
/**
 * pp_sex_c(+Term)
 * 
 * Main control predicate for pretty-printing compound s-expressions.
 * Directs the expression to appropriate handlers based on its structure.
 * 
 * @param Term The term or expression to be printed.
 */
pp_sex_c(V) :- 
    % If the term has already been handled, immediately succeed.
    pp_sexi_c(V), 
    !.

% PLDoc header for pp_sexi_c/1 predicate
/**
 * pp_sexi_c(+Term)
 * 
 * Handles final printing of certain compound s-expressions, including user-defined terms and commands.
 * 
 * @param Term The term to process and pretty print.
 */
pp_sexi_c(V) :- 
    % If the term is ready for final writing (i.e., no further recursion), succeed.
    is_final_write(V), 
    !.

% Special case for user module: strips the user context
pp_sexi_c((USER:Body)) :- 
    USER == user,  % Only handles the user context.
    !, 
    % Prints the Body as an s-expression.
    pp_sex(Body).

% Special case for 'exec' commands: marks them with '!'
pp_sexi_c(exec([H|T])) :- 
    is_list(T), 
    !,
    write('!'),  % Writes a '!' for exec commands.
    pp_sex_l([H|T]).

% Similar case for '!' commands: marks them with '!'
pp_sexi_c(!([H|T])) :- 
    is_list(T), 
    !,
    write('!'),  % Writes a '!' for these expressions.
    pp_sex_l([H|T]).

/* previously: This section was handling a more complex recursive structure but is commented out
   as unlooped_fbug predicate was no longer relevant to this version.
% pp_sexi_c([H|T]) :- 
%     is_list(T),
%     !,
%     unlooped_fbug(pp_sexi_c, pp_sex_l([H|T])).
*/

% A general case for handling lists
pp_sexi_c([H|T]) :- 
    is_list(T), 
    !, 
    pp_sex_l([H|T]).

% Handles the equality case
pp_sexi_c(=(H, B)) :- 
    !, 
    % Pretty prints the head and body of an equality expression.
    pp_sexi_hb(H, B), 
    !.

% Fallback for compound terms
pp_sexi_c(V) :- 
    compound_name_list(V, F, Args), 
    write_mobj(F, Args), 
    !.

% Skips certain compound terms to simplify the output
pp_sexi_c(Term) :- 
    compound_name_arity(Term, F, 0), 
    !, 
    pp_sex_c([F]).

% Prints a compound term with its functor and arguments as an s-expression
pp_sexi_c(Term) :- 
    Term =.. [Functor|Args], 
    always_dash_functor(Functor, DFunctor), 
    format('(~w ', [DFunctor]), 
    write_args_as_sexpression(Args), 
    write(')'), 
    !.

% Handles specific concept predicates with a link-like format
pp_sexi_c(Term) :- 
    allow_concepts, 
    Term =.. [Functor|Args], 
    format('(EvaluationLink (PredicateNode "~w") (ListLink ', [Functor]), 
    write_args_as_sexpression(Args), 
    write('))'), 
    !.

% Rewrites and prints a compound term
pp_sexi_c(Term) :- 
    Term =.. [Functor|Args], 
    always_dash_functor(Functor, DFunctor), 
    format('(~w ', [DFunctor]), 
    write_args_as_sexpression(Args), 
    write(')'), 
    !.

% Pretty prints with a tab and calls pp_sex for further formatting
pp_sexi(2, Result) :- 
    write('\t\t'), 
    pp_sex(Result).
/* 
   current_column/1 retrieves the current column position of the output stream.
   This predicate uses two methods: checking line_position and stream_position_data.
*/

% Retrieve the current column position from the current output stream
% @param Column - The column number to be returned
current_column(Column) :- 
    current_output(Stream), % Get the current output stream
    line_position(Stream, Column), % Get the column from the stream using line_position
    !. % Cut to ensure no backtracking occurs

% Fallback to stream_property if line_position is not available
% @param Column - The column number to be returned
current_column(Column) :- 
    stream_property(current_output, position(Position)), % Get the stream position properties
    stream_position_data(column, Position, Column). % Extract the column number

/* 
   min_indent/1 ensures the output is indented by a specified number of spaces.
   It uses current_column to decide whether to add a new line before indenting.
*/

% Ensure that the output has at least Sz spaces of indentation
% @param Sz - The required indentation size
min_indent(Sz) :- 
    current_column(Col), % Get the current column position
    Col > Sz, % Check if current column is already indented enough
    nl, % If not, start a new line
    indent_len(Sz). % Indent to the desired length

% If the current column is less than the required size, pad with spaces
% @param Sz - The required indentation size
min_indent(Sz) :- 
    current_column(Col), % Get the current column position
    Need is Sz - Col, % Calculate how many spaces are needed
    indent_len(Need), % Add the required number of spaces
    !. % Prevent backtracking

% Always ensure indentation by starting a new line if necessary
% @param Sz - The required indentation size
min_indent(Sz) :- 
    nl, % Start a new line
    indent_len(Sz). % Add the required indentation

% Helper predicate to print 'Need' spaces
% @param Need - The number of spaces to print
indent_len(Need) :- 
    forall(between(1, Need, _), write(' ')). % Write a space for each number in the range

/*
   w_proper_indent/2 ensures proper indentation before executing a given goal G.
   It calculates the indentation level based on the flag value and the given N.
*/

% Write with proper indentation based on a flag
% @param N - The base indentation size
% @param G - The goal to execute
w_proper_indent(N, G) :- 
    flag(w_in_p, X, X), % Get the current flag value for indentation level
    XX is (X * 2) + N, % Calculate the indentation size
    setup_call_cleanup(min_indent(XX), G, true). % Ensure proper indentation and execute the goal

/* 
   w_in_p/1 increments the indentation flag for nested structures,
   ensuring proper indentation inside complex expressions.
*/

% Temporarily increment the indentation flag and execute a goal
% @param G - The goal to execute
w_in_p(G) :- 
    setup_call_cleanup(flag(w_in_p, X, X + 1), G, flag(w_in_p, _, X)). % Increment flag, execute G, then reset flag

/*
   always_dash_functor/2 ensures that a functor is replaced with a 'dashed' version
   if it doesn't match the second argument (used for transforming functor names).
*/

% Ensure functors are replaced with dashed versions if necessary
% @param A - The original functor
% @param B - The transformed functor
always_dash_functor(A, B) :- 
    once(dash_functor(A, B)), % Perform the dash transformation once
    A \=@= B, % Ensure the original and transformed functors are not syntactically equal
    !. % Cut to prevent backtracking

% If A and B are already equal, no transformation is needed
% @param A - The original functor
always_dash_functor(A, A).

/*
   dash_functor/2 transforms a functor into a 'dashed' version unless it's a symbol.
   The transformation is skipped if it's already a symbol.
*/

dash_functor(A,C):- \+ symbol(A),!,C=A.
% dash_functor(A,C):- p2m(A,B),A\==B,!,always_dash_functor(B,C).
dash_functor(ASymbolProc,O):- fail, symbol_contains(ASymbolProc,'_'),
    symbol_contains(ASymbolProc,'atom'),
    current_predicate(system:ASymbolProc/_),
    symbolic_list_concat(LS,'atom',ASymbolProc),
    symbolic_list_concat(LS,'symbol',SymbolProc),
    always_dash_functor(SymbolProc,O),!.
dash_functor(ASymbolProc,O):- symbol_concat('$',LS,ASymbolProc),!,
    symbol_concat('%',LS,SymbolProc),
    always_dash_functor(SymbolProc,O).

dash_functor(Functor,DFunctor):- fail,
   symbolic_list_concat(L,'_',Functor), L\=[_],
   symbolic_list_concat(L,'-',DFunctor).



% Print the arguments of a compound term in S-expression format
% @param Args - A list of arguments to print
write_args_as_sexpression([]). % Base case: empty argument list
write_args_as_sexpression([H|T]) :- 
    write(' '), % Print a space between arguments
    pp_sex(H), % Pretty-print the first argument
    write_args_as_sexpression(T). % Recursively print the rest

% Print the elements of a list in S-expression format
% @param List - The list to print
print_list_as_sexpression([]). % Base case: empty list
print_list_as_sexpression([H]) :- 
    pp_sex(H). % Print the only element if it's a singleton list

% Print the elements of a list with spaces between them
% @param List - The list to print
print_list_as_sexpression([H|T]) :- 
    pp_sex(H), % Print the first element
    write(' '), % Print a space between elements
    print_list_as_sexpression(T). % Recursively print the rest

/*
   with_indents/2 modifies the src_indents option for the duration of a goal's execution.
   This allows temporarily changing the indentation settings during specific operations.
*/

% Modify src_indents option for the execution of a goal
% @param TF - The new value for the src_indents option
% @param Goal - The goal to execute with the new setting
with_indents(TF, Goal) :- 
    as_tf(TF, Value), % Convert TF to a true/false value
    with_option(src_indents, Value, Goal). % Temporarily set src_indents and execute Goal

% Check if source indentation is disabled
no_src_indents :- 
    option_else(src_indents, TF, true), % Get the src_indents option value
    !, 
    TF == 'False'. % Check if indentation is disabled

% Check if quoting symbols are disabled
no_quoting_symbols :- 
    option_else(no_quoting_symbols, TF, true), % Get the no_quoting_symbols option value
    !, 
    TF == 'True'. % Check if quoting symbols are disabled

/*
   with_no_quoting_symbols/2 modifies the no_quoting_symbols option for the duration of a goal's execution.
*/

% Modify no_quoting_symbols option for the execution of a goal
% @param TF - The new value for the no_quoting_symbols option
% @param Goal - The goal to execute with the new setting
with_no_quoting_symbols(TF, Goal) :- 
    with_option(no_quoting_symbols, TF, Goal). % Temporarily set no_quoting_symbols and execute Goal

/*
   allow_concepts/0 checks if the use of concepts is allowed by examining the concepts option.
   This predicate is currently disabled due to the 'fail' at the start of the clause.
*/

% Check if the use of concepts is allowed
allow_concepts :- 
    !, 
    fail, % This predicate is currently disabled
    option_else(concepts, TF, 'False'), % Get the value of the concepts option
    \+ TF == 'False'. % Ensure that concepts are not explicitly disabled

% The following commented-out code used to handle concept-based logic but was removed:
% previously: dash_functor(ASymbolProc,O):- fail, symbol_contains(ASymbolProc,'_').
/* Directives to ensure that necessary files are loaded for the Prolog interpreter.
   These files include various modules needed for meta programming, evaluation,
   and utilities, e.g., `metta_interp`, `metta_compiler`, etc. */

/**
 * with_concepts/2
 *
 * Executes a Goal while enabling or disabling a specific concept feature based on the TF (true/false) argument.
 *
 * @param TF Boolean, true enables concepts, false disables concepts.
 * @param Goal The goal to execute with the given concept setting.
 *
 * @example
 * ?- with_concepts(true, my_goal).
 * Executes `my_goal` with concepts enabled.
 */
with_concepts(TF, Goal) :-
    % Set the `concepts` option to either true or false (TF) and then execute the given Goal
    with_option(concepts, TF, Goal).

/**
 * dont_quote/1
 *
 * Determines whether a given atom should not be quoted based on certain conditions (e.g., length, type).
 *
 * @param Atom The atom to check.
 * @example
 * ?- dont_quote('.').
 * true.
 */
dont_quote(Atom) :-
    % Check if the Atom is a single character and if it's a punctuation symbol
    symbol_length(Atom, 1), !,
    char_type(Atom, punct).

dont_quote(Atom) :-
    % Check if Atom is a symbol and if it remains the same in both uppercase and lowercase
    symbol(Atom), upcase_atom(Atom, Atom), downcase_atom(Atom, Atom).

/**
 * should_quote/1
 *
 * Determines whether an atom should be quoted. This is based on conditions such as it not being a symbol or string.
 *
 * @param Atom The atom to check for quoting.
 * @example
 * ?- should_quote('example atom').
 * true.
 */
should_quote(Atom) :-
    % Ensure that the Atom is not a symbol or string, and fail if so
    \+ symbol(Atom), \+ string(Atom), !, fail.

should_quote(Atom) :-
    % If the atom should not be quoted based on the dont_quote/1 predicate, skip quoting
    \+ dont_quote(Atom),
    % Fetch the character list of the atom
    symbol_chars(Atom, Chars),
    % Check if the characters or symbol require quoting based on additional rules
    once(should_quote_chars(Chars); should_quote_symbol_chars(Atom, Chars)).

/**
 * contains_unescaped_quote/1
 *
 * Checks if a list of characters contains an unescaped quote. This helps in determining whether to quote a string.
 *
 * @param Chars A list of characters.
 * @example
 * ?- contains_unescaped_quote(['"', 'a', '"']).
 * true.
 */
contains_unescaped_quote(['"']) :-
    % A single quote at the end of the list should fail
    !, fail.

contains_unescaped_quote(['"' | _]) :-
    % If the first character is a quote, succeed
    !.

contains_unescaped_quote(['\\', '"' | T]) :-
    % If the sequence is an escaped quote, continue checking the rest of the list
    !, contains_unescaped_quote(T).

contains_unescaped_quote([_ | T]) :-
    % Recursively check the rest of the list for unescaped quotes
    contains_unescaped_quote(T).

/**
 * should_quote_chars/1
 *
 * Determines whether a list of characters should be quoted based on various conditions (e.g., spaces, quotes, etc.).
 *
 * @param Chars A list of characters representing the atom.
 * @example
 * ?- should_quote_chars(['e', 'x', 'a', 'm', 'p', 'l', 'e', ' ', 'a', 't', 'o', 'm']).
 * true.
 */
should_quote_chars([]).
should_quote_chars(['"' | Chars]) :-
    % If the first character is a quote, check the rest for unescaped quotes
    !, contains_unescaped_quote(Chars).

should_quote_chars(Chars) :-
    % Ensure quoting if the list contains any problematic characters such as spaces, quotes, or commas
    member('"', Chars);         % Contains a double quote
    member(' ', Chars);         % Contains a space
    member('''', Chars);        % Contains a single quote
    %  member('/', Chars);      % Contains a slash (disabled for now, as per original code)
    member(',', Chars);         % Contains a comma
    (fail, member('|', Chars)). % Contains a pipe (this condition is intentionally failing)

/**
 * should_quote_symbol_chars/2
 *
 * Checks if a symbol (represented by Atom and its first character Digit) should be quoted. This mainly focuses on
 * numeric values starting with a digit and ensuring they're treated appropriately.
 *
 * @param Atom The atom to check.
 * @param Chars A list of characters representing the atom.
 */
should_quote_symbol_chars(Atom, [Digit | _]) :-
    % If the first character is a digit and the Atom doesn't represent a number, fail
    fail, char_type(Digit, digit), \+ symbol_number(Atom, _).

/* Previously:
 * The following line was meant to match symbols with numbers, but was disabled.
 * The condition ensures that symbols with numeric prefixes do not get special treatment.
 * symbol(Atom),  % Ensure that the input is a symbol
 */


% Example usage:
% ?- should_quote('123abc').
% true.
% ?- should_quote('123.456').
% false.


:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_eval).



