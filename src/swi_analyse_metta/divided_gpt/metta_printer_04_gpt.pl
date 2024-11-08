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