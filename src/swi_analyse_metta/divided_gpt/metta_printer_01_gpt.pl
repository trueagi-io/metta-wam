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