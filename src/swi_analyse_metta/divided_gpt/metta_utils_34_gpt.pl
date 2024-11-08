/* 
  This is the main entry predicate for processing different types of input. 
  It first checks if the input is an attributed variable, and then dispatches the appropriate handlers based on the input type.
*/

%% wqs0(+X)
% Entry point for various types of input
% 
% @param X The input to be processed, can be of various types like variables, lists, compound terms, etc.
% @example
%   ?- wqs0(attvar(X)).
%   Processed attributed variable.
wqs0(X):- 
    % If X is an attributed variable, process it
    attvar(X), 
    !, 
    wqs(attvar(X)).

% Handle the special case for 'nl_now', triggering an immediate newline
wqs0(nl_now):- 
    !, 
    nl_now.

% Handle empty strings, skipping them
wqs0(X):- 
    X=='', 
    !.

% Handle empty lists, skipping them
wqs0(X):- 
    X==[], 
    !.

% If the input is a grid structure, print the grid
wqs0(X):- 
    is_grid(X), 
    !, 
    print_grid(X).

% If the input is a callable term, evaluate the call
wqs0(G):- 
    compound(G), 
    G = call(C), 
    callable(C), 
    !, 
    call(C).

% If the input is a single element list, process the element recursively
wqs0([T]):- 
    !, 
    wqs(T).

% If the head of the list is a string, write the string and process the tail
wqs0([H|T]):- 
    string(H), 
    !, 
    write(H), 
    write_nbsp, 
    wqs(T).

% Skip compound terms with 'skip' structure and process the tail
wqs0([H|T]):- 
    compound(H), 
    skip(_) = H, 
    !, 
    wqs(T).

% Process the head of the list, check if a newline is needed, and process the tail
wqs0([H|T]):- 
    wqs(H), 
    need_nl(H, T), 
    wqs(T), 
    !.

% Handle objects, attempt to "tersify" (simplify) them, and process recursively
wqs0(X):- 
    is_object(X), 
    tersify1(X, Q), 
    X \== Q, 
    !, 
    wqs(Q).

% If the input is an object, show its shape
wqs0(X):- 
    is_object(X), 
    show_shape(X), 
    !.

% Handle strings that contain the character '~', format and write them with color
wqs0(X):- 
    string(X), 
    atom_contains(X, '~'), 
    catch((sformat(S, X, []), color_write(S)), _, fail), 
    !.

% Handle simple strings, write them with color
wqs0(X):- 
    string(X), 
    !, 
    color_write(X).

% Dead code: previously handled writing strings with special cases. 
% It's skipped now as it's redundant.
% wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
% wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
% wqs([H|T]):- compound(H),!, writeq(H), wqs(T).

% Handle the case where input is a callable term
wqs0(call(C)):- 
    !, 
    call(C).

% Handle non-compound terms, writing them with a non-breaking space
wqs0(X):- 
    \+ compound(X), 
    !, 
    write_nbsp, 
    write(X).

% Delegate compound terms to wqs1
wqs0(C):- 
    compound(C), 
    wqs1(C), 
    !.

% If nothing else works, delegate to wqs2
wqs0(C):- 
    wqs2(C).

% Dead code: previously handled ANSI terms with special conditions.
% It's skipped now due to performance reasons.
% wqs(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

% Delegate ANSI term processing to wqs2
wqs2(S):- 
    term_contains_ansi(S), 
    !, 
    write_nbsp, 
    write_keeping_ansi_mb(S).

% Dead code: previously handled HTML writing.
% Skipped as HTML generation is no longer required.
% wqs2(P):- wants_html,!,pp(P).

% File directive: Declare a thread-local variable for wqs_fb
:- thread_local(t_l:wqs_fb/1).

% If a thread-local wqs_fb handler is set, call it
wqs2(X):- 
    t_l:wqs_fb(P1), 
    call(P1, X), 
    !.

% Dead code: previously wrapped wqs2 handling with writeq.
% It's skipped now as the new handlers perform better.
% wqs2(X):- with_wqs_fb(writeq,X).

% Write the output using the fallback handler if no specialized handler is found
wqs2(X):- 
    with_wqs_fb(writeq, print(X)), 
    !.

% Dead code: alternative write term strategy. Skipped for simplicity.
% wqs2(X):- with_wqs_fb(writeq,((write_nbsp,write_term(X,[quoted(true)])))).

% Helper to set a thread-local wqs_fb handler for a goal
with_wqs_fb(FB, Goal):- 
    locally(t_l:wqs_fb(FB), Goal).

% Convert a term to a string for output
as_arg_str(C, S):- 
    wots_vs(S, print(C)).

% Check if a string is a valid ANSI string
arg_string(S):- 
    string(S), 
    !.
arg_string(S):- 
    term_contains_ansi(S), 
    !.

% Delegate non-compound terms to wqs0
wqs1(C):- 
    \+ compound(C), 
    !, 
    wqs0(C).

% Write ANSI terms
wqs1(S):- 
    term_is_ansi(S), 
    !, 
    write_keeping_ansi_mb(S).

% Handle formatted output with color
wqs1(format(C, N)):- 
    catch((sformat(S, C, N), color_write(S)), _, fail), 
    !.

% Handle formatted writef calls
wqs1(writef(C, N)):- 
    !, 
    writef(C, N).

% Quoted term handling, color write the output
wqs1(q(C)):- 
    \+ arg_string(C), 
    wots_hs(S, writeq(C)), 
    color_write(S), 
    !.

% Print bold term with color
wqs1(g(C)):- 
    \+ arg_string(C), 
    wots_vs(S, bold_print(wqs1(C))), 
    print(g(S)), 
    !.

% Handle print_ss terms
wqs1(print_ss(C)):- 
    \+ arg_string(C), 
    wots_vs(S, print_ss(C)), 
    wqs1(print_ss(S)), 
    !.

% Handle bold printing for a term
wqs1(b(C)):- 
    \+ arg_string(C), 
    wots_vs(S, bold_print(wqs1(C))), 
    color_write(S).

% Handle ANSI term writing
wqs1(T):- 
    \+ is_list(T), 
    term_contains_ansi(T), 
    !, 
    write_keeping_ansi_mb(T).

% Print normalized grid representation
wqs1(grid_rep(norm, C)):- 
    writeq(grid_rep(norm, C)), 
    !.

% Special case for handling grid terms
wqs1(grid(C)):- 
    writeq(grid(C)), 
    !.

% Output right-hand side of a rule
wqs1(rhs(RHS)):- 
    nl_now, 
    wqnl(rhs(RHS)), 
    nl_now.

% Dead code: specialized grid operations, now skipped.
% wqs1(grid_ops(norm,C)):- writeq(norm(C)),!.

% Pretty print terms
wqs1(pp(P)):- 
    wots_vs(S, pp_no_nl(P)), 
    write((S)).

% Pretty print terms with no newline
wqs1(ppt(P)):- 
    wots_vs(S, ppt_no_nl(P)), 
    write((S)).

% Handle wqs terms
wqs1(wqs(P)):- 
    wots_vs(S, wqs(P)), 
    write((S)).

% Handle color printing for wqs terms
wqs1(wqs(C, P)):- 
    wots_vs(S, wqs(P)), 
    color_print(C, S).

% Print term values
wqs1(vals(C)):- 
    writeq(vals(C)), 
    !.

% Dead code: handled colored values, no longer needed.
% wqs1(colors_cc(C)):- \+ arg_string(C), as_arg_str(C,S),wqs(colorsz(S)).

% Bold print with ANSI handling
wqs1(io(C)):- 
    \+ arg_string(C), 
    wots_vs(S, bold_print(wqs(C))), 
    write(io(S)).

% Underline the printed term
wqs1(uc(C, W)):- 
    !, 
    write_nbsp, 
    color_print(C, call(underline_print(format("\t~@", [wqs(W)])))).

% Color-print terms with specified color
wqs1(cc(C, N)):- 
    is_color(C), 
    !, 
    color_print(C, call(writeq(cc(C, N)))).

% Write navigation command
wqs1(write_nav_cmd(C, N)):- 
    !, 
    write_nav_cmd(C, N).

% Handle colored terms followed by normal processing
wqs1(-(C, N)):- 
    is_color(C), 
    !, 
    color_print(C, call(writeq(C))), 
    write('-'), 
    wqs(N).