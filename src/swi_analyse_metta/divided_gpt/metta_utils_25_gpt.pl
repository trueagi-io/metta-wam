/**
 * into_title_str/2
 * Converts a compound term into a string.
 * This predicate handles compound terms by breaking them into their name and arguments,
 * processing each argument, and then reassembling them into a string.
 * @param Term The term to convert.
 * @param Str The resulting string.
 * @example
 * ?- into_title_str(foo(bar, baz), Str).
 * Str = "foo(bar, baz)".
 */
into_title_str(Term, Str) :- 
    compound(Term),                      % Check if the term is a compound.
    compound_name_arguments(Term, Name, Args),  % Decompose the term into its name and arguments.
    /* previously: include(not_p1(plain_var),Args,Nonvars) - Skipped because 'include' was filtering plain variables */
    Args = Nonvars,                       % For now, just assign Args directly to Nonvars.
    my_maplist(tersify, Nonvars, ArgsT),  % Apply tersify to each non-variable argument.
    into_title_str([Name, "(", ArgsT, ")"], Str), % Reconstruct the string from the name and arguments.
    !.                                    % Cut to prevent backtracking.

% If the term is not a compound, attempt to format it into a string.
into_title_str(Term, Str) :- 
    catch(sformat(Str, '~p', [Term]), _, term_string(Term, Str)).

%---------------------------------------------------------------------------

/**
 * has_short_id/3
 * Associates an entity with its corresponding UUID or ID.
 * Handles multiple cases for identifying different types of entities like test IDs, objects, and grids.
 * @param Entity The entity being checked.
 * @param Type The type of the entity (testid, object, grid).
 * @param UUID The corresponding UUID or ID for the entity.
 * @example
 * ?- has_short_id(test1, testid, UUID).
 * UUID = 'some-uuid'.
 */
has_short_id(TestID, testid, UUID) :- 
    is_valid_testname(TestID),           % Check if the TestID is a valid test name.
    test_id_atom(TestID, UUID).          % Convert the TestID to its UUID.

has_short_id(Obj, object, OID) :- 
    is_object(Obj),                      % Check if Obj is a valid object.
    obj_to_oid(Obj, OID).                % Convert Obj to its object ID (OID).

has_short_id(Grid, grid, GID) :- 
    is_grid(Grid),                       % Check if Grid is a valid grid.
    grid_to_gid(Grid, GID).              % Convert Grid to its grid ID (GID).

%---------------------------------------------------------------------------

/**
 * is_valid_linkid/3
 * Validates an ID by checking if it belongs to a known entity type.
 * Handles validation for multiple entity types like test IDs, objects, and grids.
 * @param ID The ID being validated.
 * @param Type The type of the entity (testid, object, grid, group).
 * @param Entity The entity corresponding to the ID.
 * @example
 * ?- is_valid_linkid('test-id', testid, TestID).
 * TestID = 'test-id'.
 */
is_valid_linkid(ID, testid, TestID) :- 
    atom_id(ID, TestID),                 % Convert the atom ID to TestID.
    is_valid_testname(TestID),           % Ensure the TestID is valid.
    !.                                   % Cut to prevent backtracking.

is_valid_linkid(ID, object, Obj) :- 
    known_object(ID, Obj),               % Ensure the ID corresponds to a known object.
    !.

is_valid_linkid(ID, grid, Grid) :- 
    known_grid(ID, Grid),                % Ensure the ID corresponds to a known grid.
    !.

% This clause seems to handle "group" type IDs, related to tests. 
% It is specialized and depends on the current test context.
is_valid_linkid(ID, group, Grp) :- 
    get_current_test(TestID),            % Get the current test ID.
    is_why_grouped_g(TestID, _Count, ID, Grp).  % Check why this ID is grouped.

%---------------------------------------------------------------------------

/**
 * wqs_c/1
 * A versatile printing predicate with special handling for different term types.
 * Handles various types including strings, variables, lists, and custom compound terms.
 * @param S The term to be printed.
 */
wqs_c(S) :- 
    term_is_ansi(S),                     % Check if the term uses ANSI formatting.
    !, write_keeping_ansi_mb(S).         % Write with ANSI formatting.

wqs_c(S) :- 
    (string(S); is_codelist(S); is_charlist(S)),  % Check if S is a string, code list, or character list.
    catch(format('~s', [S]), _, writeq(S)).      % Print the string or quoted term.

wqs_c(S) :- 
    empty_wqs_c(S),                      % Handle an empty term.
    !.

wqs_c(S) :- 
    var(S),                              % Check if S is a variable.
    !, write(var(S)).                    % Print the variable representation.

wqs_c(S) :- 
    atom(S),                             % Check if S is an atom.
    into_title_str(S, TS),               % Convert the atom to a title string.
    write(TS),                           % Print the title string.
    !.

% Previously: wqs_c(S):- atom(S),write(S),!.
% Skipped because `into_title_str/2` already handles printing atoms.

wqs_c(S) :- 
    \+ compound(S),                      % If S is not a compound, print it as a term.
    !, notrace(catch(format('~p', [S]), _, write(S))).

wqs_c(title(S)) :- 
    !, wqs_c(S).                         % If S is marked as a title, print it recursively.

wqs_c(H+T) :- 
    !, wqs_c(H),                         % Print the head of the list.
    write_nbsp,                          % Write a non-breaking space.
    wqs_c(T).                            % Print the tail recursively.

wqs_c(S) :- 
    is_grid(S),                          % If S is a grid, print the grid.
    print_grid(S),
    !.

wqs_c(S) :- 
    is_vm(S),                            % If S is a virtual machine, print it.
    pp(S),
    !.

wqs_c(L) :- 
    is_list(L),                          % If S is a list, filter out empty elements.
    include(non_empty_wqs_c, L, LL),     % Keep only non-empty elements.
    !, wqs_c_l(LL).                      % Print the list recursively.

wqs_c([H|T]) :- 
    pp([H|T]),                           % Print the head and tail of the list.
    !.

wqs_c(H) :- 
    callable_arity(H, 0),                % Check if H is a goal with 0 arity.
    is_writer_goal(H),                   % Check if H is a writer goal.
    catch(call_e_dmsg(H), _, fail),      % Try calling the goal and printing its message.
    !.

% Previously: wqs_c(H):- callable_arity(H,0),call(H),!.
% Skipped because specialized writer goals handle this case better.

wqs_c(H) :- 
    locally(t_l:wqs_fb(pp_no_nl), wqs(H)), % Locally apply the wqs_fb flag and call wqs/1.
    !.

%---------------------------------------------------------------------------

% Helper predicates for list printing (used by wqs_c/1)

wqs_c_l([]) :- !.                        % Base case for an empty list.

wqs_c_l([H]) :- 
    wqs_c(H),                            % Print the single element in the list.
    !.

wqs_c_l([H|T]) :- 
    wqs_c(H),                            % Print the head of the list.
    write_nbsp,                          % Write a non-breaking space.
    wqs_c_l(T),                          % Print the rest of the list.
    !.

%---------------------------------------------------------------------------

/**
 * ppt/1
 * Pretty prints a term.
 * It handles various conditions like VM maps, ANSI formatting, and custom HTML printing.
 * @param G The term to be printed.
 */
ppt(_):- is_print_collapsed,!.          % If print is collapsed, do nothing.

ppt(G) :- 
    stack_check_or_call(4000, writeq(G)), % Perform a stack check or print the term.
    !.

ppt(G) :- 
    is_vm_map(G),                       % If G is a VM map, print it using 'write_map'.
    !, write_map(G, 'ppt').

ppt(S) :- 
    term_is_ansi(S),                    % Handle ANSI-formatted terms.
    !, write_keeping_ansi_mb(S).

% Previously: ppt(P):- compound(P),wqs1(P),!.
% Skipped because it's redundant with other compound printing logic.

ppt(P) :- 
    \+ ansi_main, wants_html,           % If HTML is desired and ANSI is off.
    !, ptcol_html(P), write_br.

ppt(P) :- 
    \+ \+ ((tersify(P, Q), !, pp(Q))),  % Try to tersify and pretty print.
    !.

ppt(Color, P) :- 
    \+ ansi_main, wants_html,           % If HTML is desired with colors.
    !, with_color_span(Color, ptcol_html(P)), write_br.

ppt(Color, P) :- 
    \+ \+ ((tersify(P, Q), !, pp(Color, Q))), % Try to tersify and print with color.
    !.

%---------------------------------------------------------------------------

% Other utility predicates for printing

write_br :- 
    ansi_main, 
    !, nl.                              % Write a new line if ANSI is enabled.

write_br :- 
    write('<br>').                      % Write an HTML line break.

ptc(Color, Call) :- 
    pp(Color, call(Call)).              % Print a call with color.

% Meta-predicates for pretty printing

:- meta_predicate(ppnl(+)).
ppnl(Term) :- 
    is_list(Term), 
    !, g_out(wqs(Term)).                % Print a list using wqs/1.

ppnl(Term) :- 
    nl_if_needed, format('~q', [Term]), % Print the term with formatting.
    nl_if_needed_ansi.

:- meta_predicate(pp(+)).
pp(Color, P) :- 
    \+ ansi_main, wants_html,           % Handle pretty printing with color and HTML.
    !, with_color_span(Color, pp(P)), write_br.

pp(Color, P) :- 
    ignore((quietlyd((wots_hs(S, pp(P)), !, color_print(Color, S))))). % Print quietly with color.

pp(_):- 
    is_print_collapsed, !.              % If print is collapsed, skip.

pp(_Term):- 
    nl_if_needed, fail.                 % Fail and print a new line if necessary.

pp(Term) :- 
    \+ ansi_main, wants_html,           % Handle pretty printing with HTML.
    !, wots_vs(SS, ptcol_html_scrollable(Term)), write(SS), write_br.