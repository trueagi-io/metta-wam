/* previously: pretty_grid(O) was commented out because its functionality was replaced or optimized elsewhere. 
   The use of the `catch/3` in this code suggests that it was handling errors when displaying grids, 
   which might have been deemed unnecessary or handled in a more centralized way.
*/

/**
 * pp_hook_g1(+O)
 * 
 * A hook predicate that prints various types of terms (e.g., grids, objects, colors).
 * This is used in contexts where specific term types need to be visualized in particular ways.
 * 
 * @param O The term to be printed.
 * 
 * @example
 * ?- pp_hook_g1(rhs(x)).
 *    Prints "rhs(x)" formatted in bold.
 */
pp_hook_g1(O) :-  
    plain_var(O),  % Check if O is an uninstantiated variable.
    !,             % If true, cut to prevent backtracking and fail since we don't print plain variables.
    fail.

pp_hook_g1(O) :-  
    attvar(O),        % Check if O is an attributed variable.
    !,                % If true, handle the attributed variable.
    is_colorish(O),   % Check if O has color properties.
    data_type(O, DT), % Retrieve the data type of O.
    writeq('...'(DT)), % Write the data type in a quoted form.
    !.               % Cut to prevent further rules from being executed.

pp_hook_g1(S) :-  
    term_is_ansi(S),  % Check if the term S is an ANSI-compatible term (for formatting purposes).
    !,                % If true, handle it accordingly.
    write_nbsp,       % Write a non-breaking space.
    write_keeping_ansi_mb(S).  % Write S while preserving ANSI formatting.

% previously: term_contains_ansi was commented out because the ANSI formatting for terms 
% containing special sequences may have been replaced with other logic.
% pp_hook_g1(S) :- term_contains_ansi(S), !, fail, write_nbsp, write_keeping_ansi_mb(S).

pp_hook_g1(rhs(O)) :-  
    write_nbsp,           % Write a non-breaking space.
    nl,                   % Print a newline.
    bold_print(print(r_h_s(O))),  % Print the right-hand side (r_h_s) of the term O in bold.
    !.

pp_hook_g1(iz(O)) :-  
    compound(O),          % Check if O is a compound term.
    O = info(_),          % Further check if O has the form info(_).
    underline_print(print(izz(O))),  % Print O with underlined text.
    !.

pp_hook_g1(O) :-  
    is_grid(O),           % Check if O is a grid (specific term type).
    /* previously: This line involving sub_term was commented out because it was deemed redundant. 
       The sub_term check for '$VAR'(_) was unnecessary in the current context.
    */
    pretty_grid(O).       % Call the pretty_grid predicate to print the grid.

pp_hook_g1(O) :-  
    is_object(O),            % Check if O is an object (a complex term).
    into_solid_grid(O, G),   % Convert O into a solid grid form.
    wots(SS, pretty_grid(G)), % Fetch the grid representation and process it.
    write(og(SS)),           % Write the grid (og stands for "output grid").
    !.

pp_hook_g1(shape_rep(grav,O)) :-  
    is_points_list(O),       % Check if O is a list of points.
    as_grid_string(O, S),    % Convert the list of points into a grid string.
    wotsq(O, Q),             % Perform some query/processing on O (wotsq likely prints or returns a result).
    print(shape_rep(grav, S, Q)), % Print the shape representation with gravity.
    !.

pp_hook_g1(vals(O)) :-  
    !, 
    writeq(vals(O)),  % Print the value of O quoted.
    !.

% previously: l2r(O) code block was commented out, possibly because it duplicates grid representation functionality.
% pp_hook_g1(l2r(O)) :- into_solid_grid_strings(l2r(O), Str), Str \=@= l2r(O), print_term_no_nl(Str), !.

pp_hook_g1(localpoints(O)) :-  
    is_points_list(O),       % Check if O is a list of points.
    as_grid_string(O, S),    % Convert the list of points to a grid string.
    wotsq(O, Q),             % Perform query/processing on O.
    print(localpoints(S, Q)), % Print the local points.
    !.

pp_hook_g1(C) :-  
    compound(C),              % Check if C is a compound term.
    compound_name_arguments(C, F, [O]), % Extract the functor and arguments of the compound term.
    is_points_list(O),        % Check if O is a list of points.
    length(O, N), N > 2,      % Ensure O contains more than two points.
    as_grid_string(O, S),     % Convert O to a grid string.
    compound_name_arguments(CO, F, [S]), % Reconstruct the compound term with the grid string.
    print(CO),                % Print the new compound term.
    !.

pp_hook_g1(O) :-  
    is_points_list(O),         % Check if O is a list of points.
    as_grid_string(O, S),      % Convert the list to a grid string.
    write(S),                  % Write the grid string.
    !.

pp_hook_g1(O) :-  
    is_real_color(O),          % Check if O represents a real color.
    color_print(O, call(writeq(O))), % Print the color in a special colored format.
    !.

pp_hook_g1(O) :-  
    is_colorish(O),            % Check if O has color properties.
    data_type(O, DT),          % Get the data type of O.
    writeq('...'(DT)),         % Print the data type of O.
    !.

pp_hook_g1(_) :-  
    \+ in_pp(ansi),            % If we are not in ANSI mode, fail.
    !,
    fail.

pp_hook_g1(Grp) :-  
    current_predicate(pp_ilp/1),  % Check if the predicate pp_ilp/1 exists.
    is_rule_mapping(Grp),        % Check if Grp is a rule mapping.
    pp_ilp(Grp),                 % Call pp_ilp to print the rule mapping.
    !.

pp_hook_g1(O) :-  
    atom(O),                    % Check if O is an atom.
    atom_contains(O, 'o_'),      % Ensure O contains the substring 'o_'.
    pp_parent([LF|_]),          % Get the parent of the current term.
    \+ (LF == lf; LF == objFn), % Ensure LF is not 'lf' or 'objFn'.
    resolve_reference(O, Var),  % Resolve O to its reference Var.
    O \== Var,                  % Ensure O is different from Var.
    \+ plain_var(Var),          % Ensure Var is not a plain variable.
    !,
    write_nbsp,                 % Write a non-breaking space.
    writeq(O),                  % Print O quoted.
    write(' /* '),              % Write a comment opening.
    show_indiv(Var),            % Show the individual Var.
    write(' */ ').              % Close the comment.

pp_hook_g1(O) :-  
    is_object(O),               % Check if O is an object.
    pp_no_nl(O),                % Print O without a newline.
    !.

pp_hook_g1(O) :-  
    is_group(O),                % Check if O is a group.
    pp_no_nl(O),                % Print O without a newline.
    !.

% previously: change_obj was commented out, possibly due to redundancy in object comparison and presentation.
% pp_hook_g1(change_obj(N, O1, O2, Sames, Diffs)) :- showdiff_objects5(N, O1, O2, Sames, Diffs), !.

pp_hook_g1(O) :-  
    is_vm_map(O),               % Check if O is a VM map.
    data_type(O, DT),           % Get the data type of O.
    writeq('..map.'(DT)),       % Print the data type of the VM map.
    !.

pp_hook_g1(O) :-  
    is_gridoid(O),              % Check if O is a grid-like object.
    show_indiv(O),              % Show the individual properties of the grid-like object.
    !.

% previously: change_obj and diff were commented out, possibly due to updates in the diff and object change visualization mechanism.
% pp_hook_g1(O) :- O = change_obj(O1, O2, _Same, _Diff), w_section(showdiff_objects(O1, O2)), !.
% pp_hook_g1(O) :- O = change_obj(O1, O2, _Same, _Diff), w_section(object, [O1, O2], with_tagged('h5', pp(O))).

pp_hook_g1(O) :-  
    O = showdiff(O1, O2),       % If O represents a difference between O1 and O2.
    !,
    showdiff(O1, O2).           % Show the differences between O1 and O2.

% previously: compound(O), wqs1(O) was commented out as part of an optimization or replacement.
% pp_hook_g1(O) :- compound(O), wqs1(O), !.

pp_hook_g1(O) :-  
    \+ compound(O),             % If O is not a compound term, fail.
    fail.

pp_hook_g1(G) :-  
    '@'(pp_hook_g1a(G), user).  % Call pp_hook_g1a in the context of the user.

pp_hook_g1a(G) :-  
    \+ current_prolog_flag(debug, true),  % If debug mode is off.
    current_predicate(pp_hook_g2/1),     % Check if pp_hook_g2/1 exists.
    lock_doing(in_pp_hook_g3, any, pp_hook_g2(G)),  % Lock and execute pp_hook_g2 for G.
    !.