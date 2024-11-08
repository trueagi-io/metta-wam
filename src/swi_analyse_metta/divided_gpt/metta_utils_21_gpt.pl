/** 
 * arc_portray_t/2
 * This predicate portrays various types of graphical representations
 * based on the type of the input `G`. It handles virtual maps, grids, 
 * and general printing as fallback.
 *
 * @param G The graphical object to be portrayed.
 * @param _ Unused parameter.
 * @example
 * ?- arc_portray_t(Grid, _).
 */
arc_portray_t(G, _) :- 
    % If `G` is a virtual map, portray it using write_map with 'arc_portray_t'.
    is_vm_map(G), 
    !, 
    write_map(G, 'arc_portray_t').

arc_portray_t(G, _) :- 
    % If `G` is a grid, portray it as a grid and display its data type.
    is_grid(G),  
    !, 
    data_type(G, W), 
    writeq(grid(W)).

arc_portray_t(G, _) :- 
    % Fallback case: simply print `G`.
    print(G), 
    !.

/** 
 * arc_portray/2
 * This is the main predicate to portray graphical objects. It tries 
 * to handle virtual maps, enable terse portrayals, and includes error handling.
 *
 * @param G The graphical object to be portrayed.
 * @param TF Terse flag to determine the display mode.
 * @example
 * ?- arc_portray(Map, true).
 */
arc_portray(G, _) :- 
    % If `G` is a virtual map, portray it using write_map with 'arc_portray'.
    is_vm_map(G),  
    !, 
    write_map(G, 'arc_portray').

arc_portray(G, TF) :- 
    % If TF is true, use terse portrayal and call `arc_portray_t`.
    TF == true, 
    portray_terse, 
    arc_portray_t(G, TF), 
    !.

arc_portray(G, TF) :- 
    % Catch any errors in `arc_portray_nt` and ensure that arc portrayal is never attempted again on failure.
    catch(arc_portray_nt(G, TF), E, (writeln(E), never_let_arc_portray_again, fail)), 
    !.

% arc_portray(G, _TF) :- 
%     writeq(G), !.
% 
% The above line was skipped as it would simply print the term in a quoted form, 
% which might not be the desired behavior when portraying more complex graphical objects.

/** 
 * arc_portray_nt/2
 * This helper predicate portrays objects in the debugger.
 * It handles grids, objects, and groups.
 *
 * @param G The graphical object to be portrayed.
 * @param TF Terse flag.
 */
arc_portray_nt(G, false) :- 
    % If `G` is a grid, print the grid.
    is_grid(G), 
    print_grid(G), 
    !.

% arc_portray_nt([G|L],_False) :- 
%     is_object(G), !, pp([G|L]).
% 
% The above line was skipped because it uses a specific object printing function (pp),
% which might not be suitable for all cases and has been replaced with a more generic approach.

% arc_portray_nt(G0, true) :- 
%     is_group(G0), ppt(G0), !.
% arc_portray_nt(G0, false) :- 
%     is_group(G0), ppt(G0), !.
% 
% These lines were commented out because they duplicate the group handling logic below.

arc_portray_nt(G0, Tracing) :- 
    % If `G0` is a group, convert it to a list and write a tersified version.
    is_group(G0), 
    into_list(G0, G), 
    length(G, L),  % Check the length of the group.
    maplist(tersify, G0, GG), 
    write(GG),
    if_t(Tracing == false,
         in_cmt((
             % In the case of non-tracing, print additional information about why it was grouped.
             dash_chars,
             once(((why_grouped(_TestID, Why, WG), WG =@= G, fail); (Why = (size2D = L)))), 
             !,
             print_grid(Why, G), nl_now,
             dash_chars))).

arc_portray_nt(G, _False) :- 
    % If `G` is an object, handle its graphical representation.
    is_object(G), 
    wots(S, writeg(G)),
    global_grid(G, GG), 
    !,
    print_grid(GG),
    write(S), 
    !.

arc_portray_nt(G, false) :- 
    % If `G` can be printed via the grid printing mechanism, do so and check its dimensions.
    via_print_grid(G), 
    !, 
    grid_size(G, H, V), 
    !, 
    H > 0, 
    V > 0, 
    print_grid(H, V, G).

% Other portray cases for object printing in the tracer.
arc_portray_nt(G, true) :- 
    is_object(G), 
    underline_print((ppt(G))).

arc_portray_nt(G, true) :- 
    via_print_grid(G), 
    write_nbsp, 
    underline_print((ppt(G))), 
    write_nbsp.

arc_portray_nt(G, true) :- 
    tersify(G, O), 
    write_nbsp, 
    writeq(O), 
    write_nbsp.

arc_portray_nt(G0, _) :- 
    % If `G0` is not gridoid, simply print it.
    \+ is_gridoid(G0), 
    !, 
    print(G0).

/** 
 * arc_portray_pairs/3
 * Portrays pairs of graphical objects based on their type.
 *
 * @param Type The type of the pairs to be portrayed.
 * @param TF Terse flag.
 * @param Pairs The list of key-value pairs.
 */
arc_portray_pairs(Type, TF, Pairs) :- 
    % Print the length of the pairs for debugging.
    length(Pairs, N),
    writeln(arc_portray_pairs(Type, TF, len(N))),
    % Swap keys and values for sorting and portrayal.
    swap_kv(Pairs, VKPairs),
    keysort(VKPairs, SVKPairs),
    my_maplist(tc_arg(2), SVKPairs, SVKPairs2),
    arc_portray_type_pairs(TF, SVKPairs2).

/** 
 * arc_portray_type_pairs/2
 * Handles portraying pairs of grid types side by side.
 *
 * @param TF Terse flag.
 * @param Pairs The list of key-value pairs.
 */
arc_portray_type_pairs(TF, Pairs) :- 
    % If the first two elements in the pairs are grids, print them side by side.
    append(Left, [K1-V1, K2-V2|Right], Pairs), 
    is_grid(V1), 
    is_grid(V2), 
    !,
    append(Left, [call-print_side_by_side(yellow, V1, K1, _, V2, K2)|Right], PairsM),
    arc_portray_type_pairs(TF, PairsM).

arc_portray_type_pairs(TF, Pairs) :- 
    % Default case: portray each pair.
    forall(member(K-V, Pairs), arc_portray_pair(Pairs, K, V, TF)).

/** 
 * swap_kv/2
 * Swaps keys and values in the pair list.
 *
 * @param Pairs The input list of pairs.
 * @param VKPairs The output list with swapped keys and values.
 */
swap_kv([_-V|Pairs], VKPairs) :- 
    % Skip plain variables.
    plain_var(V), 
    !, 
    swap_kv(Pairs, VKPairs).

swap_kv([K-V|Pairs], ['-'(Type, K-V)|VKPairs]) :- 
    % Swap key and value and determine the type of the value.
    data_type(V, Type),
    swap_kv(Pairs, VKPairs).

swap_kv([], []) :- 
    % Base case: empty list.
    true.

/** 
 * arc_portray_pair/4
 * Portrays a single pair of key-value.
 *
 * @param Ps The list of pairs.
 * @param K The key.
 * @param Val The value.
 * @param TF Terse flag.
 */
arc_portray_pair(Ps, K, Val, TF) :- 
    % Handle printing of a single pair and newline if needed.
    nl_if_needed,
    arc_portray_1_pair(Ps, K, Val, TF),
    nl_if_needed_ansi.

/** 
 * arc_portray_1_pair/4
 * Handles special cases for a single key-value pair.
 *
 * @param Ps The list of pairs.
 * @param K The key.
 * @param Val The value.
 * @param TF Terse flag.
 */
arc_portray_1_pair(_Ps, call, Val, _TF) :- 
    % If the key is 'call', execute the value as a predicate.
    !, 
    call(Val).

arc_portray_1_pair(Ps, K, Val, TF) :- 
    % Default case: print the key and portray or print the value.
    (via_print_grid(Val) -> 
        print_grid(K, Val)
    ;  
        (print(K), write('= '), once(arc_portray(Val, TF); print(Val)))
    ),
    ignore(arc_portray_pair_optional(Ps, K, Val, TF)), 
    !.

/** 
 * arc_portray_pair_optional/4
 * Optionally portrays a pair if it meets certain conditions.
 *
 * @param Ps The list of pairs.
 * @param K The key.
 * @param Val The value.
 * @param TF Terse flag.
 */
arc_portray_pair_optional(Ps, K, Val, TF) :- 
    % Only handle lists of objects here.
    once((Val \== [], is_list(Val), my_maplist(is_object, Val))).