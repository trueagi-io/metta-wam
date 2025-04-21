/* ----------------------------------------------------------------------
   "Tree-style" dependency printing
   ---------------------------------------------------------------------- */

% We'll store visited as mfa(Space,Name,Arity)
% and gather children the same way
% where we unify on (ParentSpace, ParentName, ParentArity) => children.
tree_deps(Name, Arity) :-
    find_space(Name, Arity, Space),
    tree_deps(Space, Name, Arity).

tree_deps(ParentSpace, Name, Arity) :-
    format("~nDependency Tree for ~q:~q/~q:\n", [ParentSpace, Name, Arity]),
    print_root(ParentSpace, Name, Arity).

print_root(ParentSpace, Name, Arity) :-
    % We don't know ParentSpace a priori; find it in transpiler_depends_on(ParentSpace, Name, Arity, _, _, _)
    % or we can find it by also scanning the DB. One strategy is: if there's a fact with (ParentSpace,Name,Arity,_,_,_) or
    % if not found, treat it as "unknown_space". For simplicity, let's pick the first that shows up or default.
    find_space(Name, Arity, ParentSpace),
    format("~q:~q/~q\n", [ParentSpace, Name, Arity]),
    % Gather children
    findall(mfa(CSpace, CName, CArity),
            transpiler_depends_on(ParentSpace, Name, Arity, CSpace, CName, CArity),
            Children),
    length(Children, Count),
    print_children(Children, 0, Count, "", [mfa(ParentSpace,Name,Arity)]).

print_children([], _, _, _, _).
print_children([mfa(CSpace,CName,CArity)|Rest], Index, Count, Prefix, VisitedIn) :-
    ( Index+1 =:= Count -> BranchSym = "└── " ; BranchSym = "├── " ),

    ( memberchk(mfa(CSpace,CName,CArity), VisitedIn) ->
        % cycle
        format("~w~w(*) ~q:~q/~q~n",[Prefix, BranchSym, CSpace, CName, CArity]),
        VisitedNext = VisitedIn
    ;   % normal
        format("~w~w~q:~q/~q~n",[Prefix, BranchSym, CSpace, CName, CArity]),
        % find grandchildren
        findall(mfa(GSpace,GName,GArity),
                transpiler_depends_on(CSpace, CName, CArity, GSpace, GName, GArity),
                GrandKids),
        length(GrandKids, GCount),
        ( Index+1 =:= Count -> NextPrefix = "~w    " ; NextPrefix = "~w│   " ),
        format(atom(NewPrefix), NextPrefix, [Prefix]),
        print_children(GrandKids, 0, GCount, NewPrefix, [mfa(CSpace,CName,CArity)|VisitedIn]),
        VisitedNext = [mfa(CSpace,CName,CArity)|VisitedIn]
    ),

    NextIndex is Index + 1,
    print_children(Rest, NextIndex, Count, Prefix, VisitedNext).

/* ----------------------------------------------------------------------
   show_recompile(+Name, +Arity)
   Prints out the recompile list for (Name/Arity).
   Example usage:
     ?- show_recompile('cons-cdr', 3).
   ---------------------------------------------------------------------- */
show_recompile(Name, Arity) :-
    find_space(Name, Arity, Space),
    show_recompile(Space, Name, Arity).
show_recompile(Space, Name, Arity) :-
    format("~n~nFunctions to recompile after redefining ~p:~q/~q in correct order:~n",[Space, Name,Arity]),
    find_recompile_order(Space, Name, Arity, List),
    forall(member(mfa(M, N, A), List),
           format("   ~q:~q/~q~n",[M, N, A])).

/* ----------------------------------------------------------------------
   find_recompile_order(+Name, +Arity, -Order)

   Produces a topological ordering (list) of all spaces/preds
   that must be compiled for `Name/Arity`.
   If there's a cycle, we skip re-visiting a node (no infinite loops).
   Now uses 6-arity (reverse direction to find who depends on Name/Arity).
   ---------------------------------------------------------------------- */

find_recompile_order(Name, Arity, Order) :-
    find_space(Name, Arity, Space),
    find_recompile_order(Space, Name, Arity, Order).

find_recompile_order(Space, Name, Arity, Order) :-
    dfs_post_order(Space, Name, Arity, [], Rev),
    list_to_set(Rev, Set),
    reverse(Set, Order).

% We don't know the space of Name/Arity, so find any that appear as ChildName,ChildArity:
find_space( Name, Arity, Space) :- current_self(Space),transpiler_depends_on(Space, Name, Arity, _, _, _),!.
find_space( Name, Arity, Space) :- current_self(Space), transpiler_depends_on(_, _, _, Space, Name, Arity),!.
find_space( Name, Arity, Space) :- transpiler_depends_on(Space, Name, Arity, _, _, _),!.
find_space( Name, Arity, Space) :- transpiler_depends_on(_, _, _, Space, Name, Arity),!.
find_space(_Name,_Arity, Self)  :- current_self(Self).

/* ----------------------------------------------------------------------
   dfs_post_order(ParentSpace, ParentName, ParentArity, +Visited, -PostOrder)

   Depth-first, post-order collection.
   If mfa(Space,Name,Arity) is visited, skip.
   Otherwise:
     1) Recurse on nodes that depend on (Space,Name,Arity) as *child*, then
     2) Append this node.
   Because we want "who depends on me", we look for:
     transpiler_depends_on(ChildSpace, ChildName, ChildArity, Space, Name, Arity)
   i.e. the child is the one that has me as a dependency.
   ---------------------------------------------------------------------- */
dfs_post_order(Space, Name, Arity, Vis, []) :-
    memberchk(mfa(Space,Name,Arity), Vis), !.  % already visited => no additions

dfs_post_order(Space, Name, Arity, Vis, PostOrder) :-
    % gather children who *depend on* (Space,Name,Arity)
    findall(mfa(CSpace,CName,CArity),
            transpiler_depends_on(CSpace, CName, CArity, Space, Name, Arity),
            Children),
    % recursively visit children, then add this node
    dfs_list(Children, [mfa(Space,Name,Arity)|Vis], ChildrenPost),
    append(ChildrenPost, [mfa(Space,Name,Arity)], PostOrder).

dfs_list([], _, []).
dfs_list([mfa(NSpace,NName,NArity)|Rest], Vis, AllPost) :-
    dfs_post_order(NSpace, NName, NArity, Vis, Post1),
    dfs_list(Rest, Vis, Post2),
    append(Post1, Post2, AllPost).

transpiler_depends_on(Some, CName, CArity, Some, PName, PArity):-
   transpiler_depends_on(CName, CArity, PName, PArity).


/**
 * find_tree_deps(+ParentSpace, +Name, +Arity, -MFALIST)
 *
 * Unifies MFALIST with all (Space,Name,Arity) nodes reachable
 * from (ParentSpace,Name,Arity) via transpiler_depends_on/6,
 * including the starting node itself. Avoids cycles by skipping
 * already-visited nodes.
 *
 * The final MFALIST is a set (no duplicates) in DFS (pre-order) order.
 */
find_tree_deps(ParentSpace, Name, Arity, MFALIST) :-
    % We'll first do a DFS that may collect duplicates
    % if multiple paths reach the same node.
    % Then we'll pass that result to list_to_set/2 to remove duplicates.
    dfs_tree_deps(ParentSpace, Name, Arity, [], RawList),
    list_to_set(RawList, MFALIST).

/**
 * dfs_tree_deps(+Space, +Name, +Arity, +Visited, -List)
 *
 * Recursive helper that does the DFS in a pre-order style:
 *   - if we've already visited this node, return []
 *   - otherwise, put this node at the head,
 *     then DFS over each child to collect the sub-lists
 */
dfs_tree_deps(Space, Name, Arity, Visited, []) :-
    % If we've already visited this node, return empty.
    memberchk(mfa(Space, Name, Arity), Visited),
    !.

dfs_tree_deps(Space, Name, Arity, Visited, [mfa(Space,Name,Arity)|ChildrenList]) :-
    % 1. Mark this node visited
    % 2. Find all direct children
    % 3. Recursively gather each child's subtree
    findall(mfa(CS, CN, CA),
            transpiler_depends_on(Space, Name, Arity, CS, CN, CA),
            ChildNodes),
    dfs_tree_deps_list(ChildNodes, [mfa(Space,Name,Arity)|Visited], ChildrenList).

/**
 * dfs_tree_deps_list(+ListOfMFAs, +Visited, -AllChildren)
 *
 * Walks each child in turn, collecting DFS expansions and appending.
 */
dfs_tree_deps_list([], _Visited, []).
dfs_tree_deps_list([mfa(CSpace,CName,CArity)|Rest], Visited, All) :-
    % DFS on the first child
    dfs_tree_deps(CSpace, CName, CArity, Visited, ThisChild),
    % Then DFS on the remaining children
    dfs_tree_deps_list(Rest, Visited, MoreChildren),
    % Combine them
    append(ThisChild, MoreChildren, All).


output_prolog(Converted):- output_prolog(cyan,Converted).
output_prolog(Color,Converted):-
   inotrace(
    (printable_vars(Converted,ConvertedC),
                         color_g_mesg(Color, output_language(prolog, output_prolog0(ConvertedC))))).

output_prolog0(Converted):- is_list(Converted), maplist(output_prolog0,Converted).
output_prolog0(Converted --> B):-  print_pl_source(Converted --> B).
output_prolog0(:-B):- !,  print_pl_source(:-B).
output_prolog0(Converted:-B):- !, nl, print_pl_source(Converted:-B).
output_prolog0(Converted):- print_pl_source(Converted:-true).

inotrace(G):-
  ignore( \+ notrace(G)).

print_ast(HB):- print_ast( yellow, HB).
print_ast(Color,HB):-
   inotrace((printable_vars(HB,HBP),
   color_g_mesg(Color,
     output_language( ast, (writeln('======='), ppt(HBP)))))).

printable_vars(HB,HBPN):-
   copy_term(HB,HBP),
   set_vnames(HBP),
   copy_term_nat(HBP,HBPN),
   numbervars(HBPN,0,_,[]),!.

set_vnames(HBP):-
 term_variables(HBP,Vars),
  maplist(only_names,Vars).


only_names(Var):- % del_attr(Var,cns),
  ignore((get_attr(Var,vn,VN),Var = '$VAR'(VN))),!.
only_names(Var):-  ignore(catch(del_attr(Var,cns),_,fail)),
  ignore((get_attr(Var,vn,VN),nop(ignore(Var = '$VAR'(VN))))).


