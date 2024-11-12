/* Directive explanation: This directive ensures that we handle certain
   grid objects correctly using the tersify/2 predicates */
% Ensuring proper formatting of grids and objects

% PLDoc header for print_info/1
%! print_info(+Val) is det.
% This predicate prints information about the value Val.
print_info(Val),

% This line ensures that Val is not a list with one element.
Val \= [_],

% PLDoc header for compare_objects/2
%! compare_objects(+Val, -Diffs) is det.
% This predicate compares the object Val and generates differences in Diffs.
compare_objects(Val,Diffs),

% PLDoc header for color_print/2
%! color_print(+Color, +Message) is det.
% This predicate prints a message in the specified color.
color_print(cyan,call(arc_portray_pair(Ps,diffs(K),Diffs,TF))))).

/* Old commented-out arc_portray/1 with an old example.
   Previously: used an extra tracing block for portraying */
% arc_portray(G):- \+ \+ catch((wots_hs(S,( tracing->arc_portray(G,true);arc_portray(G,false))),write(S),ttyflush),_,fail).

% PLDoc header for arc_portray/1
%! arc_portray(+G) is semidet.
% This predicate attempts to portray G using various conditions.
% If G is not a compound, we fail immediately.
arc_portray(G):- \+ compound(G),fail.

% If G is a virtual machine (VM), print '..VM..' and succeed.
arc_portray(G):- is_vm(G), !, write('..VM..').

% If arc_portray is not set to 't' or 'f', print collapsed version.
arc_portray(G):- \+ nb_current(arc_portray,t),\+ nb_current(arc_portray,f),is_print_collapsed,!,
  locally(nb_setval(arc_portray,t),arc_portray1(G)).

% Otherwise, proceed with portrayal and mark as 't'.
arc_portray(G):- \+ nb_current(arc_portray,f),!, locally(nb_setval(arc_portray,t),arc_portray1(G)).

% In case arc_portray is marked as 'f', portray with 'f'.
arc_portray(G):- locally(nb_setval(arc_portray,f),arc_portray1(G)).

% PLDoc header for arc_portray1/1
%! arc_portray1(+G) is det.
% Internal helper for portraying G with depth tracking.
arc_portray1(G):-
  % Ensure we do not exceed portrayal depth.
  flag(arc_portray_current_depth,X,X), X < 3,
  \+ \+
    % Execute portrayal and handle exceptions.
    setup_call_cleanup(flag(arc_portray_current_depth,X,X+1),
      catch(((tracing->arc_portray(G,true); arc_portray(G,false)),ttyflush),
        E,(fail,format(user_error,"~N~q~n",[E]),fail)),
      flag(arc_portray_current_depth,_,X)).

/* The following via_print_grid/1 predicates are responsible
   for grid-based objects and check different types of grid-related data. */

% PLDoc header for via_print_grid/1
%! via_print_grid(+G) is semidet.
% This predicate verifies whether G is a grid-based object.
% If we are tracing, we skip and fail the predicate.
%via_print_grid(G):- tracing,!,fail.

% If G is a list of points, proceed with portrayal.
via_print_grid(G):- is_points_list(G).

% If G is a grid, proceed.
via_print_grid(G):- is_grid(G).

% If G has object properties, we fail to avoid processing.
via_print_grid(G):- is_obj_props(G),!,fail.

% If G is an object, continue.
via_print_grid(G):- is_object(G).

% If G is a group, continue.
via_print_grid(G):- is_group(G).

% If G is a gridoid (a grid-like structure), continue.
via_print_grid(G):- is_gridoid(G).

% PLDoc header for terseA/3
%! terseA(+I, +L, -Result) is det.
% This predicate simplifies or "terse-ifies" attribute lists or differences.
terseA(_,[],[]):- !.

% If the list L has more than 10 elements, replace it with an ellipsis and count.
terseA(_,L,'... attrs ...'(N)):- is_list(L),length(L,N),N>10,!.

% Recursively process attributes in the list.
terseA(I,[A|L],[B|LL]):- terseA(I,A,B), terseA(I,L,LL),!.

% Simplify dif/2 attributes if the first argument equals I.
terseA(I,dif(A,B),B):- A==I,!.
terseA(I,dif(B,A),B):- A==I,!.

% Handle special case for put_attr/3 predicates with attribute B being ci.
terseA(_,put_attr(_,B,A),A):- B==ci,!.

% General case for put_attr/3.
terseA(_,put_attr(_,B,A),B=A):-!.

% By default, return the original element.
terseA(_,A,A):-!.

% PLDoc header for simple_enough/1
%! simple_enough(+I) is semidet.
% This predicate checks if a term is simple enough to be displayed as-is.
simple_enough(I):- plain_var(I).
simple_enough(I):- atomic(I).
simple_enough(I):- \+ compound(I),!.

% Skip specific compound structures like multiplication and addition.
simple_enough(_*_):-!.
simple_enough(_+_):-!.

% If A is a functor with arity 1, check its argument.
simple_enough(A):- functor(A,_,1),tc_arg(1,A,E),!,simple_enough(E).

/* Previously: handled numbers and atoms as simple enough directly */
%simple_enough(I):- number(I).
%simple_enough(I):- atom(I).

% PLDoc header for tersify0/2
%! tersify0(+I, -O) is det.
% This predicate attempts to simplify or "tersify" terms.
% If the term is simple, return it as-is.
tersify0(I,O):- simple_enough(I),!,I=O.

% If I is an attribute variable, extract and simplify its attributes.
tersify0(I,av(C,Others)):- attvar(I),copy_term(I,C,Attrs),terseA(C,Attrs,Others),!.

% Otherwise, return I unchanged.
tersify0(I,I):- var(I),!.

/* Skipped certain terification rules for virtual machine maps, 
   as they require specialized handling */
%tersifyC(D):- is_vm_map(D),!.

% PLDoc header for tersifyC/1
%! tersifyC(+Term) is semidet.
% This predicate checks for specific compound types that are left unsimplified.
tersifyC(av(_,_)).
tersifyC(objFn(_,_)).
tersifyC(groupFn(_,_)).
tersifyC(objFn(_)).
tersifyC(groupFn(_)).

% PLDoc header for tersify1/2
%! tersify1(+I, -O) is det.
% A more advanced tersify predicate that handles more complex terms.
% If the input is simple, return it as-is.
tersify1(I,O):- simple_enough(I),!,I=O.

% Handle special case for av/2 structures.
tersify1(av(_,Blue), -(Blue)):-!.

% If the term is compound and matches specific types, return it unchanged.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.

% Special handling for grid-related terms.
tersify1(gridFn(I),gridFn(I)):-!. % previously: applied tersifyG/2 to grids.

% Handling grids by simplifying them into grid names.
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).

% If I is a group, map its elements and optionally summarize the group.
tersify1(I,groupFn(O,List)):- is_group(I), mapgroup(tersify1,I,List), mapgroup(obj_to_oid,I,OIDs), length(List,N),
  !, ignore((get_current_test(TestID),is_why_grouped(TestID,N,Why,OIDs),!,O=Why)).

% Handle object references.
tersify1(I,Q):- is_object(I),object_ref_desc(I,Q),!.

% Special cases for virtual machine maps.
tersify1(I,O):- is_vm_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_vm_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').

/* Tersify grid structures by simplifying and binding variables */
% PLDoc header for tersifyG/2
%! tersifyG(+I, -O) is det.
% This predicate simplifies grids by numbering their variables.
tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.