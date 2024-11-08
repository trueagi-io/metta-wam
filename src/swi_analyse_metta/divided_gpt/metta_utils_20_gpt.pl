  unless permission or license is granted (contact at business@logicmoo.org)
*/

% Directives for meta-predicate declarations indicating how arguments 
% are handled in higher-order predicates.
:- meta_predicate(print_grid(+,+,+,+)).   % Declares that print_grid/4 expects inputs (+ means instantiated terms)
:- meta_predicate(print_grid(+,+,+)).     % Declares that print_grid/3 expects inputs (+ means instantiated terms)

% The following commented line is an autoload directive that was previously 
% used to load HTML writing predicates. It's now commented, likely due to it being unused in this context.
%:- autoload(library(http/html_write),[html/3,print_html/1]).

/** <predicate> is_debugging/1
    Check if debugging is enabled for a given module or context.
    This predicate is used to determine whether to run debugging logic.
    
    @param M The debugging context or module.
    @example
      ?- is_debugging(my_module).
*/
is_debugging(M):- \+ \+ debugging(M),!.  % Checks if the module M is in debugging mode.
is_debugging(_):- is_testing,!.  % Also allows if the system is in testing mode.
% This line was previously uncommented but has been skipped now. Likely redundant with new debugging logic.
%is_debugging(_):- menu_or_upper('B').

/** <predicate> debug_m/2
    Debugging utility that prints or logs information depending on the context.
    If debugging is enabled for module M, it will print the term Tiny.

    @param M The debugging context or module.
    @param Tiny The term or data to debug.
*/
debug_m(_,Tiny):- display_length(Tiny,Len),Len<30,!,pp(Tiny).  % Only display if the term's string length is less than 30.
debug_m(M,_):- \+ is_debugging(M),!.  % Skip if debugging is not enabled for module M.
% Skipped this version of debug_m because it was previously unused for lists.
%debug_m(_,List):- is_list(List),!,print_ss(List).
debug_m(_,Term):- pp(Term).  % If none of the above conditions apply, print the term.

% The debug_c predicates are similar but allow conditional execution with call/1.

/** <predicate> debug_c/2
    Conditional debugging that executes and prints the result of a goal 
    if debugging is enabled for a module.
    
    @param M The debugging context or module.
    @param C The goal to execute and debug.
*/
debug_c(M,_):- \+ is_debugging(M),!.  % Skip if debugging is not enabled for module M.
debug_c(_,C):- call(C),!.  % Call the goal if debugging is enabled.
debug_c(M,C):- wots_hs(S,C),debug_m(M,S),!.  % Log the result of the goal using the helper predicate wots_hs.

/** <predicate> wno/1
    Wrapper around a goal G to ensure some context settings are applied 
    during its execution. Specifically, it sets the value of the 
    `print_collapsed` flag to 10.

    @param G The goal to be executed.
*/
:- meta_predicate(wno(0)).  % Declares that wno/1 takes a goal (0 means a goal).
wno(G):-
  locally(b_setval(print_collapsed,10), G).  % Locally sets a flag, executes the goal, and then restores the flag.

/** <predicate> print_collapsed/2
    Runs a goal with a specific setting for collapsing print size.

    @param Size The size for print collapsing.
    @param G The goal to be executed.
*/
:- meta_predicate(print_collapsed(0)).  % Declares that print_collapsed/2 takes a goal.
print_collapsed(Size,G):-
  locally(b_setval(print_collapsed,Size), print_collapsed0(Size,G)).  % Sets the collapse size and executes the goal.

/** <predicate> print_collapsed0/2
    Lower-level predicate that actually performs the print operation based 
    on the given size.

    @param Size The size for print collapsing.
    @param G The goal to be executed.
*/
:- meta_predicate(print_collapsed0(0)).  % Declares that print_collapsed0/2 takes a goal.
print_collapsed0(Size,G):- Size<10, !, call(G).  % If the size is less than 10, just call the goal.
% Skipped alternative branch for print_collapsed0, was previously unused.
% print_collapsed(Size,G):-  call(G).
print_collapsed0(Size,G):- Size>=10, !, wots_hs(_S,G).  % If the size is 10 or more, use wots_hs to handle printing.
print_collapsed0(_,G):- wots_vs(S,G),write(S).  % Another form of handling print collapse with wots_vs.

/** <predicate> tersify/2
    Tries to simplify or compress a complex term for easier display. 
    This is helpful for debugging large structures.
    
    @param I The input term.
    @param O The simplified output term.
*/
tersify(I,O):- tracing,!,I=O.  % If tracing is enabled, don't modify the term.
% This version of tersify was previously skipped, possibly to avoid handling attvars unnecessarily.
% tersify(I,O):- term_variables(I,Vs), \+ ( member(V,Vs), attvar(V)),!,I=O.
tersify(I,O):- tersify23(I,O),!.  % Applies more specific tersification logic.
tersify(X,X):-!.  % If no transformation is applied, the term remains unchanged.

/** <predicate> srw_arc/2
    Attempts to simplify and represent certain complex data types, such as grids 
    and VM maps, in a compressed format for easier debugging.

    @param I The input term.
    @param O The output (compressed or simplified) term.
*/
% This version of srw_arc was skipped, as grid handling was deemed unnecessary in this context.
%srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
srw_arc(I,O):- is_vm_map(I),!, O='..vvmm..'.  % If the input is a VM map, simplify it to '..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.  % If the input is a grid, simplify it to '..grid..'.
% Skipped handling for long lists as it was previously commented out.
%srw_arc(List,O):- current_prolog_flag(dmsg_len,Three),
%  is_list(List),length(List,L),L>Three,
%   append([A,B,C],[F|_],List),F \='...'(_), !,
%  simplify_goal_printed([A,B,C,'....'(L>Three)],O).
% Further list handling versions were skipped as well.
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
srw_arc(I,O):- tersify(I,O),!,I\==O,!.

% Multifile and dynamic declarations for hook predicates related to simple rewriting.
:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

/** <predicate> dumpst_hook:simple_rewrite/2
    Hook for simple term rewriting, potentially used for debugging or logging. 
    It is currently disabled (`fail`), but the logic remains for potential future use.
*/
dumpst_hook:simple_rewrite(I,O):- fail, notrace(catch(arc_simple_rewrite(I,O),_,fail)).

% The following is an internal predicate for simple rewriting, skipped because it's 
% part of an experimental or optional feature.
arc_simple_rewrite(I,O):-
  \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(bfly_startup/0),
  current_predicate(is_group/1),
  b_setval(arc_can_portray,nil),
  locally(b_setval(arc_can_portray,nil),once((compound(I), lock_doing(srw_arc,I,srw_arc(I,O))))), I\==O, I\=@=O, !, \+ I=O,
  b_setval(arc_can_portray,t).

% This flag setting was skipped as it permanently disables the `pp_hook`.
%:- set_prolog_flag(never_pp_hook, true).

% portray_terse/0 is used to enforce a more concise display style.
portray_terse:- true,!.

/** <predicate> arc_portray/2
    Custom term portrayal logic for specific term types. Uses 
    `write_keeping_ansi_mb/1` if ANSI codes are detected.

    @param S The term to portray.
    @param TF The output format (not used here).
*/
:- discontiguous arc_portray/2.

arc_portray(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).  % Handle terms that use ANSI formatting.
arc_portray(_,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t), !, fail.  % Skip if `pp_hook` is disabled.
arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs).  % Portray maps by showing their pairs.