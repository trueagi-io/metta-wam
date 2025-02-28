/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

% ==============================
% Prolog to MeTTa transpilation (which uses the Host SWI-Prolog compiler)
% This Prolog code block is mainly aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================

% Setting the file encoding to ISO-Latin-1
%:- encoding(iso_latin_1).
% Flushing the current output
:- flush_output.
% Setting the Rust backtrace to Full
:- setenv('RUST_BACKTRACE',full).
% Loading various library files
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
%:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
:- dynamic(transpiler_clause_store/9).
:- ensure_loaded(metta_compiler_lib).

% ==============================
% MeTTa to Prolog transpilation (which uses the Host SWI-Prolog compiler)
% Aimed at compiling/optimizing and transforming
% Prolog predicates to functional equivalents and vice versa, with special attention
% to handling different logical constructs and performing conversions between
% functions and predicates.
% ==============================
:- dynamic(metta_compiled_predicate/3).
:- multifile(metta_compiled_predicate/3).


% =======================================
% TODO move non flybase specific code between here and the compiler
%:- ensure_loaded(flybase_main).
% =======================================
%:- set_option_value(encoding,utf8).

mutex_create_once(MutexId):- mutex_property(Was,status(_)),MutexId==Was,!.
mutex_create_once(MutexId):- mutex_create(MutexId),!.

:- initialization(mutex_create_once(transpiler_mutex_lock)).
:- at_halt(mutex_destroy(transpiler_mutex_lock)).

%transpiler_enable_interpreter_calls.
transpiler_enable_interpreter_calls :- fail.

transpiler_show_debug_messages.
%transpiler_show_debug_messages :- fail.

:-dynamic(transpiler_stub_created/3).
% just so the transpiler_stub_created predicate always exists
transpiler_stub_created(space,dummy,0).

:- dynamic(transpiler_stub_created/1).
% just so the transpiler_stub_created predicate always exists
transpiler_stub_created(dummy).

:- dynamic(transpiler_depends_on/4).
% just so the transpiler_depends_on predicate always exists
transpiler_depends_on(dummy1,0,dummy2,0).

% just so the transpiler_clause_store predicate always exists
% transpiler_clause_store(f,arity,clause_number,types,rettype,lazy,retlazy,head,body)
transpiler_clause_store(dummy,0,0,[],'Any',[],eager,dummy,dummy).

:- dynamic(transpiler_stored_eval/3).
transpiler_stored_eval([],true,0).


as_p1(X,X):- \+ compound(X),!.
as_p1(is_p1(Code,Ret),Ret):- !, call(Code).
as_p1(is_p1(_Src,Code,Ret),Ret):-!,call(Code).
as_p1(is_p1(_Type,_Src,Code,Ret),Ret):-!,call(Code).
as_p1(X,X).

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).

:- op(700,xfx,'=~').

compound_non_cons(B):-  compound(B),  \+ B = [_|_].
iz_conz(B):- compound(B), B=[_|_].

'=~'(A,B):- notrace('=~0'(A,B)).

'=~0'(A,B):- compound_non_cons(B),!,into_list_args(B,BB),!,'=~'(A,BB).
'=~0'(B,A):- compound_non_cons(B),!,into_list_args(B,BB),!,'=~'(A,BB).
'=~0'(A,B):- iz_conz(A),iz_conz(B),!,A=B.
'=~0'(A,B):- var(A),iz_conz(B),!,A=B.
'=~0'(A,B):- iz_conz(A),var(B),!,A=B.
'=~0'(A,B):- compound_non_cons(A),var(B),!,A=..B.
'=~0'(A,B):- compound_non_cons(B),!,A=B.
'=~0'(A,B):- '=..'(A,B).


%into_list_args(A,AA):- is_ftVar(A),AA=A.
%into_list_args(C,[C]):- \+ compound(C),!.
into_list_args(C,C):- \+ compound(C),!.
into_list_args(A,AA):- is_ftVar(A),AA=A.
into_list_args([H|T],[H|T]):- \+ is_list(T),!.
into_list_args([H,List,A],HT):- H == x_assign,!,append(List,[A],HT),!.
into_list_args([H|T],[H|T]):- is_list(T),!.
into_list_args(x_assign(List, A),[H|T]):- append(List,[A],[H|T]),!.
into_list_args(holds(A),AA):- !, into_list_args(A,AA),!.
into_list_args(C,FArgs):- compound_name_arguments(C,F,Args),!,into_list_args([F|Args],FArgs).

compound_name_list(AsPred,FP,PredArgs):- var(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- iz_conz(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- into_list_args(AsPred,[FP|PredArgs]),!.
compound_name_list(AsPred,FP,PredArgs):- compound_non_cons(AsPred),!,compound_name_arguments(AsPred,FP,PredArgs).

strip_m(M:BB,BB):- nonvar(BB),nonvar(M),!.
strip_m(BB,BB).


transpiler_depends_on(Some, CName, CArity, Some, PName, PArity):-
   transpiler_depends_on(CName, CArity, PName, PArity).

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


compiler_assertz(Info):-
  unnumbervars_clause(Info,Assert),
  assertz(Assert),output_prolog(Info).

cname_var(Sym,Src):-  gensym(Sym,SrcV),
    put_attr(Src,vn,SrcV).
    %ignore(Src='$VAR'(SrcV)), debug_var(SrcV,Src).


output_prolog(Converted):- output_prolog(cyan,Converted).
output_prolog(Color,Converted):-
   inotrace((printable_vars(Converted,ConvertedC),
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
     output_language( ast, (writeln('======='), print_tree_nl(HBP)))))).

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



subst_varnames(Convert,Converted):-
  subst_vars(Convert,Converted,[], NVL),
  memorize_varnames(NVL).


cns:attr_unify_hook(_V,_T):- true.

%must_det_lls(G):- catch(G,E,(wdmsg(E),fail)),!.
%must_det_lls(G):- rtrace(G),!.
must_det_lls((A,B)):- !, must_det_lls(A),must_det_lls(B).
must_det_lls(G):- catch(G,E,(wdmsg(E),fail)),!.
%must_det_lls(G):- must_det_ll(G).
must_det_lls(G):- notrace,nortrace,trace,call(G),!.

extract_constraints(V,VS):- var(V),get_attr(V,vn,Name),get_attr(V,cns,Set),!,extract_constraints(Name,Set,VS),!.
extract_constraints(V,VS):- var(V),!,ignore(get_types_of(V,Types)),extract_constraints(V,Types,VS),!.
extract_constraints(Converted,VSS):- term_variables(Converted,Vars),
      % assign_vns(0,Vars,_),
       maplist(extract_constraints,Vars,VSS).
extract_constraints(V,[],V=[]):-!.
extract_constraints(V,Types,V=Types).


label_vns(S,G,E):- term_variables(G,Vars),assign_vns(S,Vars,E),!.
assign_vns(S,[],S):-!.
assign_vns(N,[V|Vars],O):- get_attr(V,vn,_),!, assign_vns(N,Vars,O).
assign_vns(N,[V|Vars],O):- format(atom(VN),'~q',['$VAR'(N)]),
  put_attr(V,vn,VN), N2 is N+1, assign_vns(N2,Vars,O).

label_arg_types(_,_,[]):-!.
label_arg_types(F,N,[A|Args]):-
  label_arg_n_type(F,N,A),N2 is N+1,
  label_arg_types(F,N2,Args).

% label_arg_n_type(F,0,A):- !, label_type_assignment(A,F).
label_arg_n_type(F,N,A):- compound(F),functor(F,Fn,Add),Is is Add+N, !, label_arg_n_type(Fn,Is,A).
label_arg_n_type(F,N,A):- add_type_to(A,arg(F,N)),!.

add_type_to(V,T):- is_list(T), !, maplist(add_type_to(V),T).
add_type_to(V,T):- T =@= val(V),!.
add_type_to(V,T):- ground(T),arg_type_hints(T,H),!,add_1type_to(V,H).
add_type_to(V,T):- add_1type_to(V,T),!.

add_1type_to(V,T):- is_list(T), !, maplist(add_1type_to(V),T).
add_1type_to(V,T):-
 must_det_lls((
   get_types_of(V,TV),
   append([T],TV,TTV),
   set_types_of(V,TTV))).

label_type_assignment(V,O):-
 must_det_lls((
   get_types_of(V,TV), get_types_of(O,TO),
   if_t(\+ (member(val(X),TV), fullvar(X)),
           (add_type_to(V,val(O)),add_type_to(V,TO))),
   %add_type_to(O,val(V)),
           add_type_to(O,TV),
   !)).

is_functor_val(val(_)).

%(: if (-> False $_ $else $else))
%(: if (-> False $T $T $T))

arg_type_hints(arg(is_True,1),'Bool').
arg_type_hints(arg(==,0),'Bool').
arg_type_hints(arg(match,0),['Empty','%Undefined%']).
arg_type_hints(arg(empty,0),'Empty').
arg_type_hints(val('Empty'),'Empty').
arg_type_hints(val('True'),'Bool').
arg_type_hints(val('False'),'Bool').
arg_type_hints(val(Val),[val(Val)|Types]):- get_val_types(Val,Types).
arg_type_hints(arg('println!',0),'UnitAtom').
arg_type_hints(arg(F,Arg),[arg(F,Arg)|Types]):-
   findall(Type,get_farg_type(F,Arg,Type),List),merge_types(List,Types),Types\==[].

get_farg_type(F,Arg,Type):- get_type(F,Res),(Res=[Ar|List],Ar=='->'), (Arg==0->last(List,TypeM);nth1(Arg,List,TypeM)),(nonvar(TypeM)->TypeM=Type;Type='%Var').
get_val_type(Val,Type):- get_type(Val,TypeM),(nonvar(TypeM)->TypeM=Type;Type='%Var%').
get_val_types(Val,Types):- findall(Type,get_val_type(Val,Type),List),merge_types(List,Types).
merge_types(List,Types):- list_to_set(List,Types),!.

get_just_types_of(V,Types):- get_types_of(V,VTypes),exclude(is_functor_val,VTypes,Types).

get_types_of(V,Types):- attvar(V),get_attr(V,cns,Types),!.
get_types_of(V,Types):- compound(V),V=arg(_,_),!,Types=[V].
get_types_of(V,Types):- findall(Type,get_type_for_args(V,Type),Types).

get_type_for_args(V,Type):- get_type(V,Type), Type\==[], Type\=='%Undefined%', Type\=='list'.

set_types_of(V,_Types):- nonvar(V),!.
set_types_of(V,Types):- list_to_set(Types,Set),put_attr(V,cns,Set),   nop(wdmsg(V=Types)).

precompute_typeinfo(HResult,HeadIs,AsBodyFn,Ast,Result) :-
 must_det_lls((
    HeadIs = [FnName|Args],
    LazyArgsList=[], FinalLazyOnlyRet = lazy,
    f2p(HeadIs,LazyArgsList,HResult,FinalLazyOnlyRet,AsBodyFn,NextBody),
    HeadAST=[assign,HResult,[call(FnName)|Args]],
    Ast = [=,HeadIs,NextBody],
    ast_to_prolog_aux(no_caller,[],HeadAST,_HeadC),
    ast_to_prolog(no_caller,[],NextBody,_NextBodyC),
    extract_constraints(Ast,Result))).

:- use_module(library(gensym)).          % for gensym/2
:- use_module(library(pairs)).           % for group_pair_by_key/2
:- use_module(library(logicmoo_utils)).  % for print_tree_nl/1 (pretty-print)

/** <module> combine_transform_and_collect_subterm

    Demonstration of a two-pass approach:
      1) Transform an S-expression so that *nested* calls `[Fn|Args]`
         become `'$VAR'('temp_N')` with an assignment `'temp_N' = eval([Fn|...])`.
         The top-level call is preserved.
      2) Collect underscore variables in the *final expression* by
         enumerating all subterms with sub_term_safely/2. Whenever we see a call
         (either `[Fn|Args]` or a compound `Fn(...)`), we look for underscore
         variables in the arguments and note them as `arg(Fn,Pos)`.

    We then show how to run this on a "big" expression with match-body, if,
    let*, etc., using logicmoo_utils to print results.
*/


/* ---------------------------------------------------------------------
   (1) TRANSFORMATION PASS
   --------------------------------------------------------------------- */

/** transform_expr(+OldExpr, -Assignments, -NewTopExpr) is det

    Leaves the **top-level** `[Fn|Args]` intact,
    but for each *nested* call `[SubFn|SubArgs]`, create a fresh
    variable `'$VAR'('temp_N')` and an assignment `'temp_N' = eval([...])`.
*/
transform_expr(OldExpr, Assignments, NewTopExpr) :-
    % unify real Prolog variables as '$VAR'(N), though underscores remain atoms
    numbervars(OldExpr, 0, _, [attvar(skip)]),
    transform_top(OldExpr, NewTopExpr, Assignments, 0, _).

transform_top(Var, Var, [], C, C) :- fullvar(Var), !.
transform_top(Var, Var, [], C, C) :- data_term(Var), !.
transform_top(Var, Var, [], C, C) :- Var==[], !.
transform_top(Var, Var, [], C, C) :- \+ is_list(Var).
transform_top([Fn|Args], [Fn|NewArgs], Assignments, C0, C2) :- atom(Fn), !, transform_list_subcalls(Args, NewArgs, Assignments, C0, C2).
transform_top(List, ListOut, Assignments, C0, C2) :- is_list(List), !, transform_list_subcalls(List, ListOut, Assignments, C0, C2).
transform_top(Anything, Anything, [], C, C).

transform_list_subcalls([], [], [], C, C).
transform_list_subcalls([X|Xs], [Xn|Xsn], Assignments, C0, C2) :-
    transform_subcall(X, Xn, A1, C0, C1),
    transform_list_subcalls(Xs, Xsn, A2, C1, C2),
    append(A1, A2, Assignments).

/** transform_subcall(+Expr, -ExprOut, -Assignments, +C0, -C1) is det

    For *nested* calls `[Fn|Args]`, produce `'$VAR'(temp_N)` plus assignment.
    Otherwise, just keep recursing.
*/
transform_subcall(Var, Var, [], C, C) :-
    \+ is_list(Var), !.
transform_subcall([], [], [], C, C) :- !.
transform_subcall([Fn|Args], TmpVar, [Assignment|Arest], C0, C2) :- atom(Fn), !,
    transform_list_subcalls(Args, NewArgs, Aargs, C0, C1),
    gensym('_temp_', TempName),
    TmpVar = '$VAR'(TempName),
    Assignment = (TmpVar - eval([Fn|NewArgs])),
    append(Aargs, [], Arest),
    C2 is C1.

transform_subcall(List, ListOut, A, C0, C2) :-
    is_list(List),
    transform_list_subcalls(List, ListOut, A, C0, C2).


/** var_call_refs(+Expression, -VarMappings) is det

    After transformation, we gather references to "underscore variables."
    We do this by enumerating all subterms with sub_term_safely/2, checking for
    calls that are either:
      - `[Fn|Args]` (a Prolog list with an atom head), or
      - A compound with an atom functor.

    For each Arg that starts with `_`, or is a Prolog var,
    we produce `_var - arg(Fn,Pos)`.

    Finally group and produce `_varName = [arg(Fn,Pos1), arg(Fn,Pos2), ...]`.
*/
var_call_refs(Expression, VarMappings) :-
    numbervars(Expression, 0, _, [attvar(skip)]),

    % collect all subterms
    findall(Sub, sub_term_safely(Sub, Expression), SubTerms),

    % for each subterm that is a "function call", gather references
    gather_all_function_calls(SubTerms, RawPairs),

    % group and unify
    sort(RawPairs, Sorted),
    group_pair_by_key(Sorted, Grouped),
    maplist(to_equals_pair, Grouped, VarMappings).

to_equals_pair(K-List, K-List).

%group_pair_by_key(X,Y):- !, group_pairs_by_key(X,Y).
group_pair_by_key([], []):-!.
group_pair_by_key([M-N|T0], Done) :- select(K-Vs,T0,TR), M=@=K,!,
     flatten([N,Vs],Flat),list_to_set(Flat,Set), group_pair_by_key([M-Set|TR], Done).
group_pair_by_key([M-N|T0],[M-Set|Done]):-
  flatten([N],Flat),list_to_set(Flat,Set),
  group_pair_by_key(T0, Done).

/** gather_all_function_calls(+SubTerms, -AllPairs) is det

    For each subterm in SubTerms, if it's recognized as a function call,
    call process_function_args/4 to gather underscore variables in the arguments.
*/
gather_all_function_calls([], []).
gather_all_function_calls([Term|Rest], AllPairs) :-
    (   is_function_call(Term, Fn, Args)
    ->  process_function_args(Fn, Args, 1, Pairs)
    ;   Pairs = []
    ),
    gather_all_function_calls(Rest, More),
    append(Pairs, More, AllPairs).

/** is_function_call(+Term, -Fn, -Args) is semidet

    1. If Term is a list [Fn|Args] with `atom(Fn)`, treat it as a call.
    2. If Term is a compound with `functor = Fn` (an atom) and arguments `Args`,
       also treat it as a call. For example, `eval([quote,F])` => Fn=eval, Args=[[quote,F]].
*/
is_function_call(List, Fn, Args) :-
    is_list(List),
    List = [Fn|Args],
    atom(Fn),!, \+ non_function(Fn).

is_function_call(Compound, Fn, Var) :-
    compound(Compound),
    Compound = (Var - eval([Fn|_])),
    atom(Fn),!, \+ non_function(Fn).


non_function(Fn):- var(Fn),!,fail.
non_function('&self').

process_function_args(Fn, Var, _, [Var-arg(Fn,0)]):- is_underscore_var(Var),!.
process_function_args(_, [], _, []).
process_function_args(Fn, [Arg|R], N, [Arg-arg(Fn,N)|Out]) :-
    is_underscore_var(Arg),
    !,
    N2 is N + 1,
    process_function_args(Fn, R, N2, Out).
process_function_args(Fn, [_|R], N, Out) :-
    N2 is N + 1,
    process_function_args(Fn, R, N2, Out).

/** is_underscore_var(+X) is semidet

    True if X is:
      - a real Prolog variable (var/1 or '$VAR'(_)),
      - or an atom starting with `_`.
*/
is_underscore_var(X) :- var(X), !.
is_underscore_var('$VAR'(_)) :- !.
is_underscore_var(X) :-
    atom(X),
    sub_atom(X, 0, 1, _, '_').



/* ---------------------------------------------------------------------
   (3) PUTTING IT ALL TOGETHER
   --------------------------------------------------------------------- */

combine_transform_and_collect(OldExpr, Assignments, NewExpr, VarMappings) :-
    transform_expr(OldExpr, Assignments, NewExpr),
    % Collect references from both the new expression AND the assignments.
    % That way, if you have  _temp_9 = eval([quote,F]) ...
    % the subterm [quote,F] is recognized.
    var_call_refs(OldExpr+Assignments, VarMappings).


/** test_combine_big is det

    Demo with `=`, `match-body`, `if`, nested `let*`, etc.
    Also picks up any compound calls like eval([quote,_goal]).
*/
test_combine_big :-
    OldExpr =
      [ =,
        [ match_body, _info, _body, _kb, _rb, _goal ],
        [ if,
          [==, _body, []],
          eval([quote,_goal]),          % an example compound call
          [ 'let*',
            [ [ [ _cur, _rest ],
                [ decons_atom, _body ]],
              [ _debugging, 'True' ],
              [ _bugcheck, 'False' ],
              [ [],
                [ if,
                  _debugging,
                  [ println,
                    [ quote,
                      [ "IN",
                        ["cur=", _cur],
                        ["goal=", _goal]]]],
                  []]],
              [ _12,
                [ if,
                  _bugcheck,
                  [ 'let*',
                    [ [ RetVal,
                        [ backward_chain, _info,_cur,_kb,_rb ] ],
                      [ _m,
                        [ collapse, [ equalz, [quote,_cur], _RetVal ] ] ],
                      [ [],
                        [ if,
                          [==,_m,[]],
                          [ println,
                            [ quote,
                              [ "BAD",
                                ["cur=", _cur ],
                                ["retVal=", RetVal]]]],
                          []]]],
                    []],
                  []]],
              [ [ quote, _cur ],
                [ backward_chain, _info,_cur,_kb,_rb ]],
              [ [],
                [ if,
                  _debugging,
                  [ println,
                    [ quote,
                      [ 'OUT',
                        ["cur=", _cur],
                        ["goal=", _goal]]]],
                  []]]],
            [ match_body, _info,_rest,_kb,_rb,_goal]]]],

    combine_transform_and_collect(OldExpr, Assignments, NewExpr, VarMappings),

    writeln("=== Original Expression ==="),
    print_tree_nl(OldExpr),

    writeln("=== Assignments (subcalls replaced) ==="),
    print_tree_nl(Assignments),

    writeln("=== New Expression ==="),
    print_tree_nl(NewExpr),

    writeln("=== Var Mappings (underscore variables) ==="),
    append(Assignments,VarMappings,SM),sort(SM,S),
    print_tree_nl(S).

%:- test_combine_big.



in_type_set(Set,Type):- Set==Type,!.
in_type_set(Set,Type):- compound(Set),arg(_,Set,Arg),in_type_set(Arg,Type).

b_put_set(Set,Type):- functor(Set,_,Arg),!,b_put_nset(Set,Arg,Type).
b_put_nset(Set,_,Type):- in_type_set(Set,Type),!.
b_put_nset(Set,N,Type):- arg(N,Set,Arg),
   (compound(Arg)->b_put_set(Arg,Type);b_setarg(N,Set,[Type|Arg])).

is_type_set(Set):-compound(Set),Set=ts(_).
is_var_set(Set):- compound(Set),Set=vs(_).
foc_var(Cond,vs([Var-Set|LazyVars]),TypeSet):-!,
    (var(Set)->(Cond=Var,TypeSet=Set,TypeSet=ts([]));
   (Var==Cond -> TypeSet = Set ;
   (nonvar(LazyVars) -> foc_var(Cond,vs(LazyVars),TypeSet);
    (TypeSet=ts([]),LazyVars=[Var-TypeSet|_])))).
foc_var(Cond,Set,TSet):-add_type(Set,[Cond-TSet]),ignore(TSet=ts(List)),ignore(List=[]).

add_type(Cond,Type,LazyVars):-is_var_set(LazyVars),!,must_det_lls((foc_var(Cond,LazyVars,TypeSet),!,add_type(TypeSet,Type))).
add_type(Cond,Type,_LazyVars):- add_type(Cond,Type),!.

add_type(Cond,Type):-attvar(Cond),get_attr(Cond,ti,TypeSet),!,must_det_lls(add_type(TypeSet,Type)).
add_type(Cond,Type):-var(Cond),!,must_det_lls(put_attr(Cond,ti,ts(Type))),!.
add_type(Cond,Type):-is_type_set(Cond),!,must_det_lls(b_put_set(Cond,Type)),!.
add_type(Cond,Type):-is_var_set(Cond),!,must_det_lls(b_put_set(Cond,Type)),!.
add_type(Cond,Type):- dmsg(unable_to_add_type(Cond,Type)).

remove_stub(Space,Fn,Arity):- \+ transpiler_stub_created(Space,Fn,Arity),!.
remove_stub(Space,Fn,Arity):- retract(transpiler_stub_created(Space,Fn,Arity)),!,
  transpile_impl_prefix(Fn,Arity,IFn),abolish(IFn/Arity),!.


ensure_callee_site(Space,Fn,Arity):- check_supporting_predicates(Space,Fn/Arity),!.
ensure_callee_site(Space,Fn,Arity):-transpiler_stub_created(Space,Fn,Arity),!.
ensure_callee_site(Space,Fn,Arity):-
 must_det_lls((
    compiler_assertz(transpiler_stub_created(Space,Fn,Arity)),
    transpile_call_prefix(Fn,Arity,CFn),

((current_predicate(CFn/Arity) -> true ;
  must_det_lls((( functor(CallP,CFn,Arity),
    CallP=..[CFn|Args],
    transpile_impl_prefix(Fn,Arity,IFn), CallI=..[IFn|Args],
    %dynamic(IFn/Arity),
    append(InArgs,[OutArg],Args),
    Clause= (CallP:-((pred_uses_impl(Fn,Arity),CallI)*->true;(mc_fallback_unimpl(Fn,Arity,InArgs,OutArg)))),
    compiler_assertz(Clause),
    %output_prolog(Clause),
    %create_and_consult_temp_file(Space,CFn/Arity,[Clause])
    true))))))),!.

%transpile_prefix('').
transpile_impl_prefix('mi_').
:- dynamic(is_transpile_impl_prefix/3).
transpile_impl_prefix(F,Arity,Fn):- is_transpile_impl_prefix(F,Arity,Fn)*->true;(transpile_impl_prefix(Prefix),FNArity is Arity-1,atomic_list_concat([Prefix,FNArity,'__',F],Fn),asserta(is_transpile_impl_prefix(F,Arity,Fn))).

transpile_call_prefix('mc_').
:- dynamic(is_transpile_call_prefix/3).
transpile_call_prefix(F,Arity,Fn):- is_transpile_call_prefix(F,Arity,Fn)*->true;(transpile_call_prefix(Prefix),FNArity is Arity-1,atomic_list_concat([Prefix,FNArity,'__',F],Fn),asserta(is_transpile_call_prefix(F,Arity,Fn))).


prefix_impl_preds(Prefix,F,A):- prefix_impl_preds_pp(Prefix,F,A).
prefix_impl_preds('mc_',F,A):- is_transpile_call_prefix(F,A,Fn),current_predicate(Fn/A), \+ prefix_impl_preds_pp(_,F,A).
prefix_impl_preds('mi_',F,A):- is_transpile_impl_prefix(F,A,Fn),current_predicate(Fn/A), \+ prefix_impl_preds_pp(_,F,A).

prefix_impl_preds_pp(Prefix,F,A):- predicate_property('mc_2__:'(_,_,_),file(File)),predicate_property(Preds,file(File)),functor(Preds,Fn,A),
    ((transpile_impl_prefix(Prefix);transpile_call_prefix(Prefix)),atom_list_concat([Prefix,_FNArity,'__',F],Fn)).

maplist_and_conj(_,A,B):- fullvar(A),!,B=A.
maplist_and_conj(_,A,B):- \+ compound(A),!,B=A.
maplist_and_conj(P2,(A,AA),[B|BB]):- !, maplist_and_conj(P2,A,B), maplist_and_conj(P2,AA,BB).
maplist_and_conj(P2,[A|AA],[B|BB]):- !, call(P2,A,B), maplist_and_conj(P2,AA,BB).
maplist_and_conj(P2,A,B):- call(P2,A,B), !.

notice_callee(Caller,Callee):-
   ignore((
     extract_caller(Caller,CallerInt,CallerSz),
     extract_caller(Callee,F,LArgs1),!,
     notice_callee(CallerInt,CallerSz,F,LArgs1))).

notice_callee(CallerInt,CallerSz,F,LArgs1):-
    ignore((
        CallerInt \== no_caller,
        F \== exec0,
        CallerInt  \== exec0,
        \+ (transpiler_depends_on(CallerInt,CallerSzU,F,LArgs1U), CallerSzU=@=CallerSz, LArgs1U=@=LArgs1),
         compiler_assertz(transpiler_depends_on(CallerInt,CallerSz,F,LArgs1)),
         (transpiler_show_debug_messages -> format("; Asserting: transpiler_depends_on(~q,~q,~q,~q)\n",[CallerInt,CallerSz,F,LArgs1]) -> true),
         ignore((current_self(Space),ensure_callee_site(Space,CallerInt,CallerSz))),
         output_prolog(transpiler_depends_on(CallerInt,CallerSz,F,LArgs1)) )),
    ignore((
         current_self(Space),ensure_callee_site(Space,F,LArgs1))).

extract_caller(Var,_,_):- fullvar(Var),!,fail.
extract_caller([H|Args],F,CallerSzP1):- !, extract_caller(fn_eval(H,Args,_),F,CallerSzP1).
extract_caller(fn_impl(F,Args,_),F,CallerSzP1):- !, extract_caller(fn_eval(F,Args,_),F,CallerSzP1).
extract_caller(fn_eval(F,Args,_),F,CallerSzP1):- is_list(Args), !, length(Args,CallerSz),CallerSzP1 is CallerSz+1.
extract_caller(fn_eval(F,Args,_),F,CallerSzP1):- !, \+ is_list(Args), !, CallerSzP1= _.
extract_caller(fn_native(F,Args),F,CallerSz):- !, length(Args,CallerSz).
extract_caller(caller(CallerInt,CallerSz),CallerInt,CallerSz):-!.
extract_caller((CallerInt/CallerSz),CallerInt,CallerSz):-!.
extract_caller(H:-_,CallerInt,CallerSz):- !, extract_caller(H,CallerInt,CallerSz).
extract_caller([=,H,_],CallerInt,CallerSz):-  !, extract_caller(H,CallerInt,CallerSz).
extract_caller(P,F,A):- \+ callable(P),!, F=P,A=0.
extract_caller(P,F,A):- \+ is_list(P), functor(P,F,A).


maybe_lazy_list(_,_,_,[],[]):-!.
maybe_lazy_list(Caller,F,N,[Arg|Args],[ArgO|ArgsO]):- maybe_argo(Caller,F,N,Arg,ArgO),
  N2 is N +1,
  maybe_lazy_list(Caller,F,N2,Args,ArgsO).

maybe_argo(_Caller,_F,_N,Arg,Arg):- is_list(Arg),!.
maybe_argo(_Caller,_F,_N,Arg,Arg):- \+ compound(Arg),!.
maybe_argo(Caller,_F,_N,Arg,ArgO):- ast_to_prolog_aux(Caller,[],Arg,ArgO).


optimize_prolog(_,Converted,Optimized):- \+ compound(Converted),!,Converted=Optimized.
optimize_prolog(_,Converted,Optimized):- is_list(Converted),!,Converted=Optimized.
optimize_prolog(FL,Converted,Optimized):-
   compound_name_arguments(Converted,F,Args),
   maplist(optimize_prolog([F|FL]),Args,OArgs),
   compound_name_arguments(Optimized,F,OArgs), !.
optimize_prolog(_,Prolog,Prolog).


de_eval(eval(X),X):- compound(X),!.

call1(G):- call(G).
call2(G):- call(G).
call3(G):- call(G).
call4(G):- call(G).
call5(G):- call(G).

trace_break:- trace,break.

:- if(debugging(metta(compiler_bugs))).
:- set_prolog_flag(gc,false).
:- endif.

call_fr(G,Result,FA):- current_predicate(FA),!,call(G,Result).
call_fr(G,Result,_):- Result=G.


% !(compile-body! (+ 1 $x) )
% !(compile-body! (assertEqualToResult (Add (S (S Z)) (S (S (S Z)))) ((S (S (S (S (S Z))))))) )
compile_body(Body, Output):-
 must_det_lls((
  term_variables(Body,BodyVars),
  maplist(cname_var('In_'),BodyVars),
  compile_for_exec(Ret, Body, Code),
  Output = is_p1(_Type,Body,Code,Ret),
  cname_var('Out_',Ret),
  %transpile_eval(Body,Output),
  guess_varnames(Output,PrintCode),
  print_tree_nl(out(Ret):-(PrintCode)))).

on_compile_for_exec.

% ?- compile_for_exec(RetResult, is(pi+pi), Converted).
compile_for_exec(Res,I,O):-
 on_compile_for_exec,
   %ignore(Res='$VAR'('RetResult')),
 must_det_lls((
   compile_for_exec0(Res,I,O))).

compile_for_exec0(Res,I,eval_args(I,Res)):- is_ftVar(I),!.
compile_for_exec0(Res,(:- I),O):- !, compile_for_exec0(Res,I,O).

compile_for_exec0(Converted,I, PrologCode):- !,
  must_det_lls((transpile_eval(I,Converted, PrologCode))).

compile_for_exec0(Res,I,BB):-
   compile_for_exec1(I, H:-BB),
   arg(1,H,Res).

compile_for_exec0(Res,I,BB):- fail,
   %ignore(Res='$VAR'('RetResult')),
   compile_flow_control(exec(),Res,I,O),
   head_preconds_into_body(exec(Res),O,_,BB).

%compile_for_exec0(Res,I,O):- f2p(exec(),Res,I,O).

compile_for_exec1(AsBodyFn, Converted) :-
 must_det_lls((
   Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   f2p([exec0],[],HResult,eager,AsBodyFn,NextBody),
   %optimize_head_and_body(x_assign([exec0],HResult),NextBody,HeadC,NextBodyB),
   ast_to_prolog_aux(no_caller,[],[native(exec0),HResult],HeadC),
   %ast_to_prolog(no_caller,[],[[native(trace)]|NextBody],NextBodyC).
   ast_to_prolog(no_caller,[],NextBody,NextBodyC))).

arrange_lazy_args(N,x(_,Y),N-Y).

get_operator_typedef_props(X,FnName,Largs,Types,RetType) :-
   get_operator_typedef(X,FnName,Largs,Types,RetType),!.
get_operator_typedef_props(_,_,Largs,Types,'Any') :-
    length(Types,Largs),
    maplist(=('Any'), Types).

member_var(X, [H|T]) :- X == H ; member_var(X, T).

intersect_var([],_,[]).
intersect_var([H|T],X,Y) :-
    intersect_var(T,X,Y0),
    (member_var(H,X) -> Y=[H|Y0] ; Y=Y0).

union_var([],X,X).
union_var([H|T],X,Y) :-
    union_var(T,X,Y0),
    (member_var(H,X) -> Y=Y0 ; Y=[H|Y0]).

get_property_lazy(x(_,L),L).

get_property_evaluate(x(E,_),E).

determine_eager_vars_case_aux(L,L,[],[]).
determine_eager_vars_case_aux(Lin,Lout,[[Match,Target]|Rest],EagerVars) :-
   determine_eager_vars(eager,_,Match,EagerVarsMatch),
   determine_eager_vars(Lin,LoutTarget,Target,EagerVarsTarget),
   determine_eager_vars_case_aux(Lin,LoutRest,Rest,EagerVarsRest),
   intersect_var(EagerVarsTarget,EagerVarsRest,EagerVars0),
   union_var(EagerVarsMatch,EagerVars0,EagerVars),
   (LoutTarget=eager,LoutRest=eager -> Lout=eager ; Lout=lazy).

determine_eager_vars(lazy,lazy,A,[]) :- fullvar(A),!.
determine_eager_vars(eager,eager,A,[A]) :- fullvar(A),!.
determine_eager_vars(eager,eager,[Var|_],[]):- fullvar(Var),!. % avoid binding free var to 'if'
determine_eager_vars(Lin,Lout,['if',If,Then,Else],EagerVars) :- !,
   determine_eager_vars(eager,_,If,EagerVarsIf),
   determine_eager_vars(Lin,LoutThen,Then,EagerVarsThen),
   determine_eager_vars(Lin,LoutElse,Else,EagerVarsElse),
   intersect_var(EagerVarsThen,EagerVarsElse,EagerVars0),
   union_var(EagerVarsIf,EagerVars0,EagerVars),
   (LoutThen=eager,LoutElse=eager -> Lout=eager ; Lout=lazy).
determine_eager_vars(Lin,Lout,['if',If,Then],EagerVars) :- !,
   determine_eager_vars(eager,_,If,EagerVars),
   determine_eager_vars(Lin,Lout,Then,_EagerVarsThen).
% for case, treat it as nested if then else
determine_eager_vars(Lin,Lout,['case',Val,Cases],EagerVars) :- !,
   determine_eager_vars(eager,_,Val,EagerVarsVal),
   determine_eager_vars_case_aux(Lin,Lout,Cases,EagarVarsCases),
   union_var(EagerVarsVal,EagarVarsCases,EagerVars).
determine_eager_vars(Lin,Lout,['let',V,Vbind,Body],EagerVars) :- !,
   determine_eager_vars(eager,eager,Vbind,EagerVarsVbind),
   determine_eager_vars(Lin,Lout,Body,EagerVarsBody),
   union_var([V],EagerVarsVbind,EagerVars0),
   union_var(EagerVars0,EagerVarsBody,EagerVars).
determine_eager_vars(Lin,Lout,['let*',[],Body],EagerVars) :- !,determine_eager_vars(Lin,Lout,Body,EagerVars).
determine_eager_vars(Lin,Lout,['let*',[[V,Vbind]|T],Body],EagerVars) :- !,
   determine_eager_vars(eager,eager,Vbind,EagerVarsVbind),
   determine_eager_vars(Lin,Lout,['let*',T,Body],EagerVarsBody),
   union_var([V],EagerVarsVbind,EagerVars0),
   union_var(EagerVars0,EagerVarsBody,EagerVars).
determine_eager_vars(_,RetLazy,[Fn|Args],EagerVars) :- atom(Fn),!,
   length(Args,LenArgs),
   LenArgsPlus1 is LenArgs+1,
   (transpiler_clause_store(Fn,LenArgsPlus1,_,_,_,ArgsLazy0,RetLazy0,_,_) ->
      maplist(get_property_lazy,ArgsLazy0,ArgsLazy),
      get_property_lazy(RetLazy0,RetLazy)
   ;
      RetLazy=eager,
      length(ArgsLazy, LenArgs),
      maplist(=(eager), ArgsLazy)),
   maplist(determine_eager_vars,ArgsLazy,_,Args,EagerVars0),
   foldl(union_var,EagerVars0,[],EagerVars).
determine_eager_vars(_,eager,A,EagerVars) :- is_list(A),!,
   maplist(determine_eager_vars(eager),_,A,EagerVars0),foldl(union_var,EagerVars0,[],EagerVars).
determine_eager_vars(_,eager,_,[]).

set_eager_or_lazy(Vlist,V,R) :- (member_var(V,Vlist) -> R=eager ; R=lazy).

combine_lazy_types_props(lazy,x(E,lazy),x(E,lazy)) :- !.
combine_lazy_types_props(_,x(E,_),x(E,eager)).

transpiler_stored_eval_lookup(Convert,PrologCode0,Converted0):-
  transpiler_stored_eval(ConvertM,PrologCode0,Converted0),
  ConvertM =@= Convert,ConvertM = Convert,!.

transpile_eval(Convert,Converted) :-
  transpile_eval(Convert,Converted,PrologCode),!,
  call(PrologCode).

transpile_eval(Convert0,Converted,PrologCode) :-
   subst_varnames(Convert0,Convert),
   (transpiler_stored_eval_lookup(Convert,PrologCode0,Converted0) ->
      PrologCode=PrologCode0,
      Converted=Converted0
   ;
      f2p([],[],Converted,eager,Convert,Code),
      ast_to_prolog(no_caller,[],Code,PrologCode),
      compiler_assertz(transpiler_stored_eval(Convert,PrologCode,Converted))
   ).

% !(compile-for-assert (plus1 $x) (+ 1 $x) )
compile_for_assert(HeadIs, AsBodyFn, Converted) :-
 must_det_lls((
   current_self(Space),
   %subst_varnames(HeadIsIn+AsBodyFnIn,HeadIs+AsBodyFn),
   %leash(-all),trace,
   HeadIs=[FnName|Args],
   length(Args,LenArgs),
   LenArgsPlus1 is LenArgs+1,
   atomic_list_concat(['mc_',LenArgs,'__',FnName],FnNameWPrefix),
   ensure_callee_site(Space,FnName,LenArgsPlus1),
   remove_stub(Space,FnName,LenArgsPlus1),
   % retract any stubs
   (transpiler_stub_created(FnName/LenArgsPlus1) ->
      retract(transpiler_stub_created(FnName/LenArgsPlus1)),
      findall(Atom0, (between(1, LenArgsPlus1, I0) ,Atom0='$VAR'(I0)), AtomList0),
      H=..[FnNameWPrefix|AtomList0],
      (transpiler_show_debug_messages -> format("Retracting stub: ~q\n",[H]) ; true),
      retractall(H)
   ; true),
   %AsFunction = HeadIs,
   must_det_lls((
      Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
      get_operator_typedef_props(_,FnName,LenArgs,Types0,RetType0),
      maplist(arg_eval_props,Types0,TypeProps),
      arg_eval_props(RetType0,RetProps),
      determine_eager_vars(lazy,ResultEager,AsBodyFn,EagerArgList),
      maplist(set_eager_or_lazy(EagerArgList),Args,EagerLazyList),
      % EagerLazyList: eager/lazy
      % TypeProps: x(doeval/noeval,eager/lazy)
      % FinalLazyArgs: x(doeval/noeval,eager/lazy)
      maplist(combine_lazy_types_props,EagerLazyList,TypeProps,FinalLazyArgs),
      combine_lazy_types_props(ResultEager,RetProps,FinalLazyRet),

      findall(ClauseIDt,transpiler_clause_store(FnName,LenArgsPlus1,ClauseIDt,_,_,_,_,_,_),ClauseIdList),
      (ClauseIdList=[] ->
         ClauseId=0
      ;
         max_list(ClauseIdList,ClauseIdm1),ClauseId is ClauseIdm1+1
      ),
      compiler_assertz(transpiler_clause_store(FnName,LenArgsPlus1,ClauseId,Types0,RetType0,FinalLazyArgs,FinalLazyRet,HeadIs,AsBodyFn)),
      maplist(arrange_lazy_args,Args,FinalLazyArgs,LazyArgsList),
      get_property_lazy(FinalLazyRet,FinalLazyOnlyRet),

      %precompute_typeinfo(HResult,HeadIs,AsBodyFn,Ast,TypeInfo),

        OldExpr = [defn,HeadIs,AsBodyFn],

        combine_transform_and_collect(OldExpr, Assignments, _NewExpr, VarMappings),

        %writeln("=== Original Expression ==="), print_ast(OldExpr),
        %writeln("=== Assignments (subcalls replaced) ==="), print_ast(Assignments),
        %writeln("=== New Expression ==="), print_ast(NewExpr),
        writeln("=== Assignments / Var Mappings (underscore variables) ==="),
        append(Assignments,VarMappings,SM),sort(SM,S),
        group_pair_by_key(S,SK),
        print_ast(magenta, SK),


      %output_prolog(magenta,TypeInfo),
      %print_ast( green, Ast),

      f2p(HeadIs,LazyArgsList,HResult,FinalLazyOnlyRet,AsBodyFn,NextBody),



      LazyEagerInfo=[resultEager:ResultEager,retProps:RetProps,finalLazyRet:FinalLazyRet,finalLazyOnlyRet:FinalLazyOnlyRet,
                      args_list:Args,lazyArgsList:LazyArgsList,eagerLazyList:EagerLazyList,typeProps:TypeProps,finalLazyArgs:FinalLazyArgs],

       output_prolog(LazyEagerInfo),


      %format("HeadIs:~q HResult:~q AsBodyFn:~q NextBody:~q\n",[HeadIs,HResult,AsBodyFn,NextBody]),
      %(var(HResult) -> (Result = HResult, HHead = Head) ;
      %   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),

      %ast_to_prolog_aux(no_caller,[FnName/LenArgsPlus1],[assign,HResult,[call(FnName)|Args]],HeadC),
      HeadAST=[assign,HResult,[call(FnName)|Args]],
      %ast_to_prolog(no_caller,HeadAST,HeadC),
      append(Args,[HResult],HArgs),
      HeadC =.. [FnNameWPrefix|HArgs],


      print_ast( yellow, [=,HeadAST,NextBody]),


      ast_to_prolog(caller(FnName,LenArgsPlus1),[FnName/LenArgsPlus1],NextBody,NextBodyC),
      %format("###########1 ~q",[Converted]),
      %numbervars(Converted,0,_),
      %format("###########2 ~q",[Converted]),
      extract_constraints(Converted,EC),
      optimize_prolog([],Converted,Optimized),
      output_prolog('#F08080',[EC]),!,
      output_prolog('#ADD8E6',[Converted]),!,
      if_t(Optimized\=@=Converted,
             output_prolog(green,Optimized)),

      tree_deps(Space, FnName, LenArgsPlus1),

      show_recompile(Space, FnName, LenArgsPlus1),
      true
   )))).



no_conflict_numbervars(Term):-
    findall(N,(sub_term_safely(E,Term),compound(E), '$VAR'(N)=E, integer(N)),NL),!,
    max_list([-1|NL],Max),Start is Max + 1,!,
    numbervars(Term,Start,_,[attvar(skip),singletons(true)]).

%compile_for_assert(HeadIs, AsBodyFn, Converted) :-
%   format("compile_for_assert: ~q ~q\n",[HeadIs, AsBodyFn]),
%   HeadIs=[FnName|Args],
%   length(Args,LenArgs),
%   LenArgsPlus1 is LenArgs+1,
%   AsFunction = HeadIs,
%   must_det_lls((
%   Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
%   /*funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),*/
%   f2p(HeadIs,HResult,AsFunction,HHead),
%   (var(HResult) -> (Result = HResult, HHead = Head) ;
%      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
%   %verbose_unify(Converted),
%   f2p(HeadIs,Result,AsBodyFn,NextBody),
%   %RetResult = Converted,
%   %RetResult = _,
%   format("000000 ~q        xxx          ~q 000000\n\n",[Head,NextBody]),
%   optimize_head_and_body(Head,NextBody,HeadC,NextBodyB),
%   format("111111 ~q        xxx           ~q 111111\n\n",[HeadC,NextBodyB]),
%   ast_to_prolog(Caller,[FnName/LenArgsPlus1],NextBodyB,NextBodyC),
%   format("222222 ~q        222222\n\n",[NextBodyC]),
%   %fbug([convert(Convert),head_preconds_into_body(HeadC:-NextBodyC)]),
%   %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),
%   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

% --------------------------------
%    FUNCTS_TO_PREDS EXPLANATION
% --------------------------------

% functs_to_preds is a predicate that converts all Term functions to their equivalent predicates.
% It takes three arguments - RetResult, which will hold the result of the function evaluation,
% Convert, which is the function that needs to be converted, and Converted, which will hold the equivalent predicate.
% Example:
%
%     ?- functs_to_preds(RetResult, is(pi+pi), Converted).
%
%     Converted =  (pi(_A),
%                   +(_A, _A, _B),
%                   _C is _B,
%                   eval_args(_C, RetResult)).
%
functs_to_preds(I,OO):-
   must_det_lls(functs_to_preds0(I,OO)),!.

functs_to_preds0([Eq,H,B],OO):- Eq == '=', compile_for_assert(H, B, OO),!.
functs_to_preds0(EqHB,OO):- compile_head_for_assert(EqHB,OO),!.

functs_to_preds0(I,OO):-
   sexpr_s2p(I, M),
   f2p(_,[],_,_Evaluated,M,O),
   expand_to_hb(O,H,B),
   head_preconds_into_body(H,B,HH,BB),!,
   OO = ':-'(HH,BB).

optimize_head_and_body(Head,Body,HeadNewest,BodyNewest):-
   label_body_singles(Head,Body),
   color_g_mesg('#707084',print_pl_source(( Head :- Body))),
   (merge_and_optimize_head_and_body(Head,Body,HeadNew,BodyNew),
      % iterate to a fixed point
      (((Head,Body)=@=(HeadNew,BodyNew))
      ->  (HeadNew=HeadNewest,BodyNew=BodyNewest)
      ;  optimize_head_and_body(HeadNew,BodyNew,HeadNewest,BodyNewest))).

merge_and_optimize_head_and_body(Head,Converted,HeadO,Body):- nonvar(Head),
   Head = (PreHead,True),!,
   merge_and_optimize_head_and_body(PreHead,(True,Converted),HeadO,Body).
merge_and_optimize_head_and_body(AHead,Body,Head,BodyNew):-
   assertable_head(AHead,Head),
   must_optimize_body(Head,Body,BodyNew).

assertable_head(x_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), atom(F), Head=..[F|NewArgs],!.
assertable_head(Head,Head).

label_body_singles(Head,Body):-
   term_singletons(Body+Head,BodyS),
   maplist(label_body_singles_2(Head),BodyS).
label_body_singles_2(Head,Var):- sub_var_safely(Var,Head),!.
label_body_singles_2(_,Var):- ignore(Var='$VAR'('_')).

must_optimize_body(A,B,CC):- once(optimize_body(A,B,C)), C \=@= B,!, must_optimize_body(A,C,CC).
must_optimize_body(_,B,C):- B =C.

optimize_body(_HB,Body,BodyNew):- is_ftVar(Body),!,Body=BodyNew.
%optimize_body( HB,eval_args(VT,R),eval_args(VT,R)):-!, must_optimize_body(HB,VT,VTT).
optimize_body( HB,with_space(V,T),with_space(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,limit(V,T),limit(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,findall(V,T,R),findall(V,TT,R)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,loonit_assert_source_tf(V,T,R3,R4), loonit_assert_source_tf(V,TT,R3,R4)):-!,
  must_optimize_body(HB,T,TT).

optimize_body( HB,(B1*->B2;B3),(BN1*->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2), optimize_body(HB,B3,BN3).
optimize_body( HB,(B1->B2;B3),(BN1->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), must_optimize_body(HB,B2,BN2), must_optimize_body(HB,B3,BN3).
optimize_body( HB,(B1:-B2),(BN1:-BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1*->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1;B2),(BN1;BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
% TODO FIXME optimize_body( HB,(B1,B2),(BN1)):- optimize_conjuncts(HB,(B1,B2),BN1).
%optimize_body(_HB,==(Var, C), Var=C):- self_eval(C),!.
optimize_body( HB,x_assign(A,B),R):- optimize_x_assign_1(HB,A,B,R),!.
%optimize_body(_HB,x_assign(A,B),x_assign(AA,B)):- p2s(A,AA),!.
optimize_body(_HB,Body,BodyNew):- Body=BodyNew.

/*
optimize_body(_Head,Body,BodyNew):- var(Body),!,Body=BodyNew.
optimize_body(Head,(B1*->B2;B3),(BN1*->BN2;BN3)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2), optimize_body(Head,B3,BN3).
optimize_body(Head,(B1->B2;B3),(BN1->BN2;BN3)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2), optimize_body(Head,B3,BN3).
optimize_body(Head,(B1,B2),(BN1)):- B2==true,!, optimize_body(Head,B1,BN1).
optimize_body(Head,(B2,B1),(BN1)):- B2==true,!, optimize_body(Head,B1,BN1).
optimize_body(Head,(B1,B2),(BN1,BN2)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).
optimize_body(Head,(B1:-B2),(BN1:-BN2)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).
optimize_body(Head,(B1;B2),(BN1;BN2)):-!, optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).
optimize_body(_Head,Body,BodyNew):- Body=BodyNew.
*/

optimize_x_assign_1(_,Var,_,_):- is_ftVar(Var),!,fail.
optimize_x_assign_1(HB,Compound,R,Code):- \+ compound(Compound),!, optimize_x_assign(HB,Compound,R,Code).
optimize_x_assign_1(HB,[H|T],R,Code):- !, optimize_x_assign(HB,[H|T],R,Code).
optimize_x_assign_1(HB,Compound,R,Code):- p2s(Compound,MeTTa),   optimize_x_assign(HB,MeTTa,R,Code).
%optimize_x_assign_1(_,[Pred| ArgsL], R, x_assign([Pred| ArgsL],R)).

optimize_x_assign(_,[Var|_],_,_):- is_ftVar(Var),!,fail.
optimize_x_assign(_,[Empty], _, (!,fail)):-  Empty == empty,!.
optimize_x_assign(_,[+, A, B], C, plus(A , B, C)):- number_wang(A,B,C), !.
optimize_x_assign(_,[-, A, B], C, plus(B , C, A)):- number_wang(A,B,C), !.
%optimize_x_assign(_,[+, A, B], C, +(A , B, C)):- !.
%optimize_x_assign(_,[-, A, B], C, +(B , C, A)):- !.
optimize_x_assign(_,[*, A, B], C, *(A , B, C)):- number_wang(A,B,C), !.
optimize_x_assign(_,['/', A, B], C, *(B , C, A)):- number_wang(A,B,C), !.
%optimize_x_assign(_,[*, A, B], C, *(A , B, C)):- !.
%optimize_x_assign(_,['/', A, B], C, *(B , C, A)):- !.
%optimize_x_assign(_,[fib, B], C, fib(B, C)):- !.
%optimize_x_assign(_,[fib1, A,B,C,D], R, fib1(A, B, C, D, R)):- !.
optimize_x_assign(_,['pragma!',N,V],Empty,set_option_value_interp(N,V)):-
   nonvar(N),ignore((fail,Empty='Empty')), !.
optimize_x_assign((H:-_),Filter,A,filter_head_arg(A,Filter)):- fail, compound(H), arg(_,H,HV),
  HV==A, is_list(Filter),!.
%optimize_x_assign(_,[+, A, B], C, '#='(C , A + B)):- number_wang(A,B,C), !.
%optimize_x_assign(_,[-, A, B], C, '#='(C , A - B)):- number_wang(A,B,C), !.
optimize_x_assign(_,[match,KB,Query,Template], R, Code):-  match(KB,Query,Template,R) = Code.

optimize_x_assign(HB,MeTTaEvalP, R, Code):- \+ is_ftVar(MeTTaEvalP),
  compound_non_cons(MeTTaEvalP), p2s(MeTTaEvalP,MeTTa),
  MeTTa\=@=MeTTaEvalP,!, optimize_body(HB, x_assign(MeTTa, R), Code).

% optimize_x_assign(_,_,_,_):- !,fail.
optimize_x_assign((H:-_),[Pred| ArgsL], R, Code):- var(R), atom(Pred), ok_to_append(Pred),
  append([Pred| ArgsL],[R], PrednArgs),Code=..PrednArgs,
  (H=..[Pred|_] -> nop(set_option_value('tabling',true)) ; current_predicate(_,Code)),!.

number_wang(A,B,C):-
  (numeric(C);numeric(A);numeric(B)),!,
  maplist(numeric_or_var,[A,B,C]),
  maplist(decl_numeric,[A,B,C]),!.


data_term(Convert):- fullvar(Convert),!,fail.
data_term('&self').
data_term(Convert):- self_eval(Convert),!.

into_equals(RetResultL,RetResult,Equals):- into_x_assign(RetResultL,RetResult,Equals).

into_x_assign(RetResultL,RetResult,true):- is_ftVar(RetResultL), is_ftVar(RetResult), RetResult=RetResultL,!.
into_x_assign(RetResultL,RetResult,Code):- var(RetResultL), Code = x_assign(RetResult,RetResultL).
into_x_assign(RetResultL,RetResult,Code):- Code = x_assign(RetResultL,RetResult).

numeric(N):- number(N),!.
numeric(N):- get_attr(N,'Number','Number').
numeric(N):- get_decl_type(N,DT),(DT=='Int',DT=='Number').

decl_numeric(N):- numeric(N),!.
decl_numeric(N):- ignore((var(N),put_attr(N,'Number','Number'))).

numeric_or_var(N):- var(N),!.
numeric_or_var(N):- numeric(N),!.
numeric_or_var(N):- \+ compound(N),!,fail.
numeric_or_var('$VAR'(_)).

get_decl_type(N,DT):- attvar(N),get_atts(N,AV),sub_term_safely(DT,AV),atom(DT).

fullvar(V) :- var(V), !.
fullvar('$VAR'(_)).


ast_to_prolog(Caller,DontStub,A,Result) :-
   maplist(ast_to_prolog_aux(Caller,DontStub),A,B),
   combine_code_list(B,Result).

ast_to_prolog_aux(_,_,A,A) :- fullvar(A),!.
ast_to_prolog_aux(_,_,H,H):- \+ compound(H),!.
ast_to_prolog_aux(Caller,DontStub,list(A),B) :- !,maplist(ast_to_prolog_aux(Caller,DontStub),A,B).
ast_to_prolog_aux(_,_,[Var|Rest],[Var|Rest]):- fullvar(Var),!.
ast_to_prolog_aux(Caller,DontStub,[prolog_if,If,Then,Else],R) :- !,
   ast_to_prolog(Caller,DontStub,If,If2),
   ast_to_prolog(Caller,DontStub,Then,Then2),
   ast_to_prolog(Caller,DontStub,Else,Else2),
   R=((If2) *-> (Then2);(Else2)).
ast_to_prolog_aux(Caller,DontStub,[is_p1,Code0,R],is_p1(Code1,R)) :- !,ast_to_prolog(Caller,DontStub,Code0,Code1).
ast_to_prolog_aux(Caller,DontStub,[is_p1,Type,Src,Code0,R],is_p1(Type,Src,Code1,R)) :- !,ast_to_prolog(Caller,DontStub,Code0,Code1).
ast_to_prolog_aux(Caller,DontStub,[native(FIn)|ArgsIn],A) :- !,
 must_det_lls((
   FIn=..[F|Pre], % allow compound natives
   append(Pre,ArgsIn,Args0),
   label_arg_types(F,1,Args0),
   maplist(ast_to_prolog_aux(Caller,DontStub),Args0,Args1),
   label_arg_types(F,1,Args1),
   A=..[F|Args1],
   notice_callee(Caller,A))).
ast_to_prolog_aux(Caller,DontStub,[assign,A,[call(FIn)|ArgsIn]],R) :- (fullvar(A); \+ compound(A)),callable(FIn),!,
 must_det_lls((
   FIn=..[F|Pre], % allow compound natives
   append(Pre,ArgsIn,Args00),
   maybe_lazy_list(Caller,F,1,Args00,Args0),
   label_arg_types(F,1,Args0),
   maplist(ast_to_prolog_aux(Caller,DontStub),Args0,Args1),
   length(Args0,LArgs),
   atomic_list_concat(['mc_',LArgs,'__',F],Fp),
   label_arg_types(F,0,[A|Args1]),
   LArgs1 is LArgs+1,
   append(Args1,[A],Args2),
   R=..[Fp|Args2],
   (Caller=caller(CallerInt,CallerSz),(CallerInt-CallerSz)\=(F-LArgs1),\+ transpiler_depends_on(CallerInt,CallerSz,F,LArgs1) ->
      assertz(transpiler_depends_on(CallerInt,CallerSz,F,LArgs1)),
      (transpiler_show_debug_messages -> format("Asserting: transpiler_depends_on(~q,~q,~q,~q)\n",[CallerInt,CallerSz,F,LArgs1]) ; true)
   ; true),
   ((current_predicate(Fp/LArgs1);member(F/LArgs1,DontStub)) ->
      true
   ; check_supporting_predicates('&self',F/LArgs1)),
   notice_callee(Caller,F/LArgs1))).

ast_to_prolog_aux(Caller,DontStub,[assign,A,X0],(A=X1)) :-   must_det_lls(label_type_assignment(A,X0)), ast_to_prolog_aux(Caller,DontStub,X0,X1),label_type_assignment(A,X1),!.
ast_to_prolog_aux(Caller,DontStub,[prolog_match,A,X0],(A=X1)) :- ast_to_prolog_aux(Caller,DontStub,X0,X1),!.

ast_to_prolog_aux(Caller,DontStub,[prolog_catch,Catch,Ex,Catcher],R) :-  ast_to_prolog(Caller,DontStub,Catch,Catch2), R=  catch(Catch2,Ex,Catcher).
ast_to_prolog_aux(_Caller,_DontStub,[prolog_inline,Prolog],R) :- !, R= Prolog.
ast_to_prolog_aux(Caller, DontStub, if_or_else(If,Else),R):-
  ast_to_prolog_aux(Caller, DontStub, (If*->true;Else),R).
ast_to_prolog_aux(Caller, DontStub, Smack,R):-
               compound(Smack),
               Smack=..[NSF, _,_AnyRet, Six66,_Self, FArgs,Ret],
               (NSF = eval_args;NSF = eval_20),
               \+ atom_concat(find,_,NSF),
               \+ atom_concat(_,e,NSF),
               Six66 == 666,
    ast_to_prolog_aux(Caller,DontStub,eval(FArgs,Ret),R).
ast_to_prolog_aux(Caller,DontStub, eval([F|Args],Ret),R):- atom(F),is_list(Args),
   ast_to_prolog_aux(Caller,DontStub,[assign,Ret,[call(F),Args]],R), !.

ast_to_prolog_aux(_,_,'#\\'(A),A).

%ast_to_prolog_aux(_,_,A=B,A=B):- must_det_lls(label_type_assignment(A,B)).



ast_to_prolog_aux(Caller,DontStub,(True,T),R) :- True == true, ast_to_prolog_aux(Caller,DontStub,T,R).
ast_to_prolog_aux(Caller,DontStub,(T,True),R) :- True == true, ast_to_prolog_aux(Caller,DontStub,T,R).
ast_to_prolog_aux(Caller,DontStub,(H;T),(HH;TT)) :- ast_to_prolog_aux(Caller,DontStub,H,HH),ast_to_prolog_aux(Caller,DontStub,T,TT).
ast_to_prolog_aux(Caller,DontStub,(H,T),(HH,TT)) :- ast_to_prolog_aux(Caller,DontStub,H,HH),ast_to_prolog_aux(Caller,DontStub,T,TT).
ast_to_prolog_aux(Caller,DontStub,do_metta_runtime(T,G),do_metta_runtime(T,GGG)) :- !, ast_to_prolog_aux(Caller,DontStub,G,GG),combine_code(GG,GGG).
ast_to_prolog_aux(Caller,DontStub,loonit_assert_source_tf(T,G),loonit_assert_source_tf(T,GG)) :- !, ast_to_prolog_aux(Caller,DontStub,G,GG).
ast_to_prolog_aux(Caller,DontStub,findall(T,G,L),findall(T,GG,L)) :- !, ast_to_prolog_aux(Caller,DontStub,G,GG).
ast_to_prolog_aux(Caller,DontStub,FArgs,NewFArgs):- 
   \+ is_list(FArgs), 
   compound(FArgs),!,
   compound_name_arguments(FArgs, Name, Args),
   maplist(ast_to_prolog_aux(Caller,DontStub),Args,NewArgs),
   compound_name_arguments(NewCompound, Name, NewArgs),NewFArgs=NewCompound.  


%ast_to_prolog_aux(Caller,DontStub,[H],HH) :- ast_to_prolog_aux(Caller,DontStub,H,HH).
%ast_to_prolog_aux(Caller,DontStub,[H|T],(HH,TT)) :- ast_to_prolog_aux(Caller,DontStub,H,HH),ast_to_prolog_aux(Caller,DontStub,T,TT).

ast_to_prolog_aux(_,_,A,A).

combine_code_list(A,R) :- !,
   combine_code_list_aux(A,R0),
   (R0=[] -> R=true
   ; R0=[R1] -> R=R1
   ; R0=[H|T],
      combine_code_list(T,T0),
      R=..[',',H,T0]).

combine_code_list_aux([],[]).
combine_code_list_aux([true|T],R) :- !,combine_code_list_aux(T,R).
combine_code_list_aux([H|T],R) :- H=..[','|H0],!,append(H0,T,T0),combine_code_list_aux(T0,R).
combine_code_list_aux([H|T],[H|R]) :- combine_code_list_aux(T,R).


check_supporting_predicates(Space,F/A) :- % already exists
   A1 is A-1,
   atomic_list_concat(['mc_',A1,'__',F],Fp),
   with_mutex(transpiler_mutex_lock,
      (current_predicate(Fp/A) -> true ;
         findall(Atom0, (between(1, A, I0) ,Atom0='$VAR'(I0)), AtomList0),
         H=..[Fp|AtomList0],
         Am1 is A-1,
         findall(Atom1, (between(1, Am1, I1), Atom1='$VAR'(I1)), AtomList1),
         B=..[u_assign,[F|AtomList1],'$VAR'(A)],
%         (transpiler_enable_interpreter_calls -> G=true;G=fail),
%         assertz(transpiler_stub_created(F/A)),
%         create_and_consult_temp_file(Space,Fp/A,[H:-(format("; % ######### warning: using stub for:~q\n",[F]),G,B)]))).
         assertz(transpiler_stub_created(F/A)),
         (transpiler_show_debug_messages -> format("; % ######### warning: creating stub for:~q\n",[F]) ; true),
         (transpiler_enable_interpreter_calls ->
            create_and_consult_temp_file(Space,Fp/A,[H:-(format("; % ######### warning: using stub for:~q\n",[F]),B)])
         ;
            create_and_consult_temp_file(Space,Fp/A,[H:-('$VAR'(A)=[F|AtomList1])])
         )
      )
   ).


create_and_consult_temp_file(_Space,F/A,PredClauses):- fail,!,
    abolish(F/A),maplist(compiler_assertz,PredClauses).

create_and_consult_temp_file(Space,F/A,PredClauses):- fail,
        must_det_lls((
        %1)Createthememoryfilehandle
        new_memory_file(MemFile),

        %2)Openthememoryfileforwriting
        open_memory_file(MemFile,write,TempStream),

        %Writethetabledpredicatetothememoryfile
        format(TempStream,':-multifile((~q)/~q).~n',[metta_compiled_predicate,3]),
        format(TempStream,':-dynamic((~q)/~q).~n',[metta_compiled_predicate,3]),
        format(TempStream,'~N~q.~n',[metta_compiled_predicate(Space,F,A)]),

        format(TempStream,':-multifile((~q)/~q).~n',[F,A]),
        format(TempStream,':-dynamic((~q)/~q).~n',[F,A]),

        %Iftablingisturnedon:
        if_t(
        option_value('tabling','True'),
        format(TempStream,':-~q.~n',[table(F/A)])
    ),

    %Writeeachclause
    maplist(write_clause(TempStream),PredClauses),

    %Closethewritestream
    close(TempStream),

    %3)Openthememoryfileforreading
    open_memory_file(MemFile,read,ConsultStream),


    %4)Consultorloadtheclausesfromthememorystream
    %IfyourPrologsupportsconsult/1onastream,youcoulddo:
    %consult(ConsultStream).
    %Otherwise,useload_files/2withstream/1:
    load_files(user,[stream(ConsultStream)]),

    %Closethereadstream
    close(ConsultStream),

    %5)Freethememoryfile(noneedforon-diskcleanup)
    free_memory_file(MemFile),

    %Confirmthepredicateispresent
    current_predicate(F/A)
    )),!.

% Predicate to create a temporary file and write the tabled predicate
create_and_consult_temp_file(Space,F/A, PredClauses) :-
  must_det_lls((
    % Generate a unique temporary memory buffer
    tmp_file_stream(text, TempFileName, TempFileStream),
    % Write the tabled predicate to the temporary file
    format(TempFileStream, ':- multifile((~q)/~q).~n', [metta_compiled_predicate, 3]),
    format(TempFileStream, ':- dynamic((~q)/~q).~n', [metta_compiled_predicate, 3]),
    format(TempFileStream, '~N~q.~n',[metta_compiled_predicate(Space,F,A)]),

    format(TempFileStream, ':- multifile((~q)/~q).~n', [F, A]),
    format(TempFileStream, ':- dynamic((~q)/~q).~n', [F, A]),
    %if_t( \+ option_value('tabling',false),
    if_t(option_value('tabling','True'),format(TempFileStream,':- ~q.~n',[table(F/A)])),
    maplist(write_clause(TempFileStream), PredClauses),
    % Close the temporary file
    close(TempFileStream),
    % Consult the temporary file
    % abolish(F/A),
    /*'&self':*/
    % sformat(CAT,'cat ~q',[TempFileName]), shell(CAT),
    consult(TempFileName),

    % listing(F/A),
    % Delete the temporary file after consulting
    %delete_file(TempFileName),
    current_predicate(F/A),
    %listing(metta_compiled_predicate/3),
    true)).


write_to_streams(StreamList, Format, Args) :-
    % Write to each stream in the list
    forall(member(Stream, StreamList),
           format(Stream, Format, Args)),
    % Write to stdout
    format(user_output, Format, Args),
    flush_output(user_output). % Ensure output is displayed immediately


%metta_compiled_predicate(_,F,A):- metta_compiled_predicate(F,A).

% Helper predicate to write a clause to the file
write_clause(Stream, Clause) :-
    subst_vars(Clause,Can),
    write_canonical(Stream, Can),
    write(Stream, '.'),
    nl(Stream).

u_assign(FList,R):- is_list(FList),!,eval_args(FList,R).
u_assign(FList,R):- var(FList),nonvar(R), !, u_assign(R,FList).
u_assign(FList,R):- FList=@=R,!,FList=R.
u_assign(FList,R):- number(FList), var(R),!,R=FList.
u_assign(FList,R):- self_eval(FList), var(R),!,R=FList.
u_assign(FList,R):- var(FList),!,freeze(FList,u_assign(FList,R)).
u_assign(FList,R):- \+ compound(FList), var(R),!,R=FList.
u_assign([F|List],R):- F == ':-',!, trace_break,as_tf(clause(F,List),R).
u_assign(FList,RR):- (compound_non_cons(FList),u_assign_c(FList,RR))*->true;FList=~RR.
u_assign(FList,RR):-
  u_assign_list1(FList,RR)*->true;u_assign_list2(FList,RR).

u_assign_list1([F|List],R):- eval_args([F|List],R), nonvar(R), R\=@=[F|List].
u_assign_list2([F|List],R):- atom(F),append(List,[R],ListR),
  catch(quietly(apply(F,ListR)),error(existence_error(procedure,F/_),_),
     catch(quietly(as_tf(apply(F,List),R)),error(existence_error(procedure,F/_),_),
        quietly(catch(eval_args([F|List],R),_, R=[F|List])))).

%u_assign([V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(_Eq,V),!,maplist(eval_args,VI,VO).

u_assign_c((F:-List),R):- !, R = (F:-List).

/*
u_assign_c(Cmp,RR):-
  functor(Cmp,F,_),
  current_predicate(F,_),
  debug(todo,'u_assign_c INTERP: ~q',[Cmp]),!,
  call(Cmp,RR).*/
u_assign_c(FList,RR):-
  functor(FList,F,_), % (F == 'car-atom' -> trace ; true),
  (catch(quietlY(call(FList,R)),error(existence_error(procedure,F/_),_),
     catch(quietlY(as_tf(FList,R)),error(existence_error(procedure,F/_),_),
      ((p2m(FList,[F0|List0]),catch(eval_args([F0|List0],R),_, R=~[F0|List0])))))),!,
         R=RR.
u_assign_c(FList,RR):- as_tf(FList,RR),!.
u_assign_c(FList,R):- compound(FList), !, FList=~R.

quietlY(G):- call(G).

var_prop_lookup(_,[],eager).
var_prop_lookup(X,[H-R|T],S) :-
   X == H,S=R;  % Test if X and H are the same variable
   var_prop_lookup(X,T,S).  % Recursively check the tail of the list

:- discontiguous f2p/6.

f2p(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
   is_ftVar(Convert),!, % Check if Convert is a variable
   var_prop_lookup(Convert,LazyVars,L),
   lazy_impedance_match(L,ResultLazy,Convert,[],RetResult,Converted).

f2p(_HeadIs, _LazyVars, RetResult, ResultLazy, '#\\'(Convert), Converted) :-
   (ResultLazy=eager ->
      RetResult=Convert,
      Converted=[]
   ;  Converted=[assign,RetResult,[is_p1,'Char','#\\'(Convert),[],Convert]]).

% If Convert is a number or an atom, it is considered as already converted.
f2p(_HeadIs, _LazyVars, RetResult, _ResultLazy, Convert, Converted) :-
    once(number(Convert); atomic(Convert); \+ compound(Convert); data_term(Convert)),
    %(ResultLazy=eager -> C2=Convert ; C2=[is_p1,[],Convert]),
    %Converted=[[assign,RetResult,C2]],
    RetResult=Convert, Converted=[],
    % For OVER-REACHING categorization of dataobjs %
    % wdmsg(data_term(Convert)),
    %trace_break,
    !.  % Set RetResult to Convert as it is already in predicate form

% If Convert is a number or an atom, it is considered as already converted.
f2p(_HeadIs, _LazyVars, RetResult, ResultLazy, Convert, Converted) :- % HeadIs\=@=Convert,
    %once(number(Convert); atom(Convert); data_term(Convert)),  % Check if Convert is a number or an atom
    once(number(Convert); atomic(Convert); \+compound(Convert); data_term(Convert)),
    must_det_lls(get_val_types(Convert,Types)),
    (ResultLazy=eager -> C2=Convert ; C2=[is_p1,Types,Convert,[],Convert]),
    Converted=[[assign,RetResult,C2]],
    % For OVER-REACHING categorization of dataobjs %
    % wdmsg(data_term(Convert)),
    %trace_break,
    !.  % Set RetResult to Convert as it is already in predicate form

f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted):-
   Convert=[Fn|_],
   atom(Fn),
   compile_flow_control(HeadIs,LazyVars,RetResult,ResultLazy, Convert, Converted),!.

% !(compile-body! (call-fn! compile_body (call-p writeln "666"))
f2p(HeadIs, _LazyVars, RetResult, ResultLazy, Convert, Converted) :- HeadIs\=@=Convert,
    Convert=[Fn,Native|Args],atom(Fn),unshebang(Fn,'call-p'),!,
   must_det_lls((
    compile_maplist_p2(as_prolog,Args,NewArgs,PreCode),
    %RetResult = 'True',
    compile_maplist_p2(from_prolog_args(ResultLazy),NewArgs,Args,PostCode),
    append([PreCode,[[native(Native),NewArgs],[assign,RetResult,'True']],PostCode],Converted))).
unshebang(S,US):- symbol(S),(symbol_concat(US,'!',S)->true;US=S).

compile_maplist_p2(_,[],[],[]).
compile_maplist_p2(P2,[Var|Args],[Res|NewArgs],PreCode):- \+ fullvar(Var), call(P2,Var,Res), !,
  compile_maplist_p2(P2,Args,NewArgs,PreCode).
compile_maplist_p2(P2,[Var|Args],[Res|NewArgs],TheCode):-
  compile_maplist_p2(P2,Args,NewArgs,PreCode),
  append([[native(P2),Var,Res]],PreCode,TheCode).

% !(compile-body! (call-fn length $list))
f2p(HeadIs, _LazyVars, RetResult, ResultLazy, Convert, Converted) :-  HeadIs\=@=Convert,
    Convert=[Fn,Native|Args],atom(Fn),unshebang(Fn,'call-fn'),!,
    compile_maplist_p2(as_prolog,Args,NewArgs,PreCode),
    append(NewArgs,[Result],CallArgs),
    compile_maplist_p2(from_prolog_args(maybe(ResultLazy)),[Result],[RetResult],PostCode),
    append([PreCode,[[native(Native),CallArgs]],PostCode],Converted).

% !(compile-body! (call-fn-nth 0 wots version))
f2p(HeadIs, _LazyVars, RetResult, ResultLazy, Convert, Converted) :- HeadIs\=@=Convert,
   Convert=[Fn,Nth,Native|SIn],atom(Fn),unshebang(Fn,'call-fn-nth'),integer(Nth),!,
   compile_maplist_p2(as_prolog,SIn,S,PreCode),
   length(Left,Nth),
   append(Left,Right,S),
   append(Left,[R|Right],Args),!,
    compile_maplist_p2(from_prolog_args(maybe(ResultLazy)),[R],[RetResult],PostCode),
    append([PreCode,[[native(Native),Args]],PostCode],Converted).

% !(compile-body! (length-p (a b c d) 4))
% !(compile-body! (format! "~q ~q ~q" (a b c)))
f2p(HeadIs, _LazyVars, RetResult, ResultLazy, Convert, Converted) :-  HeadIs\=@=Convert,
    is_host_predicate(Convert,Native,_Len),!,Convert=[_|Args],
    compile_maplist_p2(as_prolog,Args,NewArgs,PreCode),
    %RetResult = 'True',
    compile_maplist_p2(from_prolog_args(maybe(ResultLazy)),NewArgs,Args,PostCode),
    append([PreCode,[[native(Native),NewArgs],[assign,RetResult,'True']],PostCode],Converted).


% !(compile-body! (length-fn (a b c d)))
f2p(HeadIs, _LazyVars, RetResult, ResultLazy, Convert, Converted) :-  HeadIs\=@=Convert,
    Convert=[Fn|Args],
    is_host_function([Fn|Args],Native,_Len),!,
    compile_maplist_p2(as_prolog,Args,NewArgs,PreCode),
    append(NewArgs,[Result],CallArgs),
    compile_maplist_p2(from_prolog_args(maybe(ResultLazy)),[Result],[RetResult],PostCode),
    append([PreCode,[[native(Native),CallArgs]],PostCode],Converted).


f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- HeadIs\=@=Convert,
   Convert=[Fn|_], \+ atom(Fn),
    Args = Convert,
    length(Args, N),
    % create an eval-args list. TODO FIXME revisit this after working out how lists handle evaluation
    length(EvalArgs, N),
    maplist(=(eager), EvalArgs),
    maplist(f2p(HeadIs, LazyVars),NewArgs, EvalArgs, Args, NewCodes),
    append(NewCodes,CombinedNewCode),
    Code=[assign,RetResult0,list(NewArgs)],
    append(CombinedNewCode,[Code],Converted0),
    lazy_impedance_match(eager,ResultLazy,RetResult0,Converted0,RetResult,Converted).

update_laziness(x(X,_),x(_,Y),x(X,Y)).

% prememptive flow contols
f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted):- fail,
   Convert=[Fn|_],
   atom(Fn),
   compile_flow_control1(HeadIs,LazyVars,RetResult,ResultLazy, Convert, Converted),!.

% unsupported flow contols
f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted):- fail,
   Convert=[Fn|_],
   atom(Fn),
   compile_flow_control2(HeadIs,LazyVars,RetResult,ResultLazy, Convert, Converted),!.

f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- HeadIs\=@=Convert,
   Convert=[Fn|Args],
   atom(Fn),!,
   length(Args,Largs),
   LenArgsPlus1 is Largs+1,
   (transpiler_clause_store(Fn,LenArgsPlus1,_,_,_,ArgsLazy0,x(_,RetLazy0),_,_) ->
      UpToDateArgsLazy=ArgsLazy0,
      RetLazy=RetLazy0
   ;
      RetLazy=eager,
      length(UpToDateArgsLazy, Largs),
      maplist(=(x(doeval,eager)), UpToDateArgsLazy)),
   % get the evaluation/laziness based on the types, but then update from the actual signature using 'update_laziness'
   get_operator_typedef_props(_,Fn,Largs,Types0,_RetType0),
   maplist(arg_eval_props,Types0,EvalArgs0),
   maplist(update_laziness,EvalArgs0,UpToDateArgsLazy,EvalArgs),
   maplist(do_arg_eval(HeadIs,LazyVars),Args,EvalArgs,NewArgs,NewCodes),
   append(NewCodes,CombinedNewCode),
   Code=[assign,RetResult0,[call(Fn)|NewArgs]],
   append(CombinedNewCode,[Code],Converted0),
   lazy_impedance_match(RetLazy,ResultLazy,RetResult0,Converted0,RetResult,Converted).

f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted):- fail,
   Convert=[Fn|_],
   atom(Fn),
   compile_flow_control3(HeadIs,LazyVars,RetResult,ResultLazy, Convert, Converted),!.


% The catch-all If no specific case is matched, consider Convert as already converted.
%f2p(_HeadIs, LazyVars, _RetResult, ResultLazy, x_assign(Convert,Res), x_assign(Convert,Res)):- !.
%f2p(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, Code):- into_x_assign(Convert,RetResult,Code).

%f2p(HeadIs, LazyVars,  list(Convert), ResultLazy,  Convert, []) :- trace,HeadIs\=@=Convert,
%   is_list(Convert),!.
f2p(HeadIs, LazyVars, list(Converted), _ResultLazy, Convert, Codes) :- %HeadIs\=@=Convert,
   is_list(Convert),!,
   length(Convert, N),
   % create an eval-args list. TODO FIXME revisit this after working out how lists handle evaluation
   % such as maplist(=(ResultLazy), EvalArgs), 
   length(EvalArgs, N),
   maplist(=(eager), EvalArgs),
   maplist(f2p_skip_atom(HeadIs, LazyVars),Converted,EvalArgs,Convert,Allcodes),
   append(Allcodes,Codes).

f2p_skip_atom(_HeadIs, _LazyVars,Converted, _EvalArgs, Convert,true):-
  \+ compound(Convert), !, Converted = Convert.
f2p_skip_atom(HeadIs, LazyVars,Converted,EvalArgs,Convert,Allcodes):-
   f2p(HeadIs, LazyVars,Converted,EvalArgs,Convert,Allcodes).


f2p(HeadIs,LazyVars,_RetResult,EvalArgs,Convert, Code):-
   format(user_error,"Error in f2p ~q ~q ~q ~q\n",[HeadIs,LazyVars,Convert,EvalArgs]),
   user_io(print_ast(Convert)),
   trace, throw(0),
   Code=Convert.

lazy_impedance_match(L,L,RetResult0,Converted0,RetResult0,Converted0).
lazy_impedance_match(lazy,eager,RetResult0,Converted0,RetResult,Converted) :-
   append(Converted0,[[native(as_p1),RetResult0,RetResult]],Converted).
lazy_impedance_match(eager,lazy,RetResult0,Converted0,RetResult,Converted) :-
   append(Converted0,[[assign,RetResult,[is_p1,_Type,Converted0,[],RetResult0]]],Converted).

arg_eval_props('Number',x(doeval,eager)) :- !.
arg_eval_props('Bool',x(doeval,eager)) :- !.
arg_eval_props('LazyBool',x(doeval,lazy)) :- !.
arg_eval_props('Any',x(doeval,eager)) :- !.
arg_eval_props('Atom',x(doeval,lazy)) :- !.
arg_eval_props('Expression',x(doeval,lazy)) :- !.
arg_eval_props(_,x(doeval,eager)).

do_arg_eval(_,_,Arg,x(noeval,_),Arg,[]).
do_arg_eval(HeadIs,LazyVars,Arg,x(doeval,lazy),[is_p1,_Type,Arg,SubCode,SubArg],Code) :-
   f2p(HeadIs,LazyVars,SubArg,eager,Arg,SubCode),
   Code=[].
do_arg_eval(HeadIs,LazyVars,Arg,x(doeval,eager),NewArg,Code) :- f2p(HeadIs,LazyVars,NewArg,eager,Arg,Code).

:- discontiguous(compile_flow_control/6).
:- discontiguous(compile_flow_control3/6).
:- discontiguous(compile_flow_control2/6).
:- discontiguous(compile_flow_control1/6).



add_assignment(A,B,CodeOld,CodeNew) :-
   (fullvar(A),var(B) ->
      B=A,CodeNew=CodeOld
   ; var(A),fullvar(B) ->
      A=B,CodeNew=CodeOld
   ;  append(CodeOld,[[assign,A,B]],CodeNew)).

compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
   Convert=['case',Value,Cases],!,
   f2p(HeadIs,LazyVars,ValueResult,eager,Value,ValueCode),
   compile_flow_control_case(HeadIs,LazyVars,RetResult,LazyEval,ValueResult,Cases,Converted0),
   append(ValueCode,Converted0,Converted).

compile_flow_control_case(_,_,RetResult,_,_,[],Converted) :- !,Converted=[[assign,RetResult,'Empty']].
compile_flow_control_case(HeadIs,LazyVars,RetResult,LazyEval,ValueResult,[[Match,Target]|Rest],Converted) :-
   f2p(HeadIs,LazyVars,MatchResult,eager,Match,MatchCode),
   f2p(HeadIs,LazyVars,TargetResult,LazyEval,Target,TargetCode),
   compile_flow_control_case(HeadIs,LazyVars,RestResult,LazyEval,ValueResult,Rest,RestCode),
   append(TargetCode,[[assign,RetResult,TargetResult]],T),
   append(RestCode,[[assign,RetResult,RestResult]],R),
   append(MatchCode,[[prolog_if,[[prolog_match,ValueResult,MatchResult]],T,R]],Converted).

/*
compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
  Convert = ['case', Eval, CaseList],!,
  f2p(HeadIs, LazyVars, Var, eager, Eval, CodeCanFail),
  case_list_to_if_list(Var, CaseList, IfList, [empty], IfEvalFails),
  compile_test_then_else(RetResult, LazyVars, LazyEval, CodeCanFail, IfList, IfEvalFails, Converted).

case_list_to_if_list(_Var, [], [empty], EvalFailed, EvalFailed) :-!.
case_list_to_if_list(Var, [[Pattern, Result] | Tail], Next, _Empty, EvalFailed) :-
    (Pattern=='Empty'; Pattern=='%void%'), !, % if the case Failed
    case_list_to_if_list(Var, Tail, Next, Result, EvalFailed).
case_list_to_if_list(Var, [[Pattern, Result] | Tail], Out, IfEvalFailed, EvalFailed) :-
    case_list_to_if_list(Var, Tail, Next, IfEvalFailed, EvalFailed),
    Out = ['if', [metta_unify, Var, Pattern], Result, Next].
*/

% !(compile-body! (function 1))
% !(compile-body! (function (throw 1)))
% !(compile-body! (superpose ((throw 1) (throw 2))))
compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
  (Convert =~ ['function', ['return', Body]] ; Convert =~ ['function', Body]),!,
  f2p(HeadIs,LazyVars,RetResult,LazyEval,Body,BodyCode),
  Converted = [[prolog_catch,BodyCode,metta_return(FunctionResult),FunctionResult=RetResult]].

compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
  Convert =~ ['return', Body],!,
  f2p(HeadIs,LazyVars,RetResult,LazyEval,Body,BodyCode),
  append(BodyCode,[[prolog_inline,throw(metta_return(RetResult))]],Converted).

compile_flow_control(HeadIs, LazyVars, RetResult, ResultLazy, Convert, CodeForSrc) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['eval', Src],
   f2p(HeadIs, LazyVars, RetResult, ResultLazy, Src, CodeForSrc).

compile_flow_control(HeadIs, LazyVars, RetResult, ResultLazy, Convert, (CodeForSpace,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['evalc', Src, Space],
   f2p(HeadIs, LazyVars, ResSpace,  ResultLazy, Space,CodeForSpace),
   f2p(HeadIs, LazyVars, RetResult, ResultLazy,   Src,CodeForSrc),
   Converted = with_space(ResSpace,CodeForSrc).

compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
  Convert = ['if',Cond,Then,Else],!,
  %Test = is_True(CondResult),
  f2p(HeadIs,LazyVars,CondResult,eager,Cond,CondCode),
  append(CondCode,[[native(is_True),CondResult]],If),
  compile_test_then_else(RetResult,LazyVars,LazyEval,If,Then,Else,Converted).

compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
  Convert =~ ['if',Cond,Then],!,
  %Test = is_True(CondResult),
  f2p(HeadIs,LazyVars,CondResult,eager,Cond,CondCode),
  append(CondCode,[[native(is_True),CondResult]],If),
  compile_test_then_else(RetResult,LazyVars,LazyEval,If,Then,'Empty',Converted).

compile_test_then_else(RetResult,LazyVars,LazyEval,If,Then,Else,Converted):-
  f2p(HeadIs,LazyVars,ThenResult,LazyEval,Then,ThenCode),
  f2p(HeadIs,LazyVars,ElseResult,LazyEval,Else,ElseCode),
  % cannnot use add_assignment here as might not want to unify ThenResult and ElseResult
  append(ThenCode,[[assign,RetResult,ThenResult]],T),
  append(ElseCode,[[assign,RetResult,ElseResult]],E),
  Converted=[[prolog_if,If,T,E]].

compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert = ['let',Var,Value1,Body],!,
  f2p(HeadIs,LazyVars,ResValue1,eager,Value1,CodeForValue1),
  add_assignment(Var,ResValue1,CodeForValue1,CodeForValue2),
  f2p(HeadIs,LazyVars,RetResult,LazyEval,Body,BodyCode),
  append(CodeForValue2,BodyCode,Converted).

compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :- %dif_functors(HeadIs,Convert),
  Convert =~ ['let*',Bindings,Body],!,
   must_det_lls((
    maplist(compile_let_star(HeadIs,LazyVars),Bindings,CodeList),
    append(CodeList,Code),
    f2p(HeadIs,LazyVars,RetResult,LazyEval,Body,BodyCode),
    append(Code,BodyCode,Converted))).

compile_let_star(HeadIs,LazyVars,[Var,Value1],Code) :-
  f2p(HeadIs,LazyVars,ResValue1,eager,Value1,CodeForValue1),
  add_assignment(Var,ResValue1,CodeForValue1,Code).



%compile_flow_control2(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, x_assign(Convert,RetResult)) :-   is_ftVar(Convert), var(RetResult),!.

compile_flow_control2(_HeadIs,_RetResult,Convert,_):- \+ compound(Convert),!,fail.
compile_flow_control2(_HeadIs,_RetResult,Convert,_):- compound_name_arity(Convert,_,0),!,fail.
:- op(700,xfx, =~).
compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, (Code1,Eval1Result=Result,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ chain(Eval1,Result,Eval2),!,
   f2p(HeadIs, LazyVars, Eval1Result, ResultLazy, Eval1,Code1),
   f2p(HeadIs, LazyVars, RetResult, ResultLazy, Eval2,Converted).

/*
compile_flow_control2(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['bind!',Var,Value],is_ftVar(Value),!,
   Converted = eval_args(['bind!',Var,Value],RetResult).
compile_flow_control2(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['bind!',Var,Value], Value =~ ['new-space'],!,
   Converted = eval_args(['bind!',Var,Value],RetResult).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['bind!',Var,Value],
   f2p(HeadIs, LazyVars, ValueResult, ResultLazy, Value,ValueCode),
   Converted = (ValueCode,eval_args(['bind!',Var,ValueResult],RetResult)).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- %  dif_functors(HeadIs,Convert),
  once(Convert =~ if(Cond,Then,Else);Convert =~ 'if'(Cond,Then,Else)),
  !,Test = is_True(CondResult),
  f2p(HeadIs, LazyVars, CondResult, ResultLazy, Cond,CondCode),
  compile_test_then_else(RetResult,LazyVars,ResultLazy,(CondCode,Test),Then,Else,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-error'(Value,Then,Else),!,Test = is_Error(ValueResult),
  f2p(HeadIs, LazyVars, ValueResult, ResultLazy, Value,ValueCode),
  compile_test_then_else(RetResult,LazyVars,ResultLazy,(ValueCode,Test),Then,Else,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-empty'(Value,Then,Else),!,Test = is_Empty(ValueResult),
  f2p(HeadIs, LazyVars, ValueResult, ResultLazy, Value,ValueCode),
  compile_test_then_else(RetResult,LazyVars,ResultLazy,(ValueCode,Test),Then,Else,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
  (Convert =~ 'if-non-empty-expression'(Value,Then,Else)),!,
  (Test = ( \+ is_Empty(ValueResult))),
  f2p(HeadIs, LazyVars, ValueResult, ResultLazy, Value,ValueCode),
  compile_test_then_else(RetResult,LazyVars,ResultLazy,(ValueCode,Test),Then,Else,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ ['if-equals',Value1,Value2,Then,Else],!,Test = equal_enough(ResValue1,ResValue2),
    f2p(HeadIs, LazyVars, ResValue1, ResultLazy, Value1,CodeForValue1),
    f2p(HeadIs, LazyVars, ResValue2, ResultLazy, Value2,CodeForValue2),
  compile_test_then_else(RetResult,LazyVars,ResultLazy,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).
*/
compile_flow_control1(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    Convert =~ ['assertEqual',Value1,Value2],!,
    cname_var('Src_',Src),
    cname_var('FA_',ResValue1),
    cname_var('FA_',ResValue2),
    cname_var('FARL_',L1),
    cname_var('FARL_',L2),
    f2p(HeadIs, LazyVars, ResValue1, ResultLazy, Value1,CodeForValue1),
    f2p(HeadIs, LazyVars, ResValue2, ResultLazy, Value2,CodeForValue2),
    Converted =
              (Src = Convert,
               loonit_assert_source_tf(Src,
                (findall(ResValue1,CodeForValue1,L1),
                 findall(ResValue2,CodeForValue2,L2)),
                 equal_enough(L1,L2),RetResult)).


compile_flow_control1(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    Convert =~ ['assertEqualToResult',Value1,Value2],!,
    f2p(HeadIs, LazyVars, ResValue1, ResultLazy, Value1,CodeForValue1),
    ast_to_prolog(HeadIs,[],CodeForValue1,Prolog),

    Converted = loonit_assert_source_tf(Convert,
                findall(ResValue1,Prolog,L1),
                 equal_enough(L1,Value2),RetResult).


compile_flow_control2(_HeadIs, _LazyVars, RetResult, _ResultLazy, Convert, Converted) :-
     Convert =~ 'add-atom'(Where,What), !,
     =(What,WhatP),
     Converted = as_tf('add-atom'(Where,WhatP),RetResult).

compile_flow_control2(_HeadIs, _LazyVars, RetResult, _ResultLazy, Convert, Converted) :-
     Convert =~ 'add-atom'(Where,What,RetResult), !,
     =(What,WhatP),
     Converted = as_tf('add-atom'(Where,WhatP),RetResult).


compile_flow_control2(_HeadIs, _LazyVars, RetResult, _ResultLazy, Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_ftVar(ValueL),
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    Converted = eval_args(['superpose',ValueL],RetResult),
    cname_var('MeTTa_SP_',ValueL).

compile_flow_control2(HeadIs, _LazyVars, RetResult, _ResultLazy, Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_list(ValueL),
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    cname_var('SP_Ret',RetResult),
    maplist(f2p_assign(HeadIs,RetResult),ValueL,CodeForValueL),
    list_to_disjuncts(CodeForValueL,Converted),!.


maybe_unlistify([UValueL],ValueL,RetResult,[URetResult]):- fail, is_list(UValueL),!,
  maybe_unlistify(UValueL,ValueL,RetResult,URetResult).
maybe_unlistify(ValueL,ValueL,RetResult,RetResult).

list_to_disjuncts([],false).
list_to_disjuncts([A],A):- !.
list_to_disjuncts([A|L],(A;D)):-  list_to_disjuncts(L,D).


%f2p_assign(_HeadIs,V,Value,is_True(V)):- Value=='True'.
f2p_assign(_HeadIs,ValueR,Value,ValueR=Value):- \+ compound(Value),!.
f2p_assign(_HeadIs,ValueR,Value,ValueR=Value):- is_ftVar(Value),!.
f2p_assign(HeadIs,ValueResult,Value,Converted):-
   f2p(HeadIs, _LazyVars, ValueResultR, _ResultLazy, Value,CodeForValue),
   %into_equals(ValueResultR,ValueResult,ValueResultRValueResult),
   ValueResultRValueResult = (ValueResultR=ValueResult),
   combine_code(CodeForValue,ValueResultRValueResult,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert,Converted) :-
  Convert =~ ['println!',Value],!,
  Converted = (ValueCode,eval_args(['println!',ValueResult], RetResult)),
  f2p(HeadIs, LazyVars, ValueResult, ResultLazy, Value,ValueCode).



compile_flow_control4(HeadIs, LazyVars, RetResult, ResultLazy, Convert,CodeForValueConverted) :-
    % TODO: Plus seems an odd name for a variable - get an idea why?
    % Plus signifies something with numbers
    Convert =~ [Plus,N,Value], atom(Plus),
    transpile_call_prefix(Plus,PrefixPlus),
    current_predicate(PrefixPlus/3), number(N),
    \+ number(Value), \+ is_ftVar(Value),!,
    f2p(HeadIs, LazyVars, ValueResult, ResultLazy, Value,CodeForValue),!,
    Converted =.. [PrefixPlus,N,ValueResult,RetResult],
    combine_code(CodeForValue,Converted,CodeForValueConverted).

compound_equals(COL1,COL2):- COL1=@=COL2,!,COL1=COL2.
compound_equals(COL1,COL2):- compound_equals1(COL1,COL2).
compound_equals1(COL1,COL2):- is_ftVar(COL1),!,is_ftVar(COL2),ignore(COL1=COL2),!.
compound_equals1(COL1,COL2):- compound(COL1),!,compound(COL2), COL1=COL2.

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    Convert =~ ['superpose',COL],compound_equals(COL,'collapse'(Value1)),
    f2p(HeadIs, LazyVars, ResValue1, ResultLazy, Value1,CodeForValue1),
    Converted = (findall(ResValue1,CodeForValue1,Gathered),member(RetResult,Gathered)).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    Convert =~ ['collapse',Value1],!,
    f2p(HeadIs, LazyVars, ResValue1, ResultLazy, Value1,CodeForValue1),
    Converted = (findall(ResValue1,CodeForValue1,RetResult)).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    Convert =~ ['compose',Value1],!, Convert2 =~ ['collapse',Value1],!,
    compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert2, Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['unify-if',Value1,Value2,Then,Else],!,Test = metta_unify(ResValue1,ResValue2),
    f2p(HeadIs, LazyVars, ResValue1, ResultLazy, Value1,CodeForValue1),
    f2p(HeadIs, LazyVars, ResValue2, ResultLazy, Value2,CodeForValue2),
  compile_test_then_else(RetResult,LazyVars,ResultLazy,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).


/*
% match(Space,f(1)=Y,Y)
compile_flow_control2(HeadIs, LazyVars, Y, ResultLazy, Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert=~ match(Space,AsFunctionY,YY),
    nonvar(AsFunctionY),( AsFunctionY =~ (AsFunction=Y)), nonvar(AsFunction),
    !, Y==YY,
    f2p(HeadIs, LazyVars, Y, ResultLazy, AsFunction,Converted),!.
*/
compile_flow_control2(HeadIs, LazyVars, Atom, ResultLazy, Convert,Converted) :-
   Convert=~ match(Space,Q,T),Q==T,Atom=Q,!,
  compile_flow_control2(HeadIs, LazyVars, Atom, ResultLazy, 'get-atoms'(Space),Converted).

compile_flow_control2(_HeadIs, _LazyVars, Match, _ResultLazy, Convert,Converted) :-
    Convert=~ 'get-atoms'(Space),
    Converted = metta_atom_iter(Space,Match).

compile_flow_control2(HeadIs, _LazyVars, AtomsVar, _ResultLazy, Convert,Converted) :-
    Convert=~ 'get-atoms'(Space), AtomsVar = Pattern,
    compile_pattern(HeadIs,Space,Pattern,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(Space,Pattern,Template),!,
    f2p(HeadIs, LazyVars, RetResult, ResultLazy, Template,TemplateCode),
    compile_pattern(HeadIs,Space,Pattern,SpacePatternCode),
    combine_code(SpacePatternCode,TemplateCode,Converted).

compile_pattern(_HeadIs,Space,Match,SpaceMatchCode):-
  SpaceMatchCode = metta_atom_iter(Space,Match).

metta_atom_iter(Space,Match):-
  metta_atom_iter('=',10,Space,Space,Match).



make_with_space(Space,MatchCode,MatchCode):- Space=='&self',!.
make_with_space(Space,MatchCode,with_space(Space,MatchCode)):- Space\=='&self'.

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(_Space,Match,Template),!,
   must_det_lls((
    f2p(HeadIs, LazyVars, _, ResultLazy, Match,MatchCode),
    into_equals(RetResult,Template,TemplateCode),
    combine_code(MatchCode,TemplateCode,Converted))).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- dif_functors(HeadIs,Convert),
   Convert =~ ['if-decons',Atom,Head,Tail,Then,Else],!,Test = unify_cons(AtomResult,ResHead,ResTail),
    f2p(HeadIs, LazyVars, AtomResult, ResultLazy, Atom,AtomCode),
    f2p(HeadIs, LazyVars, ResHead, ResultLazy, Head,CodeForHead),
    f2p(HeadIs, LazyVars, ResTail, ResultLazy, Tail,CodeForTail),
    compile_test_then_else(RetResult,LazyVars,ResultLazy,(AtomCode,CodeForHead,CodeForTail,Test),Then,Else,Converted).


compile_flow_control1(_HeadIs, _LazyVars, RetResult, _ResultLazy, Convert,is_True(RetResult)) :- is_compiled_and(AND),
   Convert =~ [AND],!.

compile_flow_control1(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body],!,
   f2p(HeadIs, LazyVars, RetResult, ResultLazy, Body,BodyCode),
    compile_test_then_else(RetResult,LazyVars,ResultLazy,BodyCode,'True','False',Converted).

compile_flow_control1(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(HeadIs, LazyVars, B1Res, ResultLazy, Body1,Body1Code),
   f2p(HeadIs, LazyVars, RetResult, ResultLazy, Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   Converted = (Body1Code,AE,Body2Code),!.


compile_flow_control1(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(HeadIs, LazyVars, B1Res, ResultLazy, Body1,Body1Code),
   f2p(HeadIs, LazyVars, _, ResultLazy, Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   compile_test_then_else(RetResult,LazyVars,ResultLazy,(Body1Code,AE,Body2Code),'True','False',Converted).

compile_flow_control1(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2|BodyMore],!,
   And2 =~ [AND,Body2|BodyMore],
   Next =~ [AND,Body1,And2],
   compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy,  Next, Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, sequential(Convert), Converted) :- !,
   compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, transpose(Convert), Converted).

compile_flow_control2(HeadIs, _LazyVars, RetResult, _ResultLazy, transpose(Convert), Converted,Code) :- !,
   maplist(each_result(HeadIs,RetResult),Convert, Converted),
   list_to_disjuncts(Converted,Code).


each_result(HeadIs,RetResult,Convert,Converted):-
   f2p(HeadIs, _LazyVars, OneResult, _ResultLazy, Convert,Code1),
   into_equals(OneResult,RetResult,Code2),
   combine_code(Code1,Code2,Converted).

compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Converter, Converted):- de_eval(Converter,Convert),!,
   compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted).

compile_flow_control2(HeadIs, LazyVars, _Result, ResultLazy, Convert, Converted) :- fail,
   functor(Convert,Func,PA),
   functional_predicate_arg(Func,PA,Nth),
   Convert =~ [Func|PredArgs],
   nth1(Nth,PredArgs,Result,FuncArgs),
   RetResult = Result,
   AsFunct =~ [Func|FuncArgs],
   compile_flow_control2(HeadIs, LazyVars, RetResult, ResultLazy, AsFunct, Converted).

dif_functors(HeadIs,_):- var(HeadIs),!,fail.
dif_functors(HeadIs,_):- \+ compound(HeadIs),!.
dif_functors(HeadIs,Convert):- compound(HeadIs),compound(Convert),
  compound_name_arity(HeadIs,F,A),compound_name_arity(Convert,F,A).

is_compiled_and(AND):- member(AND,[ (','), ('and'), ('and-seq')]).

flowc.
unnumbervars_clause(Cl,ClU):-
  copy_term_nat(Cl,AC),unnumbervars(AC,UA),copy_term_nat(UA,ClU).
% ===============================
%  Compile in memory buffer
% ===============================
is_clause_asserted(AC):- unnumbervars_clause(AC,UAC),
  expand_to_hb(UAC,H,B),
  H=..[Fh|Args],
  length(Args,N),
  N1 is N-1,
  atomic_list_concat(['mc_',N1,'__',Fh],FPrefixed),
  H2=..[FPrefixed|Args],
  clause(H2,B,Ref),clause(HH,BB,Ref),
  strip_m(HH,HHH),HHH=@=H2,
  strip_m(BB,BBB),BBB=@=B,!.

%get_clause_pred(UAC,F,A):- expand_to_hb(UAC,H,_),strip_m(H,HH),functor(HH,F,A).


% :- dynamic(needs_tabled/2).

add_assertion(Space,[AC|List]):- is_list(List),!,
   expand_to_hb(AC,H,_),as_functor_args(H,F,A),abolish(F/A),
   maplist(add_assertion(Space),[AC|List]).
add_assertion(Space,AC):- unnumbervars_clause(AC,UAC), add_assertion1(Space,UAC).
add_assertion1(_,AC):- /*'&self':*/is_clause_asserted(AC),!.
%add_assertion1(_,AC):- get_clause_pred(AC,F,A), \+ needs_tabled(F,A), !, pfcAdd(/*'&self':*/AC),!.
add_assertion1(_Space,ACC) :- !, assertz(ACC).

add_assertion1(Space,ACC) :- throw(not_here),
   must_det_lls((
     copy_term(ACC,AC,_),
     expand_to_hb(AC,H,_),
     as_functor_args(H,F,A), as_functor_args(HH,F,A),
    with_mutex(transpiler_mutex_lock,(
      % assert(AC),
      % Get the current clauses of my_predicate/1
      findall(HH:-B,clause(/*'&self':*/HH,B),Prev),
      copy_term(Prev,CPrev,_),
      % Create a temporary file and add the new assertion along with existing clauses
      append(CPrev,[AC],NewList),
      cl_list_to_set(NewList,Set),
      length(Set,N),
      if_t(N=2,
         (Set=[X,Y],
            numbervars(X),
            numbervars(Y)
         %nl,display(X),
         %nl,display(Y),
         %nl
         )),
      %wdmsg(list_to_set(F/A,N)),
      abolish(/*'&self':*/F/A),
      create_and_consult_temp_file(Space,F/A, Set)
    ))
)).

as_functor_args(AsPred,F,A):- as_functor_args(AsPred,F,A,_ArgsL),!.

as_functor_args(AsPred,F,A,ArgsL):-var(AsPred),!,
  (is_list(ArgsL);(integer(A),A>=0)),!,
   length(ArgsL,A),
   (symbol(F)->
      AsPred =..[F|ArgsL]
   ;
      (AsPred = [F|ArgsL])).

%as_functor_args(AsPred,_,_,_Args):- is_ftVar(AsPred),!,fail.
as_functor_args(AsPred,F,A,ArgsL):- \+ iz_conz(AsPred),
  AsPred=..List,!, as_functor_args(List,F,A,ArgsL),!.
%as_functor_args([Eq,R,Stuff],F,A,ArgsL):- (Eq == '='),
%   into_list_args(Stuff,List),append(List,[R],AsPred),!,
%   as_functor_args(AsPred,F,A,ArgsL).
as_functor_args([F|ArgsL],F,A,ArgsL):-  length(ArgsL,A),!.

cl_list_to_set([A|List],Set):-
  member(B,List),same_clause(A,B),!,
  cl_list_to_set(List,Set).
cl_list_to_set([New|List],[New|Set]):-!,
  cl_list_to_set(List,Set).
cl_list_to_set([A,B],[A]):- same_clause(A,B),!.
cl_list_to_set(List,Set):- list_to_set(List,Set).

same_clause(A,B):- A==B,!.
same_clause(A,B):- A=@=B,!.
same_clause(A,B):- unnumbervars_clause(A,AA),unnumbervars_clause(B,BB),same_clause1(AA,BB).
same_clause1(A,B):- A=@=B.
same_clause1(A,B):- expand_to_hb(A,AH,AB),expand_to_hb(B,BH,BB),AB=@=BB, AH=@=BH,!.

%clause('is-closed'(X),OO1,Ref),clause('is-closed'(X),OO2,Ref2),Ref2\==Ref, OO1=@=OO2.

% Convert a list of conditions into a conjunction
list_to_conjunction(C,[CJ]):- \+ is_list(C), !, C = CJ.
list_to_conjunction([], true).
list_to_conjunction([Cond], Cond).
list_to_conjunction([H|T], RestConj) :- H == true, !, list_to_conjunction(T, RestConj).
list_to_conjunction([H|T], (H, RestConj)) :-
    list_to_conjunction(T, RestConj).

% Utility: Combine and flatten a single term into a conjunction
combine_code(Term, Conjunction) :-
    flatten_term(Term, FlatList),
    list_to_conjunction(FlatList, Conjunction).

% combine_code/3: Combines Guard and Body into a flat conjunction
combine_code(Guard, Body, Combined) :-
    combine_code(Guard, FlatGuard), % Flatten Guard
    combine_code(Body, FlatBody),   % Flatten Body
    combine_flattened(FlatGuard, FlatBody, Combined).

% Combine two flattened terms intelligently
combine_flattened(true, Body, Body) :- !.
combine_flattened(Guard, true, Guard) :- !.
combine_flattened(Guard, Body, (Guard, Body)).

% Flatten terms into a flat list
flatten_term(C, CJ):- C==[],!,CJ=C.
flatten_term(C, [CJ]):- \+ compound(C), !, C = CJ.
flatten_term((A, B), FlatList) :- !, % If Term is a conjunction, flatten both sides
    flatten_term(A, FlatA),
    flatten_term(B, FlatB),
    append(FlatA, FlatB, FlatList).
flatten_term(List, FlatList) :- is_list(List),
    !, % If Term is a list, recursively flatten its elements
    maplist(flatten_term, List, NestedLists),
    append(NestedLists, FlatList).
flatten_term([A | B ], FlatList) :-  !, % If Term is a conjunction, flatten both sides
    flatten_term(A, FlatA),
    flatten_term(B, FlatB),
    append(FlatA, FlatB, FlatList).
flatten_term(Term, [Term]). % Base case: single term, wrap it in a list


fn_eval(Fn,Args,Res):- is_list(Args),symbol(Fn),transpile_call_prefix(Fn,Pred),Pre=..[Pred|Args],
  catch(call(Pre,Res),error(existence_error(procedure,_/_),_),Res=[Fn|Args]).

fn_native(Fn,Args):- apply(Fn,Args).
%fn_eval(Fn,Args,[Fn|Args]).

assign(X,list(Y)):- is_list(Y),!,X=Y.
assign(X,X).

x_assign(X,X).













end_of_file.




























compile_head_variablization(Head, NewHead, HeadCode) :-
   must_det_lls((
      as_functor_args(Head,Functor,A,Args),
      % Find non-singleton variables in Args
      fix_non_singletons(Args, NewArgs, Conditions),
      list_to_conjunction(Conditions,HeadCode),
      as_functor_args(NewHead,Functor,A,NewArgs))).

fix_non_singletons(Args, NewArgs, [Code|Conditions]) :-
   sub_term_loc(Var, Args, Loc1), is_ftVar(Var),
   sub_term_loc_replaced(==(Var), _Var2, Args, Loc2, ReplVar2, NewArgsM),
   Loc1 \=@= Loc2,
   Code = same(ReplVar2,Var),
fix_non_singletons(NewArgsM, NewArgs, Conditions).
fix_non_singletons(Args, Args, []):-!.


sub_term_loc(A,A,self).
sub_term_loc(E,Args,e(N,nth1)+Loc):- is_list(Args),!, nth1(N,Args,ST),sub_term_loc(E,ST,Loc).
sub_term_loc(E,Args,e(N,arg)+Loc):- compound(Args),arg(N,Args,ST),sub_term_loc(E,ST,Loc).

sub_term_loc_replaced(P1,E,Args,LOC,Var,NewArgs):- is_list(Args), !, sub_term_loc_l(nth1,P1,E,Args,LOC,Var,NewArgs).
sub_term_loc_replaced(P1,E,FArgs,LOC,Var,NewFArgs):- compound(FArgs), \+ is_ftVar(FArgs),!,
   compound_name_arguments(FArgs, Name, Args),
   sub_term_loc_l(arg,P1,E,Args,LOC,Var,NewArgs),
   compound_name_arguments(NewCompound, Name, NewArgs),NewFArgs=NewCompound.
   sub_term_loc_replaced(P1,A,A,self,Var,Var):- call(P1,A).


sub_term_loc_l(Nth,P1,E,Args,e(N,Nth)+Loc,Var,NewArgs):-
   reverse(Args,RevArgs),
   append(Left,[ST|Right],RevArgs),
   sub_term_loc_replaced(P1,E,ST,Loc,Var,ReplaceST),
   append(Left,[ReplaceST|Right],RevNewArgs),
   reverse(RevNewArgs,NewArgs),
   length([_|Right], N).


/*
as_functor_args(AsPred,F,A,ArgsL):-    nonvar(AsPred),!,into_list_args(AsPred,[F|ArgsL]),    length(ArgsL,A).
as_functor_args(AsPred,F,A,ArgsL):-
   nonvar(F),length(ArgsL,A),AsPred = [F|ArgsL].
*/

compile_for_assert(HeadIs, AsBodyFn, Converted) :- (AsBodyFn =@= HeadIs ; AsBodyFn == []), !,/*trace,*/  compile_head_for_assert(HeadIs,Converted).

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(Head, AsBodyFn, Converted) :-
   once(compile_head_variablization(Head, HeadC, CodeForHeadArgs)),
   \+(atomic(CodeForHeadArgs)), !,
   compile_for_assert(HeadC,
      (CodeForHeadArgs,AsBodyFn), Converted).

compile_for_assert(HeadIs, AsBodyFn, Converted) :- is_ftVar(AsBodyFn), /*trace,*/
   AsFunction = HeadIs,!,
   must_det_lls((
   Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   f2p(HeadIs, LazyVars, HResult, ResultLazy, AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   NextBody = x_assign(AsBodyFn,Result),
   optimize_head_and_body(Head,NextBody,HeadC,BodyC),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

% PLACEHOLDER


% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   compile_head_args(Head,HeadC,CodeForHeadArgs),
   f2p(HeadIs, LazyVars, Result, ResultLazy, AsBodyFn,NextBody),
   combine_code(CodeForHeadArgs,NextBody,BodyC),!,
   optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),!.



% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% ===============================


% Conversion is possible between a function and a predicate of arity when the result is at the nth arg
:- dynamic decl_functional_predicate_arg/3.

% Converion is possible between a  function and predicate is tricky
functional_predicate_arg_tricky(is, 2, 1). % E.g. eval_args(is(+(1,2)),Result) converts to is(Result,+(1,2)).
% Defining standard mappings for some common functions/predicates
decl_functional_predicate_arg(append, 3, 3).
decl_functional_predicate_arg(+, 3, 3).
decl_functional_predicate_arg(pi, 1, 1).
decl_functional_predicate_arg('Empty', 1, 1).
decl_functional_predicate_arg(call,4,4).
decl_functional_predicate_arg(eval_args, 2, 2).
decl_functional_predicate_arg(edge, 2, 2).
decl_functional_predicate_arg('==', 2, 2).
decl_functional_predicate_arg('is-same', 2, 2).
decl_functional_predicate_arg(assertTrue, 2, 2).
decl_functional_predicate_arg(case, 3, 3).
decl_functional_predicate_arg(assertFalse, 2, 2).
decl_functional_predicate_arg('car-atom', 2, 2).
decl_functional_predicate_arg(match,4,4).
decl_functional_predicate_arg('TupleConcat',3,3).
decl_functional_predicate_arg('new-space',1,1).

decl_functional_predicate_arg(superpose, 2, 2).

do_predicate_function_canonical(F,FF):- predicate_function_canonical(F,FF),!.
do_predicate_function_canonical(F,F).
predicate_function_canonical(is_Empty,'Empty').

pi(PI):- PI is pi.

% Retrieve Head of the List
'car-atom'(List, Head):- eval_H(['car-atom', List], Head).


% Mapping any current predicate F/A to a function, if it's not tricky
functional_predicate_arg(F, A, L):- decl_functional_predicate_arg(F, A, L).
functional_predicate_arg(F, A, L):- (atom(F)->true;trace), predicate_arity(F,A),
  \+ functional_predicate_arg_tricky(F,A,_), L=A,
  \+ decl_functional_predicate_arg(F, A, _).
functional_predicate_arg(F, A, L):- functional_predicate_arg_tricky(F, A, L).

predicate_arity(F,A):- metta_atom('&self',[:,F,[->|Args]]), length(Args,A).
predicate_arity(F,A):- current_predicate(F/A).
% Certain constructs should not be converted to functions.
not_function(P):- atom(P),!,not_function(P,0).
not_function(P):- callable(P),!,functor(P,F,A),not_function(F,A).
not_function(F,A):- is_arity_0(F,FF),!,not_function(FF,A).
not_function(!,0).
not_function(print,1).
not_function((':-'),2).
not_function((','),2).
not_function((';'),2).
not_function(('='),2).
not_function(('or'),2).

not_function('a',0).
not_function('b',0).
not_function(F,A):- is_control_structure(F,A).
not_function(A,0):- atom(A),!.
not_function('True',0).
not_function(F,A):- predicate_arity(F,A),AA is A+1, \+ decl_functional_predicate_arg(F,AA,_).

needs_call_fr(P):- is_function(P,_Nth),functor(P,F,A),AA is A+1, \+ current_predicate(F/AA).

is_control_structure(F,A):- atom(F), atom_concat('if-',_,F),A>2.

'=='(A, B, Res):- as_tf(equal_enough(A, B),Res).
'or'(G1,G2):- G1 *-> true ; G2.
'or'(G1,G2,Res):- as_tf((G1 ; G2),Res).

% Function without arguments can be converted directly.
is_arity_0(AsFunction,F):- compound(AsFunction), compound_name_arity(AsFunction,F,0).

% Determines whether a given term is a function and retrieves the position
% in the predicate where the function Result is stored/retrieved
is_function(AsFunction, _):- is_ftVar(AsFunction),!,fail.
is_function(AsFunction, _):- AsFunction=='$VAR',!, trace, fail.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction,F), \+ not_function(F,0), !,Nth=1.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction,_), !,Nth=1.
is_function(AsFunction, Nth) :-
    callable(AsFunction),
    functor(AsFunction, Functor, A),
    \+ not_function(Functor, A),
    AA is A + 1,
    functional_predicate_arg_maybe(Functor, AA, Nth).

functional_predicate_arg_maybe(F, AA, Nth):- functional_predicate_arg(F, AA, Nth),!.
functional_predicate_arg_maybe(F, AA, _):- A is AA - 1,functional_predicate_arg(F,A,_),!,fail.
functional_predicate_arg_maybe(F, Nth, Nth):- asserta(decl_functional_predicate_arg(F, Nth, Nth)),!.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_head_for_assert(HeadIs, (Head:-Body)):-
   compile_head_for_assert(HeadIs, NewHeadIs,Converted),
   head_preconds_into_body(NewHeadIs,Converted,Head,Body).

head_as_is(Head):-
   as_functor_args(Head,Functor,A,_),!,
   head_as_is(Functor,A).
head_as_is(if,3).

compile_head_for_assert(Head, Head, true):-
   head_as_is(Head),!.

compile_head_for_assert(Head, NewestHead, HeadCode):-
   compile_head_variablization(Head, NewHead, VHeadCode),
   compile_head_args(NewHead, NewestHead, AHeadCode),
   combine_code(VHeadCode,AHeadCode,HeadCode).

% Construct the new head and the match body
compile_head_args(Head, NewHead, HeadCode) :-
   must_det_lls((
      as_functor_args(Head,Functor,A,Args),
      maplist(f2p_assign(Head),NewArgs,Args,CodeL),
      as_functor_args(NewHead,Functor,A,NewArgs),
      list_to_conjuncts(CodeL,HeadCode))),!.







:- op(700,xfx,'=~').



compile_for_assert(HeadIs, AsBodyFn, Converted) :- (AsBodyFn =@= HeadIs ; AsBodyFn == []), !,/*trace,*/  compile_head_for_assert(HeadIs,Converted).

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(Head, AsBodyFn, Converted) :-
   once(compile_head_variablization(Head, HeadC, CodeForHeadArgs)),
   \+(atomic(CodeForHeadArgs)), !,
   compile_for_assert(HeadC,
      (CodeForHeadArgs,AsBodyFn), Converted).

compile_for_assert(HeadIs, AsBodyFn, Converted) :- fail,is_ftVar(AsBodyFn), /*trace,*/
   AsFunction = HeadIs,!,
   must_det_lls((
   Converted = (HeadC :- BodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   %funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   f2p(HeadIs, LazyVars, HResult, ResultLazy, AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   NextBody = x_assign(AsBodyFn,Result),
   optimize_head_and_body(Head,NextBody,HeadC,BodyC),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   format("~q ~q ~q\n",[HeadIs, AsBodyFn, Converted]),
   AsFunction = HeadIs,
   must_det_lls((
   Converted = (HeadC :- NextBodyC),  % Create a rule with Head as the converted AsFunction and NextBody as the converted AsBodyFn
   /*funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),*/
   f2p(HeadIs, LazyVars, HResult, ResultLazy, AsFunction,HHead),
   (var(HResult) -> (Result = HResult, HHead = Head) ;
      funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head)),
   %verbose_unify(Converted),
   f2p(HeadIs, LazyVars, Result, ResultLazy, AsBodyFn,NextBody),
   %RetResult = Converted,
   %RetResult = _,
   optimize_head_and_body(Head,NextBody,HeadC,NextBodyC),
   %fbug([convert(Convert),head_preconds_into_body(HeadC:-NextBodyC)]),
   %if_t(((Head:-NextBody)\=@=(HeadC:-NextBodyC)),fbug(was(Head:-NextBody))),
   nop(ignore(Result = '$VAR'('HeadRes'))))),!.

% If Convert is of the form (AsFunction=AsBodyFn), we perform conversion to obtain the equivalent predicate.
compile_for_assert(HeadIs, AsBodyFn, Converted) :-
   AsFunction = HeadIs, Converted = (HeadCC :- BodyCC),
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, _Nth, Head),
   compile_head_args(Head,HeadC,CodeForHeadArgs),
   f2p(HeadIs, LazyVars, Result, ResultLazy, AsBodyFn,NextBody),
   combine_code(CodeForHeadArgs,NextBody,BodyC),!,
   optimize_head_and_body(HeadC,BodyC,HeadCC,BodyCC),!.


/*
*/
metta_predicate(eval_args(evaluable,eachvar)).
metta_predicate(eval_true(matchable)).
metta_predicate(with_space(space,matchable)).
metta_predicate(limit(number,matchable)).
metta_predicate(findall(template,matchable,listvar)).
metta_predicate(match(space,matchable,template,eachvar)).

head_preconds_into_body(Head,Body,Head,Body):- \+ compound(Head),!.
head_preconds_into_body((PreHead,True),Converted,Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body((True,PreHead),Converted,Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body(PreHead,(True,Converted),Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body(PreHead,(Converted,True),Head,Body):- True==true,!,
  head_preconds_into_body(PreHead,Converted,Head,Body).
head_preconds_into_body((AsPredO,Pre),Converted,Head,Body):-
  head_preconds_into_body(Pre,(AsPredO,Converted),Head,Body).

head_preconds_into_body(AHead,Body,Head,BodyNew):-
    assertable_head(AHead,Head),
    optimize_body(Head,Body,BodyNew).


assertable_head(u_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), atom(F),!, Head=..[F|NewArgs].
assertable_head(Head,Head).

ok_to_append('$VAR'):- !, fail.
ok_to_append(_).

p2s(P,S):- into_list_args(P,S).

non_compound(S):- \+ compound(S).

did_optimize_conj(Head,B1,B2,B12):- optimize_conj(Head,B1,B2,B12), B12\=@=(B1,B2),!.


optimize_conjuncts(Head,(B1,B2,B3),BN):- B3\==(_,_),
  did_optimize_conj(Head,B2,B3,B23),
  optimize_conjuncts(Head,B1,B23,BN), !.
optimize_conjuncts(Head,(B1,B2,B3),BN):-
  did_optimize_conj(Head,B1,B2,B12),
  optimize_conjuncts(Head,B12,B3,BN),!.
%optimize_conjuncts(Head,(B1,B2),BN1):- optimize_conj(Head,B1,B2,BN1).
optimize_conjuncts(Head,(B1,B2),BN1):- did_optimize_conj(Head,B1,B2,BN1),!.
optimize_conjuncts(Head,B1,B2,(BN1,BN2)):-
   must_optimize_body(Head,B1,BN1), must_optimize_body(Head,B2,BN2).

optimize_conj(_, x_assign(Term, C), x_assign(True,CC), eval_true(Term)):- 'True'==True, CC==C.
optimize_conj(_, x_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(_, B1,BT,B1):- assumed_true(BT),!.
optimize_conj(_, BT,B1,B1):- assumed_true(BT),!.
%optimize_conj(Head, x_assign(Term, C), x_assign(True,CC), Term):- 'True'==True,
%     optimize_conj(Head, x_assign(Term, C), is_True(CC), CTerm).
%optimize_conj(Head,B1,BT,BN1):- assumed_true(BT),!, optimize_body(Head,B1,BN1).
%optimize_conj(Head,BT,B1,BN1):- assumed_true(BT),!, optimize_body(Head,B1,BN1).
optimize_conj(Head,B1,B2,(BN1,BN2)):-
   optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).

assumed_true(B2):- var(B2),!,fail.
assumed_true(eval_true(B2)):-!,assumed_true(B2).
assumed_true(B2):- B2== true,!.
assumed_true(B2):- B2==x_assign('True', '$VAR'('_')),!.
assumed_true(X==Y):- assumed_true(X=Y).
assumed_true(X=Y):- var(X),var(Y), X=Y.
assumed_true(X=Y):- is_ftVar(X),is_ftVar(Y), X=Y.


filter_head_arg(H,F):- var(H),!,H=F.
filter_head_arge(H,F):- H = F.

code_callable(Term,_CTerm):- var(Term),!,fail.
code_callable(Term, CTerm):- current_predicate(_,Term),!,Term=CTerm.
%code_callable(Term, CTerm):- current_predicate(_,Term),!,Term=CTerm.







:- discontiguous f2p/4.

% If Convert is a variable, the corresponding predicate is just eval_args(Convert, RetResult)
f2p(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = eval_args(Convert, RetResult).  % Set Converted to eval_args(Convert, RetResult)

% If Convert is a variable, the corresponding predicate is just eval_args(Convert, RetResult)
f2p(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = eval_args(Convert, RetResult).  % Set Converted to eval_args(Convert, RetResult)
f2p(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, RetResultConverted) :-
     number(Convert),!,into_equals(RetResult,Convert,RetResultConverted).

f2p(_HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- % HeadIs\=@=Convert,
     is_arity_0(Convert,F), !, Converted = x_assign([F],RetResult),!.



/*f2p(HeadIs, LazyVars, RetResult, ResultLazy,  ConvertL, (Converted,RetResultL=RetResult)) :- is_list(ConvertL),
   maplist(f2p_assign(HeadIs),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Converted).*/

% If Convert is an "eval_args" function, we convert it to the equivalent "is" predicate.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, EvalConvert,Converted):- EvalConvert =~ eval_args(Convert),  !,
  must_det_lls((f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted))).

% placeholder

f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted):-
    compound(Convert), Convert = x_assign(C, Var), compound_non_cons(C),into_list_args(C,CC),!,
    f2p(HeadIs, LazyVars, RetResult, ResultLazy, x_assign(CC, Var), Converted).

f2p(_HeadIs, LazyVars, _RetResult, ResultLazy, Convert, Converted):-
    compound(Convert), Convert = x_assign(C, _Var), is_list(C),Converted = Convert,!.

f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
     atom(Convert),  functional_predicate_arg(Convert,Nth,Nth2),
      Nth==1,Nth2==1,
      HeadIs\=@=Convert,
      Convert = F,!,
      must_det_lls((
        do_predicate_function_canonical(FP,F),
        compound_name_list(Converted,FP,[RetResult]))).

% PLACEHOLDER

% If Convert is an "is" function, we convert it to the equivalent "is" predicate.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, is(Convert),(Converted,is(RetResult,Result))):- !,
   must_det_lls((f2p(HeadIs, LazyVars, Result, ResultLazy, Convert, Converted))).

% If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, or(AsPredI,Convert), (AsPredO *-> true; Converted)) :- fail, !,
  must_det_lls((f2p(HeadIs, LazyVars, RetResult, ResultLazy, AsPredI, AsPredO),
               f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted))).

f2p(HeadIs, LazyVars, RetResult, ResultLazy, (AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_lls((f2p(HeadIs, LazyVars, RetResult, ResultLazy, AsPredI, AsPredO),
               f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted))).
f2p(HeadIs, LazyVars, RetResult, ResultLazy, SOR,or(AsPredO, Converted)) :-
  SOR =~ or(AsPredI, Convert),
  must_det_lls((f2p(HeadIs, LazyVars, RetResult, ResultLazy, AsPredI, AsPredO),
               f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted))),!.

% If Convert is a "," (and) function, we convert it to the equivalent "," (and) predicate.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, (AsPredI, Convert), (AsPredO, Converted)) :- !,
  must_det_lls((f2p(HeadIs, LazyVars, _RtResult, ResultLazy, AsPredI, AsPredO),
               f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted))).

% If Convert is a ":-" (if) function, we convert it to the equivalent ":-" (if) predicate.
f2p(_HeadIs, LazyVars, RetResult, ResultLazy,  Convert, Converted) :- Convert =(H:-B),!,
  RetResult=(H:-B), Converted = true.

f2p(_HeadIs, LazyVars, _RetResult, ResultLazy,  N=V, Code) :- !, into_equals(N,V,Code).





% If Convert is a list, we convert it to its termified form and then proceed with the functs_to_preds conversion.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :- fail,
   is_list(Convert),
   once((sexpr_s2p(Convert,IS), \+ IS=@=Convert)), !,  % Check if Convert is a list and not in predicate form
   must_det_lls((f2p(HeadIs, LazyVars, RetResult, ResultLazy,  IS, Converted))).  % Proceed with the conversion of the predicate form of the list.

f2p(HeadIs, LazyVars, RetResult, ResultLazy,  ConvertL, Converted) :- fail,
   is_list(ConvertL),
   maplist(f2p_assign(HeadIs),RetResultL,ConvertL, ConvertedL),
   list_to_conjuncts(ConvertedL,Conjs),
   into_x_assign(RetResultL,RetResult,Code),
   combine_code(Conjs,Code,Converted).


f2p(HeadIs, LazyVars, RetResultL, ResultLazy,  ConvertL, Converted) :- fail,
   is_list(ConvertL),
   ConvertL = [Convert],
   f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Code), !,
   into_equals(RetResultL,[RetResult],Equals),
   combine_code(Code,Equals,Converted).


% If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    rev_sub_sterm(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
  %  sub_term_safely(AsFunction, Convert), AsFunction\==Convert,
    callable(AsFunction),  % Check if AsFunction is callable
    compile_flow_control(HeadIs, LazyVars, Result, ResultLazy, AsFunction, AsPred),
    HeadIs\=@=AsFunction,!,
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    f2p(HeadIs, LazyVars, RetResult, ResultLazy, (AsPred,Converting), Converted).  % Proceed with the conversion of the remaining terms

% If any sub-term of Convert is a function, convert that sub-term and then proceed with the conversion.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    rev_sub_sterm(AsFunction, Convert),  % Get the deepest sub-term AsFunction of Convert
    callable(AsFunction),  % Check if AsFunction is callable
    is_function(AsFunction, Nth),  % Check if AsFunction is a function and get the position Nth where the result is stored/retrieved
    HeadIs\=@=AsFunction,
    funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, AsFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    f2p(HeadIs, LazyVars, RetResult, ResultLazy,  (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

% If AsFunction is a recognized function, convert it to a predicate.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, AsFunction,AsPred):- % HeadIs\=@=AsFunction,
   is_function(AsFunction, Nth),  % Check if AsFunction is a recognized function and get the position Nth where the result is stored/retrieved
   funct_with_result_is_nth_of_pred(HeadIs,AsFunction, RetResult, Nth, AsPred),
   \+ ( compound(AsFunction), arg(_,AsFunction, Arg), is_function(Arg,_)),!.

% If any sub-term of Convert is an eval_args/2, convert that sub-term and then proceed with the conversion.
f2p(HeadIs, LazyVars, RetResult, ResultLazy, Convert, Converted) :-
    rev_sub_sterm0(ConvertFunction, Convert), % Get the deepest sub-term AsFunction of Convert
    callable(ConvertFunction),  % Check if AsFunction is callable
    ConvertFunction = eval_args(AsFunction,Result),
    ignore(is_function(AsFunction, Nth)),
    funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred),  % Convert AsFunction to a predicate AsPred
    subst(Convert, ConvertFunction, Result, Converting),  % Substitute AsFunction by Result in Convert
    f2p(HeadIs, LazyVars, RetResult, ResultLazy,  (AsPred, Converting), Converted).  % Proceed with the conversion of the remaining terms

/* MAYBE USE ?
% If Convert is a compound term, we need to recursively convert its arguments.
f2p(HeadIs, LazyVars, RetResult, ResultLazy,  Convert, Converted) :- fail,
    compound(Convert), !,
    Convert =~ [Functor|Args],  % Deconstruct Convert to functor and arguments
    maplist(convert_argument, Args, ConvertedArgs),  % Recursively convert each argument
    Converted =~ [Functor|ConvertedArgs],  % Reconstruct Converted with the converted arguments
    (callable(Converted) -> f2p(HeadIs, LazyVars, RetResult, ResultLazy,  Converted, _); true).  % If Converted is callable, proceed with its conversion
% Helper predicate to convert an argument of a compound term
convert_argument(Arg, ConvertedArg) :-
    (callable(Arg) -> ftp(_, _, Arg, ConvertedArg); ConvertedArg = Arg).
*/




% This predicate is responsible for converting functions to their equivalent predicates.
% It takes a function 'AsFunction' and determines the predicate 'AsPred' which will be
% equivalent to the given function, placing the result of the function at the 'Nth' position
% of the predicate arguments. The 'Result' will be used to store the result of the 'AsFunction'.
%
% It handles cases where 'AsFunction' is a variable and when it's an atom or a compound term.
% For compound terms, it decomposes them to get the functor and arguments and then reconstructs
% the equivalent predicate with the 'Result' at the 'Nth' position.
%
% Example:
% funct_with_result_is_nth_of_pred(HeadIs,+(1, 2), Result, 3, +(1, 2, Result)).

into_callable(Pred,AsPred):- is_ftVar(Pred),!,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=AsPred,!.
into_callable(Pred,AsPred):- iz_conz(Pred), !,AsPred=holds(Pred).
into_callable(Pred,AsPred):- Pred=call_fr(_,_,_),!,AsPred=Pred.
into_callable(Pred,AsPred):- Pred =~ Cons,  !,AsPred=holds(Cons).

funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred):-
  var(AsPred),!,
  funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, Pred),
  into_callable(Pred,AsPred).

funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred):-
  var(AsFunction),!,
  funct_with_result_is_nth_of_pred0(HeadIs,Function, Result, Nth, AsPred),
  into_callable(Function,AsFunction).

funct_with_result_is_nth_of_pred(HeadIs,AsFunction, Result, Nth, AsPred):-
  funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, AsPred).

% Handles the case where AsFunction is a variable.
% It creates a compound term 'AsPred' and places the 'Result' at the 'Nth' position
% of the predicate arguments, and the 'AsFunction' represents the functional form with
% arguments excluding the result.
funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, AsPred) :-
    is_ftVar(AsFunction),!,
   compound(AsPred),
    compound_name_list(AsPred,FP,PredArgs),
    nth1(Nth,PredArgs,Result,FuncArgs),
    do_predicate_function_canonical(FP,F),
    AsFunction =~ [F,FuncArgs].

% Handles the case where 'AsFunction' is not a variable.
% It decomposes 'AsFunction' to get the functor and arguments (FuncArgs) of the function
% and then it constructs the equivalent predicate 'AsPred' with 'Result' at the 'Nth'
% position of the predicate arguments.
funct_with_result_is_nth_of_pred0(HeadIs,AsFunctionO, Result, Nth, (AsPred)) :-
   de_eval(AsFunctionO,AsFunction),!,funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, Nth, AsPred).

funct_with_result_is_nth_of_pred0(HeadIs,AsFunction, Result, _Nth, AsPred) :-
   nonvar(AsFunction),
   compound(AsFunction),
   \+ is_arity_0(AsFunction,_),
   functor(AsFunction,F,A),
   HeadIs\=@=AsFunction,
   \+ (compound(HeadIs), (is_arity_0(HeadIs,HF);functor(HeadIs,HF,_))-> HF==F),
   (into_x_assign(AsFunction, Result,AsPred)
       -> true
       ; (AA is A+1,
           (FAA=(F/AA)),
           \+ current_predicate(FAA), !,
           AsPred = call_fr(AsFunction,Result,FAA))).


funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, (AsPred)) :-
   (atom(AsFunction)->AsFunction =~ [F | FuncArgs]; compound_name_list(AsFunction,F,FuncArgs)),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
    nth1(Nth, PredArgs, Result, FuncArgs), % It places 'Result' at the 'Nth' position
    AA is Nth+1, \+ current_predicate(F/AA),
    do_predicate_function_canonical(FP,F),
    AsPred =~ [FP | PredArgs]. % It forms the predicate 'AsPred' by joining the functor with the modified arguments list.



funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, Result, Nth, (AsPred)) :-
    nonvar(AsFunction),
    AsFunction =~ [F | FuncArgs],
    do_predicate_function_canonical(FP,F),
    length(FuncArgs, Len),
   ignore(var(Nth) -> is_function(AsFunction,Nth); true),
   ((number(Nth),Nth > Len + 1) -> throw(error(index_out_of_bounds, _)); true),
   (var(Nth)->(between(1,Len,From1),Nth is Len-From1+1);true),
    nth1(Nth,PredArgs,Result,FuncArgs),
    AsPred =~ [FP | PredArgs].

% optionally remove next line
funct_with_result_is_nth_of_pred0(_HeadIs,AsFunction, _, _, _) :-
    var(AsFunction),
    throw(error(instantiation_error, _)).

% The remove_funct_arg/3 predicate is a utility predicate that removes
% the Nth argument from a predicate term, effectively converting a
% predicate to a function. The first argument is the input predicate term,
% the second is the position of the argument to be removed, and the third
% is the output function term.
remove_funct_arg(AsPred, Nth, AsFunction) :-
    % Decompose AsPred into its functor and arguments.
    AsPred =~ [F | PredArgs],
    % Remove the Nth element from PredArgs, getting the list FuncArgs.
    nth1(Nth,PredArgs,_Result,FuncArgs),
    % Construct AsFunction using the functor and the list FuncArgs.
    do_predicate_function_canonical(F,FF),
    compound_name_list(AsFunction,FF,FuncArgs).

% rev_sub_sterm/2 predicate traverses through a given Term
% and finds a sub-term within it. The sub-term is unifiable with ST.
% This is a helper predicate used in conjunction with others to inspect
% and transform terms.

rev_sub_sterm(ST, Term):- rev_sub_sterm0(ST, Term), ST\=@=Term.
rev_sub_sterm0(_, Term):- never_subterm(Term),!,fail.
rev_sub_sterm0(ST, Term):- Term =~ if(Cond,_Then,_Else),!,rev_sub_sterm0(ST, Cond).
rev_sub_sterm0(ST, Term):- Term =~ 'if-error'(Cond,_Then,_Else),!,rev_sub_sterm0(ST, Cond).
rev_sub_sterm0(ST, Term):- Term =~ 'if-decons'(Cond,_Then,_Else),!,rev_sub_sterm0(ST, Cond).
rev_sub_sterm0(ST, Term):- Term =~ 'chain'(Expr,_Var,_Next),!,rev_sub_sterm0(ST, Expr).
rev_sub_sterm0(ST, Term):-
    % If Term is a list, it reverses the list and searches for a member
    % in the reversed list that is unifiable with ST.
    is_list(Term),!,rev_member(E,Term),rev_sub_sterm0(ST, E).
rev_sub_sterm0(ST, Term):-
    % If Term is a compound term, it gets its arguments and then recursively
    % searches in those arguments for a sub-term unifiable with ST.
    compound(Term), compound_name_list(Term,_,Args),rev_sub_sterm0(ST, Args).
rev_sub_sterm0(ST, ST):-
    % If ST is non-var, not an empty list, and callable, it unifies
    % ST with Term if it is unifiable.
    nonvar(ST), ST\==[], callable(ST).

never_subterm(Term):- is_ftVar(Term).
never_subterm([]).
never_subterm('Nil').
%never_subterm(F):- atom(F),not_function(F,0).

% rev_member/2 predicate is a helper predicate used to find a member
% of a list. It is primarily used within rev_sub_sterm/2 to
% traverse through lists and find sub-terms. It traverses the list
% from the end to the beginning, reversing the order of traversal.
rev_member(E,[_|L]):- rev_member(E,L).
rev_member(E,[E|_]).

% Continuing from preds_to_functs/2
% Converts a given predicate representation to its equivalent function representation
preds_to_functs(Convert, Converted):-
  % Verbose_unify/1 here may be used for debugging or to display detailed unification information
  verbose_unify(Convert),
  % Calls the auxiliary predicate preds_to_functs0/2 to perform the actual conversion
  preds_to_functs0(Convert, Converted).

% if Convert is a variable, Converted will be the same variable
preds_to_functs0(Convert, Converted) :-
    is_ftVar(Convert), !,
    Converted = Convert.

% Converts the rule (Head :- Body) to its function equivalent
preds_to_functs0((Head:-Body), Converted) :- !,
  % The rule is converted by transforming Head to a function AsFunction and the Body to ConvertedBody
  (
   pred_to_funct(Head, AsFunction, Result),
   %ignore(Result = '$VAR'('HeadRes')),
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   compound(BE),arg(Nth,BE,ArgRes),sub_var_safely(Result,ArgRes),
   remove_funct_arg(BE, Nth, AsBodyFunction),
   append(Left,[eval_args(AsBodyFunction,Result)|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody),
   preds_to_functs0(NewBody,ConvertedBody),
   % The final Converted term is constructed
   into_equals(AsFunction,ConvertedBody,Converted)).

% Handles the case where Convert is a conjunction, and AsPred is not not_function.
% It converts predicates to functions inside a conjunction
preds_to_functs0((AsPred, Convert), Converted) :-
    \+ not_function(AsPred),
    pred_to_funct(AsPred, AsFunction, Result),
    sub_var_safely(Result, Convert), !,
    % The function equivalent of AsPred _xs Result in Convert
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs0(Converting, Converted).

% Handles the special case where eval_args/2 is used and returns the function represented by the first argument of eval_args/2
preds_to_functs0(eval_args(AsFunction, _Result), AsFunction) :- !.

% Handles the general case where Convert is a conjunction.
% It converts the predicates to functions inside a conjunction
preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

% Handles the case where AsPred is a compound term that can be converted to a function
preds_to_functs0(AsPred, eval_args(AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.

% any other term remains unchanged
preds_to_functs0(X, X).

% Converts a given predicate AsPred to its equivalent function term AsFunction
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), % Checks if AsPred is a compound term
    functor(AsPred, F, A), % Retrieves the functor F and arity A of AsPred
    functional_predicate_arg(F, A, Nth),!, % Finds the Nth argument where the result should be
    arg(Nth, AsPred, Result), % Retrieves the result from the Nth argument of AsPred
    remove_funct_arg(AsPred, Nth, AsFunction). % Constructs the function AsFunction by removing the Nth argument from AsPred

% If not found in functional_predicate_arg/3, it tries to construct AsFunction by removing the last argument from AsPred
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    functor(AsPred, _, Nth),
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

% body_member/4 is utility predicate to handle manipulation of body elements in the clause, but the exact implementation details and usage are not provided in the given code.
body_member(Body,BE,NewBE,NewBody):-
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   append(Left,[NewBE|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody).
% combine_clauses/3 is the main predicate combining clauses with similar heads and bodies.
% HeadBodiesList is a list of clauses (Head:-Body)
% NewHead will be the generalized head representing all clauses in HeadBodiesList
% NewCombinedBodies will be the combined bodies of all clauses in HeadBodiesList.
combine_clauses(HeadBodiesList, NewHead, NewCombinedBodies) :-
    % If HeadBodiesList is empty, then NewCombinedBodies is 'false' and NewHead is an anonymous variable.
    (HeadBodiesList = [] -> NewCombinedBodies = false, NewHead = _ ;
    % Find all Heads in HeadBodiesList and collect them in the list Heads
    findall(Head, member((Head:-_), HeadBodiesList), Heads),
    % Find the least general head among the collected Heads
    least_general_head(Heads, LeastHead),
    functor(LeastHead,F,A),functor(NewHead,F,A),
    % Transform and combine bodies according to the new head found
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)),
    \+ \+ (
     Print=[converting=HeadBodiesList,newHead=NewHead],
     numbervars(Print,0,_,[attvar(skip)]),fbug(Print),
        nop(in_cmt(print_pl_source(( NewHead :- NewCombinedBodies))))),!.

% Predicate to find the least general unified head (LGU) among the given list of heads.
% Heads is a list of head terms, and LeastGeneralHead is the least general term that unifies all terms in Heads.
least_general_head(Heads, LeastGeneralHead) :-
    lgu(Heads, LeastGeneralHead).

% the LGU of a single head is the head itself.
lgu([Head], Head) :- !.
% find the LGU of the head and the rest of the list.
lgu([H1|T], LGU) :-
    lgu(T, TempLGU),
    % Find generalization between head H1 and temporary LGU
    generalization(H1, TempLGU, LGU).

% generalization/3 finds the generalization of two heads, Head1 and Head2, which is represented by GeneralizedHead.
% This predicate is conceptual and will require more complex processing depending on the actual structures of the heads.
generalization(Head1, Head2, GeneralizedHead) :-
    % Ensure the functor names and arities are the same between Head1 and Head2.
    functor(Head1, Name, Arity),
    functor(Head2, Name, Arity),
    functor(GeneralizedHead, Name, Arity),
    % Generalize the arguments of the heads.
    generalize_args(Arity, Head1, Head2, GeneralizedHead).

% no more arguments to generalize.
generalize_args(0, _, _, _) :- !.
% generalize the corresponding arguments of the heads.
generalize_args(N, Head1, Head2, GeneralizedHead) :-
    arg(N, Head1, Arg1),
    arg(N, Head2, Arg2),
    % If the corresponding arguments are equal, use them. Otherwise, create a new variable.
    (Arg1 = Arg2 -> arg(N, GeneralizedHead, Arg1); arg(N, GeneralizedHead, _)),
    % Continue with the next argument.
    N1 is N - 1,
    generalize_args(N1, Head1, Head2, GeneralizedHead).

% transform_and_combine_bodies/3 takes a list of clause heads and bodies, a new head, and produces a combined body representing all the original bodies.
% The new body is created according to the transformations required by the new head.
transform_and_combine_bodies([(Head:-Body)|T], NewHead, CombinedBodies) :-
    % Transform the body according to the new head.
    transform(Head, NewHead, Body, TransformedBody),
    % Combine the transformed body with the rest.
    combine_bodies(T, NewHead, TransformedBody, CombinedBodies).

/* OLD
% Define predicate combine_clauses to merge multiple Prolog clauses with the same head.
% It receives a list of clauses as input and returns a combined clause.
combine_clauses([Clause], Clause) :- !.  % If there's only one clause, return it as is.
combine_clauses(Clauses, (Head :- Body)) :-  % If there are multiple clauses, combine them.
    Clauses = [(Head :- FirstBody)|RestClauses],  % Decompose the list into the first clause and the rest.
    combine_bodies(RestClauses, FirstBody, Body).  % Combine the bodies of all the clauses.

% Helper predicate to combine the bodies of a list of clauses.
% The base case is when there are no more clauses to combine; the combined body is the current body.
combine_bodies([], Body, Body).
combine_bodies([(Head :- CurrentBody)|RestClauses], PrevBody, Body) :-
    % Combine the current body with the previous body using a conjunction (,).
    combine_two_bodies(PrevBody, CurrentBody, CombinedBody),
    % Recursively combine the rest of the bodies.
    combine_bodies(RestClauses, CombinedBody, Body).

% Predicate to combine two bodies.
% Handles the combination of different Prolog constructs like conjunctions, disjunctions, etc.
combine_two_bodies((A, B), (C, D), (A, B, C, D)) :- !.  % Combine conjunctions.
combine_two_bodies((A; B), (C; D), (A; B; C; D)) :- !.  % Combine disjunctions.
combine_two_bodies(A, B, (A, B)).  % Combine simple terms using conjunction.
*/

% if there are no more bodies, the accumulated Combined is the final CombinedBodies.
combine_bodies([], _, Combined, Combined).
% combine the transformed body with the accumulated bodies.
combine_bodies([(Head:-Body)|T], NewHead, Acc, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    % Create a disjunction between the accumulated bodies and the transformed body.
    NewAcc = (Acc;TransformedBody),
    combine_bodies(T, NewHead, NewAcc, CombinedBodies).

% combine_code/3 combines Guard and Body to produce either Guard, Body, or a conjunction of both, depending on the values of Guard and Body.
combine_code(Guard, Body, Guard) :- Body==true, !.
combine_code(Guard, Body, Body) :- Guard==true, !.
combine_code(Guard, Body, (Guard, Body)).

% create_unifier/3 creates a unification code that unifies OneHead with NewHead.
% If OneHead and NewHead are structurally equal, then they are unified and the unification Guard is 'true'.
% Otherwise, the unification code is 'metta_unify(OneHead,NewHead)'.

create_unifier(OneHead,NewHead,Guard):- OneHead=@=NewHead,OneHead=NewHead,!,Guard=true.
create_unifier(OneHead,NewHead,Guard):- compound(OneHead),
  compound_name_list(OneHead,_,Args1),
  compound_name_list(NewHead,_,Args2),
  create_unifier_goals(Args1,Args2,Guard),!.
create_unifier(OneHead,NewHead,u(OneHead,NewHead)).

create_unifier_goals([V1],[V2],u(V1,V2)):-!.
create_unifier_goals([V1|Args1],[V2|Args2],RightGuard):-!,
  create_unifier_goals(Args1,Args2,Guard),
  combine_code(u(V1,V2),Guard,RightGuard).
create_unifier_goals([],[],true).


% transform/4 combines unification code with Body to produce NewBody according to the transformations required by NewHead.
% It uses create_unifier/3 to generate the unification code between OneHead and NewHead.
transform(OneHead, NewHead, Body, NewBody):- create_unifier(OneHead,NewHead,Guard),
   combine_code(Guard,Body,NewBody).



compile_for_assert_eq(_Eq,H,B,Result):-
  compile_for_assert(H,B,Result), !.

:- dynamic(metta_compiled_predicate/3).

same(X,Y):- X =~ Y.



