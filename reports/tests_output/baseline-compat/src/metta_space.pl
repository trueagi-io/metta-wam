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



:- ensure_loaded(metta_compiler).
%:- ensure_loaded(metta_compiler).
% TODO move non flybase specific code between here and the compiler
%:- ensure_loaded(flybase_main).

:- multifile(is_pre_statistic/2).
:- dynamic(is_pre_statistic/2).
save_pre_statistic(Name):- is_pre_statistic(Name,_)-> true; (statistics(Name,AS),term_number(AS,FN),
              pfcAdd_Now(is_pre_statistic(Name,FN))).
pre_statistic(N,V):- is_pre_statistic(N,V)-> true ; V = 0.
post_statistic(N,V):- statistics(N,VV),term_number(VV,FV),pre_statistic(N,WV), V0 is FV-WV, (V0<0 -> V = 0 ; V0=V).
term_number(T,N):- sub_term(N,T),number(N).


call_match([G]):-!, call(G).
call_match([G|GG]):- !, call(G), call_match(GG).
call_match(G):- call(G).

'save-space!'(Space,File):-
 setup_call_cleanup(
  open(File,write,Out,[]),
  with_output_to(Out,
   forall(get_atoms(Space,Atom),
      write_src(Atom))),
  close(Out)).


:- dynamic(repeats/1).
:- dynamic(not_repeats/1).
assert_new(P):- notrace(catch(call(P),_,fail)),!,
  assert_new1(repeats(P)).
assert_new(P):- pfcAdd_Now(P), flag(assert_new,TA,TA+1),assert_new1(not_repeats(P)),!.

retract1(P):- \+ call(P),!.
retract1(P):- ignore(\+ retract(P)).

assert_new1(P):- \+ \+ call(P),!.
assert_new1(P):- pfcAdd_Now(P).


:- dynamic(fb_pred/3).
:- dynamic(mod_f_a/3).
decl_m_fb_pred(Mod,Fn,A):- var(Mod),!,mod_f_a(Mod,Fn,A).
decl_m_fb_pred(Mod,Fn,A):- mod_f_a(Mod,Fn,A)->true;
   (dynamic(Mod:Fn/A),
  pfcAdd_Now(mod_f_a(Mod,Fn,A))).
:- dynamic(fb_pred_file/3).
decl_fb_pred(Fn,A):-
   (fb_pred(Fn,A)-> true; (dynamic(Fn/A),pfcAdd_Now(fb_pred(Fn,A)))),
   ignore((nb_current(loading_file,File),
    (fb_pred_file(Fn,A,File)-> true; pfcAdd_Now(fb_pred_file(Fn,A,File))))).
% Import necessary libraries
:- use_module(library(readutil)).


skip(_).

% ===============================
% MeTTa Python incoming interface
% ===============================

% ============================
% %%%% Atom Manipulations
% ============================

% Clear all atoms from a space
'clear-atoms'(SpaceNameOrInstance) :-
  dout(space,['clear-atoms',SpaceNameOrInstance]),
  space_type_method(Type,clear_space,Method), call(Type,SpaceNameOrInstance),!,
  dout(space,['type-method',Type,Method]),
  call(Method,SpaceNameOrInstance).

% Add an atom to the space
'add-atom'(SpaceNameOrInstance, Atom) :-      % dout(space,['add-atom',SpaceNameOrInstance, Atom]),
 ((   space_type_method(Type,add_atom,Method), call(Type,SpaceNameOrInstance),!,
    if_t((SpaceNameOrInstance\=='&self' ; Type\=='is_asserted_space'),
       dout(space,['type-method',Type,Method,SpaceNameOrInstance,Atom])),
    call(Method,SpaceNameOrInstance,Atom))).
% Add Atom
'add-atom'(Environment, AtomDeclaration, Result):-
      eval_args(['add-atom', Environment, AtomDeclaration], Result).

% remove an atom from the space
'remove-atom'(SpaceNameOrInstance, Atom) :-
    dout(space,['remove-atom',SpaceNameOrInstance, Atom]),
    space_type_method(Type,remove_atom,Method), call(Type,SpaceNameOrInstance),!,
    dout(space,['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom).
% Remove Atom
'remove-atom'(Environment, AtomDeclaration, Result):- eval_args(['remove-atom', Environment, AtomDeclaration], Result).

% Add an atom to the space
'replace-atom'(SpaceNameOrInstance, Atom, New) :-
    dout(space,['replace-atom',SpaceNameOrInstance, Atom, New]),
    space_type_method(Type,replace_atom,Method), call(Type,SpaceNameOrInstance),!,
    dout(space,['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom, New).
% Replace Atom
'atom-replace'(Environment, OldAtom, NewAtom, Result):- eval_args(['atom-replace', Environment, OldAtom, NewAtom], Result).

% Count atoms in a space
'atom-count'(SpaceNameOrInstance, Count) :-
    dout(space,['atom-count',SpaceNameOrInstance]),
    space_type_method(Type,atom_count,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance,Count),
    dout(space,['type-method-result',Type,Method,Count]).
% Count Atoms
'atom-count'(Environment, Count):- eval_args(['atom-count', Environment], Count).

% Fetch all atoms from a space
'get-atoms'(SpaceNameOrInstance, AtomsL) :-
    dout(space,['get-atoms',SpaceNameOrInstance]),
    space_type_method(Type,get_atoms,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, AtomsL),
    %dout(space,['type-method-result',Type,Method,Count]).
    %length(AtomsL,Count),
    true.
% Get Atoms
'get-atoms'(Environment, Atoms):- eval_args(['get-atoms', Environment], Atoms).

% Iterate all atoms from a space
'atoms_iter'(SpaceNameOrInstance, Iter) :-
    dout(space,['atoms_iter',SpaceNameOrInstance]),
    space_type_method(Type,atoms_iter,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, Iter),
    dout(space,['type-method-result',Type,Method,Iter]).

% Match all atoms from a space
'atoms_match'(SpaceNameOrInstance, Atoms, Template, Else) :-
    space_type_method(Type,atoms_match,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, Atoms, Template, Else),
    dout(space,['type-method-result',Type,Method,Atoms, Template, Else]).


% Query all atoms from a space
'space_query'(SpaceNameOrInstance, QueryAtom, Result) :-
    space_type_method(Type,query,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, QueryAtom, Result),
    dout(space,['type-method-result',Type,Method,Result]).


subst_pattern_template(SpaceNameOrInstance, Pattern, Template) :-
    dout(space,[subst_pattern_template,SpaceNameOrInstance, Pattern, Template]),
    'atoms_match'(SpaceNameOrInstance, Pattern, Template, []).

/*
space_query_vars(SpaceNameOrInstance, Query, Vars) :- is_as_nb_space(SpaceNameOrInstance),!,
    fetch_or_create_space(SpaceNameOrInstance, Space),
    call_metta(Space,Query,Vars).
*/ :- dynamic(was_asserted_space/1).

was_asserted_space('&self').
was_asserted_space('&stdlib').
was_asserted_space('&corelib').
was_asserted_space('&flybase').
/*
was_asserted_space('&attentional_focus').
was_asserted_space('&belief_events').
was_asserted_space('&goal_events').
was_asserted_space('&tempset').
was_asserted_space('&concepts').
was_asserted_space('&belief_events').
*/
is_asserted_space(X):- was_asserted_space(X).
is_asserted_space(X):-          \+ is_as_nb_space(X), \+ py_named_space(X),!.

is_python_space_not_prolog(X):- \+ is_as_nb_space(X), \+ is_asserted_space(X).

:- dynamic(is_python_space/1).

:- dynamic(py_named_space/1).

%py_named_space('&self').
%py_named_space('&vspace').
% Function to check if an atom is registered as a space name
:- dynamic is_registered_space_name/1.
is_as_nb_space('&nb').
is_as_nb_space(G):- is_valid_nb_space(G) -> true ;
                 is_registered_space_name(G),nb_current(G,S),is_valid_nb_space(S).

is_nb_space(G):- nonvar(G), is_as_nb_space(G).
% ============================
% %%%% Pattern Matching
% ============================
% Pattern Matching with an else branch
%'match'(Environment, Pattern, Template, ElseBranch, Result):-
%  eval_args(['match', Environment, Pattern, Template, ElseBranch], Result).
% Pattern Matching without an else branch
'match'(Environment, Pattern, Template, Result):-
  eval_args(['match', Environment, Pattern, Template], Result).
%'match'(_Environment, Pattern, Template, Result):- callable(Pattern),!, call(Pattern),Result=Template.
%'match'(_Environment, Pattern, Template, Result):- !, is_True(Pattern),Result=Template.


'new-space'(Space):- gensym('hyperon::space::DynSpace@_',Name),
   fetch_or_create_space(Name, Space).

:- dynamic(is_python_space/1).
% ===============================
% MeTTa Python incoming interface
% ===============================

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_as_nb_space,new_space,init_space).
space_type_method(is_as_nb_space,clear_space,clear_nb_atoms).
space_type_method(is_as_nb_space,add_atom,add_nb_atom).
space_type_method(is_as_nb_space,remove_atom,remove_nb_atom).
space_type_method(is_as_nb_space,replace_atom,replace_nb_atom).
space_type_method(is_as_nb_space,atom_count,atom_nb_count).
space_type_method(is_as_nb_space,get_atoms,get_nb_atoms).
%space_type_method(is_as_nb_space,get_atoms,arg(1)).
space_type_method(is_as_nb_space,atom_iter,atom_nb_iter).
%space_type_method(is_as_nb_space,query,space_nb_query).

% Clear all atoms from a space
clear_nb_atoms(SpaceNameOrInstance) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    nb_setarg(1, Space, []).

% Add an atom to the space
add_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    NewAtoms = [Atom | Atoms],
    nb_setarg(1, Space, NewAtoms).

% Count atoms in a space
atom_nb_count(SpaceNameOrInstance, Count) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    length(Atoms, Count).

% Remove an atom from a space
remove_nb_atom(SpaceNameOrInstance, Atom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    select(Atom, Atoms, UpdatedAtoms),
    nb_setarg(1, Space, UpdatedAtoms).

% Fetch all atoms from a space
get_nb_atoms(SpaceNameOrInstance, Atoms) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms).

% Replace an atom in the space
replace_nb_atom(SpaceNameOrInstance, OldAtom, NewAtom) :-
    fetch_or_create_space(SpaceNameOrInstance, Space),
    arg(1, Space, Atoms),
    ( (select(Found, Atoms, TempAtoms),OldAtom=@=Found)
    ->  NewAtoms = [NewAtom | TempAtoms],
        nb_setarg(1, Space, NewAtoms)
    ;   false
    ).



% Function to confirm if a term represents a space
is_valid_nb_space(Space):- compound(Space),functor(Space,'Space',_).

% Find the original name of a given space
space_original_name(Space, Name) :-
    is_registered_space_name(Name),
    nb_current(Name, Space).

% Register and initialize a new space
init_space(Name) :-
    Space = 'Space'([]),
    asserta(is_registered_space_name(Name)),
    nb_setval(Name, Space).

fetch_or_create_space(Name):- fetch_or_create_space(Name,_).
% Fetch an existing space or create a new one
fetch_or_create_space(NameOrInstance, Space) :-
    (   atom(NameOrInstance)
    ->  (is_registered_space_name(NameOrInstance)
        ->  nb_current(NameOrInstance, Space)
        ;   init_space(NameOrInstance),
            nb_current(NameOrInstance, Space))
    ;   is_valid_nb_space(NameOrInstance)
    ->  Space = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_space(Space).


% Match Pattern in Space and produce Template
'match'(Space, Pattern, Template) :-
    'get-atoms'(Space, Atoms),
    'match-pattern'(Atoms, Pattern, Template).

% Simple pattern match
'match-pattern'([], _, []).
'match-pattern'([H |_T], H, H) :- !.
'match-pattern'([_H| T], Pattern, Template) :- 'match-pattern'(T, Pattern, Template).

%is_python_space(X):- python_object(X).

ensure_space(X,Y):- catch(ensure_space_py(X,Y),_,fail),!.
ensure_space(_N,_V):- fail.

% ===============================
% Clause Database interface
% ===============================
%dout(space,Call):- skip(Call).
if_metta_debug(Goal):- getenv('VSPACE_VERBOSE','2'),!,ignore(call(Goal)).
if_metta_debug(_):-!.
if_metta_debug(Goal):- !,ignore(call(Goal)).
dout(_,_):-!.
dout(W,Term):- notrace(if_metta_debug((format('~N; ~w ~@~n',[W,write_src(Term)])))).

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_asserted_space,new_space,init_space).
space_type_method(is_asserted_space,clear_space,clear_nb_atoms).
space_type_method(is_asserted_space,add_atom,metta_assertdb_add).
space_type_method(is_asserted_space,remove_atom,metta_assertdb_rem).
space_type_method(is_asserted_space,replace_atom,metta_assertdb_replace).
space_type_method(is_asserted_space,atom_count,metta_assertdb_count).
space_type_method(is_asserted_space,get_atoms,metta_assertdb_get_atoms).
space_type_method(is_asserted_space,atom_iter,metta_assertdb_iter).
%space_type_method(is_asserted_space,query,space_nb_query).

%:- dynamic(for_metta/2).
%for_metta(_,T):- fb_pred(F,A),functor(T,F,A),call(T).
metta_assertdb_ls(KB):-
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,_],
  listing(MP).

metta_assertdb_add(KB,AtomIn):-
 must_det_ll((subst_vars(AtomIn,Atom),
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,Atom],
  assert_new(MP))).
metta_assertdb_rem(KB,Old):- metta_assertdb_del(KB,Old).
metta_assertdb_del(KB,Atom):- subst_vars(Atom,Old),
  decl_m_fb_pred(user,metta_atom_asserted,2),
   MP = metta_atom(KB,Old),
  copy_term(MP,Copy), clause(MP,true,Ref), MP=@= Copy, !, erase(Ref). % ,metta_assertdb('DEL',Old).
metta_assertdb_replace(KB,Old,New):- metta_assertdb_del(KB,Old), metta_assertdb_add(KB,New).



atom_count_provider(Self,Count):-
    user:loaded_into_kb(Self,Filename),
     once(user:asserted_metta_pred(Mangle,Filename)),
     mangle_iz(Mangle,Iz),
     member(P,[Mangle,Iz]),
     between(2,8,Arity),
     functor(Data,P,Arity),
     predicate_property(Data,number_of_clauses(CC)),
     predicate_property(Data,number_of_rules(RC)),
     Count is CC - RC.

atom_count_provider(KB,Count):-
 must_det_ll((
  AMA = metta_atom_asserted,
  decl_m_fb_pred(user,AMA,2),
  MP =.. [AMA,KB,_],
  predicate_property(MP,number_of_clauses(SL2)),
  predicate_property(MP,number_of_rules(SL3)),
  %metta_assertdb_ls(KB),
      full_atom_count(SL1),
  Count is SL1 + SL2 - SL3)),!.

metta_assertdb_count(KB,Count):-
    findall(C,atom_count_provider(KB,C),CL),
    sumlist(CL,Count).



%metta_assertdb_count(KB,Count):- writeln(metta_assertdb_count_in(KB,Count)), findall(Atom,for_metta(KB,Atom),AtomsL),length(AtomsL,Count),writeln(metta_assertdb_count_out(KB,Count)).
metta_assertdb_iter(KB,Atoms):-
     MP =.. [metta_atom,KB,Atoms],
     call(MP).



metta_iter_bind(KB,Query,Vars,VarNames):-
  term_variables(Query,QVars),
  align_varnames(VarNames,Vars),
  TV = dout(space,['match',KB,Query,QVars,Vars,VarNames]),
%  \+ \+ (numbervars(TV,0,_,[]),print(tv=TV),nl),
  ignore(QVars=Vars),
%  \+ \+ (numbervars(TV,0,_,[]),print(qv=TV),nl),
  \+ \+ (%numbervars(TV,0,_,[]),
         writeq(av=TV),nl),
  space_query_vars(KB,Query,TF),TF\=='False'.


% Query from hyperon.base.GroundingSpace
space_query_vars(KB,Query,Vars):- is_asserted_space(KB),!,
    decl_m_fb_pred(user,metta_atom_asserted,2),
    call_metta(KB,Query,Vars),
    dout('RES',space_query_vars(KB,Query,Vars)).


metta_assertdb_get_atoms(KB,Atom):- metta_atom(KB,Atom).
/*

%metta_assertdb_iter_bind(KB,Query,Template,AtomsL):-
decl_m_fb_pred(user,metta_atom_asserted,2), findall(Template,metta_atom(KB,Query),AtomsL).
metta_assertdb_iter_bind(KB,Query,Vars):-
  ignore(term_variables(Query,Vars)),
  print(metta_assertdb(['match',KB,Query,Vars])),nl,
     AMA = metta_atom_asserted,
     decl_m_fb_pred(user,AMA,2),
     MP =.. [AMA,KB,Query],

  (MP*->true;call_metta_assertdb(KB,Query,Vars)),
  metta_assertdb('RES',metta_assertdb_iter_bind(KB,Query,Vars)).
%metta_assertdb_iter_bind(KB,Atom,Template):- metta_assertdb_stats, findall(Template,metta_assertdb_iter(KB,Atom),VarList).

metta_assertdb_iter_bind(KB,Atoms,Vars):-
  metta_assertdb_stats,
  term_variables(Atoms,AVars),
  metta_assertdb_iter(KB,Atoms), ignore(AVars = Vars).
*/


align_varnames(VarNames,Vars):-
  list_to_set(VarNames,NameSet),
  merge_named_vars(NameSet,VarNames,Vars).

merge_named_vars([],_VarNames,_Vars):-!.
merge_named_vars([N|NameSet],VarNames,Vars):-
  merge_named(N,_V,VarNames,Vars),
  merge_named_vars(NameSet,VarNames,Vars).
%merge_named_vars(_,_,_).

merge_named(_,_,[],[]):-!.
merge_named(N,V,[N|VarNames],[V|Vars]):-
  merge_named(N,V,VarNames,Vars).


call_metta( KB,Query,_Vars):- metta_atom(KB,Query).
call_metta(_KB,Query,_Vars):- metta_to_pyswip([],Query,Call),!,
  %print(user:Call),nl,
    user:call(Call).

metta_to_pyswip(_PS,Query,Call):- var(Query),!,Call=Query.
metta_to_pyswip(_PS,Query,Call):- \+ compound(Query),!,Call=Query,!.
metta_to_pyswip(PS,Query,Call):- is_list(Query),Query=[Q|Uery],!,cmpd_to_pyswip(PS,Q,Uery,Call).
metta_to_pyswip(PS,Query,Call):- Query=..[Q|Uery], cmpd_to_pyswip(PS,Q,Uery,Call).

cmpd_to_pyswip(PS,Q,Uery,Call):- atom(Q),maplist(metta_to_pyswip([Q|PS]),Uery,Cery),Call=..[Q|Cery].
cmpd_to_pyswip(PS,"and",Uery,Call):- maplist(metta_to_pyswip(PS),Uery,Args),list_to_conjuncts(Args,Call).


'show-metta-def'(Pred, []):-
  'get-metta-src'(Pred,[_|SrcL]),
  maplist(write_src_nl,SrcL).

write_src_nl(Src):- format('~N'),write_src(Src),format('~N').

%'get-metta-src'(Pred,[Len|SrcL]):- findall(['AtomDef',Src],'get-metta-src1'(Pred,Src),SrcL), length(SrcL,Len).
'get-metta-src'(Pred,[Len|SrcL]):- findall(Src,'get-metta-src1'(Pred,Src),SrcL), length(SrcL,Len).
'get-metta-src1'(Pred,Src):-
  current_self(Space),
  metta_atom(Space,F,A,List),
  once((sub_var(Pred,A)->Src = [F,A,List];sub_var(Pred,F)->Src = [F,A|List])).

% is a quine
'AtomDef'(X,['AtomDef',X]).


sort_on(C,R,A,B):- (A==B-> R= (=) ; must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA+A,BB+B)))),!.
tokens(X,VL):- unaccent_atom(X,A),!, findall(E,(is_tokenizer(T),call(T,A,E)),L),predsort(sort_on(length_fw_len),L,S),last(S,VL).

length_fw_len([W|List],L+WL):- length(List,L),atom_length(W,WL).

print_token_args:- make,
   fb_arg(X),tokens(X,A0),
   exclude(is_dash,A0,A),tterm(A,AT),writeq(X),write('    '),writeq(AT),write('  '),write_src(A),nl,fail.
is_dash('_').
is_dash('-').
tterm([A],A):-!.
tterm([A,':',B|M],BA):- atom(A),!,BA=..[A,B|M].
tterm([A,B|M],BA):- atom(B),!,BA=..[B,A|M].
tterm([A|B],BA):- atom(A),!,BA=..[B|A].
tterm(A,A).

is_tokenizer(into_list).
is_tokenizer(to_case_break_atoms).
is_tokenizer(atom_to_stem_list).
is_tokenizer(tokenize_atom).
%is_tokenizer(double_metaphone).



is_an_arg_type(S,T):- flybase_identifier(S,T),!.
has_type(S,Type):- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,Aft>0.


call_sexpr(S):- once_writeq_nl(call_sexpr(S)).
%call_sexpr(Space,Expr,Result):-

:- dynamic(fb_pred/2).

full_atom_count(SL):- flag(total_loaded_atoms,SL,SL),SL>1,!.
full_atom_count(SL):- findall(NC,(fb_pred(F,A),metta_stats(F,A,NC)),Each), sumlist(Each,SL).

heartbeat :-
    % Get the current time and the last printed time
    get_time(CurrentTime),
    % Check if the global variable is set
    (   nb_current(last_printed_time, _)
    ->  true
    ;   nb_setval(last_printed_time, CurrentTime)
    ),

    nb_getval(last_printed_time, LastPrintedTime),

    % Calculate the difference
    Diff is CurrentTime - LastPrintedTime,

    % If the difference is greater than or equal to 60 seconds (1 minute)
    (   Diff >= 60
    ->  % Print the heartbeat message and update the last printed time
        metta_stats
    ;   % Otherwise, do nothing
        true
    ).

metta_stats:- gc_now,
   writeln('\n\n\n\n\n\n;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   writeln(';~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   full_atom_count(SL),
   format("~N~n; Total\t\tAtoms (Atomspace size): ~`.t ~D~108|~n",[SL]),
   get_time(CurrentTime), nb_setval(last_printed_time, CurrentTime),
   post_statistic(memory,Mem),
   post_statistic(atom_space,AS),
   post_statistic(cputime,TotalSeconds),
   post_statistic(atoms,Concepts),
   flag(assert_new,CTs,CTs),
   post_statistic(stack,StackMem),


   PM is Mem + StackMem,
   RM is Mem-AS,
   PA is RM//(SL+1),
   APS is 60*floor(SL/(TotalSeconds+1)),
   ACS is AS//(Concepts+1),

   pl_stats('SymbolAtoms',Concepts),
   pl_stats('Random samples',CTs),
   skip((pl_stats('Bytes Per Atom (Average)',PA), pl_stats('Bytes Per ConceptNode (Average)',ACS))),
   skip((pl_stats('Relational Memory',RM), pl_stats('ConceptNode Memory',AS))),
   %pl_stats('Queryspace Memory',StackMem),
   %CPU is CPUTime-57600,
   format_time(TotalSeconds, Formatted),
   skip((pl_stats('Atoms per minute',APS))),
   pl_stats('Total Memory Used',PM),
   pl_stats('Runtime (days:hh:mm:ss)',Formatted),
   nl,nl,!.
metta_stats(F):- for_all(fb_pred(F,A),metta_stats(F,A)).
metta_stats(F,A):- metta_stats(F,A,NC), pl_stats(F/A,NC).
metta_stats(F,A,NC):- functor(P,F,A),predicate_property(P,number_of_clauses(NC)).
pl_stats(Stat):- statistics(Stat,Value),pl_stats(Stat,Value).
pl_stats(Stat,[Value|_]):- nonvar(Value),!, pl_stats(Stat,Value).
pl_stats(Stat,Value):- format("~N;\t\t~@: ~`.t ~@~100|",[format_value(Stat),format_value(Value)]),!.


% AsPred to print the formatted result.
format_value(Value) :- float(Value),!,format("~2f",[Value]),!.
format_value(Bytes) :- integer(Bytes),format_bytes(Bytes, Formatted), write(Formatted).
format_value(Term)  :- format("~w",[Term]).
%  Base case: If the number is 1G or more, show it in gigabytes (G).
format_bytes(Bytes, Formatted) :-  Bytes >= 1073741824, GB is Bytes / 1073741824, format(string(Formatted), '~2fG', [GB]).
% If the number is less than 1G, show it in megabytes (M).
format_bytes(Bytes, Formatted) :- Bytes >= 104857600, Bytes < 1073741824, !, MB is Bytes / 1048576, D is floor(MB), format(string(Formatted), '~DM', [D]).
% If the number is less than 1K, show it in bytes (B).
format_bytes(Bytes, Formatted) :- format(string(Formatted), '~D', [Bytes]).
% % If the number is less than 1M, show it in kilobytes (K).
%format_bytes(Bytes, Formatted) :- Bytes >= 1024, Bytes < 1048576, !, KB is Bytes / 1024, format(string(Formatted), '~0fK', [KB]).

% Convert total seconds to days, hours, minutes, seconds, and milliseconds.
format_time(TotalSeconds, Formatted) :-
    Seconds is floor(TotalSeconds),
    % Get days, remaining seconds
    Days is div(Seconds, 86400),
    Remain1 is mod(Seconds, 86400)-57600,
    format_time(string(Out),'%T',Remain1),
    % Format the result
    format(string(Formatted), '~w:~w', [Days, Out]).

% AsPred to print the formatted time.
print_formatted_time(TotalSeconds) :-
    format_time(TotalSeconds, Formatted),
    writeln(Formatted).


metta_final:-
    save_pre_statistic(memory),
    save_pre_statistic(atoms),
    save_pre_statistic(atom_space).
/*
symbol(X):- atom(X).
symbol_number(S,N):- atom_number(S,N).
symbol_string(S,N):- atom_string(S,N).
symbol_chars(S,N):- atom_chars(S,N).
symbol_length(S,N):- atom_length(S,N).
symbol_concat(A,B,C):- atom_concat(A,B,C).
symbolic_list_concat(A,B,C):- atomic_list_concat(A,B,C).
symbolic_list_concat(A,B):- atomic_list_concat(A,B).
symbol_contains(T,TT):- atom_contains(T,TT).
*/
search_for1(X):-
  forall((metta_atom(_Where,What),contains_var(X,What)),
    (nl,write_src_nl(What))).

search_for2(X):-
  forall((metta_file_src(_Where,What),contains_var(X,What)),
    (nl,write_src_nl(What))).


metta_file_src(Where,What):-
  loaded_into_kb(Where,File), metta_file_buffer(_,What,Vars,File,_Loc),
  ignore(maplist(name_the_var,Vars)).


guess_metta_vars(What):-
  ignore(once((metta_file_buffer(_,What0,Vars,_File,_Loc),
     alpha_unify(What,What0),
     maplist(name_the_var,Vars)))).
name_the_var(N=V):- ignore((atom_concat('_',NV,N),V='$VAR'(NV))).

alpha_unify(What,What0):- What=@=What0,(nonvar(What)->What=What0;What==What0).

