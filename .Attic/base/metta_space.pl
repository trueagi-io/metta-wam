:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- ensure_loaded(metta_compiler).
%:- ensure_loaded(metta_compiler).
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(swi_flybase).

:- multifile(is_pre_statistic/2).
:- dynamic(is_pre_statistic/2).
save_pre_statistic(Name):- is_pre_statistic(Name,_)-> true; (statistics(Name,AS),term_number(AS,FN),assert(is_pre_statistic(Name,FN))).
pre_statistic(N,V):- is_pre_statistic(N,V)-> true ; V = 0.
post_statistic(N,V):- statistics(N,VV),term_number(VV,FV),pre_statistic(N,WV), V0 is FV-WV, (V0<0 -> V = 0 ; V0=V).
term_number(T,N):- sub_term(N,T),number(N).


call_match([G]):-!, call(G).
call_match([G|GG]):- !, call(G), call_match(GG).
call_match(G):- call(G).


:- dynamic(repeats/1).
:- dynamic(not_repeats/1).
assert_new(P):- call(P),!,assert_new1(repeats(P)).
assert_new(P):- assert(P), flag(assert_new,TA,TA+1),assert_new1(not_repeats(P)),!.

retract1(P):- \+ call(P),!.
retract1(P):- ignore(\+ retract(P)).

assert_new1(P):- \+ \+ call(P),!.
assert_new1(P):- assert(P).


:- dynamic(fb_pred/3).
:- dynamic(mod_f_a/3).
decl_m_fb_pred(Mod,Fn,A):- var(Mod),!,mod_f_a(Mod,Fn,A).
decl_m_fb_pred(Mod,Fn,A):- mod_f_a(Mod,Fn,A)->true;(dynamic(Mod:Fn/A),assert(mod_f_a(Mod,Fn,A))).
:- dynamic(fb_pred_file/3).
decl_fb_pred(Fn,A):-
   (fb_pred(Fn,A)-> true; (dynamic(Fn/A),assert(fb_pred(Fn,A)))),
   ignore((nb_current(loading_file,File),
    (fb_pred_file(Fn,A,File)-> true; assert(fb_pred_file(Fn,A,File))))).
% Import necessary libraries
:- use_module(library(readutil)).


skip(_).

% ===============================
% MeTTa Python incoming interface
% ===============================

  
% Clear all atoms from a space
'clear-atoms'(SpaceNameOrInstance) :- 
  debug_metta(['clear-atoms',SpaceNameOrInstance]),
  space_type_method(Type,clear_space,Method), call(Type,SpaceNameOrInstance),!,
  debug_metta(['type-method',Type,Method]),
  call(Method,SpaceNameOrInstance).

% Add an atom to the space
'add-atom'(SpaceNameOrInstance, Atom) :- 
    debug_metta(['add-atom',SpaceNameOrInstance, Atom]),
    space_type_method(Type,add_atom,Method), call(Type,SpaceNameOrInstance),!,
    debug_metta(['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom).

% remove an atom from the space
'remove-atom'(SpaceNameOrInstance, Atom) :- 
    debug_metta(['remove-atom',SpaceNameOrInstance, Atom]),
    space_type_method(Type,remove_atom,Method), call(Type,SpaceNameOrInstance),!,
    debug_metta(['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom).

% Add an atom to the space
'replace-atom'(SpaceNameOrInstance, Atom, New) :- 
    debug_metta(['replace-atom',SpaceNameOrInstance, Atom, New]),
    space_type_method(Type,replace_atom,Method), call(Type,SpaceNameOrInstance),!,
    debug_metta(['type-method',Type,Method]),
    call(Method,SpaceNameOrInstance,Atom, New).

% Count atoms in a space
'atom-count'(SpaceNameOrInstance, Count) :-
    debug_metta(['atom-count',SpaceNameOrInstance]),
    space_type_method(Type,atom_count,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance,Count),
    debug_metta(['type-method-result',Type,Method,Count]).

% Fetch all atoms from a space
'get-atoms'(SpaceNameOrInstance, AtomsL) :-
    debug_metta(['get-atoms',SpaceNameOrInstance]),
    space_type_method(Type,get_atoms,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, AtomsL),
    length(AtomsL,Count),
    debug_metta(['type-method-result',Type,Method,Count]).

% Iterate all atoms from a space
'atoms_iter'(SpaceNameOrInstance, Iter) :-
    debug_metta(['atoms_iter',SpaceNameOrInstance]),
    space_type_method(Type,atoms_iter,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, Iter),
    debug_metta(['type-method-result',Type,Method,Iter]).

% Match all atoms from a space
'atoms_match'(SpaceNameOrInstance, Atoms, Template, Else) :-
    space_type_method(Type,atoms_match,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, Atoms, Template, Else),
    debug_metta(['type-method-result',Type,Method,Atoms, Template, Else]).


% Query all atoms from a space
'space_query'(SpaceNameOrInstance, QueryAtom, Result) :-
    space_type_method(Type,query,Method), call(Type,SpaceNameOrInstance),!,
    call(Method,SpaceNameOrInstance, QueryAtom, Result),
    debug_metta(['type-method-result',Type,Method,Result]).
    

subst_pattern_template(SpaceNameOrInstance, Pattern, Template) :-
    debug_metta([subst_pattern_template,SpaceNameOrInstance, Pattern, Template]),
    'atoms_match'(SpaceNameOrInstance, Pattern, Template, []).
    
/*
space_query_vars(SpaceNameOrInstance, Query, Vars) :- is_as_nb_space(SpaceNameOrInstance),!,
    fetch_or_create_space(SpaceNameOrInstance, Space),
    call_metta(Space,Query,Vars).
*/


was_asserted_space('&flybase').

is_asserted_space(X):- was_asserted_space(X).
is_asserted_space(X):-          \+ is_as_nb_space(X), \+ py_named_space(X),!.

is_python_space_not_prolog(X):- \+ is_as_nb_space(X), \+ is_asserted_space(X).

:- dynamic(is_python_space/1).

py_named_space('&self').
py_named_space('&vspace').
is_as_nb_space('&nb').
is_as_nb_space(N):- is_nb_space(N).
%is_python_space(X):- python_object(X).

ensure_space(_N,_V):- fail.

% ===============================
% Clause Database interface
% ===============================
%debug_metta(Call):- skip(Call).
if_metta_debug(Goal):- getenv('VSPACE_VERBOSE','2'),!,ignore(call(Goal)).
if_metta_debug(_):-!.
if_metta_debug(Goal):- !,ignore(call(Goal)).
debug_metta(Term):- notrace(if_metta_debug((format('~N; ~@~n',[write_src(Term)])))).
debug_metta(Msg,Term):- notrace(if_metta_debug((format('~N; ~w: ~@~n',[Msg,write_src(Term)])))),!.

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
metta_assertdb_ls(KB):-listing(KB:for_metta/2).
metta_assertdb_add(KB,New):- decl_m_fb_pred(KB,for_metta,2), MP = KB:for_metta(KB,New), assert_new(MP).
metta_assertdb_rem(KB,Old):- metta_assertdb_del(KB,Old).
metta_assertdb_del(KB,Old):- decl_m_fb_pred(KB,for_metta,2), MP = KB:for_metta(KB,Old),
  copy_term(MP,Copy), clause(MP,true,Ref), MP=@= Copy, !, erase(Ref). % ,metta_assertdb('DEL',Old).
metta_assertdb_replace(KB,Old,New):- metta_assertdb_del(KB,Old), metta_assertdb_add(KB,New).
metta_assertdb_count(KB,Count):-
 must_det_ll((
  decl_m_fb_pred(KB,for_metta,2), full_symbol_count(SL1),
  MP = KB:for_metta(_,_),
  predicate_property(MP,number_of_clauses(SL2)),
  predicate_property(MP,number_of_rules(SL3)),
  %metta_assertdb_ls(KB),
  Count is SL1 + SL2 - SL3)),!.
metta_assertdb_count(_KB,0):-!.
%metta_assertdb_count(KB,Count):- writeln(metta_assertdb_count_in(KB,Count)), findall(Atom,for_metta(KB,Atom),AtomsL),length(AtomsL,Count),writeln(metta_assertdb_count_out(KB,Count)).
metta_assertdb_iter(KB,Atoms):- decl_m_fb_pred(KB,for_metta,2), KB:for_metta(KB,Atoms).



metta_iter_bind(KB,Query,Vars,VarNames):-
  term_variables(Query,QVars),
  align_varnames(VarNames,Vars),
  TV = debug_metta(['match',KB,Query,QVars,Vars,VarNames]),
%  \+ \+ (numbervars(TV,0,_,[]),print(tv=TV),nl),
  ignore(QVars=Vars),
%  \+ \+ (numbervars(TV,0,_,[]),print(qv=TV),nl),
  \+ \+ (%numbervars(TV,0,_,[]),
         writeq(av=TV),nl),
  space_query_vars(KB,Query,TF),TF\=='False'.


% Query from hyperon.base.GroundingSpace
space_query_vars(KB,Query,Vars):- is_asserted_space(KB),!,
    decl_m_fb_pred(KB,for_metta,2), 
    call_metta(KB,Query,Vars),
    debug_metta('RES',space_query_vars(KB,Query,Vars)).


metta_assertdb_get_atoms(KB,AtomsL):- decl_m_fb_pred(KB,for_metta,2), findall(Atom,KB:for_metta(KB,Atom),AtomsL).
/*

%metta_assertdb_iter_bind(KB,Query,Template,AtomsL):- decl_m_fb_pred(KB,for_metta,2), findall(Template,KB:for_metta(KB,Query),AtomsL).
metta_assertdb_iter_bind(KB,Query,Vars):-
  ignore(term_variables(Query,Vars)),
  print(metta_assertdb(['match',KB,Query,Vars])),nl,
  decl_m_fb_pred(KB,for_metta,2), (KB:for_metta(KB,Query)*->true;call_metta_assertdb(KB,Query,Vars)),
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


call_metta( KB,Query,_Vars):- KB:for_metta(KB,Query).
call_metta(_KB,Query,_Vars):- metta_to_pyswip([],Query,Call),!,
  print(user:Call),nl,user:call(Call).

metta_to_pyswip(_PS,Query,Call):- var(Query),!,Call=Query.
metta_to_pyswip(_PS,Query,Call):- \+ compound(Query),!,Call=Query,!.
metta_to_pyswip(PS,Query,Call):- is_list(Query),Query=[Q|Uery],!,cmpd_to_pyswip(PS,Q,Uery,Call).
metta_to_pyswip(PS,Query,Call):- Query=..[Q|Uery], cmpd_to_pyswip(PS,Q,Uery,Call).

cmpd_to_pyswip(PS,Q,Uery,Call):- atom(Q),maplist(metta_to_pyswip([Q|PS]),Uery,Cery),Call=..[Q|Cery].
cmpd_to_pyswip(PS,"and",Uery,Call):- maplist(metta_to_pyswip(PS),Uery,Args),list_to_conjuncts(Args,Call).





% ===============================
%       PRINTERS
% ===============================


pp_sax(V) :- is_final_write(V),!.
pp_sax(S) :-  \+ allow_concepts,!, write_src(S).
pp_sax(S) :- is_englishy(S),!,print_concept("StringValue",S).
pp_sax(S) :- symbol_length(S,1),symbol_string(S,SS),!,print_concept("StringValue",SS).
pp_sax(S) :- is_an_arg_type(S,T),!,print_concept("TypeNode",T).
pp_sax(S) :- has_type(S,T),!,format('(~wValueNode "~w")',[T,S]).
pp_sax(S) :- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,(Aft>0->format('(~wValueNode "~w")',[Type,S]);format('(TypeNode "~w")',[Type])).
pp_sax(S) :- print_concept("ConceptNode",S).

print_concept( CType,V):- allow_concepts, !, write("("),write(CType),write(" "),ignore(with_concepts(false,write_src(V))),write(")").
print_concept(_CType,V):- ignore(write_src(V)).
write_val(V):- number(V),!, write_src(V).
write_val(V):- compound(V),!, write_src(V).
write_val(V):- write('"'),write(V),write('"').

% Base case: atoms are printed as-is.
pp_as(V) :- \+ \+ pp_sex(V),flush_output.
pp_sex(V) :- is_final_write(V),!.
pp_sex('!'(V)) :- write('!'),!,pp_sex(V).
%pp_sex('') :- format('(EmptyNode null)',[]).
pp_sex('') :- format('()',[]).
pp_sex([]):-  !, write('()').
pp_sex('='(N,V)):- allow_concepts, !, format("~N;; ~w == ~n",[N]),!,pp_sex(V).
pp_sex(V) :- (number(V) ; is_dict(V)), !, print_concept('ValueAtom',V).
pp_sex(V) :- (symbol(V),symbol_number(V,N)), !, print_concept('ValueAtom',N).
pp_sex(S) :- symbol(S), always_dash_functor(S,D), pp_sax(D),!.
pp_sex(S) :- string(S),!, print_concept('StringValue',S).
% Lists are printed with parentheses.
pp_sex(V) :- \+ compound(V), !, format('~p',[V]).
pp_sex(V) :- V = '$VAR'(_), !, format('$~p',[V]).
pp_sex('!'(V)) :- write('!'),!,pp_sex(V).
pp_sex(listOf(S,_)) :- !,pp_sex(listOf(S)).
pp_sex(listOf(S)) :- !,format('(ListValue ~@)',[pp_sex(S)]).

pp_sex([H|T]) :- \+ no_src_indents, atom(H),member(H,['If','cond','let','let*']),!,
  with_indents(true,w_proper_indent(2,w_in_p(pp_sexi([H|T])))).

pp_sex([H|T]) :- is_list(T), length(T,Args),Args =< 2, fail,
   wots(SS,((with_indents(false,(write('('), pp_sex(H), print_list_as_sexpression(T), write(')')))))),
   ((atom_length(SS,Len),Len < 20) ->write(SS);
      with_indents(true,w_proper_indent(2,w_in_p(pp_sexi([H|T]))))),!.
/*

pp_sex([H|T]) :- is_list(T),atom(H),upcase_atom(H,U),downcase_atom(H,U),!,
   with_indents(false,(write('('), pp_sex(H), print_list_as_sexpression(T), write(')'))).

%pp_sex([H,B,C|T]) :- T==[],!,
%  with_indents(false,(write('('), pp_sex(H), print_list_as_sexpression([B,C]), write(')'))).
*/
pp_sex(V) :- no_src_indents,!,pp_sexi(V).

pp_sex(V) :- w_proper_indent(2,w_in_p(pp_sexi(V))).

no_src_indents:- option_else(src_indents,TF,true),!,TF==false.

pp_sexi(V) :- is_final_write(V),!.
pp_sexi([H|T]) :- is_list(T),!,
   write('('), pp_sex(H), print_list_as_sexpression(T), write(')').
% Compound terms.
%pp_sex(Term) :- compound(Term), Term =.. [Functor|Args], write('('),format('(~w ',[Functor]), write_args_as_sexpression(Args), write(')').
%pp_sex(Term) :- Term =.. ['=',H|Args], length(Args,L),L>2, write('(= '),  pp_sex(H), write('\n\t\t'), maplist(pp_sex(2),Args).
pp_sexi(Term) :- Term==[],!,write('()').
pp_sexi(Term) :- compound_name_arity(Term,F,0),!,pp_sexi([F]).
pp_sexi(Term) :- Term =.. [Functor|Args], always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.
pp_sexi(Term) :- allow_concepts, Term =.. [Functor|Args], format('(EvaluationLink (PredicateNode "~w") (ListLink ',[Functor]), write_args_as_sexpression(Args), write('))'),!.
pp_sexi(Term) :- Term =.. [Functor|Args],
   always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.

pp_sex(2,Result):- write('\t\t'),pp_sex(Result).


current_column(Column) :- current_output(Stream), line_position(Stream, Column),!.
current_column(Column) :- stream_property(current_output, position(Position)), stream_position_data(column, Position, Column).
min_indent(Sz):- current_column(Col),Col>Sz,nl,indent_len(Sz).
min_indent(Sz):- current_column(Col),Need is Sz-Col,indent_len(Need),!.
min_indent(Sz):- nl, indent_len(Sz).
indent_len(Need):- forall(between(1,Need,_),write(' ')).

w_proper_indent(N,G):-
  flag(w_in_p,X,X), %(X==0->nl;true),
  XX is (X*2)+N,setup_call_cleanup(min_indent(XX),G,true).
w_in_p(G):- setup_call_cleanup(flag(w_in_p,X,X+1),G,flag(w_in_p,_,X)).


always_dash_functor(A,B):- once(dash_functor(A,B)),A\=@=B,!.
always_dash_functor(A,A).

dash_functor(A,C):- \+ symbol(A),!,C=A.
dash_functor(A,C):- p2m(A,B),A\==B,!,always_dash_functor(B,C).
dash_functor(Functor,DFunctor):-
   symbol(Functor), atomic_list_concat(L,'-',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'-',DFunctor).
dash_functor(Functor,DFunctor):- fail,
   symbol(Functor), atomic_list_concat(L,'_',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'-',DFunctor).
dash_functor(Functor,DFunctor):-
   symbol(Functor), atomic_list_concat(L,'_',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'_',DFunctor).

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

% Print arguments of a compound term.
write_args_as_sexpression([]).
write_args_as_sexpression([H|T]) :- write(' '), pp_sex(H), write_args_as_sexpression(T).

% Print the rest of the list.
print_list_as_sexpression([]).
%print_list_as_sexpression([H]):- w_proper_indent(pp_sex(H)),!.
print_list_as_sexpression([H|T]):- write(' '), pp_sex(H), print_list_as_sexpression(T).

call_sexpr(S):- writeln(call=S).


:- dynamic(fb_pred/2).

full_symbol_count(SL):- flag(total_loaded_atoms,SL,SL),SL>1,!.
full_symbol_count(SL):- findall(NC,(fb_pred(F,A),metta_stats(F,A,NC)),Each), sumlist(Each,SL).

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
   writeln('\n\n\n\n\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   writeln('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   full_symbol_count(SL),
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
symbol_contains(T,TT):- atom_contains(T,TT).
*/

