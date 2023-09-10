:- encoding(octet).

:- flush_output.

skip(_).

:- include(swi_support).

:- set_option_value(encoding,octet).
%:- set_option_value(max_per_file,10_000_000).
%:- set_option_value(max_per_file,1_000).
%:- set_option_value(max_per_file,300).
:- set_option_value(max_per_file,inf).


:- set_option_value(max_disk_cache,1000).
:- set_option_value(samples_per_million,100).
:- set_option_value(full_canon,true).
:- setenv('RUST_BACKTRACE',full).

%:- pyswip_dir(Dir),with_cwd(Dir,ensure_loaded(swi_support)).

/*
:- mutifile  column_names/2.
:- mutifile  column_names_ext/2.
:- mutifile  flybase_tables/1.
:- mutifile  guess_rest/4.
:- mutifile  ncRNA_genes_fb_scheme/1.
:- mutifile  numeric_value_p_n/3.
:- mutifile  table_n_type/4.
%:- pyswip_dir(Dir),with_cwd(Dir,ensure_loaded(fb_induced_types)).
*/

:- multifile(is_pre_statistic/2).
:- dynamic(is_pre_statistic/2).
save_pre_statistic(Name):- is_pre_statistic(Name,_)-> true; (statistics(Name,AS),term_number(AS,FN),assert(is_pre_statistic(Name,FN))).
pre_statistic(N,V):- is_pre_statistic(N,V)-> true ; V = 0.
post_statistic(N,V):- statistics(N,VV),term_number(VV,FV),pre_statistic(N,WV), V0 is FV-WV, (V0<0 -> V = 0 ; V0=V).
term_number(T,N):- sub_term(N,T),number(N).


flybase_identifier('FBab', 'aberration').
flybase_identifier('FBal', 'allele').
flybase_identifier('FBba', 'balancer').
flybase_identifier('FBbt', 'anatomy term').
flybase_identifier('FBch', 'chromosome arm').
flybase_identifier('FBcl', 'clone').
flybase_identifier('FBcv', 'controlled vocabulary').
flybase_identifier('FBdv', 'developmental stage term').
flybase_identifier('FBgg', 'gene group').
flybase_identifier('FBgn', 'gene').
flybase_identifier('FBhh', 'human disease').
flybase_identifier('FBig', 'interaction').
flybase_identifier('FBim', 'image').
flybase_identifier('FBlc', 'large dataset metadata').
flybase_identifier('FBmc', 'molecular construct').
flybase_identifier('FBms', 'molecular segment').
flybase_identifier('FBpl', 'probe').
flybase_identifier('FBpp', 'polypeptide').
flybase_identifier('FBrf', 'reference').
flybase_identifier('FBsf', 'sequence feature').
flybase_identifier('FBsn', 'strain').
flybase_identifier('FBst', 'stock').
flybase_identifier('FBtc', 'cell line').
flybase_identifier('FBti', 'transposable element insertion').
flybase_identifier('FBto', 'experimental tools').
flybase_identifier('FBte', 'transgenic element').
flybase_identifier('FBtp', 'transposon'). %flybase_identifier('FBtp', 'transgenic construct or natural transposon').
flybase_identifier('FBtr', 'transcript').

% FlyBase prefixes
atom_prefix(Prefix, flybase, Desc):- flybase_identifier(Prefix, Desc).
% Some common OBO prefixes (Note: these are more generalized and not specific to FlyBase)
atom_prefix('GO', obo, 'Gene Ontology').
atom_prefix('PO', obo, 'Plant Ontology').
atom_prefix('DOID', obo, 'Disease Ontology').
atom_prefix('UBERON', obo, 'Uber-anatomy ontology').
atom_prefix('CHEBI', obo, 'Chemical Entities of Biological Interest').



%./KBs/SUMO-OBO/gene-merged-SUMO.kif
%
%FBbt_00051628

concept_type(Arg,Type):-
   fb_arg(Arg),
   fb_arg_table_n(Arg,Fn,N),
   table_n_type(Fn,N,Type).

good_concept(E1):- var(E1),!,table_columns(F1,P1),nth1(N1,P1,E2),(E1=E2;E1=nth(N1,F1)).
good_concept(E1):- atom(E1),!, is_good_atom_name(E1).
good_concept(E1):- number(E1),!, E1>300.
good_concept(listOf(E1,_)):- good_concept(E1),atom(E1).
good_concept(listOf(E1)):- good_concept(E1),atom(E1).

%:- abolish(maybe_corisponds/2).
:- dynamic(maybe_corisponds/2).


is_good_atom_name(E1):- atom(E1), atom_length(E1,L),L>=2, \+ atom_number(E1,_).

fb_pred_g(F,A):-fb_pred(F,A), \+ skipped_anotations(F).


mine_corisponds(Concept1,Corispondance):-
 fb_arg_table_n(Concept1,Fn1,Nth1),is_good_atom_name(Concept1),
 fb_arg_table_n(Concept1,Fn2,Nth2),
 (Fn1+Nth1)@>(Fn2+Nth2),
 once((table_colnum_type(Fn1,Nth1,Type1),nonvar(Type1),
       table_colnum_type(Fn2,Nth2,Type2),nonvar(Type2))),
 (maybe_corisponds('ConceptMapFn'(Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Type2,Nth2,Fn2/*Arity2*/))
  = Corispondance).

mine_overlaps:-
  retractall(maybe_corisponds(_,_)),
  time(once(mine_overlaps1)),
  skip(mine_overlaps2).

mine_overlaps1:-
  for_all(mine_corisponds(Concept1,How), assert_progress(mine_overlaps1(Concept1),How)).

mine_overlaps2_slow:- 
 % for_all(mine_typelevel_overlaps,true),
  for_all(mine_atomspace_overlaps,true).

mine_typelevel_overlaps:-
  for_all(mine_typelevel_overlaps(Concept1,SC1,SC2),
    assert_progress(mine_typelevel_overlaps(Concept1),maybe_corisponds(SC1,SC2))).

mine_typelevel_overlaps(Concept1,'ConceptMapFn'(Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Type2,Nth2,Fn2/*Arity2*/)):-

  %fail, % Skip over simple type named things

  Type1=Concept1,Type2=Concept1,
  table_columns(Fn1,Atom1), table_columns(Fn2,Atom2),
  fb_pred_g(Fn1,Arity1), fb_pred_g(Fn2,Arity2),
  Fn1@>Fn2, nth1(Nth1,Atom1,Concept1),
  good_concept(Concept1),
  once((nth1(Nth2,Atom2,Concept1),length(Atom1,Arity1),length(Atom2,Arity2))).

mine_atomspace_overlaps:-
  fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2),
  once((functor(Atom1,Fn1,Arity1),functor(Atom2,Fn2,Arity2),
  call(Atom1), arg(Nth1,Atom1,Concept1),good_concept(Concept1), arg(Nth2,Atom2,Concept1),call(Atom2))),
  once((
    table_colnum_type(Fn1,Nth1,Type1),nonvar(Type1),
  table_colnum_type(Fn2,Nth2,Type2),nonvar(Type1))),
  assert_progress(Concept1,maybe_corisponds('ConceptMapFn'(Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(Type2,Nth2,Fn2/*Arity2*/))).

fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2):- 
  fb_pred_g(Fn1,Arity1), fb_pred_g(Fn2,Arity2), Fn1@>=Fn2,
  between(1,Arity1,Nth1),between(1,Arity2,Nth2),
  (Fn1==Fn2-> (Nth1>Nth2); true).
  
fb_two_preds(Fn1,Nth1,Arity1,Fn2,Nth2,Arity2):- 
  fb_pred_g(Fn1,Arity1), fb_pred_g(Fn2,Arity2),Fn1@>Fn2,
  mine_typelevel_overlaps(_,'ConceptMapFn'(_Type1,Nth1,Fn1/*Arity1*/),'ConceptMapFn'(_Type2,Nth2,Fn2/*Arity2*/)).

table_colnum_type(Fn,Nth,Type):- table_n_type(Fn,Nth,TypeC,TypeB),(nonvar(TypeB)->Type=TypeB;Type=TypeC).

make_atom(Fn,Nth,Atom,Arg):- fb_pred_g(Fn,Arity),functor(Atom,Fn,Arity),arg(Nth,Atom,Arg).

synth_conj(QV,(Atom1),(Atom2)):-
  maybe_corisponds('ConceptMapFn'(Type1,Nth1,Fn1),'ConceptMapFn'(Type2,Nth2,Fn2)),
  make_atom(Fn1,Nth1,Atom1,Arg1),
  make_atom(Fn2,Nth2,Atom2,Arg2),
  Fn1\=@=Fn2,
  skip(Type1),skip(Type2),
  Arg1=Arg2,QV=Arg1.

synth_query(Len,Query):- synth_query(_,Len,Query).

synth_query(_,1,[Atom]):- !, make_atom(Atom).
synth_query(QV,N,[Q1,Q2|Query]):-
   M is N -1,
   synth_conj(QV,Q1,Q2),
   (M>1 -> dif(QV,QV2) ; true),
   synth_query(QV2,M,[Q2|Query]),
   all_dif_functors([Q1,Q2|Query]).

all_dif_functors(List):- \+ (select(Q1,List,Rest),member(Q2,Rest),functor(Q1,F1,_),functor(Q2,F2,_), F1==F2, \+ (ok_if_dupped(F1))).
make_atom(Atom):- fb_pred_g(F,A),functor(Atom,F,A).

ok_if_dupped(best_gene_summary).

try_overlaps:- try_overlaps(5).

try_overlaps(N):-
  synth_query(N,Query),
  \+ \+ (call_match(Query),
         pp_fb(grounded=Query),
         ignore(maybe_english(Query))),nl,nl,

  pp_fb(ungrounded='!'(Query)),nl,nl,nl.

no_english(fbrf_pmid_pmcid_doi,_).
no_english(physical_interactions_mitab,8).

maybe_english(Query):-
         extract_concepts(Query,Concepts),!,
         ignore((maybe_english(Query,Concepts))),!.

maybe_english(_Query,Concepts):- select(C,Concepts,Rest),is_englishy(C),member(C2,Rest),is_englishy(C2),!, pp_fb(english=[C,C2]).
maybe_english(_Query,Concepts):- pp_fb(concepts=Concepts), maplist(some_english,Concepts).

is_englishy(C):- \+ atom(C), \+ string(C), !, fail.
is_englishy(C):- split_string(C, ". ", " ", [_,_,_|_]).
is_englishy(C):- atom_contains(C,". ").

some_english(Term):-
  ignore((fb_arg_table_n(C,Fn1,Nth1), \+ no_english(Fn1,Nth1),is_englishy(C),
  make_atom(Fn1,Nth1,Atom,English),
  arg(Nth2,Atom,Term),Nth2\==Nth1,
  call(Atom),English\=='',!,
  pp_fb(Term=English))).

extract_concepts(Query,Concepts):-
   findall(C,(sub_term(C,Query),atomic(C),good_concept(C)),L),
   predsort(longest_first,L,Concepts).

longest_first(R,A,B):- into_len(A,L1),into_len(B,L2),compare(R,L2,L1).
into_len(A,0):- var(A),!.
into_len(A,L):- \+ string(A), !, sformat(S,"~w",[A]),into_len(S,L).
into_len(A,0+A):- atom_contains(A," ").
into_len(A,L+A):- atom_length(A,L1), (L1 == 11 -> L = 0 ; L is - L1).

assert_progress(Concept,Atom):- Atom=..[OP,A1,A2], A1@>A2,!,AtomSwp=..[OP,A2,A1],!,assert_progress(Concept,AtomSwp).
assert_progress(Concept,Atom):- call(Atom),!,pp_fb(already(Concept)=Atom).
assert_progress(Concept,Atom):- pp_fb(assert_progress(Concept)=Atom),assert(Atom).

cleanup_arities:- for_all((fb_pred(F,2),fb_pred(F,N),N>2),retract(fb_pred(F,2))).

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


/*
:- ensure_loaded('./reqs/obo_core/prolog/obo_core/goslim.pl').
:- ensure_loaded('./reqs/obo_metadata/prolog/obo_metadata.pl').
:- ensure_loaded('./reqs/obo_metadata/prolog/obo_metadata/iao_metadata.pl').
:- ensure_loaded('./reqs/obo_metadata/prolog/obo_metadata/oio.pl').
:- ensure_loaded('./reqs/obo_ro/prolog/obo_ro/ro.pl').

:- attach_packs('./reqs',[]).
:- ensure_loaded(library(obo_metadata)).
:- goslim:ensure_loaded(library(obo_core/goslim)).
:- ensure_loaded(library(obo_ro/ro)).

937_381_148
*/
:- dynamic(mod_f_a/3).
decl_m_fb_pred(Mod,Fn,A):- var(Mod),!,mod_f_a(Mod,Fn,A).
decl_m_fb_pred(Mod,Fn,A):- mod_f_a(Mod,Fn,A)->true;(dynamic(Mod:Fn/A),assert(mod_f_a(Mod,Fn,A))).
decl_fb_pred(Fn,A):- fb_pred(Fn,A)-> true; (dynamic(Fn/A),assert(fb_pred(Fn,A))).
% Import necessary libraries
:- use_module(library(readutil)).
:- dynamic(ontology_info/2).
:- dynamic(ontology_info/3).

loaded_from_file_count(X):- flag(loaded_from_file_count,X,X).
incr_file_count(X):- flag(loaded_from_file_count,X,X+1),  flag(total_loaded_atoms,TA,TA+1).

should_cache:- loaded_from_file_count(X), option_else(max_disk_cache,Num,1000), X=<Num.
reached_file_max:- option_value(max_per_file,Y),Y\==inf,loaded_from_file_count(X),X>=Y.
should_fix_args :- fail, \+ should_sample.
should_sample :- should_show_data(_),!.
should_sample :- 
  once(option_value(samples_per_million,Fifty);Fifty=50), loaded_from_file_count(X), Y is X mod 1_000_000,!, Y >= 0, Y =< Fifty,!.
should_show_data(X):- loaded_from_file_count(X), once((X=<13,X>=10); (X>0,(0 is X rem 1_000_000))),
  format(user_error,'~N',[]),
  format(user_output,'~N',[]),
  heartbeat.

assert_OBO(P,X,Y):- assert_OBO(ontology_info(P,X,Y)).
assert_OBO(Fn,Cols):- OBO=..[Fn|Cols], assert_OBO(OBO).
assert_OBO(OBO):-
 functor(OBO,Fn,A),
 ignore(( A>=2,A<700,
 must_det_ll((
  heartbeat,
  OBO=..[Fn|Cols],
  make_assertion(Fn,Cols,Data,OldData),
  functor(Data,FF,AA),
  decl_fb_pred(FF,AA),
  (%call(Data)->true;
   must_det_ll((assert(Data),incr_file_count(_),
     ignore((((should_show_data(X),
       ignore((OldData\==Data,write(oldData(X)),write(=),write_src(OldData))),
       write(newData(X)),write(=),write_src(Data))))),
     ignore((
       fail, option_value(output_stream,OutputStream),
       is_stream(OutputStream),
       should_show_data(X1),X1<1000,must_det_ll((display(OutputStream,Data),writeln(OutputStream,'.')))))))))))),!.

% Convert a function and its arguments into a compound term
into_datum(Fn, [D|DataL], Data):-
    (option_value(pred_va, true) -> Data =.. [Fn,D,DataL]; Data =.. [Fn,D|DataL]).

% Create a new assertion from old data
make_assertion(Fn, Cols, NewData, OldData):-
    into_datum(Fn, Cols, OldData),
    OldData =.. [Fn|Args],
    % skip(if_t(var(ArgTypes), must_det_ll_r((once((length(Args,Len),length(ArgTypes,Len),once((table_columns(Fn,ArgTypes);table_columns(F,ArgTypes))))))))),
    maybe_fix_args(Fn, Args, NewArgs),
    maybe_sample(Fn, NewArgs),
    NewData =.. [Fn|NewArgs], !.

maybe_fix_args( Fn,Args,NewArgs):- should_fix_args, 
  fb_argtypes(Fn,ArgTypes), fix_list_args(Fn,ArgTypes,Args,NewArgs),!.
maybe_fix_args(_Fn,Args,Args).



load_obo_files:-
  %load_obo('./reqs/obonet/tests/data/*.obo'),
  load_obo('./data/ftp.flybase.net/releases/current/precomputed_files/*/*.obo').

% Main entry point
load_obo(Filename) :- \+ atomic(Filename),
  absolute_file_name(Filename,X,[read(exists),extension(['']),file_type(directory),
     file_errors(fail),solutions(first)]), !, load_obo(X).
load_obo(Filename) :- \+ atomic(Filename), !,
  absolute_file_name(Filename,X,[read(exists),extension(['']), file_errors(fail),solutions(first)]), !, load_obo(X).
load_obo(Filename) :-
  atomic(Filename), \+ exists_file(Filename), expand_file_name(Filename,List),
  List\==[], List\==[Filename],
  maplist(load_obo,List).
load_obo(Directory) :-
  atomic(Directory), exists_directory(Directory),
  directory_file_path(Directory, "*.obo", Filename),
  expand_file_name(Filename,List),!,maplist(load_obo,List).
load_obo(Filename) :-
 must_det_ll((
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
    Type = 'OntologyFile',
    assert_OBO(id_type,Id,Type),
    nb_setval(obo_id,Id),nb_setval(obo_type,Type),
    assert_OBO('pathname',Id,Filename),!,
    assert_OBO('basename',Id,BaseName),!,
    assert_OBO('directory',Id,Directory),!,
    setup_call_cleanup(open(Filename, read, Stream),
      process_stream_repeat(Stream),
      close(Stream)))),
 fb_stats.

process_stream_repeat(Stream):-
  repeat,
     nb_current(obo_type,Type),
     nb_current(obo_id, Id),
     once((read_line_to_string(Stream, Line),
     (should_show_data(_) -> writeln(Line); true),
        normalize_space(chars(Chars),Line))),
        Chars\==[],
        once(process_stream_chars(Stream, Type, Chars, Id)),
     ((at_end_of_stream(Stream);reached_file_max) -> ! ; fail).


process_stream(Stream,_Type,_Id) :- (at_end_of_stream(Stream);reached_file_max),!.
process_stream(Stream, Type, Id) :-
  must_det_ll((
    read_line_to_string(Stream, Line), %writeln(Line),
    normalize_space(chars(Chars),Line),
    process_stream_chars(Stream, Type, Chars, Id))).


into_rest(Rest,RestChars,RestStr):-
  string_chars(Str,Rest),
  normalize_space(chars(RestChars),Str),
  string_chars(RestStr,RestChars).


process_stream_chars(_Stream, _, [e,n,d,'_',o,f,'_',f,i,l,e], _):-!.
process_stream_chars(Stream, _, [], _):-!, process_stream(Stream, _, _).

process_stream_chars(Stream, _, ['['|Chars], _):- !,
 must_det_ll(( append(Left,[']'],Chars), atom_chars(Type,Left),!,
  nb_setval(obo_type,Type),
  nop(process_stream(Stream, Type, _Id)))).

process_stream_chars(Stream, Type, Chars, _):-
  get_key(Key,Chars,Rest),Key == id,
  into_rest(Rest,RestChars,_RestStr),
  atom_chars(Id,RestChars), assert_OBO(id_type,Id,Type),
  nb_setval(obo_id,Id),nb_setval(obo_type,Type),
  nop(process_stream(Stream, Type, Id)).

process_stream_chars(Stream, Type, Chars, Id):-
 must_det_ll((
    get_key(Key,Chars,Rest),
    into_rest(Rest,RestChars,RestStr),
    process_rest_line(Type,Id,Key,RestChars,RestStr))),
    nop(process_stream(Stream, Type, Id)).

process_rest_line(Type,Id,Reln,Rest,_):- Reln = id,
   get_some_items([item(Id)],Rest,[]),!, assert_OBO(id_type,Id,Type),!.
process_rest_line(_Type,Id,Ref,_Chars,S):-
   member(Ref,[name,comment]),
   assert_OBO(Ref,Id,S),!.

process_rest_line(Type,Id,Reln,Chars,_):-  Reln = relationship,!,
  must_det_ll((
   key_like_string(KeyLike,Chars,Rest),
    atom_chars(Key,KeyLike),
    into_rest(Rest,RestChars,RestStr),
    process_rest_line(Type,Id,Key,RestChars,RestStr))).

process_rest_line(_Type,Id,Ref,Chars,_):-
    \+ (member(C,Chars),member(C,['!','[','"'])),
    ( \+ member(' ',Chars)-> atom_chars(S,Chars);string_chars(S,Chars)),
    assert_OBO(Ref,Id,S),!.

process_rest_line(_Type,Id,is_a,Chars,Str):-
    member('!',Chars), atomic_list_concat([L,R],'!',Str),
    normalize_space(atom(T),L),normalize_space(string(N),R),
    assert_OBO(is_a,Id,T), assert_OBO(name,T,N),!.

process_rest_line(_Type,Id,Reln,Chars,_):-
  %  member(Reln,[synonym]),
    get_some_items(List,Chars,[]),
    maplist(arg(1),List,Args),
    Assert=..[Reln,Id|Args],
    assert_OBO(Assert),!.

%process_rest_line(_Type,Id,Reln,Chars,_):- get_some_items(List,Chars,[]), maplist(arg(1),List,Args), assert_OBO(Reln,Id,Args).
process_rest_line(Type,Id,Miss,Rest,Str):-
  pp_fb(process_rest_line(Type,Id,Miss,Rest,Str)),!.

/*
Given the DCG rules we've defined, the input

``` OBO

[Term]
id: FBcv:0000391
name: bang sensitive
namespace: phenotypic_class
def: "A phenotype exhibited following mechanical shock and consisting of a brief period of intense, uncoordinated motor activity (legs and wings flailing, abdomen coiling) followed by a prolonged period of paralysis." [FlyBase:FBrf0022877]
synonym: "easily shocked" RELATED [FlyBase:FBrf0022877]
is_a: FBcv:0000389 ! paralytic

```
Would be parsed into the following Prolog terms:
```
[
    bracketed(['Term']),
    key('id'), item('FBcv:0000391'),
    key('name'), item('bang sensitive'),
    key('namespace'), item('phenotypic_class'),
    key('def'), quoted("A phenotype exhibited following mechanical shock and consisting of a brief period of intense, uncoordinated motor activity (legs and wings flailing, abdomen coiling) followed by a prolonged period of paralysis."), bracketed(['FlyBase:FBrf0022877']),
    key('synonym'), quoted("easily shocked"), keyword('RELATED'), bracketed(['FlyBase:FBrf0022877']),
    key('is_a'), item('FBcv:0000389'), named('paralytic')
]
```

*/


get_key(Key)-->key_like_string(Chars),[':'],{atom_chars(Key,Chars)},!.
get_some_items(I)--> [' '],!,get_some_items(I).
get_some_items(_,[],[]):-!.
get_some_items([H|T])-->get_one_item(H),get_some_items(T). get_some_items([])-->[].
get_one_item(I)--> [' '],!,get_one_item(I).
get_one_item(quoted(Item))-->[x,s,d,':'],symbol_or_url(Chars),{atom_chars(Item,[x,s,d,':'|Chars])}.
get_one_item(quoted(Item))-->[h,t,t,p],symbol_or_url(Chars),{string_chars(Item,[h,t,t,p|Chars])}.
get_one_item(quoted(Item))-->[f,t,p],symbol_or_url(Chars),{string_chars(Item,[f,t,p|Chars])}.
get_one_item(quoted(Item))-->['"'],string_until_end_quote(Chars),{string_chars(Item,Chars)}.
get_one_item(named(Item))-->['!'],whs,named_like_string(Chars),{atom_chars(Item,Chars)}.
get_one_item(bracketed(Items))-->['['],whs,items(Items),whs,[']'].
get_one_item(bracketed(Items))-->['{'],whs,items(Items),whs,['}'].
%get_one_item(item(Item))--> whs,key_like_string(Chars),whs,{Chars \==[], atom_chars(Item,Chars)}.
get_one_item(keyword(Keyword))-->whs,id_like_string(Chars),{Chars\==[]},whs,{atom_chars(Keyword,Chars)}.
get_one_item(text(Text))-->named_like_string(Chars),{string_chars(Text,Chars)}.
get_one_item(text(Text),[H|T],[]):- ground([H|T]),string_chars(Text,[H|T]),!.
items([Item|Rest])-->item(Item),whs,[','],whs,items(Rest).
items([Item])-->item(Item),!.
item(Item)-->symbol_or_url(Chars),{Chars\==[],atom_chars(Item,Chars)}.
key_like_string([H|T])-->[H],{\+member(H,[':',' ','\t','\n'])},key_like_string(T).
key_like_string([])-->[].
id_like_string([H|T])-->[H],{\+member(H,['!',' ','\t','\n',',','[',']','{','}','"'])},id_like_string(T).
id_like_string([])-->[].
symbol_or_url([H|T])-->[H],{\+member(H,[',','[',']','"',' '])},symbol_or_url(T).
symbol_or_url([])-->[].
string_until_end_quote([])-->['"'],!.
string_until_end_quote([H|T])-->(['\\',H];[H]),!,string_until_end_quote(T).
named_like_string([H|T])-->[H],{\+member(H,['\n'])},named_like_string(T).
named_like_string([])-->[].
whs-->[''],!,whs. whs-->[].

:- discontiguous column_names_ext/2.
:- discontiguous primary_column/2.

must_det_ll_r((G1,G2)):- !, once(G1),must_det_ll_r(G2).
must_det_ll_r(G):- call(G).

% Safely executes the given Goal and prints any exception raised.
% Usage: safe(+Goal, +Info).
safe(Goal, Info) :-
    % Try to call Goal. If an exception is raised, unify Exception with the exception.
    catch(Goal, Exception,
        % If an exception is raised, portray the clause (Info :- Goal)
        % along with the exception, then rethrow the exception.
        (catch_ignore(portray_clause(exception:Exception:(Info:- Goal))), throw(Exception))
    ).
% Safely executes the given Goal and prints any exception raised.
% Usage: safe(+Goal).
safe(Goal) :- safe(Goal,safe/1).


skipped_anotations(fbgn_exons2affy1_overlaps).
skipped_anotations(gene_rpkm_matrix).
skipped_anotations(dmel_gene_sequence_ontology_annotations).
skipped_anotations(fbgn_annotation_ID).


gc_now:- set_option_value(gc,true), garbage_collect,garbage_collect_atoms,garbage_collect_clauses.

extreme_debug(_).

numbervars_w_singles(P):- term_singletons(P, Vars),
  numbervars(Vars,260,_,[attvar(bind),singletons(false)]),
  numbervars(P,14,_,[attvar(bind),singletons(true)]).


pp_fb(P):- format("~N "),  \+ \+ (numbervars_w_singles(P), pp_fb1(P)),flush_output.
pp_fb1(P):- write_src(P),!,nl.
:- if(current_predicate(pp_ilp/1)).
pp_fb1(P):- pp_as(P),!,format("~N"),pp_ilp(P),!.
:- endif.
pp_fb1(P):- pp_as(P),!.
pp_fb1(P):- print(P),!,nl.
pp_fb1(P):- fbdebug1(P),!,nl.

for_all(G1,G2):-
  forall(G1,G2).

allow_concepts:- option_else(concepts,TF,true), \+ TF == false.
with_concepts(TF,Goal):- with_option(concepts,TF,Goal).

direct_mapping(NC,NC):- var(NC),!.
direct_mapping(NC,OO):- is_list(NC),!,maplist(direct_mapping,NC,OO).
direct_mapping(!,no_more).
direct_mapping(fail,'False').
direct_mapping(true,'True').
direct_mapping(prolog,meTTa).
direct_mapping('[|]','Cons').
direct_mapping(( ';' ),or).
direct_mapping(( ',' ),and).
direct_mapping(( '\\+' ),if_not).
direct_mapping(( ':-' ),entailed_by).
direct_mapping('=..','term_2_list').
direct_mapping(NC,NC):- \+ compound(NC),!.
direct_mapping((G,E),O):- conjuncts_to_list((G,E),List), into_sequential(List,O),!.
direct_mapping((A->B;C),O):- !, direct_mapping(if_then_else(A,B,C),O).
direct_mapping((A->B),O):- !, direct_mapping(if_then(A,B),O).
direct_mapping(I,O):- I=..[F|II],maplist(direct_mapping,[F|II],OO),O=..OO.

print_metta_src:- mmake,
  for_all((source_file(Pred,File),
          atom_contains(File,flybase)),
         print_metta_src(Pred)).

print_metta_src(F/A):- !, print_metta_src(F,A).
print_metta_src(Pred):- functor(Pred,F,A), print_metta_src(F,A).

print_metta_src(F,A):- functor(Head,F,A),
  nl,nl,nl,
  for_all(clause(Head,Body), pp_metta(Head,Body)).
pp_metta(Head,Body):- Body == true,!, pp_metta(=(Head,'True')).
pp_metta(Head,Body):- Body == false,!, pp_metta(=(Head,'False')).
pp_metta(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP),!,
  pp_metta(=(Head,SP)).


pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

into_sequential(Body,SP):- \+ is_list(Body), conjuncts_to_list(Body,List),into_sequential(List,SP).
into_sequential(List,SP):- length(List,L),L>1, SP =.. [sequential|List],!.
into_sequential([SP],SP):-!.
into_sequential([],'True').

write_src(V):- allow_concepts,!,with_concepts(false,write_src1(V)),flush_output.
write_src(V):- write_src1(V),!.

write_src1(V):- var(V),!, ignore(pp_sex(V)).
write_src1(''):- !, writeq('').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).
write_src1(V):- atom(V),needs_quoted_in_metta(V,_),!, atom_string(V,S),writeq(S).
write_src1(V):- atom(V),!,write(V).
write_src1(V):- pp_sex(V),!.

needs_quoted_in_metta('','"').
needs_quoted_in_metta(V,'"'):- atom_contains(V," ").
needs_quoted_in_metta(V,'"'):- atom_contains(V,"/").
needs_quoted_in_metta(V,'"'):- atom_contains(V,'"').
needs_quoted_in_metta(V,'"'):- atom_contains(V,'"').
needs_quoted_in_metta(V,'"'):- atom_contains(V,',').
%needs_quoted_in_metta(V,"'"):- atom_length(V,L),L==1.
%needs_quoted_in_metta(V,"'"):- atom_contains(V,")").
needs_quoted_in_metta(V,'"'):- atom_contains(V,"|").
needs_quoted_in_metta(V,'"'):- atom_contains(V,"'").

pp_sax(S) :-  \+ allow_concepts,!, write_src(S).
pp_sax(S) :- is_englishy(S),!,print_concept("StringValue",S).
pp_sax(S) :- atom_length(S,1),atom_string(S,SS),!,print_concept("StringValue",SS).
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
pp_sex(V) :- var(V), !, format('$~p',[V]).
pp_sex(V) :- direct_mapping(V,D),V\==D,!,pp_sex(D).
%pp_sex('') :- format('(EmptyNode null)',[]).
pp_sex('') :- format('()',[]).
pp_sex([]):-  !, write('()').
pp_sex('='(N,V)):- allow_concepts, !, format("~N;; ~w == ~n",[N]),!,pp_sex(V).
pp_sex(V) :- (number(V) ; is_dict(V)), !, print_concept('ValueAtom',V).
pp_sex(V) :- (atom(V),atom_number(V,N)), !, print_concept('ValueAtom',N).
pp_sex(S) :- atom(S), always_dash_functor(S,D), pp_sax(D),!.
pp_sex(S) :- string(S),!, print_concept('StringValue',S).
% Lists are printed with parentheses.
pp_sex(V) :- \+ compound(V), !, format('~p',[V]).
pp_sex(V) :- V = '$VAR'(_), !, format('$~p',[V]).
pp_sex(listOf(S,_)) :- !,pp_sex(listOf(S)).
pp_sex(listOf(S)) :- !,format('(ListValue ~@)',[pp_sex(S)]).
pp_sex('!'(S)) :- write('!'),pp_sex(S).
pp_sex([H|T]) :- is_list(T),!, write('('), pp_sex(H), print_list_as_sexpression(T), write(')').
% Compound terms.
%pp_sex(Term) :- compound(Term), Term =.. [Functor|Args], write('('),format('(~w ',[Functor]), write_args_as_sexpression(Args), write(')').

%pp_sex(Term) :- Term =.. ['=',H|Args], length(Args,L),L>2, write('(= '),  pp_sex(H), write('\n\t\t'), maplist(pp_sex(2),Args).
pp_sex(Term) :- Term =.. [Functor|Args], always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.
pp_sex(Term) :- allow_concepts, Term =.. [Functor|Args], format('(EvaluationLink (PredicateNode "~w") (ListLink ',[Functor]), write_args_as_sexpression(Args), write('))'),!.
pp_sex(Term) :- Term =.. [Functor|Args],
   always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.

pp_sex(2,Arg):- write('\t\t'),pp_sex(Arg).

always_dash_functor(A,B):- once(dash_functor(A,B)),A\=@=B,!.
always_dash_functor(A,A).

dash_functor(A,C):- \+ atom(A),!,C=A.
dash_functor(A,C):- direct_mapping(A,B),A\==B,!,always_dash_functor(B,C).
dash_functor(Functor,DFunctor):-
   atom(Functor), atomic_list_concat(L,'-',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'-',DFunctor).
dash_functor(Functor,DFunctor):- fail,
   atom(Functor), atomic_list_concat(L,'_',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'-',DFunctor).
dash_functor(Functor,DFunctor):-
   atom(Functor), atomic_list_concat(L,'_',Functor), L\=[_],maplist(always_dash_functor,L,LL),
   atomic_list_concat(LL,'_',DFunctor).


is_an_arg_type(S,T):- flybase_identifier(S,T),!.
has_type(S,Type):- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,Aft>0.

% Print arguments of a compound term.
write_args_as_sexpression([]).
write_args_as_sexpression([H|T]) :- write(' '), pp_sex(H), write_args_as_sexpression(T).

% Print the rest of the list.
print_list_as_sexpression([]).
print_list_as_sexpression([H|T]) :- write(' '), pp_sex(H), print_list_as_sexpression(T).

call_sexpr(S):- writeln(call=S).


fbug(P) :- format("~N"), with_output_to(user_error,pp_fb(P)),!.
fbug(N=V) :- nonvar(N), !, fbdebug1(N:-V).
fbug(V) :- compound(V),functor(V,F,_A),!,fbdebug1(F:-V).
fbug(V) :- fbdebug1(debug:-V).
fbdebug1(Message) :-
  % ISO Standard: flush_output/1
  flush_output(user_output),
  flush_output(user_error),
  catch(portray_clause(user_error,Message,[]),_,catch_ignore(format(user_error, "~n/* ~q. */~n", [Message]))),
  %format(user_error, "~n/* ~p. */~n", [Message]),
  flush_output(user_error).


swi_only(_):- is_scryer,!,fail.
swi_only(G):- call(G).
is_scryer:- \+  current_prolog_flag(libswipl,_).
:- use_module(library(csv)).

%:- current_prolog_flag(libswipl,_)->use_module(library(logicmoo_utils)); true.





/* mined
; Total         Atoms (Atomspace size): .................................................. 56,354,849
;               ConceptNodes: ............................................................. 9,472,616
;               Bytes Per Atom (Average): ....................................................... 140
;               Bytes Per ConceptNode (Average): ................................................ 120
;               Relational Memory: ............................................................ 7.39G
;               ConceptNode Memory: ........................................................... 1.07G
;               Atoms per minute: ......................................................... 3,491,880
;               Total Physical Memory Used: ................................................... 9.08G
;               Runtime (days:hh:mm:ss): ................................................. 0:00:16:08


; Total         Atoms (Atomspace size): .................................................. 38,812,356
;               ConceptNodes: ............................................................. 9,380,821
;               Total Memory Used: ............................................................ 8.26G
;               Runtime (days:hh:mm:ss): ................................................. 0:00:19:15


; Total         Atoms (Atomspace size): .................................................. 38,822,366
;               ConceptNodes: ............................................................. 9,824,355
;               Random samples: ................................................................. 805
;               Total Memory Used: ............................................................ 8.18G
;               Runtime (days:hh:mm:ss): ................................................. 0:00:08:28

*/

recount_total_loaded_atoms:- flag(total_loaded_atoms,_,0),full_atom_count(Was),flag(total_loaded_atoms,_,Was).

% Convert flybase data from CSV to Prolog format.
load_flybase:- is_scryer,!,load_flybase_files.
load_flybase:- make,recount_total_loaded_atoms,!,load_flybase_files,!,cleanup_arities,!,fb_stats.
load_flybase_dirs:-
  load_flybase('./data/ftp.flybase.net/releases/current/das_precomputed'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*'),
  load_flybase('./data/ftp.flybase.net/releases/current/./*sv'),!.



/*
declare -a StringArray=(\
"fbgn_fbtr_fbpp_expanded_*.tsv.gz" \
"physical_interactions_mitab_fb_*.tsv.gz" \
"dmel_gene_sequence_ontology_annotations_fb_*.tsv.gz" \
"gene_map_table_*.tsv.gz" \
"ncRNA_genes_fb_*.json.gz" \
"gene_association.fb.gz" \
"gene_genetic_interactions_*.tsv.gz" \
"allele_genetic_interactions_*.tsv.gz" \
"allele_phenotypic_data_*.tsv.gz" \
"disease_model_annotations_fb_*.tsv.gz" \
"dmel_human_orthologs_disease_fb_*.tsv.gz" \
"fbrf_pmid_pmcid_doi_fb_*.tsv.gz")
*/

load_flybase_files:-
   ftp_data(Dir),
    with_cwd(Dir,load_flybase_files_ftp).



load_flybase_das_11:-
  % DAS's 11 tsv and 1 json file
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/fbgn_fbtr_fbpp_expanded_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/physical_interactions_mitab_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/dmel_gene_sequence_ontology_annotations_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/gene_map_table_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/ncRNA_genes_fb_*.json'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/gene_association_*.fb',tsv),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/gene_genetic_interactions_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/allele_genetic_interactions_fb_*.tsv'),
  % Note: this file replaces 'allele_phenotypic_data_*.tsv' from FB2023_01 onward.
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/alleles/genotype_phenotype_data_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/allele_phenotypic_data_fb_*.tsv'),


  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/disease_model_annotations_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/dmel_human_orthologs_disease_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/fbrf_pmid_pmcid_doi_fb_*.tsv'),
  format("~n================================================================================================="),
  format("~n=====================================Das Checkpoint=============================================="),
  format("~n================================================================================================="),
              fb_stats,
  format("~n================================================================================================="),
  format("~n================================================================================================="),
  format("~n=================================================================================================~n"),
  !.

load_flybase_files_ftp:-

 %% load_flybase_obo_files,
  load_flybase_das_11,
  % 36 more that DAS doesnt load
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/alleles/fbal_to_fbgn_fb_*.tsv'),

  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/clones/cDNA_clone_data_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/clones/genomic_clone_data_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/collaborators/fbgn_uniprot_fb_*.tsv'),
  %load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/collaborators/gp_information.fb'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/collaborators/pmid_fbgn_uniprot_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/automated_gene_summaries.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/automated_gene_summaries_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/best_gene_summary_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/Dmel_enzyme_data_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/dmel_unique_protein_isoforms_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/fbgn_annotation_ID_fb_*.tsv'),
  with_option([pred_va=true],load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/fbgn_exons2affy1_overlaps.tsv')),
  with_option([pred_va=true],load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/fbgn_exons2affy2_overlaps.tsv')),
  with_option([pred_va=false],load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/fbgn_fbtr_fbpp_fb_*.tsv')),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/fbgn_gleanr_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/fbgn_NAseq_Uniprot_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/gene_functional_complementation_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/gene_group_data_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/gene_groups_HGNC_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/gene_rpkm_matrix_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/gene_rpkm_report_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/gene_snapshots_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/pathway_group_data_fb_*.tsv'),
  %load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/insertions/construct_maps.zip'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/insertions/fu_gal4_table_fb_*.json'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/insertions/insertion_mapping_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/map_conversion/cyto-genetic-seq.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/metadata/dataset_metadata_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/orthologs/dmel_paralogs_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/references/entity_publication_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/species/organism_list_fb_*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/stocks/stocks_FB*.tsv'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/synonyms/fb_synonym_fb_*.tsv'),
  format("~n================================================================================================="),
  format("~n==========================Should be 18 minute Checkpoint========================================="),
  format("~n================================================================================================="),
  fb_stats,
  format("~n================================================================================================="),
  format("~n================================================================================================="),
  format("~n=================================================================================================~n"),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/map_conversion/cytotable.txt',tsv),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/map_conversion/genome-cyto-seq.txt',tsv),
  load_fbase_after_17,
  !.

load_fbase_after_17:-
  %load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/genes/scRNA-Seq_gene_expression_fb_*.tsv'),
  must_det_ll(load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/transposons/transposon_sequence_set.gff',tsv)),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/transposons/transposon_sequence_set.fa'),
  load_obo_files,
 %% load_flybase_chado,
  !.

load_flybase_obo_files:-
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/*/ncRNA_genes_fb_*.json'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/chebi_fb_*.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/doid.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/fly_anatomy.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/fly_development.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/flybase_controlled_vocabulary.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/flybase_stock_vocabulary.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/gene_group_FB*.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/go-basic.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/image.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/psi-mi.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/slice.chebi.obo'),
  load_flybase('./data/ftp.flybase.net/releases/current/precomputed_files/ontologies/so-simple.obo'),
  !.

load_flybase_chado:-  % 359 tables with 937,381,148 rows

  with_option([row_1_is_header=true,max_per_file=100_000],load_flybase('./data/tsv_exports/public/*.tsv')).


est_size( 248_392_754, feature_relationship).
est_size( 141_933_327, dbxrefprop).
est_size(  98_464_502, featureloc).
est_size(  92_616_770, feature).
est_size(  78_909_675, analysisfeature).
est_size(  61_025_742, feature_dbxref).
est_size(  53_031_863, library_featureprop).
est_size(  39_950_320, dbxref).
est_size(  27_923_222, library_feature).
est_size(  23_805_222, feature_relationshipprop).
est_size(  21_280_000, featureprop).
est_size(   7_474_186, feature_synonym).
est_size(   6_554_428, synonym).
est_size(   5_578_281, feature_pub).
est_size(   5_341_101, featureprop_pub).
est_size(   4_865_119, feature_relationship_pub).
est_size(   2_813_406, feature_interactionprop).
est_size(   2_464_356, feature_cvterm).
est_size(   1_950_808, feature_cvtermprop).
est_size(   1_377_259, feature_interaction).
est_size(   1_116_491, feature_genotype).
est_size(     888_211, pubprop).
est_size(     734_871, featureloc_pub).
est_size(     688_735, pubauthor).
est_size(     518_570, genotype_synonym).
est_size(     495_849, genotype).
est_size(     491_539, feature_pubprop).
est_size(     466_210, phenstatement).
est_size(     413_339, pub_dbxref).
est_size(     382_055, genotype_dbxref).
est_size(     351_943, phendesc).
est_size(     277_993, phenotype_comparison_cvterm).
est_size(     254_299, feature_expressionprop).
est_size(     252_545, phenotype_comparison).
est_size(     251_929, pub).
est_size(     242_345, pub_relationship).
est_size(     227_407, feature_expression).
est_size(     213_361, cvterm_relationship).
est_size(     212_143, cvterm_dbxref).
est_size(     209_165, interaction_cvterm).
est_size(     195_001, cvtermsynonym).
est_size(     180_312, expression_cvterm).
est_size(     167_583, update_track).
est_size(     150_402, feature_relationshipprop_pub).
est_size(     149_856, stockcollection_stock).
est_size(     149_856, stock).
est_size(     149_836, stock_genotype).
est_size(     146_847, interactionprop).
est_size(     122_005, interaction_group).
est_size(     119_612, feature_interaction_pub).
est_size(     112_785, interaction_pub).
est_size(     112_782, interaction).
est_size(     101_688, interaction_group_feature_interaction).
est_size(      96_406, feature_grpmember_pub).
est_size(      94_766, cvterm).
est_size(      79_467, expression_cvtermprop).
est_size(      74_874, interactionprop_pub).
est_size(      73_829, library_interaction).
est_size(      57_145, organism).
est_size(      48_731, humanhealthprop).
est_size(      41_076, feature_grpmember).
est_size(      36_961, expression).
est_size(      23_566, library_cvterm).
est_size(      23_484, library_cvtermprop).
est_size(      21_252, cvtermprop).
est_size(      19_798, libraryprop).
est_size(      18_397, phenotype).
est_size(      17_872, phenotype_cvterm).
est_size(      16_618, humanhealth_dbxrefprop).
est_size(      16_530, interaction_expressionprop).
est_size(      16_319, humanhealth_pub).
est_size(      15_401, library_synonym).
est_size(      15_356, humanhealth_dbxref).
est_size(      15_143, cell_line_feature).
est_size(      14_973, libraryprop_pub).
est_size(      13_695, interaction_expression).
est_size(      13_219, interaction_cell_line).
est_size(      10_721, library_pub).
est_size(       9_871, library_relationship).
est_size(       9_852, humanhealthprop_pub).
est_size(       9_559, library_dbxref).
est_size(       8_340, library_relationship_pub).
est_size(       7_096, grp_pub).
est_size(       6_720, cell_line_pub).
est_size(       6_658, grp_relationship).
est_size(       6_606, strain_synonym).
est_size(       5_991, grp_synonym).
est_size(       5_948, humanhealth_synonym).
est_size(       5_786, strainprop).
est_size(       5_784, strainprop_pub).
est_size(       5_770, library).
est_size(       5_544, grp_cvterm).
est_size(       5_445, cell_line_synonym).
est_size(       5_278, library_expression).
est_size(       5_188, grpprop).
est_size(       5_160, grpmember).
est_size(       4_470, humanhealth_dbxrefprop_pub).
est_size(       4_451, library_expressionprop).
est_size(       4_416, grpprop_pub).
est_size(       4_320, stock_cvterm).
est_size(       3_833, library_dbxrefprop).
est_size(       3_830, grpmemberprop).
est_size(       3_778, genotype_cvterm).
est_size(       3_745, humanhealth_featureprop).
est_size(       3_722, library_strainprop).
est_size(       3_722, library_strain).
est_size(       3_626, humanhealth_feature).
est_size(       2_642, grp_dbxref).
est_size(       2_264, humanhealth_relationship).
est_size(       2_221, humanhealth_relationship_pub).
est_size(       2_094, strain_pub).
est_size(       2_011, grp_relationship_pub).
est_size(       1_940, strain_cvtermprop).
est_size(       1_940, strain_cvterm).
est_size(       1_815, grp).
est_size(       1_778, strain_dbxref).
est_size(       1_777, strain).
est_size(       1_740, organism_dbxref).
est_size(       1_644, feature_humanhealth_dbxref).
est_size(       1_541, humanhealth_cvtermprop).
est_size(       1_541, humanhealth_cvterm).
est_size(       1_516, humanhealth).
est_size(       1_301, cell_lineprop_pub).
est_size(       1_292, cell_lineprop).
est_size(       1_216, cell_line_dbxref).
est_size(       1_199, cell_line_libraryprop).
est_size(       1_082, cell_line_library).
est_size(       1_014, organism_pub).
est_size(         822, organismprop).
est_size(         732, organismprop_pub).
est_size(         715, cell_line_cvterm).
est_size(         519, db).
est_size(         436, strain_relationship_pub).
est_size(         436, strain_relationship).
est_size(         321, cell_line).
est_size(         309, analysis).
est_size(         239, stockprop).
est_size(         172, cell_line_relationship).
est_size(         140, strain_featureprop).
est_size(         140, strain_feature).
est_size(         108, strain_phenotypeprop).
est_size(          97, humanhealth_pubprop).
est_size(          74, cell_line_cvtermprop).
est_size(          72, cv).
est_size(          55, strain_phenotype).
est_size(          41, environment).
est_size(          28, stockcollectionprop).
est_size(          27, contact).
est_size(          19, environment_cvterm).
est_size(          12, organism_library).
est_size(           8, stockcollection).
est_size(           2, lock).
est_size(           1, analysisgrp).
est_size(           1, analysisgrpmember).
est_size(           1, analysisprop).
est_size(           1, audit_chado).
est_size(           1, cell_line_strain).
est_size(           1, cell_line_strainprop).
est_size(           1, cvtermpath).
est_size(           1, eimage).
est_size(           1, expression_image).
est_size(           1, expression_pub).
est_size(           1, expressionprop).
est_size(           1, feature_cvterm_dbxref).
est_size(           1, feature_phenotype).
est_size(           1, featuremap).
est_size(           1, featuremap_pub).
est_size(           1, featurepos).
est_size(           1, featurerange).
est_size(           1, genotype_cvtermprop).
est_size(           1, genotype_pub).
est_size(           1, genotypeprop).
est_size(           1, genotypeprop_pub).
est_size(           1, grp_pubprop).
est_size(           1, grp_relationshipprop).
est_size(           1, grpmember_cvterm).
est_size(           1, grpmember_pub).
est_size(           1, grpmemberprop_pub).
est_size(           1, humanhealth_phenotype).
est_size(           1, humanhealth_phenotypeprop).
est_size(           1, interaction_cvtermprop).
est_size(           1, library_grpmember).
est_size(           1, library_humanhealth).
est_size(           1, library_humanhealthprop).
est_size(           1, organism_cvterm).
est_size(           1, organism_cvtermprop).
est_size(           1, organism_grpmember).
est_size(           1, project).
est_size(           1, stock_dbxref).
est_size(           1, stock_pub).
est_size(           1, stock_relationship).
est_size(           1, stock_relationship_pub).
est_size(           1, stockprop_pub).
est_size(           1, tableinfo).

% Load flybase data in Prolog format.
load_fb_cache:-
  load_fb_mask('./data/ftp.flybase.net/releases/current/precomputed_files/*/*pl'),
  load_fb_mask('flybase_data/public.*.pl'),
  load_fb_mask('flybase_data/*fb_2023_01.pl').

% Process a file or directory path with a given predicate.
with_wild_path(Fnicate, Dir) :- extreme_debug(fbug(with_wild_path(Fnicate, Dir))),fail.
with_wild_path(_Fnicate, []) :- !.
with_wild_path(Fnicate, Dir) :-  is_scryer, atom(Dir), !, must_det_ll_r((atom_chars(Dir,Chars), with_wild_path(Fnicate, Chars))).
with_wild_path(Fnicate, Chars) :-  \+ is_scryer, \+ atom(Chars), !, must_det_ll_r((name(Atom,Chars), with_wild_path(Fnicate, Atom))).
with_wild_path(Fnicate, File) :- exists_file(File), !, must_det_ll_r(( call(Fnicate, File))).
with_wild_path(Fnicate, File) :- with_wild_path_swi(Fnicate, File).
with_wild_path(Fnicate, Dir) :-  exists_directory(Dir), !,
  must_det_ll_r((directory_files(Dir, Files),
  maplist(directory_file_path(Dir,Files),Paths),
  maplist(atom_chars,Paths,CharPaths),
  maplist(with_wild_path(Fnicate), CharPaths))), !.
with_wild_path(Fnicate, File) :- is_list(File), !,  must_det_ll_r((maplist(with_wild_path(Fnicate), File))).
with_wild_path(Fnicate, File) :- must_det_ll_r((call(Fnicate, File))).


with_wild_path_swi(Fnicate, File) :-
  compound(File),
  absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(directory)]),
  '\\=@='(Dir, File), !,
  with_wild_path(Fnicate, Dir).
with_wild_path_swi(Fnicate, File) :-
  compound(File), !,
  absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(['csv', 'tsv', ''])]),
  '\\=@='(Dir, File), !,
  with_wild_path(Fnicate, Dir).
with_wild_path_swi(Fnicate, File) :-
  atom_contains(File, '*'),
  expand_file_name(File, List), !,
  maplist(with_wild_path(Fnicate), List).
with_wild_path_swi(Fnicate, File) :-
  exists_directory(File),
  directory_file_path(File, '*.*sv', Wildcard),
  expand_file_name(Wildcard, List), !,
  maplist(Fnicate, List).

% ===============================
% MeTTa Python incoming interface
% ===============================
%debug_metta(Call):- skip(Call).
if_metta_debug(Goal):- getenv('VSPACE_VERBOSE','2'),!,ignore(call(Goal)).
if_metta_debug(_).
debug_metta(Term):- if_metta_debug((format('~N; ~@~n',[write_src(Term)]))).
debug_metta(Msg,Term):- if_metta_debug((format('~N; ~w: ~@~n',[Msg,write_src(Term)]))),!.

%:- dynamic(for_metta/2).
%for_metta(_,T):- fb_pred(F,A),functor(T,F,A),call(T).
metta_ls(KB):-
  listing(KB:for_metta/2).
metta_add(KB,New):- decl_m_fb_pred(KB,for_metta,2), MP = KB:for_metta(KB,New), assert_new(MP), debug_metta(['add-atom',KB,New]).
metta_rem(KB,Old):- debug_metta(['remove-atom',KB,Old]),metta_del(KB,Old).
metta_del(KB,Old):- decl_m_fb_pred(KB,for_metta,2), MP = KB:for_metta(KB,Old),
  copy_term(MP,Copy), clause(MP,true,Ref), MP =@= Copy, !, erase(Ref). % ,debug_metta('DEL',Old).
metta_replace(KB,Old,New):- debug_metta(['atom-replace',KB,Old,New]),!, metta_del(KB,Old), metta_add(KB,New).
metta_count(KB,Count):-
 must_det_ll((
  debug_metta(['atom-count',KB]),
  decl_m_fb_pred(KB,for_metta,2), full_atom_count(SL1),
  MP = KB:for_metta(_,_),
  predicate_property(MP,number_of_clauses(SL2)),
  predicate_property(MP,number_of_rules(SL3)),
  %metta_ls(KB),
  Count is SL1 + SL2 - SL3)),!.
metta_count(_KB,0):-!.
%metta_count(KB,Count):- writeln(metta_count_in(KB,Count)), findall(Atom,for_metta(KB,Atom),AtomsL),length(AtomsL,Count),writeln(metta_count_out(KB,Count)).
metta_iter(KB,Atoms):- decl_m_fb_pred(KB,for_metta,2), KB:for_metta(KB,Atoms).
metta_atoms(KB,AtomsL):- debug_metta(['get-atoms',KB]), decl_m_fb_pred(KB,for_metta,2), findall(Atom,KB:for_metta(KB,Atom),AtomsL).
%metta_iter_bind(KB,Query,Template,AtomsL):- decl_m_fb_pred(KB,for_metta,2), findall(Template,KB:for_metta(KB,Query),AtomsL).
metta_iter_bind(KB,Query,Vars):-
  term_variables(Query,Vars),
  debug_metta(['match',KB,Query,Vars]),
  decl_m_fb_pred(KB,for_metta,2), KB:for_metta(KB,Query),
  debug_metta('RES',metta_iter_bind(KB,Query,Vars)).
%metta_iter_bind(KB,Atom,Template):- fb_stats, findall(Template,metta_iter(KB,Atom),VarList).
/*
metta_iter_bind(KB,Atoms,Vars):-
  fb_stats,
  term_variables(Atoms,AVars),
  metta_iter(KB,Atoms), ignore(AVars = Vars).
*/

'&flybase':for_metta('&flybase',P):- fb_pred(F,A),length(L,A),P=[F|L],apply(F,L).
/*
%encoding_trial('iso-8859-1').
%encoding_trial('us-ascii').
%encoding_trial('utf-8').
encoding_trial(ascii).
encoding_trial(iso_latin_1).
encoding_trial(octet).
encoding_trial(text).
encoding_trial(unicode_be).
encoding_trial(unicode_le).
encoding_trial(utf8).
encoding_trial(wchar_t).

try_encoding:- 'allele_genetic_interactions'('14-3-3epsilon[18A2]',X,Y,Z),
  encoding_trial(ET),set_stream(current_output,encoding(ET)),catch(write(ET=[X,Y,Z]),_,fail),fail.
*/

load_fb_mask(Filename):- is_scryer,atom(Filename),name(Filename,Chars),!,load_fb_mask(Chars).
load_fb_mask(Filename):- expand_file_name(Filename,Files1),maplist(load_fb_cache,Files1).
load_fb_cache(File):- with_wild_path(load_fb_cache0,File).
load_fb_cache0(File):- file_name_extension(Name,_E,File),
  atomic_list_concat([Pub,Table],'.',Name),
  atomic_list_concat([Pub,Table,qlf],'.',OutputFile),!,
  load_fb_cache(File,OutputFile,Table).
load_fb_cache0(File):- file_name_extension(Name,_E,File),
  atomic_list_concat([Table],'.',Name),
  atomic_list_concat([Table,qlf],'.',OutputFile),
  load_fb_cache(File,OutputFile,Table).


load_fb_json(Ext,File,OutputFile,Fn):- fbug(load_fb_json(Ext,File,OutputFile,Fn)).
load_fb_obo(Ext,File,OutputFile,Fn):- fbug(load_fb_obo(Ext,File,OutputFile,Fn)),!.
%load_fb_obo(Ext,File,OutputFile,Fn):- fbug(load_fb_obo(Ext,File,OutputFile,Fn)),
%  (current_predicate(load_obo/1)->load_obo(File);true).


data_pred(X,Y):- atomic_list_concat(List,'/',X),List\==[],List\=[_],!,last(List,L),data_pred(L,Y).
data_pred(X,Y):- atomic_list_concat(List,'_',X),once(not_trimmed_path(List,NewList)),
  NewList\==[],NewList\==List,atomic_list_concat(NewList,'_',Y),!.
data_pred(X,Y):- atomic_list_concat([L,_|_],'_fb_',X),!,data_pred(L,Y).
data_pred(X,X).

is_trimmed_path(X):- atom_contains(X,'0'),!.
is_trimmed_path('fb').
is_trimmed_path('data').
%is_trimmed_path(Atom):- atom_chars(Atom,Chars), read_term_from_chars(Chars,Term,[]),number(Term),!.
not_trimmed_path([H|List],NewList):- is_trimmed_path(H),!,not_trimmed_path(List,NewList).
not_trimmed_path([H|List],[H|NewList]):- !, not_trimmed_path(List,NewList).
not_trimmed_path([],[]).


%file_to_sep(_File,9).
file_to_sep(File,','):- file_name_extension(_,csv,File),!.
file_to_sep(File,'\t'):- file_name_extension(_,tsv,File),!.


is_swipl:- \+ is_scryer.

:- if(is_scryer).
read_line_to_chars(S,L):- is_scryer,!,get_line_to_chars(S,L,[]).
:- endif.
read_line_to_chars(S,L):- read_line_to_string(S,Str),string_chars(Str,L).


% Assert a given term if no variant of it already exists in the database.
% Usage: fb_assert(+Term).
fb_assert(Term) :-
    % Check if Term is a rule (Head :- Body) or a fact (just Head).
    ( Term = (Head :- Body)
    -> copy_term(Body, CopiedBody)
    ; (Head = Term, CopiedBody = true)
    ),
    % Copy the Head to generate a new term with fresh variables.
    copy_term(Head, CopiedHead),
    % If no variant of CopiedHead exists in the database with the same body,
    % assert Term; otherwise, succeed without asserting Term.
    ( \+ (clause(CopiedHead, CopiedBody), variant(CopiedHead, Head))
    -> assertz(Term)
    ; true
    ).

:- dynamic(done_reading/1).

load_flybase(Sep,File,Stream,OutputStream,Fn):-
 must_det_ll_r((
  ignore(swi_only(format(OutputStream,":- ~q.\n",[encoding(utf8)]))),
  atomic_list_concat([data,Fn],'_',Fn0),
  data_pred(Fn0,Fn),
  load_flybase_sv(Sep,File,Stream,OutputStream,Fn))).

% Sep,File,Stream,OutputStream,Fn
load_flybase_sv(Sep,File,Stream,OutputStream,Fn):- at_end_of_stream(Stream),!,
  once(load_fb_data(_ArgTypes,File,Stream,Fn,Sep,end_of_file,OutputStream)).

load_flybase_sv(Sep,File,Stream,OutputStream,Fn):-
 must_det_ll_r((
  flag(loaded_from_file_count,_,0),
  ignore(once((table_columns(File,Header);table_columns(Fn,Header)))),
  fix_header_names(Fn,Header,ArgTypes),
  forall((table_columns(File,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(File,ColInfo))),
  forall((table_columns(Fn,ColInfo),ArgTypes\==ColInfo),pp_fb(odd_table_columns(Fn,ColInfo))),
  ((primary_column(Fn,Name),nth1(N,ArgTypes,Name))->NArgTypes=[N|ArgTypes];NArgTypes=[1|ArgTypes]),
  if_t(is_list(ArgTypes),add_table_n_types(Fn,1,ArgTypes)),
  ground(NArgTypes),

  time((repeat,
  read_line_to_chars(Stream, Chars),
  once(load_flybase_chars(NArgTypes,File,Stream,Fn,Sep,Chars,OutputStream)),
  once(done_reading(File);at_end_of_stream(Stream)),!,
  once(load_fb_data(NArgTypes,File,Stream,Fn,Sep,end_of_file,OutputStream)))),
  flag(loaded_from_file_count,X,X),!,
  fb_stats(Fn),
  pl_stats(File,X))).


%save_conversion_data(ArgTypes,Fn,OutputStream,Data):- maplist(write_flybase_data(ArgTypes,ArgTypes,Fn,OutputStream),Data).

is_really_header_row([H|_],_Names):- atom_concat('',_,H),!.

%read_csv_stream(Sep,CharsStream,Header):- read_string(CharsStream, "\n", "\r\true ",_,)
read_csv_stream(Sep,CharsStream,Header):- %  \+ option_value(full_canon,[]),!,
  read_line_to_string(CharsStream,Chars),
  (Chars == end_of_file -> Header= Chars ; atomic_list_concat(Header, Sep, Chars)).
read_csv_stream(Sep,CharsStream,Header):- \+ option_value(full_canon,[]),!, read_line_to_string(CharsStream,Chars),
  (Chars == end_of_file -> Header= Chars ; split_string(Chars, Sep, "\s\true\n", Header)).
read_csv_stream(Sep,CharsStream,Header):-
  name(Sep,[SepCode]),
  csv_options(CompiledHeaderOptions,[separator(SepCode)]),
  csv_read_row(CharsStream, HeaderRow, CompiledHeaderOptions),
  HeaderRow=..[_|Header],!.

read_csv(Sep,Chars,Header):- \+ option_value(full_canon,[]),!, split_string(Chars, Sep, "\s\true\n", Header).
read_csv(Sep,Chars,Header):-
  open_string(Chars,CharsStream),read_csv_stream(Sep,CharsStream,Header).


attempt_header_row(Sep,Chars,Fn,Header,ArgTypes):-
  read_csv(Sep,Chars,Header),
  fix_header_names(Fn,Header,ArgTypes),!.

:- dynamic(t_h_n/3).

load_flybase_chars(ArgTypes,File,_Stream,_Fn,Sep,Chars,_OutputStream):-
  ( \+ member(Sep,Chars); (['#','#',' '|_]=Chars) ;  (ground(ArgTypes),['#'|_]=Chars)),
  %writeln(comment(Sep)=Chars),!,
  (format("~n ; ~s",[Chars])),
  ignore((flag(loaded_from_file_count,X,X),X>100,!,assert(done_reading(File)))).

load_flybase_chars([N|ArgTypes],File,Stream,Fn,Sep,Chars,OutputStream):-
  var(ArgTypes),member(Sep,Chars),['#'|_]=Chars,
  (format("~n ; Maybe Header: ~s",[Chars])),
  attempt_header_row(Sep,Chars,Fn,Header,ArgTypes),
  is_really_header_row(Header,ArgTypes),
  (fbug(t_h_n(Fn,Header,ArgTypes)),fb_assert(t_h_n(Fn,Header,ArgTypes))),!,
  load_fb_data([N|ArgTypes],File,Stream,Fn,Sep,is_swipl,OutputStream).

load_flybase_chars([N|ArgTypes],File,Stream,Fn,Sep,Chars,OutputStream):- is_swipl,
  attempt_header_row(Sep,Chars,Fn,Header,_),
  write_flybase_data([N|ArgTypes],OutputStream,Fn,Header),!,
  load_fb_data([N|ArgTypes],File,Stream,Fn,Sep,is_swipl,OutputStream).
 


load_fb_data(_ArgTypes,File,_Stream,_Fn,_Sep,Data,_OutputStream):-  
  (Data == end_of_file;done_reading(File)),!.

load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl,OutputStream):-  !, % \+ option_value(full_canon,[]), !,
  (option_value(max_per_file,Max)->true;Max=inf),
  fbug(load_fb_data(ArgTypes,File,Max,Fn,Sep)),
  add_table_n_types(Fn,1,ArgTypes),!,
   repeat,
     once(read_csv_stream(Sep,Stream,Data)),
     flag(loaded_from_file_count,X,X),
      (((Data== end_of_file);(X>Max)) -> assert(done_reading(File)) ; 
       (once(write_flybase_data(ArgTypes,OutputStream,Fn,Data)),fail)),!.

load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl,OutputStream):- !,
   name(Sep,[SepCode]),
  csv_options(CompiledOptions,[separator(SepCode)]),
  (option_value(max_per_file,Max)->true;Max=inf),
  fbug(load_fb_data(ArgTypes,File,Max,Fn,Sep)),
  add_table_n_types(Fn,1,ArgTypes),!,
   repeat,
     once((csv_read_row(Stream, RData, CompiledOptions))),
     flag(loaded_from_file_count,X,X),
      (((RData== end_of_file);(X>Max)) -> assert(done_reading(File)) ; 
       (RData =..[_|Data], 
       once(write_flybase_data(ArgTypes,OutputStream,Fn,Data)),fail)),!.

% recursion depth 16 million rows
load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl,OutputStream):- 
  name(Sep,[SepCode]),
  csv_options(CompiledOptions,[strip(true),convert(true),separator(SepCode)]),
   (option_value(max_per_file,Max)->true;Max=inf),
     once((csv_read_row(Stream, RData, CompiledOptions))),
     flag(loaded_from_file_count,X,X), 
      (((RData== end_of_file);(X>Max)) -> assert(done_reading(File)) ; 
       (RData =..[_|Data], once(write_flybase_data(ArgTypes,OutputStream,Fn,Data)),
         load_fb_data(ArgTypes,File,Stream,Fn,Sep, is_swipl,OutputStream))),!.


has_list(Header):- is_list(Header),member(listOf(_,_),Header).


:- dynamic(fb_pred/2).

full_atom_count(SL):- flag(total_loaded_atoms,SL,SL),SL>1,!.
full_atom_count(SL):- findall(NC,(fb_pred(F,A),fb_stats(F,A,NC)),Each), sumlist(Each,SL).

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
        fb_stats
    ;   % Otherwise, do nothing
        true
    ).

fb_stats:- gc_now,
   writeln('\n\n\n\n\n\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
   writeln('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
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

   pl_stats('ConceptNodes',Concepts),
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
fb_stats(F):- for_all(fb_pred(F,A),fb_stats(F,A)).
fb_stats(F,A):- fb_stats(F,A,NC), pl_stats(F/A,NC).
fb_stats(F,A,NC):- functor(P,F,A),predicate_property(P,number_of_clauses(NC)).
pl_stats(Stat):- statistics(Stat,Value),pl_stats(Stat,Value).
pl_stats(Stat,[Value|_]):- nonvar(Value),!, pl_stats(Stat,Value).
pl_stats(Stat,Value):- format("~N;\t\t~@: ~`.t ~@~100|",[format_value(Stat),format_value(Value)]),!.


% Predicate to print the formatted result.
format_value(Value) :- float(Value),!,format("~2f",[Value]),!.
format_value(Bytes) :- integer(Bytes),format_bytes(Bytes, Formatted), write(Formatted).
format_value(Term)  :- format("~w",[Term]).
%  Base case: If the number is 1G or more, show it in gigabytes (G).
format_bytes(Bytes, Formatted) :-  Bytes >= 1073741824, GB is Bytes / 1073741824, format(atom(Formatted), '~2fG', [GB]).
% If the number is less than 1G, show it in megabytes (M).
format_bytes(Bytes, Formatted) :- Bytes >= 104857600, Bytes < 1073741824, !, MB is Bytes / 1048576, D is floor(MB), format(atom(Formatted), '~DM', [D]).
% If the number is less than 1K, show it in bytes (B).
format_bytes(Bytes, Formatted) :- format(atom(Formatted), '~D', [Bytes]).
% % If the number is less than 1M, show it in kilobytes (K).
%format_bytes(Bytes, Formatted) :- Bytes >= 1024, Bytes < 1048576, !, KB is Bytes / 1024, format(atom(Formatted), '~0fK', [KB]).

% Convert total seconds to days, hours, minutes, seconds, and milliseconds.
format_time(TotalSeconds, Formatted) :-
    Seconds is floor(TotalSeconds),
    % Get days, remaining seconds
    Days is div(Seconds, 86400),
    Remain1 is mod(Seconds, 86400)-57600,
    format_time(atom(Out),'%T',Remain1),
    % Format the result
    format(atom(Formatted), '~w:~w', [Days, Out]).

% Predicate to print the formatted time.
print_formatted_time(TotalSeconds) :-
    format_time(TotalSeconds, Formatted),
    writeln(Formatted).
    
load_fb_cache(_File,OutputFile,_Fn):- exists_file(OutputFile),!,ensure_loaded(OutputFile),!.
load_fb_cache(File,_OutputFile,_Fn):- load_files([File],[qcompile(large)]).

load_flybase(File):- file_name_extension(_,Ext,File),!, load_flybase(File,Ext).
load_flybase(File,Ext):-
   with_wild_path(load_flybase0(Ext),File),!.

load_flybase0(Ext,_File):-  Ext=='pl',!.
load_flybase0(Ext,File):-
  file_name_extension(Name,_,File),
  atomic_list_concat([Name,'pl'],'.',OutputFile),
  data_pred(Name,Fn), 
  load_flybase(Ext,File,OutputFile,Fn).

%load_flybase2:- load_flybase('./data/ftp.flybase.net/releases/current/allele_genetic_interactions_fb_2023_01.tsv','allele_genetic_interactions_fb_2023_01.pl',allele_genetic_interactions).
%load_flybase(_Ext,_File,OutputFile,_Fn):- exists_file(OutputFile),size_file(OutputFile,N),N>100,!.

:- dynamic(load_state/2).
load_flybase(_Ext,File,_OutputFile,_Fn):- load_state(File,_),!.
load_flybase(Ext,File,OutputFile,Fn):- file_to_sep(File,Sep),!,
  assert(load_state(File,loading)),
  extreme_debug(fbug(load_flybase(Ext,File,OutputFile,Fn))),
  setup_call_cleanup(open(File,read,Stream),
       setup_call_cleanup(open(OutputFile,write,OutputStream,[encoding(utf8)]),
           %load_flybase_sv(Sep,File,Stream,OutputStream,Fn),
           load_flybase(Sep,File,Stream,OutputStream,Fn),
    close(OutputStream)),
  close(Stream)),!,
  retract(load_state(File,loading)),
  assert(load_state(File,loaded)),fb_stats.


%load_flybase(Ext,File,OutputFile,Fn):-  Ext==obo,!,load_fb_obo(Ext,File,OutputFile,Fn).
%load_flybase(Ext,File,OutputFile,Fn):-  Ext==json,!,load_fb_json(Ext,File,OutputFile,Fn).
load_flybase(Ext,File,OutputFile,Fn):- fbug(load_flybase(Ext,File,OutputFile,Fn)),!.



write_flybase_data(_ArgTypes,_OutputStream,_Fn,[]):-!.
write_flybase_data(_ArgTypes,_OutputStream,_Fn,['']):-!.
write_flybase_data(ArgTypes,OutputStream,Fn,DataL0):-
  make_assertion(ArgTypes,Fn,DataL0,Data,OldData),
  ignore((
    heartbeat,
    functor(Data,F,A), A>=2,
   (fb_pred(F,A)-> true; (dynamic(F/A),assert(fb_pred(F,A)))),
    flag(loaded_from_file_count,X,X+1),    
    flag(total_loaded_atoms,TA,TA+1),
    assert(Data),
    ignore((((has_list(ArgTypes)->(X<23,X>20); (X<13,X>10)); (X>0,(0 is X rem 1_000_000),fb_stats)),nl,nl,fbug(X=Data),ignore((OldData\==DataL0,fbug(oldData=OldData))))),
    catch_ignore(ignore((X<1000,must_det_ll_r((write_canonical(OutputStream,Data),writeln(OutputStream,'.')))))))),!.

make_assertion(ArgTypes,Fn,DataL0,Data,DataL0):-
 must_det_ll_r((
    into_datum(Fn,DataL0,Data0),
    Data0=..[F|Args],
    skip(if_t(var(ArgTypes),must_det_ll_r((once((length(Args,Len),length(ArgTypes,Len),once((table_columns(Fn,ArgTypes);table_columns(F,ArgTypes))))))))),
    fix_list_args(Fn,ArgTypes,Args,DataL), 
    Data=..[F|DataL])).


fix_list_args(_,_,Y,Y):- option_value(early_canon,[]), \+ should_sample,!.
%fix_list_args(_Fn,_ArgTypes,[X],[X]):-!.
fix_list_args(Fn,ArgTypes,Args,NewArgs):-
 must_det_ll_r((
  primary_term(Fn,ArgTypes,Args,Term,NewArgTypes),
  fix_elist_args(Term,Fn,1,NewArgTypes,Args,NewArgs),
  extreme_debug(ignore(((Args \== NewArgs,fbug(NewArgs))))))).
fix_list_args(_Fn,_ArgTypes,Args,Args):-!.

primary_term(_Fn,[N|ArgTypes],_Args,_Term,ArgTypes):- number(N),!.
primary_term(_Fn,[N|ArgTypes],Args,Term,ArgTypes):- number(N),!,nth1(N,Args,Term).
primary_term(_Fn,ArgTypes,_Args,_Term,ArgTypes):-!.
primary_term(_Fn,ArgTypes,Args,Term,NewArgTypes):-
   append(L,[primary(Name)|R],ArgTypes),
   append(L,[Name|R],NewArgTypes),
   length(L,N),nth0(N,Args,Term).
primary_term( Fn,ArgTypes,Args,Term,ArgTypes):-
   primary_column(Fn,Name),
   nth1(N,ArgTypes,Name),!,
   nth1(N,Args,Term),!.
primary_term(_Fn,ArgTypes,[Term|_],Term,ArgTypes):-!.
primary_term(_Fn,ArgTypes,_Args,_Term,ArgTypes).

fix_elist_args(Term,Fn,N,[Nth|ArgTypes],Args,NewArgs):- number(Nth),!,fix_elist_args(Term,Fn,N,ArgTypes,Args,NewArgs).
fix_elist_args(Term,Fn,N,[Type|ArgTypes],[Concept|Args],[Arg|NewArgs]):- !,
   must_det_ll((adjust_type(Term,Fn,N,Type,Concept,Arg), N2 is N +1,  fix_elist_args(Term,Fn,N2,ArgTypes,Args,NewArgs))).
fix_elist_args(_Term,_Fn,_N,_,X,X).

adjust_type(Term,Fn,N,listOf(Type),Arg,NewL):- must_det_ll((nonvar(Type),as_list([],Arg,New),is_list(New),
   maplist(adjust_type(Term,Fn,N,Type),New,NewL))).
adjust_type(Term,Fn,N,listOf(Type,Seps),Arg,NewL):- must_det_ll((nonvar(Type),as_list(Seps,Arg,New),is_list(New),
   maplist(adjust_type(Term,Fn,N,Type),New,NewL))).
adjust_type(Term,Fn,N,Type,Concept,Arg):- numeric_value_p_n(Fn,N,_),!,
   must_det_ll(((atom_number(Concept,Arg)->true;(Concept=Arg)),assert_type_of(Term,Fn,N,Type,Arg))).
adjust_type(Term,Fn,N,Type,Concept,Arg):- must_det_ll((fix_concept(Concept,Arg), assert_type_of(Term,Fn,N,Type,Arg))).
adjust_type(_Term,_Fn,_N,_,X,X).

:- dynamic(fb_arg/1).
:- dynamic(fb_arg_table_n/3).
assert_type_of(_Term,_Fn,_N,_Type,_Arg):- \+ should_sample,!.
assert_type_of(Term,Fn,N,Type,Arg):- is_list(Arg),!,maplist(assert_type_of(Term,Fn,N,Type),Arg).
assert_type_of(_Term,Fn,N,_Type,Arg):- 
 must_det_ll_r((
   assert_new(fb_arg(Arg)),
   assert_new(fb_arg_table_n(Arg,Fn,N)))).

:- dynamic(fb_arg_type/1).
:- dynamic(table_n_type/3).
add_table_n_types(_Fn,_,ArgTypes):- \+ is_list(ArgTypes),!.
add_table_n_types(Fn,1,[N|ArgTypes]):- number(N),!,
   add_table_n_types(Fn,1,ArgTypes).
add_table_n_types(Fn,N,[Type|ArgTypes]):-!,
  sub_term(Sub,Type),atom(Sub),!,
  assert_new(fb_arg_type(Sub)),
  assert_new(table_n_type(Fn,N,Sub)),
  N2 is N+1, add_table_n_types(Fn,N2,ArgTypes),!.
add_table_n_types(_Fn,_,[]).

is_concept(Arg):- fb_arg(Arg).
is_concept_type(Type):- fb_arg_type(Type).

arg_table_n_type(Arg,Fn,N,Type):- table_n_type(Fn,N,Type),once((fb_pred(Fn,A),functor(G,Fn,A), arg(N,G,Arg),call(G),
  \+ is_list(Arg), \+ as_list(Arg,[]))).

is_valueatom(Fn,N,Type):- arg_table_n_type(Arg,Fn,N,Type),atom_number(Arg,_).

:- dynamic(numeric_value_p_n/3).
fis_valueatom(PNList,Len):- findall(P-N,is_valueatom(P,N,_Type),PNList),length(PNList,Len).

save_value_atom_cols:- forall(is_valueatom(Fn,N,Type),assert_new(numeric_value_p_n(Fn,N,Type))),
  listing(numeric_value_p_n/3).

as_list(A,New):- is_list(A),!,New = A.
as_list(A,New):- var(A),!,New = [].
as_list('-',[]). as_list("-",[]). as_list('',[]).
as_list(N,[N]):- number(N),!.
as_list("",[]). as_list(' ',[]). as_list(" ",[]).

as_list(_,S,O):- as_list(S,O),!.
as_list(SepL,A,ListO):-  member(Sep,SepL),catch_ignore(atomic_list_concat(List,Sep,A)),List\=[_],!,maplist(fix_concept,List,ListO).
as_list(_,A,ListO):-  member(Sep,['|',',',';']),catch_ignore(atomic_list_concat(List,Sep,A)),List\=[_],!,maplist(fix_concept,List,ListO).
as_list(_Sep,A,[AO]):- fix_concept(A,AO).

fix_concept(A,A):- \+ atom(A), \+ string(A),!.
fix_concept(A,AO):- reprefix(List,To),member(E,List),atom_concat(E,AM,A),atom_concat(To,AM,AO).
fix_concept(A,N):- atom(A),!,N=A.
%fix_concept(A,N):- atom(A),!,(atom_number(A,N)->true;N=A).
fix_concept(S,A):- number_string(A,S),!.
fix_concept(S,A):- atom_string(A,S),!.

% FBcv_0000743 % "FBtp0000743 %CL:0000743 % WBPhenotype_0000743 
reprefix(['GO_','GO--','BiologicalProcess:GO:'],'GO:').
reprefix(['flybase:','FLYBASE:','comment:'],'').



:- discontiguous column_description/4. 
:- discontiguous primary_column/2. 
:- discontiguous column_names/2. 
:- discontiguous file_location/2.



% 466_896_429
% Descriptions for allele_genetic_interactions columns
% Descriptions for genotype_phenotype_data columns
% For the file allele_genetic_interactions_*.tsv
% For the file genotype_phenotype_data_*.tsv



column_description(allele_FBal, "Current FlyBase identifier (FBal) of allele.", identifier, 'Allele Identifier').
column_description(allele_symbol, "Current FlyBase allele symbol.", symbol, 'Allele Symbol').
column_description('Bin_value', "The expression bin classification of this gene in this RNA-Seq experiment, based on RPKM value.", numeric, 'Expression Bin').
column_description('Cluster_Cell_Type_ID', "The FlyBase FBbt ID for the cell type represented by the cell cluster.", identifier, 'Cell Type').
column_description('Cluster_Cell_Type_Name', "The FlyBase name for the cell type represented by the cell cluster.", name, 'Cell Type Name').
column_description('Cluster_ID', "The FlyBase FBlc ID for the dataset representing the cell cluster.", identifier, 'Cell Cluster').
column_description('Cluster_Name', "The FlyBase name for the dataset representing the cell cluster.", name, 'Cell Cluster Name').
column_description('Clustering_Analysis_ID', "The FlyBase FBlc ID for the dataset representing the clustering analysis.", identifier, 'Dataset').
column_description('Clustering_Analysis_Name', "The FlyBase name for the dataset representing the clustering analysis.", name, 'Dataset Name').
column_description('Count_used', "Indicates if the RPKM expression value was calculated using only the exonic regions unique to the gene and not overlapping exons of other genes (Unique), or, if the RPKM expression value was calculated based on all exons of the gene regardless of overlap with other genes (Total).", category, 'Count Type').
column_description(current_fullname, "Current full name used in FlyBase for the object.", name, 'Name').
column_description(current_symbol, "Current symbol used in FlyBase for the object.", symbol, 'Symbol').
column_description('DATASAMPLE_NAME_(DATASET_ID)', "Each subsequent column reports the gene RPKM values for the sample listed in the header.", matrix, 'Expression Matrix').
column_description('FBgn', "The unique FlyBase gene ID for this gene.", identifier, 'Gene').
column_description('FBgn_id', "Unique FlyBase gene ID.", identifier, 'Gene').
column_description('FBrf', "Current FlyBase identifer (FBrf) of publication from which data came.", identifier, 'Publication Identifier').
column_description('FBrf_id', "FlyBase reference ID for the publication.", identifier, 'Reference').
column_description(gene_fullname, "The official full name for this gene.", name, 'Gene Name').
column_description('Gene_ID', "The FlyBase FBgn ID for the expressed gene.", identifier, 'Gene').
column_description(gene_primary_id, "The unique FlyBase gene ID for this gene.", identifier, 'Gene').
column_description('Gene_Symbol', "The FlyBase symbol for the expressed gene.", symbol, 'Gene Symbol').
column_description(gene_symbol, "The official FlyBase symbol for this gene.", symbol, 'Gene Symbol').
column_description(gene_type, "The type of gene.", category, 'Gene Type').
column_description('GeneSymbol', "The official FlyBase symbol for this gene.", symbol, 'Gene Symbol').
column_description(interaction, "Interaction information associated with allele.", text, 'Interaction Info').
column_description('Interaction_type', "Type of interaction observed, either 'suppressible' or 'enhanceable'.", category, 'Interaction Type').
column_description('Mean_Expression', "The average level of expression of the gene across all cells of the cluster.", numeric, 'Expression Level').
column_description(organism_abbreviation, "Abbreviation indicating the species of origin.", abbreviation, 'Organism').
column_description('Parent_library_FBlc', "The unique FlyBase ID for the dataset project to which the RNA-Seq experiment belongs.", identifier, 'Dataset Project').
column_description('Parent_library_name', "The official FlyBase symbol for the dataset project to which the RNA-Seq experiment belongs.", name, 'Dataset Project Name').
column_description(phenotype_id, "Phenotypic identifier associated with the genotype.", identifier, 'Phenotype Identifier').
column_description(phenotype_name, "Phenotypic name associated with the genotype.", name, 'Phenotype Name').
column_description('PMID', "PubMed ID for the publication.", identifier, 'Publication').
column_description(primary_FBid, "Primary FlyBase identifier for the object.", identifier, 'Object').
column_description('Pub_ID', "The FlyBase FBrf ID for the reference in which the expression was reported.", identifier, 'Publication').
column_description('Pub_miniref', "The FlyBase citation for the publication in which the expression was reported.", citation, 'Publication Citation').
column_description('Publication_FBrf', "Current FlyBase identifier (FBrf) of publication from which the data came.", identifier, 'Publication Reference').
column_description(reference, "Current FlyBase identifer (FBrf) of publication from which data came.", identifier, 'Publication Identifier').
column_description('Release_ID', "The D. melanogaster annotation set version from which the gene model used in the analysis derives.", version, 'Annotation Version').
column_description('RNASource_FBlc', "The unique FlyBase ID for the RNA-Seq experiment used for RPKM expression calculation.", identifier, 'RNA-Seq Experiment').
column_description('RNASource_name', "The official FlyBase symbol for the RNA-Seq experiment used for RPKM expression calculation.", name, 'RNA-Seq Experiment Name').
column_description('RPKM_value', "The RPKM expression value for the gene in the specified RNA-Seq experiment.", numeric, 'Expression Value').
column_description('Source_Tissue_Anatomy', "The anatomical region of the source tissue used for the experiment.", category, 'Tissue Anatomy').
column_description('Source_Tissue_Sex', "The sex of the source tissue used for the experiment.", category, 'Tissue Sex').
column_description('Source_Tissue_Stage', "The life stage of the source tissue used for the experiment.", category, 'Tissue Stage').
column_description('Spread', "The proportion of cells in the cluster in which the gene is detected.", proportion, 'Expression Spread').
column_description('Total_exon_base_count', "The number of bases in all exons of this gene.", numeric, 'Total Exonic Base Count').
column_description('UniProt_database', "Database in UniProt where the protein is listed (either UniProt/TrEMBL or UniProt/Swiss-Prot).", category, 'Protein Database').
column_description('UniProt_id', "Unique identifier for the protein in UniProt.", identifier, 'Protein').
column_description('Unique_exon_base_count', "The number of exonic bases unique to the gene (not overlapping exons of other genes).", numeric, 'Exonic Base Count').
column_description(listOf(fullname_synonym, ['|']), "Non-current full name(s) associated with the object.", list, 'Name Synonyms').
column_description(listOf(genotype_FBids, [/, ' ']), "Current FlyBase identifier(s) of the components that make up the genotype.", list, 'Genotype Identifiers').
column_description(listOf(genotype_symbols, [/, ' ']), "Current FlyBase symbol(s) of the components that make up the genotype.", list, 'Genotype Symbols').
column_description(listOf('Interacting_gene_FBgn', ['|']), "Current FlyBase identifier (FBgn) of gene(s) involved in the interacting genotype.", list, 'Gene Identifier').
column_description(listOf('Interacting_gene_symbol', ['|']), "Current FlyBase symbol of gene(s) involved in the interacting genotype.", list, 'Gene Symbol').
column_description(listOf(qualifier_ids, ['|']), "Qualifier identifier(s) associated with phenotypic data for genotype.", list, 'Qualifier Identifiers').
column_description(listOf(qualifier_names, ['|']), "Qualifier name(s) associated with phenotypic data for genotype.", list, 'Qualifier Names').
column_description(listOf('Starting_gene_FBgn', ['|']), "Current FlyBase identifier (FBgn) of gene(s) involved in the starting genotype.", list, 'Gene Identifier').
column_description(listOf('Starting_gene_symbol', ['|']), "Current FlyBase symbol of gene(s) involved in the starting genotype.", list, 'Gene Symbol').
column_description(listOf(symbol_synonym, ['|']), "Non-current symbol(s) associated with the object.", list, 'Symbol Synonyms').

primary_column(fb_synonym, primary_FBid).
primary_column(gene_genetic_interactions, 'Starting_gene_FBgn').
primary_column(gene_rpkm_matrix, gene_primary_id).
primary_column(gene_rpkm_report, 'FBgn').
primary_column(genotype_phenotype_data, genotype_FBids).
primary_column(pmid_fbgn_uniprot, 'FBgn_id').
primary_column('scRNA-Seq_gene_expression', 'Gene_ID').
primary_column(allele_genetic_interactions, allele_FBal).
primary_column(fbgn_exons2affy1_overlaps, 'FBgn').
primary_column(fbgn_exons2affy2_overlaps, 'FBgn').
primary_column(dataset_metadata, 'Item_ID').
primary_column(dmel_paralogs, 'Paralog_FBgn').





too_generic(Var):- var(Var),!,fail.
too_generic(pub_id).
too_generic(X):- \+ atomic_list_concat([_,_,_|_],'_',X).


fix_header_names(Fn,Header,GNames):- 
   maplist(fix_header_names(Header,Fn),Header,ArgTypes),
   include( \=(''),ArgTypes,GNames).


%fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_','_id','_ID']),atom_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(_FL,_Fn,ID,Out):- number(ID),!,Out=ID.
fix_header_names(FL,Fn,listOf(ID),listOf(Out)):- fix_header_names(FL,Fn,ID,Out),!.
fix_header_names(FL,Fn,listOf(ID,Sep),listOf(Out,Sep)):- fix_header_names(FL,Fn,ID,Out),!.
fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_']),atom_concat(MID,RF,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,Out):- member(RF,['#',' ','_']),atom_concat(RF,MID,ID),!,fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,Out):- member(RF,['__',' ']),atomic_list_concat(MIDL,RF,ID),MIDL\=[_],atomic_list_concat(MIDL,'_',MID),!,
   fix_header_names(FL,Fn,MID,Out).
fix_header_names(FL,Fn,ID,listOf(AOut)):- member(RF,['(es)','(s)','ids']),atomic_list_concat([Left,Right],RF,ID),atomic_list_concat([Left,Right],'_',MID),!,
   fix_header_names(FL,Fn,MID,AOut),!. % atom_concat('ListOf_',AOut,Out),!.
fix_header_names(FL,Fn,TT,listOf(AOut)):-
   member(IDs=ID,['IDs'='ID']),
   atom_concat(Type,IDs,TT),
   atom_concat(Type,ID,MID),
   fix_header_names(FL,Fn,MID,AOut),!.
fix_header_names(FL,Fn,ID,listOf(AOut)):- member(RFS=RF,['_IDs'='_ID','IDs'='ID']),
   atomic_list_concat([Left,Right],RFS,ID),
   atomic_list_concat([Left,Right],RF,MID),!,
   fix_header_names(FL,Fn,MID,AOut),!. % atom_concat('ListOf_',AOut,Out),!.


fix_header_names(_,_,Name,Name):- \+ too_generic(Name),!.
fix_header_names(_,_,Name,Name):- atomic_list_concat([_,_|_],'_',Name),!.
%fix_header_names(_,Fn,ID,Out):- atomic_list_concat([Fn,ID],'_column_',Out).
%fix_header_names(FieldList,Fn,ID,Out):- atomic_list_concat([Fn,ID],'_',Out), \+ member(Out,FieldList).
fix_header_names(_,_,Name,Name).


pmt :-flybase_tables(FBT),for_all(member(T,FBT), ( '\\+'(flybase_cols(T,_)) -> format('~N~q.~n',[get_fbt(T)]);true)).
use_flybase_cols(Table,Columns):-
 must_det_ll((
  maplist(fix_header_names(Columns,Table),Columns,ArgTypes),
  assert(flybase_col_names(Table,Columns,ArgTypes)),
  do_arity_2_names(Table,ArgTypes))).

do_arity_2_names(Table,[ID|ArgTypes]):-
  must_det_ll_r((
  atom_concat('data_',Table,F),
  length([ID|ArgTypes],Arity),
  length(Args,Arity),
  DataCall=..[F|Args],
  do_arity_2_names_dc(Table,DataCall,2,ArgTypes))).

do_arity_2_names_dc(Table,DataCall,N,[Nth|ArgTypes]):-
  do_arity_2_names_dc1(Table,DataCall,N,Nth),!,
  N2 is N+1, do_arity_2_names_dc(Table,DataCall,N2,ArgTypes).
do_arity_2_names_dc(_Table,_DataCall,_N,[]).

do_arity_2_names_dc1(Table,DataCall,N,Nth):-
 must_det_ll((
  arg(1,DataCall,Arg1Data),
  arg(N,DataCall,Arg2Data),
  make_arity_2_name(Table,Nth,Arity2),
  Arg1=..[Table,Arg1Data],
  clip_id(Nth,NthNoID),
  (Nth==NthNoID -> Arg2=Arg2Data ;  Arg2 =..[NthNoID,Arg2Data]),
  Arity2Call=..[Arity2,Arg1,Arg2],
  fbug((Arity2Call:-DataCall)),
  fb_assert((Arity2Call:-DataCall)))).

make_arity_2_name(Table,Nth,Arity2):-
  clip_id(Nth,NthNoID),
  (atom_concat(Table,_,Nth)
    -> Arity2 = Nth
    ; atomic_list_concat([Table,NthNoID],'_',Arity2)).


clip_id(Nth,ID):- (atom_concat(ID,'_id',Nth)->true;Nth=ID),!.




setup_flybase_cols:- forall(flybase_cols(Table,Columns),
  use_flybase_cols(Table,Columns)).

%:- load_flybase("das_precomputed/allele_genetic_interactions_fb_2022_06.tsv").




flybase_cols(allele_genetic_interactions,['##allele_symbol','allele_FBal#',interaction,'FBrf#']).

flybase_cols(analysis,[ analysis_id,name,description,program,programversion,algorithm,sourcename,sourceversion,sourceuri,timeexecuted]).
flybase_cols(analysisfeature,[ analysisfeature_id,feature_id,analysis_id,rawscore,normscore,significance,identity]).
flybase_cols(analysisgrp,[ analysisgrp_id,rawscore,normscore,significance,identity,analysis_id,grp_id]).
flybase_cols(analysisgrpmember,[ analysisgrpmember_id,rawscore,normscore,significance,identity,analysis_id,grpmember_id]).
flybase_cols(analysisprop,[ analysisprop_id,analysis_id,type_id,value]).
flybase_cols(audit_chado,[ audit_transaction,transaction_timestamp,userid,audited_table,record_pkey,record_ukey_cols,record_ukey_vals,audited_cols,audited_vals]).

flybase_cols(cell_line,[ cell_line_id,name,uniquename,organism_id,timeaccessioned,timelastmodified]).
flybase_cols(cell_line_loaderm,[ cell_line_loaderm_id,cell_line_id,loaderm_id,pub_id,rank]).
flybase_cols(cell_line_loadermprop,[ cell_line_loadermprop_id,cell_line_loaderm_id,type_id,value,rank]).
flybase_cols(cell_line_dbxref,[ cell_line_dbxref_id,cell_line_id,dbxref_id,is_current]).
flybase_cols(cell_line_feature,[ cell_line_feature_id,cell_line_id,feature_id,pub_id]).
flybase_cols(cell_line_library,[ cell_line_library_id,cell_line_id,library_id,pub_id]).
flybase_cols(cell_line_libraryprop,[ cell_line_libraryprop_id,cell_line_library_id,type_id,value,rank]).
flybase_cols(cell_line_relationship,[ cell_line_relationship_id,subject_id,object_id,type_id]).
flybase_cols(cell_line_strain,[ cell_line_strain_id,strain_id,cell_line_id,pub_id]).
flybase_cols(cell_line_strainprop,[ cell_line_strainprop_id,cell_line_strain_id,type_id,value,rank]).
flybase_cols(cell_line_synonym,[ cell_line_synonym_id,cell_line_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(cell_lineprop,[ cell_lineprop_id,cell_line_id,type_id,value,rank]).
flybase_cols(cell_lineprop_pub,[ cell_lineprop_pub_id,cell_lineprop_id,pub_id]).
flybase_cols(cell_line_pub,[ cell_line_pub_id,cell_line_id,pub_id]).
flybase_cols(contact,[ contact_id,description,name]).
flybase_cols(cv,[ cv_id,name,definition]).
flybase_cols(loaderm,[ loaderm_id,cv_id,definition,dbxref_id,is_obsolete,is_relationshiptype,name]).
flybase_cols(loaderm_dbxref,[ loaderm_dbxref_id,loaderm_id,dbxref_id,is_for_definition]).
flybase_cols(loaderm_relationship,[ loaderm_relationship_id,type_id,subject_id,object_id]).
flybase_cols(loadermpath,[ loadermpath_id,type_id,subject_id,object_id,cv_id,pathdistance]).
flybase_cols(loadermprop,[ loadermprop_id,loaderm_id,type_id,value,rank]).
flybase_cols(loadermsynonym,[ loadermsynonym_id,loaderm_id,name,type_id]).
flybase_cols(db,[ db_id,name,contact_id,description,urlprefix,url]).
flybase_cols(dbxref,[ dbxref_id,db_id,accession,version,description,url]).
flybase_cols(dbxrefprop,[ dbxrefprop_id,dbxref_id,type_id,value,rank]).
flybase_cols(eimage,[ eimage_id,eimage_data,eimage_type,image_uri]).
flybase_cols(environment,[ environment_id,uniquename,description]).
flybase_cols(environment_loaderm,[ environment_loaderm_id,environment_id,loaderm_id]).
flybase_cols(expression,[ expression_id,uniquename,md5checksum,description]).
flybase_cols(expression_loaderm,[ expression_loaderm_id,expression_id,loaderm_id,rank,loaderm_type_id]).
flybase_cols(expression_loadermprop,[ expression_loadermprop_id,expression_loaderm_id,type_id,value,rank]).
flybase_cols(expression_image,[ expression_image_id,expression_id,eimage_id]).
flybase_cols(expressionprop,[ expressionprop_id,expression_id,type_id,value,rank]).
flybase_cols(expression_pub,[ expression_pub_id,expression_id,pub_id]).
flybase_cols(feature,[ feature_id,dbxref_id,organism_id,name,uniquename,residues,seqlen,md5checksum,type_id,is_analysis,timeaccessioned,timelastmodified,is_obsolete]).
flybase_cols(feature_loaderm,[ feature_loaderm_id,feature_id,loaderm_id,pub_id,is_not]).
flybase_cols(feature_loaderm_dbxref,[ feature_loaderm_dbxref_id,feature_loaderm_id,dbxref_id]).
flybase_cols(feature_loadermprop,[ feature_loadermprop_id,feature_loaderm_id,type_id,value,rank]).
flybase_cols(feature_dbxref,[ feature_dbxref_id,feature_id,dbxref_id,is_current]).
flybase_cols(feature_expression,[ feature_expression_id,expression_id,feature_id,pub_id]).
flybase_cols(feature_expressionprop,[ feature_expressionprop_id,feature_expression_id,type_id,value,rank]).
flybase_cols(feature_genotype,[ feature_genotype_id,feature_id,genotype_id,chromosome_id,rank,cgroup,loaderm_id]).
flybase_cols(feature_grpmember,[ feature_grpmember_id,grpmember_id,feature_id]).
flybase_cols(feature_grpmember_pub,[ feature_grpmember_pub_id,pub_id,feature_grpmember_id]).
flybase_cols(feature_humanhealth_dbxref,[ feature_humanhealth_dbxref_id,humanhealth_dbxref_id,feature_id,pub_id]).
flybase_cols(feature_interaction,[ feature_interaction_id,feature_id,interaction_id,role_id,rank]).
flybase_cols(feature_interactionprop,[ feature_interactionprop_id,feature_interaction_id,type_id,value,rank]).
flybase_cols(feature_interaction_pub,[ feature_interaction_pub_id,feature_interaction_id,pub_id]).
flybase_cols(feature_phenotype,[ feature_phenotype_id,feature_id,phenotype_id]).
flybase_cols(feature_pubprop,[ feature_pubprop_id,feature_pub_id,type_id,value,rank]).
flybase_cols(feature_relationship,[ feature_relationship_id,subject_id,object_id,type_id,rank,value]).
flybase_cols(feature_relationshipprop,[ feature_relationshipprop_id,feature_relationship_id,type_id,value,rank]).
flybase_cols(feature_relationshipprop_pub,[ feature_relationshipprop_pub_id,feature_relationshipprop_id,pub_id]).
flybase_cols(feature_relationship_pub,[ feature_relationship_pub_id,feature_relationship_id,pub_id]).
flybase_cols(feature_synonym,[ feature_synonym_id,synonym_id,feature_id,pub_id,is_current,is_internal]).
flybase_cols(featureloc,[ featureloc_id,feature_id,srcfeature_id,fmin,is_fmin_partial,fmax,is_fmax_partial,strand,phase,residue_info,locgroup,rank]).
flybase_cols(featureloc_pub,[ featureloc_pub_id,featureloc_id,pub_id]).
flybase_cols(featuremap,[ featuremap_id,name,description,unittype_id]).
flybase_cols(featuremap_pub,[ featuremap_pub_id,featuremap_id,pub_id]).
flybase_cols(featurepos,[ featurepos_id,featuremap_id,feature_id,map_feature_id,mappos]).
flybase_cols(featureprop,[ featureprop_id,feature_id,type_id,value,rank]).
flybase_cols(featureprop_pub,[ featureprop_pub_id,featureprop_id,pub_id]).
flybase_cols(feature_pub,[ feature_pub_id,feature_id,pub_id]).
flybase_cols(featurerange,[ featurerange_id,featuremap_id,feature_id,leftstartf_id,leftendf_id,rightstartf_id,rightendf_id,rangestr]).
flybase_cols(genotype,[ genotype_id,uniquename,description,name,is_obsolete]).
flybase_cols(genotype_loaderm,[ genotype_loaderm_id,genotype_id,loaderm_id,pub_id,is_not,rank]).
flybase_cols(genotype_loadermprop,[ genotype_loadermprop_id,genotype_loaderm_id,type_id,value,rank]).
flybase_cols(genotype_dbxref,[ genotype_dbxref_id,genotype_id,dbxref_id,is_current]).
flybase_cols(genotype_synonym,[ genotype_synonym_id,genotype_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(genotypeprop,[ genotypeprop_id,genotype_id,type_id,value,rank,cvalue_id]).
flybase_cols(genotypeprop_pub,[ genotypeprop_pub_id,genotypeprop_id,pub_id]).
flybase_cols(genotype_pub,[ genotype_pub_id,genotype_id,pub_id]).
flybase_cols(grp,[ grp_id,name,uniquename,type_id,is_analysis,is_obsolete]).
flybase_cols(grp_loaderm,[ grp_loaderm_id,is_not,loaderm_id,grp_id,pub_id]).
flybase_cols(grp_dbxref,[ grp_dbxref_id,is_current,dbxref_id,grp_id]).
flybase_cols(grp_pubprop,[ grp_pubprop_id,value,rank,type_id,grp_pub_id]).
flybase_cols(grp_relationship,[ grp_relationship_id,value,rank,type_id,subject_id,object_id]).
flybase_cols(grp_relationshipprop,[ grp_relationshipprop_id,value,rank,type_id,grp_relationship_id]).
flybase_cols(grp_relationship_pub,[ grp_relationship_pub_id,pub_id,grp_relationship_id]).
flybase_cols(grp_synonym,[ grp_synonym_id,synonym_id,grp_id,pub_id,is_current,is_internal]).
flybase_cols(grpmember,[ grpmember_id,rank,type_id,grp_id]).
flybase_cols(grpmember_loaderm,[ grpmember_loaderm_id,is_not,loaderm_id,grpmember_id,pub_id]).
flybase_cols(grpmemberprop,[ grpmemberprop_id,value,rank,type_id,grpmember_id]).
flybase_cols(grpmemberprop_pub,[ grpmemberprop_pub_id,pub_id,grpmemberprop_id]).
flybase_cols(grpmember_pub,[ grpmember_pub_id,pub_id,grpmember_id]).
flybase_cols(grpprop,[ grpprop_id,value,rank,type_id,grp_id]).
flybase_cols(grpprop_pub,[ grpprop_pub_id,pub_id,grpprop_id]).
flybase_cols(grp_pub,[ grp_pub_id,pub_id,grp_id]).
flybase_cols(humanhealth,[ humanhealth_id,name,uniquename,organism_id,dbxref_id,is_obsolete]).
flybase_cols(humanhealth_loaderm,[ humanhealth_loaderm_id,humanhealth_id,loaderm_id,pub_id]).
flybase_cols(humanhealth_loadermprop,[ humanhealth_loadermprop_id,humanhealth_loaderm_id,type_id,value,rank]).
flybase_cols(humanhealth_dbxref,[ humanhealth_dbxref_id,humanhealth_id,dbxref_id,is_current]).
flybase_cols(humanhealth_dbxrefprop,[ humanhealth_dbxrefprop_id,humanhealth_dbxref_id,type_id,value,rank]).
flybase_cols(humanhealth_dbxrefprop_pub,[ humanhealth_dbxrefprop_pub_id,humanhealth_dbxrefprop_id,pub_id]).
flybase_cols(humanhealth_feature,[ humanhealth_feature_id,humanhealth_id,feature_id,pub_id]).
flybase_cols(humanhealth_featureprop,[ humanhealth_featureprop_id,humanhealth_feature_id,type_id,value,rank]).
flybase_cols(humanhealth_phenotype,[ humanhealth_phenotype_id,humanhealth_id,phenotype_id,pub_id]).
flybase_cols(humanhealth_phenotypeprop,[ humanhealth_phenotypeprop_id,humanhealth_phenotype_id,type_id,value,rank]).
flybase_cols(humanhealth_pubprop,[ humanhealth_pubprop_id,value,rank,type_id,humanhealth_pub_id]).
flybase_cols(humanhealth_relationship,[ humanhealth_relationship_id,subject_id,object_id,type_id,value,rank]).
flybase_cols(humanhealth_relationship_pub,[ humanhealth_relationship_pub_id,humanhealth_relationship_id,pub_id]).
flybase_cols(humanhealth_synonym,[ humanhealth_synonym_id,humanhealth_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(humanhealthprop,[ humanhealthprop_id,humanhealth_id,type_id,value,rank]).
flybase_cols(humanhealthprop_pub,[ humanhealthprop_pub_id,humanhealthprop_id,pub_id]).
flybase_cols(humanhealth_pub,[ humanhealth_pub_id,humanhealth_id,pub_id]).
flybase_cols(interaction,[ interaction_id,uniquename,type_id,description,is_obsolete]).
flybase_cols(interaction_cell_line,[ interaction_cell_line_id,cell_line_id,interaction_id,pub_id]).
flybase_cols(interaction_loaderm,[ interaction_loaderm_id,interaction_id,loaderm_id]).
flybase_cols(interaction_loadermprop,[ interaction_loadermprop_id,interaction_loaderm_id,type_id,value,rank]).
flybase_cols(interaction_expression,[ interaction_expression_id,expression_id,interaction_id,pub_id]).
flybase_cols(interaction_expressionprop,[ interaction_expressionprop_id,interaction_expression_id,type_id,value,rank]).
flybase_cols(interaction_group,[ interaction_group_id,uniquename,is_obsolete,description]).
flybase_cols(interaction_group_feature_interaction,[ interaction_group_feature_interaction_id,interaction_group_id,feature_interaction_id,rank,ftype]).
flybase_cols(interactionprop,[ interactionprop_id,interaction_id,type_id,value,rank]).
flybase_cols(interactionprop_pub,[ interactionprop_pub_id,interactionprop_id,pub_id]).
flybase_cols(interaction_pub,[ interaction_pub_id,interaction_id,pub_id]).
flybase_cols(library,[ library_id,organism_id,name,uniquename,type_id,is_obsolete,timeaccessioned,timelastmodified]).
flybase_cols(library_loaderm,[ library_loaderm_id,library_id,loaderm_id,pub_id]).
flybase_cols(library_loadermprop,[ library_loadermprop_id,library_loaderm_id,type_id,value,rank]).
flybase_cols(library_dbxref,[ library_dbxref_id,library_id,dbxref_id,is_current]).
flybase_cols(library_dbxrefprop,[ library_dbxrefprop_id,library_dbxref_id,type_id,value,rank]).
flybase_cols(library_expression,[ library_expression_id,expression_id,library_id,pub_id]).
flybase_cols(library_expressionprop,[ library_expressionprop_id,library_expression_id,type_id,value,rank]).
flybase_cols(library_feature,[ library_feature_id,library_id,feature_id]).
flybase_cols(library_featureprop,[ library_featureprop_id,library_feature_id,type_id,value,rank]).
flybase_cols(library_grpmember,[ library_grpmember_id,grpmember_id,library_id]).
flybase_cols(library_humanhealth,[ library_humanhealth_id,humanhealth_id,library_id,pub_id]).
flybase_cols(library_humanhealthprop,[ library_humanhealthprop_id,library_humanhealth_id,type_id,value,rank]).
flybase_cols(library_interaction,[ library_interaction_id,interaction_id,library_id,pub_id]).
flybase_cols(library_relationship,[ library_relationship_id,subject_id,object_id,type_id]).
flybase_cols(library_relationship_pub,[ library_relationship_pub_id,library_relationship_id,pub_id]).
flybase_cols(library_strain,[ library_strain_id,strain_id,library_id,pub_id]).
flybase_cols(library_strainprop,[ library_strainprop_id,library_strain_id,type_id,value,rank]).
flybase_cols(library_synonym,[ library_synonym_id,synonym_id,library_id,pub_id,is_current,is_internal]).
flybase_cols(libraryprop,[ libraryprop_id,library_id,type_id,value,rank]).
flybase_cols(libraryprop_pub,[ libraryprop_pub_id,libraryprop_id,pub_id]).
flybase_cols(library_pub,[ library_pub_id,library_id,pub_id]).
flybase_cols(lock,[ lock_id,username,locktype,lockname,lockrank,lockstatus,timeaccessioend,timelastmodified,chadoxmlfile,comment,task]).
flybase_cols(organism,[ organism_id,abbreviation,genus,species,common_name,comment]).
flybase_cols(organism_loaderm,[ organism_loaderm_id,organism_id,loaderm_id,rank,pub_id]).
flybase_cols(organism_loadermprop,[ organism_loadermprop_id,organism_loaderm_id,type_id,value,rank]).
flybase_cols(organism_dbxref,[ organism_dbxref_id,organism_id,dbxref_id,is_current]).
flybase_cols(organism_grpmember,[ organism_grpmember_id,grpmember_id,organism_id]).
flybase_cols(organism_library,[ organism_library_id,organism_id,library_id]).
flybase_cols(organismprop,[ organismprop_id,organism_id,type_id,value,rank]).
flybase_cols(organismprop_pub,[ organismprop_pub_id,organismprop_id,pub_id]).
flybase_cols(organism_pub,[ organism_pub_id,organism_id,pub_id]).
flybase_cols(phendesc,[ phendesc_id,genotype_id,environment_id,description,type_id,pub_id]).
flybase_cols(phenotype,[ phenotype_id,uniquename,observable_id,attr_id,value,cvalue_id,assay_id]).
flybase_cols(phenotype_comparison,[ phenotype_comparison_id,genotype1_id,environment1_id,genotype2_id,environment2_id,phenotype1_id,phenotype2_id,pub_id,organism_id]).
flybase_cols(phenotype_comparison_loaderm,[ phenotype_comparison_loaderm_id,phenotype_comparison_id,loaderm_id,rank]).
flybase_cols(phenotype_loaderm,[ phenotype_loaderm_id,phenotype_id,loaderm_id,rank]).
flybase_cols(phenstatement,[ phenstatement_id,genotype_id,environment_id,phenotype_id,type_id,pub_id]).
flybase_cols(project,[ project_id,name,description]).
flybase_cols(pub,[ pub_id,title,volumetitle,volume,series_name,issue,pyear,pages,miniref,type_id,is_obsolete,publisher,pubplace,uniquename]).
flybase_cols(pub_dbxref,[ pub_dbxref_id,pub_id,dbxref_id,is_current]).
flybase_cols(pub_relationship,[ pub_relationship_id,type_id,subject_id,object_id]).
flybase_cols(pubauthor,[ pubauthor_id,pub_id,rank,editor,surname,givennames,suffix]).
flybase_cols(pubprop,[ pubprop_id,pub_id,type_id,value,rank]).
flybase_cols(stock,[ stock_id,dbxref_id,organism_id,name,uniquename,description,type_id,is_obsolete]).
flybase_cols(stock_loaderm,[ stock_loaderm_id,stock_id,loaderm_id,pub_id]).
flybase_cols(stock_dbxref,[ stock_dbxref_id,stock_id,dbxref_id,is_current]).
flybase_cols(stock_genotype,[ stock_genotype_id,stock_id,genotype_id]).
flybase_cols(stock_relationship,[ stock_relationship_id,subject_id,object_id,type_id,value,rank]).
flybase_cols(stock_relationship_pub,[ stock_relationship_pub_id,stock_relationship_id,pub_id]).
flybase_cols(stockcollection,[ stockcollection_id,type_id,contact_id,name,uniquename]).
flybase_cols(stockcollection_stock,[ stockcollection_stock_id,stockcollection_id,stock_id]).
flybase_cols(stockcollectionprop,[ stockcollectionprop_id,stockcollection_id,type_id,value,rank]).
flybase_cols(stockprop,[ stockprop_id,stock_id,type_id,value,rank]).
flybase_cols(stockprop_pub,[ stockprop_pub_id,stockprop_id,pub_id]).
flybase_cols(stock_pub,[ stock_pub_id,stock_id,pub_id]).
flybase_cols(strain,[ strain_id,name,uniquename,organism_id,dbxref_id,is_obsolete]).
flybase_cols(strain_loaderm,[ strain_loaderm_id,strain_id,loaderm_id,pub_id]).
flybase_cols(strain_loadermprop,[ strain_loadermprop_id,strain_loaderm_id,type_id,value,rank]).
flybase_cols(strain_dbxref,[ strain_dbxref_id,strain_id,dbxref_id,is_current]).
flybase_cols(strain_feature,[ strain_feature_id,strain_id,feature_id,pub_id]).
flybase_cols(strain_featureprop,[ strain_featureprop_id,strain_feature_id,type_id,value,rank]).
flybase_cols(strain_phenotype,[ strain_phenotype_id,strain_id,phenotype_id,pub_id]).
flybase_cols(strain_phenotypeprop,[ strain_phenotypeprop_id,strain_phenotype_id,type_id,value,rank]).
flybase_cols(strain_relationship,[ strain_relationship_id,subject_id,object_id,type_id,value,rank]).
flybase_cols(strain_relationship_pub,[ strain_relationship_pub_id,strain_relationship_id,pub_id]).
flybase_cols(strain_synonym,[ strain_synonym_id,strain_id,synonym_id,pub_id,is_current,is_internal]).
flybase_cols(strainprop,[ strainprop_id,strain_id,type_id,value,rank]).
flybase_cols(strainprop_pub,[ strainprop_pub_id,strainprop_id,pub_id]).
flybase_cols(strain_pub,[ strain_pub_id,strain_id,pub_id]).
flybase_cols(synonym,[ synonym_id,name,type_id,synonym_sgml]).
flybase_cols(tableinfo,[ tableinfo_id,name,primary_key_column,is_view,view_on_table_id,superclass_table_id,is_updateable,modification_date]).
flybase_cols(update_track,[ update_track_id,release,fbid,time_update,author,statement,comment,annotation_id]).


table_columns(T,List):- table_columns_tt(TT,List), eigther_contains(T,TT),!.

table_columns_tt(TT,List):- column_names(TT,List).
table_columns_tt(TT,List):- column_names(TT,List).
table_columns_tt(TT,List):- flybase_cols(TT,List).
table_columns_tt(TT,List):- t_h_n(TT,_,List).

eigther_contains(TT,T):- TT=T,!.
eigther_contains(T,TT):- atom_contains(T,TT),!.
eigther_contains(TT,T):- atom_contains(T,TT),!.






list_column_names:-
  forall((column_names(T,CNs),once((length(CNs,Len),Len>=2,fb_pred(T,Len)))),
  (print(column_names(T,CNs)),nl)).


%:- ensure_loaded(read_obo).

%:- prolog_load_context(source,This),forall((source_file(P0,This),functor(P0,F,0)),writeln(add_history1(F))).
%add_history1(setup_flybase_cols)
%add_history1(pmt)
ah:- add_history1(fb_stats),
  add_history1(mine_overlaps),
  add_history1(load_flybase).
ah:- add_history(fb_stats),
  add_history(mine_overlaps),
  add_history(try_overlaps),
  add_history(load_flybase).
:- ah,ah,ah.

%:- initialization(load_flybase).
:- save_pre_statistic(memory).
:- save_pre_statistic(atoms).
:- save_pre_statistic(atom_space).

:- fb_stats.



