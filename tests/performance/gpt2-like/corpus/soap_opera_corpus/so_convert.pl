:- encoding(iso_latin_1).
:- module(so_convert,[]).
:- encoding(iso_latin_1).


% :- include(weightless_so_convert).

so_convert_preds([converting/3,is_word/1,is_word/2,ngram/5,ngram/6,trigram/3,trigram/4,tok_split/3,tok_split/4]).

declare_preds(X):- dynamic(X),multifile(X).

:- so_convert_preds(L), maplist(declare_preds,L).

:- declare_preds(scene_info/5).
% :- ensure_loaded(converts_trigrams).

:- use_module(library(logicmoo_utils)).
:- must((prolog_load_context(directory,Dir),absolute_file_name('../..',PlData,[relative_to(Dir),file_type(directory)]),
  assert_if_new(user:file_search_path(pldata,PlData)))).


% debug printing
debugln(X):- debugln_xfrm(X,S), dmsg(S).
fmt_pllm(X):- debugln_xfrm(X,S), fmt(S).

debugln_xfrm(Insts,S):- var(Insts), !, sformat(S,"~p",[Insts]).
debugln_xfrm(i(X),S):- \+ is_list(X) -> debugln_xfrm(X,S) ; maplist(debugln_xfrm,X,Y),atomics_to_string(Y,' ',S).
debugln_xfrm([N|A],S):- is_list(A),!,maplist(debugln_xfrm,[N|A],Y),atomics_to_string(Y,' ',S).
debugln_xfrm((F/A),S):- functor(P,F,A),predicate_property(P,number_of_clauses(Insts)),!,sformat(S,"~w=~d~n",[(F/A),Insts]).
debugln_xfrm(w(E),S):- sformat(S,'~p',E),!.
debugln_xfrm('$'(E),S):- get_flag(E,Insts),!,sformat(S,"~w=~d~n",[E,Insts]).
debugln_xfrm(N=V,S):- integer(V),!,sformat(S,"~n\t~w\t= ~d ",[N,V]).
debugln_xfrm(N=V,S):- !,sformat(S,"~n\t~w\t= ~w ",[N,V]).
debugln_xfrm([N],S):- !, debugln_xfrm(N,S).
debugln_xfrm(C,S):- tok_split(C,S,_),!.
debugln_xfrm(C,S):- compound(C),!,sformat(S,"~p",[C]).
%debugln_xfrm(C,S):- compound(C),compound_name_arguments(C,N,A),debugln_xfrm([N|A],S).
debugln_xfrm(nl,'\n'):-!.
debugln_xfrm([],''):-!.
debugln_xfrm(E,E).

nlu:- ensure_loaded(library(logicmoo_nlu)),assert_if_new(is_nlu).
nlu_stop:- ensure_loaded(library(logicmoo_nlu)),retractall(is_nlu).
%:- ensure_loaded(library(logicmoo_nlu/parser_link_grammar)).
:- nlu.
%convert_corpus:- functor(P,ngram,6), predicate_property(P,number_of_clauses(N)),N>2.
convert_corpus_file:-
  tell('corpus_file.pl'),
  convert_corpus,
  told.

convert_corpus:- 
  convert_corpus_in_mem.

reconvert_corpus:- 
  so_convert_preds(L),
  maplist(abolish,L),
  maplist(declare_preds,L),
  convert_corpus_in_mem.

convert_corpus_in_mem:- 
 make,
 convert_from_corpus,
 compute_corpus_extents,
 nop(reconvert_from_trigrams),!.

corpus_stat(corpus_converting). corpus_stat(corpus_nodes). corpus_stat(corpus_node_overlap).
corpus_stat(corpus_unique_toks). corpus_stat(corpus_total_toks). 
corpus_stat(corpus_convos).

set_last_oc(OC):- nb_setval(last_oc,OC).
get_last_oc(OC):- nb_current(last_oc,OC).

% convert_from_corpus:- converting(_,string,_),!,forall(converting(XX,string,Val),add_converting_str(XX,Val)).
convert_from_corpus:- convert_from_corpus(pldata('corpus/soap_opera_corpus/soc_corpus.txt')).


in_temp_dir(G):-
 must(absolute_file_name(pldata('corpus/tmpdata'),Dir,[access(read),file_type(directory)])),
 setup_call_cleanup(working_directory(X,Dir),mor_rtrace(G),working_directory(_,X)).
 

convert_from_corpus(Path):-
 debugln(["reading corpus...",Path]),
 must(absolute_file_name(Path,File,[access(read)])),
setup_call_cleanup(
 forall(corpus_stat(Stat),set_flag(Stat,0)),
 time(( setup_call_cleanup(open(File,read,In,[encoding(iso_latin_1)]), convert_from_corpus_stream(In), close(In)),
 forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))))),
 save_converting).

convert_from_corpus_stream(In):- 
 set_flag(file_line,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 (inc_flag(file_line),
  read_line_to_string(In,Str),get_flag(file_line,X),
  nb_setval('$last_text',Str),
  once(add_file_convert(X,Str)), fail)).


:- add_history(load_converting).
load_converting:- in_temp_dir(load_converting0).
load_converting0:-
  so_convert_preds(L),maplist(load_converting,L).

load_converting(MFA):- !, compute_module(MFA,M,F/A),
 functor(P,F,A),MP=M:P,
 atomic_list_concat(['done_',M,'_',F,'_',A,'.pl'],File),
 (predicate_property(MP,number_of_clauses(Before));Before=0),!,
 ignore((exists_file(File) -> ensure_loaded(File) ; true)),
 (predicate_property(MP,number_of_clauses(After));After=0),!,
 debugln(M:F/A=(Before->After)).

compute_module(MFA,M,FA):- strip_module(MFA,M0,FA),compute_m(M0,M),!.

compute_m(user,so_convert).
compute_m(M,M).

save_converting:- in_temp_dir(save_converting0).
save_converting0:-
  so_convert_preds(L),maplist(save_converting,L).
save_converting(MFA):- !, compute_module(MFA,M,F/A),
 atomic_list_concat(['done_',M,'_',F,'_',A,'.pl'],File),
 tell(File),
 writeq(:- encoding(iso_latin_1)),writeln('.'),
 listing(F/A),
 % functor(P,F,A),forall(P,(writeq(P),writeln('.'))),
 told.


save_stat(G):- 
  ( \+ G -> assert(G) ; true),
  nop((writeq(G),writeln('.'))).

use_extent(is_word,1). use_extent(tok_split,3). use_extent(trigram,3). use_extent(ngram,5).
compute_corpus_extents:-
 debugln("compute corpus extents..."),
 time((forall(use_extent(F,A),compute_extent(F,A)))).

min_of(X,Y,X):-X<Y,!. min_of(_,Y,Y).
max_of(X,Y,X):-X>Y,!. max_of(_,Y,Y).
inc_flag(F):- flag(F,X,X+1).
compute_extent(F,A):-
  functor(NGram,F,A),
  set_flag(total_fa,0),
  set_flag(min_fa,999999999),
  set_flag(max_fa,0),
  forall(NGram,(ngram_val(NGram,NN),
     flag(total_fa,Total,Total+NN),
     get_flag(min_fa,Min),min_of(Min,NN,NewMin),set_flag(min_fa,NewMin),
     get_flag(max_fa,Max),max_of(Max,NN,NewMax),set_flag(max_fa,NewMax),
     append_term(NGram,NN,NGramStat),save_stat(NGramStat))),  
  get_flag(total_fa,Total),
  get_flag(min_fa,Min),
  get_flag(max_fa,Max),
  predicate_property(NGram,number_of_clauses(Insts)),
  max_of(Insts,1,Insts1), % avoid division by zero
  Mean is round(Total/Insts1),
  High is ((Max-Mean)/2 + Mean),
  Low is (Mean-Min)/2 + Min,
  set_flag(med_high_fa, High), set_flag(med_low_fa, Low),
 nop((
  % adds 20 seconds and is not yet used
  set_flag(above_mean_fa, 0), set_flag(above_med_high_fa, 0), set_flag(num_min_fa, 0),
  set_flag(below_mean_fa, 0), set_flag(below_med_low_fa, 0),
  append_term(NGram,NN,NGramStatN),
  forall(NGramStatN,
    (ignore((NN=Min,inc_flag(num_min_fa))),
     ignore((NN>High,inc_flag(above_med_high_fa))),
     ignore((NN<Low,inc_flag(below_med_low_fa))),
     (NN =< Mean ->inc_flag(below_mean_fa);inc_flag(above_mean_fa)))),
  get_flag(num_min_fa, NEMin), get_flag(above_med_high_fa, NAMedHi),
  get_flag(below_mean_fa, NBMean), get_flag(above_mean_fa, NAMean),  
  get_flag(below_med_low_fa, NBMedLo),
  NAMeanNAMedHi is NAMean-NAMedHi,
  NBMeanNBMedLo is NBMean-NBMedLo,
  NBMedLoNEMin is NBMedLo-NEMin,
 !)),
  Props = [
      (min->min)=NEMin,
      (min->low)=NBMedLoNEMin,
      (low->mean)=NBMeanNBMedLo,
      (mean->high)=NAMeanNAMedHi,
      (high->max)=NAMedHi,
      '---------'='------------',
      (min->max)=Insts, 
      nl,
      min=Min,
      low=Low,
      mean=Mean,
      high=High,
      max=Max,
      total=Total],
  maplist(save_extents(F,A),Props),
  debugln([extent_props(F/A),Props]),!.

save_extents(_,_,(_=x)):-!.
save_extents(F,A,X=Y):- !, assert(extent_props(F,A,X,Y)). 
save_extents(_,_,_):-!.

ngram_val(NGram,NN):- ngram_key(NGram,Key),get_flag(Key,NN).

ngram_inc(NGram):- ngram_inc(NGram,_NN).
ngram_inc(NGram,NN):- ngram_key(NGram,Key),flag(Key,NN,NN+1).

ngram_key(tok_split(O,_,_),O):-!.
ngram_key(is_word(O),O):-!.
ngram_key(trigram(A,B,C),Key):- !, join_text([A,B,C],Key).
ngram_key(ngram(Loc,A,B,C,D,_),Key):- !, ngram_key(ngram(Loc,A,B,C,D),Key).
ngram_key(ngram(_Loc,oc(_),B,C,oc(_)),Key):- !, join_text([oc,B,C,oc],Key).
ngram_key(ngram(_Loc,oc(_),A,B,C),Key):- !, join_text([oc,A,B,C],Key).
ngram_key(ngram(_Loc,A,B,C,oc(_)),Key):- !, join_text([A,B,C,oc],Key).
ngram_key(ngram(_Loc,A,B,C,D),Key):- join_text([A,B,C,D],Key).

join_text(List,Key):- atomic_list_concat(List,',',Key).

save_corpus_stats:-
 time((tell('plm.pl'),
 write('
 :- style_check(- discontiguous).
 :- X= (is_word/2,ngram/6),
    dynamic(X),multifile(X). \n'),
  listing([is_word/2,ngram/6]), told)).

qconvert_corpus:- 
  save_corpus_stats,
  debugln("Compiling now..."),
  time(so_convert:qcompile(plm)),
  debugln("Loading now..."),
  time(so_convert:ensure_loaded(plm)),
  debugln("Corpus Ready").


add_file_convert(X,Str):- atomic_list_concat([E,N|List],'\t',Str), !, mor_rtrace(add_converting(X,E,N,List)),!.
add_file_convert(X,Str):- atomic_list_concat([E|List],'\t',Str), !, mor_rtrace(add_converting(X,E,'',List)),!.

begin_scene:- 
  %listing(tmp:in_scene/1),
  retractall(tmp:in_scene(_)),
  nop(wdmsg(begin_scene)),current_so_file(File),
  debugln(file=File),
  current_so_show(Show),current_so_file(File),
  atom_concat(Show,'_scene_',SS),gensym(SS,SceneName),
  nb_setval(current_scene,SceneName),
  nop(format('~N:- ~q.~n', [begin_scene(SceneName)])),!.

delist_lists([X],Y):- is_list(X),!,delist_lists(X,Y).
delist_lists(X,X).
%send_scene(actor(Info),SceneName):- writeq(Info),write(', '),
%send_scene(Info,SceneName):- fail, format('~N~q.~n',[in_scene(Info,SceneName)]).


tokenize_sents_ww(Who,What,S,S9):- must_det_l((tokenize_atom(S,S2),!, fix_token_sents_pass1(S2,S3), !,
  fix_token_sents(Who,What,S3,S4),!,flatten(S4,S5), maplist(de_s,S5,S6),delist_lists(S6,S9))).

toks_join1('-'):- fail.
toks_join(H1,H1):- toks_join1(H1),!.
toks_join(H1,'.'):- join_with_dot(H1),!.
toks_join('\'','s').
toks_join('\'',A):- nop(atom_length(A,1)).
%toks_join(_,'-').
%toks_join('-',_).
toks_join(H1,H2,H12):- toks_join(H1,H2), atom_concat(H1,H2,H12).

never_toks_join1('"').
never_toks_join1(']').
never_toks_join1('[').
never_toks_join(H1,H2):- (never_toks_join1(H1);never_toks_join1(H2)),!.
never_toks_join(_,'\'').
never_toks_join('\'',_).
never_toks_join('.','\'').
never_toks_join('\'','.').

a_number(A):- number(A),!.
a_number(A):- atom(A),atom_number(A,_).

dashes(H2):- H2=='-';H2=='--'.

%:- multifile(contraction/4).
register_all_lexical_items(A,B):- wdmsg(register_all_lexical_items(A,B)).
:- ensure_loaded('/opt/logicmoo_workspace/packs_xtra/logicmoo_chat/npc/Assets/NL/contractions.prolog').
contraction(_,A,B,C,D):- contraction(A,B,C,D). 
contraction(r,U,did,U,d):-downcase_atom(U,D),contract_is(D),!.
contraction(r,U,is,U,s):- downcase_atom(U,D),contract_is(D),!.
contraction(r,U,us,U,s):- downcase_atom(U,D),contract_us(D),!.
contraction(r,U,is,U,s):- downcase_atom(U,D),contract_is(D),!.
contraction(r,U,am,U,m).
contraction(r,U,are,U,re).
contraction(r,U,are,U,r).
contraction(r,U,have,U,ve).
contraction(r,did,not,didn,t).
contraction(r,U,them,U,em).
contraction(r,U,has,U,est).
contraction(r,U,has,U,st).
contraction(r,U,will,U,ll).
contraction(r,U,would,U,d).
contraction(r,U,about,U,bout).
contraction(r,you,all,y,all).
contraction(r,you,know,y,know).
contraction(r,Could,not,Couldn,t):- atom_concat(Could,n,Couldn),nop(once(atom_concat(_,d,Could);atom_concat(_,t,Could);atom_concat(_,s,Could))).
contraction(R,Watching,U,Watchin,U):- R\==c, U\==s, atom_concat(A,in,Watchin),atom_length(A,L),L>1,atom_concat(Watchin,g,Watching).
contraction(r,do,you,d,ya).
contraction(r,come,on,c,mon).
contraction(r,A,B,C,D):- downcase_atom(C,DC), C\==DC,contraction(r,DA,B,DC,D),toPropercase(DA,A).
contraction(r,A,B,C,D):- contraction(c,A,B,C,D).
% contraction(r,can, not, can, t)
contract_us(let).
contract_is(it).
contract_is(how).
contract_is(X):- atom_concat('wh',_,X).
contract_is(X):- atom_concat('th',_,X).

fix_token_sents_pass1([H1,'-',H2|T],[H12|Out]):- \+ dashes(H1),\+ dashes(H2),atomic_list_concat([H1,'-',H2],H12),!,fix_token_sents_pass1(T,Out).
fix_token_sents_pass1(['-','-'|T],Out):- !,fix_token_sents_pass1(['--'|T],Out).
fix_token_sents_pass1([H1,'.',H2|T],[H12|Out]):- a_number(H1), a_number(H2),atomic_list_concat([H1,'.',H2],H12),!,fix_token_sents_pass1(T,Out).

fix_token_sents_pass1(['\'','s',ING|T],Out):- atom_concat(_,ing,ING),fix_token_sents_pass1(['is',ING|T],Out).
fix_token_sents_pass1([A,'\'',B|T],Out):- contraction(r,C,D,A,B),!,fix_token_sents_pass1([C,D|T],Out).
%fix_token_sents_pass1([A,B|T],Out):- contraction(c,C,D,A,B),!,fix_token_sents_pass1([C,D|T],Out).
fix_token_sents_pass1(['\'',Ve|T],Out):- contraction(r,we,Have,we,Ve),fix_token_sents_pass1([Have|T],Out).
fix_token_sents_pass1(['\'','s'|T],Out):- fix_token_sents_pass1(['\'s'|T],Out).
fix_token_sents_pass1(['\'','t'|T],Out):- fix_token_sents_pass1(['\'t'|T],Out).
fix_token_sents_pass1(['('|T],Out):- fix_token_sents_pass1(['['|T],Out).
fix_token_sents_pass1([')'|T],Out):- fix_token_sents_pass1([']'|T],Out).
fix_token_sents_pass1([E,'.',R,'.'|T],Out):- atom_length(E,1),atom_length(R,1),R\=='.',atomic_list_concat([E,'.',R,'.'],ER),!,fix_token_sents_pass1([ER|T],Out).
fix_token_sents_pass1(['.','.'|T],Out):- !,fix_token_sents_pass1(['.'|T],Out).
fix_token_sents_pass1(['?','?'|T],Out):- !,fix_token_sents_pass1(['?'|T],Out).
fix_token_sents_pass1([H1,':',H2|T],[H12|Out]):- a_number(H1), a_number(H2),atomic_list_concat([H1,':',H2],H12),!,fix_token_sents_pass1(T,Out).
fix_token_sents_pass1([H1,H2|T],Out):- \+ never_toks_join(H1,H2), toks_join(H1,H2,H12),!,fix_token_sents_pass1([H12|T],Out).
fix_token_sents_pass1([H1,H1|T],Out):- \+ never_toks_join(H1,H1), !, fix_token_sents_pass1([H1|T],Out).
fix_token_sents_pass1([H1|T],[H1|TT]):- !, fix_token_sents_pass1(T,TT).
fix_token_sents_pass1([],[]).

join_with_dot(W):- atom(W),atom_number(W,_).
join_with_dot('St').
join_with_dot('Dr').
join_with_dot('Mr').
join_with_dot('Ms').
join_with_dot('Mrs').
%join_with_dot(W):-atom_length(W,1).

s_puct(W):-member(W,['--','.','?','!',']']).

is_first_word(_,'[',action).
is_first_word(W,R,W):- nldata_dictionary_some01:atom_or_string(R),downcase_atom(R,D),R\==D.

% fix_token_sents(Who,What,[X],Y):- is_list(X),!,fix_token_sents(Who,What,X,Y).
fix_token_sents(_, _, [], []):-!.
fix_token_sents(Who, What, In, Out):- stack_depth(X),X>5000,dumpST,wdmsg(fix_token_sents(Who, What, In, Out)),break.

fix_token_sents(_,_,[Who,and,Actor2,':'|Rest], Out):- fix_token_sents(together([Who,Actor2]),speak,Rest,Out),!.
fix_token_sents(_,_,[Who,And,Actor2,':'|Rest], Out):- fix_token_sents(fle([Who,And,Actor2]),speak,Rest,Out),!.
fix_token_sents(_,_,[Who,Actor2,':'|Rest], Out):- fix_token_sents(fl([Who,Actor2]),speak,Rest,Out),!.
fix_token_sents(_,_,[Who,':'|Rest], Out):-  fix_token_sents(Who,speak,Rest,Out),!.

fix_token_sents(_,_,['[',Who,and,Actor2|Rest], Out):- is_pers_name(Who), fix_token_sents(together([Who,Actor2]),action,['['|Rest],Out),!.
fix_token_sents(_,_,['[', Who|Rest], Out):- is_pers_name(Who), fix_token_sents(Who,action,['['|Rest],Out),!.
fix_token_sents(Who,What,['['|Rest], [Out1,Out2]):- append(L,[']'|R],Rest), 
 fix_token_sents(Who,action,L,Out1),!,
 fix_token_sents(Who,What,R,Out2),!.

fix_token_sents(Who,What,Rest,[Out1,Out2]):- append(L,['['|R],Rest), \+ member(']',L), L\==[],
  fix_token_sents(Who,What,L,Out1),
  fix_token_sents(Who,What,['['|R],Out2), !.
/*
fix_token_sents(Who,What,Rest,[Out1,Out2]):- append(L,[']'|R],Rest), \+ member('[',L),
  append(L,[']'],LR),
  fix_token_sents(Who,LR,Out1),
  fix_token_sents(Who,What,R,Out2), !.
*/

fix_token_sents(_,action,[Who,and,Actor2|Rest], Out):- is_pers_name(Who), fix_token_sents(together([Who,Actor2]),action,Rest,Out),!.
fix_token_sents(_,action,[Who|Rest], Out):- is_pers_name(Who), fix_token_sents(Who,action,Rest,Out),!.

fix_token_sents(Who,What,SSS,[Out1,Out2]):- 
   append(L,[L2,W,R|RR],SSS), \+ join_with_dot(L2),s_puct(W),append(L,[L2,W],LS),
   is_first_word(What,R,Waht2),
   fix_token_sents(Who,What,LS,Out1), !,
   fix_token_sents(Who,Waht2,[R|RR],Out2), !.


fix_token_sents(Who,What,[X],Y):- is_list(X),!,fix_token_sents(Who,What,X,Y).
%%fix_token_sents(Who,What,['[',Actor,and,Actor2|Rest], together([Actor,Actor2]):Out):- is_pers_name(Actor),
%   fix_token_sents(Who,What,['['|Rest],Out).

%fix_token_sents(_,What,['[',Actor|Rest],Out):- is_pers_name(Actor),fix_token_sents(Actor,What,['['|Rest],Out).


fix_token_sents(Who,_,['['|Rest], [s(Who,action,Rest)]).
fix_token_sents(unk,action,Out1,Out1):-!.
fix_token_sents(Who,What,Out1,s(Who,What,Out1)):-!.


de_s((O),O).
maybe_delist_into_action(Who,['[',Before,']'],Who:action([Before])):-!.
maybe_delist_into_action(unk,Before,Before):-!.
maybe_delist_into_action(Who,Before,Who:action(Before)).

%fix_token_sents(Who,Info,Actor,After):- atomic_list_concat([Actor,After],' ',Info),!.
send_scene(action(Info),After):- atomic(Info),tokenize_sents_ww(world,action,Info,Toks),fix_token_sents(unk,action,Toks,After),!.
send_scene(act_speak('Airdate',_),[]):-!.
send_scene(act_speak(W,S),SS):-  tokenize_sents_ww(W,speak,S,SS),!.
send_scene(narrate(W,S),SS):- tokenize_sents_ww(W,narrate,S,SS),!.
%send_scene(act_speak(W,S),_SceneName,W:speak:S)
%send_scene(Info,SceneName,unk:unk:SInfo):- append_term(Info,SceneName,SInfo).
send_scene(Info,Info).


write_scene_events([E]):- writeqf(E),!.
write_scene_events([s(_,_,[])|Events]):- write_scene_events(Events).
write_scene_events([E|Events]):- writeqf(E),!,format(',~n   '),write_scene_events(Events).
write_scene_events(E):- writeqf(E).

name_dvars2(A,'$VAR'(O)):- toPropercase(A,P),replace_in_string(' ','_',P,O).

%subst_name_vars_body(_,A,A):-!.
%subst_name_vars_body([],A,A):-!.
subst_name_vars_body(Vs,A,'$VAR'(A)):- member_eq0(A,Vs),!.
subst_name_vars_body(_,A,O):- \+ compound(A),!,A=O.
subst_name_vars_body(Vs,A,O):- is_list(A),!,maplist(subst_name_vars_body(Vs),A,O).
subst_name_vars_body(Vs,A,O):- 
  compound_name_arguments(A,N,Args),
  maplist(subst_name_vars_body(Vs),Args,ArgsO),
  compound_name_arguments(O,N,ArgsO),!.

writeqf(s(_,_,[])):-!.
writeqf(s(world,action,Act)):-!, writeqf(action(Act)).
writeqf(s(W,action,Act)):-!, writeqf(W:action(Act)).

writeqf(s(A,speak,F)):- !, writeqf(A:F).
writeqf(A:F):- \+ \+ (atomic(F); (sub_term(E,F), compound(E), E\='$VAR'(_), \+ is_list(E))),!, ansicall(yellow,faf(A,F)).
writeqf(A:F):- !, ansicall(cyan,faf(A,F)).
writeqf(E):- ansicall(yellow,writeq(E)).

faf(A,F):- format('~q:~t~15|~q',[A,F]).

%finish_scene:- \+ nb_current(current_scene,_SceneName), once(begin_scene), fail.
finish_scene:- 
  mor_rtrace((nb_current(current_scene,SceneName),  
  findall(Info,tmp:in_scene(actor(Info)),ActorsUL),sort(ActorsUL,ActorsL),length(ActorsL,L))),
  dmsg(finish_scene(ActorsL)),
  L < 20,
  %listing(tmp:in_scene/1),
  findall(Stuff,(retract(tmp:in_scene(Info)),Info\=actor(_),send_scene(Info,Stuff)),EventsL),
  mor_rtrace((flatten(EventsL,Events),
  Events\==[],  
  atomic_list_concat(ActorsL,'_',AL),
  atomic_list_concat([SceneName,with,L,characters,AL],'_',SceneNameA),
  maplist(name_dvars2,ActorsL,ActorsLV), 
  subst_name_vars_body(ActorsL,Events,VEvents),
  format("~Nscene_info( ~q, ~q, ~q, ~q, [~n~n   ",[SceneNameA,L,ActorsL,ActorsLV]),  
  write_scene_events(VEvents),
  format("~N ]).~n"),
  nop(format('~N:- ~q.~n~n~n', [end_scene(SceneName)])))),!,nl,nl.
finish_scene:- dmsg(finish_scene),!.

%find_monologs:- 

scene_info:- 
  
  scene_info(A, B,Actors1,ActorsLV,List),
  B > 1,    
  scene_info(AA,B,_,ActorsLV,List),
  AA @< A,
  wdmsg(A+AA),
  Actors1=ActorsLV,
  wdmsg(List).




maybe_trim_white(W,A):- always_trim_white(W,A),A\==W,!.
always_trim_white(W,A):- split_string(W, "", "\s\t\n", [S]),atom_string(A,S),!.

set_show(Show):- always_trim_white(Show,ShowT),nb_setval('so_show',ShowT),!.

show_date([M,DY],ShowDate):- atom(DY),atomic_list_concat([D,Y],'.',DY),show_date([M,D,Y],ShowDate).
show_date([M,D,Y],ShowDate):- atom(Y),atom_number(Y,YY),atom_number(M,MM),atom_number(D,DD),show_date([MM,DD,YY],ShowDate).
show_date([M,D,Y],ShowDate):- number(Y),Y>70,sformat(ShowDate,"19~|~`0t~d~2+_~|~`0t~d~2+_~|~`0t~d~2+",[Y,M,D]).
show_date([M,D,Y],ShowDate):- number(Y),Y<70,sformat(ShowDate,"20~|~`0t~d~2+_~|~`0t~d~2+_~|~`0t~d~2+",[Y,M,D]).
show_date([S,E],ShowDate):- atom(S),atom_number(S,SS),atom_number(E,EE),sformat(ShowDate,"S~|~`0t~d~2+E~|~`0t~d~2+",[SS,EE]).
show_date(List,ShowDate):- atomic_list_concat(List,'_',ShowDate).
more2([_,_|_]).
find_date(List):- is_list(List),!,atomic_list_concat(List,' ',Data),!,find_date(Data).
find_date(Data):- more2(List),atomic_list_concat(List,'season',Data),last(List,Date),!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'-trans-',Data),last(List,Date),!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'trans-da-',Data),last(List,Date),!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'.shtml',Data),List=[Date|_],!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'.html',Data),List=[Date|_],!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'.htm',Data),List=[Date|_],!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'/ep',Data),last(List,Date),!,find_date(Date).
find_date(Data):- more2(List),atomic_list_concat(List,'/',Data),last(List,Date),!,find_date(Date).
find_date(Date):- more2(List),atomic_list_concat(List,'-',Date),show_date(List,ShowDate),!,find_date(ShowDate).
find_date(Date):- add_show_date(Date),(nb_current('so_show',Show);Show='@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'),!,nop(debugln(Show=Date)),
 current_so_file(File),debugln(file=File).
add_scene_cline(A,E):- atom_string(E,S),asserta_if_new(tmp:in_scene(narrator(A))),assertz(tmp:in_scene(action(narrate(A,S)))).
add_scene_line(_,''):-!.
add_scene_line('Airdate',_):-!.
add_scene_line(A,E):- atom_string(E,S),asserta_if_new(tmp:in_scene(actor(A))),asserta_if_new(is_pers_name(actor(A))),
 assertz(tmp:in_scene(act_speak(A,S))).
add_scene_event(E):- atom_string(E,S),assertz(tmp:in_scene(action(S))).

add_show_date(Date):-nb_setval('so_date',Date).

current_so_show(SF):- nb_current('so_show',Show),nb_current('so_date',Date),
  replace_in_string([' '='_'],Show,ShowName),
  format(atom(SF),"~w_~w",[ShowName,Date]).
current_so_file(File):- current_so_show(SF),
  format(atom(File),"~w.nldata",[SF]).

trans_change:- nop(mor_rtrace((finish_scene))).
scene_change:-  mor_rtrace((finish_scene)),!,mor_rtrace((begin_scene)),!. 
add_converting(X,LINEPiper,E,List):- atom_concat('LINE ',Piper,LINEPiper),!,add_converting(X,'LINE',Piper,[E|List]).
add_converting(X,LINEPiper,E,List):- is_list(List),List\==[],!,atomic_list_concat(List,' ',A),!,add_converting(X,LINEPiper,E,A).
add_converting(X,LINEPiper,W,List):- maybe_trim_white(W,A),!,add_converting(X,LINEPiper,A,List).
add_converting(X,LINEPiper,E,W):- maybe_trim_white(W,A),!,add_converting(X,LINEPiper,E,A).
add_converting(_,'REG-FORMAT','',''):-!,trans_change.
add_converting(_,'DECODE-FORMAT','',''):-!,trans_change.
add_converting(_,'YEARINDEX',Show,_):- set_show(Show),!,trans_change.
add_converting(_,'PYEARINDEX',Show,_):- set_show(Show),!,trans_change.
add_converting(_,'TLINK',Show,Data):- finish_scene,set_show(Show),find_date(Data),!.
add_converting(_,'TRANSCRIPT',Show,Data):- set_show(Show),find_date(Data),!,begin_scene.
add_converting(_,'SCENE-EVENT','[SCENE_CHANGE]',''):- !,scene_change.
add_converting(_,'SCENE_CHANGE',_,_):- !,scene_change.
add_converting(_,'SCENECHANGE',_,_):- !,scene_change.
add_converting(_,_,'SCENECHANGE',_):- !,scene_change.
add_converting(_,_,'[SCENECHANGE]',_):- !,scene_change.
% add_converting(X,E,N,List):- debugln(add_converting_now(X,E,N,List)),fail.
add_converting(_,'SCENE-EVENT',Event,''):- mor_rtrace(add_scene_event(Event)),!.
add_converting(_,'LINE',Piper,Said):- mor_rtrace(add_scene_line(Piper,Said)),!.
add_converting(_,'CLINE',CPiper,Said):- mor_rtrace(add_scene_cline(CPiper,Said)),!.

add_converting(X,E,N,List):- debugln(add_converting(X,E,N,List)).

mor_rtrace(G):- once(G),!.
mor_rtrace(G):- rtrace(G).
/*
 flag(speech_act,A,A+1),
 get_flag(corpus_convos,Z),
 XX is ((Z+1)*100_000_000_000)+(A*10_000_000)+X, 
 add_converting_str(XX,Str).
*/
add_punct(X,X):- last(X,E),member(E,['?','.','!']).
add_punct(X,Y):- append(X,['.'],Y).

add_converting_str(_,"XXXXXXXXXXX"):- inc_flag(corpus_convos), 
  %C = 100_000_000_000, Buffer is floor(XX/C)*C + 01111111111,  
  %ignore(add_conversation_converting(Buffer)), !,
  set_flag(speech_act,1),!.
%add_converting_str(XX,Str):- 1 is XX mod 2, !, add_converting_said(said,"Al",XX,Str),!. 
%add_converting_str(XX,Str):- add_converting_said(said,"Jo",XX,Str),!. 


add_converting_str(XX,Str) :-
 must_det_l((
   string(Str),
   assert_converting_v(XX,string,Str),
   tokenize_atom(Str,Toks),!,
   pretok(Toks,PreToks0),
   add_punct(PreToks0,PreToks),
   text_to_tree(PreToks,Tree),
   assert_converting(XX,text_to_tree,Tree),
   %writeq(sample_tree(Tree)),writeln('.'),
   unphrasify(Tree,List),
   assert_converting(XX,unphrasify,List),
   tree_to_toks(List,PostToks),!,
   assert_converting(XX,tree_to_toks,PostToks),
   add_converting_toks(XX,PostToks))).

 
/* Old Way
add_converting_str(XX,Str):- 
 tokenize_atom(Str,Toks),
 maplist(downcase_atom,Toks,TokList), 
 pretok(TokList,PreToks),!,
 add_converting_toks(XX,PreToks).
 
*/

tree_to_toks:- mmake, forall(sample_tree(Tree),tree_to_toks1(Tree)).
sample_tree(['SEQBAR',['CORENLP',['S',['CC','And'],['ADVP',['RB',then]],['NP',['NP',['PRP$',her],['NN',son]],[',',','],['NP',['NNP','Ben']],[',',',']],['VP',['VP',['VBZ',turns],['NP',['DT',all],['NNP','Sith']]],['CC',and],['VP',['VBZ',joins],['NP',['DT',the],['JJ',dark],['NN',side]]]],['.','.']]],['CORENLP',['S',['PRN',['S',['NP',['DT','That']],['VP',['VBD',had],['S',['VP',['TO',to],['VP',['VB',have],['VP',['VBN',factored],['PP',['IN',into],['NP',['PRP$',her],['NNS',reasons]]],['S',['VP',['TO',to],['VP',['VB',stay],['ADVP',['RB',away]],['PP',['IN',from],['NP',['NP',['DT',the],['NN',call]],['PP',['IN',of],['NP',['DT',the],['NN',force]]]]]]]]]]]]]]],[',',','],['VB',do],['RB',not],['NP',['PRP',you]],['VP',['VB',think]],['.',?]]]]).
sample_tree(['CORENLP',['S',['NP',['PRP','I']],['VP',['VB',hate],['S',['VP',['TO',to],['VP',['VB',say],['S',['NP',['PRP',it]],['VP',['VB','buuut.']],[',',',']]]]]],['.','.']],['S',['VP',[',',',']],['.','.']]]).
sample_tree(['SEQBAR',['CORENLP',['SBAR',['NP',['WP',who]],['S',['VP',['MD',would],['VP',['VB',pick],['NP',['NN',kylo]]]]],['.',?]]],['CORENLP',['S',['ADVP',['RB',definitely]],['ADVP',['RB',not]],['NP',['PRP',me]]]]]).
sample_tree(['SEQBAR',['CORENLP',['S',['S',['NP',['PRP','He']],['VP',['VBD',was],['NP',['NP',['NNP','Luke'],['POS','\'s']],['NNP','Padwan']]]],[',',','],['CC',but],['S',['NP',['PRP',he]],['VP',['VBD',turned]]],['.','.']]],['SEQBAR',['S',['NP',['PRP','It']],['VP',['AUX',has],['RB',not],['VP',['AUX',been],['VP',['VBN',shown],['FRAG',['WHADVP',['WRB',why]]]]]],['.','.']],['CORENLP',['S',['PRN',['S',['NP',['PRP','He']],['VP',['VBZ',is],['ADVP',['RB',no],['RBR',longer]],['NP',['NNP','Jedi']]]]],[',',','],['NP',['PRP',he]],['VP',['VBZ',is],['ADJP',['JJ',sith]],['ADVP',['RB',now]]]]]]]).
sample_tree(['CORENLP',['SBAR',['INTJ',['UH','Well']],[',',','],['SBAR',['IN',if],['S',['NP',['PRP',it]],['VP',['VBZ',is],['NP',['NNP','Rey']]]]],[',',','],['ADVP',['RB',then]],['WHADVP',['WRB',why]],['S',['VBD',did],['NP',['PRP',it]],['RB',not],['VP',['VB',wake],['SBAR',['WHADVP',['WRB',when]],['S',['NP',['NNP','Klyo']],['VP',['VBD',came],['PP',['IN',into],['NP',['NN',power]]]]]]]]]]).
sample_tree([ 'CORENLP',
     [ 'SBAR',
       [ 'NP',
         ['WP','Who']],
       [ 'S',
         ['VBZ',is],
         [ 'NP',
           ['PRP$',your],
           ['JJ',favorite],
           ['NN',character]]],
       ['.',?]]]).
sample_tree(['SEQBAR',['CORENLP',['S',['INTJ',['UH','Well']],[',',','],['NP',['PRP',it]],['VP',['VBZ','\'s'],['NP',['DT',a],['NN',movie]]],['.','.']]],['CORENLP',['S',['NP',['PRP','He']],['VP',['MD',could],['VP',['VB',show],['PRT',['RP',up]]]]]]]).
sample_tree(['CORENLP',['S',['VB','Are'],['NP',['PRP',you]],['NP',['NP',['DT',a],['NN',fan]],['PP',['IN',of],['NP',['DT',the],['NML',['NNP','Star'],['NNPS','Wars']],['NN',series]]]],['.',?]]]).
sample_tree(['CORENLP',['S',['NP',['PRP','I']],['VP',['VB',think],['SBAR',['S',['NP',['PRP',he]],['VP',['VBD',was],['ADVP',['RB',just]],['VP',['VBG',giving],['NP',['DT',a],['JJ',giant],['JJ',middle],['NN',finger]],['PP',['IN',to],['NP',['DT',the],['NN',audience]]]]]]]]]]).
sample_tree(['CORENLP',['S',['ADVP',['RB','Obviously']],['NP',['NNP','Darth'],['NNP','Vader']],['VP',['VBZ',is],['NP',['NP',['DT',the],['JJS',best]],['CC',and],['NP',['NP',['DT',the],['JJ',original],['JJ',bad],['NN',guy]],['PP',['IN',of],['NP',['NNP','Star'],['NNPS','Wars']]]]]]]]).
sample_tree(['SEQBAR',['CORENLP',['S',['NP',['NNP','James'],['NNP','Earl'],['NNP','Jones']],['VP',['VBZ',does],['NP',['DT',the],['NN',voice]],[',',','],['SBAR',['RB',even],['IN',though],['S',['NP',['PRP',he]],['VP',['VBZ',is],['RB',not],['VP',['VBN',listed],['PP',['IN',in],['NP',['DT',the],['NNS',credits]]]]]]]],['.','.']]],['CORENLP',['S',['NP',['NNP','David'],['NNP','Prowse']],['VP',['VBD',did],['NP',['DT',the],['NN',acting]]]]]]).
sample_tree(['CORENLP',['S',['S',['NP',['PRP','I']],['VP',['VB','\'m'],['ADVP',['RB',still]],['ADJP',['RB',really],['JJ',bummed],['PP',['IN',about],['NP',['DT',that]]]]]],[',',','],['CC',but],['S',['NP',['PRP','I']],['VP',['VB','\'m'],['ADJP',['JJ',sure],['SBAR',['S',['NP',['PRP',they]],['VP',['MD','\'ll'],['VP',['VB',figure],['NP',['NN',something]],['PRT',['RP',out]],['PP',['IN',for],['NP',['NP',['NNP','Leia']],['PP',['IN',in],['NP',['DT','The'],['JJ','Last'],['NNP','Jedi']]]]]]]]]]]]]]).
tree_to_toks1(Tree):-
 print_tree_nl(i=Tree),
 unphrasify(Tree,UTree),
 print_tree_nl(o:-UTree),
 nop((visible_rtrace([+call,+exit],tree_to_toks(Tree,O)),
 notrace(wdmsg(O)))).


contains_phrase(Ls):- sub_term(E,Ls),atom(E),(is_penn_long(E);E=='NP').
contains_phrase(Ls):- member(E,Ls),is_list(E),member(Sl,E),is_list(Sl).

unphrasify([], []) :- !.
%unphrasify([S|Ls], FlatL) :- is_penn_long(S), unphrasify(Ls, FlatL).
unphrasify(['VP'|Ls], FlatL) :- !, unphrasify(Ls, FlatL).
unphrasify(['PP'|Ls], FlatL) :- !, unphrasify(Ls, FlatL).
unphrasify([S|Ls], [mark(S)|FlatL]) :- (is_penn_long(S), contains_phrase(Ls)  ),!, unphrasify(Ls, FlatL).
unphrasify([S|Ls], FlatL) :- S=='NP', sub_var('NP', Ls), unphrasify(Ls, FlatL).
unphrasify([L|Ls], [L|NewLs]) :- 
    dont_flatten(L),!,
    unphrasify(Ls, NewLs),!.
unphrasify([L|Ls], FlatL) :-
    unphrasify(L, NewL),
    unphrasify(Ls, NewLs),
    append(NewL, NewLs, FlatL).
unphrasify(L, [L]).

not_is_list(X):- \+ is_list(X).

dont_flatten([_|L]):- sub_var('NP',L),!, fail.
dont_flatten([S|_]):- is_penn_long(S),!, fail.
dont_flatten([S|_]):- is_penn_tag(S).

tree_to_toks(X,Y):- notrace(unphrasify(X,XX)), tree_to_toks(s,XX,YY),cleanup_toks(YY,Y).
tree_to_toks(C,X,Y):- tree_to_tokz(C,X,M),!,notrace(flatten([M],Y)).

cleanup_toks([],[]).
cleanup_toks([mark(_)|YY],Y):-!,cleanup_toks(YY,Y).
cleanup_toks([np,X,np|YY],[X|Y]):-!,cleanup_toks(YY,Y).
cleanup_toks([np|Rest],[X|Y]):- append(Toks,[np|More],Rest),atomic_list_concat(Toks,'-',X),!,cleanup_toks(More,Y).
cleanup_toks([X|YY],[X|Y]):-!,cleanup_toks(YY,Y).

too_long('CORENLP').
too_long('VP').
too_long('PP').
too_long('NML').
too_long('FRAG').
too_long(X):- atom_concat(_,'BAR',X).
too_long(X):- atom_concat('S',_,X).
is_penn_tag(S):- atom(S),upcase_atom(S,S), S\=='I'.
is_penn_long(S):-is_penn_tag(S),too_long(S).

tree_to_tokz(_,Item,Item):- atomic(Item),!.
tree_to_tokz(C,['NP'|Items],X):- !, tree_l_to_toks(C,Items,List), notrace(undbltok(List,Un)), wrap_seg(np,Un,X).
%tree_to_tokz(C,[_,Item],X):- !, tree_to_tokz(C,Item,X).
tree_to_tokz(C,[S|Items],List):- notrace(is_penn_long(S)), Items\==[], !, tree_to_tokz(C,Items,List).
tree_to_tokz(C,[S|Items],X):- notrace(is_penn_tag(S)), Items\==[], !, tree_l_to_toks(C,Items,List), =(S,D), wrap_seg(D,List,X).
tree_to_tokz(C,Items,Toks):-  is_list(Items),!,tree_l_to_toks(C,Items,List),!,flatten(List,Toks),!.
tree_to_tokz(_C,X,X):- !.

clean_innerd([],[]).
clean_innerd([D,E,D|Inner],[E|ReIn]):-!,clean_innerd(Inner,ReIn).
clean_innerd([S|Inner],[S|ReIn]):- clean_innerd(Inner,ReIn).
wrap_seg(O,List,X):- O\=='np',List=X.
wrap_seg(O,List,X):- append([D|Inner],[D],List),clean_innerd(Inner,ReIn),wrap_seg(O,ReIn,X).
wrap_seg(D,List,X):- append([D|List],[D],X),!.
%wrap_seg(D,List,X):- dbltok(D,List,X).

tree_l_to_toks(C,Items,O):- maplist(tree_to_toks(C),Items,List),flatten(List,O).

assert_converting(XX,P,Parse):- assert_if_new(converting(XX,P,Parse)),nop(save_converting(converting/3)).
assert_converting_v(XX,P,Parse):- assert_converting(XX,P,Parse),dmsg(converting(XX,P,Parse)).

do_converting(XX,_Str,F2):- converting(XX,F2,_),!.
do_converting(XX,Str,F2):-
  catch(call(F2,Str,Result),E,(dumpST,format('% % % ERROR: ~p~n',[E --> call(F2,Str,Result)]),fail)),!,
  assert_converting(XX,F2,Result),!.

text_to_tree([],[]).
text_to_tree(TokList, Tree):- \+ string(TokList),!, atomics_to_string(TokList,' ',Text),!,text_to_tree(TokList,Text,Tree).
text_to_tree(Text,    Tree):- tokenize_atom(Text,TokList), text_to_tree(TokList,Text,Tree).

text_to_tree(TokList,Text,Tree):- member('"',TokList), !, text_to_best_tree(Text,Tree).
text_to_tree(TokList, _,['SEQBAR',X,Y]):- append(Left,[LE|Right],TokList), Right\==[],
  member(LE,['.','?','!']),append(Left,[LE],Said),!, text_to_tree(Said,X), text_to_tree(Right,Y).
text_to_tree(_TokList,Text,Tree):- text_to_best_tree(Text,Tree),!.
text_to_tree(_TokList,Text,Tree):- text_to_lgp_tree(Text,Tree),!.


all_letters(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

retokify([],[]).
retokify([E|APreToks],[sp|PreToks]):- \+ atomic(E), retokify(APreToks,PreToks).
retokify([E|APreToks],[F|PreToks]):- downcase_atom(E,F),retokify(APreToks,PreToks).

add_converting_toks(_,[]):- !.
add_converting_toks(X,[A]):- !, add_converting_toks(X,[A,'.']).
add_converting_toks(XX,APreToks):-
 retokify(APreToks,PreToks),
 maplist(add_occurs(is_word),PreToks),
 inc_flag(corpus_converting),
 ignore(add_ngrams(except_symbols,trigram,3,skip,PreToks)),
 predbltok(PreToks,ReToks0),
 dbltok(oc,ReToks0,ReToks),!,
 XX1 is XX+1,
 append([oc(XX)|ReToks],[oc(XX1)],Grams),!,
 assert_converting_v(XX,grams,Grams),
 add_ngrams(except_none,ngram,4,XX,Grams).

add_ngrams(Except,F,N,Loc,Grams):- length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),add_1ngram(Except,F,Loc,NGram)).

except_none(_).
add_1ngram(Except,F,Loc,List):- 
 (Except == except_none ; maplist(Except,List)),!,
 (Loc==skip->W=..[F|List];W=..[F,Loc|List]),
 ngram_inc(W,X),
 (Loc==skip-> (( \+ W -> assert(W) ; true)) ; assert(W)),
 (X=0->(inc_flag(corpus_nodes));inc_flag(corpus_node_overlap)),!.

add_occurs(F,Tok):- P=..[F,Tok],
  ignore(( \+ P, assert(P), inc_flag(corpus_unique_toks) )),
  ngram_inc(P),inc_flag(corpus_total_toks).

except_symbols(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

pretok([],[]).
%pretok(['.'],[]):-!.
pretok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],',',Y),pretok([Y|Nxt],O).
pretok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat([A,S],'-',F),!, pretok(Grams,ReTok).
pretok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'`',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
%pretok([','|Grams],ReTok):- pretok(Grams,ReTok).
%pretok(['-'|Grams],ReTok):- pretok(Grams,ReTok).
%pretok([A,B,C|Grams],ReTok):- trigram(A,B,C,N), N>40, !,ngram_key(trigram(A,B,C),Key),pretok([Key|Grams],ReTok).
pretok(['!'|Grams],ReTok):- pretok(['.'|Grams],ReTok).
pretok([S|Grams],[S|ReTok]):- pretok(Grams,ReTok).

predbltok([],[]).
predbltok(['.'],[]):-!.
predbltok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],',',Y),predbltok([Y|Nxt],O).
predbltok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat([A,S],'-',F),!, predbltok(Grams,ReTok).
predbltok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([A,'`',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([','|Grams],ReTok):- predbltok(Grams,ReTok).
predbltok(['!'|Grams],ReTok):- predbltok(['.'|Grams],ReTok).
predbltok([S|Grams],[S|ReTok]):- predbltok(Grams,ReTok).

% dbltok(_,X,X):-!.
%dbltok(oc,[],[]):-!.
dbltok(_,[S],[S]):- is_full_tok(S),!.
%dbltok(Pre,[S],[PS]):- atoms_join(Pre,S,PS).
dbltok(Pre,[],[PS]):-!, atoms_join(Pre,oc,PS).
dbltok(Pre,[S|I],[S|O]):- is_full_tok(S),!,dbltok(Pre,I,O).
dbltok(Pre,[S|Grams],[PS|ReTok]):- atoms_join(Pre,S,PS), dbltok(S,Grams,ReTok).



undbltok(I,O):- is_list(I),!,maplist(undbltok,I,O).
undbltok(S,PS):- into_mw(S,[PS|_]),!.
undbltok(S,S):- !.

is_full_tok(O):- atom(O),atomic_list_concat([_,_|_],':',O).

atoms_join(A,B,O):- tok_split(O,A,B),!,ngram_inc(tok_split(O,A,B)).
atoms_join(A,B,O):- atomic_list_concat([A,B],':',O),!,assert(tok_split(O,A,B)),ngram_inc(tok_split(O,A,B)).

% @TODO use average 
%as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).
%ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).

add_blanks(N,S,Slotted):- \+ is_list(S),!,add_blanks(N,[S],Slotted).
add_blanks(_,[],[]):-!.

add_blanks(N,[A,B|Sent],[O|Slotted]):- tok_split(O,A,B),!,add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],[O|Slotted]):- \+ \+ tok_split(_,S,_),!, tok_split(O,S,_),add_blanks(N,Sent,Slotted).
add_blanks(N,[O|Sent],[O|Slotted]):- atom(O), tok_split(O,_,_),!,add_blanks(N,Sent,Slotted).

add_blanks(N,[len(S)|Sent],Slotted):- integer(S),length(L,S),!,add_blanks(N,Sent,Mid),append(L,Mid,Slotted).
add_blanks(N,[S|Sent],[A|Slotted]):- string(S),atom_string(A,S),!,add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],Slotted):- var(S),!,between(1,N,L),add_blanks(N,[1-L|Sent],Slotted).
add_blanks(N,[Lo-Hi|Sent],Slotted):- (integer(Lo);integer(Hi)),!,between(Lo,Hi,L),length(S,L),add_blanks(N,Sent,Mid),append(S,Mid,Slotted).
add_blanks(N,[S|Sent],Slotted):- is_list(S),!,flatten(S,SL),append(SL,Sent,SLSent),!,add_blanks(N,SLSent,Slotted).
add_blanks(N,[S|Sent],Slotted):- atom(S),into_mw(S,SL),!,append(SL,Sent,SLSent),!,add_blanks(N,SLSent,Slotted).
add_blanks(N,[S|Sent],[S|Slotted]):- add_blanks(N,Sent,Slotted).

into_mw(S,SL):- into_mw0(S,SL),SL\==[S],!.
into_mw0(S,SL):- atomic_list_concat([M,_|_],':',S),!,into_mw0(M,SL).
into_mw0(S,SL):- atomic_list_concat(SL,',',S).
into_mw0(S,SL):- atomic_list_concat(SL,' ',S).
into_mw0(S,SL):- atomic_list_concat(SL,'_',S).

loc_dists(Loc1,Loc2, NN):- NN is abs(Loc1-Loc2).
loc_dists(Loc1,Loc2,Loc3, NN):- NN is (abs(Loc1-Loc2) + abs(Loc3-Loc2) + abs(Loc1-Loc3))/3.

%:- so_convert:ensure_loaded(plm).
% added for conversations
ngram(Loc,A,oc(X),B,C,NN):- nonvar(X), ngram(Loc,_,_,A,oc(X),_),ngram(_ULoc,oc(X),B,C,_,NN).
ngram(Loc,A,B,oc(X),C,NN):- nonvar(X), ngram(Loc,_,A,B,oc(X),_),ngram(_ULoc,oc(X),C,_,_,NN).

autoc(Sent):- autoc(1,Sent).
autoc(N,Sent):- 
  retractall(used_cl(ngram(_,_,_,_))),
  add_blanks(N,Sent,Slotted),no_repeats( map_sent(_,_Loc,Slotted)),fmt_so_convert(Slotted).

good_toks(Key,E):- functor(P,ngram,6),arg(6,P,E),no_repeats(Key,(P,ngram_key(P,Key))).


:- add_history(reconvert_corpus).

:- fixup_exports.

:-dynamic(used_cl/1).

map_sent(_,_,Sent):- ground(Sent),!.
map_sent(LR,Loc,Sent):- var(Sent), length(Sent,9),map_sent(LR,Loc,Sent).
map_sent(LR,Loc,List):- LR=lr,append(Left,[X|More],List),nonvar(X),Left\==[],!,map_sent(LR,Loc,[X|More]),map_sent(rl,Loc,List).
map_sent(LR,Loc,[A,B,C,D|More]):- some_ngram(Loc,A,B,C,D,_Fire), map_sent(LR,Loc,[C,D|More]).
map_sent(LR,Loc,[A,B,C,D|More]):- some_ngram(Loc,A,B,C,_,_Fire), map_sent(LR,Loc,[B,C,D|More]).
map_sent(_,Loc,List):- ABCDO=[_,_,_,_,_Occurs],append(List,_,ABCDO), apply(some_ngram,[Loc|ABCDO]).


some_ngram(_PrevLoc,A,B,C,D,N):- pick_ngram(Loc,A,B,C,D,N),may_use(Loc,A,B,C,D,N).

pick_ngram(Loc,A,B,C,D,N):- maplist(var,[A,B,C,D])->rnd_ngram(Loc,A,B,C,D,N);ngram(Loc,A,B,C,D,N).

rnd_ngram(Loc,A,B,C,D,N):-  G = ngram(Loc,A,B,C,D,N),
 predicate_property(G,number_of_clauses(R)),
  CN is random(R)+1, nth_clause(G,CN,Ref),clause(G,Body,Ref),Body.


cf:- consult(corpus_file).
qc:- qcompile(corpus_file).

%:- ignore(( \+ prolog_load_context(reloading, true),time(consult(corpus_file)))).

:- style_check(- singleton).


%:- add_history((good_toks(Key,E),E>20)).
%:- add_history((autoc([ 'like:you',len(200)]))).
%:- add_history((autoc([oc,'like:you',len(200)]))).
%:- add_history((autoc([ 'oc:like', 'like:you',len(200)]))).
%:- add_history((autoc([ 'like',len(200)]))).
%:- add_history((autoc([len(10),like,len(200)]))).
:- add_history(load_converting).
:- add_history(convert_corpus).
:- add_history(tree_to_toks).
:- add_history(convert_corpus).

may_use(Loc,_,B,C,D,_):- \+ used_cl(ngram(A,B,C,D)), assert(used_cl(ngram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.


gen6([A,B,C,D,E,F,G,H]=N):-
  ngram(Loc1,E,F,G,H,Z), ngram(Loc2,C,D,E,F,Y), ngram(Loc3,A,B,C,D,X), N is X+Y+Z.
:- fixup_exports.

dotit:- 
  ignore(( 
    \+ prolog_load_context(reloading, true), 
     ignore(load_converting), 
     ignore(convert_corpus))).

