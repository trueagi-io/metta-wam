
:- ensure_loaded(metta_testing).
:- ensure_loaded(swi_support).
:- ensure_loaded(swi_flybase).
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_python).

load_metta(Filename):-
 atom(Filename),exists_file(Filename),!,
 clear_spaces,
 load_metta('&self',Filename).
load_metta(Filename):- with_wild_path(load_metta,Filename),!,loonit_report.

load_metta(Self,Filename):-
 atom(Filename),exists_file(Filename),!,
 track_load_into_file(Filename,
   setup_call_cleanup(open(Filename,read,In),
    ((directory_file_path(Directory, _BaseName, Filename),
      with_cwd(Directory,once(load_metta_stream(Self,In))))), close(In))).
load_metta(Self,Filename):- with_wild_path(load_metta(Self),Filename),!,loonit_report.



clear_spaces:- clear_space(_).
clear_space(S):-
   retractall(metta_defn(S,_,_)),
   retractall(metta_type(S,_,_)),
   retractall(metta_atom(S,_)).

load_metta_stream(Fn,String):- string(String),!,open_string(String,Stream),load_metta_stream(Fn,Stream).
load_metta_stream(_Fn,In):- (at_end_of_stream(In);reached_file_max),!.
load_metta_stream(Self,In):- repeat,
  once(read_metta(In,Read)),
  once((Read==end_of_file->true;do_metta(Self,load,Read))),
  (at_end_of_stream(In);reached_file_max),!.


:- nb_setval(self_space, '&self').
evals_to(XX,Y):- Y==XX,!.   evals_to(XX,Y):- Y=='True',!, XX\=='False'.

eval_arg(A,AA):-
  nb_current(self_space,Space),
  eval_args0(5,Space,A,AA).

:- discontiguous eval_args0/4.
:- discontiguous eval_args1/4.
:- discontiguous eval_args2/4.

eval_args0(Depth,Self,X,Y):- nonvar(Y),!,eval_args0(Depth,Self,X,XX),evals_to(XX,Y).
eval_args0(Depth,Self,X,Y):-
  no_repeats_var(Y),
  D1 is Depth-1,D2 is D1-1,
  eval_args1(D1,Self,X,M),
  eval_args1(D2,Self,M,Y).
%eval_args0(Depth,Self,X,Y):- eval_args1(Depth,Self,X,Y)*->true;Y=[].

eval_args1(Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval_args0(Depth,Self),VI,VO).
eval_args1(Depth,Self,['assertEqual',X,Y],TF):- !, ((loonit_asserts((fa_eval_args0(Depth,Self,X,XX),fa_eval_args0(Depth,Self,Y,YY)),XX=@=YY))),as_tf(XX=@=YY,TF).
eval_args1(Depth,Self,['assertEqualToResult',X,Y],TF):- !, (( loonit_asserts((fa_eval_args0(Depth,Self,X,L)),L=@=Y))),as_tf(L=@=Y,TF).
eval_args1(_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
eval_args1(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args1(Depth,_Slf,X,Y):- Depth<1,!,fail,Y=X.
eval_args1(Depth,Self,[V|VI],VVO):-  \+ is_list(VI),eval_args0(Depth,Self,VI,VM),
  ( VM\==VI -> eval_args0(Depth,Self,[V|VM],VVO) ;
    (eval_args0(Depth,Self,V,VV), (V\==VV -> eval_args0(Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).
eval_args1(Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval_args0(Depth,Self,X,Y).
eval_args1(Depth,Self,X,Y):- eval_args2(Depth,Self,X,M),(M\==X->eval_args0(Depth,Self,M,Y);Y=X).

cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
fa_eval_args0(Depth,Self,X,L):- findall(E,eval_args0(Depth,Self,X,E),L).

is_case(AA,[AA,Value],Value):-!.
is_case(AA,[AA|Value],Value).

into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).



eval_args2(Depth,_Slf,Name,Value):- atom(Name),!, nb_current(Name,Value).
eval_args2(Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Depth,Space,Goal).
% Macro Functions
eval_args2(Depth,Self,['case',A,[Case1|CaseN]|NonCases],Res):- !,
          eval_arg(A,AA),
           ((member(Case,[Case1|CaseN]),
              is_case(AA,Case,Value))->
             (eval_args0(Depth,Self,Value,R1),
              eval_args0(Depth,Self,NonCases,R2),
              combine_result(R1,R2,Res));
             (fail,eval_args0(Depth,Self,NonCases,Res))).
/*
eval_args2(Depth,Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval_args0(Depth,Self,A,AA),AA\==A,
   eval_args0(Depth,Self,[F,AA|Args],Res).


eval_args2(Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval_args0(Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval_args0(Depth,Self,[F,A1|NewArgs],Res)))).
*/

eval_args2(Depth,Self,['import!',Other,File],Space):- into_space(Self,Other,Space),!, load_metta(Space,File).
eval_args2(Depth,Self,['bind!',Other,Expr],Value):- into_name(Self,Other,Name),!,eval_args0(Depth,Self,Expr,Value),nb_setval(Name,Value).


is_and(S):- \+ atom(S),!,fail.
is_and('#COMMA'). is_and(','). is_and('and').
eval_args2(Depth,_Slf,[And],'True'):- is_and(And),!.
eval_args2(Depth,Self,[And,X|Y],TF):- is_and(And),!,eval_args0(Depth,Self,X,TF1),is_true(TF1), eval_args2(Depth,Self,[And|Y],TF).

eval_args2(Depth,Self,['if',TF,Then,Else],Res):- !, ( \+ eval_args0(Depth,Self,TF,'False') -> eval_args0(Depth,Self,Then,Res);eval_args0(Depth,Self,Else,Res) ).
eval_args2(Depth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.


eval_args2(Depth,Self,['let',A,A5,AA],AAO):- !,eval_args0(Depth,Self,A5,A),eval_args0(Depth,Self,AA,AAO).
eval_args2(Depth,Self,['let*',[Let0|LetRest],Body],RetVal):-
    findall('let'(Var,Val), member([Var,Val],[Let0|LetRest]),LetStars),
    eval_args0(Depth,Self,[progn,[progn|LetStars],Body],RetVal).
eval_args2(Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args0(Depth,Self),List,Res),flatten(Res,Flat).

eval_args2(Depth,Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
eval_args2(Depth,Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
eval_args2(Depth,Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2.
eval_args2(Depth,Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).
eval_args2(Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Depth,Space,PredDecl).

eval_args2(Depth,Self,['get-type',Fn],Type):-!,metta_type(Self,Fn,List),last_element(List,Type).
last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.



%[superpose,[1,2,3]]
eval_args2(Depth,Self,['superpose',List],Res):- !, member(E,List),eval_args0(Depth,Self,E,Res).
eval_args2(Depth,Self, [F|Term], Res):- fail,
   member(ATerm,Term), get_sa_p1(setarg,ST,ATerm,P1),
   %compound(ST), %is_list(ST),
   ST = [SF,List],
  SF=='superpose',% List\==[],
    is_list(List), %maplist(atomic,List),
   call(P1,Var),!,
   member(Var,List),
   eval_args2(Depth,Self, [F|Term], Res).
get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).
eval_args2(Depth,Self, Term, Res):-
   get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), is_list(ST),ST = [F,List],F=='superpose', %maplist(atomic,List),
   call(P1,Var),
   %max_counting(F,20),
   !, member(Var,List),
   eval_args2(Depth,Self, Term, Res).

max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).


%eval_args2(Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args2(Depth,Self,['or',X,Y],TF):- !, (eval_args0(Depth,Self,X,TF);eval_args0(Depth,Self,Y,TF)).

eval_args2(Depth,_Slf,LESS,Res):- once(eval_selfless(LESS,Res)),LESS\==Res,!.

as_tf(G,'True'):- call(G),!. as_tf(_,'False').
eval_selfless(['==',X,Y],TF):-!,as_tf(X=@=Y,TF).
eval_selfless(['=',X,Y],TF):-!,as_tf(X=Y,TF).
eval_selfless(['>',X,Y],TF):-!,as_tf(X@>Y,TF).
eval_selfless(['%',X,Y],TF):-!,as_tf(X@>Y,TF).
eval_selfless(['<',X,Y],TF):-!,as_tf(X@<Y,TF).
eval_selfless(['=>',X,Y],TF):-!,as_tf(X@>=Y,TF).
eval_selfless(['<=',X,Y],TF):-!,as_tf(X@=<Y,TF).
eval_selfless(LIS,Y):-  notrace((
   LIS=[F,_,_], atom(F), current_op(_,yfx,F),
   catch((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.

% less Macro-ey Functions

metta_atom_iter(Depth,_Slf,[]):-!.
metta_atom_iter(Depth,Other,[Equal,H,B]):- '=' == Equal,!, metta_defn(Other,H,B).
metta_atom_iter(Depth,_Slf,[And]):- is_and(And),!.
metta_atom_iter(Depth,Self,[And,X|Y]):- is_and(And),!,metta_atom_iter(Depth,Self,X),metta_atom_iter(Depth,Self,[And|Y]).
metta_atom_iter(Depth,Other,H):- metta_atom(Other,H).
metta_atom_iter(Depth,Other,H):- Depth>0, D2 is Depth -1, metta_defn(Other,H,B),metta_atom_iter(D2,Other,B).

/*
; Bind &kb to a new empty Space
!(bind! &kb (new-space))

; Some knowledge
(= (frog $x)
   (and (croaks $x)
        (eat_flies $x)))
(= (croaks Fritz) True)
(= (eat_flies Fritz) True)
(= (croaks Sam) True)
(= (eat_flies Sam) True)
(= (green $x)
   (frog $x))

; Define conditional
(: ift (-> Bool Atom Atom))
(= (ift True $then) $then)

; For anything that is green, assert it is Green in &kb
!(ift (green $x)
      (add-atom &kb (Green $x)))

; Retrieve the inferred Green things: Fritz and Sam.
!(assertEqualToResult
  (match &kb (Green $x) $x)
  (Fritz Sam))
*/
:- discontiguous eval_args3/4.
eval_args2(Depth,Self,PredDecl,Res):- eval_args3(Depth,Self,PredDecl,Res).

eval_args3(Depth,Self,X,Y):- metta_atom_iter(Depth,Self,[=,X,Y]).
eval_args3(Depth,Self,PredDecl,Res):- term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval_args0(Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))).
eval_args3(Depth,Self,['ift',CR,Then],RO):- trace,
   metta_defn(Self,['ift',R,Then],Become),eval_args0(Depth,Self,CR,R),eval_args0(Depth,Self,Then,_True),eval_args0(Depth,Self,Become,RO).


eval_args2(Depth,Self,PredDecl,Res):- eval_args4(Depth,Self,PredDecl,Res).
eval_args4(Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args0(Depth,Self,[F2|X2],Y2),eval_args0(Depth,Self,X1,Y1).
eval_args4(Depth,_Slf,L1,Res):- is_list(L1),maplist(self_eval,L1),!,Res=L1.
eval_args4(Depth,Self,[F|X],[F|Y]):- is_function(F),is_list(X),maplist(eval_args0(Depth,Self),X,Y),X\=@=Y.



%metta_atom_iter(Depth,Other,H):- metta_atom(Other,H).
%metta_atom_iter(Depth,Other,H):- eval_args0(Depth,Other,H,_).

metta_atom_iter_ref(Other,['=',H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).

is_function(F):- atom(F).
is_false(X):- eval_arg(X,Y), (Y=0;Y='False'),!.
is_conz(Self):- compound(Self), Self=[_|_].

%dont_x(eval_args0(Depth,Self,metta_if(A<B,L1,L2),R)).
dont_x(eval_args0(Depth,_Self,_<_,_)).

into_fp(D,D):- \+ \+ dont_x(D),!.
into_fp(ListX,CallAB):-
  sub_term(STerm,ListX),needs_expanded(STerm,Term),
  %copy_term(Term,CTerm),
  =(Term,CTerm),
  substM(ListX,CTerm,Var,CallB), fn_append1(Term,Var,CallA),
  into_fp((CallA,CallB),CallAB).
into_fp(A,A).

needs_expand(Expand):- compound(Expand),functor(Expand,F,N),N>=1,atom_concat(metta_,_,F).
needs_expanded(eval_arg(Term,_),Expand):- !,sub_term(Expand,Term),compound(Expand),Expand\=@=Term,
   compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).
needs_expanded([A|B],Expand):- sub_term(Expand,[A|B]), compound(Expand), \+ is_conz(Expand), \+ is_ftVar(Expand), needs_expand(Expand).

fn_append1(eval_arg(Term,X),X,eval_arg(Term,X)):-!.
fn_append1(Term,X,eval_arg(Term,X)).


run_file_arg:- current_prolog_flag(argv,P),append(_,['--args'|Rest],P),Rest\==[],!,maplist(load_metta('&self'),Rest).

loon:- loonit_reset, cls, make, run_file_arg, !, loonit_report.
loon:- time(loon_metta('./examples/compat/test_scripts/*.metta')),fail.
loon:- repl.


% Check if parentheses are balanced in a list of characters
balanced_parentheses(Chars) :- balanced_parentheses(Chars, 0).
balanced_parentheses([], 0).
balanced_parentheses(['('|T], N) :- N1 is N + 1, balanced_parentheses(T, N1).
balanced_parentheses([')'|T], N) :- N > 0, N1 is N - 1, balanced_parentheses(T, N1).
balanced_parentheses([H|T], N) :- H \= '(', H \= ')', balanced_parentheses(T, N).
% Recursive function to read lines until parentheses are balanced.
repl_read(NewAccumulated, Read):-
    atom_concat(Atom, '.', NewAccumulated),
    catch((read_term_from_atom(Atom, Term, []), Read=call(Term)), E,
       (write('Syntax error: '), write(E), nl, repl_read(Read))),!.
repl_read(NewAccumulated, Read):-
    normalize_space(string(Renew),NewAccumulated), Renew \== NewAccumulated, !,
    repl_read(Renew, Read).
repl_read(NewAccumulated,exec(Read)):- string_concat("!",Renew,NewAccumulated), !,
    repl_read(Renew, Read).
repl_read(NewAccumulated, Read):- string_chars(NewAccumulated, Chars),
    balanced_parentheses(Chars), length(Chars, Len), Len > 0, parse_sexpr_untyped(NewAccumulated, Read), !.
repl_read(Accumulated, Read) :- read_line_to_string(current_input, Line), repl_read(Accumulated, Line, Read).
repl_read(Accumulated, "", Read):- !, repl_read(Accumulated, Read).
repl_read(_Accumulated, Line, Read):- Line == end_of_file, !, Read = Line.
repl_read(Accumulated, Line, Read) :- atomics_to_string([Accumulated," ",Line], NewAccumulated), !,
    repl_read(NewAccumulated, Read).
repl_read(Read) :- repl_read("", Read).



repl:-
   current_input(In),
   repeat, make,
   ((nb_current(self_space,Self),Self\==[])->true;Self='&self'),
   format('~N~n'), format(atom(P),'metta@~w: ',[Self]),
   setup_call_cleanup(prompt(Was,P),
      (once(read_metta(In,Read)),once(do_repl(Self,Read))),
       prompt(_,Was)).
do_repl(_Self,end_of_file):- writeln('\n\n% To restart, use: ?- repl.').
do_repl(Self,!):- !, repl_read(Exec),do_repl(Self,exec(Exec)).
do_repl(Self,Read):- string(Read),!,add_history01(Read),repl_read(Read,Term),
  do_metta(Self,load,Term),!,fail.
do_repl(Self,exec(Exec)):- !,
 (with_output_to(string(H),(write('!'),write_src(Exec))),add_history0(H)),
  do_metta_exec(Self,Exec),!,fail.
do_repl(_Slf,call(Term)):- add_history1(Term), !, call(Term),!, fail.
do_repl(Self,Read):-
  (with_output_to(string(H),write_src(Read)),add_history01(H)), do_metta(Self,load,Read),!,fail.

read_metta1(_,O2):- clause(t_l:s_reader_info(O2),_,Ref),erase(Ref).
read_metta1(In,Read):- current_input(In0),In==In0,!, repl_read(Read).
read_metta1(In,Read):- peek_char(In,Char), read_metta1(In,Char,Read).
read_metta1(In,Char,Read):- char_type(Char,white),get_char(In,Char),put(Char),!,read_metta1(In,Read).
read_metta1(In,';',Read):- read_line_to_string(In,Str),write_comment(Str),!,read_metta1(In,Read).
read_metta1(In,_,Read1):- once(parse_sexpr_untyped(In,Read1)),!.
read_metta(In,Read):- read_metta1(In,Read1),
  (Read1=='!'
     -> (read_metta1(In,Read2), Read=exec(Read2))
     ; Read = Read1).

write_comment(Cmt):- format('~N%;~w~n',[Cmt]).
do_metta_cmt(_,'$COMMENT'(Cmt,_,_)):- write_comment(Cmt),!.
do_metta_cmt(_,'$STRING'(Cmt)):- write_comment(Cmt),!.
do_metta_cmt(Self,[Cmt]):- !, do_metta_cmt(Self, Cmt),!.


mfix_vars1(I,O):- var(I),!,I=O.
mfix_vars1([H|T],[HH|TT]):- !, mfix_vars1(H,HH),mfix_vars1(T,TT).
mfix_vars1(I,O):- \+ atom(I),!,I=O.
mfix_vars1('$_','$VAR'('_')).
mfix_vars1('$','$VAR'('_1')).
mfix_vars1(I,'$VAR'(O)):- atom_concat('$',N,I),atom_number(N,Num),atom_concat('Num',Num,M),!,svar_fixvarname(M,O).
mfix_vars1(I,'$VAR'(O)):- atom_concat('$',M,I),!,svar_fixvarname(M,O).
mfix_vars1(I,I).

cons_to_l3(Cons,[Cons0,H,T],[H|TT]):- !, Cons0==Cons,!, cons_to_l3(Cons,T,TT).
cons_to_l3(Cons,Nil0,T):- is_cf_nil(Cons,Nil),Nil0==Nil,!,T=[].
cons_to_l3(_Cons,A,A).

cons_to_l(I,O):- I=='Nil',!,O=[].
cons_to_l(I,O):- I=='T',!,O='True'.
cons_to_l(C,O):- \+ compound(C),!,O=C.
%cons_to_l(N,NO):- cons_to_l3('Cons',N,NO),!.
cons_to_l([Cons,H|T],[HH|TT]):- Cons=='Cons',!, cons_to_l(H,HH),cons_to_l(T,TT).
cons_to_l([Cons|List],List):- Cons=='::',!.
cons_to_l([H|T],[HH|TT]):- !, cons_to_l(H,HH),cons_to_l(T,TT).
cons_to_l(I,I).

is_cons_f(Cons):- is_cf_nil(Cons,_).
is_cf_nil('Cons','Nil').

maybe_fix_vars(I,exec(O)):- compound(I),I=exec(M),!,maybe_fix_vars(M,O).
maybe_fix_vars(I,O):-
 must_det_ll((
  mfix_vars1(I,M),
  cons_to_l(M,O))).

subst_vars(M,N):- sub_term(V,M),compound(V),V='$VAR'(_),!,
  substM(M,V,_NewVar,MM),!,subst_vars(MM,N).
subst_vars(M,M).

metta_anew(load,NV):- subst_vars(NV,Cl),assert_if_new(Cl),ppm(NV).
metta_anew(unload,NV):- subst_vars(NV,Cl),ignore((clause(Cl,_,Ref),clause(Cl2,_,Ref),Cl=@=Cl2,erase(Ref),ppm(Cl))).



:- dynamic((metta_type/3,metta_defn/3,metta_atom/2)).

into_space(Self,'&self',Self):-!.
into_space(_,Other,Other).
into_name(_,Other,Other).

eval_f_args(Self,F,ARGS,[F|EARGS]):- maplist(eval_args0(Depth,Self),ARGS,EARGS).
self_eval(X):- var(X),!.
self_eval(X):- number(X),!.
self_eval([]).
self_eval(X):- is_list(X),!,fail.
%self_eval(X):- is_ref(X),!,fail.
self_eval(X):- atom(X),!.
self_eval('True'). self_eval('False').

combine_result(TF,R2,R2):- TF == [], !.
combine_result(TF,_,TF):-!.

do_metta(Self,LoadExec,Term):- once(maybe_fix_vars(Term,NewTerm)),Term\=@=NewTerm,!,
  do_metta(Self,LoadExec,NewTerm),!.
do_metta(Self,LoadExec,Term):- do_metta1(Self,LoadExec,Term),!.
do_metta(Self,LoadExec,Term):- ppm(unknown_do_metta(Self,LoadExec,Term)).

do_metta1(Self,_,Cmt):- nonvar(Cmt),do_metta_cmt(Self,Cmt),!.
do_metta1(Self,_,exec(Exec)):- !,do_metta_exec(Self,Exec),!.
do_metta1(Self,exec,Exec):- !,do_metta_exec(Self,Exec),!.

do_metta1(Self,Load,[':',Fn,TypeDecl]):- metta_anew(Load,metta_type(Self,Fn,TypeDecl)),!.
do_metta1(Self,Load,['=',PredDecl,True]):- True == 'True',!, metta_anew(Load,metta_atom(Self,PredDecl)).
do_metta1(Self,Load,['=',HeadFn,PredDecl]):- metta_anew(Load,metta_defn(Self,HeadFn,PredDecl)), nop((fn_append(HeadFn,X,Head), fn_append(PredDecl,X,Body), metta_anew((Head:- Body)))),!.
do_metta1(Self,Load,PredDecl):- metta_anew(Load,metta_atom(Self,PredDecl)).

do_metta_exec(Self,Var):- var(Var), !, ppm(eval(Var)), freeze(Var,wdmsg(laterVar(Self,Var))).
do_metta_exec(Self,TermV):-!, ppm(:- metta_eval(TermV)),
  subst_vars(TermV,Term),
  forall(eval_args0(10,Self,Term,X),(format('%'),writeln(X))),!.

s2p(I,O):- sexpr_sterm_to_pterm(I,O),!.

:- loonit_reset.
