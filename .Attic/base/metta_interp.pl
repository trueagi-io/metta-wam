:- ensure_loaded(swi_support).
:- ensure_loaded(swi_flybase).
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_python).

load_metta(File):- load_metta('&self',File).
load_metta(Self,Filename):-
 atom(Filename),exists_file(Filename),!,
 track_load_into_file(Filename,
   setup_call_cleanup(open(Filename,read,In),
    ((directory_file_path(Directory, _BaseName, Filename),
      with_cwd(Directory,once(load_metta_stream(Self,In))))), close(In))).

load_metta(Self,Filename):- with_wild_path(load_metta(Self),Filename),!.

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
  eval_args(Space,A,AA).

eval_args(Self,X,Y):- nonvar(Y),!,eval_args(Self,X,XX),evals_to(XX,Y).
eval_args(Self,X,Y):- eval_args0(Self,X,Y).
%eval_args(Self,X,Y):- eval_args0(Self,X,Y)*->true;Y=[].

eval_args0(_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args0(Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval_args(Self),VI,VO).
eval_args0(Self,[V|VI],VVO):-  \+ is_list(VI),eval_args(Self,VI,VM),
  ( VM\==VI -> eval_args(Self,[V|VM],VVO) ;
    (eval_args(Self,V,VV), (V\==VV -> eval_args(Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_args0(Self,['assertEqual',X,Y],TF):- !, fa_eval_args(Self,X,XX),fa_eval_args(Self,Y,YY),!,trace,as_tf(XX=@=YY,TF).
eval_args0(Self,['assertEqualToResult',X,Y],TF):- !,fa_eval_args(Self,X,L),!,trace,as_tf(L=@=Y,TF).
eval_args0(Self,[X|Nil],[Y]):- Nil ==[],!,eval_args(Self,X,Y).
eval_args0(Self,X,Y):- eval_args1(Self,X,M),(M\==X->eval_args(Self,M,Y);Y=X).

cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
fa_eval_args(Self,X,L):- findall(E,eval_args(Self,X,E),L).

is_case(AA,[AA,Value],Value):-!.
is_case(AA,[AA|Value],Value).

into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).

eval_args1(Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Space,Goal).
%[superpose,[1,2,3]]
eval_args1(Self,['superpose',List],Res):- !, member(E,List),eval_args(Self,E,Res).
eval_args1(Self, Term, Res):-
   get_setarg_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), is_list(ST),ST = [F,List],F=='superpose', %maplist(atomic,List),
   call(P1,Var),!,
   member(Var,List),
   eval_args1(Self, Term, Res).

% Macro Functions
eval_args1(Self,['case',A,[Case1|CaseN]|NonCases],Res):- !,
          eval_arg(A,AA),
           ((member(Case,[Case1|CaseN]),
              is_case(AA,Case,Value))->
             (eval_args(Self,Value,R1),
              eval_args(Self,NonCases,R2),
              combine_result(R1,R2,Res));
             (fail,eval_args(Self,NonCases,Res))).
/*
eval_args1(Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval_args(Self,A,AA),AA\==A,
   eval_args(Self,[F,AA|Args],Res).


eval_args1(Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval_args(Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval_args(Self,[F,A1|NewArgs],Res)))).
*/

eval_args1(Self,['import!',Other,File],Space):- into_space(Self,Other,Space),!, load_metta(Space,File).
eval_args1(Self,[  ',',X|Y],TF):- eval_args(Self,X,TF1),eval_args1(Self,[','|Y],TF2),combine_result(TF1,TF2,TF).
eval_args1(Self,['and',X|Y],TF):- eval_args(Self,X,TF1),eval_args1(Self,['and'|Y],TF2),combine_result(TF1,TF2,TF).
eval_args1(Self,['if',TF,Then,Else],Res):- !, ( \+ eval_args(Self,TF,'False') -> eval_args(Self,Then,Res);eval_args(Self,Else,Res) ).

eval_args1(Self,['let',A,A5,AA],AAO):- !,eval_args(Self,A5,A),eval_args(Self,AA,AAO).
eval_args1(Self,['let*',[Let0|LetRest],Body],RetVal):-
    findall('let'(Var,Val), member([Var,Val],[Let0|LetRest]),LetStars),
    eval_args(Self,[progn,[progn|LetStars],Body],RetVal).
eval_args1(Self,['colapse'|List], Flat):- !, maplist(eval_args(Self),List,Res),flatten(Res,Flat).

eval_args1(Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
eval_args1(Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
eval_args1(Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2.
eval_args1(Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).
eval_args1(Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Space,PredDecl).

%eval_args1(Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args1(Self,['or',X,Y],TF):- !, (eval_args(Self,X,TF);eval_args(Self,Y,TF)).
eval_args1(Self,PredDecl,Res):- eval_args2(Self,PredDecl,Res).

as_tf(G,'True'):- call(G),!. as_tf(_,'False').
eval_selfless(['==',X,Y],TF):-!,as_tf(X=@=Y,TF).
eval_selfless(['>',X,Y],TF):-!,as_tf(X@>Y,TF).
eval_selfless(['<',X,Y],TF):-!,as_tf(X@<Y,TF).
eval_selfless(['=>',X,Y],TF):-!,as_tf(X@>=Y,TF).
eval_selfless(['<=',X,Y],TF):-!,as_tf(X@=<Y,TF).
eval_selfless(LIS,Y):-  notrace((
   LIS=[F,_,_], atom(F), current_op(_,yfx,F),
   catch((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.

% less Macro-ey Functions
eval_args2(_Slf,LESS,Res):- eval_selfless(LESS,Res),!.
eval_args2(_Slf,L1,Res):- !, is_list(L1),maplist(self_eval,L1),!,Res=L1.
eval_args2(Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args(Self,[F2|X2],Y2),eval_args(Self,X1,Y1).
eval_args2(Self,[F|X],[F|Y]):- is_function(F),is_list(X),maplist(eval_args(Self),X,Y),X\=@=Y.
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
eval_args2(Self,X,Y):- metta_defn(Self,X,W),ppm(metta_defn(Self,X,W)), eval_args(Self,W,Y).
eval_args2(Self,PredDecl,Res):- term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval_args(Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))).
eval_args2(Self,['ift',CR,Then],RO):- trace,
   metta_defn(Self,['ift',R,Then],Become),eval_args(Self,CR,R),eval_args(Self,Then,_True),eval_args(Self,Become,RO).



%metta_atom_iter(Other,H):- metta_atom(Other,H).
%metta_atom_iter(Other,H):- eval_args(Other,H,_).

%metta_atom_iter(Other,H):-metta_defn(Other,H,B),metta_atom_iter(Other,B).
metta_atom_iter(Other,H):-metta_atom(Other,H).
metta_atom_iter_ref(Other,['=',H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).

is_function(F):- atom(F).
is_false(X):- eval_arg(X,Y), (Y=0;Y='False'),!.
is_conz(Self):- compound(Self), Self=[_|_].

%dont_x(eval_args(Self,metta_if(A<B,L1,L2),R)).
dont_x(eval_args(_Self,_<_,_)).

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

do:- cls, make, current_prolog_flag(argv,P),append(_,['--args'|Rest],P),Rest\==[],!,maplist(load_metta('&self'),Rest).
do:- repl.


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

ppm(Cl):-
  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).

:- dynamic((metta_type/3,metta_defn/3,metta_atom/2)).

into_space(Self,'&self',Self):-!.
into_space(_,Other,Other).


substM(T, F, R, R):- T==F,!.
substM(T, _, _, R):- \+ compound(T),!,R=T.
substM([H1|T1], F, R, [H2|T2]) :- !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- C1 =.. [Fn|A1], substM_l(A1,F,R,A2),!, C2 =.. [Fn|A2].
substM_l([], _, _, []).  substM_l([H1|T1], F, R, [H2|T2]) :- substM(H1, F, R, H2), substM_l(T1, F, R, T2).


eval_f_args(Self,F,ARGS,[F|EARGS]):- maplist(eval_args(Self),ARGS,EARGS).
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
  forall(eval_args(Self,Term,X),(format('%'),writeln(X))),!.

s2p(I,O):- sexpr_sterm_to_pterm(I,O),!.

:- ensure_loaded(metta_testing).
