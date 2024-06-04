load_and_trim_history:-
 notrace((
  current_input(In), %catch(load_history,_,true),
  ignore(install_readline(In)) )).

%repl:- option_value('repl',prolog),!,prolog.
%:- ensure_loaded(metta_toplevel).

%:- discontiguous do_metta_exec/3.

%repl:- setup_call_cleanup(flag(repl_level,Was,Was+1),repl0,
 % (flag(repl_level,_,Was),(Was==0 -> maybe_halt(7) ; true))).

repl:-  catch(repl2,end_of_input,true).

repl1:-
   with_option('doing_repl',true,
     with_option(repl,true,repl2)). %catch((repeat, repl2, fail)'$aborted',true).
repl2:-
   load_and_trim_history,
   repeat,
     %set_prolog_flag(gc,true),
     reset_caches,
     garbage_collect,
     %set_prolog_flag(gc,false),
     %with_option(not_a_reload,true,make),
      ignore(catch((ignore(catch(once(repl3),restart_reading,true))),
          give_up(Why),pp_m(red,gave_up(Why)))),
      %set_prolog_flag(gc,true),
      fail.
repl3:-
    notrace(( reset_eval_num,
     write_answer_output,
     current_self(Self),
     current_read_mode(repl,Mode),
     %ignore(shell('stty sane ; stty echo')),
     %current_input(In),
      'format'(atom(P),'metta ~w ~w> ',[Self, Mode]))),
      setup_call_cleanup(
         notrace(prompt(Was,P)),
         notrace((ttyflush,repl_read(Expr),ttyflush)),
         notrace(prompt(_,Was))),
      if_trace(replt,fbug(repl_read(Mode,Expr))),
      %fbug(repl_read(Expr)),
      notrace(if_t(Expr==end_of_file,throw(end_of_input))),
      %ignore(shell('stty sane ; stty echo')),
      notrace(ignore(check_has_directive(Expr))),
      once(do_metta(repl_true,Mode,Self,Expr,_)).



check_has_directive(V):- var(V),!,fail.
check_has_directive('log.'):- switch_to_mettalog,!.
check_has_directive('rust.'):- switch_to_mettarust,!.
check_has_directive(Atom):- symbol(Atom),symbol_concat(_,'.',Atom),!.
check_has_directive(call(N=V)):- nonvar(N),!, set_directive(N,V).
check_has_directive(call(Rtrace)):- rtrace == Rtrace,!, rtrace,notrace(throw(restart_reading)).
check_has_directive(NEV):- symbol(NEV), symbolic_list_concat([N,V],'=',NEV), set_directive(N,V).
check_has_directive([AtEq,Value]):-symbol(AtEq),symbol_concat('@',Name,AtEq), set_directive(Name,Value).
check_has_directive(ModeChar):- symbol(ModeChar),metta_interp_mode(ModeChar,_Mode),!,set_directive(repl_mode,ModeChar).
check_has_directive('@'):- do_show_options_values,!,notrace(throw(restart_reading)).
check_has_directive(AtEq):-symbol(AtEq),symbol_concat('@',NEV,AtEq),option_value(NEV,Foo),fbug(NEV=Foo),!,notrace(throw(restart_reading)).
check_has_directive(_).

set_directive(N,V):- symbol_concat('@',NN,N),!,set_directive(NN,V).
set_directive(N,V):- N=='mode',!,set_directive((repl_mode),V).
set_directive(N,V):- show_call(set_option_value_interp(N,V)),!,notrace(throw(restart_reading)).

read_pending_white_codes(In):-
  read_pending_codes(In,[10],[]),!.
read_pending_white_codes(_).

call_for_term_variables4v(Term,[]  ,as_tf(Term,TF),NamedVarsList,TF):- get_global_varnames(NamedVarsList),!.
call_for_term_variables4v(Term,[X]  ,       Term,      NamedVarsList,X):- get_global_varnames(NamedVarsList).



% Check if parentheses are balanced in a list of characters
balanced_parentheses(Str):- string(Str), string_chars(Str,Chars),!,balanced_parentheses(Chars, 0).
balanced_parentheses(Chars) :- balanced_parentheses(Chars, 0).
balanced_parentheses([], 0).
balanced_parentheses(['('|T], N) :- N1 is N + 1, !, balanced_parentheses(T, N1).
balanced_parentheses([')'|T], N) :- N > 0, N1 is N - 1, !, balanced_parentheses(T, N1).
balanced_parentheses([H|T], N) :- H \= '(', H \= ')', !, balanced_parentheses(T, N).
% Recursive function to read lines until parentheses are balanced.

repl_read(NewAccumulated, Expr):-
    symbol_concat(Atom, '.', NewAccumulated),
    catch_err((read_term_from_atom(Atom, Term, []), Expr=call(Term)), E,
       (write('Syntax error: '), writeq(E), nl, repl_read(Expr))),!.


%repl_read(Str, Expr):- ((clause(t_l:s_reader_info(Expr),_,Ref),erase(Ref))).
repl_read("!", '!'):-!.
repl_read("+", '+'):-!.
repl_read(Str,Atom):- atom_string(Atom,Str),metta_interp_mode(Atom,_),!.

repl_read(Str, Expr):- symbol_concat('@',_,Str),!,atom_string(Expr,Str).
repl_read(Str, _Expr):- symbol_concat(')',_,Str),!,fbug(repl_read_syntax(Str)),throw(restart_reading).
repl_read(NewAccumulated, Expr):-
    normalize_space(string(Renew),NewAccumulated),
    Renew \== NewAccumulated, !,
    repl_read(Renew, Expr).
%repl_read(Str, 'add-atom'('&self',Expr)):- symbol_concat('+',W,Str),!,repl_read(W,Expr).
%repl_read(NewAccumulated,exec(Expr)):- string_concat("!",Renew,NewAccumulated), !, repl_read(Renew, Expr).
repl_read(NewAccumulated, Expr):- string_chars(NewAccumulated, Chars),
    balanced_parentheses(Chars), length(Chars, Len), Len > 0,
    read_metta(NewAccumulated,Expr),
    normalize_space(string(Renew),NewAccumulated),
    add_history_string(Renew).
repl_read(Accumulated, Expr) :- read_line_to_string(current_input, Line), repl_read(Accumulated, Line, Expr).

repl_read(_, end_of_file, end_of_file):- throw(end_of_input).

repl_read(Accumulated, "", Expr):- !, repl_read(Accumulated, Expr).
repl_read(_Accumulated, Line, Expr):- Line == end_of_file, !, Expr = Line.
repl_read(Accumulated, Line, Expr) :- symbolics_to_string([Accumulated," ",Line], NewAccumulated), !,
    repl_read(NewAccumulated, Expr).

repl_read(O2):- clause(t_l:s_reader_info(O2),_,Ref),erase(Ref).
repl_read(Expr) :- repeat,
  remove_pending_buffer_codes(_,Was),text_to_string(Was,Str),
      repl_read(Str, Expr),
        % once(((symbol(Expr1),symbol_concat('@',_,Expr1), \+ atom_contains(Expr1,"="), repl_read(Expr2)) -> Expr=[Expr1,Expr2] ; Expr1 = Expr)),
        % this cutrs the repeat/0
        ((peek_pending_codes(_,Peek),Peek==[])->!;true).

add_history_string(Str):- notrace(ignore(add_history01(Str))),!.

add_history_src(Exec):- notrace(ignore((Exec\=[],with_output_to(string(H),with_indents(false,write_src(Exec))),add_history_string(H)))).

add_history_pl(Exec):- var(Exec), !.
add_history_pl(eval(_,catch_red(PL),_)):- !,add_history_pl(PL).
add_history_pl(show_failure(PL)):-!,add_history_pl(PL).
add_history_pl(as_tf(PL,_OUT)):-!,add_history_pl(PL).
add_history_pl(Exec):- notrace(ignore((Exec\=[],with_output_to(string(H),with_indents(false,(writeq(Exec),writeln('.')))),add_history_string(H)))).


:- nb_setval(variable_names,[]).




  %call_for_term_variables5(Term,[],as_tf(Term,TF),[],TF):- symbol(Term),!.
call_for_term_variables5(Term,[],[],[],as_tf(Term,TF),[],TF):- ground(Term),!.
call_for_term_variables5(Term,DC,[],[],call_nth(Term,TF),DC,TF):- ground(Term),!.
call_for_term_variables5(Term,_,[],[_=Var],call_nth(Term,Count),['Count'=Count],Var).
call_for_term_variables5(Term,_,[_=Var],[],call_nth(Term,Count),['Count'=Count],Var).
call_for_term_variables5(Term,_,Vars,[_=Var],Term,Vars,Var).
call_for_term_variables5(Term,_,[_=Var],Vars,Term,Vars,Var).
call_for_term_variables5(Term,_,SVars,Vars,call_nth(Term,Count),[Vars,SVars],Count).



is_interactive(From):- notrace(is_interactive0(From)).
is_interactive0(From):- From==false,!,fail.
is_interactive0(From):- symbolic(From),is_stream(From),!, \+ stream_property(From,filename(_)).
is_interactive0(From):- From = repl_true,!.
is_interactive0(From):- From = true,!.


:- set_prolog_flag(history, 3).

inside_assert(Var,Var):- \+ compound(Var),!.
inside_assert([H,IA,_],IA):- symbol(H),symbol_concat('assert',_,H),!.
inside_assert(Conz,Conz):- is_conz(Conz),!.
inside_assert(exec(I),O):- !, inside_assert(I,O).
inside_assert(Eval,O):- functor(Eval,eval_H,A), A1 is A-1, arg(A1,Eval,I),!, inside_assert(I,O).
%inside_assert(eval_H(I,C),eval_H(O,C)):- !, inside_assert(I,O).
%inside_assert(eval_H(A,B,I,C),eval_H(A,B,O,C)):- !, inside_assert(I,O).
inside_assert(call(I),O):- !, inside_assert(I,O).
inside_assert( ?-(I), O):- !, inside_assert(I,O).
inside_assert( :-(I), O):- !, inside_assert(I,O).
inside_assert(Var,Var).

current_read_mode(repl,Mode):- ((option_value(repl_mode,Mode),Mode\==[])->true;Mode='+'),!.
current_read_mode(file,Mode):- ((nb_current(file_mode,Mode),Mode\==[])->true;Mode='+').



eval(all(Form)):- nonvar(Form), !, forall(eval(Form),true).
eval(Form):-   current_self(Self),   do_metta(true,exec,Self,Form,_Out).

eval(Form,Out):-current_self(Self),eval_H(500,Self,Form,Out).
eval(Self,Form,Out):- eval_H(500,Self,Form,Out).

eval_I(Self,Form,OOut):-
    eval_H(500,Self,Form,Out),
    trace,
    xform_out(Out,OOut).

xform_out(Out,OOut):- is_returned(Out),!,OOut=Out.
xform_out(_Out,'Empty').


name_vars(P):- ignore(name_vars0(P)).
name_vars0(X=Y):- X==Y,!.
name_vars0(X='$VAR'(X)).

reset_cache.
reset_caches:- forall(clause(reset_cache,Body),forall(rtrace_on_error(Body),true)).

interactively_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut):-
  reset_caches,
  catch(interactively_do_metta_exec00(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),
         Error,write_src(error(Error,From,TermV))).


interactively_do_metta_exec00(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut):-
  catch(interactively_do_metta_exec01(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),
         '$aborted',fbug(aborted(From,TermV))).

% Interactively executes a mettalog command if certain conditions are met and hides results based on file settings.
interactively_do_metta_exec01(file(_), Self, _TermV, Term, X, _NamedVarsList, _Was, _Output, _FOut) :-
    file_hides_results(Term), !,
    eval_args(Self, Term, X).

interactively_do_metta_exec01(From,Self,_TermV,Term,X,NamedVarsList,Was,VOutput,FOut):-
  notrace((
    reset_eval_num,
    Result = res(FOut),
    Prev = prev_result('Empty'),
    inside_assert(Term,BaseEval),
    (is_compatio
       -> option_else(answer,Leap,leap)
         ;   option_else(answer,Leap,each)),
    option_else('maximum-result-count',MaxResults,inf), % infinate answers
    option_else('initial-result-count',LeashResults,10), % if print the first 10 answers without stopping
    Control = contrl(MaxResults,Leap),
    Skipping = _,
    % Initialize Control as a compound term with 'each' as its argument.
    %GG = interact(['Result'=X|NamedVarsList],Term,trace_off),
    (((From = file(_Filename), option_value('exec',skip),  \+ always_exec(BaseEval)))
     -> (GG = (skip(Term),deterministic(Complete)),
               %Output =
                %FOut = "Skipped",
                Skipping = 1,!,
                %color_g_mesg('#da70d6', (write('% SKIPPING: '), writeq(eval_H(500,Self,BaseEval,X)),writeln('.'))),
                % color_g_mesg('#fa90f6', (writeln('; SKIPPING'), with_indents(true,write_src(exec(BaseEval))))),
               %  if_t(is_list(BaseEval),add_history_src(exec(TermV))),
                 true
             )
        ; GG =      %$ locally(set_prolog_flag(gc,false),
           (
                             ((  (Term),deterministic(Complete),
                                xform_out(VOutput,Output), nb_setarg(1,Result,Output)))),
    !, % metta_toplevel
   flag(result_num,_,0),
   PL=eval(Self,BaseEval,X),
 ( % with_indents(true,
  \+ \+ (
   user:maplist(name_vars,NamedVarsList),
   user:name_vars('OUT'=X),
    % add_history_src(exec(BaseEval)),
      if_t(Skipping==1,writeln(' ; SKIPPING')),
     %if_t(TermV\=BaseEval,color_g_mesg('#fa90f6', (write('; '), with_indents(false,write_src(exec(BaseEval)))))),
      if_t((is_interactive(From);Skipping==1),
          (
            if_t( \+ option_value(doing_repl,true),
              if_t( \+ option_value(repl,true),
                if_t(   option_value(prolog,true), add_history_pl(PL)))),
            if_t(option_value(repl,true), add_history_src(exec(BaseEval))))),

      prolog_only((color_g_mesg('#da70d6', (write('% DEBUG:   '), writeq(PL),writeln('.'))))),
      true))))),

   in_answer_io(format('~N[')),!,

   (forall_interactive(
    From, WasInteractive,Complete, %may_rtrace
     (timed_call(GG,Seconds)),
     ((Complete==true->!;true),
       %repeat,
       set_option_value(interactive,WasInteractive),
       Control = contrl(Max,DoLeap),
       nb_setarg(1,Result,Output),
       current_input(CI),
       read_pending_codes(CI,_,[]),
       flag(result_num,R,R+1),
       flag(result_num,ResNum,ResNum),
       reset_eval_num,
     if_t(ResNum=<Max,
         ((((ResNum==1,Complete==true)->(not_compatio(format('~NDeterministic: ',  [])), !);          %or Nondet
           ( Complete==true -> (not_compatio(format('~NLast Result(~w): ',[ResNum])),! );
                               not_compatio(format('~NNDet Result(~w): ',[ResNum]))))),
      ignore(((
            not_compatio(if_t( \+ symbolic(Output), nop(nl))),
            %if_t(ResNum==1,in_answer_io(format('~N['))),
            in_answer_io(if_t((Prev\=@=prev_result('Empty')),write(', '))),
            nb_setarg(1,Prev,Output),
            user_io(with_indents(is_mettalog,
             color_g_mesg_ok(yellow,
              \+ \+
               (maplist(maybe_assign,NamedVarsList),
                not_compatio(write_asrc(Output)),
                in_answer_io(write_asrc(Output))))))  ))),

      not_compatio(with_output_to(user_error,give_time('Execution',Seconds))),
      %not_compatio(give_time('Execution',Seconds),
       color_g_mesg(green,
           ignore((NamedVarsList \=@= Was ->(not_compatio((
                reverse(NamedVarsList,NamedVarsListR),
                maplist(print_var,NamedVarsListR), nop(nl)))) ; true))))),
       (
         (Complete \== true, WasInteractive, DoLeap \== leap,
                LeashResults > ResNum, ResNum < Max) ->
         (write("press ';' for more solutions "),get_single_char_key(C),
           not_compatio((writeq(key=C),nl)),
         (C=='b' -> (once(repl),fail) ;
         (C=='m' -> make ;
         (C=='t' -> (nop(set_debug(eval,true)),rtrace) ;
         (C=='T' -> (set_debug(eval,true));
         (C==';' -> true ;
         (C==esc('[A',[27,91,65]) -> nb_setarg(2, Control, leap) ;
         (C=='L' -> nb_setarg(1, Control, ResNum) ;
         (C=='l' -> nb_setarg(2, Control, leap) ;
         (((C=='\n');(C=='\r')) -> (!,fail);
         (!,fail))))))))))));
       (Complete\==true, \+ WasInteractive, Control = contrl(Max,leap)) -> true ;
        (((Complete==true ->! ; true)))))
                    *-> (ignore(Result = res(FOut)),ignore(Output = (FOut)))
                    ; (flag(result_num,ResNum,ResNum),(ResNum==0->
      (in_answer_io(nop(write('['))),not_compatio(format('~N<no-results>~n~n')),!,true);true))),
                    in_answer_io(write(']\n')),
   ignore(Result = res(FOut)).

maybe_assign(N=V):- ignore(V='$VAR'(N)).

mqd:-
  forall(metta_atom(_KB,['query-info',E,T,Q]),
     (writeln(E),
      term_variables(T,TVs),
      term_variables(Q,QVs),
      intersection(TVs,QVs,_,_,SVs),
      notrace(eval(['match','&flybase',Q,T],SVs)))).


get_single_char_key(O):- get_single_char(C),get_single_char_key(C,O).
get_single_char_key(27,esc(A,[27|O])):- !,read_pending_codes(user_input,O,[]),name(A,O).
get_single_char_key(C,A):- name(A,[C]).

forall_interactive(file(_),false,Complete,Goal,After):- !,   Goal, (Complete==true ->  ( After,!)  ;  (  \+  After )).
forall_interactive(prolog,false,Complete,Goal,After):- !,  Goal, (Complete == true -> ! ; true), quietly(After).
forall_interactive(From,WasInteractive,Complete,Goal,After):-
   (is_interactive(From) -> WasInteractive = true ; WasInteractive = false),!,
    Goal, (Complete==true ->  ( quietly(After),!)  ;  (  quietly( \+ After) )).



print_var(Name=Var) :- print_var(Name,Var).
write_var(V):- var(V), !, write_dvar(V),!.
write_var('$VAR'(S)):-  !, write_dvar(S),!.
write_var(V):- write_dvar(V),!.
%print_var(Name,_Var) :- symbol_concat('Num',Rest,Name),atom_number(Rest,_),!.
print_var(Name,Var):- write_var(Name), write(' = '), write_bsrc(Var), nl.

write_asrc(Var):- Var=='Empty',is_compatio,!.
write_asrc(Var):- write_bsrc(Var),!.

write_bsrc(Var):- Var=='Empty',!,write(Var).
write_bsrc(Var):- ground(Var),!,write_src(Var).
write_bsrc(Var):- copy_term(Var,Copy,Goals),Var=Copy,write_bsrc(Var,Goals).
write_bsrc(Var,[]):- write_src(Var).
write_bsrc(Var,[G|Goals]):- write_src(Var), write(' { '),write_src(G),maplist(write_src_space,Goals),writeln(' } ').

write_src_space(Goal):- write(' '),write_src(Goal).


get_term_variables(Term, DontCaresN, CSingletonsN, CNonSingletonsN) :-
    term_variables(Term, AllVars),
    get_global_varnames(VNs),
    writeqln(term_variables(Term, AllVars)=VNs),
    term_singletons(Term, Singletons),
    term_dont_cares(Term, DontCares),
    include(not_in_eq(Singletons), AllVars, NonSingletons),
    include(not_in_eq(DontCares), NonSingletons, CNonSingletons),
    include(not_in_eq(DontCares), Singletons, CSingletons),
    maplist(into_named_vars,[DontCares, CSingletons,  CNonSingletons],
                           [DontCaresN, CSingletonsN, CNonSingletonsN]),
    writeqln([DontCaresN, CSingletonsN, CNonSingletonsN]).

term_dont_cares(Term, DontCares):-
  term_variables(Term, AllVars),
  get_global_varnames(VNs),
  include(has_sub_var(AllVars),VNs,HVNs),
  include(underscore_vars,HVNs,DontCareNs),
  maplist(arg(2),DontCareNs,DontCares).

into_named_vars(Vars,L):- is_list(Vars), !, maplist(name_for_var_vn,Vars,L).
into_named_vars(Vars,L):- term_variables(Vars,VVs),!,into_named_vars(VVs,L).

has_sub_var(AllVars,_=V):- sub_var(V,AllVars).
underscore_vars(V):- var(V),!,name_for_var(V,N),!,underscore_vars(N).
underscore_vars(N=_):- !, symbolic(N),!,underscore_vars(N).
underscore_vars(N):- symbolic(N),!,symbol_concat('_',_,N).

get_global_varnames(VNs):- nb_current('variable_names',VNs),VNs\==[],!.
get_global_varnames(VNs):- prolog_load_context(variable_names,VNs),!.
maybe_set_var_names(List):- List==[],!.
maybe_set_var_names(List):- % fbug(maybe_set_var_names(List)),
   is_list(List),!,nb_linkval(variable_names,List).
maybe_set_var_names(_).

name_for_var_vn(V,N=V):- name_for_var(V,N).

name_for_var(V,N):- var(V),!,get_global_varnames(VNs),member(N=VV,VNs),VV==V,!.
name_for_var(N=_,N):- !.
name_for_var(V,N):- term_to_atom(V,N),!.


really_trace:- once(option_value('exec',rtrace);option_value('eval',rtrace);is_debugging((exec));
  is_debugging((eval))).
% !(pragma! exec rtrace)
may_rtrace(Goal):- really_trace,!,  really_rtrace(Goal).
may_rtrace(Goal):- Goal*->true;( \+ tracing, trace,really_rtrace(Goal)).
really_rtrace(Goal):- is_transpiling,!,rtrace(call(Goal)).
really_rtrace(Goal):- with_debug((e),with_debug((exec),Goal)).

rtrace_on_existence_error(G):- !, catch_err(G,E, (fbug(E=G),  \+ tracing, trace, rtrace(G))).
%rtrace_on_existence_error(G):- catch(G,error(existence_error(procedure,W),Where),rtrace(G)).

%prolog_only(Goal):- !,Goal.
prolog_only(Goal):- if_trace(prolog,Goal).

write_compiled_exec(Exec,Goal):-
%  ignore(Res = '$VAR'('ExecRes')),
  compile_for_exec(Res,Exec,Goal),
  notrace((color_g_mesg('#114411',print_pl_source(answer2(Res):-Goal)))).

verbose_unify(Term):- verbose_unify(trace,Term).
verbose_unify(What,Term):- term_variables(Term,Vars),maplist(verbose_unify0(What),Vars),!.
verbose_unify0(What,Var):- put_attr(Var,verbose_unify,What).
verbose_unify:attr_unify_hook(Attr, Value) :-
    format('~N~q~n',[verbose_unify:attr_unify_hook(Attr, Value)]),
    vu(Attr,Value).
vu(_Attr,Value):- is_ftVar(Value),!.
vu(fail,_Value):- !, fail.
vu(true,_Value):- !.
vu(trace,_Value):- trace.


% Entry point for the user to call with tracing enabled
toplevel_goal(Goal) :-
   term_variables(Goal,Vars),
   interact(Vars, Goal, trace_off).

% Entry point for the user to call with tracing enabled
trace_goal(Goal) :-
    trace_goal(Goal, trace_on).

% Handle tracing
trace_goal(Goal, Tracing) :-
    (Tracing == trace_on -> writeln('Entering goal:'), writeln(Goal) ; true),
    term_variables(Goal, Variables),
    ( call(Goal) ->
        (Tracing == trace_on -> writeln('Goal succeeded with:'), writeln(Variables) ; true),
        interact(Variables, Goal, Tracing)
    ;   (Tracing == trace_on -> writeln('Goal failed.') ; true),
        false
    ).

% Interaction with the user
interact(Variables, Goal, Tracing) :-
    call(Goal),write('Solution: '), write_src(Variables),
    write(' [;next]?'),
    get_single_char(Code),
    (command(Code, Command) ->
        handle_command(Command, Variables, Goal, Tracing)
    ;   writeln('Unknown command.'), interact(Variables, Goal, Tracing) % handle unknown commands
    ).


:- dynamic(is_installed_readline_editline/1).
:- volatile(is_installed_readline_editline/1).
install_readline_editline:- current_input(Input), install_readline(Input),!.

install_readline(Input):- is_installed_readline_editline(Input),!.
install_readline(_):- is_compatio,!.
install_readline(_):-!.
install_readline(Input):-
   assert(is_installed_readline_editline(Input)),
   install_readline_editline1,
   use_module(library(readline)),
   use_module(library(editline)),
   nop(catch(load_history,_,true)),
    add_history_string("!(pfb3)"),
    add_history_string("!(load-flybase-full)"),
    add_history_string("!(obo-alt-id $X BS:00063)"),
    add_history_string("!(and (total-rows $T TR$) (unique-values $T2 $Col $TR))"),
    nop(ignore(editline:el_wrap)),
    nop(ignore(editline:add_prolog_commands(Input))).


:- dynamic  setup_done/0.
:- volatile setup_done/0.

install_readline_editline1 :-
   setup_done,
   !.
install_readline_editline1 :-
   asserta(setup_done),
  '$toplevel':(
   '$clean_history',
   apple_setup_app,
   '$run_initialization',
   '$load_system_init_file',
   set_toplevel,
   '$set_file_search_paths',
   init_debug_flags,
   start_pldoc,
   opt_attach_packs,
   load_init_file,
   catch(setup_backtrace, E1, print_message(warning, E1)),
   catch(setup_readline,  E2, print_message(warning, E2)),
   catch(setup_history,   E3, print_message(warning, E3)),
   catch(setup_colors, E4, print_message(warning, E4))),
   install_readline(user_input).


% Command descriptions
command(59, retry).    % ';' to retry
command(115, skip).    % 's' to skip to the next solution
command(108, leap).    % 'l' to leap (end the debugging session)
command(103, goals).   % 'g' to show current goals
command(102, fail).    % 'f' to force fail
command(116, trace).   % 't' to toggle tracing
command(117, up).      % 'u' to continue without interruption
command(101, exit).    % 'e' to exit the debugger
command(97, abort).    % 'a' to abort
command(98, break).    % 'b' to set a breakpoint
command(99, creep).    % 'c' to proceed step by step
command(104, help).    % 'h' for help
command(65, alternatives).    % 'A' for alternatives
command(109, make).       % 'm' for make (recompile)
command(67, compile).     % 'C' for Compile (compile new executable)

:- style_check(-singleton).

% Command implementations
handle_command(make, Variables, Goal, Tracing) :-
    writeln('Recompiling...'),
    % Insert the logic to recompile the code.
    % This might involve calling `make/0` or similar.
    make,  % This is assuming your Prolog environment has a `make` predicate.
    fail. % interact(Variables, Goal, Tracing).

handle_command(compile, Variables, Goal, Tracing) :-
    writeln('Compiling new executable...'),
    % Insert the logic to compile a new executable.
    % This will depend on how you compile Prolog programs in your environment.
    % For example, you might use `qsave_program/2` to create an executable.
    % Pseudocode: compile_executable(ExecutableName)
    fail. % interact(Variables, Goal, Tracing).
handle_command(alternatives, Variables, Goal, Tracing) :-
    writeln('Showing alternatives...'),
    % Here you would include the logic for displaying the alternatives.
    % For example, showing other clauses that could be tried for the current goal.
    writeln('Alternatives for current goal:'),
    writeln(Goal),
    % Pseudocode: find_alternatives(Goal, Alternatives)
    % Pseudocode: print_alternatives(Alternatives)
    fail. % interact(Variables, Goal, Tracing).
% Extend the command handling with the 'help' command implementation
handle_command(help, Variables, Goal, Tracing) :-
    print_help,
    fail. % interact(Variables, Goal, Tracing).
handle_command(abort, _, _, _) :-
    writeln('Aborting...'), abort.
handle_command(break, Variables, Goal, Tracing) :-
    writeln('Breakpoint set.'), % Here you should define what 'setting a breakpoint' means in your context
    fail. % interact(Variables, Goal, Tracing).
handle_command(creep, Variables, Goal, Tracing) :-
    writeln('Creeping...'), % Here you should define how to 'creep' (step by step execution) through the code
    trace. % interact(Variables, Goal, Tracing).
handle_command(retry, Variables, Goal, Tracing) :-
    writeln('Continuing...'),!.
    %trace_goal(Goal, Tracing).
handle_command(skip, Variables, Goal, Tracing) :-
    writeln('Skipping...').
handle_command(leap, _, _, _) :-
    writeln('Leaping...'), nontrace. % Cut to ensure we stop the debugger
handle_command(goals, Variables, Goal, Tracing) :-
    writeln('Current goal:'), writeln(Goal),
    writeln('Current variables:'), writeln(Variables),
    bt,fail. % interact(Variables, Goal, Tracing).
handle_command(fail, _, _, _) :-
    writeln('Forcing failure...'), fail.
handle_command(trace, Variables, Goal, Tracing) :-
    (Tracing == trace_on ->
        NewTracing = trace_off, writeln('Tracing disabled.')
    ;   NewTracing = trace_on, writeln('Tracing enabled.')
    ),
    interact(Variables, Goal, NewTracing).
handle_command(up, Variables, Goal, Tracing) :-
    writeln('Continuing up...'),
    repeat,
    ( trace_goal(Goal, Tracing) -> true ; !, fail ).
handle_command(exit, _, _, _) :-
    writeln('Exiting debugger...'), !. % Cut to ensure we exit the debugger

:- style_check(+singleton).


% Help description
print_help :-
    writeln('Debugger commands:'),
    writeln('(;)  next             - Retry with next solution.'),
    writeln('(g)  goal             - Show the current goal.'),
    writeln('(u)  up               - Finish this goal without interruption.'),
    writeln('(s)  skip             - Skip to the next solution.'),
    writeln('(c)  creep or <space> - Proceed step by step.'),
    writeln('(l)  leap             - Leap over (the debugging).'),
    writeln('(f)  fail             - Force the current goal to fail.'),
    writeln('(B)  back             - Go back to the previous step.'),
    writeln('(t)  trace            - Toggle tracing on or off.'),
    writeln('(e)  exit             - Exit the debugger.'),
    writeln('(a)  abort            - Abort the current operation.'),
    writeln('(b)  break            - Break to a new sub-REPL.'),
    writeln('(h)  help             - Display this help message.'),
    writeln('(A)  alternatives     - Show alternative solutions.'),
    writeln('(m)  make             - Recompile/Update the current running code.'),
    writeln('(C)  compile          - Compile a fresh executable (based on the running state).'),
    writeln('(E)  error msg        - Show the latest error messages.'),
    writeln('(r)  retry            - Retry the previous command.'),
    writeln('(I)  info             - Show information about the current state.'),
    !.




