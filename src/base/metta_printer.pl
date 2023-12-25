% ===============================
%       PRINTERS
% ===============================
% 'ppc' and 'ppc1' rules pretty-print original terms and convert them to metta if different,
% printing the converted forms as well.
ppc(Msg,Term):- ppc1(Msg,Term), p2m(Term,MeTTa),!, (MeTTa\==Term -> ppc1(p2m(Msg),MeTTa) ; true).

ppc1(Msg,Term):- \+ \+ ( ppct(Msg,Term) ),!.

ppc1(Msg,Term):- \+ \+ ( ignore(guess_pretty(Term)),
   writeln('---------------------'),
   write(p(Msg)),write(':'),nl,
   portray_clause(Term),
   writeln('---------------------'),
   \+ \+ (print_tree(?-show_cvts(Term))),nl,
    writeln('---------------------'),
     write(s(Msg)),write(':'),nl,
     write_src(Term),nl).

ppct(Msg,Term):- is_list(Term),!,
  writeln('---------------------'),
  numbervars(Term,666,_,[attvar(bind)]),
  write((Msg)),write(':'),nl,
  write_src(Term),nl.
ppct(Msg,Term):- Term=(_ :- _),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  portray_clause(Term),nl.
ppct(Msg,Term):- Term=(_=_),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  numbervars(Term,444,_,[attvar(bind)]),
  write_src(Term),nl.
ppct(Msg,Term):- Term=(_ :- _),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  numbervars(Term,222,_,[attvar(bind)]),
  print_tree(Term),nl.

% 'pp_metta' rule is responsible for pretty-printing metta terms.
pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

% The predicate with_indents/2 modifies the src_indents option value during the execution of a goal.
% The first argument is the desired value for src_indents,
% and the second argument is the Goal to be executed with the given src_indents setting.
with_indents(TF, Goal) :-
    % Set the value of the `src_indents` option to TF and then execute the Goal
    with_option(src_indents, TF, Goal).

% The predicate allow_concepts/0 checks whether the use of concepts is allowed.
% It does this by checking the value of the concepts option and ensuring it is not false.
allow_concepts :- !, fail,
    % Check if the option `concepts` is not set to false
    option_else(concepts, TF, 'False'),
    \+ TF == 'False'.

% The predicate with_concepts/2 enables or disables the use of concepts during the execution of a given goal.
% The first argument is a Boolean indicating whether to enable (true) or disable (false) concepts.
% The second argument is the Goal to be executed with the given concepts setting.
with_concepts(TF, Goal) :-
    % Set the value of the `concepts` option to TF and then execute the Goal
    with_option(concepts, TF, Goal).


% Various 'write_src' and 'write_src0' rules are handling the writing of the source,
% dealing with different types of values, whether they are lists, atoms, numbers, strings, compounds, or symbols.
write_src(V):- notrace(write_src0(V)).
write_src0(V):- V ==[],!,write('()').
write_src0(V):- allow_concepts,!,with_concepts('False',write_src1(V)),flush_output.
write_src0(V):- is_list(V),!,pp_sexi(V).
write_src0(V):- write_src1(V),!.

% Handling the final write when the value is a variable or a '$VAR' structure.
is_final_write(V):- var(V), !, format('$~p',[V]).
is_final_write('$VAR'(S)):- !, write('$'),write(S).

% Handling more cases for 'write_src1', when the value is a number, a string, a symbol, or a compound.
write_src1(V) :- is_final_write(V),!.
write_src1((Head:-Body)) :- !, print_metta_clause0(Head,Body).
write_src1(''):- !, write('()').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).

% Continuing with 'write_src1', 'write_mobj', and related rules,
% handling different cases based on the value’s type and structure, and performing the appropriate writing action.
write_src1(V):- symbol(V), should_quote(V),!,
  symbol_string(V,S),writeq(S).
write_src1(V):- symbol(V),!,write(V).
write_src1(V):- compound(V), \+ is_list(V),!,write_mobj(V).
write_src1(V):- pp_sex(V),!.

write_mobj(V) :- is_final_write(V),!.
write_mobj(V):- ( \+ compound(V) ; is_list(V)),!, write_src0(V).

write_mobj(V):- compound_name_list(V,F,Args),write_mobj(F,Args),!.
write_mobj(V):- writeq(V).
write_mobj(exec,[V]):- !, write('!'),write_src(V).
write_mobj('$OBJ',[_,S]):- write('['),write_src(S),write(' ]').
write_mobj('{...}',[S]):- write('{'),write_src(S),write(' }').
write_mobj('[...]',[S]):- write('['),write_src(S),write(' ]').
write_mobj('$STRING',[S]):- !, writeq(S).
write_mobj(F,Args):- fail, mlog_sym(K),!,pp_sexi([K,F|Args]).
write_mobj(F,Args):- pp_sexi([F|Args]).

% Rules for determining when a symbol needs to be quoted in metta.
dont_quote(Atom):- atom_length(Atom,1), !, char_type(Atom,punct).
dont_quote(Atom):- atom(Atom),upcase_atom(Atom,Atom),downcase_atom(Atom,Atom).

should_quote(Atom) :- \+ atom(Atom), \+ string(Atom),!,fail.
should_quote(Atom) :-
   \+ dont_quote(Atom),
   % atom(Atom),  % Ensure that the input is an atom
    atom_chars(Atom, Chars),
    once(should_quote_chars(Chars);should_quote_atom_chars(Atom,Chars)).

contains_unescaped_quote(['"']):- !, fail. % End with a quote
contains_unescaped_quote(['"'|_]) :- !.
contains_unescaped_quote(['\\', '"'|T]) :- !, contains_unescaped_quote(T).
contains_unescaped_quote([_|T]) :- contains_unescaped_quote(T).

% Check if the list of characters should be quoted based on various conditions
should_quote_chars([]).
should_quote_chars(['"'|Chars]):- !, contains_unescaped_quote(Chars).
should_quote_chars(Chars) :-
      member('"', Chars);         % Contains quote not captured with above clause
      member(' ', Chars);         % Contains space
      member('''', Chars);        % Contains single quote
    %  member('/', Chars);         % Contains slash
      member(',', Chars);         % Contains comma
      (fail,member('|', Chars)).         % Contains pipe
%should_quote_atom_chars(Atom,_) :- atom_number(Atom,_),!.
should_quote_atom_chars(Atom,[Digit|_]) :- fail, char_type(Digit, digit), \+ atom_number(Atom,_).

% Example usage:
% ?- should_quote('123abc').
% true.
% ?- should_quote('123.456').
% false.

