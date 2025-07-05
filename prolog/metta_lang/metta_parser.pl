/* <not-a-module> Metta File Processor

This module provides utilities to process `.metta` files by reading S-expressions and comments from an input stream
and writing them as structured facts to an output stream, including the position information of each element within the file.

The module is designed to work with multifile predicates to store the metadata and content of processed files
in a modular manner. It facilitates the extraction of S-expressions, lists, and comments from the input file
while tracking their line, column, and character positions.

The main components of the module are:
  - `process_expressions/3`: Reads and processes the contents of a file (S-expressions and comments) and writes the processed information, including file positions, to an output stream.
  - `make_DL/4`: Generates and asserts facts for each processed item based on its type (list or non-list) and its position in the file.

### Multifile Declarations:
This module makes use of the following multifile predicates, which can be extended across different modules:
  - `afn_stem_filename/3`: Associates the absolute file name, file name stem, and the original file name.
  - `metta_file_buffer/7`: Stores processed S-expression data along with its file position and source file.
  - `metta_file_comment/5`: Stores processed comment data along with its file position and source file.

### Example Use Case:
1. Processing a `.metta` file:
   ```prolog
   process_expressions('example.metta', InStream, OutStream).
   ```
   This will read the S-expressions and comments from `example.metta`, process them, and output structured facts to `OutStream`.

2. Assertions made by `make_DL/4` will be based on the *
type of items being processed, allowing for dynamic and flexible *
handling of lists and individual items.s * *
*/


% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

%!  read_metta(+In, -Expr) is det.
%
%   Reads a MeTTa expression from an input source.
%
%   This predicate reads expressions from `In`, choosing different parsing methods
%   based on the input type and stream properties. If the current input stream differs
%   from `In`, it uses `parse_sexpr_untyped/2` to read the expression.
%
%   @arg In   The input source from which to read the expression.
%   @arg Expr The parsed S-expression (variables are free attvars with `vn` attributes)
%
%   @example
%     % Read an expression from the input stream.
%     ?- read_metta(In, Expr).
%
read_metta(In, Expr) :-
    % If `In` is the current input stream, read with `repl_read/1`.
    current_input(In0), In == In0, !, repl_read(Expr).

read_metta(I, O) :-
    % Use read_file_sexpr/2 to parse the input into an S-expression.
    catch(parse_sexpr_untyped(I,M),E,handle_read_error(E)),
    untyped_to_metta(M, O).

handle_read_error(E):-
    ignore(show_read_error(E)),
    notrace(throw(E)).
    %throw('$aborted'),

:- export(extract_lvars/3).

%! read_file_sexpr(+Stream:stream, -Item) is det.
%
% Reads a single item (S-expression or comment) from the specified stream, handling different formats and encodings.
% Throws an error with stream position if the S-expression cannot be parsed.
% @arg Stream Stream from which to read.
% @arg Item The item read from the stream with variables as `$VAR`/1s
read_file_sexpr(I,O):-
  catch(parse_sexpr(I,O),E,show_read_error(E)).

show_read_error(E):-
  write_src_uo(E),
  print_message(error,E),!,
  fail.


%!  parse_sexpr_untyped(+Input, -Output) is det.
%
%   Parses an untyped S-expression from the input.
%
%   This predicate acts as a wrapper around `parse_sexpr/2`, which performs
%   the actual parsing of S-expressions.
%
%   @arg Input  The input from which the S-expression is parsed.
%   @arg Output The parsed S-expression (variables are free attvars with `vn` attributes
parse_sexpr_untyped(I, O) :-
    % Call the helper predicate to parse the S-expression.
    parse_sexpr(I, M),
    subst_varnames(M, O).


%! parse_sexpr(+Stream:stream, -Item) is det.
%
% Reads a single item (S-expression or comment) from the specified stream, handling different formats and encodings.
% Throws an error with stream position if the S-expression cannot be parsed.
% @arg Stream Stream from which to read.
% @arg Item The item read from the stream with variables as `$VAR`/1s
parse_sexpr(I,O):- string(I), open_string(I,S),!,parse_sexpr(S,O).
parse_sexpr(_, O) :-   % Remove clause if it exists for a previous read.
   clause(t_l:s_reader_info(O), _, Ref), erase(Ref).
parse_sexpr(I,O):-
  setup_call_cleanup( flag('$file_src_ordinal',Ordinal,Ordinal+1_000_000),
    setup_call_cleanup(
       (nb_current('$file_src_depth', Lvl)->true;(Lvl=0,nb_setval('$file_src_depth', Lvl))),
        cont_sexpr(is_delimiter,I, O),
        b_setval('$file_src_depth', Lvl)),
   nop(flag('$file_src_ordinal',_,Ordinal))).



%!  subst_vars(+TermWDV, -NewTerm) is det.
%
%   Substitutes variables in `TermWDV` to produce `NewTerm`, setting variable names as needed.
%
%   This predicate invokes `subst_vars/3` with an accumulator and named variables list,
%   then optionally sets the variable names.
%
%   @arg TermWDV  The term with potential variables.
%   @arg NewTerm  The term with variables substituted.
%
subst_vars(TermWDV, NewTerm):-
     subst_vars(TermWDV, NewTerm, NamedVarsList),
     maybe_set_var_names(NamedVarsList).

%!  subst_vars(+TermWDV, -NewTerm, -NamedVarsList) is det.
%
%   Substitutes variables in `TermWDV` and collects them in `NamedVarsList`.
%
%   @arg TermWDV       The term with variables.
%   @arg NewTerm       The term with substituted variables.
%   @arg NamedVarsList The list of named variables.
%
subst_vars(TermWDV, NewTerm, NamedVarsList) :-
   must_det_lls((
     subst_vars(TermWDV, NewTerm, [], NamedVarsList),
     if_t(fast_option_value('vn', 'true'), memorize_varnames(NamedVarsList)))).



subst_varnames(Convert,Converted):-
  must_det_lls((
   subst_vars(Convert,Converted,[], NVL),
   memorize_varnames(NVL),
   maybe_set_var_names(NVL))).


memorize_varnames(NamedVarsList):- \+ compound(NamedVarsList),!.
memorize_varnames([NamedVar|NamedVarsList]):- !,
  memorize_varname(NamedVar),
  memorize_varnames(NamedVarsList).
memorize_varnames(_).

memorize_varname(NamedVar):-  \+ compound(NamedVar),!.
%memorize_varname(Name=Var):- var(Var),atomic(Name),Var = '$'(Name), !.
memorize_varname(Name=Var):- var(Var),atomic(Name),put_attr(Var,vn,Name).
memorize_varname(_).



%!  subst_vars(+Term, -Term, +Acc, -NamedVarsList) is det.
%
%   Recursively substitutes variables in lists and compound terms, using an accumulator.
%
%   @arg Term           The term to process for substitution.
%   @arg Term           The resulting term with substitutions.
%   @arg Acc            Accumulator for variable tracking.
%   @arg NamedVarsList  List of named variables after substitution.
%


subst_vars(Term, Term, NamedVarsList, NamedVarsList) :-
     % Base case: return variable terms directly.
     var(Term), !.
subst_vars([], [], NamedVarsList, NamedVarsList):- !.
subst_vars('$VAR'('_'), _, NamedVarsList, NamedVarsList) :- !.
subst_vars('$VAR'(VName), Var, Acc, NamedVarsList) :-
     % Substitute variables with `VName`, applying fixes if necessary.
     nonvar(VName), svar_fixvarname_dont_capitalize(VName, Name), !,
     (memberchk(Name = Var, Acc) -> NamedVarsList = Acc ; (!, Var = _, NamedVarsList = [Name = Var | Acc])).
subst_vars(Term, Var, Acc, NamedVarsList) :-
     % Substitute variables with names starting with `$`.
     atom(Term), symbol_concat('$', DName, Term), dvar_name(DName, Name), !,
     subst_vars('$VAR'(Name), Var, Acc, NamedVarsList).
subst_vars(TermI, TermO, Acc, NamedVarsListNew) :-
     % Substitute variables with `VName`, applying fixes if necessary.
     sub_term_safely(Sub,TermI),compound(Sub), Sub='$'(VName),
     must_det_lls((nonvar(VName), svar_fixvarname_dont_capitalize(VName, Name),
     Var = _,
     (memberchk(Name = Var, Acc) -> NamedVarsList = Acc ; (!, Var = _, NamedVarsList = [Name = Var | Acc])),
     subst001(TermI,Sub,Var,MidTerm), MidTerm \=@= TermI,
     subst_vars(MidTerm, TermO, NamedVarsList,NamedVarsListNew))),!.
subst_vars([TermWDV | RestWDV], [Term | Rest], Acc, NamedVarsList) :- !,
     subst_vars(TermWDV, Term, Acc, IntermediateNamedVarsList),
     subst_vars(RestWDV, Rest, IntermediateNamedVarsList, NamedVarsList).
subst_vars(TermWDV, NewTerm, Acc, NamedVarsList) :-
     % Recursively handle compound terms.
     compound(TermWDV), !,
     compound_name_arguments(TermWDV, Functor, ArgsWDV),
     subst_vars(ArgsWDV, Args, Acc, NamedVarsList),
     compound_name_arguments(NewTerm, Functor, Args).
subst_vars(Term, Term, NamedVarsList, NamedVarsList).

/*
subst_vars(Term, Term, NamedVarsList, NamedVarsList) :-
     % Base case: return variable terms directly.
     var(Term), !.
subst_vars([], [], NamedVarsList, NamedVarsList):- !.
subst_vars(TermI, TermO, Acc, NamedVarsList) :-
    sub_term_safely(Sub,TermI),denotes_var(Sub,DName), % DName \== '_', !,
    must_det_lls((subst001(TermI,Sub,Var,MidTerm),
    MidTerm \=@= TermI,
    subst_vars(MidTerm, TermO, [DName=Var|Acc],NamedVarsList))).


subst_vars('$VAR'(Anon), _AnonVar, NamedVarsList, NamedVarsList) :- '__' == Anon, !.
subst_vars('$'(Anon), _AnonVar, NamedVarsList, NamedVarsList) :- '_' == Anon, !.

subst_vars([TermWDV | RestWDV], [Term | Rest], Acc, NamedVarsList) :- !,
     subst_vars(TermWDV, Term, Acc, IntermediateNamedVarsList),
     subst_vars(RestWDV, Rest, IntermediateNamedVarsList, NamedVarsList).
subst_vars('$VAR'(VName), Var, Acc, NamedVarsList) :-
     % Substitute variables with `VName`, applying fixes if necessary.
     nonvar(VName), svar_fixvarname_dont_capitalize(VName, Name), !,
     (memberchk(Name = Var, Acc) -> NamedVarsList = Acc ; (!, Var = _, NamedVarsList = [Name = Var | Acc])).
subst_vars(Term, Var, Acc, NamedVarsList) :- atom(Term), symbol_concat('$', DName, Term), dvar_name(DName, Name), !,
     trace, throw(cant_be_var(Term)),
     % Substitute variables with names starting with `$`.
     subst_vars('$VAR'(Name), Var, Acc, NamedVarsList).
subst_vars(TermWDV, NewTerm, Acc, NamedVarsList) :-
     % Recursively handle compound terms.
     compound(TermWDV), !,
     compound_name_arguments(TermWDV, Functor, ArgsWDV),
     subst_vars(ArgsWDV, Args, Acc, NamedVarsList),!,
     compound_name_arguments(NewTerm, Functor, Args).
subst_vars(Term, Term, NamedVarsList, NamedVarsList).
*/

denotes_var(Var,_Name):- var(Var),!,fail.
denotes_var('$VAR'(U),Name):- !, U \== '__', dvar_name(U, Name).
denotes_var('$'(U), Name):- !, U \== '_', dvar_name(U, Name).
denotes_var(Term, Name):- atom(Term), symbol_concat('$', DName, Term), dvar_name(DName, Name),!.


%!  extract_lvars(?A, ?B, ?After) is det.
%
%   Extract L-vars (logical variables).
%
%   This predicate extracts logical variables by processing the initial list
%   of variables (if any), removing incomplete variables, and copying them
%   into the output.
%
%   @arg A      The first term from which to extract logical variables.
%   @arg B      The list of variables to process.
%   @arg After  The resulting list after processing.
%
extract_lvars(A,B,After):-
    % Retrieve the variable list, if available, otherwise initialize to an empty list.
     (get_varname_list(Before)->true;Before=[]),
    % Remove incomplete variables from the list.
     remove_incompletes(Before,CBefore),!,
    % Copy the logical variables from the input.
     copy_lvars(A,CBefore,B,After),!.


%!  copy_lvars(:TermVAR, ?Vars, :TermNV, ?NVars) is det.
%
%   Copy logical variables.
%
%   This predicate copies logical variables from the first term, passing
%   through a list of variables, and outputs the copied result.
%
%   @arg TermVAR The term with logical variables to copy.
%   @arg Vars    The list of current variables.
%   @arg TermNV  The output term after copying logical variables.
%   @arg NVars   The updated list of variables after copying.
%

% Removed as it was commented out in the original code.
% copy_lvars( VAR,Vars,VAR,Vars):- var(VAR),!.

copy_lvars(Term, Vars, Out, VarsO) :-
    % If the term is an empty list, directly return it and pass along Vars.
    Term == [], !,
    must_det_ll((Out = Term, VarsO = Vars)).
copy_lvars(VAR, Vars, Out, VarsO) :-
    % If the term is a variable, return it unchanged and pass along Vars.
    var(VAR), !,
    must_det_ll((Out = VAR, VarsO = Vars)).
copy_lvars([H|T], Vars, [NH|NT], VarsO) :-
    % If the term is a list, recursively process the head and tail.
    !, copy_lvars(H, Vars, NH, SVars), !,
    copy_lvars(T, SVars, NT, VarsO).
copy_lvars('?'(Inner), Vars, Out, VarsO) :-
    % If the term is a '?'-prefixed variable, process the inner term and handle accordingly.
    !, copy_lvars(Inner, Vars, NInner, VarsO),
    must_det_ll((symbol(NInner) -> atom_concat_or_rtrace('?', NInner, Out) ; Out = '?'(NInner))), !.
copy_lvars(VAR, Vars, Out, VarsO) :-
    % If the term is a logical variable, register it and return the new variable list.
    svar(VAR, Name) -> must_det_ll(symbol(Name)), !,
    must_det_ll(register_var(Name = Out, Vars, VarsO)).
copy_lvars(VAR, Vars, Out, VarsO) :-
    % If the term is atomic (non-compound), return it unchanged.
    \+ compound(VAR), !,
    must_det_ll((Out = VAR, VarsO = Vars)).
copy_lvars(Term,Vars,NTerm,VarsO):-
    % If the term is compound, decompose it and process the arguments.
    Term =.. [F | Args],
    (svar(F,_)-> copy_lvars( [F|Args],Vars,NTerm,VarsO);
        % Reconstruct the term after copying the arguments.
       (copy_lvars(Args,Vars,NArgs,VarsO), NTerm=..[F|NArgs])),!.



%=

%!  svar(?Var, ?NameU) is det.
%
%   Checks if this is a KIF (Knowledge Interchange Format) variable and converts it to a name suitable for Prolog.
%
%   @arg Var   The variable to check.
%   @arg NameU The name of the variable after conversion.
%
svar(SVAR, UP) :-
    % Handle the case where the name is already bound.
    nonvar(UP), !, trace_or_throw(nonvar_svar(SVAR, UP)).
svar(Var, Name) :-
    % If the variable is unbound, fix its name.
    var(Var), !, must_det_ll(svar_fixvarname(Var, Name)).
svar('$VAR'(Var), Name) :-
    % Handle Prolog internal variables.
    number(Var), Var > -1, !,
    must_det_ll(format(atom(Name), '~w', ['$VAR'(Var)])), !.
svar('$VAR'(Name), VarName) :-
    % Process '$VAR' variables.
    !, must_det_ll(svar_fixvarname(Name, VarName)).
svar('?'(Name), NameU) :-
    % Handle variables prefixed by '?'.
    svar_fixvarname(Name, NameU), !.
svar(_, _) :-
    % Fail if not in a valid KIF context.
    \+ kif_ok, !, fail.
svar(VAR, Name) :-
    % If the variable starts with '?', fix its name.
    symbol(VAR), atom_concat_or_rtrace('?', A, VAR), non_empty_atom(A),
    svar_fixvarname(VAR, Name), !.
svar([], _) :-
    % Fail if the variable is an empty list.
    !, fail.
svar('#'(Name), NameU) :-
    % Handle variables prefixed by '#'.
    !, svar(Name, NameU), !.
svar('@'(Name), NameU) :-
    % Handle variables prefixed by '@'.
    svar_fixvarname(Name, NameU), !.
svar(VAR, Name) :-
    % If the variable starts with '@', fix its name.
    symbol(VAR), atom_concat_or_rtrace('@', A, VAR), non_empty_atom(A),
    svar_fixvarname(VAR, Name), !.

kif_ok:- fail.

:- export(svar_fixvarname/2).

%=

%!  svar_fixvarname(?SVARIN, ?UP) is det.
%
%   Fix the variable name.
%
%   @arg SVARIN The input variable name.
%   @arg UP     The fixed variable name.
%
svar_fixvarname(SVAR, UP) :-
    % If the name is already bound, throw an error.
    nonvar(UP), !, trace_or_throw(nonvar_svar_fixvarname(SVAR, UP)).
%
svar_fixvarname(SVAR, UP) :-
    % Fix the name if it follows certain conventions.
    svar_fixname(SVAR, UP), !.
svar_fixvarname(SVAR, UP) :-
    % If fixing fails, throw an error.
    fail, trace_or_throw(svar_fixname(SVAR, UP)).
svar_fixvarname(SVAR, UP):- integer(SVAR),!,sformat(SUP,'~w',['$VAR'(SVAR)]), svar_fixvarname(SUP, UP).
svar_fixvarname(SVAR, UP):- integer(SVAR),UP=SVAR,!.
svar_fixvarname(SVAR, UP):- svar(SVAR,UP),!.
svar_fixvarname(SVAR, UP):- n_to_vn(UP,SVAR),!.
% convert_to_var_name(+Input, -VarName)
% Converts Input (atom or string) into a Prolog-legal variable name.
% It replaces illegal characters with underscores and then prefixes the name with '_'.
svar_fixvarname(Input, VarName) :-
    % Convert input to character list
    (   atom(Input) -> atom_chars(Input, Chars)
    ;   string(Input) -> string_chars(Input, Chars)
    ),
    % Transform each character into an allowed sequence
    maplist(char_to_allowed_sequence, Chars, TransformedList),
    % Flatten the list of lists into a single list of chars
    flatten(TransformedList, SafeChars),
    % Always prefix with '_'
    VarChars = ['_'|SafeChars],
    atom_chars(VarName, VarChars).

% char_to_allowed_sequence(+Char, -Sequence)
% If Char is alphanumeric or '_', it is kept as is.
% Otherwise, convert it into '_<ASCII_CODE>'.
char_to_allowed_sequence(Char, [Char]) :-
    (char_type(Char, alnum); Char == '_'), !.
char_to_allowed_sequence(Char, Sequence) :-
    % Get ASCII code
    char_code(Char, Code),
    % Convert code to a list of digits
    number_chars(Code, CodeChars),
    % Build a sequence like ['_', '5', '4'] for Code = 54, for example
    Sequence = ['_'|CodeChars].


%!  svar_fixname(?Var, ?NameO) is det.
%
%   Fix the name of the variable if needed.
%
%   @arg Var    The variable to fix.
%   @arg NameO  The output variable name.
%
svar_fixname(Var, NameO) :-
    % If the variable is unbound, get its name.
    var(Var), !,
    variable_name_or_ref(Var, Name), sanity(nonvar(Name)), !,
    svar_fixvarname(Name, NameO).
svar_fixname('$VAR'(Name), UP) :-
    % Process Prolog internal '$VAR' variables.
    !, svar_fixvarname(Name, UP).
svar_fixname('@'(Name), UP) :-
    % Handle variables prefixed by '@'.
    !, svar_fixvarname(Name, UP).
svar_fixname('?'(Name), UP) :-
    % Handle variables prefixed by '?'.
    !, svar_fixvarname(Name, UP).
svar_fixname('block'(Name), UP) :-
    % Handle 'block' variables.
    !, svar_fixvarname(Name, UP).

svar_fixname('_', '_') :- !.
svar_fixname('', '__') :- !.

svar_fixname(SVAR, SVARO) :-
    % If the name is already valid, return it as is.
    ok_var_name(SVAR), !, SVARO = SVAR.
svar_fixname('??', '_') :-
    % Special case for '??'.
    !.
svar_fixname(QA, AU) :-
    % Handle variables starting with '??'.
    atom_concat_or_rtrace('??', A, QA), non_empty_atom(A), !,
    svar_fixvarname(A, AO), atom_concat_or_rtrace('_', AO, AU).
svar_fixname(QA, AO) :-
    % Handle variables starting with '?'.
    atom_concat_or_rtrace('?', A, QA), non_empty_atom(A), !,
    svar_fixvarname(A, AO).
svar_fixname(QA, AO) :-
    % Handle variables starting with '@'.
    atom_concat_or_rtrace('@', A, QA), non_empty_atom(A), !,
    svar_fixvarname(A, AO).
svar_fixname(NameU, NameU) :-
    % Handle variables starting with '_', followed by numbers.
    atom_concat_or_rtrace('_', Name, NameU),
    non_empty_atom(Name), atom_number(Name, _), !.
svar_fixname(NameU, NameUO) :-
    % Handle variables starting with '_', followed by a non-number.
    atom_concat_or_rtrace('_', Name, NameU), non_empty_atom(Name),
 \+ atom_number(Name,_),!,svar_fixvarname(Name,NameO),atom_concat_or_rtrace('_',NameO,NameUO).
svar_fixname(I,O):-
    % Perform final adjustments on the variable name by replacing special characters.
 notrace((
  notrace(catch(fix_varcase(I,M0),_,fail)),
  atom_subst(M0,'@','_AT_',M1),
  atom_subst(M1,'?','_Q_',M2),
  atom_subst(M2,':','_C_',M3),
  atom_subst(M3,'-','_',O),
  ok_var_name(O))),!.

%=

%!  fix_varcase(?I, ?O) is det.
%
%   Fix the case of a variable name.
%
%   @arg I  The input variable name.
%   @arg O  The output variable name after case adjustment.
%
fix_varcase(Word, Word) :-
    % If the word starts with '_', leave it unchanged.
    atom_concat_or_rtrace('_', _, Word), !.
fix_varcase(Word, WordC) :- string(Word),atom_string(Atom,Word),!,fix_varcase(Atom, WordC).
fix_varcase('', '__') :- !.
fix_varcase(Word, WordC) :- atom(Word),downcase_atom(Word, UC),Word=UC,atom_concat('_',UC,WordC),!.
fix_varcase(Word, WordC) :-
    % Convert the first letter to uppercase.
    !, atom_codes(Word, [F | R]), to_upper(F, U), atom_codes(WordC, [U | R]).
fix_varcase(Word, Word) :-
    % If the word is already uppercase, leave it unchanged.
    upcase_atom(Word, UC), UC = Word, !.
fix_varcase(Word, WordC) :-
    % Convert the first letter to uppercase if the word is lowercase.
    downcase_atom(Word, UC), UC = Word, !,
    atom_codes(Word, [F | R]), to_upper(F, U), atom_codes(WordC, [U | R]).
fix_varcase(Word, Word).  % Handle mixed-case words.

:- export(ok_varname_or_int/1).

%!  ok_varname_or_int(?Name) is det.
%
%   Checks if a name is a valid variable name or an integer.
%
%   @arg Name The name to check.
%
ok_varname_or_int(Name) :-
    % Check if the name is a valid atom.
    symbol(Name), !, ok_var_name(Name).
ok_varname_or_int(Name) :-
    % Check if the name is a number.
    number(Name).

%!  ok_var_name(?Name) is det.
%
%   Checks if the name is a valid variable name.
%
%   @arg Name The name to validate.
%
quietly_sreader(G):- notrace(G).
ok_var_name(Name):-
    % Ensure the name follows valid Prolog variable naming rules.
 notrace((
  quietly_sreader(( symbol(Name),atom_codes(Name,[C|_List]),char_type(C,prolog_var_start),
      notrace(catch(read_term_from_atom(Name,Term,[variable_names(Vs)]),_,fail)),
      !,var(Term),Vs=[RName=RVAR],!,RVAR==Term,RName==Name)))).

%:- export(ok_codes_in_varname/1).
%ok_codes_in_varname([]).
%ok_codes_in_varname([C|List]):-!,ok_in_varname(C),ok_codes_in_varname(List).

%:- export(ok_in_varname/1).
%ok_in_varname(C):-sym_char(C),\+member(C,`!@#$%^&*?()`).

%% atom_upper( ?A, ?U) is det.
%
% Atom Upper.
%
atom_upper(A,U):-string_upper(A,S),quietly_sreader(((atom_string(U,S)))).


%!  io_to_err(+Goal) is det.
%
%   Redirects the output of the given Goal to the user_error stream.
%   This is used to log information to the error output.
%
%   @arg Goal The goal whose output is redirected to user_error.
%
%   @example Redirect the output of a simple print goal:
%       ?- io_to_err(write('Error message')).
%
io_to_err(Goal):-
    with_output_to(user_error, Goal).

%!  log_progress(+Fmt, +Args) is det.
%
%   Logs progress messages to the terminal with bold black text.
%   The message is formatted according to the given format and arguments.
%
%   @arg Fmt  The format string used by ansi_format/3.
%   @arg Args The arguments to be used in the format string.
:- dynamic(enabled_log_progress/0).
log_progress(_Fmt, _Args):- \+ enabled_log_progress, !.
log_progress(Fmt, Args):-
    ttyflush,
    io_to_err(ansi_format([bold, hfg(black)], Fmt, Args)),
    ttyflush.

%!  log_error(+Fmt, +Args) is det.
%
%   Logs error messages to the terminal with bold red text.
%   The message is formatted according to the given format and arguments.
%
%   @arg Fmt  The format string used by ansi_format/3.
%   @arg Args The arguments to be used in the format string.
%
%   @example Log an error message:
%       ?- log_error('Failed to open file: ~w', ['missing.txt']).
%
log_error(Fmt, Args):-
    ttyflush,
    io_to_err(ansi_format([bold, hfg(red)], Fmt, Args)),
    ttyflush.


% Define the dynamic predicate to store comments and their positions.
:- dynamic(metta_file_comment/5).

%! main_init is det.
%
% Main entry point for the program. It initializes by fetching command line arguments
% and passing them to handle_arguments/1 for further processing.
main_init :-
    current_prolog_flag(os_argv, [_|OsArgV]),  % Retrieve command line arguments.
    ignore(handle_arguments(OsArgV)),           % Pass arguments to the handler.
    sleep(3),!.
    %halt.  % Exit Prolog.

%! handle_files_option(+Flag:atom, +OsArgV:list(atom), :P1) is det.
%
% Proccess content from the input files.
% @arg Flag The stream where output should be sent (stdout or file).
% @arg OsArgV List of input file names.
% @arg P1 Apply P1 to the List of existing input file names in OsArgV that are found after Flag
handle_files_option(Flag,OsArgV,P1):-
    append(_,[Flag|Rest],OsArgV),!,
    forall((member(InputFile, Rest), exists_file(InputFile)), call(P1,InputFile)).

%! handle_arguments(+OsArgV:list(atom)) is det.
%
% Processes the command line arguments to determine the input and output handling.
% It also checks if the '.buffer~' file needs to be regenerated by comparing modification times.
% @arg OsArgV List of command line arguments.
handle_arguments(OsArgV) :-
    handle_files_option('--stdout',OsArgV,show_input_files(current_output)),!.
handle_arguments(OsArgV) :-
    handle_files_option('--regen',OsArgV,gen_tmp_file(true)),!.
handle_arguments(OsArgV) :-
    handle_files_option('--gen',OsArgV,gen_tmp_file(false)),!.

%! show_input_files(+Output:stream, +InputFile:atom) is det.
%
% Outputs the contents of the input file to the specified output stream.
% @arg Output The stream where output should be sent.
% @arg InputFile The name of the input file.
show_input_files(Output, InputFile) :-
    check_input_file(InputFile),  % Check if the input file exists and is readable.
    setup_call_cleanup(
        open(InputFile, read, InStream),              % Open the input file for reading.
        process_expressions(InputFile,InStream, Output),        % Process expressions and write to the output stream.
        close(InStream)                               % Close the input stream upon completion.
    ).

%! handle_input_file(+InputFile:atom) is det.
%
% Handles the regeneration of buffer files for the specified input file.
% @arg InputFile The name of the input file.
gen_tmp_file(Forced, InputFile) :-
    file_name_extension(InputFile, 'buffer~', OutputFile),  % Formulate the output file name.
    check_input_file(InputFile),  % Ensure the input file exists and is readable.
    (  (Forced ; needs_regeneration(InputFile, OutputFile)) ->  % Check if the buffer~ file needs regeneration.
        check_output_file(OutputFile),  % Ensure the output file is writable.
        check_file_size(InputFile, OutputFile),  % Ensure the output file is not empty and is at least 50% the expected size.
        setup_call_cleanup(
            open(OutputFile, write, OutStream),  % Open the output file for writing.
            show_input_files(OutStream, InputFile),  % Process expressions and write to the file.
            close(OutStream)  % Close the output stream upon completion.
        ),
        log_progress('Info: Regenerated: ~w~n', [OutputFile])
    ;   hide_op(log_progress('Info: No need to regenerate: ~w~n', [OutputFile]))
    ).

% skips code when used as a wrapper
hide_op(_).

%! check_input_file(+InputFile:atom) is det.
%
% Verifies that the input file exists and is readable. If not, throws an error.
% @arg InputFile The input file path.
check_input_file(InputFile) :-
    (   exists_file(InputFile) -> true
    ;   log_error('Error: Input file ~w does not exist.~n', [InputFile])
    ),
    (   access_file(InputFile, read) -> true
    ;   log_error('Error: Input file ~w is not readable.~n', [InputFile])
    ).

%! check_output_file(+OutputFile:atom) is det.
%
% Verifies that the output file is writable. If not, throws an error.
% @arg OutputFile The output file path.
check_output_file(OutputFile) :-
    (   exists_file(OutputFile) -> true
    ;   log_progress('Info: Output file ~w does not exist and will be created.~n', [OutputFile])
    ),
    (   access_file(OutputFile, write) -> true
    ;   log_error('Error: Output file ~w is not writable.~n', [OutputFile])
    ).

%! check_file_size(+InputFile:atom, +OutputFile:atom) is det.
%
% Ensures that the output file is at least 50% of the size of the input file.
% If the output file is smaller than this threshold, the file is considered too small.
% @arg InputFile The input file path.
% @arg OutputFile The output file path.
check_file_size(InputFile, OutputFile) :-
    size_file(InputFile, InputSize),  % Get the size of the input file.
    (   exists_file(OutputFile) ->
        size_file(OutputFile, OutputSize),
        MinSize is InputSize // 2,  % Calculate 50% of the input file size.
        (   OutputSize >= MinSize -> true
        ;   log_progress('Warning: Output file ~w is only ~@ bytes (too small), regenerating (>~@ bytes required).~n', [OutputFile, scaled_units(OutputSize), scaled_units(MinSize)])
        )
    ;   true  % Output file does not exist, so it will be created.
    ).

%! needs_regeneration(+InputFile:atom, +OutputFile:atom) is semidet.
%
% Checks if the '.buffer~' file needs to be regenerated by comparing modification times.
% If the input file is newer than the buffer file, returns true.
% @arg InputFile The input file path.
% @arg OutputFile The output buffer file path.
needs_regeneration(InputFile, OutputFile) :-
    exists_file(OutputFile),  % Check if the buffer~ file exists.
    time_file(InputFile, InputTime),  % Get the modification time of the input file.
    time_file(OutputFile, OutputTime),  % Get the modification time of the output file.
    InputTime > OutputTime.  % Regenerate if the input file is newer.

needs_regeneration(InputFile, _OutputFile) :-
    \+ exists_file(InputFile),  % If input file does not exist, fail.
    log_error('Error: Input file ~w does not exist.~n', [InputFile]),
    fail.

:- use_module(library(process)).
:- use_module(library(time)).
:- dynamic ok_to_stop/1.

%! count_lines_in_file(+FileName:atom, -LineCount:int) is det.
%
% Uses the Bash `wc -l` command to count the number of lines in the specified file.
% @arg FileName The name of the file to count lines in.
% @arg LineCount The number of lines in the file.

% First clause: Windows (using a manual line-counting approach).
count_lines_in_file(FileName, LineCount) :- is_win64,  !, % Succeeds if we're on 64-bit Windows
    open(FileName, read, Stream),
    read_count_lines(Stream, 0, LineCount),
    close(Stream).

% Second clause: Unix-like systems (using 'wc -l').
count_lines_in_file(FileName, LineCount) :-
    process_create(path(wc), ['-l', FileName], [stdout(pipe(Out))]),
    read_line_to_string(Out, Result),  % Read the output from the `wc -l` command
    close(Out),  % Close the stream
    split_string(Result, " ", " ", [LineStr|_]),  % Extract the line count
    number_string(LineCount, LineStr).  % Convert the string to an integer

% Helper predicate to read lines from a stream until EOF, incrementing a counter.
read_count_lines(Stream, FinalCount, FinalCount) :- at_end_of_stream(Stream),   !.    % Stop if we've hit the end of the file
read_count_lines(Stream, CurrentCount, FinalCount) :-
    read_line_to_codes(Stream, _),   % Read one line (ignore its content here)
    NextCount is CurrentCount + 1,
    read_count_lines(Stream, NextCount, FinalCount).

%! report_file_progress(+FileName:atom, +InStream:stream, +TotalLines:int, +StartTime:float) is det.
%
% Reports the progress of file processing by calculating the percentage of lines processed every 30 seconds.
% It also estimates the time remaining until completion based on the current processing speed.
% Stops when ok_to_stop(FileName, true) is asserted or when the stream is closed or at the end of the file.
% @arg FileName The input file being translated.
% @arg InStream The input stream being processed.
% @arg TotalLines The total number of lines in the file.
% @arg StartTime The time when the process started.
report_file_progress(FileName, InStream, TotalLines, StartTime) :-
    sleep(10),  % Initial delay before progress reporting starts
    TimeBetweenReports = 15,
    repeat,
    ( stop_reporting(FileName, InStream, TotalLines, StartTime)
      -> log_progress('~t - Stopping reporting on ~w progress.~n', [FileName])
      ; (once(report_progress_so_far(FileName, get_percent_done(InStream, TotalLines), StartTime, TimeLeft)),
         ((number(TimeLeft), HalfTimeLeft is TimeLeft / 2, HalfTimeLeft < TimeBetweenReports, n_max(HalfTimeLeft,2,MinTime)) -> sleep(MinTime) ; sleep(TimeBetweenReports)),  % Sleep for HalfTime seconds between progress reports
         fail)).  % And Continue reporting

n_max(N1,N2,N2):- N1<N2,!.
n_max(N1,_,N1).

%! stop_reporting(+FileName:atom, +InStream:stream, +TotalLines:int, +StartTime:floatf) is semidet.
%
% Determines whether the reporting process should stop, based on whether the stream has ended or errors are detected.
stop_reporting(FileName, InStream, _TotalLines, _StartTime):-
    (   ok_to_stop(FileName, true) ->  % Check if ok_to_stop has been asserted as true
        log_progress('Info: Processing progress: 100% ', [])
    ;   stream_property(InStream, error(true)) ->  % Check if there's a stream error
        log_error('Warning: Stream error.', [])
    ;   (stream_property(InStream, end_of_stream(At)), At \== not) ->  % Check if the stream has reached the end
        log_progress('Info: Stream ~w End-of-Stream.', [At])
    ;   \+ stream_property(InStream, position(_)) ->  % Check if the stream is closed
        log_error('Info: Stream closed.', [])
    ; fail ).  % Continue reporting if none of the above conditions are met



%! remaining_time(+PercentDone: float, +StartTime: float, -RemainingTime: integer) is det.
%
% Calculate the remaining time required to complete a task based on the percentage of the task already completed and the start time.
% This predicate calculates the elapsed time from the start time to the current time, then uses this along with the task completion percentage to compute the remaining time.
%
% @param PercentDone The percentage of the task that has been completed, expressed as a float (e.g., 50.0 for 50%).
% @param StartTime The start time of the task, expressed in epoch seconds.
% @param RemainingTime The computed remaining time to complete the task, also in seconds.
%
% This predicate assumes that PercentDone is a positive value greater than zero. If it is zero or negative, a default remaining time of 60 seconds is returned to avoid division by zero or other meaningless calculations.
%
remaining_time(PercentDone, StartTime, RemainingTime) :-
    PercentDone > 0,   % Ensure that PercentDone is greater than 0 to avoid division by zero
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,  % Calculate the time elapsed since the start
    TotalTime is ElapsedTime / (PercentDone / 100),  % Estimate total time based on current progress
    RemainingTime is TotalTime - ElapsedTime, !.  % Compute remaining time
remaining_time(_, _, 60).  % Return a default remaining time if PercentDone is not greater than 0


%! get_percent_done(+InStream: stream, +TotalLines: int, -Percent: float) is det.
%
% Calculates and logs the percentage of lines processed so far in a stream based on the total number of lines. This predicate not only calculates the percentage but also logs the progress directly.
%
% @param InStream The input stream from which lines are being read.
% @param TotalLines The total number of lines in the stream.
% @param Percent The percentage of lines processed thus far, calculated and used for logging.
%
get_percent_done(InStream, TotalLines, Percent):-
    stream_property(InStream, position(Position)),
    stream_position_data(line_count, Position, CurrentLine),  % Get the current line number being processed
    Percent is (CurrentLine / TotalLines) * 100,  % Calculate the percentage completed
    log_progress('Info: Processing progress:\t ~2f% (Now ~d of ~d lines) ', [Percent, CurrentLine, TotalLines]).


%! report_progress_so_far(+FileName: string, +CalcPercent: predicate, +StartTime: float, -RemainingTime: float) is det.
%
% Reports the progress and the estimated time remaining for processing a file, based on a percentage calculation predicate provided.
%
% @param FileName The name of the file being processed.
% @param CalcPercent A lambda that when called, computes the percentage of the task completed. It should have a signature like `calc_percent_done(-Percent: float)`.
% @param StartTime The start time of the file processing, typically captured using `get_time/1`.
% @param RemainingTime The computed remaining time to complete the task, also in seconds.
%
% This predicate assumes the `CalcPercent` predicate handles all necessary file stream interactions to determine the progress.
%
report_progress_so_far(FileName, CalcPercent, StartTime, RemainingTime):-
    call(CalcPercent, PercentDone),  % Call the provided predicate to calculate the percentage completed
    remaining_time(PercentDone, StartTime, RemainingTime),
    (   number(RemainingTime) ->
        format_time_remaining(RemainingTime, TimeLeft)  % Convert estimated time into a human-readable format
    ;   TimeLeft = 'N/A'  % If no lines have been processed, or an error occurred, time left is unknown
    ),
    % Log the progress and estimated time remaining
    log_progress('\tProcessing file ~w: ~2f% complete. \tEstimated time remaining: ~w', [FileName, PercentDone, TimeLeft]).


%! scaled_units(+Number:int) is det.
%
% Formats a large number into a human-readable form using T (terabytes), G (gigabytes),
% M (megabytes), or K (kilobytes), depending on the magnitude of the number.
% @arg Number The number to format.
scaled_units(Number) :-
    (   Number >= 1_020_000_000_000 ->  % Terabytes (T)
        ScaledNumber is Number / 1_000_000_000_000,
        decimal_units(ScaledNumber, 'T')
    ;   Number >= 1_020_000_000 ->  % Gigabytes (G)
        ScaledNumber is Number / 1_000_000_000,
        decimal_units(ScaledNumber, 'G')
    ;   Number >= 1_020_000 ->  % Megabytes (M)
        ScaledNumber is Number / 1_000_000,
        decimal_units(ScaledNumber, 'M')
    ;   Number >= 1_100 ->  % Kilobytes (K)
        ScaledNumber is Number / 1_000,
        decimal_units(ScaledNumber, 'K')
    ;   % If it's less than 1000, just print the number
        format('~d', [Number])
    ).

%! decimal_units(+ScaledNumber:float, +Unit:atom) is det.
%
% Formats the scaled number based on its magnitude
% - Two decimal places if less than 10.
% - One decimal place if between 10 and 100.
% - No decimal places if greater than or equal to 100.
decimal_units(ScaledNumber, Unit) :-
    (   ScaledNumber < 10 ->
    format('~2f~w', [ScaledNumber, Unit])  % Two decimal places
    ;   ScaledNumber < 100 ->
    format('~1f~w', [ScaledNumber, Unit])  % One decimal places
    ;   % No decimal places
    format('~0f~w', [ScaledNumber, Unit])  % Zero decimal place
    ).

%! format_time_remaining(+Seconds:float, -FormattedTime:atom) is det.
%
% Formats the estimated time remaining (in seconds) into a human-readable format (HH:MM:SS).
% @arg Seconds The estimated time remaining in seconds.
% @arg FormattedTime The formatted time as an atom in the format HH:MM:SS.
format_time_remaining(Seconds, FormattedTime) :-
    Hours is floor(Seconds / 3600),
    Minutes is floor((Seconds - Hours * 3600) / 60),  % Convert seconds to integer minutes
    RemainingSeconds is floor(Seconds - Hours * 3600 - Minutes * 60),  % Remaining seconds after hours and minutes
    format(atom(FormattedTime), '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Minutes, RemainingSeconds]).


%! process_expressions(+FileName:atom, +InStream:stream, +OutStream:stream) is det.
%
% Reads and processes S-expressions and comments from the input stream, writing results with their position to the output stream.
% It also handles multifile declarations and tracks file-related metadata.
%
% @arg FileName The name of the input file being processed.
% @arg InStream The input stream from which to read the S-expressions and comments.
% @arg OutStream The output stream where the processed data and assertions are written.
%
% The process involves reading the stream for S-expressions, comments, and positions,
% then writing those as structured facts to the output. This includes handling multifile
% declarations (`afn_stem_filename/3`, `metta_file_buffer/7`, and `metta_file_comments/5`)
% for modular handling of facts across multiple files.
:- dynamic ok_to_stop/1.

process_expressions(FileName,_InStream, _OutStream) :- atomic(FileName), fail,
    cache_file(FileName, BufferFile),
    exists_file(BufferFile),
    use_cache_file(FileName, BufferFile),
    ensure_loaded(BufferFile), !.

process_expressions(FileName, InStream, OutStream) :-
    % Get total number of lines in the file
    count_lines_in_file(FileName, TotalLines),
    size_file(FileName, InputSize),
    log_progress('~NInfo: File ~w is ~@ bytes (~@ lines)~n', [FileName, scaled_units(InputSize), scaled_units(TotalLines)]),

    % Set the dynamic predicate ok_to_stop/1 to false initially
    assertz(ok_to_stop(FileName, false)),

    % Start a thread to report progress every 30 seconds
    % get_time(StartTime),  % Record the start time
    % thread_create(report_file_progress(FileName, InStream, TotalLines, StartTime), _, [detached(true)]),

    ignore(stream_property(InStream, file_name(Stem))),  % Get the file name of the stream.
    ignore(Stem = FileName),  % Assign the input file name if no stream file name.
    absolute_file_name(Stem, AFNStem),  % Get the absolute path of the file.


   WriteOutput = write_readably(OutStream),

   % Record the absolute file name, file name stem, and the original file name.

   % Declare multifile predicates for storing file-related facts.
   call(WriteOutput, :- multifile(user:afn_stem_filename/3)),
   call(WriteOutput, :- dynamic(user:afn_stem_filename/3)),
   call(WriteOutput, :- dynamic(user:metta_file_buffer/7)),
   call(WriteOutput, :- multifile(user:metta_file_buffer/7)),
   call(WriteOutput,  afn_stem_filename(AFNStem, Stem, FileName)),
    locally(b_setval('$file_src_name', AFNStem),
     locally(b_setval('$file_src_write_readably', WriteOutput),
      locally(b_setval('$file_src_depth', 0),
       setup_call_cleanup(flag('$file_src_ordinal', Was, 0),
        % Start reading and processing expressions from the input stream.
         process_expressions_now(FileName, InStream),
                          flag('$file_src_ordinal', _, Was))))).

:- thread_initialization(nb_setval('$file_src_name',[])).
:- thread_initialization(nb_setval('$file_src_write_readably', [])).
:- thread_initialization(nb_setval('$file_src_depth',0)).

process_expressions_now(FileName, InStream):-
    repeat,
    read_file_sexpr(InStream, Item),  % Read an S-expression or comment from the input stream.
    Item = end_of_file, !,
    % If end of file is reached, stop processing and update the ok_to_stop flag.
    retractall(ok_to_stop(FileName, _)),  % Remove the previous value
    assertz(ok_to_stop(FileName, true)),  % Set ok_to_stop to true to signal the thread to stop
    !.

%! make_DL(+InStream:stream, +OutStream:stream, +FileName:atom, +Item:term) is det.
%
% Creates assertions based on the items read from the stream. If the item is a list, it is
% converted into a Prolog fact, otherwise, it is wrapped into a `do_file_item/5` assertion.
%
% @arg InStream The input stream from which to read position information.
% @arg OutStream The output stream where the assertions are written.
% @arg FileName The name of the file being processed, used as part of the assertion.
% @arg Item The S-expression or term being processed, which will be asserted or wrapped in an assertion.
%
% This predicate is responsible for converting items into assertions or facts:
% - If the item is a list, it is turned into a fact where the functor is derived from the `FileName`.
% - Otherwise, it generates a `do_file_item/5` fact which contains the item's position in the file.
make_DL(_InStream, OutStream, FileName, Item) :-
    is_list(Item), !,
    Assertion =.. [FileName | Item],  % Create a fact with the file name as the functor and list as arguments.
    write_readably(OutStream, Assertion), !.  % Write the fact to the output.

make_DL(InStream, OutStream, FileName, Item) :-
    read_position(InStream, Line, Col, CharPos, _),  % Retrieve the position of the item in the file.
    Assertion = (:- do_file_item(Item, Line, Col, CharPos, FileName)),  % Wrap the item in a `do_file_item/5` fact.
    write_readably(OutStream, Assertion), !.  % Write the assertion to the output.

%! write_readably(+OutStream:stream, +Item:term) is det.
%
% Writes a Prolog term to the output stream in a human-readable form.
% @arg OutStream Stream to which the term is written.
% @arg Item The term to be written.
write_readably(OutStream, Item) :- is_stream(OutStream),!,
    write_term(OutStream, Item, [quoted(true)]),
    writeln(OutStream, '.').  % Append a period and a newline.
write_readably(OutputP1, Item) :- callable(OutputP1),!, ignore(call(OutputP1, Item)).
write_readably(_, _).


%! cont_sexpr(+EndChar:atom, +Stream:stream, -Item) is det.
%
% Reads a single item (S-expression or comment) from the specified stream, handling different formats and encodings.
% Throws an error with stream position if the S-expression cannot be parsed.
% @arg EndChar that denotes the end of a symbol.
% @arg Stream Stream from which to read.
% @arg Item The item read from the stream.
cont_sexpr(EndChar,  Stream, Item):-
   skip_spaces(Stream),  % Ignore whitespace before reading the expression.
   read_line_char(Stream, StartRange),
   cont_sexpr_once(EndChar,  Stream, Item), !,
   read_line_char(Stream, EndRange),
   Range = range(StartRange,EndRange),
   push_item_range(Item, Range).


cont_sexpr_once(EndChar, Stream, Item) :-
    skip_spaces(Stream),
    get_char(Stream, Char),
    cont_sexpr_from_char(EndChar, Stream, Char, Item), !.

% If EOF, return end_of_file
cont_sexpr_from_char(_EndChar, _Stream, end_of_file, end_of_file).

% If '!' followed by '(', '#', or file depth 0, read a directive to be executed
cont_sexpr_from_char(EndChar, Stream, '!', Item) :-
    peek_char(Stream, Next),
    \+ paren_pair_functor(_, Next, _),
    if_t(Next == ' ',  nb_current('$file_src_depth', 0) ),
    once(
       Next == '('
     ; Next == '['
     ; Next == '#'
     ; true
     ; nb_current('$file_src_depth', 0)),

    cont_sexpr_once(EndChar, Stream, Subr), !,
    Item = exec(Subr).

% allow in mettalog the special ` ,(<eval this>) ` form in intepreter
cont_sexpr_from_char(EndChar, Stream, ',', Item) :-
    peek_char(Stream, Next),
    Next == '(',
    %\+ paren_pair_functor(_, Next, _),
    %Next \== ' ',
    cont_sexpr_once(EndChar, Stream, Subr), !,
    Item = exec(Subr).

% If '(', read an S-expression list.
cont_sexpr_from_char(_EndChar, Stream, '(', Item) :-
    read_list(')', Stream, Item).

% If '[', read an S-expression list.
cont_sexpr_from_char(_EndChar, Stream, '[', Item) :- prolog_term_start('['),
    read_list(']', Stream, List),
    univ_list_to_item(List,Item).

% If '{', read an S-expression list.
cont_sexpr_from_char(_EndChar, Stream, '{', Item) :- prolog_term_start('{'),
    read_list('}', Stream, List),
    univ_list_to_item(List,Item).

% If '<', read an S-expression list.
cont_sexpr_from_char(_EndChar, Stream, '<', Item) :- prolog_term_start('<'),
    read_list('>', Stream, List),
    univ_list_to_item(List,Item).

% If '[', '{', etc. - using paren_pair_functor
cont_sexpr_from_char(_EndChar, Stream, Char, Item) :- paren_pair_functor(Char, EndOfParen, Functor),
    read_list(EndOfParen, Stream, It3m),
    Item = [Functor, It3m].

% If '#' followed by '(', read SExpr as Prolog Expression
cont_sexpr_from_char(EndChar, Stream, '#', Item) :- peek_char(Stream, '('),
    cont_sexpr_once(EndChar, Stream, Subr),
    univ_maybe_var(Item, Subr).

% Unexpected end character
cont_sexpr_from_char(EndChar, Stream, Char, Item) :- paren_pair_functor(_, Char, _),
    nb_current('$file_src_depth', 0),
    sformat(Reason, "Unexpected end character: '~w'", [Char]),
    throw_stream_error(Stream, syntax_error(unexpected_char(Char), Reason)),
    % keep going we consumed the Char (if thorw_stream_error/2 permits)
    cont_sexpr(EndChar,  Stream, Item).

% If '"', read a quoted string.
cont_sexpr_from_char(_EndChar, Stream, '"', Item) :-
    read_quoted_string(Stream, '"', Item).

% If '#' followed by '{', read Prolog syntax until '}' and a period
cont_sexpr_from_char(_EndChar, Stream, '#', Item) :- peek_char(Stream, '{'),
    read_prolog_syntax(Stream, Subr),
    Subr = {Item}.

% If '\'', read a quoted symbol.
cont_sexpr_from_char(_EndChar, Stream, '\'', Item) :-
    read_quoted_symbol(Stream, '\'', Item).

% If '`', read a backquoted symbol.
cont_sexpr_from_char(_EndChar, Stream, '`', Item) :-
    read_quoted_symbol(Stream, '`', Item).

% Otherwise, read a symbolic expression.
cont_sexpr_from_char(EndChar, Stream, Char, Item) :-
    read_symbolic(EndChar, Stream, Char, Item).


univ_list_to_item([H|List],Item):- is_list(List),atom(H),!,compound_name_arguments(Item,H,List).
univ_list_to_item([H|List],Item):- is_list(List), \+ atom(H),!,Item=..['holds',H|List].
univ_list_to_item(Else,Item):- prolog_term_start(S), paren_pair_functor(S,_,Functor), !, Item = [Functor,Else].
univ_list_to_item(Else,Item):- Item = ['???',Else].

can_do_level(0).
can_do_level(_).

% #( : user #(load_metta_file &self various_syntaxes.metta) )
univ_maybe_var(Item,[F|Subr]):- is_list(Subr), atom(F), Item =.. [F|Subr],!.
univ_maybe_var('#'(Subr),Subr):- !.

read_prolog_syntax(Stream, Clause) :-
    % Stop if at the end of the stream.
    at_end_of_stream(Stream), !, Clause = end_of_file.
read_prolog_syntax(Stream, Clause) :-
    % Handle errors while reading a clause.
    catch(read_prolog_syntax_unsafe(Stream, Clause), E,
           throw_stream_error(Stream,E)), !.
read_prolog_syntax_unsafe(Stream, Term) :-
    % Set options for reading the clause with metadata.
    Options = [ variable_names(Bindings),
                term_position(Pos),
                subterm_positions(RawLayout),
                syntax_errors(error),
                comments(Comments),
                module(trans_mod)],
    % Read the term with the specified options.
    read_term(Stream, Term, Options),
    (   (fail, Term == end_of_file)
    ->  true
    ;   % Store term position and variable names.
        b_setval('$term_position', Pos),
        nb_setval('$variable_names', Bindings),
        % Display information about the term.
        maplist(star_vars,Bindings),
        nop(display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments))).

star_vars(N=V):- ignore('$VAR'(N) = V).

%!  maybe_name_vars(+List) is det.
%
%   Conditionally sets the variable names if the list is not empty.
%
%   @arg List is the list of variable names.
maybe_name_vars(List):- \+ is_list(List), !.
maybe_name_vars([]):-!.
maybe_name_vars([N=Var|List]):-
    maybe_name_var(N,Var),
    maybe_name_vars(List).

maybe_name_var(_,Var):- nonvar(Var),!.
maybe_name_var(_,Var):- get_attr(Var,vn,_),!.
maybe_name_var(N,Var):-
    svar_fixname(N,NN), % ignore((Var = '$VAR'(NN))))),
    put_attr(Var,vn,NN),!.
maybe_name_var(_N,_Var).

n_to_vn(N,NN):- n_to_vn0(N,NNS),name(NN,NNS).
n_to_vn0(N,NN):- attvar(N),!,get_attr(N,vn,NN),!.
n_to_vn0(N,NN):- var(N),!,sformat(NN,'~p',[N]).
n_to_vn0(N,NN):- integer(N),sformat(NN,'~p',['$VAR'(N)]).
n_to_vn0(N,NN):- number(N),sformat(NN,'~p',['$VAR'(N)]).
n_to_vn0(N,NN):- string(N),!,atom_string(A,N),!,n_to_vn0(A,NN).
n_to_vn0(N,NN):- \+ atom(N),!,sformat(NN,'_~p',[N]).
n_to_vn0('_','_'):-!.
n_to_vn0(N,NN):-atom_concat('$',N1,N),!,sformat(NN,'~w',[N1]).
n_to_vn0(N,NN):-atom_concat('_',N1,N),!,sformat(NN,'_~w',[N1]).
n_to_vn0(N,NN):-!,sformat(NN,'_~w',[N]).

better_typename(TypeName1,TypeName2,array):- var(TypeName1),var(TypeName2),!.
better_typename(TypeName1,TypeName2,TypeName1):- var(TypeName2),!.
better_typename(TypeName1,TypeName2,TypeName2):- var(TypeName1),!.
better_typename(TypeName1,TypeName2,TypeName1):- TypeName2=unknown,!.
better_typename(metta_unknown,TypeName,TypeName).
better_typename(metta_other,TypeName,TypeName).
better_typename(_,TypeName,TypeName).

push_item_range(Item, Range):-
    ignore((
     nb_current('$file_src_depth', Lvl), can_do_level(Lvl),
     subst_vars(Item, Term, [], NamedVarsList),
     flag('$file_src_ordinal',Ordinal,Ordinal),
     Buffer = user:metta_file_buffer(Lvl,Ordinal,TypeNameCompound, Term,  NamedVarsList, Context,Range),
     BufferC= user:metta_file_buffer(Lvl,Ordinal,TypeNameCompound,_TermC,_NamedVarsListC,Context,Range),
     copy_term(Buffer,BufferC),
     ignore(xrefed_outline_type(Term,Outline,TypeName1)),
     ignore((Lvl==0,type_symbol_clause(TypeName2,_Symbol,Term), \+ member(TypeName2,[ref(_)]))),
     better_typename(TypeName1,TypeName2,TypeName),
     ((nonvar(Outline),Outline\=@=Item) -> TypeNameCompound=indexed(TypeName,Outline); TypeNameCompound=TypeName),
        % Assert the parsed content into the Metta buffer part
       ignore((nb_current('$file_src_name', Context), Context \==[], \+ Buffer, assert(BufferC))),
       ignore((nb_current('$file_src_write_readably', P1), P1 \==[], callable(P1), call(P1, BufferC))))),
       !.


metta_caller(Clause, Symbol):- is_definition(decl(_),Symbol,Clause).
metta_callee(Clause, Symbol):- is_definition(ref(_) ,Symbol,Clause).

into_op_head_body(Clause,Op,Head,Body):- var(Clause),!,freeze(into_op_head_body(Clause,Op,Head,Body)).
into_op_head_body(exec(List),Op,Head,Body):- !, into_op_head_body_exec(List,Op,Head,Body).
into_op_head_body('$COMMENT'(List,_,_),none,[],List):- !.
into_op_head_body([Op|List],Op,Head,Body):- nonvar(Op), op_type(import,Op),!,append(Body,[Head],List).
into_op_head_body([Op,Head|Body],Op,Head,Body):- nonvar(Op), op_type(_,Op),!.
into_op_head_body(Head,'=',Head,[]):- is_list(Head).

into_op_head_body_exec([Op|List],Op,Head,Body):- nonvar(Op), op_type(import,Op),!,append(Body,[Head],List).
into_op_head_body_exec([Op,Head|Body],Op,Head,Body):- nonvar(Op), op_type(_,Op),!.
into_op_head_body_exec(Body,[],[],Body).

is_exec(exec(_)).

is_definition(Type,Symbol,Clause):-
   freeze(Type, (is_exec(Clause),compound(Type))),
   freeze(Clause, (is_exec(Clause),compound(Type))),
   into_op_fun_rest_body(Clause,Op,Fun,Rest,Body),
   type_op_head_rest_body(Type,Symbol,Op,Fun,Rest,Body).

type_symbol_clause(Type,Symbol,Clause):-
  clause_type_op_fun_rest_body(Type,Symbol,Clause,_Op,_Fun,_Rest,_Body).

clause_type_op_fun_rest_body(Type,Symbol,Clause,Op,Fun,Rest,Body):-
   ( ( \+ var(Clause)) -> true ; (user:metta_file_buffer(0,_Ord,_Kind, Clause, VL, _Filename, _LineCount),
           ignore(maybe_name_vars(VL)))),
   once(into_op_fun_rest_body(Clause,Op,Fun,Rest,Body)),
   type_op_head_rest_body(Type,Symbol,Op,Fun,Rest,Body).


into_op_fun_rest_body(Clause,Op,Fun,Rest,Body):-
  into_op_head_body(Clause,Op,Head,Body), split_head(Head,Fun,Rest).

split_head([Fun|Rest],Fun,Rest):- is_list(Rest),!.
split_head(Head,Head,[]).

type_op_head_rest_body(decl(import), Symbol, Op,_Head,_Rest, Body):- op_type(import,Op),    sub_symbol(Symbol,Body).
type_op_head_rest_body(decl(use), Symbol, Op,_Head,_Rest, Body):- op_type(import,Op),    sub_symbol(Symbol,Body).
type_op_head_rest_body(ref(a), Symbol, Op, Head,_Rest,_Body):- op_type(import,Op), !, sub_symbol(Symbol,Head).

type_op_head_rest_body(ref(a), Symbol,_Op,_Head, Rest, Body):- not_promiscuous(Symbol),sub_symbol(Symbol,[Body, Rest]).
type_op_head_rest_body(Type,Symbol, Op, Head,_Rest,_Body):- op_type(Type,Op),!,sub_symbol(Symbol,Head).

not_promiscuous(Symbol):- var(Symbol), !, freeze(Symbol,not_promiscuous(Symbol)).
not_promiscuous(Symbol):- number(Symbol),!, fail.
not_promiscuous(Symbol):- \+ promiscuous_symbol(Symbol).

%promiscuous_symbol(+Term) is semidet.
promiscuous_symbol(Term):- \+ atom(Term),!,fail.
promiscuous_symbol('=').
promiscuous_symbol(':').
promiscuous_symbol('->').
%promiscuous_symbol(Atom):- sub_atom(Atom,0,1,After,Sub),(After==0->(!,fail);true),promiscuous_symbol_S(Sub).
promiscuous_symbol(Atom):- atom_concat(_,'=',Atom),!.
promiscuous_symbol(Atom):- atom_concat('@',_,Atom),!.

sub_symbol(Symbol,Head):- ground(Symbol),!,sub_var_safely(Symbol,Head),!.
sub_symbol(Symbol,Head):- \+ var(Symbol), once(sub_term_safely(Symbol,Head)),!.
sub_symbol(Symbol,Head):- sub_term_safely(Symbol,Head),atom(Symbol),!.
sub_symbol(Symbol,Head):- sub_term_safely(Symbol,Head),string(Symbol),!.
sub_symbol(Symbol,Head):- sub_term_safely(Symbol,Head),atomic(Symbol),!.
sub_symbol(Symbol,Head):- sub_term_safely(Symbol,Head),!.

xrefed_outline_type(Val,Val,variable):- is_ftVar(Val),!.
xrefed_outline_type(Val,Val,number):- number(Val),!.
xrefed_outline_type(Val,Val,string):- string(Val),!.
xrefed_outline_type(Val,Val,constant):- symbolic(Val),!.
xrefed_outline_type('$COMMENT'(Cmt,_,_),Cmt,metta_comment):-!.
xrefed_outline_type('exec'([Op|Rest]),'exec'([Op|Rest]),KindNumber):- op_execkind(Op,KindNumber),nonvar(KindNumber),!.
xrefed_outline_type('exec'(Cmt),'exec'(Cmt),metta_other):-!.
xrefed_outline_type([EQ,Outline|_],Outline,metta_defun):- EQ=='=',!.
xrefed_outline_type([CT,Outline|Stuff],[CT,Outline|Stuff],metta_typedecl):- CT==':',!.
xrefed_outline_type([Op|Rest],[Op|Rest],KindNumber):- op_execkind(Op,KindNumber),!.
xrefed_outline_type(Decl,Decl,metta_other):- is_list(Decl),!.
xrefed_outline_type(ELSE,ELSE,metta_unknown):-!.

op_execkind(Op,_):- \+ atomic(Op),!,is_list(Op).
op_execkind(Op,metta_import):- op_type(import,Op),!.
op_execkind(Op,metta_directive):- atom(Op),atom_concat(_,'!',Op),!.
op_execkind(Op,metta_symbol):- atom(Op),atom_concat('&',_,Op),!.

op_type(_,Op):- \+ atom(Op),!,fail.
op_type(import,Op):- import_op(Op).
op_type(decl(use),'bind!'). op_type(decl(use),'pragma!'). op_type(decl(doc),'@doc').
op_type(ref_assert,Op):- atom_concat('assert',_,Op).
op_type(decl(impl),'='). op_type(decl(ftype),':'). op_type(decl(ftype),':<').

import_op(Op):- \+ atom(Op),!,fail.
import_op(Op):- atom_contains(Op,"include").
import_op(Op):- atom_contains(Op,"import").
import_op(Op):- atom_contains(Op,"load").


%! throw_stream_error(+Stream:stream, +Reason:term) is det.
%
% Throws an error including the current stream position for better debugging.
% @arg Stream The input stream.
% @arg Reason The reason for the error.
throw_stream_error(Stream, Reason) :-
    read_position(Stream, Line, Col, CharPos, _),
    throw(stream_error(Line:Col:CharPos, Reason)).

%! read_single_line_comment(+Stream:stream) is det.
%
% Reads a single-line comment from the stream and asserts it with the position.
% A comment starts with ';' and continues to the end of the line.
% @arg Stream The input stream from which to read.
read_single_line_comment(Stream) :-
    % read_char(Stream, ';'),  % Skip the ';' character.
    read_line_char(Stream, line_char(Line1, Col)),
    %succ(Col0, Col1),
    read_line_to_string(Stream, Comment),
    atom_length(Comment,Len), EndCol is Col + Len,
   Range = range(line_char(Line1, Col), line_char(Line1, EndCol)),
   push_item_range('$COMMENT'(Comment, Line1, Col), Range).

%! read_position(+Stream:stream, -Line:integer, -Col:integer, -CharPos:integer) is det.
%
% Reads the current line, column, and character position from the input stream.
% @arg Stream Stream from which to read position.
% @arg Line The current line number.
% @arg Col The current column number.
% @arg CharPos The current character position in the stream.
% @arg Position The current `$position/4` Term of the stream.
read_position(Stream, Line, Col, CharPos, Position) :-
    stream_property(Stream, position(Position)),  % Get the current position from the stream.
    stream_position_data(line_count, Position, Line),  % Extract the line number.
    stream_position_data(line_position, Position, Col),  % Extract the column number.
    stream_position_data(char_count, Position, CharPos).  % Extract the character position.

read_line_char(Stream, line_char(Line0, Col)):-
  read_position(Stream, Line, Col, _, _), succ(Line0, Line).

%! skip_spaces(+Stream:stream) is det.
%
% Skips spaces, single-line comments (starting with `;`), and block comments (between `/*` and `*/`),
% including nested block comments. It continues to read until a non-space, non-comment character is encountered.
%
% @arg Stream The stream from which to read and skip spaces/comments.
skip_spaces(Stream) :-
    peek_char(Stream, Char),
    (   Char = ';' ->
        ( read_single_line_comment(Stream),  % If the character is ';', read a single-line comment.
          skip_spaces(Stream))  % After reading the comment, continue skipping spaces.
    ;   Char = '/' ->
        skip_block_comment(Stream)  % Check if this is the start of a block comment.
    ;   is_like_space(Char) ->
        ( get_char(Stream, _),  % Consume the space character.
          skip_spaces(Stream))  % Continue skipping spaces.
    ;   true  % Non-space, non-comment character found; stop skipping.
    ), !.

%! is_like_space(+Char:char) is semidet.
%
% Checks if a character is a space or similar (e.g., tabs, newlines).
%
% @arg Char The character to check.
is_like_space(Char):- char_type(Char,white),!.
is_like_space(Char):- char_type(Char,end_of_line),!.
is_like_space(Char):- char_type(Char,space),!.
is_like_space(Char):- char_type(Char,cntrl),!.

%! skip_block_comment(+Stream:stream) is det.
%
% Skips over a block comment (starting with `/*` and ending with `*/`), supporting nested block comments.
% The function captures the block comment along with its position and stores it in the database.
%
% @arg Stream The input stream from which to skip the block comment.
skip_block_comment(Stream) :-
    peek_string(Stream, 2, LookAhead),
    (   LookAhead = "/*" ->
        read_block_comment(Stream)  % If we see the block comment start, read and handle it.
    ;   true  % Otherwise, no block comment, continue processing.
    ).


skip_chars(_, N):- N<1,!.
skip_chars(Stream, N):- Nm1 is N -1, get_char(Stream,_), !, skip_chars(Stream, Nm1).


%! read_block_comment(+Stream:stream) is det.
%
% Reads a block comment (including nested block comments) from the stream
% and asserts it with the starting position. A block comment starts with '/*' and
% continues until the closing '*/'.
%
% @arg Stream The input stream from which to read the block comment.
read_block_comment(Stream) :-
    read_line_char(Stream, StartRange),  % Capture the start position.
    %get_string(Stream, 2, _),  % Skip the '/*' characters.
    skip_chars(Stream, 2),
    read_nested_block_comment(Stream, 1, Chars),  % Read the block comment, supporting nested ones.
    string_chars(Comment, Chars),
   read_line_char(Stream, EndRange),   %capture the end pos
   Range = range(StartRange,EndRange),
   StartRange = line_char(Line, Col),
   push_item_range('$COMMENT'(Comment, Line, Col), Range).

%! read_nested_block_comment(+Stream:stream, +Level:int, -Comment:list) is det.
%
% Reads a block comment (including nested block comments) and returns the comment as a list of characters.
% The comment starts with '/*' and continues until the closing '*/', supporting nesting.
%
% @arg Stream The stream from which to read.
% @arg Level  The current level of block comment nesting (initially 1 when called from `read_block_comment`).
% @arg Comment The list of characters read within the block comment.
read_nested_block_comment(Stream, Level, Comment) :-
    read_nested_block_comment(Stream, Level, [], Comment).

read_nested_block_comment(Stream, Level, Acc, Comment) :-
    peek_string(Stream, 2, LookAhead),
    (   LookAhead = "*/" ->
        (   skip_chars(Stream, 2),  % Consume the '*/'.
            NewLevel is Level - 1,  % Decrease the nesting level.
            (   NewLevel = 0 ->
                reverse(Acc, Comment)  % If outermost comment is closed, return the accumulated comment.
            ;   read_nested_block_comment(Stream, NewLevel, ['*', '/' | Acc], Comment)  % Continue, append '*/'.
            )
        )
    ;   LookAhead = "/*" ->
        (   skip_chars(Stream, 2),  % Consume the '/*'.
            NewLevel is Level + 1,  % Increase the nesting level.
            read_nested_block_comment(Stream, NewLevel, ['/', '*' | Acc], Comment)  % Continue, append '/*'.
        )
    ;   (   get_char(Stream, Char),  % Read any other character.
            read_nested_block_comment(Stream, Level, [Char | Acc], Comment)  % Accumulate the character and continue.
        )
    ).


%! read_list(+EndChar:atom, +Stream:stream, -List:list) is det.
%
% Reads a list from the stream until the closing parenthesis is encountered.
% It skips comments while reading the list but asserts them with their positions.
% Throws an error with stream position if the list cannot be parsed correctly.
% @arg Stream Stream from which to read.
% @arg List The list read from the stream.
% @arg EndChar that denotes the end of the list.
read_list(EndChar,  Stream, List):-
  nb_current('$file_src_depth', LvL),
  flag('$file_src_ordinal',Ordinal,Ordinal+1),
  succ(LvL,LvLNext),
  read_position(Stream, Line, Col, CharPos, _),
 setup_call_cleanup(
  nb_setval('$file_src_depth', LvLNext),
  catch(read_list_cont(EndChar,  Stream, List),
        stream_error(_Where,Why),
        throw(stream_error(Line:Col:CharPos,Why))),
  nb_setval('$file_src_depth', LvL)).

read_list_cont(EndChar,  Stream, List) :-
    skip_spaces(Stream),  % Skip any leading spaces before reading.

    peek_char(Stream, Char), !,
    ( chall(EndChar,Char) ->  % Closing parenthesis signals the end of the list.
        get_char(Stream, _),  % Consume the closing parenthesis.
        List = []
    ; Char = end_of_file ->  % Unexpected end of file inside the list.
        throw_stream_error(Stream, syntax_error(unexpected_end_of_file, "Unexpected end of file in list"))
    ; ( cont_sexpr(EndChar,  Stream, Element),  % Read the next S-expression.
        read_list_cont(EndChar,  Stream, Rest),  % Continue reading the rest of the list.
        List = [Element | Rest])  % Add the element to the result list.
    ), !.

%! read_quoted_string(+Stream:stream, +EndChar:atom, -String:atom) is det.
%
% Reads a quoted string from the stream until the corresponding ending quote is found.
% Handles escape sequences within the string.
% Throws an error with stream position if the quoted string cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar that denotes the end of the quoted string.
% @arg String The string read from the stream.
read_quoted_string(Stream, EndChar,  String) :-
    read_until_char(Stream, EndChar,  Chars),  % Read characters until the ending quote.
    string_chars(String, Chars).  % Convert the list of characters to a string.

%! read_quoted_symbol(+Stream:stream, +EndChar:atom, -Symbol:atom) is det.
%
% Reads a quoted symbol from the stream, handling escapes and storing the result as a symbol.
% Throws an error with stream position if the quoted symbol cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar that closes the quoted symbol.
% @arg Symbol The symbol read from the stream.
read_quoted_symbol(Stream, EndChar,  Symbol) :-
    read_until_char(Stream, was_end(EndChar),  Chars),
    ((EndChar == '\'', Chars = [Char])
             -> Symbol='#\\'(Char); atom_chars(Symbol, Chars)).

%! read_until_char(+Stream:stream, +EndChar:atom, -Chars:list) is det.
%
% Reads characters from the stream until the specified end character is encountered.
% This function is used to help read quoted strings and symbols.
% Throws an error with stream position if the end character is not found.
% @arg Stream Stream from which to read.
% @arg EndChar that indicates the end of the reading.
% @arg Chars List of characters read until the end character.
read_until_char(Stream, EndChar,  Chars) :-
    get_char(Stream, Char),
    (   Char = end_of_file -> throw_stream_error(Stream, unexpected_end_of_file(read_until_char(EndChar)))
    ;   chall(EndChar,Char) -> Chars = []
    ;   Char = '\\' -> get_char(Stream, NextChar),
                       maybe_escape(Char, NextChar, CharRead),
                       read_until_char(Stream, EndChar,  RestChars),
                       Chars = [CharRead | RestChars]
    ;   read_until_char(Stream, EndChar,  RestChars),
        Chars = [Char | RestChars]
    ).

chall(Test,Char):- \+ compound(Test),!, Test == Char.
chall(Test,Char):- call(Test,Char),!.
was_end(X,Y):- X==Y.

maybe_escape('\\', 'n', '\n').
maybe_escape('\\', 't', '\t').
maybe_escape('\\', 'r', '\r').
maybe_escape(_Char, NextChar, NextChar).


%! read_symbolic(+EndChar:atom, +Stream:stream, +FirstChar:atom, -Symbolic:atom) is det.
%
% Reads a symbolic expression starting with a specific character, possibly incorporating more complex syntaxes.
% Throws an error with stream position if the symbolic expression cannot be parsed.
% @arg EndChar that indicates the end of the reading unless escaped.
% @arg Stream Stream from which to read.
% @arg FirstChar The first character of the symbolic expression.
% @arg Symbolic The complete symbolic expression read.
read_symbolic(EndChar,  Stream, FirstChar, Symbolic) :-
    read_symbolic_cont(EndChar,  Stream, RestChars),
    classify_and_convert_charseq([FirstChar| RestChars], Symbolic), !.

%! classify_and_convert_charseq(+Chars:list, -Symbolic:term) is det.
%
% Classifies and converts a sequence of characters into a Prolog term,
% handling special cases like variables, numbers, and symbolic terms.
%
% @param Chars    The input list of characters.
% @param Symbolic The resultant Prolog term or symbol, which could be a variable,
%                 number, or an atom.
classify_and_convert_charseq(Chars, Symbolic) :-
    % First, classify and convert the character sequence using the helper predicate.
    classify_and_convert_charseq_(Chars, Symbol),

    % If the classified symbol is an integer, and the original characters contain a '.',
    % convert it to a floating point number.
    ( ( integer(Symbol), memberchk('.', Chars))
      -> Symbolic is Symbol * 1.0   % Convert to floating-point number.
       ; Symbolic = Symbol).        % Otherwise, keep the symbol as is.



%    ast_to_prolog_aux(_,A,O) :- compound(A), A='$VAR'(String),svar_fixvarname(String,UP),O='$VAR'(UP),!.
%    ast_to_prolog_aux(DontStub,[assign,A,E],OO):-  compound(A), A='$VAR'(String),svar_fixvarname(String,UP),String\=UP,O='$VAR'(UP),!,ast_to_prolog_aux(DontStub,[assign,O,E],OO).

%! classify_and_convert_charseq_(+Chars:list, -Symbolic:term) is det.
%
% Helper predicate that attempts to classify the character sequence.
% Handles special cases such as Prolog variables and numbers.
%
% @param Chars    The input list of characters.
% @param Symbolic The resultant Prolog term or symbol.

% Case 1: If the character sequence starts with '$', treat it as a variable.
classify_and_convert_charseq_(['$'| RestChars], '$VAR'(SymbolicVar)) :-
    !,
    atom_chars(Symbolic, RestChars),  % Convert the rest of the characters into a variable name.
    svar_fixvarname(Symbolic,SymbolicVar).

% Case 2: Attempt to interpret the characters as a Prolog term using `read_from_chars/2`.
% This handles more complex syntaxes like numbers, dates, etc.
classify_and_convert_charseq_(Chars, Symbolic) :-
    notrace(catch(read_from_chars(Chars, Symbolic), _, fail)),  % Safely attempt to parse the characters.
    atomic(Symbolic),  % Ensure the result is atomic.
    !.
% Case 3: If no other case applies, convert the characters directly into an atom.
classify_and_convert_charseq_(Chars, Symbolic) :-
    atom_chars(Symbolic, Chars).  % Convert the character sequence into an atom.


%! read_symbolic_cont(+EndChar:atom, +Stream:stream, -Chars:list) is det.
%
% Continues reading symbolic characters from the stream until a delimiter is encountered.
% If a backslash is followed by a delimiter, the delimiter is added as a regular character.
% @arg EndChar that indicates the end of the reading unless escaped.
% @arg Stream Stream from which to read.
% @arg Chars List of characters read, forming part of a symbolic expression.
read_symbolic_cont(EndChar,  Stream, Chars) :-
    peek_char(Stream, NextChar),
    (   is_delimiter(NextChar) -> Chars = []  % Stop when a delimiter is found.
    ;   (chall(EndChar,NextChar)) -> Chars = []  % Stop when an EndChar is found.
    ; ( get_char(Stream, NextChar),
        (   NextChar = '\\' ->  % If it's a backslash, read the next char.
          ( get_char(Stream, EscapedChar),
            read_symbolic_cont(EndChar,  Stream, RestChars),
            Chars = [EscapedChar | RestChars] ) % Add the escaped char normally.
        ; ( read_symbolic_cont(EndChar,  Stream, RestChars),
            Chars = [NextChar | RestChars] ) % Continue reading the symbolic characters.
        ))
    ), !.


%! is_delimiter(+Char:atom) is semidet.
%
% Determines if a character is a delimiter for reading symbolic expressions.
% @arg Char Character to check.
is_delimiter(Char) :-
    char_type(Char, space) ;  % Space is a delimiter.
    arg(_, v( /*'(', ')', */ end_of_file), Char).  % Other delimiters include parentheses and end of file.

% Ensure the program runs upon initialization.
% :- initialization(main_init, main).

