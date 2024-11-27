%! metta_called_at(+Path:atom, +Term:term, -By:term, -Location:term) is nondet.
%  Find the callers and locations of the goal =Term=, starting from
%  the file =Path=. =Location= will be bound to all the callers and
%  locations that the =Term= is called from like =Caller-Location=.
:- include(lsp_metta_include).

% textDocument/references: returns a list of specific locations where the symbol is referenced or called from. Moreover, it includes the results from textDocument/implementation (which itself includes textDocument/definition and textDocument/declaration), providing a comprehensive overview of the symbol's usage across the codebase.
metta_called_at(Path, Term, By, Location) :-
  matta_name_callable(Term, Callable),
  xref_metta_source(Path),
  user:metta_file_buffer(0,_Ord,_Kind,CallerLine, _VL, Path, Location),
  metta_callee(CallerLine, Callable),
  metta_caller(CallerLine, By).



xref_metta_defined(Path, Target, Ref):-
  xref_metta_defined(Type, Target, Path, Ref), Type\=ref(_).

xref_metta_defined(Type, Target, Path, Ref):-
  xref_metta_defined(Type, Target, _Term, Path, Ref).

xref_metta_defined(Type, Target, Term, Path, PosStart):-
   type_expand(Type,RefType),
   user:metta_file_buffer(0,_Ord,_Kind,Term,VL, Path, PosStart),
  ignore(maybe_name_vars(VL)),
  once(type_symbol_clause(ExpTypeO,Target,Term)),ExpTypeO=RefType.

type_expand(Var,Var):- var(Var),!.
type_expand(definition,RefType):- member(RefType, [decl(_)]).
type_expand(declaration,RefType):- member(RefType, [decl(use)]).
type_expand(references,RefType):- member(RefType, [ref(_)]).
type_expand(typeDefinition,RefType):- member(RefType, [decl(ftype)]).
type_expand(implementation,RefType):- member(RefType, [decl(_),decl(use)]).

% textDocument/declaration: returns the specific location of the symbol's type declaration, which can include its function definition, symbol definition, etc. Since only one location can be returned, the system chooses the most relevant type declaration for the symbol.
% textDocument/implementation: returns a list of specific locations where the symbol is implemented. Additionally, it includes the locations returned by both textDocument/definition and textDocument/declaration, showing the full picture of where the symbol is implemented and its type associations.
% textDocument/definition: returns the specific location in the document or file where the symbol is defined or documented. It points to the exact spot where the symbol is introduced in the code.
type_defined_at(Type, HintPath, NameArity, Location):-  metta_defined_at(Type, HintPath, NameArity, Location).
type_defined_at(_Type, HintPath, NameArity, Location):- prolog_file_or_var(HintPath), prolog_defined_at(HintPath, NameArity, Location).

prolog_file_or_var(HintPath):- var(HintPath),!.
prolog_file_or_var(HintPath):- atomic(HintPath), file_name_extension(_,Ext,HintPath), Ext == pl. % TODO we might use transpiled outputs like .datalog

% Arity 4
metta_defined_at(Type, HintPath, NameArity, Location):- metta_defined_at(Type, HintPath, NameArity, _, Location).

% Arity 5
%metta_defined_at(RefType, HintPath, Target, Term, Location):- Target=Name/Arity, nonvar(Name),!,metta_defined_at(RefType, HintPath, Name, Term, Location).
%metta_defined_at(RefType, HintPath, Target, Term, Location):- nonvar(HintPath),!, xref_metta_source(HintPath), metta_defined_at(RefType, HintPath, Target, Term, Location).
metta_defined_at(RefType, HintPath, NameArity, Term, Location):-
  xref_metta_source(HintPath),
  matta_name_callable(NameArity, Target),
  each_type_at_sorted(Target, Sort),!,
  assertion(var(Path)), % remember Path is a free variable on purpose (HintPath is a hint about where the predicate was found by the user.. not where it was defined or referenced)
  member(each_type_at(Target,Term, Path, Ref, Type), Sort),
  once(type_expand(RefType,Type)),
  path_doc(Path, Doc),
  once(metta_relative_ref_location(Doc, Term, Ref, Location)).

/*
metta_defined_at(Type, HintPath, Callable, Term, Location) :-
  %matta_name_callable(NameArity, Callable),
  xref_metta_source(HintPath),
  xref_metta_defined(Type, HintPath, Callable, Term, Path, Ref),
  path_doc(Path, Doc),
  once(metta_relative_ref_location(Doc, Callable, Ref, Location)).
*/

%! matta_name_callable(?Name:functor, ?Callable:term) is det.
%  True when, if Name = Func/Arity, Callable = Func(_, _, ...) with
%  =Arity= args.
matta_name_callable(Name/_, Callable) :- nonvar(Name), !, matta_name_callable(Name, Callable).
matta_name_callable(Name/Arity, Callable) :- integer(Arity),!,
  length(FakeArgs, Arity),
  (Callable = [Name|FakeArgs];
   Callable =.. [Name|FakeArgs]).
matta_name_callable(Name, Name).
%matta_name_callable(Name, [Name|_]):- atom(Name).

%! metta_relative_ref_location(+Path:atom, +Goal:term, +Position:position(int, int), -Location:dict) is semidet.
%  Given =Goal= found in =Path= and position =Position= (from
%  metta_called_at/3), =Location= is a dictionary suitable for sending as an
%  LSP response indicating the position in a file of =Goal=.
metta_relative_ref_location(_, _, Dict, Location) :- is_dict(Dict),!, Location=Dict.
metta_relative_ref_location(Here, Goal, '$stream_position'(A,B,C,D), Out):- !, line_col('$stream_position'(A,B,C,D), line_char(Line0,Char0)),
   metta_relative_ref_location(Here, Goal,  position(Line0, Char0), Out).
metta_relative_ref_location(Here, _, position(Line0, Char0),
            _{uri: Here, range: _{start: _{line: Line0, character: Char0},
                      end: _{line: Line1, character: 0}}}) :-
  !, succl(Line0, Line1).
metta_relative_ref_location(Here, _, line_char(Line0, Char0),
            _{uri: Here, range: _{start: _{line: Line0, character: Char0},
                      end: _{line: Line1, character: 0}}}) :-
  !, succl(Line0, Line1).
metta_relative_ref_location(Here, _, range(line_char(Line0,Char0),line_char(Line1,Char1)),
            _{uri: Here, range: _{start: _{line: Line0, character: Char0},
                      end: _{line: Line1, character: Char1}}}) :- !.

metta_relative_ref_location(Here, _, local(Line1),
            _{uri: Here, range: _{start: _{line: Line0, character: 1},
                      end: _{line: NextLine, character: 0}}}) :-
  !, succl(Line0, Line1), succl(Line1, NextLine).
metta_relative_ref_location(_, Goal, imported(Path), Location) :-
  path_doc(Path, ThereUri),
  xref_metta_source(Path),
  xref_metta_defined(Path, Goal, Loc),
  metta_relative_ref_location(ThereUri, Goal, Loc, Location).


