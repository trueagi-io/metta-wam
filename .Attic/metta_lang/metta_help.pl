

:- dynamic(user:predicate_help_fallback_hook/4).
:- multifile(user:predicate_help_fallback_hook/4).


is_documented(Symbol):- metta_atom(_KB,['@doc',Symbol|_]).

symbol_resolve(Term,Resolved):- symbol(Term),symbol_concat(Resolved,'!',Term).
symbol_resolve(Term,Resolved):- symbol(Term),symbol_concat('metta-',Term,Resolved).

user:predicate_help_fallback_hook(Path,Term,Arity,S) :- Path\=recurse(_), \+ is_documented(Term),
   symbol_resolve(Term,Resolved),is_documented(Resolved),
   lsp_metta_utils:predicate_help(recurse(Path),Resolved,Arity,S), !.

user:predicate_help_fallback_hook(Path,Term,Arity,S) :- make,
   (get_type(Term,Type)->true;Type=unknownType),
   format(string(Str),"*~w*: ~w (~w)",[Type,Term,Arity]),
   S = _{
    contents: _{
      kind: 'markdown',
      value: Str
      }
  }.



