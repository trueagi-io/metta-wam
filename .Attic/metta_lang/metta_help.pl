

:- dynamic(user:predicate_help_hook/5).
:- multifile(user:predicate_help_hook/5).


is_documented(Symbol):- metta_atom(_KB,['@doc',Symbol|_]).

:- dynamic loaded_file_time/2.
symbol_resolve(Term,Resolved):- symbol(Term),symbol_concat(Resolved,'!',Term),!.
symbol_resolve(Term,Resolved):- symbol(Term),symbol_concat(Term,'!',Resolved).
%symbol_resolve(Term,Resolved):- symbol(Term),symbol_concat('metta-',Term,Resolved).


predicate_help_hook(_,_Path,Term,_Arity,_S) :- \+ symbol(Term),!,fail.   
predicate_help_hook(last,Path,Term,Arity,S) :- Path\=recurse(_), \+ is_documented(Term),
   symbol_resolve(Term,Resolved),is_documented(Resolved),!,
   lsp_metta_utils:predicate_help(recurse(Path),Resolved,Arity,S), !.

predicate_help_hook(first,Path,Term,Arity,S):- Path\=recurse(_), \+ is_documented(Term),
   symbol_resolve(Term,Resolved),is_documented(Resolved),!,
   predicate_help_hook(first,recurse(Path),Resolved,Arity,S),!.

predicate_help_hook(last,Path, Term, _Arity, _S) :- atomic(Path),
    exists_file(Path),
    \+ is_documented(Term), 
    
    file_name_extension(_, metta, Path),  fail,
    time_file(Path, Time),  % Get the last modification time
    (   loaded_file_time(Path, Time)
    ->  % If the file was already loaded at the same time, skip reloading
        (format(user_error, 'File "~w" was already loaded at time ~w, skipping reload.~n', [Path, Time]),fail)
    ;   % Otherwise, try to load the file and remember the modification time
    (   asserta(loaded_file_time(Path, Time)),  % Remember that we loaded this file at Time
        try_load_metta_file('&self', Path),
        format(user_error,'File "~w" loaded successfully at time ~w.~n', [Path, Time]),!,
        fail
        %lsp_metta_utils:predicate_help(recurse(Path),Term,Arity,S)
    )).   


predicate_help_hook(first,_Path,Term,Arity,S):- make, 
  with_output_to(string(S),symbol_arity_help(Term,Arity)),
  string(S),S\=="",atom_length(S,Len),Len>1.

predicate_help_hook(last,_Path,Term,Arity,S) :- 
   (get_type(Term,Type)->true;Type=unknownType),
   format(string(Str),"*~w*: ~w (~w)",[Type,Term,Arity]),
   S = _{
    contents: _{
      kind: 'markdown',
      value: Str
      }
  }.


symbol_arity_help(Term,_Arity):- eval(['help!',Term],_).


try_load_metta_file(Self, Path):-
  with_option(exec, 'skip', 
     load_metta(Self, Path)).
