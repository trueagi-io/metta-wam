/*
Dict = _{
  capabilities: _{
    textDocumentSync: _{
      openClose: true,
      change: 2,  % Full sync (incremental changes)
      willSave: true,
      willSaveWaitUntil: true,
      save: _{ includeText: true }
    },
    hoverProvider: true,
    completionProvider: _{
      resolveProvider: true,
      triggerCharacters: [".", ":", "(", "<"]
    },
    signatureHelpProvider: _{
      triggerCharacters: ["(", ","],
      retriggerCharacters: [")", ","]
    },
    definitionProvider: true,
    typeDefinitionProvider: true,
    implementationProvider: true,
    declarationProvider: true,
    referencesProvider: true,
    documentHighlightProvider: true,
    documentSymbolProvider: _{
      labelSupport: true,
      hierarchicalDocumentSymbolSupport: true
    },
    workspaceSymbolProvider: true,
    codeActionProvider: _{
      codeActionKinds: [
        "quickfix", "refactor", "refactor.extract", "refactor.inline",
        "refactor.rewrite", "source.organizeImports", "source.fixAll"
      ],
      resolveProvider: true
    },
    codeLensProvider: _{ resolveProvider: true },
    documentFormattingProvider: true,
    documentRangeFormattingProvider: true,
    documentOnTypeFormattingProvider: _{
      firstTriggerCharacter: ";",
      moreTriggerCharacter: ["}", "\n"]
    },
    renameProvider: _{ prepareProvider: true },
    foldingRangeProvider: _{
      rangeLimit: 5000,
      lineFoldingOnly: true
    },
    executeCommandProvider: _{
      commands: ["command.id", "other.custom.command"]
    },
    workspace: _{
      workspaceFolders: true,
      applyEdit: true,
      didChangeWatchedFiles: _{ dynamicRegistration: true },
      configuration: true,
      symbol: _{
        symbolKind: _{
          valueSet: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]
        }
      }
    },
    semanticTokensProvider: _{
      legend: _{
        tokenTypes: [
          "namespace", "class", "enum", "interface", "struct", "typeParameter",
          "type", "parameter", "variable", "property", "enumMember", "event",
          "function", "method", "macro", "keyword", "modifier", "comment",
          "string", "number", "regexp", "operator"
        ],
        tokenModifiers: [
          "declaration", "definition", "readonly", "static", "deprecated", 
          "abstract", "async", "modification", "documentation", "defaultLibrary"
        ]
      },
      full: _{ delta: true },
      range: true
    },
    monikerProvider: true,
    documentLinkProvider: _{ resolveProvider: true },
    colorProvider: true,
    inlayHintProvider: _{ resolveProvider: true },
    callHierarchyProvider: true,
    typeHierarchyProvider: true,
    linkedEditingRangeProvider: true,
    selectionRangeProvider: true,
    diagnosticProvider: _{
      interFileDependencies: true,
      workspaceDiagnostics: true
    },
    notebookDocumentSync: _{
      notebookSelector: [
        _{
          notebook: _{
            type: "jupyter-notebook",
            scheme: "file"
          },
          cells: [_{ language: "python" }]
        }
      ]
    },
    window: _{
      workDoneProgress: true,
      showMessage: _{
        messageActionItem: _{ additionalPropertiesSupport: true }
      },
      showDocument: _{ support: true }
    },
    experimental: _{ experimentalFeature: true }  % Placeholder for any experimental features
  }
}.

*/

%:- module(lsp_metta_outline, [
%                        xref_document_symbols/2,
%                        xref_mettalog/1]).

%!  metta_atom_xref(+Atom) is semidet.
%
%   Attempts to cross-reference a given atom from a Metta buffer or already
%   asserted facts. This is used for finding known atoms.
%
%   @arg Atom The atom to be cross-referenced.
:- dynamic(metta_atom_asserted/2).
:- multifile(metta_atom_asserted/2).
:- dynamic(metta_atom_asserted_deduced/2).
:- multifile(metta_atom_asserted_deduced/2).

metta_atom_xref(Atom):- metta_atom_xref(Atom, _Path, _Loc).

metta_atom_xref(Atom, Path, Loc):- 
    metta_file_buffer(+, Atom, NamedVarsList, Path, Loc),  % Retrieve the atom from a Metta file buffer.
    \+ clause(metta_atom_asserted(_, Atom), true),  % Ensure the atom has not been asserted already.
   ignore(maybe_name_vars(NamedVarsList)).  % Set variable names based on the named variables list.
metta_atom_xref(Atom, Path, Loc):- 
    clause(metta_atom_asserted(_, Atom), true),  % Check if the atom has been asserted in the knowledge base.
    copy_term(Atom,Copy),
    ignore(metta_file_buffer(+, Atom, NamedVarsList, Path, Loc)),
    Atom =@= Copy,
    ignore(maybe_name_vars(NamedVarsList)).

%!  maybe_name_vars(+List) is det.
%
%   Conditionally sets the variable names if the list is not empty.
%
%   @arg List is the list of variable names.
maybe_name_vars(List):- \+ is_list(List), !.
maybe_name_vars([]):-!.
maybe_name_vars([N=Var|List]):-
    ignore((n_to_vn(N,NN),Var = '$VAR'(NN))),
    maybe_name_vars(List).
n_to_vn(N,NN):- var(N),!,sformat(NN,'~p',[N]).
n_to_vn(N,NN):- number(N),sformat(NN,'~p',['$VAR'(N)]).
n_to_vn(N,NN):- \+ atom(N),!,sformat(NN,'~p',[N]).
n_to_vn('_','_'):-!.
n_to_vn(N,NN):-atom_concat('$',N1,N),!,sformat(NN,'~w',[N1]).
n_to_vn(N,NN):-atom_concat('_',N1,N),!,sformat(NN,'~w',[N1]).
n_to_vn(N,NN):-!,sformat(NN,'~w',[N]).


%!  predicate_help_hook(+HookType, +Path, +Term, +Arity, -S) is semidet.
%
%   Provides help documentation for predicates in the form of hooks. If the predicate 
%   is a symbol and is documented, it retrieves the help text.
%
%   @arg HookType  The stage of hook invocation (e.g., first, last).
%   @arg Path      The file or path associated with the predicate.
%   @arg Term      The term (predicate) for which help is sought.
%   @arg Arity     The arity of the predicate.
%   @arg S         The help content to return.
:- multifile(user:predicate_help_hook/5).
:- dynamic(user:predicate_help_hook/5).
predicate_help_hook(_, _, Term, _, _) :- 
    \+ symbol(Term),  % Fail if the term is not a valid symbol.
    !, fail.
predicate_help_hook(first, Path, Term, Arity, S):-     
    Path \= recurse(_),  % Avoid recursion for already processed paths.
    \+ is_documented(Term),  % Check if the term is undocumented.
    symbol_resolve(Term, Resolved),  % Try to resolve the symbol to a documented one.
    is_documented(Resolved),  % Ensure the resolved symbol is documented.
    predicate_help_hook(first, recurse(Path), Resolved, Arity, S),  % Recurse to retrieve help for the resolved symbol.
    !.
predicate_help_hook(last, Path, Term, Arity, S):- 
    Path \= recurse(_),  % Avoid recursion for already processed paths.
    \+ is_documented(Term),  % Check if the term is undocumented.
    symbol_resolve(Term, Resolved),  % Try to resolve the symbol to a documented one.
    is_documented(Resolved),  % Ensure the resolved symbol is documented.
    lsp_metta_utils:predicate_help(recurse(Path), Resolved, Arity, S),  % Retrieve help content for the resolved symbol.
    !.
predicate_help_hook(first, Path, _Term, _Arity, _S):- 
    atomic(Path),  % Ensure the path is an atomic value (not a variable or complex term).
    make,  % Trigger the make command to compile the knowledge base. (For real-time editing)
    xref_mettalog(Path),  % Possibly cross-reference the file.
    fail.

predicate_help_hook(first, _, Term, Arity, S):-     
    all_info_string(Term, Arity, Str),  % Generate a term definition string based on its arity.
    Str \= "",  % Ensure the definition string is not empty.
    atom_length(Str, Len), 
    Len > 10,  % Ensure the definition string has a minimum length.    
    maybe_markdown(Str, S).

predicate_help_hook(last, _, Term, Arity, S):- 
   (get_type(Term, Type) -> true; Type = unknownType),  % Get the type of the term or default to 'unknownType'.
    all_info_string(Term, Arity, DS),  % Generate a term definition string.
    format(string(Str), "*Type ~w*: ~w (~w)~n~w", [Type, Term, Arity, DS]),  % Format the output as a help string.
    maybe_markdown(Str, S),!.


maybe_markdown(Str, S):- 
    S = _{ contents: _{
            kind: 'markdown',  % Define the help content format as markdown text.
            value: Str  % Store the formatted help string.
        } }, !.


%!  is_documented(+Symbol) is semidet.
%
%   Checks if a given symbol is documented by querying for an '@doc' tag in the Metta
%   knowledge base. This predicate succeeds if documentation is found.
%
%   @arg Symbol The symbol to check for documentation.
is_documented(Symbol):- 
    nonvar(Symbol),  % Ensure that the symbol is instantiated (not a variable).
    metta_atom_xref(['@doc', Symbol | _]).  % Look for a documentation entry in the knowledge base.
is_documented(Symbol):- 
    metta_atom_xref([_, Head | _]),  % Search for a matching atom in the knowledge base.
    sub_var(Symbol, Head).  % Check if the symbol appears as a subterm of Atom.

%!  is_documented_arity(+Symbol, +Arity) is semidet.
%
%   Checks if a given symbol is documented for a specific arity by inspecting its
%   parameter list or type definition in the knowledge base.
%
%   @arg Symbol The symbol to check for arity-based documentation.
%   @arg Arity  The arity (number of arguments) to check for.
is_documented_arity(Symbol, Arity):- 
    nonvar(Symbol),  % Ensure that the symbol is instantiated.
    metta_atom(_KB, ['@doc', Symbol | Docs]),  % Retrieve documentation for the symbol.
    once((member(List, Docs),  % Find a '@params' entry in the documentation.
          is_list(List), List = [Sub, Params], Sub == '@params',  % Check if the entry is a parameter list.
          is_list(Params), length(Params, Arity))).  % Verify if the arity matches the length of the parameter list.
is_documented_arity(Symbol, Arity):- 
    nonvar(Symbol),  % Ensure that the symbol is instantiated.
    get_operator_typedef1('&xref', Symbol, Arity, ParamTypes, _RetType),  % Retrieve the operator's type definition.
    is_list(ParamTypes),  % Ensure that the parameter types are in a list.
    length(ParamTypes, Arity).  % Verify if the arity matches the length of the parameter types.


%!  symbol_resolve(+Term, -Resolved) is semidet.
%
%   Attempts to resolve a symbol to its counterpart with or without an exclamation mark (!).
%
%   @arg Term The term (symbol) to resolve.
%   @arg Resolved The resolved version of the term (with or without '!').
symbol_resolve(Term, Resolved):- 
    symbol(Term),  % Ensure the term is a valid symbol.
    symbol_concat(Resolved, '!', Term), !.  % Try to resolve the symbol by removing the '!' if present.
symbol_resolve(Term, Resolved):- 
    symbol(Term),  % Ensure the term is a valid symbol.
    symbol_concat(Term, '!', Resolved).  % Try to resolve the symbol by adding a '!' if missing.

%!  all_info_string(+Term, +Arity, -S) is det.
%
%   Generates a formatted string describing the term and its arity.
%
%   @arg Term  The term (predicate) for which the description is generated.
%   @arg Arity The arity of the predicate.
%   @arg S     The resulting string description.
all_info_string(Term, Arity, Str):- 
    with_output_to(string(S), grovel_all_info(Term, Arity)),  % Generate a string output for the term's arity help.
    string(S),  % Ensure that the output is a valid string.
    S \= "",  % Ensure that the string is not empty.
    atom_length(S, Len), 
    Len > 1, % Ensure the string has a minimum length.
    format(string(Str), "```lisp~n~w~n```", [S]). 

%!  grovel_all_info(+Term, +Arity) is det.
%
%   Outputs help information for a term and its associated arity.
%
%   @arg Term  The term (predicate) for which help is displayed.
%   @arg Arity The arity to check for.
grovel_all_info(Target, Arity):- 
    forall(grovel_some_help(Target, Arity), true).  % Execute the help logic for each possible instance of the term's arity.

banner_for(Type,Target):- format('~N```~n---~n ## ~w: ~w ~n```lisp~n',[Type, Target]).
lsp_separator():- format('~N```~n---~n```lisp~n',[]).



%!  grovel_some_help(+Target, +Arity) is det.
%
%   Helper predicate to display information about each instance of a term's arity.
%
%   @arg Target The term (predicate) for which help is displayed.
%   @arg Arity The arity to check for.
grovel_some_help(Target, _) :- fail, % (for debugging) commenting out fail will let the hover show the cross-ref index
  each_type_at_sorted(Target, Sort),
  forall(member(RefType,[definition,declaration,typeDefinition,implementation,references]),
  (banner_for(RefType,Target),
    %ignore((defined_at(Type, HintPath, Target, Clause,Path,Loc),
    forall(member(each_type_at(Target,Type,Clause,Path,Loc),Sort),
      ignore((
          once(type_expand(RefType,Type)),
          write_src_xref(Clause,Type,Path,Loc),nl))))),
   banner_for('rest-of',Target).

grovel_some_help(Target, _) :- 
    xref_call(eval(['help!', Target], _)), lsp_separator().  % Evaluate the help command for the term.
grovel_some_help(Target, Arity):- number(Arity), Arity > 1,
    findall(A, is_documented_arity(Target, A), ArityDoc),  % Retrieve documented arities for the term.
    ArityDoc \== [],  % Ensure the documentation is not empty.
    \+ memberchk(Arity, ArityDoc),  % Verify if the term's arity DOES NOT matches the documented arity.
    
    format('Arity expected: ~w vs ~w~n', [ArityDoc, Arity]),lsp_separator() .  % Output a message if there's an arity mismatch.
grovel_some_help(Target, _) :- 
    each_type_at_sorted(Target, Type,Clause,Path,Loc),
    format('~@', [write_src_xref(Clause,Type,Path,Loc)]).  % Write the source cross-reference for the atom.
   
%xref_call(G):- catch(G,E,debug(server(high), "xref_call ~w", [G])).
%xref_call(G):- catch(with_no_debug(G),E,debug(server(high), "xref_call ~w", [G->E])).
xref_call(G):- with_no_debug(G).
%xref_call(G):- call(G). 

each_type_at_sorted(Target,Type,Clause,Path,Loc):-
    each_type_at_sorted(Target, Sort),
    member(each_type_at(Target,Type,Clause,Path,Loc),Sort).

each_type_at_sorted(Target, Sort):-
      findall(each_type_at(Target,Type,Clause,Path,Loc),
              each_type_at(Target,Type,Clause,Path,Loc),
              List), 
       sort(List,Sort).

each_type_at(Target,Type,Clause,Path,Loc):-
    no_repeats_var(ClauseV),    
    metta_atom_xref(Clause, Path, Loc), ClauseV = Clause,  % Cross-reference the term with known atoms.
    about_term(Clause, Target),  % Determine if the atom is related to the term.
    \+ skip_xref_atom(Clause),  % Skip atoms that are not relevant for cross-referencing.
    type_symbol_clause(Type,Target,Clause).
    %format('~@', [write_src_xref(Clause,Type,Path,Loc)]).

%!  about_term(+Atom, +Term) is semidet.
%
%   Determines if the Atom is about the given Term.
%
%   @arg Atom The atom to check.
%   @arg Term The term to match with the atom.
about_term([Op, Atom | _], Term):- fail, Op==':', is_documented(Term), !, Atom\==Term, % Dont reshow types
    sub_var(Term, Atom),  !. % Check if the term is a subterm of the atom.
about_term([_,[Atom|_]|_],Term):- sub_var(Term,Atom),!.
about_term([_,Atom|_],Term):- ==(Term,Atom),!.
about_term([Atom|_],Term):- \+ promiscuous_symbol(Term), sub_var(Term,Atom),!.
about_term(exec(Atom),Term):-!, sub_var(Term, Atom).
%promiscuous_symbol(+Term) is semidet.
promiscuous_symbol(Term):- \+ atom(Term),!,fail.
promiscuous_symbol('=').
promiscuous_symbol(':').
promiscuous_symbol('->').
%promiscuous_symbol(Atom):- sub_atom(Atom,0,1,After,Sub),(After==0->(!,fail);true),promiscuous_symbol_S(Sub).
promiscuous_symbol(Atom):- atom_concat(_,'=',Atom),!.
promiscuous_symbol(Atom):- atom_concat('@',_,Atom),!.

:- multifile(user:handle_msg_hook/3).
:- dynamic(user:handle_msg_hook/3).
% Save the last Msg.body Object for each method
user:handle_msg_hook(MethodStr, MsgBody, _) :- fail,
   atom_string(Method,MethodStr),
   nb_setval(Method, MsgBody),
   fail.

%!  write_src_xref(+Src) is det.
%
%   Outputs source code or its reference based on the nesting of the source.
%
%   @arg Src The source code or reference to output.
write_src_xref_oneloc(Src):-
  write_src_xref(Src),
  maybe_link_xref(Src).

write_src_xref(Clause,Type,Path,Loc):-
   catch_skip((write_src_xref(Clause),
     ignore(write_file_link(Type,Path,Loc)))).

write_src_xref(Clause,Path,Loc):-
   catch_skip((write_src_xref(Clause),
     ignore(write_file_link(Path,Loc)))). 

catch_skip(G):- ignore(catch(G,_,true)).

write_src_xref(Src):- % fail, 
    very_nested_src(Src), !,  % Check if the source is complex.
    wots(S, pp_sexi_l(Src)), write(S).  % Write the full source content if it's complex.
write_src_xref(Src):- 
    write_src_woi(Src).  % Otherwise, write the source content without additional information.
% Check for deeply nested lists
very_nested_src([_, _ | Src]):- is_list(Src), 
    member(M, Src), is_list(M), 
    member(E, M), is_list(E), 
    member(I, E), is_list(I), !.  
maybe_link_xref(What):- 
  ignore(once((
     metta_file_buffer(_,Atom,_,Path,Pos),
     %symbolic(Path), \+ symbol_contains(Path,'stdlib_mettalog'),
     once((alpha_unify(What,Atom); \+ (What \= Atom))),
     %next_clause(Ref, metta_file_buffer(_,_,_,Path,Pos)),     
     write_file_link(Path,Pos)))).

% next_clause(Ref, NextClause)
%   - Ref is the reference of the current clause
%   - NextClause is the next clause's reference, if it exists
next_clause(Ref, NextClause) :-
     nth_clause(Pred, Nth, Ref),
     NextIndex is Nth + 1,
     nth_clause(Pred, NextIndex, NextRef),!,
     clause(NextClause, _, NextRef).  % Get the clause at this reference

%     ~n```~n*~w*~n```lisp~n
write_file_link(Type, Path,Position):-   
  stream_position_data(line_count, Position, Line),  % Extract the line number.
  %stream_position_data(line_position, Position, Col),  % Extract the column number.
  %stream_position_data(char_count, Position, CharPos),  % Extract the character position.
  position_line(Position, Line),
  format('~n```~n[~w:~w](file://~w#L~w) _(~w)_~n```lisp~n',[Path,Line,Path,Line,Type]).
write_file_link(Path,Position):-   
  stream_position_data(line_count, Position, Line),  % Extract the line number.
  %stream_position_data(line_position, Position, Col),  % Extract the column number.
  %stream_position_data(char_count, Position, CharPos),  % Extract the character position.
  position_line(Position, Line),
  format('~n```~n[~w:~w](file://~w#L~w)~n```lisp~n',[Path,Line,Path,Line]).

position_line(Position, Line):- compound(Position),compound_name_arity(Position,'$stream_position',4),!,stream_position_data(line_count, Position, Line).
position_line(_{uri: _Here, range: _{start: _{line: Line0, character: _},
                                            end: _{line: _, character: _}}}, Line1):- number(Line0), succ(Line0,Line1), !.
position_line(_{ range: _{start: _{line: Line0, character: _}, end: _{line: _, character: _}}}, Line1):- number(Line0), succ(Line0,Line1), !.
position_line(  _{line: Line0, character: _}, Line1 ):- number(Line0),  succ(Line0,Line1), !.
%position_line(Position, Line):- stream_position_data(line_count, Position, Line), !.
position_line( P, L):- sub_term(L,P),number(P),!.
position_line( L, L).
  

%!  skip_xref_atom(+Atom) is semidet.
%
%   Checks whether the given Atom should be skipped during cross-referencing.
%
%   @arg Atom The atom to check.

%skip_xref_atom(Atom):- sub_var('@doc', Atom), !. % Skip documentation (already shown)
%skip_xref_atom(exec(_)).  % Skip execution commands.
skip_xref_atom([Pred | _]):- 
    symbol(Pred),  % Check if the predicate is a symbol.
    symbol_concat('assert', _, Pred).  % Skip 'assert*' predicates.

%! xref_maybe(+Path, +FullText) is det.
%
%   Checks if a file's content has changed. If it has, interrupts any ongoing processing
%   of that file and requeues it for reprocessing. If not, processing continues without
%   interruption.
%
%   @arg Path The file path of the Metta file.
%   @arg NewText The current text content to compare.

xref_maybe(Path, NewText) :- user:next_text(Path, OldText), OldText = NewText, !,
    debug(server(high), 'NewText for "~w" has not changed, skipping reload.~n', [Path]).
xref_maybe(Path, NewText) :- user:full_text(Path, OldText), OldText = NewText, !,
    debug(server(high), 'FullText for "~w" has not changed, skipping reload.~n', [Path]).
xref_maybe(Path, NewText) :- retractall(user:full_text(Path, _)), assertz(user:full_text(Path, NewText)),!.
xref_maybe(Path, NewText) :-
    debug(server(high), 'Text for "~w" has changed, reprocessing buffer.~n', [Path]),
    retractall(user:full_text(Path, _)),
    xref_source_expired(Path),
    asserta(user:next_text(Path, NewText)),
    xref_enqueue_file(Path).

xref_source_dir(Dirs) :- exists_directory(Dirs), enumerate_directory_files(Dirs, FileList), !, maplist(xref_mettalog, FileList).

%!  xref_mettalog(+Path) is semidet.
%
%   Checks if a Metta file needs to be reprocessed by comparing its current content 
%   with the last recorded content, ensuring that at least 20 seconds have passed since 
%   the last check.
%
%   @arg Path The file path of the Metta file.
xref_mettalog(Path) :- ignore(xref_source_path(Path)).

% Check if the file or directory should be processed
xref_source_path(Doc) :- var(Doc),!.
xref_source_path(Doc) :- maybe_doc_path(Doc, Path), !, xref_source_path(Path).
xref_source_path(Path):- \+ check_time_elapsed(Path), !, debug(server(xrefTime), 'Skipping check for "~w" as 20 seconds have not passed.~n', [Path]), !.
xref_source_path(Path):- \+ file_name_extension(_, metta, Path),!.  % Ensure the file has a .metta extension.
xref_source_path(Path) :- xref_enqueue_file(Path).

:- dynamic
   xref_file_state/2,
   xref_thread_control/1,
   xref_file_queue/1.

% Main predicate to submit a file and ensure it's processed
xref_submit_and_wait(File) :- xref_file_state(File, done), !.
xref_submit_and_wait(File) :-
    xref_enqueue_file(File),
    xref_wait_for_file(File).

disable_thread_system.

% Send an interrupt to the worker thread
xref_interrupt_worker(_File) :- disable_thread_system, !.
xref_interrupt_worker(File) :-
    ignore((xref_update_file_state(File, processing),
            xref_thread_control(ThreadID),
            thread_signal(ThreadID, throw(interrupted)))).

% Wait for a specific file to be processed completely
xref_wait_for_file(File) :-
    repeat,
    xref_file_state(File, State),
    (State \== submitted, State \== processing),
    sleep(0.1),
    fail.
xref_wait_for_file(File) :-
    xref_file_state(File, State),
    debug(server(xref), "File ~w has been processed and is now ~w.~n", [File, State]).


% Predicate to recursively enumerate files in directories, resolving symlinks.
enumerate_directory_files(Dirs, FileList) :-
    (is_list(Dirs) -> ProcessDirs = Dirs ; ProcessDirs = [Dirs]),
    findall(File,
            (member(Dir, ProcessDirs),
             absolute_file_name(Dir, AbsDir, [file_type(directory), access(read), solutions(all), file_errors(fail)]),
             enumerate_files_in_directory(AbsDir, File)),
            Files),
    list_to_set(Files, FileList).

% Recursive predicate to enumerate files within a directory
enumerate_files_in_directory(Path, Files) :-
    exists_directory(Path), !,
    directory_files(Path, Entries),
    findall(File,
            (member(Entry, Entries),
             \+ member(Entry, ['.', '..']),
             absolute_file_name(Entry, EntryPath, [relative_to(Path), solutions(all), file_errors(fail)]),
             (exists_file(EntryPath) -> File = EntryPath ; exists_directory(EntryPath), enumerate_files_in_directory(EntryPath, File))),
            Files).
enumerate_files_in_directory(_, []).  % Handle non-directory paths by returning an empty list

% xref_enqueue_file(+File) is det.
%
%   Adds a file to the processing queue and ensures the worker thread is running.
xref_enqueue_file(File) :- xref_file_queue(File),!.
xref_enqueue_file(File) :- made_metta_file_buffer(File),!.
xref_enqueue_file(Path):- disable_thread_system, !,
      get_current_text(Path, NewText),  % Get the current content of the file.
      compare_and_update_string(Path, NewText),  % Compare with the last stored content.
      call(debug_buffer_info).  % Save the current state.
xref_enqueue_file(File) :-
    xref_ensure_worker_thread_running,
    xref_update_file_state(File, submitted),
    ( xref_file_queue(File) ->
        true  % File is already in the queue; do nothing
    ;   assertz(xref_file_queue(File))
    ).


%!  check_time_elapsed(+Path) is semidet.
%
%   Checks if at least 20 seconds have passed since the last file check for the given path.
%
%   @arg Path The file path to check.
:- dynamic(last_check_time/2).

check_time_elapsed(Path) :-
    get_time(CurrentTime),  % Get the current system time.
    (   last_check_time(Path, LastCheck),  % Retrieve the last check time for the file.
        ElapsedTime is CurrentTime - LastCheck,  % Calculate the time difference.
        ElapsedTime > 20  % Ensure at least 20 seconds have passed.
    ->  retractall(last_check_time(Path, _)),  % Remove the old time entry.
        asserta(last_check_time(Path, CurrentTime))  % Update with the new time.
    ;   \+ last_check_time(Path, _),  % If no previous time is recorded, store the current time.
        asserta(last_check_time(Path, CurrentTime))
    ).

%!  get_current_text(+Path, -NewText) is det.
%
%   Retrieves the current content of a file either from fallback documentation 
%   or by reading the file itself.
%
%   @arg Path    The file path to read.
%   @arg NewText The text content of the file.
get_current_text(Path, NewText) :-
    (   source_file_text(Path, NewText)  % Attempt to get the file's content as text.
    *->  true
    ;   NewText = ""  % Default to an empty string if the file cannot be read.
    ).

%!  compare_and_update_string(+Path, +NewText) is det.
%
%   Compares the current file content with the last recorded content. If different, 
%   the buffer is reprocessed.
%
%   @arg Path    The file path.
%   @arg NewText The current text content to compare.
:- dynamic(last_retrieved_string/2).
compare_and_update_string(Path, NewText) :-
    (   last_retrieved_string(Path, OldText),  % Retrieve the last known content.
        OldText \= NewText  % Check if the content has changed.
    -> (debug(server(xref), 'Text for "~w" has changed, reprocessing buffer.~n', [Path]),  % Log the change.
        retractall(last_retrieved_string(Path, _)),  % Remove the old content entry.
        asserta(last_retrieved_string(Path, NewText)),  % Update with the new content.
        xref_source_expired(Path),
        xref_metta_file_text('&xref', Path, NewText))   % Reprocess the file with the new content.
    ;   (debug(server(xref), 'Text for "~w" has not changed, skipping reload.~n', [Path]),  % Log if no change is detected.
        xref_metta_file_text('&xref', Path, NewText))  % Still cross-reference the file for consistency.
    ).

   
   
   
    
% Update the processing state of a file
xref_update_file_state(File, State) :-
    retractall(xref_file_state(File, _)),
    assertz(xref_file_state(File, State)).

% Worker thread for processing files
xref_ensure_worker_thread_running() :-
    ( xref_thread_control(ThreadID) ->
        ( thread_property(ThreadID, status(running)) ->
            true  % The thread is running; do nothing
        ;   % Else: Thread is not running
            ( retractall(xref_thread_control(ThreadID)),
              thread_create(xref_process_files, NewThreadID, []),
              assertz(xref_thread_control(NewThreadID))
            )
        )
    ;   % Else: No thread ID stored
        ( thread_create(xref_process_files, ThreadID, []),
          assertz(xref_thread_control(ThreadID))
        )
    ).

% Worker thread that processes files from the queue
xref_process_files :-
    repeat,
    (xref_file_queue(File) ->
        retract(xref_file_queue(File)),
        once(xref_handle_file(File)),
        fail  % Continue processing files
    ; sleep(0.1),
      fail
    ).

% Handle individual files and catch interruptions
xref_handle_file(File) :-
    catch((debug(server(xref), "Processing file: ~w~n", [File]),
           setup_call_cleanup(xref_update_file_state(File, processing),
                              xref_source_now(File),
                              xref_update_file_state(File, done)),
           debug(server(xref), "Processing complete: ~w~n", [File])),
          interrupted,
          (debug(server(xref), "Processing of file ~w was interrupted, resuming...~n", [File]),
           xref_update_file_state(File, interrupted))).

xref_source_now(Path) :-
    source_file_text(Path, NewText),
    xref_metta_file_text('&xref',Path, NewText),
    call(debug_buffer_info).  % Save the current state

xref_source_expired(Path):- var(Path),!.
xref_source_expired(Doc):- maybe_doc_path(Doc,Path),!,xref_source_expired(Path).
xref_source_expired(Path):-
   doc_path(Doc,Path),
   retractall(made_metta_file_buffer(Doc)),
   retractall(gave_document_symbols(Doc,_)),
   retractall(made_metta_file_buffer(Path)),
   retractall(gave_document_symbols(Path,_)),
  %retractall(metta_file_buffer(_Mode, _Term, _NamedVarsList, Path, _Pos)),
   xref_interrupt_worker(Path).

    

:- debug(server(xref)).

%!  debug_buffer_info is det.
%
%   Saves the current state of the Metta file buffer to 'last_file.txt'.
debug_buffer_info:- 
    ignore((debugging(server(xref)),
            open('last_file.txt', write, Stream),
            with_output_to(Stream, listing(metta_file_buffer)),
            close(Stream))).


%!  xref_metta_file_text(+Self, +Path, +Text) is det.
%
%   Cross-references a Metta file's content and processes it in the correct context.
%
%   @arg Self The context (usually '&self') for the cross-reference.
%   @arg Path The file path to cross-reference.
%   @arg Text The content of the file as text.
:- dynamic(made_metta_file_buffer/1).
xref_metta_file_text(_Self, Path, _Text) :- made_metta_file_buffer(Path),!.
xref_metta_file_text(_Self, Path, _Text) :- metta_file_buffer(_Mode, _Term, _NamedVarsList, Path, _Pos), !.
xref_metta_file_text(Self, Path, Text):- fail, % (var(Text); Text==""),!,
    nop(debug(server(high), "xref_metta_file_text ~w", [Text])), 
    absolute_file_name(Path, Filename),  % Convert the file path to an absolute path.
    directory_file_path(Directory, _, Filename),  % Extract the directory path from the file path.
    debug(server(xref), "xref_metta_file_text ~w", [Path]),  % Log the cross-referencing process.
    with_option(exec, skip,  % Use options for processing in the correct context.
  locally(nb_setval(may_use_fast_buffer,t),
   locally(nb_setval(suspend_answers,true),
     with_output_to(string(_),
       include_metta_directory_file(Self,Directory,Filename))))),!.
%   This variant of `xref_metta_file_text/3` handles file inclusion by converting a 
%   file path to an absolute path, opening the file, and processing its content in 
%   the correct working directory context. It handles both string content and file 
%   reading.
xref_metta_file_text(_Self, Path, Text) :-
    asserta(made_metta_file_buffer(Path)),!,
    debug(server(xref), "xref_metta_file_text ~w", [Path]),  % Log the file path being processed.
    must_det_ll((
        % Convert the file path to an absolute path
        absolute_file_name(Path, Filename),
        % Extract the directory path from the absolute file name
        directory_file_path(Directory, _, Filename),
        % Process the file content in the specified directory context
       locally(nb_setval(may_use_fast_buffer,t),
         with_option(exec, skip,
          with_option(suspend_answers, true,
            with_cwd(Directory,
                setup_call_cleanup(
                        % Open the file or string content
                    (atomic(Text)->open_string(Text,In);open(Path, read, In, [encoding(utf8)])),
                        % Process the buffer content from the file
                    must_det_ll(xref_metta_file_text_buffer(false, Path, In)),
                        % Ensure the file is closed even in case of errors
                        catch(close(In), _, true)))))))).  

%!  xref_metta_file_text_buffer(+TFMakeFile, +Filename, +In) is det.
%
%   Processes the content of a Metta file buffer, optionally writing it to a buffer file.
%   The predicate reads lines from the input stream `In`, and asserts the parsed content 
%   into the buffer. It optionally writes the buffer content to an output file for later use.
%
%   @arg TFMakeFile  If true, also create a buffer file.
%   @arg Filename    The name of the file being processed.
%   @arg In          The input stream (either from the file or string).
xref_metta_file_text_buffer(TFMakeFile, Filename, In) :-
    (if_t(TFMakeFile,
            % Create a buffer file if needed
        (symbol_concat(Filename, '.buffer~', BufferFile),
        fbugio(creating(BufferFile)),
        write_bf(BufferFile, (:- dynamic(metta_file_buffer/5))),
        write_bf(BufferFile, (:- multifile(metta_file_buffer/5)))))),
    repeat,    
    % Count the current line in the input stream
        
    %debug(server(xref), "Pos ~w", [Pos]),  % Log the current line number.
    % Get the current mode for reading the file
    current_read_mode(file, Mode),
    % Read and parse the content of the Metta file
    skip_spaces(In),
    forall(retract(metta_file_comment(_Line, _Col, _CharPos, '$COMMENT'(Comment, CLine, CCol), CPos)),
          assertz(metta_file_buffer(+, '$COMMENT'(Comment, CLine, CCol), [], Filename, CPos))),
    stream_property(In,position(Pos)),
    read_sexpr(In, Expr),
    subst_vars(Expr, Term, [], NamedVarsList),
    % Assert the parsed content into the Metta buffer
    BufferTerm = metta_file_buffer(Mode, Term, NamedVarsList, Filename, Pos),
    %ignore(maybe_process_directives(Mode, Term)),
    assertz(BufferTerm),
    % debug(server(xref), "BufferTerm ~w", [BufferTerm]),  % Log the parsed buffer term.
    % Optionally write the buffer content to the buffer file
    if_t(TFMakeFile, write_bf(BufferFile, BufferTerm)),
    % flush_output,  % Ensure all output is flushed.
    at_end_of_stream(In),  % Stop processing once the end of the stream is reached.
    !.


maybe_process_directives(+, exec([Op|List])):-
  op_execkind(Op,import),
  last(List,Path),!,
  absolute_file_name(Path, File, [access(read), extensions(['','.metta'])]),
  xref_mettalog(File).


%!  source_file_text(+Path, -String) is det.
%
%   Retrieves the content of the specified file or fallback text, ensuring the content
%   is read as a string. It first tries to obtain the text using `doc_text_fallback/2`,
%   and if that fails, it attempts to read the file directly.
%
%   @arg Path   The file path from which the content is to be read.
%   @arg String The file content represented as a string.
%
%   @example Example usage:
%       ?- source_file_text('file.txt', Text).
%       Text = "File content here".
%
:- dynamic(user:next_text/2).
:- dynamic(user:full_text/2).
source_file_text(Doc, FullText) :- maybe_doc_path(Doc,Path), !, source_file_text(Path, FullText).
source_file_text(Path, FullText) :- user:next_text(Path, FullText),!.
source_file_text(Path, FullText) :- user:full_text(Path, FullText),!.
source_file_text(Path, String) :-
    % Tries to retrieve text using doc_text_fallback/2 first.
    findall(Str, 
        (lsp_metta_changes:doc_text_fallback(Path, D4s),
         sub_term(d(_, Str, _, _), D4s), 
         string(Str)), 
    List),
    % Combines the fallback text into a single string.
    atomics_to_string(List, String), !.

source_file_text(Path, String) :-
    % If no fallback text was found, check if the file exists.
    exists_file(Path),
    % Read the file content as a string with UTF-8 encoding.
    read_file_to_string(Path, String, [encoding(utf8)]), !.

:- dynamic(gave_document_symbols/2).
:- retractall(gave_document_symbols(_, _)). % when we reload this file
xref_document_symbols(Doc, Symbols):- gave_document_symbols(Doc, Symbols),!.
xref_document_symbols(Doc, Symbols):- %   sample_outline_test(SS),
    xref_mettalog(Doc),    
    maybe_doc_path(Doc,Path),!,
%xref_submit_and_wait(Path),
    findall(
         Symbol,
         ( xref_document_symbol(Path, Outline, KindNumber, Start:SC, End:EC),
           Symbol = _{name: Outline,
                      kind: KindNumber, 
                      location:
                      _{uri: Doc,
                        range: _{start: _{line: Start, character: SC},
                                 end: _{line: End, character: EC}}}}
         ),
         Symbols),
    retractall(gave_document_symbols(Doc, _)),
    asserta(gave_document_symbols(Doc, Symbols)).


doc_path(Doc,Path):- atomic(Doc),atom_concat('file://', Path, Doc),!.
doc_path(Doc,Path):- atomic(Path), assertion( \+ atom_concat('file://', _, Path)), atom_concat('file://', Path, Doc),!.
doc_path(Doc,Path):- nonvar(Doc),!,Path=Doc.
doc_path(Doc,_Path):- frozen(Doc,Ice),Ice=freeze(_,_),!.
doc_path(Doc,Path):- freeze(Path,atom_concat('file://', Path, Doc)).

doc_uri(Doc,DocUri):- nonvar(Doc), atom_concat('file://', _, Doc), !, DocUri = Doc. 
doc_uri(Doc,DocUri):- nonvar(Doc), atom_concat('file://', Doc, DocUri).

maybe_doc_path(Doc,Path):- atomic(Doc),atom_concat('file://', Path, Doc),!.

xref_document_symbol(Doc,  Outline, KindNumber, Start, End):- maybe_doc_path(Doc,Path),!,xref_document_symbol(Path, Outline, KindNumber, Start, End).
xref_document_symbol(Path, Path, 1, 0:0, 1000:0).
xref_document_symbol(Path, Outline, KindNumber, Start, End):- xref_document_symbol_d4(Path, Outline, KindNumber, Start, End), fail.
xref_document_symbol(Path, Outline, KindNumber, Start, End):- xref_document_symbol_fb(Path, Outline, KindNumber, Start, End).
%xref_document_symbol(Path, Outline, KindNumber, Start, End):- xref_document_symbol_examples(Path, Outline, KindNumber, Start, End).


% for Iconagraphy
xref_document_symbol_examples(_Path, "By Type...", 1, 1000:0, 10000:0).
xref_document_symbol_examples(_Path, Outline, KindNumber, Start:1, End:0):- show_example_kinds,
  lsp_xref_kind(KindNumber, KindName), KindNumber>1,
  Start is KindNumber*10+1000,End is Start+9,
  nonvar(KindName),
  atom_concat('Example ',KindName,KindExample), toPropercase(KindExample,Outline).

% Roy's `d/4`s
xref_document_symbol_d4(Doc, PrettyString, KindNumber, Start, End):- 
   doc_path(Doc,Path),lsp_metta_changes:doc_text(Path,D4s), 
   nth1(Nth,D4s,D4), nonvar(D4), 
   d4_document_symbol(Nth,D4, PrettyString, KindNumber, Start, End).

d4_document_symbol(Nth, d(_,Str,_,_), S, 12, Nth:1, End:1):- succ(Nth,End), outline_name(Str,S).
   
% Douglas' file_buffer
xref_document_symbol_fb(Doc, PrettyString, KindNumber, Start, End):- 
   doc_path(Doc,Path),
   clause(metta_file_buffer(_,What,VL,Path,PosStart),true,Ref), line_col(PosStart,Start),
   ignore(maybe_name_vars(VL)),
   once(((xrefed_outline_type_kind(What,Outline,KindName),outline_name(Outline,PrettyString),lsp_xref_kind(KindNumber, KindName)))),   
   once(((next_clause(Ref, metta_file_buffer(_,_,_,Path,PosEnd)), line_col(PosEnd,End)))-> true ; next_line(Start,End)).


outline_name(Str,S):- string(Str),!,atom_length(Str,Len),Len>2,!,S=Str.
outline_name(Str,S):- is_ftVar(Str),wots(M, write_src_woi(Str)),!,outline_name(M,S).
outline_name(Str,S):- is_list(Str), wots(M, write_src_woi(Str)),!,outline_name(M,S).
outline_name(Str,S):- Str = exec(_),wots(M, write_src_woi(Str)),!,outline_name(M,S).
outline_name(Str,S):- sformat(S,'~w',[Str]),atom_length(S,Len),Len>5.

next_line(S:SC,E:SC):- number(S),!,succ(S,E).
next_line(S,E):- number(S),!,succ(S,E).

line_col(Position,LineM1:Col):-
     stream_position_data(line_count, Position, Line),  % Extract the line number.
     LineM1 is Line-1,
     stream_position_data(line_position, Position, Col).  % Extract the column number.

xrefed_outline_type_kind(What,Outline,KindName):-
   xrefed_outline_type(What,Outline,TypeName),
   type_kind(TypeName,KindName),!.

xrefed_outline_type('$COMMENT'(Cmt,_,_),Cmt,metta_comment):-!.
xrefed_outline_type('exec'([Op|Rest]),'exec'([Op|Rest]),KindNumber):- op_execkind(Op,KindNumber),!.
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
op_type(var,'bind!'). op_type(var,'pragma!'). op_type(decl(doc),'@doc').
op_type(assert,Op):- atom_concat(assert,_,Op). 
op_type(decl(=),'='). op_type(decl(type),':'). op_type(decl(type),':<').

import_op(Op):- \+ atom(Op),!,fail.
import_op(Op):- atom_contains(Op,"include").
import_op(Op):- atom_contains(Op,"import").
import_op(Op):- atom_contains(Op,"load").


type_kind(Var,WillBe):- var(Var),!,freeze(Var,type_kind(Var,WillBe)).
type_kind(metta_import,number).
type_kind(metta_symbol,key).
type_kind(metta_directive,constant).
type_kind(metta_comment,string).
type_kind(metta_typedecl,typeParameter).
type_kind(metta_defun,function).
type_kind(metta_exec,class).
type_kind(metta_other,interface).
type_kind(Was,Keep):- clause(lsp_xref_kind(_,Was),true),!,Keep=Was.
type_kind(_,array).

lsp_xref_kind(N, LU):- number(LU),var(N),!,LU=N.
lsp_xref_kind(1, file).
lsp_xref_kind(2, module).
lsp_xref_kind(3, namespace).
lsp_xref_kind(4, package).
lsp_xref_kind(5, class).
lsp_xref_kind(6, method).
lsp_xref_kind(7, property).
lsp_xref_kind(8, field).
lsp_xref_kind(9, constructor).
lsp_xref_kind(10, enum).
lsp_xref_kind(11, interface).
lsp_xref_kind(12, function).
lsp_xref_kind(13, variable).
lsp_xref_kind(14, constant).
lsp_xref_kind(15, string).
lsp_xref_kind(16, number).
lsp_xref_kind(17, boolean).
lsp_xref_kind(18, array).
lsp_xref_kind(19, object).
lsp_xref_kind(20, key).
lsp_xref_kind(21, null).
lsp_xref_kind(22, enumMember).
lsp_xref_kind(23, struct).
lsp_xref_kind(24, event).
lsp_xref_kind(25, operator).
lsp_xref_kind(26, typeParameter).
lsp_xref_kind(26, Nonvar):- nonvar(Nonvar).

%

sample_outline_test(DocumentSymbol):-
 DocumentSymbol = _{
  name: "MyClass",
  detail: "class definition",
  kind: 5, % Corresponds to 'Class'
  location: _{
    uri: "file:///path/to/MyClassFile.pl",
    range: _{
      start: _{line: 0, character: 1},
      end: _{line: 10, character: 0}
    }
  },
  children: [
    _{
      name: "myMethod",
      detail: "method definition",
      kind: 6, % Corresponds to 'Method'
      location: _{
        uri: "file:///path/to/MyClassFile.pl",
        range: _{
          start: _{line: 2, character: 1},
          end: _{line: 4, character: 0}
        }
      },
      children: [
        _{
          name: "write('Hello, world!')",
          detail: "write statement",
          kind: 12, % Corresponds to 'Statement'
          location: _{
            uri: "file:///path/to/MyClassFile.pl",
            range: _{
              start: _{line: 2, character: 14},
              end: _{line: 2, character: 30}
            }
          }
        },
        _{
          name: "nl",
          detail: "newline statement",
          kind: 12, % Corresponds to 'Statement'
          location: _{
            uri: "file:///path/to/MyClassFile.pl",
            range: _{
              start: _{line: 2, character: 32},
              end: _{line: 2, character: 34}
            }
          }
        }
      ]
    },
    _{
      name: "myField",
      detail: "",
      kind: 8, % Corresponds to 'Field'
      location: _{
        uri: "file:///path/to/MyClassFile.pl",
        range: _{
          start: _{line: 6, character: 1},
          end: _{line: 6, character: 0}
        }
      }
    }
  ]
}.



nop_mod
:- module(lsp_metta_references, 
                     [called_at/4,
                      defined_at/3,
                      name_callable/2,
                      relative_ref_location/4,
                      help_at_position/4,
                      clause_in_file_at_position/3,
                      clause_variable_positions/3,
                      seek_to_line/2,
                      linechar_offset/3
                     ]).
/** <module> LSP Utils

Module with a bunch of helper predicates for looking through prolog
source and stuff.

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/3, exclude/3]).
%:- use_module(library(prolog_xref)).
%:- use_module(library(prolog_source), [read_source_term_at_location/3]).
%:- use_module(library(help), [help_html/3, help_objects/3]).
:- use_module(library(lynx/html_text), [html_text/1]).
:- use_module(library(solution_sequences), [distinct/2]).
:- use_module(library(lists), [append/3, member/2, selectchk/4]).
:- use_module(library(sgml), [load_html/3]).

%! called_at(+Path:atom, +Clause:term, -By:term, -Location:term) is nondet.
%  Find the callers and locations of the goal =Clause=, starting from
%  the file =Path=. =Location= will be bound to all the callers and
%  locations that the =Clause= is called from like =Caller-Location=.

% textDocument/references: returns a list of specific locations where the symbol is referenced or called from. Moreover, it includes the results from textDocument/implementation (which itself includes textDocument/definition and textDocument/declaration), providing a comprehensive overview of the symbol's usage across the codebase.
called_at(Path, Clause, By, Location) :-
    name_callable(Clause, Callable),
    xref_mettalog(Path),
    metta_line_buffer(_,CallerLine, _VL, Path, Location),
    metta_callee(CallerLine, Callable),
    metta_caller(CallerLine, By).

metta_caller(Clause, Symbol):- is_definition(decl(_),Symbol,Clause).
metta_callee(Clause, Symbol):- is_definition(ref ,Symbol,Clause).

into_op_head_body(Clause,Op,Head,Body):- var(Clause),!,freeze(into_op_head_body(Clause,Op,Head,Body)).
into_op_head_body(exec(List),Op,Head,Body):- !, into_op_head_body_exec(List,Op,Head,Body).
into_op_head_body('$COMMENT'(List,_,_),none,[],List):- !.
into_op_head_body([Op|List],Op,Head,Body):- nonvar(Op), op_type(import,Op),!,append(Body,[Head],List).
into_op_head_body([Op,Head|Body],Op,Head,Body):- nonvar(Op), op_type(_,Op),!.
into_op_head_body(Head,'=',Head,[]).

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
   ( ( \+ var(Clause)) -> true ; (metta_file_buffer(_, Clause, VL, _Filename, _LineCount),ignore(maybe_name_vars(VL)))),
   once(into_op_fun_rest_body(Clause,Op,Fun,Rest,Body)),
   type_op_head_rest_body(Type,Symbol,Op,Fun,Rest,Body).
   

into_op_fun_rest_body(Clause,Op,Fun,Rest,Body):- 
  into_op_head_body(Clause,Op,Head,Body), split_head(Head,Fun,Rest).

split_head([Fun|Rest],Fun,Rest):- is_list(Rest),!.
split_head(Head,Head,[]).

type_op_head_rest_body(var, Symbol, Op,_Head,_Rest, Body):- op_type(import,Op),    sub_symbol(Symbol,Body).
type_op_head_rest_body(ref, Symbol, Op, Head,_Rest,_Body):- op_type(import,Op), !, sub_symbol(Symbol,Head).

type_op_head_rest_body(ref, Symbol,_Op,_Head, Rest, Body):- not_promiscuous(Symbol),sub_symbol(Symbol,[Body, Rest]).
type_op_head_rest_body(Type,Symbol, Op, Head,_Rest,_Body):- op_type(Type,Op),!,sub_symbol(Symbol,Head).

not_promiscuous(Symbol):- var(Symbol), !, freeze(Symbol,not_promiscuous(Symbol)).
not_promiscuous(Symbol):- number(Symbol),!, fail.
not_promiscuous(Symbol):- \+ promiscuous_symbol(Symbol).

sub_symbol(Symbol,Head):- ground(Symbol),!,sub_var(Symbol,Head),!.
sub_symbol(Symbol,Head):- \+ var(Symbol), once(sub_term(Symbol,Head)),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),atom(Symbol),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),string(Symbol),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),atomic(Symbol),!.
sub_symbol(Symbol,Head):- sub_term(Symbol,Head),!.

xref_defined(Path, Target, Ref):-
  xref_defined(Type, Target, Path, Ref), Type\==ref.

xref_defined(Type, Target, Path, Ref):- 
  xref_defined(Type, Target, _Clause, Path, Ref).

xref_defined(Type, Target, Clause, Path, PosStart):- 
   type_expand(Type,RefType),
   metta_file_buffer(_,Clause,VL, Path, PosStart),
    ignore(maybe_name_vars(VL)), 
    once(type_symbol_clause(ExpTypeO,Target,Clause)),ExpTypeO=RefType.

type_expand(Var,Var):- var(Var),!.
type_expand(definition,RefType):- member(RefType, [decl(_)]).
type_expand(declaration,RefType):- member(RefType, [var]).
type_expand(references,RefType):- member(RefType, [ref]).
type_expand(typeDefinition,RefType):- member(RefType, [decl(type)]).
type_expand(implementation,RefType):- member(RefType, [decl(_),var]).

% textDocument/declaration: returns the specific location of the symbol's type declaration, which can include its function definition, symbol definition, etc. Since only one location can be returned, the system chooses the most relevant type declaration for the symbol.
% textDocument/implementation: returns a list of specific locations where the symbol is implemented. Additionally, it includes the locations returned by both textDocument/definition and textDocument/declaration, showing the full picture of where the symbol is implemented and its type associations.
% textDocument/definition: returns the specific location in the document or file where the symbol is defined or documented. It points to the exact spot where the symbol is introduced in the code.
defined_at(Type, HintPath, NameArity, Location):- defined_at(Type, HintPath, NameArity, _, Location).

%defined_at(RefType, HintPath, Target, Clause, Location):- Target=Name/Arity, nonvar(Name),!,defined_at(RefType, HintPath, Name, Clause, Location).
%defined_at(RefType, HintPath, Target, Clause, Location):- nonvar(HintPath),!, xref_mettalog(HintPath), defined_at(RefType, HintPath, Target, Clause, Location).
defined_at(RefType, HintPath, NameArity, Clause, Location):-  
  xref_mettalog(HintPath),
  name_callable(NameArity, Target),
  each_type_at_sorted(Target, Sort),!,
  member(each_type_at(Target,Type, Clause, Path, Ref),Sort),
  once(type_expand(RefType,Type)),
  atom_concat('file://', Path, Doc),
  once(relative_ref_location(Doc, Clause, Ref, Location)).
 
/*
defined_at(Type, HintPath, Callable, Clause, Location) :-
    %name_callable(NameArity, Callable),
    xref_mettalog(HintPath),
    xref_defined(Type, HintPath, Callable, Clause, Path, Ref),
    atom_concat('file://', Path, Doc),
    once(relative_ref_location(Doc, Callable, Ref, Location)).
*/

%! name_callable(?Name:functor, ?Callable:term) is det.
%  True when, if Name = Func/Arity, Callable = Func(_, _, ...) with
%  =Arity= args.
name_callable(Name/_, Callable) :- nonvar(Name), !, name_callable(Name, Callable).
name_callable(Name/Arity, Callable) :- integer(Arity),!,
    length(FakeArgs, Arity),
    (Callable = [Name|FakeArgs];
     Callable =.. [Name|FakeArgs]).
name_callable(Name, Name).
%name_callable(Name, [Name|_]):- atom(Name).

%! relative_ref_location(+Path:atom, +Goal:term, +Position:position(int, int), -Location:dict) is semidet.
%  Given =Goal= found in =Path= and position =Position= (from
%  called_at/3), =Location= is a dictionary suitable for sending as an
%  LSP response indicating the position in a file of =Goal=.
relative_ref_location(_, _, Dict, Location) :- is_dict(Dict),!, Location=Dict.
relative_ref_location(Here, Goal, '$stream_position'(A,B,C,D), Out):- !, line_col('$stream_position'(A,B,C,D), Line0:Char1),
   relative_ref_location(Here, Goal,  position(Line0, Char1), Out).
relative_ref_location(Here, _, position(Line0, Char1),
                      _{uri: Here, range: _{start: _{line: Line0, character: Char1},
                                            end: _{line: Line1, character: 0}}}) :-
    !, succ(Line0, Line1).
relative_ref_location(Here, _, local(Line1),
                      _{uri: Here, range: _{start: _{line: Line0, character: 1},
                                            end: _{line: NextLine, character: 0}}}) :-
    !, succ(Line0, Line1), succ(Line1, NextLine).
relative_ref_location(_, Goal, imported(Path), Location) :-
    atom_concat('file://', Path, ThereUri),
    xref_mettalog(Path),
    xref_defined(Path, Goal, Loc),
    relative_ref_location(ThereUri, Goal, Loc, Location).

/*
%! help_at_position(+Path:atom, +Line:integer, +Char:integer, -Help:string) is det.
%
%  =Help= is the documentation for the term under the cursor at line
%  =Line=, character =Char= in the file =Path=.
help_at_position(Path, Line1, Char0, S) :-
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    (predicate_help(Path, Clause, S0),
    format_help(S0, HS)) -> sformat(S, "Write this for MeTTa: `~q`\n ~w", [Clause,HS]);
      sformat(S,"```metta\n;; MeTTa Docs : ~w \n; Line: ~w, Column:~w\n```\n",
            [Path,Line1, Char0]).
    

%! format_help(+Help0, -Help1) is det.
%
%  Reformat help string, so the first line is the signature of the predicate.
format_help(HelpFull, Help) :-
    split_string(HelpFull, "\n", " ", Lines0),
    exclude([Line]>>string_concat("Availability: ", _, Line),
            Lines0, Lines1),
    exclude([""]>>true, Lines1, Lines2),
    Lines2 = [HelpShort|_],
    split_string(HelpFull, "\n", "", HelpLines),
    selectchk(HelpShort, HelpLines, "", HelpLines0),
    append([HelpShort], HelpLines0, HelpLines1),
    atomic_list_concat(HelpLines1, "\n", Help).

predicate_help(_, Pred, Help) :-
    nonvar(Pred),
    help_objects(Pred, exact, Matches), !,
    catch(help_html(Matches, exact-exact, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).
predicate_help(HerePath, Pred, Help) :-
    xref_mettalog(HerePath),
    name_callable(Pred, Callable),
    xref_defined(HerePath, Callable, Loc),
    location_path(HerePath, Loc, Path),
    once(xref_comment(Path, Callable, Summary, Comment)),
    pldoc_process:parse_comment(Comment, Path:0, Parsed),
    memberchk(mode(Signature, Mode), Parsed),
    memberchk(predicate(_, Summary, _), Parsed),
    format(string(Help), "  ~w is ~w.~n~n~w", [Signature, Mode, Summary]).
predicate_help(_, Pred/_Arity, Help) :-
    help_objects(Pred, dwim, Matches), !,
    catch(help_html(Matches, dwim-Pred, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).
*/

location_path(HerePath, local(_), HerePath).
location_path(_, imported(Path), Path).

linechar_offset(Stream, line_char(Line1, Char0), Offset) :-
    seek(Stream, 0, bof, _),
    seek_to_line(Stream, Line1),
    seek(Stream, Char0, current, Offset).

seek_to_line(Stream, N) :-
    N > 1, !,
    skip(Stream, 0'\n), %'
    NN is N - 1,
    seek_to_line(Stream, NN).
seek_to_line(_, _).

clause_variable_positions(Path, Line, Variables) :-
    xref_mettalog(Path),
    findall(Op, xref_op(Path, Op), Ops),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        ( read_source_term_at_location(
              Stream, Term,
              [line(Line),
               subterm_positions(SubPos),
               variable_names(VarNames),
               operators(Ops),
               error(Error)]),
          ( var(Error)
          -> bagof(
              VarName-Locations,
              Offsets^ColOffsets^Var^Offset^(
                  member(VarName=Var, VarNames),
                  bagof(Offset, find_var(Term, Offset, SubPos, Var), Offsets),
                  collapse_adjacent(Offsets, ColOffsets),
                  maplist(offset_line_char(Stream), ColOffsets, Locations)
              ),
              Variables)
          ; ( debug(server, "Error reading term: ~w", [Error]),
              Variables = [] )
          )
        ),
        close(Stream)
    ).

clause_in_file_at_position(Clause, Path, Position) :-
    xref_mettalog(Path),
    findall(Op, xref_op(Path, Op), Ops),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        clause_at_position(Stream, Ops, Clause, Position),
        close(Stream)
    ).

clause_at_position(Stream, Ops, Clause, Start) :-
    linechar_offset(Stream, Start, Offset), !,
    clause_at_position(Stream, Ops, Clause, Start, Offset).
clause_at_position(Stream, Ops, Clause, line_char(Line1, Char), Here) :-
    read_source_term_at_location(Stream, Terms, [line(Line1),
                                                 subterm_positions(SubPos),
                                                 operators(Ops),
                                                 error(Error)]),
    extract_clause_at_position(Stream, Ops, Terms, line_char(Line1, Char), Here,
                               SubPos, Error, Clause).

extract_clause_at_position(Stream, Ops, _, line_char(Line1, Char), Here, _,
                           Error, Clause) :-
    nonvar(Error), !, Line1 > 1,
    LineBack is Line1 - 1,
    clause_at_position(Stream, Ops, Clause, line_char(LineBack, Char), Here).
extract_clause_at_position(_, _, Terms, _, Here, SubPos, _, Clause) :-
    once(find_clause(Terms, Here, SubPos, Clause)).

%! find_clause(+Term:term, ?Offset:int, +Position:position, ?Subclause) is nondet.
%  True when =Subclause= is a subclause of =Term= at offset =Offset=
%  and =Position= is the term positions for =Term= as given by
%  read_term/3 with =subterm_positions(Position)=.
find_clause(Term, Offset, F-T, Clause) :-
    between(F, T, Offset),
    ground(Term), Clause = Term/0.
find_clause(Term, Offset, term_position(_, _, FF, FT, _), Name/Arity) :-
    between(FF, FT, Offset),
    functor(Term, Name, Arity).
find_clause(Term, Offset, term_position(F, T, _, _, SubPoses), Clause) :-
    between(F, T, Offset),
    Term =.. [_|SubTerms],
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos),
    find_clause(SubTerm, Offset, SubPos, Clause).
find_clause(Term, Offset, parentheses_term_position(F, T, SubPoses), Clause) :-
    between(F, T, Offset),
    find_clause(Term, Offset, SubPoses, Clause).
find_clause({SubTerm}, Offset, brace_term_position(F, T, SubPos), Clause) :-
    between(F, T, Offset),
    find_clause(SubTerm, Offset, SubPos, Clause).

find_containing_term(Offset, [Term|_], [F-T|_], Term, F-T) :-
    between(F, T, Offset).
find_containing_term(Offset, [Term|_], [P|_], Term, P) :-
    P = term_position(F, T, _, _, _),
    between(F, T, Offset), !.
find_containing_term(Offset, [Term|_], [PP|_], Term, P) :-
    PP = parentheses_term_position(F, T, P),
    between(F, T, Offset), !.
find_containing_term(Offset, [BTerm|_], [BP|_], Term, P) :-
    BP = brace_term_position(F, T, P),
    {Term} = BTerm,
    between(F, T, Offset).
find_containing_term(Offset, [Terms|_], [LP|_], Term, P) :-
    LP = list_position(_F, _T, Ps, _),
    find_containing_term(Offset, Terms, Ps, Term, P).
find_containing_term(Offset, [Dict|_], [DP|_], Term, P) :-
    DP = dict_position(_, _, _, _, Ps),
    member(key_value_position(_F, _T, _SepF, _SepT, Key, _KeyPos, ValuePos),
          Ps),
    get_dict(Key, Dict, Value),
    find_containing_term(Offset, [Value], [ValuePos], Term, P).
find_containing_term(Offset, [_|Ts], [_|Ps], T, P) :-
    find_containing_term(Offset, Ts, Ps, T, P).

find_var(Term, Offset, Loc, Var), Var == Term =>
    Loc = F-T, between(F, T, Offset).
find_var(Term, Offset, term_position(F, T, _, _, SubPoses), Var) =>
    between(F, T, Offset),
    % using compound_name_arguments/3 instead of =.. to handle
    % zero-arg terms properly
    compound_name_arguments(Term, _, SubTerms),
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos),
    find_var(SubTerm, Offset, SubPos, Var).
find_var(Term, Offset, parentheses_term_position(F, T, SubPoses), Var) =>
    between(F, T, Offset),
    find_var(Term, Offset, SubPoses, Var).
find_var({SubTerm}, Offset, brace_term_position(F, T, SubPos), Var) =>
    between(F, T, Offset),
    find_var(SubTerm, Offset, SubPos, Var).
find_var(Term, _Offset, _SubPos, Var), Term \== Var => fail.

