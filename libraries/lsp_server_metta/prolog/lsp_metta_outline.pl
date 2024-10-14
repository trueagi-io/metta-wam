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

metta_atom_xref(Atom):- 
    metta_file_buffer(+, Atom, NamedVarsList, _Filename, _LineCount),  % Retrieve the atom from a Metta file buffer.
    \+ clause(metta_atom_asserted(_, Atom), true),  % Ensure the atom has not been asserted already.
    nop(maybe_set_var_names(NamedVarsList)).  % Set variable names based on the named variables list.
metta_atom_xref(Atom):- 
    clause(metta_atom_asserted(_, Atom), true).  % Check if the atom has been asserted in the knowledge base.



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
    xref_file_maybe(Path),  % Possibly cross-reference the file.
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
grovel_all_info(Term, Arity):- 
    forall(grovel_some_help(Term, Arity), true).  % Execute the help logic for each possible instance of the term's arity.

%!  grovel_some_help(+Term, +Arity) is det.
%
%   Helper predicate to display information about each instance of a term's arity.
%
%   @arg Term  The term (predicate) for which help is displayed.
%   @arg Arity The arity to check for.
grovel_some_help(Term, _) :- 
    xref_call(eval(['help!', Term], _)).  % Evaluate the help command for the term.
grovel_some_help(Term, Arity):- number(Arity), Arity > 1,
    findall(A, is_documented_arity(Term, A), ArityDoc),  % Retrieve documented arities for the term.
    ArityDoc \== [],  % Ensure the documentation is not empty.
    \+ memberchk(Arity, ArityDoc),  % Verify if the term's arity DOES NOT matches the documented arity.
    format('Arity expected: ~w vs ~w~n', [ArityDoc, Arity]).  % Output a message if there's an arity mismatch.
grovel_some_help(Term, _) :- 
    metta_atom_xref(Atom),  % Cross-reference the term with known atoms.
    about_term(Atom, Term),  % Determine if the atom is related to the term.
    \+ skip_xref_atom(Atom),  % Skip atoms that are not relevant for cross-referencing.
    format('~@~n', [write_src_xref(Atom)]).  % Write the source cross-reference for the atom.


%xref_call(G):- catch(G,E,debug(server(high), "xref_call ~w", [G])).
%xref_call(G):- catch(with_no_debug(G),E,debug(server(high), "xref_call ~w", [G->E])).
xref_call(G):- with_no_debug(G).
%xref_call(G):- call(G). 

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
%promiscuous_symbol(+Term) is semidet.
promiscuous_symbol(Term):- var(Term),!,fail.
promiscuous_symbol('=').
promiscuous_symbol(':').

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
write_src_xref(Src):-
  write_src_xref1(Src),
  maybe_link_xref(Src).
write_src_xref1(Src):- % fail, 
    very_nested_src(Src), !,  % Check if the source is complex.
    wots(S, pp_sexi_l(Src)), write(S).  % Write the full source content if it's complex.
write_src_xref1(Src):- 
    write_src_woi(Src).  % Otherwise, write the source content without additional information.
% Check for deeply nested lists
very_nested_src([_, _ | Src]):- is_list(Src), 
    member(M, Src), is_list(M), 
    member(E, M), is_list(E), 
    member(I, E), is_list(I), !.  
maybe_link_xref(What):- 
  ignore(once((
     clause(metta_file_buffer(_,What0,_,File,_),true,Ref),
     symbolic(File), \+ symbol_contains(File,'stdlib_mettalog'),
     alpha_unify(What,What0),
     next_clause(Ref, metta_file_buffer(_,_,_,File,Loc)),     
     write_file_link(File,Loc)))).

% next_clause(Ref, NextClause)
%   - Ref is the reference of the current clause
%   - NextClause is the next clause's reference, if it exists
next_clause(Ref, NextClause) :-
     nth_clause(Pred, Nth, Ref),
     NextIndex is Nth + 1,
     nth_clause(Pred, NextIndex, NextRef),!,
     clause(NextClause, _, NextRef).  % Get the clause at this reference

write_file_link(File,Position):-   
  stream_position_data(line_count, Position, Line),  % Extract the line number.
  %stream_position_data(line_position, Position, Col),  % Extract the column number.
  %stream_position_data(char_count, Position, CharPos),  % Extract the character position.
  format('~n```~n[~w:~w](file://~w#L~w)~n```lisp~n',[File,Line,File,Line]).

%!  skip_xref_atom(+Atom) is semidet.
%
%   Checks whether the given Atom should be skipped during cross-referencing.
%
%   @arg Atom The atom to check.
skip_xref_atom(Atom):- sub_var('@doc', Atom), !. % Skip documentation (already shown)
skip_xref_atom(exec(_)).  % Skip execution commands.
skip_xref_atom([Pred | _]):- 
    symbol(Pred),  % Check if the predicate is a symbol.
    symbol_concat('assert', _, Pred).  % Skip 'assert*' predicates.

%!  xref_file_maybe(+Path) is semidet.
%
%   Checks if a Metta file needs to be reprocessed by comparing its current content 
%   with the last recorded content, ensuring that at least 20 seconds have passed since 
%   the last check.
%
%   @arg Path The file path of the Metta file.
xref_file_maybe(Path) :-
    file_name_extension(_, metta, Path),  % Ensure the file has a .metta extension.
    (   check_time_elapsed(Path)  % Check if enough time has passed since the last check.
    ->  (   get_current_text(Path, NewText),  % Get the current content of the file.
            compare_and_update_string(Path, NewText),  % Compare with the last stored content.
            nop(debug_buffer_info)  % Save the current state.
        )
    ;   debug(server(xref), 'Skipping check for "~w" as 20 seconds have not passed.~n', [Path]), 
        fail
    ).

%!  debug_buffer_info is det.
%
%   Saves the current state of the Metta file buffer to 'last_file.txt'.
debug_buffer_info:- 
    tell('last_file.txt'),  % Open the file for writing.
    listing(metta_file_buffer),  % Write all asserted facts about the Metta file buffer.
    told.  % Close the file.

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
    ->  true
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
    ->  debug(server(xref), 'Text for "~w" has changed, reprocessing buffer.~n', [Path]),  % Log the change.
        retractall(last_retrieved_string(Path, _)),  % Remove the old content entry.
        asserta(last_retrieved_string(Path, NewText)),  % Update with the new content.
        xref_metta_file_text('&self', Path, NewText)  % Reprocess the file with the new content.
    ;   (debug(server(xref), 'Text for "~w" has not changed, skipping reload.~n', [Path]),  % Log if no change is detected.
        xref_metta_file_text('&self', Path, NewText))  % Still cross-reference the file for consistency.
    ).

%!  xref_metta_file_text(+Self, +Path, +Text) is det.
%
%   Cross-references a Metta file's content and processes it in the correct context.
%
%   @arg Self The context (usually '&self') for the cross-reference.
%   @arg Path The file path to cross-reference.
%   @arg Text The content of the file as text.
xref_metta_file_text(_Self, Path, _Text):-
    absolute_file_name(Path, Filename),  % Convert the file path to an absolute path.
    directory_file_path(Directory, _, Filename),  % Extract the directory path from the file path.
    debug(server(xref), "xref_metta_file_text ~w", [Path]),  % Log the cross-referencing process.
    with_option(exec, skip,  % Use options for processing in the correct context.
  locally(nb_setval(may_use_fast_buffer,t),
   locally(nb_setval(suspend_answers,true),
     with_output_to(string(_),
       include_metta_directory_file('&xref',Directory,Filename))))),!.
%   This variant of `xref_metta_file_text/3` handles file inclusion by converting a 
%   file path to an absolute path, opening the file, and processing its content in 
%   the correct working directory context. It handles both string content and file 
%   reading.
xref_metta_file_text(_Self, Path, Text) :-
    debug(server(xref), "xref_metta_file_text ~w", [Path]),  % Log the file path being processed.
    must_det_ll((
        % Convert the file path to an absolute path
        absolute_file_name(Path, Filename),
        % Extract the directory path from the absolute file name
        directory_file_path(Directory, _, Filename),
        % Process the file content in the specified directory context
        with_option(exec, skip,
          with_option(suspend_answers, true,
            with_cwd(Directory,
                setup_call_cleanup(
                        % Open the file or string content
                    (atomic(Text)->open_string(Text,In);open(Path, read, In, [encoding(utf8)])),
                        % Process the buffer content from the file
                    must_det_ll(xref_metta_file_text_buffer(false, Path, In)),
                        % Ensure the file is closed even in case of errors
                        catch(close(In), _, true))))))).

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
    my_line_count(In, LineCount),
    debug(server(xref), "LineCount ~w", [LineCount]),  % Log the current line number.
    % Get the current mode for reading the file
    current_read_mode(file, Mode),
    % Read and parse the content of the Metta file
    call(read_metta2, In, Expr),
    subst_vars(Expr, Term, [], NamedVarsList),
    % Assert the parsed content into the Metta buffer
    BufferTerm = metta_file_buffer(Mode, Term, NamedVarsList, Filename, LineCount),
    assertz(BufferTerm),
    debug(server(xref), "BufferTerm ~w", [BufferTerm]),  % Log the parsed buffer term.
    % Optionally write the buffer content to the buffer file
    if_t(TFMakeFile, write_bf(BufferFile, BufferTerm)),
    flush_output,  % Ensure all output is flushed.
    at_end_of_stream(In),  % Stop processing once the end of the stream is reached.
    !.

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


