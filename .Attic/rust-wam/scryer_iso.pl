%:- module(flybase, []).



last([X],X):-!.
last([_|L],X):-last(L,X).


%% get_line_to_chars(+Stream, -Chars, +InitialChars).
%
% Reads chars from stream Stream until it finds a `\n` character.
% InitialChars will be appended at the end of Chars
get_line_to_chars(Stream, Cs0, Cs) :-
        '$get_n_chars'(Stream, 1, Char), % this also works for binary streams
        (   Char == [] -> Cs0 = Cs
        ;   Char = [C],
            Cs0 = [C|Rest],
            (   C == '\n' -> Rest = Cs
            ;   get_line_to_chars(Stream, Rest, Cs)
            )
        ).

:- use_module(library(charsio)).

:- use_module(library(arithmetic)).
:- use_module(library(assoc)).
:- use_module(library(atts)).
:- use_module(library(between)).
:- use_module(library(builtins)).

:- use_module(library(clpb)).
:- use_module(library(clpz)).
:- use_module(library(cont)).
:- use_module(library(crypto)).
:- use_module(library(csv)).
:- use_module(library(dcgs)).
:- use_module(library(debug)).
:- use_module(library(diag)).
:- use_module(library(dif)).
:- use_module(library(error)).
:- use_module(library(files)).
:- use_module(library(format)).
:- use_module(library(freeze)).
:- use_module(library(gensym)).
:- use_module(library(iso_ext)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ops_and_meta_predicates)).
:- use_module(library(ordsets)).
:- use_module(library(os)).
:- use_module(library(pairs)).
:- use_module(library(pio)).
:- use_module(library(queues)).
:- use_module(library(random)).
:- use_module(library(reif)).
:- use_module(library(sgml)).
:- use_module(library(si)).
:- use_module(library(simplex)).
:- use_module(library(sockets)).
:- use_module(library(tabling)).
:- use_module(library(terms)).
:- use_module(library(time)).
:- use_module(library(tls)).
:- use_module(library(ugraphs)).
:- use_module(library(uuid)).
:- use_module(library(xpath)).


% Convert between a name (atom or string) and a list of character codes.
name(Name,Codes):- Codes==[],!,Name=''.
name(Name,Codes):- Name=='',!,Codes=[].
name(Name,Codes):- maybe_into_atom_or_var(Name,Atom),!,name(Atom,Codes).
name(Name,Codes):- maybe_into_atom_or_var(Codes,CAtom),!,CAtom=Name.
name(Name,Codes):- atom(Codes),!,Name=Codes,!.
name(Name,Codes):- catch(atom_codes(Name,Codes),_,fail),!.
name(Atom,Codes):- writeq(name(Atom,Codes)),fail.
%name(Name,Codes):- safe(atom_codes(Name,Codes),name/2),!.



% Determine if the given directory exists.
% Usage: exists_directory(+Dir).
exists_directory(Dir) :-
    % (SWI-Prolog) Succeeds if Dir is an existing directory.
    catch(directory_exists(Dir), _, fail).


% Check if the given list is a list of single-character atoms.
% Usage: is_charslist(+List).
is_charslist([Char|List]) :-
    % Check if Char is a single-character atom.
    is_char(Char),
    % Check if List is a proper list.
    is_list(List),
    % Verify that all elements in List are single-character atoms.
    maplist(is_char, List).

% Check if the given atom consists of a single character.
% Usage: is_char(+Char).
is_char(Char) :-
    % Check if Char is an atom and if its length is 1.
    atom(Char), atom_length(Char, 1).

% Check if the given list is a list of ASCII character codes.
% Usage: is_codeslist(+List).
is_codeslist([Code|List]) :-
    % Check if Code is an ASCII character code.
    is_ascii_code(Code),
    % Check if List is a proper list.
    is_list(List),
    % Verify that all elements in List are ASCII character codes.
    maplist(is_ascii_code, List).

% Check if the given code is an ASCII character code.
% Usage: is_ascii_code(+Code).
is_ascii_code(Code) :-
    % Check if Code is a non-negative integer.
    integer(Code), Code >= 0.



% Check if the given term is a proper list (not just a cons).
% Usage: is_list(?Term).
is_list(Term) :-
    % If Term is not compound, it should be an empty list.
    \+ compound(Term), !, Term == [].
is_list([_|Tail]) :-
    % If Term is a list, verify that its tail is a proper list.
    is_list(Tail).



% Check if the given atom contains the given subatom.
% Usage: atom_contains(+Atom1, +SubAtom).
atom_contains(Atom1, SubAtom) :-
    % Check if both Atom1 and SubAtom are nonvar.
    nonvar(Atom1), nonvar(SubAtom),
    % Convert Atom1 and SubAtom to lists of character codes.
    name(Atom1, List1), name(SubAtom, List2),
    % Check if List2 is a sublist of List1.
    append(_, Tail, List1), append(List2, _, Tail).


% Assert a given term if no variant of it already exists in the database.
% Usage: assert_if_new(+Term).
assert_if_new(Term) :-
    % Check if Term is a rule (Head :- Body) or a fact (just Head).
    ( Term = (Head :- Body) 
    -> copy_term(Body, CopiedBody)
    ; (Head = Term, CopiedBody = true)
    ),
    % Copy the Head to generate a new term with fresh variables.
    copy_term(Head, CopiedHead),
    % If no variant of CopiedHead exists in the database with the same body,
    % assert Term; otherwise, succeed without asserting Term.
    ( \+ (clause(CopiedHead, CopiedBody), variant(CopiedHead, Head))
    -> assertz(Term)
    ; true
    ).


ignore(G):- call(G),!.
ignore(_).

assert(G):- assertz(G).
writeln(G):- write(G),nl.
writeln(S,G):- write(S,G),nl(S).

% Display a message for debugging.
% ISO Standard: format/2
fmt(Message) :- wdmsg(Message).

% Display the canonical term representation of a term.
% ISO Standard: write_canonical/1
display(Term) :- write_canonical(Term).

% Display the canonical term representation of a term to a given stream.
% ISO Standard: write_canonical/2
display(Stream, Term) :- write_canonical(Stream, Term).

% Display a debugging message with newline delimiters.
wdmsg(Message) :- 
  % ISO Standard: flush_output/1
  flush_output(user_output),
  flush_output(user_error),
  format(user_error, "~n~q.~n", [Message]),
  flush_output(user_error).



absolute_file_name(Relative, Absolute) :-
    % If the input file name is already absolute, return it as is.
    (   is_absolute_file_name(Relative) ->
        Absolute = Relative
    ;   % Otherwise, get the current working directory.
        working_directory(CWD, CWD),
        % Concatenate the current working directory with the relative file name.
        path_concat(CWD, Relative, Absolute)
    ).

% Check if a file name is absolute.
is_absolute_file_name(FileName) :-
    % Check if the file name starts with a slash, indicating it's an absolute path.
    sub_atom(FileName, 0, 1, _, '/').



% Expand file name patterns into lists of matching file names.
expand_file_name(Pattern, Files) :-
    split_pattern(Pattern, Dir, BasePattern),
    directory_files(Dir, AllFiles),
    include(match_pattern(BasePattern), AllFiles, Files).

% Split a file name pattern into a directory part and a base pattern.
split_pattern(Pattern, Dir, BasePattern) :-
    atom_concat(Dir, BasePattern, Pattern),
  atom_concat(_, '/', Dir), !.
split_pattern(Pattern, '.', Pattern).

% Match a pattern against a file name.
match_pattern(Pattern, File) :-
    atom_codes(Pattern, PatternCodes),
    atom_codes(File, FileCodes),
    match_pattern_codes(PatternCodes, FileCodes).

% Match a pattern against a file name (in codes representation).
match_pattern_codes([], []).
match_pattern_codes(['*'|Rest], File) :-
    append(_, Tail, File),
    match_pattern_codes(Rest, Tail).
match_pattern_codes(['?'|Rest], [_|Tail]) :-
    match_pattern_codes(Rest, Tail).
match_pattern_codes([C|Rest], [C|Tail]) :-
    \+ member(C, ['*', '?']),
    match_pattern_codes(Rest, Tail).


% Get the name and extension parts of a file name.
file_name_extension(Name, Ext, File):- var(File),!,
  maplist(into_atom_or_var,[Name, Ext],[AName, AExt]),
  atomic_list_concat([AName, AExt], '.', File),!.
file_name_extension(Name, Ext, File):- 
  maplist(into_atom_or_var,[Name, Ext, File],[AName, AExt, AFile]),
  atomic_list_concat(AtomsL, '.', AFile),
  append(NameL,[AExt],AtomsL),
  atomic_list_concat(NameL, '.', AName),!.

% Concatenate a directory and a file name into a full path.
current_path_sep('/').
directory_file_path(Dir, File, Path):- var(Path),!,
  current_path_sep(PathSep),
  maplist(into_atom_or_var,[Dir, File],[ADir, AFile]),
  atomic_list_concat([ADir, AFile], PathSep, Path),!.
directory_file_path(Dir, File, Path):- 
  maplist(into_atom_or_var,[Dir, File, Path],[ADir, AFile, APath]),
  current_path_sep(PathSep),
  atomic_list_concat(AtomsL, PathSep, APath),
  append(DirL,[AFile],AtomsL),
  atomic_list_concat(DirL, PathSep, ADir),!.


% Convert a list, number or other input into an atom or variable.
% Usage: into_atom_or_var(+Input, -AtomOrVar)
into_atom_or_var(Input, AtomOrVar):-  
    Input == [], !,                    % If Input is an empty list, then 
    AtomOrVar = ''.                    % set AtomOrVar to an empty atom.
into_atom_or_var(Element, Element):-    % If Input is already an atom or a variable,
    (atom(Element); var(Element)), !.   % just unify AtomOrVar with Input.
into_atom_or_var(Input, AtomOrVar):-    % Otherwise, try to convert Input into an atom or variable.
    catch(into_atom_or_var1(Input, AtomOrVar), _, fail).

% Try to convert an input to an atom or a variable if possible.
% Usage: maybe_into_atom_or_var(+Input, -AtomOrVar).
maybe_into_atom_or_var(S,A):-  S==[],!,A=''.
maybe_into_atom_or_var(S,A):-  \+ (atom(S);var(S)),!,  catch(into_atom_or_var1(S,A),_,fail).
  % If conversion fails, simply fail.


into_atom_or_var1(Input, AtomOrVar):-  
    Input == [], !,                 % If Input is an empty list, then 
    AtomOrVar = ''.                 % set AtomOrVar to an empty atom.
into_atom_or_var1(Input, AtomOrVar):-  
    is_codeslist(Input), !,          % If Input is a list of code points (ISO standard),
    safe(atom_codes(AtomOrVar, Input), into_atom_or_var1).  % safely convert it into an atom (ISO standard).
into_atom_or_var1(Input, AtomOrVar):-  
    is_charslist(Input), !,          % If Input is a list of characters,
    safe(atom_chars(AtomOrVar, Input), into_atom_or_var1).  % safely convert it into an atom (ISO standard).
into_atom_or_var1(Input, AtomOrVar):-  
    number(Input), !,                % If Input is a number (ISO standard),
    number_chars(Input, Chars), !,   % convert it into a list of characters (ISO standard),
    atom_chars(AtomOrVar, Chars).    % and then into an atom (ISO standard).
into_atom_or_var1(Input, AtomOrVar):-  
    phrase(format_("~w",[Input]), Chars), !,  % Use format_/1 (SWI-Prolog library) to convert Input into a list of characters,
    atom_chars(AtomOrVar, Chars).              % and then convert it into an atom (ISO standard).

% Note: The predicates atom_codes/2, atom_chars/2, number_chars/2 are part of the ISO standard.
% The predicate format_/1 is from the SWI-Prolog library.

atomics_concat(List,Joined):- some_to_atom([Joined|List],[AJoined|AList]),!, atomics_concat_2(AList,AJoined).
      
atomics_concat_2(List,Atom):- Atom=='',!, List = [].
atomics_concat_2(List,Atom):- List==[],!, Atom = ''.
atomics_concat_2([X,Y],XY):-  atom_concat(X,Y,XY),!.
atomics_concat_2([X],Y):- X=Y,!.
atomics_concat_2([X|Y],Atom):- nonvar(X),atom_concat(X,AfterX,Atom),!, atomics_concat_2(Y,AfterX).
atomics_concat_2([X,Y,Z],XY):- nonvar(Y), atomic_list_concat([X,Z],Y,XY),!.
atomics_concat_2([X,Y|Z],XY):- nonvar(Y), atomic_list_concat([X|Z],Y,XY),!.
atomics_concat_2([X,Sep|Y],Atom):- var(X),nonvar(Sep),!,
   atomic_list_concat([X|AfterSepL],Sep,Atom), 
   atomic_list_concat(AfterSepL,Sep,AfterSep),!,
   atomics_concat_2(Y,AfterSep).




atomic_list_concat(Atoms, Sep, Atom):- 
  once(atomic_list_concat0(Atoms, Sep, Atom)).

atomic_list_concat0(Atoms, Sep, Atom) :-  maybe_into_atom_or_var(Sep,ASep),!,
  atomic_list_concat0(Atoms, ASep, Atom).
atomic_list_concat0(Atoms, Sep, Atom) :-  maybe_into_atom_or_var(Atom,AAtom),!,
  atomic_list_concat0(Atoms, Sep, AAtom).
atomic_list_concat0(Atoms, Sep, Atom) :-  is_list(Atoms), maplist(into_atom_or_var,Atoms,AAtoms),!,
  atomic_list_concat2(AAtoms,Sep,Atom).
atomic_list_concat0(Atoms, Sep, Atom) :-  atomic_list_concat2(Atoms, Sep, Atom).


atomic_list_concat2(Atoms, Sep, Atom) :-  is_list(Atoms), nonvar(Atom), nonvar(Sep), !,
  atomic_list_concat3(AtomsV, Sep, Atom),!,Atoms=AtomsV.

atomic_list_concat2(Atoms, Sep, Atom) :-
    (   nonvar(Atoms), nonvar(Sep) -> 
        name(Sep, SepChars),
        maplist(name, Atoms, CharsLists),
        interleave(SepChars, CharsLists, Interleaved),
        append(Interleaved, Chars),
        name(Atom, Chars)
    ;   nonvar(Atom), nonvar(Sep) -> 
        name(Sep, SepChars),
        name(Atom, Chars),
        split_with_sep(Chars, SepChars, CharsLists),
        maplist(name, Atoms, CharsLists)
    ;   (nonvar(Atoms), nonvar(Atom) ->
        name(Atom, Chars),
        maplist(name, Atoms, CharsLists),
        interleave(SepChars, CharsLists, Interleaved),
        append(Interleaved, Chars),
        % Extract separator
        append(_, SepChars, Tail),
        append(Tail, _, Chars),
        length(SepChars, L), L > 0, % Ensure that SepChars has length greater than 0
        name(Sep, SepChars),!)
    ).

interleave(_, [], []).
interleave(_, [Last], [Last]).
interleave(Sep, [First, Second|Rest], [First, Sep|Result]) :-
    interleave(Sep, [Second|Rest], Result).

atomic_list_concat3(Atoms, Sep, Atom) :-
    (   nonvar(Atoms), nonvar(Sep) -> 
        atomic_list_concat(Atoms, Sep, Atom)
    ;   nonvar(Atom), nonvar(Sep) -> 
        split_with_sep(Atom, Sep, Atoms)
    ;   var(Sep), nonvar(Atom), nonvar(Atoms) ->
        determine_separator(Atom, Atoms, Sep)
    ).

determine_separator(Atom, [First|Rest], Sep) :-
    safe(atom_concat(First, SepRest, Atom),determine_separator_1),
    safe(atom_concat(Sep, _, SepRest),determine_separator_2),
    safe(atomic_list_concat(Rest, Sep, RestAtom),determine_separator_3),
    safe(atom_concat(Sep, RestAtom, SepRest),determine_separator_4).

some_to_atom(ARest,Rest):- ARest==[],!,Rest=[].
some_to_atom(ARest,Rest):- Rest==[],!,ARest=[].
some_to_atom(ARest,Rest):- \+compound(ARest),!,Rest=ARest.
some_to_atom([S|ARest],[A|Rest]):- into_atom_or_var(S,A),!,some_to_atom(ARest,Rest).
some_to_atom(A,A).

split_with_sep(AAtom, ASep, [AFirst|ARest]) :-
    some_to_atom([AAtom, ASep, AFirst|ARest],[Atom, Sep, First|Rest]),
    safe(sub_atom(Atom, Before, _, After, Sep),split_with_sep_2),
    safe(sub_atom(Atom, 0, Before, _, First),split_with_sep_3),
    safe(sub_atom(Atom, _, After, 0, SubAtom),split_with_sep_4),
    split_with_sep(SubAtom, Sep, Rest).
split_with_sep(AAtom, _, [Atom]):- into_atom_or_var(AAtom,Atom).

split_with_separator(Chars, Sep, [Part|Parts]) :-
    append(Part, Sep, Prefix),
    append(Prefix, Rest, Chars),
    !,
    split_with_separator(Rest, Sep, Parts).
split_with_separator(Part, _, [Part]).
 
% Implements the `flag/3` predicate, which is similar to SWI-Prolog's but uses the blackboard database.
flag(Key, OldValue, NewValue) :-
    % Check if the key exists in the blackboard.
    (   bb_get(Key, OldValue)
    ->  true
    ;   OldValue = 0
    ),
    NewValueSet is NewValue,
    % Set the new value in the blackboard.
    bb_put(Key, NewValueSet).


