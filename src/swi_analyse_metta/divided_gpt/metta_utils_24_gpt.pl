/* File Directive: Ensures this file is treated as a module, if applicable, 
   though no explicit module declaration appears here */

/* Predicate: is_writer_goal_l/1
   Checks if a given argument matches one of the specified writer goals */
%% is_writer_goal_l(+Goal) is semidet.
%  True if the given Goal is a recognized writer goal such as pp, write, or dash.
is_writer_goal_l(pp). 
is_writer_goal_l(write).  
is_writer_goal_l(dash_).

/* Predicate: maybe_color/2
   Decides whether to apply color or simply write the term, based on the presence of ANSI codes */
%% maybe_color(+Term, +Parameter) is det.
%  Writes Term with or without color formatting depending on whether the term or parameter contains ANSI codes.
%  @example maybe_color('Hello', param) -> writes 'Hello' with appropriate formatting.
maybe_color(SS,_):- 
    term_contains_ansi(SS),   % Check if the term contains ANSI codes
    !, 
    write_nbsp,               % Write a non-breaking space
    write(SS).                % Write the term

maybe_color(SS,P):- 
    term_contains_ansi(P),    % Check if the parameter contains ANSI codes
    !, 
    write_nbsp,               % Write a non-breaking space
    write(SS).                % Write the term

maybe_color(SS,P):- 
    pp_msg_color(P,C),        % Get the color associated with the parameter
    ansicall(C, is_maybe_bold(P, write(SS))), % Apply the color and possibly bold formatting
    !.

/* Predicate: write_atom/1
   Handles how atoms and non-atoms are written, with special formatting for some cases */
%% write_atom(+Atom) is det.
%  Writes the Atom, handling cases where it is not an actual atom or has specific symbols like '~'.
%  @example write_atom('Hello~World') -> formats 'Hello~World' using format/2.
write_atom(S):- 
    \+ atom(S),  % Check if the input is not an atom
    !, 
    wqs(S).      % Call another predicate to handle non-atoms

write_atom(S):- 
    atom_contains(S,'~'),  % Special handling for atoms containing '~'
    !, 
    notrace(catch(format(S,[]),_,maybe_write_atom_link(S))).

write_atom(S):- 
    maybe_write_atom_link(S),  % Try to write the atom as a link if applicable
    !.

write_atom(S):- 
    into_title_str(S,TS),  % Convert the atom into a title string and write it
    write(TS), 
    !.

/* Meta-predicate declaration: into_title_str/2 is a meta-predicate, meaning it operates on other predicates. */
:- meta_predicate(into_title_str(+,-)).

/* Predicate: into_title_str/2
   Converts various Prolog terms into a formatted string representation */
%% into_title_str(+Term, -Str) is det.
%  Converts the Term into a title-friendly string.
%  @example into_title_str('HelloWorld', Str) -> Str = 'Hello World'.
into_title_str(Term,Str):- 
    string(Term),  % If the term is already a string, return it as-is
    !, 
    Str = Term.

into_title_str(Term,Str):- 
    plain_var(Term),  % If it's a plain variable, format it using ~p
    sformat(Str,'~p',[Term]), 
    !.

into_title_str(Term,Str):- 
    var(Term),  % If it's a variable, tersify it and format
    tersify0(Term,Terse), 
    sformat(Str,'~p',[Terse]), 
    !.

into_title_str(Term,Str):- 
    term_is_ansi(Term),  % Handle ANSI-formatted terms
    wots(Str,write_keeping_ansi_mb(Term)), 
    !.

into_title_str(Term,Str):- 
    (is_codelist(Term);is_charlist(Term)),  % Handle code and character lists
    catch(sformat(Str,'~s',[Term]),_,sformat(Str,'~p',[Term])), 
    !.

into_title_str(Term,Str):- 
    is_list(Term),  % Recursively handle lists by converting each element
    my_maplist(into_title_str,Term,O3), 
    atomics_to_string(O3," ",Str), 
    !.

into_title_str([H|T],Str):- 
    into_title_str(H,A),  % Recursively convert the head and tail of a list
    into_title_str(T,B), 
    atomics_to_string([A,B]," ",Str), 
    !.

into_title_str(Term,Str):- 
    \+ callable(Term),  % For non-callable terms, format them using ~p
    sformat(Str,'~p',[Term]), 
    !.

into_title_str(format(Fmt,Args),Str):- 
    sformat(Str,Fmt,Args),  % If the term is a format/2 structure, format it
    !.

into_title_str(Term,""):- 
    empty_wqs_c(Term),  % If the term is empty, return an empty string
    !.

into_title_str(out,"Output").  % Special case for the term 'out'

into_title_str(in,"Input").    % Special case for the term 'in'

into_title_str(i,"IN").        % Special case for the term 'i'

into_title_str(o,"OUT").       % Special case for the term 'o'

/* previously: The following commented out code was used for a different, less efficient string conversion process */
%into_title_str(Term,Str):- tersify23(Term,Terse),Term\=@=Terse,!,into_title_str(Terse,Str).

into_title_str(Term,Str):- 
    callable_arity(Term,0),  % If the term has arity 0 and is a writer goal, write it
    is_writer_goal(Term),
    catch(notrace(wots(Str,call_e_dmsg(Term))),_,fail), 
    !.

into_title_str(Term,Str):- 
    catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)),  % Default case for formatting terms
    nonvar(Str), 
    atom_length(Str,E50), 
    E50 < 180, 
    !.