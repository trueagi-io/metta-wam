

writeg00(O):- compound(O),compound_name_arguments(O,F,[A]),!,call_w_pad(2,((writeq(F),write('('),writeg3(A),write(')')))).
writeg00(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg00([H|T]):- compound(H),H=(_=_), my_maplist(writeg0,[H|T]).
writeg00([H|T]):- is_list(T),call_w_pad(2,((nl,write('['),writeg2(H),my_maplist(writeg0,T),write(']'),nl))).
%writeg0(Term):- \+ ground(Term),!, \+ \+ (numbervars(Term,99799,_,[singletons(true)]),
%   subst(Term,'$VAR'('_'),'$VAR'('_____'),TermO), writeg0(TermO)).
%writeg0(V):- \+ is_list(V),!,writeq(V),nl_now.
writeg00(V):- \+ is_list(V),!,pp(V).
writeg00(X):- call_w_pad(2,pp(X)).

writeg1(N=V):- is_gridoid(V),!,print_grid(N,V),call_w_pad(2,(my_maplist(writeg1,V))).
writeg1(X):- nl_if_needed,writeg2(X),!,write_nbsp,!.
writeg2(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- is_ftVar(X),!,print(X).
writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(true)]),!.
%writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(false)]),!.
%writeg1(X):- nl_if_needed,writeg(X).
writeg2(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- writeq(X),!.
writeg3(X):- is_list(X),X\==[],X=[_,_|_],!,writeg(X).
writeg3(X):- writeg2(X).
*/



/* previously: This section contains older code related to printing terms using pp_hook_g1 and pp_hook_g.
   These predicates handle specific use cases for pretty-printing terms. Although not in active use,
   they have been retained for future debugging or extensions.
*/

% pp_hook_g1(T):-
%  nb_current('$portraying',Was)
%    ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T)))
%    ; locally(b_setval('$portraying',[T]),ptv0(T)).


/**
 * strip_vspace(+S, -Stripped)
 *
 * Recursively removes leading and trailing whitespace characters from a string S and returns the cleaned string as Stripped.
 * 
 * @param S The input string with potential whitespace
 * @param Stripped The cleaned string with unnecessary whitespace removed
 */
strip_vspace(S,Stripped):- 
    % Remove spaces before recursively stripping the string
    string_concat(' ',SS,S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove spaces after the string and recursively strip
    string_concat(SS,' ',S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove newline before the string and recursively strip
    string_concat('\n',SS,S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove newline after the string and recursively strip
    string_concat(SS,'\n',S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove tabs before the string and recursively strip
    string_concat('\t',SS,S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Remove tabs after the string and recursively strip
    string_concat(SS,'\t',S),!, 
    strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- 
    % Replace certain whitespace patterns with more compact representations and recursively strip
    replace_in_string([" \n"="\n","(   "="(  ","(\n"="( "],S,S2),S2\==S,!, 
    strip_vspace(S2,Stripped).

/* previously: An alternative method for stripping whitespace using split_string was left out, 
   likely due to performance considerations with handling large strings or frequent operations.
*/

% strip_vspace(S,Stripped):- split_string(S, "", "\t\r\n", [Stripped]).
strip_vspace(S,S).


/**
 * print_nl(+P)
 *
 * Prints the term P with a newline if necessary, optionally applying color formatting.
 * 
 * @param P The term to print
 */
print_nl(P):- 
    % Print newline if needed, then print the term without an extra newline
    nl_if_needed, 
    wots_hs(SS,pp_no_nl(P)), 
    maybe_color(SS,P), 
    nl_if_needed.


/**
 * color_write(+S)
 *
 * Writes a term with ANSI formatting if applicable.
 * 
 * @param S The term to write, which could contain ANSI codes or be a normal term
 */
color_write(S):- 
    % If the term is ANSI formatted, preserve formatting
    term_is_ansi(S), !, 
    write_keeping_ansi_mb(S).

color_write(P):- 
    % Otherwise, print the term
    wots_hs(SS,write(P)), 
    maybe_color(SS,P).


/**
 * write_keeping_ansi_mb(+P)
 *
 * Preserves ANSI formatting while writing a term.
 * 
 * @param P The term to write, potentially with ANSI formatting
 */
write_keeping_ansi_mb(P):- 
    % If the term is bold or potentially bold, preserve the formatting
    is_maybe_bold(P,write_keeping_ansi(P)).


/**
 * is_maybe_bold(+P)
 *
 * Checks if a term should be printed in bold by analyzing its content.
 * 
 * @param P The term to check
 */
is_maybe_bold(P):- 
    % Format the term into a string and check if it contains specific bold-related markers
    sformat(S,'~w',[P]), 
    atom_contains(S,'stOF').

is_maybe_bold(P,G):- 
    % If the term qualifies for bold, print it with underline and bold
    is_maybe_bold(P),!, 
    underline_print(bold_print(G)).

is_maybe_bold(_P,G):- 
    % Otherwise, just call the generic printing method
    call(G).


/**
 * pp_msg_color(+P, +C)
 *
 * Prints a message P with color C if applicable.
 * 
 * @param P The message term to print
 * @param C The color associated with the message
 */
pp_msg_color(P,C):- 
    % If the term is compound, check if it has color annotations
    compound(P), 
    pc_msg_color(P,C),!.

pp_msg_color(P,C):- 
    % Otherwise, print the message with default color settings
    must_det_ll(mesg_color(P,C)).


/**
 * pc_msg_color(+P, +C)
 *
 * Handles specific message types for colored output.
 * 
 * @param P The message term to print
 * @param C The color associated with the message
 */
pc_msg_color(iz(P),C):- 
    % If the term is wrapped in 'iz', pass it on to pp_msg_color
    pp_msg_color(P,C).

pc_msg_color(link(P,_,_),C):- 
    % If the term is a 'link', pass it on to pp_msg_color
    pp_msg_color(P,C).