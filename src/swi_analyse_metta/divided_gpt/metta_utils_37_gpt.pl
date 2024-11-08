/** 
 * @predicate print_sso(+A)
 * Print the structure and contents of a term after converting it to a string.
 * This predicate ensures that a new line is printed if needed, and it prints
 * each element of the term's structure.
 * 
 * @param A The input term to be printed.
 * 
 * @example 
 * ?- print_sso(some_term).
 * Output: 
 * print_sso(l([]))
 */
print_sso(A):- 
    % Ensure any needed newline is printed
    must_det_ll(( nl_if_needed, 
    % Convert the term A into a string representation
    into_ss_string(A,SS),!,
    % Decompose SS into its list structure
    SS = ss(L,Lst),
    % Print the list length or structure of L
    writeln(print_sso(l(L))),
    % Print each member of Lst
    forall(member(S,Lst),writeln(S)), 
    % Print a newline if needed
    nl_if_needed)),!.

/** 
 * @predicate var_or_number(+V)
 * Succeeds if the given term is either a variable or an integer.
 * 
 * @param V The input term to check.
 * 
 * @example 
 * ?- var_or_number(X).
 * true.
 * 
 * ?- var_or_number(5).
 * true.
 */
var_or_number(V):- 
    % Succeeds if V is a variable
    var(V),!.
var_or_number(V):- 
    % Succeeds if V is an integer
    integer(V),!.

/** 
 * @predicate find_longest_len(+SL, -L)
 * Find the longest string in a list of strings.
 * This variant defaults to using a starting comparison length of 10.
 * 
 * @param SL A list of strings to compare.
 * @param L The length of the longest string.
 * 
 * @example 
 * ?- find_longest_len(["apple", "banana", "cherry"], L).
 * L = 6.
 */
find_longest_len(SL,L):- 
    % Call helper with default initial value 10
    find_longest_len(SL,10,L),!.

/** 
 * @predicate find_longest_len(+SL, +N, -L)
 * Find the longest string in a list, comparing their lengths.
 * 
 * @param SL A list of strings.
 * @param N The current maximum length.
 * @param L The final maximum length.
 */
find_longest_len([],L,L).  % Base case: empty list, return current max length
find_longest_len([S|SS],N,L):- 
    % Find the length of string S
    print_length(S,N2),
    % Compare the current max length N with the new length N2, get max in NM
    max_min(N,N2,NM,_),
    % Recursively find the longest in the remaining strings
    find_longest_len(SS,NM,L).

/* 
 * Meta-predicate directive, specifying that print_with_pad takes a goal 
 * as an argument (i.e., a predicate that can be called within it).
 */
:- meta_predicate(print_with_pad(0)).
:- export(print_with_pad/1).  % Export this predicate for external use

/* 
 * previously: The following commented-out version of print_with_pad 
 * was likely used to calculate the line position manually, but was replaced 
 * due to more efficient or specialized logic in the active version.
 *
 * print_with_pad(Goal):-
 *   (line_position(current_output,O);O=0),!,
 *   O1 is O+1,
 *   call_w_pad(O1,Goal).
 */

/** 
 * @predicate print_with_pad(:Goal)
 * Print a padded output by adjusting the position of the text. 
 * 
 * @param Goal The goal or content to print with padding.
 */
print_with_pad(Goal):-
    % Get the current output position or default to 0
    (line_position(current_output,O);O=0),!,  
    % Increment the position for padding
    O1 is O+1,
    % Convert Goal into a string for printing
    wots(S,Goal),
    % Call print_w_pad with the new position and string
    print_w_pad(O1,S).

/** 
 * @predicate into_s(+Text, -S)
 * Converts various types of input (text or objects) into a string.
 * 
 * @param Text The input text or object to convert.
 * @param S The resulting string.
 */
into_s(Text,S):- 
    % Try to convert Text to a string, fail silently on errors
    notrace(catch(text_to_string(Text,S),_,fail)),!.
into_s(Obj,S):- 
    % Convert object Obj to string using wots_hs
    wots_hs(S,pp(Obj)),!.

/** 
 * @predicate print_w_pad(+Pad, +Text)
 * Print text with padding.
 * 
 * @param Pad The amount of padding before the text.
 * @param Text The text to print.
 */
print_w_pad(Pad,Text):- 
    % Convert Text to a string and split it by newline
    into_s(Text,S), 
    atomics_to_string(L,'\n',S) -> 
    % Map over each line, printing with padding
    my_maplist(print_w_pad0(Pad),L).

/** 
 * @predicate print_w_pad0(+Pad, +S)
 * Helper predicate to print a single line of text with padding.
 * 
 * @param Pad The amount of padding.
 * @param S The string to print.
 */
print_w_pad0(Pad,S):- 
    % Print newline if necessary
    nl_if_needed,
    % Print Pad number of dashes (' ') for padding
    dash_chars(Pad,' '), 
    % Write the string S
    write(S).

:- meta_predicate(call_w_pad_prev(+,0)).
call_w_pad_prev(Pad,Goal):- wots_hs(S,Goal), print_w_pad(Pad,S).

%call_w_pad(N,Goal):- wants_html,!,format('<span style="margin-left:~w0%;">',[N]),call_cleanup(call(Goal),write('</span>')).
:- meta_predicate(call_w_pad(+,0)).
call_w_pad(_N,Goal):- wants_html,!,format('<span style="margin-left:10px;">',[]),call_cleanup(call(Goal),write('</span>')).
call_w_pad(N,Goal):- nl_if_needed,wots_hs(S,dash_chars(N,' ')),!,pre_pend_each_line(S,Goal).
maybe_print_pre_pended(Out,Pre,S):- atomics_to_string(L,'\n',S), maybe_print_pre_pended_L(Out,Pre,L).
maybe_print_pre_pended_L(Out,_,[L]):- write(Out,L),!,flush_output(Out).
maybe_print_pre_pended_L(Out,Pre,[H|L]):- write(Out,H),nl(Out),!,write(Out,Pre),maybe_print_pre_pended_L(Out,Pre,L).

%pre_pend_each_line(_,Goal):- !,ignore(Goal).
:- meta_predicate(pre_pend_each_line(+,0)).
pre_pend_each_line(Pre,Goal):- write(Pre),pre_pend_each_line0(Pre,Goal).
pre_pend_each_line0(Pre,Goal):-
  current_output(Out),
  current_predicate(predicate_streams:new_predicate_output_stream/2),!,
  call(call,predicate_streams:new_predicate_output_stream([Data]>>maybe_print_pre_pended(Out,Pre,Data),Stream)),
  arc_set_stream(Stream,tty(true)),
  %arc_set_stream(Stream,buffer(false)),
  %undo(ignore(catch(close(Stream),_,true))),!,
  setup_call_cleanup(true,
   (with_output_to_each(Stream,once(Goal)),flush_output(Stream)),
    ignore(catch(close(Stream),_,true))),!.
pre_pend_each_line0(Pre,Goal):-
  with_output_to_each(string(Str),Goal)*->once((maybe_print_pre_pended(current_output,Pre,Str),nl_if_needed)).



end_of_file.



run_source_code(ShareVars, SourceCode, Vs, QQ):-
  QQ = source_buffer(SourceCode,Vs),!,
  %print(term=Sourcecode -> vs=Vs),
  maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; maplist(mort,SourceCode)).

run_source_code(ShareVars, Vs, QQ):-
  QQ = source_buffer(SourceCode,Vs),!,
  %print(term=Sourcecode -> vs=Vs),