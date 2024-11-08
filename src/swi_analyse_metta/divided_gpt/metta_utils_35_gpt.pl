/* 
  @predicate wqs1/1
  @desc Main predicate that dispatches based on the structure and properties of the input.
*/
% If N is not 0 and C is an attributed variable, extract attributes and continue processing with wqs/1.
wqs1(cc(C,N)):- 
    N \== 0, 
    attvar(C), 
    get_attrs(C,PC), 
    !, 
    wqs(ccc(PC,N)).

% If N is not 0 and C is an unbound variable, format C and continue processing.
wqs1(cc(C,N)):- 
    N \== 0, 
    var(C), 
    sformat(PC,"~p",[C]), 
    !, 
    wqs(ccc(PC,N)).

% If C is not an argument string, process it with color_print and continue with wqs/1.
wqs1(cc(C,N)):- 
    \+ arg_string(C), 
    wots_hs(S,color_print(C,C)), 
    wqs(cc(S,N)).

% Handle color_print if C is a valid color, printing with a non-breaking space.
wqs1(color_print(C,X)):- 
    is_color(C), 
    !, 
    write_nbsp, 
    color_print(C,X).

% Handle color_print if C is not a plain variable, printing with a non-breaking space.
wqs1(color_print(C,X)):- 
    \+ plain_var(C), 
    !, 
    write_nbsp, 
    color_print(C,X).

% Handle grid-like arguments with an area less than 5.
wqs1(X):- 
    into_f_arg1(X,_,Arg),
    is_gridoid(Arg),
    area_or_len(Arg,Area),
    Area < 5,
    writeq(X),
    !.

/* previously: wqs1(C):- callable(C), is_wqs(C), wots_vs(S,catch(C,_,fail)),write((S)).
   Comment: This was skipped because it's attempting to execute `C` as a goal and catch errors. 
   The logic is disabled but kept for potential future use.
*/

% Handle grid-like arguments using custom print logic for gridoid structures.
wqs1(X):- 
    is_gridoid_arg1(X), 
    print_gridoid_arg1(X).

/* 
  @predicate into_f_arg1/3
  @desc Decompose compound terms into functor and single argument.
  @example into_f_arg1(foo(bar), F, Arg) results in F = foo, Arg = bar.
*/
% Decompose a compound term into its functor F and argument Arg.
into_f_arg1(X,F,Arg):- 
    compound(X), 
    compound_name_arguments(X,F,[Arg]), 
    compound(Arg).

/* 
  @predicate is_gridoid_arg1/1
  @desc Checks if the first argument of a term is a grid-like structure.
*/
% Check if the argument of X is a grid-like structure.
is_gridoid_arg1(X):- 
    into_f_arg1(X,_F,Arg), 
    is_gridoid(Arg).

/* 
  @predicate print_gridoid_arg1/1
  @desc Print a gridoid structure with its functor and formatted argument.
*/
% Print the functor and argument for grid-like structures.
print_gridoid_arg1(X):- 
    into_f_arg1(X,F,Arg), 
    print_gridoid_arg1(F,Arg).

% If HTML is not required, format and print the grid structure with padding.
print_gridoid_arg1(F,Arg):- 
    \+ wants_html, 
    !, 
    wots_vs(VS,wqs(Arg)), 
    writeq(F), 
    write('(`'), 
    !, 
    print_with_pad(write(VS)), 
    write('`)').

% If HTML is required, wrap the grid structure in a styled HTML span.
print_gridoid_arg1(F,Arg):- 
    wots_vs(VS,wqs(Arg)),
    with_tag_style(span,"display: inline; white-space: nowrap",(writeq(F),write('({'),!,write(VS),write('})'))).

/* 
  @predicate nl_needed/1
  @desc Check if a newline is required based on the current line position.
*/
% If the current line position is beyond N, a newline is needed.
nl_needed(N):- 
    line_position(current_output,L1), 
    L1 >= N.

/* 
  @predicate nl_now/0
  @desc Print a newline if necessary.
*/
% Handle newline based on whether HTML output is required.
nl_now :- 
    wants_html, 
    !, 
    nl_if_needed_ansi.
nl_now :- 
    nl.

/* 
  @predicate nl_if_needed/0
  @desc Prints a newline if required based on the current line formatting.
*/
% Output a newline if ANSI formatting is enabled, or if HTML is in use.
nl_if_needed :- 
    ansi_main, 
    !, 
    format('~N').
nl_if_needed :- 
    ansi_in_pre, 
    ignore((nl_needed(11),write('<br/>'))), 
    !.
nl_if_needed :- 
    wants_html, 
    !, 
    ignore((nl_needed(11),write('<br/>\n'))).
nl_if_needed :- 
    format('~N').

/* previously: nl_if_needed_ansi was skipped, logic preserved for special formatting cases */
nl_if_needed_ansi :- 
    \+ ansi_main, 
    wants_html, 
    !.
nl_if_needed_ansi :- 
    nl_if_needed.

/* 
  @predicate write_nbsp/0
  @desc Writes a non-breaking space depending on the output format (ANSI or HTML).
*/
% Output a non-breaking space based on the current output format.
write_nbsp :- 
    ansi_main, 
    !, 
    write(' ').
write_nbsp :- 
    wants_html, 
    !, 
    write('&nbsp;').
write_nbsp :- 
    write(' ').

/* 
  @predicate is_breaker/1
  @desc Determines if a term is considered a "breaker" (e.g., terms with arity >= 3).
*/
% A "breaker" is defined as a compound term with arity 3 or greater.
is_breaker(P):- 
    compound(P), 
    functor(P,_,A), 
    A >= 3.

/* 
  @predicate last_f/2
  @desc Extracts the functor and arity from a compound term.
*/
% Extract the functor F from a non-compound term.
last_f(H,F):- 
    \+ compound(H), 
    data_type(H,F).

% Extract the functor and arity from a compound term.
last_f(H,F/A):- 
    compound(H), 
    !, 
    functor(H,F,A).

/* 
  @predicate need_nl/2
  @desc Determines if a newline is needed between certain terms based on line positioning and patterns.
*/
% Insert a newline if necessary based on the structure of the terms.
need_nl(H0,[H1,H2|_]):- 
    H1 \= cc(_,_), 
    last_f(H0,F0), 
    last_f(H1,F1), 
    last_f(H2,F2), 
    F0 \== F1, 
    F1 == F2, 
    !, 
    format('~N  ').

/* previously: need_nl logic was extended to handle nested conditions; disabled sections for readability */ 

% No newline needed in this case.
need_nl(_,_).


/*
need_nl(_Last,[H|_]):- last_f(H,F),
 once(nb_current(last_h,cc(LF,C));(LF=F,C=0)),
   (LF==F-> (write_nbsp, plus(C,1,CC), nb_setval(last_h,cc(F,CC))) ; ((C>2 -> nl_now ; write_nbsp), nb_setval(last_h,cc(F,0)))).

need_nl(_,_):- wants_html,!,write_nbsp.
%need_nl(_,_):- !,write_nbsp.
need_nl(H,[P|_]):- \+ is_breaker(H),is_breaker(P),line_position(user_output,L1),L1>80,nl_now,bformatc1('\t\t').
need_nl(_,_):- line_position(user_output,L1),L1>160,nl_now,bformatc1('\t\t').
need_nl(_,_).
*/

dash_chars:- wants_html,!,section_break.
dash_chars:- dash_chars(40),!.

dash_chars(_):- wants_html,!,section_break.
dash_chars(H):- integer(H), dash_border(H).
dash_chars(S):- nl_if_needed,dash_chars(60,S),nl_if_needed_ansi.
dash_chars(_,_):- wants_html,!,section_break.