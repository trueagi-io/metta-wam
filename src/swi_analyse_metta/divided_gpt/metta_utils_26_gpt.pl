/* 
  pp/1
  Prints a term, but ensures some setup with nb_setval to maintain state.
  Uses nb_current to check if arc_can_portray exists before printing.

  @param Term The term to print
  @example pp(my_term).
*/
pp(Term) :-
    % Check if the nb_current 'arc_can_portray' flag is unset, if so print the term with specific settings
    \+ nb_current(arc_can_portray, _), 
    !, 
    locally(nb_setval(arc_can_portray, t), print(Term)).

pp(Term) :- 
    % If the previous condition fails, use az_ansi to print the term in ANSI format and handle newline
    az_ansi(pp_no_nl(Term)), 
    !, 
    nl_if_needed_ansi.

/*
  Previously: ptcol(P) was a general printing routine, but this code was commented out
  It seems that ptcol_html is now preferred, and the original code is skipped. 
  It's left in case ptcol is used in a future revision or the application context changes.
  This dead code section may be skipped because ptcol_html provides better formatting for HTML output.
*/
/*
ptcol(P) :- 
    wants_html, !, 
    ptcol_html(P).

ptcol(call(P)) :- 
    callable(P), !, 
    call(P).

ptcol(P) :- 
    pp(P).
*/

% Main entry point to handle HTML output using ptcol_html_scrollable
ptcol_html(P) :- 
    ptcol_html_scrollable_0(P).

% Handle HTML scrolling output with a div tag and the scrollable attribute
ptcol_html_scrollable(P) :- 
    with_tag_ats(div, scrollable, ptcol_html_scrollable_0(P)).

% Basic HTML output handler for pretty printing within a preformatted block
ptcol_html_0(P) :- 
    with_tag(pre, ptcol_html_wo_pre(P)).

% Calls P if it's callable; otherwise, pretty prints without newlines
ptcol_html_wo_pre(call(P)) :- 
    callable(P), !, 
    in_pp_html(call(P)).

ptcol_html_wo_pre(P) :- 
    in_pp_html(print_tree_no_nl(P)).

% Scrollable HTML pretty print routine that wraps around the non-scrollable version
ptcol_html_scrollable_0(P) :- 
    ptcol_html_wo_pre(P).

/* 
  pp_wcg/1 
  Wrapper for pretty printing with an option for HTML output or a safe method to print terms.
  @param G The term or goal to print
  @example pp_wcg(my_term).
*/
pp_wcg(G) :- 
    % Check if HTML is preferred, if so, use the scrollable HTML version
    wants_html, !, 
    ptcol_html_scrollable(G).

pp_wcg(G) :- 
    % Otherwise, use a safe method to print the term, with special flags
    pp_safe(call((locally(nb_setval(arc_can_portray, t), print(G))))), !.

/*
  wqln and wqnl are simple wrappers for pretty printing with or without newlines.
  The naming convention here is somewhat arbitrary, but wqln seems to ensure newlines after printing.
*/

/* 
  wqln/1 
  Wrapper for ppnl (pretty print with newline).
  @param Term The term to print
*/
wqln(Term) :- 
    ppnl(Term).

/* 
  wqnl/1 
  Similar to wqln, but may prefer safer printing.
  @param G The term to print
*/
wqnl(G) :- 
    pp_safe(call((locally(nb_setval(arc_can_portray, nil), print(G))))), !.

/* 
  pp_safe/1 
  Ensures safe printing by checking if printing should be hidden.
  If not hidden, it will safely call or print the term.
  @param W The term to print
*/
pp_safe(_) :- 
    % Check if pp_hide is set, if so, do nothing (skip printing)
    nb_current(pp_hide, t), !.

pp_safe(call(W)) :- 
    % If W is a callable term, call it and ensure newlines around the output
    !, 
    nl_if_needed, nl_now, 
    call(W), 
    nl_now.

pp_safe(W) :- 
    % Otherwise, write the term in a quoted format, ensuring newlines around the output
    nl_if_needed, nl_now, 
    writeq(W), 
    nl_now.

/* 
  pp_safe/2 
  A version of pp_safe that allows colored printing.
  @param C The color to use
  @param W The term to print
*/
pp_safe(C, W) :- 
    color_print(C, call(pp_safe(W))).

/*
  p_p_t_no_nl/1 
  Handles printing based on whether the system wants HTML or ANSI printing.
  @param Term The term to print
*/
p_p_t_no_nl(P) :- 
    % Check if ANSI printing is disabled and HTML is wanted, use HTML print
    \+ ansi_main, wants_html, !, 
    ptcol_html(P).

p_p_t_no_nl(Term) :- 
    % Otherwise, use ANSI printing without newlines
    az_ansi(print_tree_no_nl(Term)).

/* 
  ppt_no_nl/1 
  Another variation of pretty printing without newlines, with checks for HTML and ANSI.
  @param P The term to print
*/
ppt_no_nl(P) :- 
    % Use HTML printing if applicable
    \+ ansi_main, wants_html, !, 
    ptcol_html(P).

ppt_no_nl(P) :- 
    % Otherwise, try to tersify the term and then print it without newlines
    tersify(P, Q), !, 
    pp_no_nl(Q).

/* 
  is_toplevel_printing/1 
  Determines if the term is being printed at the top level (for pretty-printing optimizations).
  @param _ Any term (ignored)
*/
is_toplevel_printing(_) :- 
    % Check if the output is a string or the cursor is near the start of the line
    \+ is_string_output, 
    line_position(current_output, N),  
    N < 2, 
    fail.

/* 
  pp_no_nl/1 
  Pretty print a term without newlines, handling variables, ANSI terms, and special cases.
  @param P The term to print
*/
pp_no_nl(P) :- 
    % If P is a variable, print it as a variable term and perform optional debug actions
    var(P), !, 
    pp(var_pt(P)), 
    nop((dumpST, ibreak)).

pp_no_nl(S) :- 
    % If S is an ANSI term, print it with ANSI support
    term_is_ansi(S), !, 
    write_keeping_ansi_mb(S).

pp_no_nl(P) :- 
    % If P is an atom containing '~', use format to handle special formatting
    atom(P), atom_contains(P, '~'), !, 
    format(P).

pp_no_nl(G) :- 
    % If G is a VM map, write it using write_map
    is_vm_map(G), !, 
    write_map(G, 'pp').

pp_no_nl(P) :- 
    % Otherwise, use a general pretty-printing mechanism with guessed formatting
    \+ \+ ((pt_guess_pretty(P, GP), ptw(GP))).

/* 
  ptw/1 
  Main pretty print wrapper that falls back to various printing techniques depending on the type of term.
  @param P The term to print
*/
ptw(P) :- 
    % If P is a variable, print it with specific settings
    var(P), !, 
    ptw(var_ptw(P)), 
    nop((dumpST, ibreak)).

ptw(G) :- 
    % If G is a VM map, write it with specific settings
    is_vm_map(G), !, 
    write_map(G, 'ptw').

ptw(S) :- 
    % If S is an ANSI term, write it with ANSI support
    term_is_ansi(S), !, 
    write_keeping_ansi_mb(S).

ptw(P) :- 
    % Otherwise, use the no-newline version of pretty printing
    p_p_t_no_nl(P), !.

/* 
  pt_guess_pretty/2 
  Attempts to pretty-print a term by guessing the format.
  @param P The original term
  @param O The guessed pretty-printed version
*/
pt_guess_pretty(P, O) :- 
    \+ nb_current(in_pt_guess_pretty, t), 
    locally(nb_setval(in_pt_guess_pretty, t), pt_guess_pretty_1(P, O)).

pt_guess_pretty(O, O).

/* 
  upcase_atom_var_l/2 
  Converts a list of atoms to uppercase, with special handling for lists of atoms.
  @param IntL Input list
  @param NameL Resulting list with uppercase atoms
*/
upcase_atom_var_l(IntL, NameL) :- 
    upcase_atom_var(IntL, NameL).

upcase_atom_var_l(IntL, NameL) :- 
    % If the input is a list, recursively apply upcase_atom_var_l
    is_list(IntL), !, 
    my_maplist(upcase_atom_var_l, IntL, NameL).

/* 
  pt_guess_pretty_1/2 
  A helper predicate for pt_guess_pretty that attempts to transform a term for pretty printing.
  @param P Input term
  @param O Transformed term
*/
pt_guess_pretty_1(P, O) :- 
    copy_term(P, O, _), 
    ignore((sub_term(Body, O), compound(Body), Body = was_once(InSet, InVars), upcase_atom_var_l(InSet, InVars))).