/* File Directive: Ensure all predicates are documented and preserved for code history */

/**
 * dash_chars(+H, +C) is det.
 * 
 * Recursively prints characters 'C' for 'H' times.
 * If H < 1, the predicate cuts immediately.
 * 
 * @param H Number of repetitions.
 * @param C Character to be printed.
 * @example dash_chars(5, '-'). 
 * This will output '-----'.
 */
dash_chars(H, _) :- 
    % If H is less than 1, cut and do nothing
    H < 1, !.
dash_chars(H, C) :- 
    % For each number between 0 and H, call bformatc1 with character C
    forall(between(0, H, _), bformatc1(C)).

/* previously:  % section_break was supposed to add HTML formatting in a certain mode (wants_html) */
% The section_break predicate was originally meant to insert an HTML break, but that functionality was removed.
/**
 * section_break is det.
 * 
 * Outputs a section break, potentially useful for marking different parts of output.
 */
section_break.

/* previously: Attempt to insert a Unicode border in specific output conditions was disabled for simplicity */
/* The commented-out code for `dash_uborder_no_nl_1` would use different logic based on line positioning, 
   but now we use a simpler method that always prints a default upper border. */
dash_uborder_no_nl_1 :- 
    % Simply format and print the default Unicode upper border
    bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1 :- 
    % Use uborder to get the border style and print it with a space
    uborder(Short, Long), !, 
    bformatc1(Short), 
    bformatc1(Long), 
    write_nbsp.

/**
 * dash_uborder_no_nl(+Width) is det.
 * 
 * Prints a dashed upper border based on the given width.
 * Uses different strategies for width equal to 1 or greater.
 * 
 * @param Width The width of the upper border to print.
 * @example dash_uborder_no_nl(3). 
 * This prints a border of width 3 using the appropriate Unicode characters.
 */
dash_uborder_no_nl(1) :- !, dash_uborder_no_nl_1.
dash_uborder_no_nl(Width) :- 
    % When width is greater than 1, print spaces and Unicode characters
    WidthM1 is Width - 1, 
    uborder(Short, Long), 
    write_nbsp, 
    write(Short), 
    dash_chars(WidthM1, Long), !.
dash_uborder_no_nl(Width) :- 
    % Alternative border style
    WidthM1 is Width - 1, 
    write_nbsp, 
    bformat('\u00AF'), 
    dash_chars(WidthM1, '\u00AF\u00AF'), !.
dash_uborder_no_nl(Width) :- 
    % Another fallback option using different formatting rules
    nl_if_needed, 
    WidthM1 is Width - 1, 
    bformatc1(' \u00AF'), 
    dash_chars(WidthM1, '\u00AF\u00AF').

/**
 * dash_uborder(+Width) is det.
 * 
 * Prints a dashed upper border and ensures a newline afterward.
 * 
 * @param Width The width of the upper border.
 */
dash_uborder(Width) :- 
    % Print the border, ensuring a newline is added afterward
    nl_if_needed, 
    dash_uborder_no_nl(Width), 
    nl_now.

/**
 * uborder(?Short, ?Long) is det.
 * 
 * Determines the characters to use for borders based on stream encoding.
 * Falls back to different border styles based on whether the stream supports UTF-8.
 * 
 * @param Short The short (single character) border.
 * @param Long The long (repeated character) border.
 */
uborder('-', '--') :- 
    % If the current stream supports UTF-8 encoding, use Unicode borders
    stream_property(current_output, encoding(utf8)), !.
uborder('\u00AF', '\u00AF\u00AF') :- !.  % Use the Unicode character for the upper border

/* previously: The alternative case for non-UTF8 encodings was removed as it seems less useful nowadays */

/**
 * dash_border_no_nl(+Width) is det.
 * 
 * Prints a dashed bottom border with no newline.
 * 
 * @param Width The width of the bottom border.
 */
dash_border_no_nl(Width) :- 
    % Print a bottom border only if needed based on line position
    nl_if_needed, 
    WidthM1 is Width - 1, 
    bformatc1(' _'), 
    dash_chars(WidthM1, '__').

/**
 * dash_border(+Width) is det.
 * 
 * Prints a dashed bottom border and ensures a newline afterward.
 * 
 * @param Width The width of the bottom border.
 */
dash_border(Width) :- 
    % Call dash_border_no_nl and make sure to add a newline at the end
    !, dash_border_no_nl(Width), nl_now, !.

/**
 * functor_test_color(+TestResult, -Color) is det.
 * 
 * Maps a test result to a corresponding color.
 * 
 * @param TestResult The result of a test (e.g., pass, fail, warn).
 * @param Color The color associated with that result.
 */
functor_test_color(pass, green).
functor_test_color(fail, red).
functor_test_color(warn, yellow).

/**
 * arcdbg(+G) is det.
 * 
 * Debugging tool that prints a structured representation of the given goal or structure.
 * If the input is a virtual machine map, it prints the map. Otherwise, it uses colors to display compound terms.
 * 
 * @param G The goal or structure to debug.
 */
arcdbg(G) :- 
    % Check if G is a virtual machine map and print accordingly
    is_vm_map(G), !, 
    write_map(G, 'arcdbg').
arcdbg(G) :- 
    % If G is a compound term, get the functor and color it based on the functor's name
    compound(G), 
    compound_name_arity(G, F, _), 
    functor_test_color(F, C), 
    wots_hs(S, print(G)), 
    color_print(C, S), 
    !, 
    nl_if_needed_ansi.
arcdbg(G) :- 
    % Otherwise, just log the goal for debugging
    u_dmsg(G).

/* previously: The portray clauses for the user module were disabled for performance reasons */
/* This commented code seems to have been skipped in favor of less complex portray logic. 
   It was supposed to handle specific types of data structures like grids or objects. */

/**
 * n_times(+N, :Goal) is det.
 * 
 * Repeats a given goal N times.
 * 
 * @param N The number of times to repeat the goal.
 * @param Goal The goal to execute repeatedly.
 */
n_times(N, Goal) :- 
    % For every number between 1 and N, execute the goal, ignoring failures
    forall(between(1, N, _), ignore(Goal)).

/**
 * banner_lines(+Color, +N) is det.
 * 
 * Prints banner lines of a specified color and thickness (number of lines).
 * 
 * @param Color The color to use for the banners.
 * @param N The thickness of the banner in lines.
 */
banner_lines(Color) :- banner_lines(Color, 1).
banner_lines(Color, N) :- 
    % If HTML output is desired, format with HTML tags
    wants_html, !, 
    format('\n<hr style="border: ~wpx solid ~w">\n', [N, Color]), !.
banner_lines(Color, N) :- 
    % Otherwise, print using terminal output and colors
    must_det_ll((
        nl_if_needed,
        n_times(N, color_print(Color, '-------------------------------------------------')), nl_now,
        n_times(N, color_print(Color, '=================================================')), nl_now,
        n_times(N, color_print(Color, '-------------------------------------------------')), nl_now,
        n_times(N, color_print(Color, '=================================================')), nl_now,
        n_times(N, color_print(Color, '-------------------------------------------------')), nl_now
    )), !.

/**
 * print_sso(+A) is det.
 * 
 * Prints a specific structure 'A' unless it contains a grid-related object, in which case, 
 * it defers to a grid-printing utility.
 * 
 * @param A The structure to print.
 */
print_sso(A) :- 
    % If A is not a compound term or contains no grid-like subterms, log the message and print
    (\+ compound(A) ; \+ (sub_term(E, A), is_gridoid(E))), !, 
    u_dmsg(print_sso(A)), !.
print_sso(A) :- 
    % Otherwise, if it's a grid, print the grid and its footer
    grid_footer(A, G, W), 
    writeln(print_sso(W)), 
    print_grid(W, G), !.