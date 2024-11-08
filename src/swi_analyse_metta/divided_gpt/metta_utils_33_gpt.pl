
    '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        '='    ',
    '                                                                          '='     ',
    '\t'='  ',
    '                         '='     '],S0,SS),p_to_br(SS,O).
*/



wqs_l(H):- 
    \+ is_list(H),  /* If H is not a list, directly process it with wqs/1 */
    !,               /* Cut to avoid further choices once non-list case is handled */
    wqs(H).          /* Call wqs/1 on the non-list input */
    
wqs_l(H):- 
    wqs(H).          /* If H is a list, directly call wqs/1 to handle it */

/**
 * @predicate wqs/1
 * Processes the input term using wqs0/1, and applies colors if applicable.
 * 
 * @param P Input term that will be processed and potentially colored.
 * 
 * @example 
 * ?- wqs(foo).  % Calls wqs0/1 to process and output foo.
 */
wqs(P):- 
    wots_hs(SS, wqs0(P)),   /* Call wots_hs/2 with the result of wqs0(P) */
    maybe_color(SS, P).     /* Apply color formatting to the output if appropriate */

/**
 * @predicate wqs/2
 * Processes the term using wqs0/1 with an additional condition.
 * 
 * @param C A condition to be checked.
 * @param P The term to be processed if the condition holds.
 */
wqs(C, P):- 
    ansicall(C, wqs0(P)),  /* Calls wqs0/1 on P if C holds, and applies ANSI formatting */
    !.                     /* Cut to ensure no backtracking */

/**
 * @predicate wqs0/1
 * Base case for processing different types of input in wqs/1 and wqs/2.
 * 
 * Handles variables, colors, maps, and ANSI terms accordingly.
 * 
 * @param X The input term to process based on its type.
 */
wqs0(X):- 
    plain_var(X),          /* If X is a plain variable */
    wqs(plain_var(X)),     /* Process the variable in wqs/1 */
    !.

wqs0(X):- 
    plain_var(X),          /* Redundant check for plain variable (could be optimized out) */
    !, 
    wqs(plain_var(X)),     /* Process the plain variable again */
    ibreak.                /* Force a break point in execution for debugging purposes */

wqs0(S):- 
    term_is_ansi(S),       /* If S is an ANSI-compatible term */
    !, 
    write_keeping_ansi_mb(S).  /* Write S to output while preserving ANSI formatting */

wqs0(C):- 
    is_colorish(C),        /* If C is a color or color-like term */
    color_print(C, C),     /* Output C with color formatting */
    !.

wqs0(G):- 
    is_vm_map(G),          /* If G is a virtual machine map */
    !, 
    write_map(G, 'wqs').   /* Write the VM map to the output with a label 'wqs' */

wqs0(X):- 
    var(X),                /* If X is an unbound variable */
    !, 
    get_attrs(X, AVs),     /* Get the attributes of X */
    !, 
    writeq(X),             /* Write the variable name */
    write('/*{'),          /* Open comment block for attributes */
    print(AVs),            /* Print the attributes */
    write('}*/').          /* Close comment block */

/* previously: old predicates for special handling removed but still present here */
/* Code dealing with specific legacy term types, replaced with more general handling */