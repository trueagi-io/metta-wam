% tterm/2: Unifies two terms.
% @param A - A term
% @param A - The same term, to ensure unification
% @example tterm(X, X).  % X will unify with X
tterm(A, A).

% Define different tokenizer predicates.
% These tokenizers are used to transform input into specific formats.
% Currently active tokenizers are listed, with one commented out.

% Predicate for tokenizer that converts input into a list.
is_tokenizer(into_list).

% Predicate for tokenizer that breaks atoms based on case.
is_tokenizer(to_case_break_atoms).

% Predicate for tokenizer that stems words from atoms.
is_tokenizer(atom_to_stem_list).

% Predicate for tokenizer that tokenizes an atom into words or subcomponents.
is_tokenizer(tokenize_atom).

% Predicate for tokenizer based on the Double Metaphone algorithm (currently skipped).
% This is commented out likely because it's not currently needed, but kept for future use.
% is_tokenizer(double_metaphone).

% is_an_arg_type/2: Checks if the first argument is of a recognized type.
% Uses flybase_identifier to identify the argument type.
% @param S - The argument to check
% @param T - The recognized type
% @example is_an_arg_type('FBgn0001234', Type).
is_an_arg_type(S, T) :- 
    flybase_identifier(S, T), 
    !.

% has_type/2: Checks if the first four characters of a string match a known identifier.
% It ensures the identifier starts at the beginning and that there is more after.
% @param S - The string to check
% @param Type - The resulting type if found
% @example has_type('FBgn0001234', Type).
has_type(S, Type) :- 
    sub_atom(S, 0, 4, Aft, FB),
    flybase_identifier(FB, Type), 
    !, 
    Aft > 0.

% call_sexpr/1: Calls an S-expression and writes it once, formatted with writeq_nl.
% @param S - The S-expression to call and write
% @example call_sexpr((foo(bar))).
call_sexpr(S) :- 
    once_writeq_nl(call_sexpr(S)).

% call_sexpr/3: This version of call_sexpr was previously defined, but skipped.
% It's commented out because it's either incomplete or replaced by a simpler version.
% call_sexpr(Space, Expr, Result):-

% Declare dynamic predicate fb_pred/2 for FlyBase predicates.
% This directive allows fb_pred/2 to be modified dynamically during runtime.
:- dynamic(fb_pred/2).

% full_atom_count/1: Gets the total count of atoms.
% If total_loaded_atoms flag exists, it retrieves the count.
% Otherwise, it calculates the sum of all metta_stats results.
% @param SL - The total count of atoms
% @example full_atom_count(Count).
full_atom_count(SL) :- 
    flag(total_loaded_atoms, SL, SL), 
    SL > 1, 
    !.

% Fallback to manually summing atom counts if the flag isn't set.
full_atom_count(SL) :- 
    findall(NC, (fb_pred(F, A), metta_stats(F, A, NC)), Each), 
    sumlist(Each, SL).

% heartbeat/0: Periodically prints a heartbeat message if more than 60 seconds have passed.
% Keeps track of last printed time using nb_setval and nb_getval.
% @example heartbeat.
heartbeat :- 
    % Get the current system time
    get_time(CurrentTime),

    % If last_printed_time doesn't exist, set it to the current time
    (   nb_current(last_printed_time, _) 
    ->  true
    ;   nb_setval(last_printed_time, CurrentTime)
    ),

    % Retrieve the last printed time
    nb_getval(last_printed_time, LastPrintedTime),

    % Calculate time difference since the last heartbeat
    Diff is CurrentTime - LastPrintedTime,

    % If more than 60 seconds have passed, print the heartbeat
    (   Diff >= 60
    ->  metta_stats  % Print stats
    ;   true  % Otherwise, do nothing
    ).

% metta_stats/0: Performs garbage collection and prints detailed memory usage statistics.
% @example metta_stats.
metta_stats :- 
    % Perform garbage collection
    gc_now,

    % Print headers to delineate the stats
    writeln('\n\n\n\n\n\n;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),
    writeln(';~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),

    % Calculate and display the total atom count
    full_atom_count(SL),
    format("~N~n; Total\t\tAtoms (Atomspace size): ~`.t ~D~108|~n", [SL]),

    % Get the current system time and update the last printed time
    get_time(CurrentTime), 
    nb_setval(last_printed_time, CurrentTime),

    % Post different statistics
    post_statistic(memory, Mem),
    post_statistic(atom_space, AS),
    post_statistic(cputime, TotalSeconds),
    post_statistic(atoms, Concepts),
    flag(assert_new, CTs, CTs),
    post_statistic(stack, StackMem),

    % Perform various calculations
    PM is Mem + StackMem,
    RM is Mem - AS,
    PA is RM // (SL + 1),
    APS is 60 * floor(SL / (TotalSeconds + 1)),
    ACS is AS // (Concepts + 1),

    % Display specific stats
    pl_stats('SymbolAtoms', Concepts),
    pl_stats('Random samples', CTs),

    % These stats are currently skipped due to some reasons (likely not essential).
    % skip/1 is used to temporarily bypass calculations or stats output.
    skip((pl_stats('Bytes Per Atom (Average)', PA), 
          pl_stats('Bytes Per ConceptNode (Average)', ACS))),
    skip((pl_stats('Relational Memory', RM), 
          pl_stats('ConceptNode Memory', AS))),

    % Other potential stats that were disabled (could be useful for specific cases).
    % pl_stats('Queryspace Memory', StackMem),
    % CPU is CPUTime-57600,

    % Format and display runtime in days:hh:mm:ss format
    format_time(TotalSeconds, Formatted),

    % Skipped atoms per minute stat.
    skip((pl_stats('Atoms per minute', APS))),

    % Display memory and runtime stats
    pl_stats('Total Memory Used', PM),
    pl_stats('Runtime (days:hh:mm:ss)', Formatted),

    nl, nl, 
    !.

% metta_stats/1: Gathers stats for a specific predicate.
% @param F - The predicate
% @example metta_stats(fb_pred).
metta_stats(F) :- 
    for_all(fb_pred(F, A), metta_stats(F, A)).

% metta_stats/2: Gathers stats for a predicate with a specific arity.
% @param F - The predicate
% @param A - The arity
% @example metta_stats(fb_pred, 2).
metta_stats(F, A) :- 
    metta_stats(F, A, NC), 
    pl_stats(F / A, NC).

% metta_stats/3: Retrieves the number of clauses for a predicate.
% @param F - The predicate
% @param A - The arity
% @param NC - The number of clauses
% @example metta_stats(fb_pred, 2, ClauseCount).
metta_stats(F, A, NC) :- 
    functor(P, F, A), 
    predicate_property(P, number_of_clauses(NC)).

% pl_stats/1: Retrieves and displays statistics based on the input Stat.
% @param Stat - The statistic to retrieve
% @example pl_stats(memory).
pl_stats(Stat) :- 
    statistics(Stat, Value), 
    pl_stats(Stat, Value).

% pl_stats/2: Handles cases where Stat has a list of values.
% @param Stat - The statistic
% @param Value - The value or first element of a list of values
pl_stats(Stat, [Value|_]) :- 
    nonvar(Value), 
    !, 
    pl_stats(Stat, Value).

% pl_stats/2: Formats and displays a specific statistic and its value.
% @param Stat - The statistic
% @param Value - The value to display
pl_stats(Stat, Value) :- 
    format("~N;\t\t~@: ~`.t ~@~100|", [format_value(Stat), format_value(Value)]), 
    !.

% format_value/1: Helper predicate to format a value (floats only).
% @param Value - The value to format
% @example format_value(3.14159).
format_value(Value) :- 
    float(Value), 
    !, 
    format("~2f", [Value]), 
    !.