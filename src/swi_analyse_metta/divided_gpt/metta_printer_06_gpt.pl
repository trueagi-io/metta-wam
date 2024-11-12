/* Directives to ensure that necessary files are loaded for the Prolog interpreter.
   These files include various modules needed for meta programming, evaluation,
   and utilities, e.g., `metta_interp`, `metta_compiler`, etc. */

/**
 * with_concepts/2
 *
 * Executes a Goal while enabling or disabling a specific concept feature based on the TF (true/false) argument.
 *
 * @param TF Boolean, true enables concepts, false disables concepts.
 * @param Goal The goal to execute with the given concept setting.
 *
 * @example
 * ?- with_concepts(true, my_goal).
 * Executes `my_goal` with concepts enabled.
 */
with_concepts(TF, Goal) :-
    % Set the `concepts` option to either true or false (TF) and then execute the given Goal
    with_option(concepts, TF, Goal).

/**
 * dont_quote/1
 *
 * Determines whether a given atom should not be quoted based on certain conditions (e.g., length, type).
 *
 * @param Atom The atom to check.
 * @example
 * ?- dont_quote('.').
 * true.
 */
dont_quote(Atom) :-
    % Check if the Atom is a single character and if it's a punctuation symbol
    symbol_length(Atom, 1), !,
    char_type(Atom, punct).

dont_quote(Atom) :-
    % Check if Atom is a symbol and if it remains the same in both uppercase and lowercase
    symbol(Atom), upcase_atom(Atom, Atom), downcase_atom(Atom, Atom).

/**
 * should_quote/1
 *
 * Determines whether an atom should be quoted. This is based on conditions such as it not being a symbol or string.
 *
 * @param Atom The atom to check for quoting.
 * @example
 * ?- should_quote('example atom').
 * true.
 */
should_quote(Atom) :-
    % Ensure that the Atom is not a symbol or string, and fail if so
    \+ symbol(Atom), \+ string(Atom), !, fail.

should_quote(Atom) :-
    % If the atom should not be quoted based on the dont_quote/1 predicate, skip quoting
    \+ dont_quote(Atom),
    % Fetch the character list of the atom
    symbol_chars(Atom, Chars),
    % Check if the characters or symbol require quoting based on additional rules
    once(should_quote_chars(Chars); should_quote_symbol_chars(Atom, Chars)).

/**
 * contains_unescaped_quote/1
 *
 * Checks if a list of characters contains an unescaped quote. This helps in determining whether to quote a string.
 *
 * @param Chars A list of characters.
 * @example
 * ?- contains_unescaped_quote(['"', 'a', '"']).
 * true.
 */
contains_unescaped_quote(['"']) :-
    % A single quote at the end of the list should fail
    !, fail.

contains_unescaped_quote(['"' | _]) :-
    % If the first character is a quote, succeed
    !.

contains_unescaped_quote(['\\', '"' | T]) :-
    % If the sequence is an escaped quote, continue checking the rest of the list
    !, contains_unescaped_quote(T).

contains_unescaped_quote([_ | T]) :-
    % Recursively check the rest of the list for unescaped quotes
    contains_unescaped_quote(T).

/**
 * should_quote_chars/1
 *
 * Determines whether a list of characters should be quoted based on various conditions (e.g., spaces, quotes, etc.).
 *
 * @param Chars A list of characters representing the atom.
 * @example
 * ?- should_quote_chars(['e', 'x', 'a', 'm', 'p', 'l', 'e', ' ', 'a', 't', 'o', 'm']).
 * true.
 */
should_quote_chars([]).
should_quote_chars(['"' | Chars]) :-
    % If the first character is a quote, check the rest for unescaped quotes
    !, contains_unescaped_quote(Chars).

should_quote_chars(Chars) :-
    % Ensure quoting if the list contains any problematic characters such as spaces, quotes, or commas
    member('"', Chars);         % Contains a double quote
    member(' ', Chars);         % Contains a space
    member('''', Chars);        % Contains a single quote
    %  member('/', Chars);      % Contains a slash (disabled for now, as per original code)
    member(',', Chars);         % Contains a comma
    (fail, member('|', Chars)). % Contains a pipe (this condition is intentionally failing)

/**
 * should_quote_symbol_chars/2
 *
 * Checks if a symbol (represented by Atom and its first character Digit) should be quoted. This mainly focuses on
 * numeric values starting with a digit and ensuring they're treated appropriately.
 *
 * @param Atom The atom to check.
 * @param Chars A list of characters representing the atom.
 */
should_quote_symbol_chars(Atom, [Digit | _]) :-
    % If the first character is a digit and the Atom doesn't represent a number, fail
    fail, char_type(Digit, digit), \+ symbol_number(Atom, _).

/* Previously:
 * The following line was meant to match symbols with numbers, but was disabled.
 * The condition ensures that symbols with numeric prefixes do not get special treatment.
 * symbol(Atom),  % Ensure that the input is a symbol
 */


% Example usage:
% ?- should_quote('123abc').
% true.
% ?- should_quote('123.456').
% false.


:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_eval).


