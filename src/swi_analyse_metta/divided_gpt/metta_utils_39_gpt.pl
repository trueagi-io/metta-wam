/* File Directives */
/** This file likely includes utility predicates that handle property 
 *  name-value pairs and operations on logical variables. */

% ----------------------------------------------------------------------
% Predicate: print_prop_val/1
% Purpose: Prints a property and its value in a formatted manner.
%
% @param N=V The input term where N is the property name and V is the value.
%            The property name is transformed using to_prop_name/2 before printing.
% @example
% ?- print_prop_val(name='John').
%         name = 'John'
%
% Previously: A direct printing predicate without formatting might have been used.
% ----------------------------------------------------------------------

print_prop_val(N=V) :-
    % Convert the name N to a property name P
    to_prop_name(N, P),
    % Print a tab before the property name and value, formatted as P = V
    format('~N\t\t'),
    % Print the property name and value
    print(P=V),
    % Print a newline for formatting
    nl.

% ----------------------------------------------------------------------
% Predicate: ignore_numvars/1
% Purpose: Ignores logical variables by matching the '$VAR' functor,
%          which is used internally by the Prolog engine to denote anonymous variables.
%
% @param Name The input variable name (which can be a Prolog '$VAR'(Name)).
% @example
% ?- ignore_numvars('$VAR'(X)).
% true.
%
% Previously: A more complex handling of variables might have been needed,
% but here we focus on ignoring them for simplicity.
% ----------------------------------------------------------------------

ignore_numvars(Name='$VAR'(Name)).

% ----------------------------------------------------------------------
% Dead Code: This section has been commented out because it was part of an older implementation
% or approach that has since been replaced. Keeping it for reference.
% 
% previously: print_old_version(N=V) :- write(N), write(' = '), write(V), nl.
% This older version was too simplistic and didn't format the output as well as the current approach.
%
% ----------------------------------------------------------------------

/* previously:
print_old_version(N=V) :- write(N), write(' = '), write(V), nl.
*/
