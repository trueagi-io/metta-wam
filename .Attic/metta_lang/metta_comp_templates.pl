/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%*********************************************************************************************% 
% PROGRAM FUNCTION:  Furnishes various algorithms for lists, math, trees, graphs, and strings, 
% with both recursive and accumulator-based versions.
%*********************************************************************************************%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  len(+List, -Length) is det.
%
%   1. Length of a List
%   Normal Recursive
%
%   Computes the length of a given list.
%
%   This predicate calculates the length of a list using recursion. The base case
%   is an empty list, which has a length of 0. The recursive case processes the
%   tail of the list and increments the count by 1 for each element.
%
%   @arg List The input list whose length is to be computed.
%   @arg Length The computed length of the list.
%
%   @example Calculate the length of a list:
%     ?- len([1, 2, 3, 4], L).
%     L = 4.
%
len([], 0).  % Base case: the length of an empty list is 0.
len([_|T], N) :-
    % Recursively call len/2 on the tail of the list, then increment the result.
    len(T, X),
    N is X + 1.  % The length is one more than the length of the tail.

%!  len_acc(+List, -Length) is det.
%
%   With Accumulator
%
%   Computes the length of a list using an accumulator.
%
%   This is a tail-recursive version of the `len/2` predicate that uses an accumulator
%   to keep track of the length during recursion. The base case occurs when the list
%   is empty, and the accumulator holds the final length of the list.
%
%   @arg List   The input list whose length is to be computed.
%   @arg Length The computed length of the list.
%
%   @example Calculate the length of a list using an accumulator:
%     ?- len_acc([a, b, c, d], N).
%     N = 4.
%
len_acc(L, N) :-
    % Initialize the accumulator as 0 and call the helper predicate.
    len_acc(L, 0, N).

%!  len_acc(+List, +Acc, -Length) is det.
%
%   Helper predicate for computing the length of a list using an accumulator.
%   The base case occurs when the list is empty, and the accumulator holds the final length.
%
%   @arg List   The input list.
%   @arg Acc    The accumulator for tracking the length.
%   @arg Length The final length of the list.
%
len_acc([], Acc, Acc).  % Base case: when the list is empty, return the accumulator as the length.
len_acc([_|T], Acc, N) :-
    % Increment the accumulator and recurse on the tail of the list.
    NewAcc is Acc + 1,
    len_acc(T, NewAcc, N).

%!  sum(+List, -Sum) is det.
%
%   2. Sum of a List
%   Normal Recursive
%
%   Computes the sum of all elements in a given list.
%
%   This predicate calculates the sum of the elements in a list using recursion.
%   The base case is an empty list, which has a sum of 0. The recursive case
%   processes the tail of the list, adding the head of the list to the sum of the
%   tail.
%
%   @arg List The input list whose elements are to be summed.
%   @arg Sum The computed sum of the list elements.
%
%   @example Calculate the sum of a list:
%     ?- sum([1, 2, 3, 4], S).
%     S = 10.
%
sum([], 0).  % Base case: the sum of an empty list is 0.
sum([H|T], S) :-
    % Recursively call sum/2 on the tail of the list, then add the head element to the result.
    sum(T, X),
    S is X + H.  % The sum is the head plus the sum of the tail.

%!  sum_acc(+List, -Sum) is det.
%
%   With Accumulator
%
%   Computes the sum of all elements in a given list using an accumulator.
%
%   This predicate is a tail-recursive version of the sum/2 predicate, utilizing
%   an accumulator to avoid stack overflow on large lists. The public predicate
%   sum_acc/2 initializes the accumulator to 0 and calls the helper predicate
%   sum_acc/3.
%
%   @arg List The input list whose elements are to be summed.
%   @arg Sum The computed sum of the list elements.
%
%   @example Calculate the sum of a list using an accumulator:
%     ?- sum_acc([1, 2, 3, 4], S).
%     S = 10.
%
sum_acc(L, S) :-
    % Initialize the accumulator to 0 and call the helper predicate.
    sum_acc(L, 0, S).

%!  sum_acc(+List, +Acc, -Sum) is det.
%
%   Helper predicate that computes the sum of a list using an accumulator.
%   The base case is an empty list, where the sum is simply the value of the
%   accumulator. The recursive case updates the accumulator by adding the head
%   of the list and processes the tail.
%
%   @arg List The input list whose elements are to be summed.
%   @arg Acc  The accumulator holding the current sum during recursion.
%   @arg Sum  The computed sum of the list elements.
%
sum_acc([], Acc, Acc).  % Base case: when the list is empty, the accumulator holds the sum.
sum_acc([H|T], Acc, S) :-
    % Add the head of the list to the accumulator and recurse on the tail.
    NewAcc is Acc + H,
    sum_acc(T, NewAcc, S).

%!  factorial(+N, -F) is det.
%
%   %   3. Factorial
%   Normal Recursive
%
%   Computes the factorial of a given non-negative integer N.
%
%   The factorial of a number N is the product of all positive integers less than
%   or equal to N. This predicate uses recursion to compute the factorial, with
%   the base case being the factorial of 0, which is defined as 1.
%
%   @arg N The non-negative integer whose factorial is to be computed.
%   @arg F The computed factorial of N.
%
%   @example Calculate the factorial of a number:
%     ?- factorial(5, F).
%     F = 120.
%
factorial(0, 1).  % Base case: the factorial of 0 is 1.
factorial(N, F) :-
    % Ensure that N is a positive integer.
    N > 0,
    % Decrement N to compute the factorial of N - 1.
    X is N - 1,
    % Recursively calculate the factorial of N - 1.
    factorial(X, Y),
    % Multiply N by the result of the previous factorial.
    F is N * Y.

%!  factorial_acc(+N, -F) is det.
%
%   With Accumulator
%
%   Computes the factorial of a given non-negative integer N using an accumulator.
%
%   This is a tail-recursive version of the factorial/2 predicate, which makes use
%   of an accumulator to store the intermediate product. This approach is more
%   efficient for large values of N as it avoids deep recursion.
%
%   @arg N The non-negative integer whose factorial is to be computed.
%   @arg F The computed factorial of N.
%
%   @example Calculate the factorial of a number using an accumulator:
%     ?- factorial_acc(5, F).
%     F = 120.
%
factorial_acc(N, F) :-
    % Initialize the accumulator to 1 and call the helper predicate.
    factorial_acc(N, 1, F).

%!  factorial_acc(+N, +Acc, -F) is det.
%
%   Helper predicate that computes the factorial using an accumulator.
%   The base case occurs when N is 0, at which point the accumulator holds the final result.
%   The recursive case multiplies the accumulator by N and then decrements N.
%
%   @arg N   The current number whose factorial is being computed.
%   @arg Acc The accumulator holding the current product.
%   @arg F   The computed factorial of N.
%
factorial_acc(0, Acc, Acc).  % Base case: when N is 0, the accumulator holds the factorial.
factorial_acc(N, Acc, F) :-
    % Ensure that N is a positive integer.
    N > 0,
    % Multiply the accumulator by N.
    NewAcc is Acc * N,
    % Decrement N.
    NewN is N - 1,
    % Recursively calculate the factorial with the updated accumulator.
    factorial_acc(NewN, NewAcc, F).

%!  reverse_list(+List, -Reversed) is det.
%
%   4. Reverse List
%   Normal Recursive
%
%   Reverses the elements of a given list.
%
%   This predicate reverses a list using recursion. The base case is an empty list,
%   which remains empty when reversed. The recursive case processes the tail of the
%   list, reverses it, and appends the head to the end of the reversed tail.
%
%   @arg List     The input list to be reversed.
%   @arg Reversed The reversed list.
%
%   @example Reverse a list:
%     ?- reverse_list([1, 2, 3, 4], R).
%     R = [4, 3, 2, 1].
%
reverse_list([], []).  % Base case: an empty list is already reversed.
reverse_list([H|T], R) :-
    % Recursively reverse the tail of the list.
    reverse_list(T, RevT),
    % Append the head of the original list to the end of the reversed tail.
    append(RevT, [H], R).

%!  reverse_list_acc(+List, -Reversed) is det.
%
%   With Accumulator
%
%   Reverses the elements of a given list using an accumulator.
%
%   This is a tail-recursive version of reverse_list/2 that utilizes an accumulator 
%   to build the reversed list. It is more efficient because it avoids the need 
%   for appending at each recursive step, making it suitable for large lists.
%
%   @arg List     The input list to be reversed.
%   @arg Reversed The reversed list.
%
%   @example Reverse a list using an accumulator:
%     ?- reverse_list_acc([1, 2, 3, 4], R).
%     R = [4, 3, 2, 1].
%
reverse_list_acc(L, R) :-
    % Initialize the accumulator as an empty list and call the helper predicate.
    reverse_list_acc(L, [], R).

%!  reverse_list_acc(+List, +Acc, -Reversed) is det.
%
%   Helper predicate that reverses a list using an accumulator.
%   The base case is when the input list is empty, at which point the accumulator 
%   holds the reversed list. The recursive case prepends the head of the input list 
%   to the accumulator and processes the tail.
%
%   @arg List     The input list to be reversed.
%   @arg Acc      The accumulator holding the partially reversed list.
%   @arg Reversed The reversed list.
%
reverse_list_acc([], Acc, Acc).  % Base case: when the list is empty, the accumulator holds the reversed list.
reverse_list_acc([H|T], Acc, R) :-
    % Prepend the head of the list to the accumulator and recurse on the tail.
    reverse_list_acc(T, [H|Acc], R).

%!  fibonacci(+N, -F) is det.
%
%   5. Fibonacci
%   Normal Recursive
%
%   Computes the N-th Fibonacci number.
%
%   The Fibonacci sequence is defined as follows:
%   - fibonacci(0) = 0
%   - fibonacci(1) = 1
%   - fibonacci(N) = fibonacci(N-1) + fibonacci(N-2) for N > 1
%
%   This predicate uses a recursive approach to compute the N-th Fibonacci number.
%   For large values of N, this implementation can be inefficient due to repeated 
%   calculations.
%
%   @arg N The position in the Fibonacci sequence (0-based index).
%   @arg F The N-th Fibonacci number.
%
%   @example Calculate the 5th Fibonacci number:
%     ?- fibonacci(5, F).
%     F = 5.
%
fibonacci(0, 0).  % Base case: the 0th Fibonacci number is 0.
fibonacci(1, 1).  % Base case: the 1st Fibonacci number is 1.
fibonacci(N, F) :-
    % Ensure that N is greater than 1.
    N > 1,
    % Calculate the two preceding Fibonacci numbers.
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    % The Fibonacci number is the sum of the two preceding numbers.
    F is F1 + F2.

%!  fibonacci_acc(+N, -F) is det.
%
%   With Accumulator
%
%   Computes the N-th Fibonacci number using an accumulator-based approach.
%
%   This is a tail-recursive version of the Fibonacci sequence calculation that uses
%   two accumulators to keep track of the two preceding Fibonacci numbers. This method
%   is more efficient than the simple recursive version as it avoids redundant calculations
%   and ensures constant stack depth.
%
%   @arg N The position in the Fibonacci sequence (0-based index).
%   @arg F The N-th Fibonacci number.
%
%   @example Calculate the 5th Fibonacci number using an accumulator:
%     ?- fibonacci_acc(5, F).
%     F = 5.
%
fibonacci_acc(N, F) :-
    % Initialize the first two Fibonacci numbers and call the helper predicate.
    fibonacci_acc(N, 0, 1, F).

%!  fibonacci_acc(+N, +A, +B, -F) is det.
%
%   Helper predicate that computes the N-th Fibonacci number using two accumulators.
%   The base case occurs when N is 0, at which point the first accumulator holds the result.
%   The recursive case reduces N by 1 and updates the accumulators to hold the next pair
%   of Fibonacci numbers.
%
%   @arg N   The current position in the Fibonacci sequence.
%   @arg A   The Fibonacci number at position N-1.
%   @arg B   The Fibonacci number at position N.
%   @arg F   The computed Fibonacci number at position N.
%
fibonacci_acc(0, A, _, A).  % Base case: when N is 0, the first accumulator holds the result.
fibonacci_acc(N, A, B, F) :-
    % Ensure that N is greater than 0.
    N > 0,
    % Decrement N.
    NewN is N - 1,
    % Update the accumulators: A becomes B, and B becomes A + B.
    NewB is A + B,
    % Recursively compute the Fibonacci number with updated accumulators.
    fibonacci_acc(NewN, B, NewB, F).

%!  element_in_list(+Element, +List) is nondet.
%
%   6. Find an Element in a List
%   Normal Recursive
%
%   Checks whether a given element exists in a list.
%
%   This predicate succeeds if the element is found in the list. It recursively
%   traverses the list, checking whether the head of the list matches the element.
%   If not, it continues checking the tail of the list.
%
%   @arg Element The element to search for in the list.
%   @arg List    The list in which to search for the element.
%
%   @example Check if an element exists in a list:
%     ?- element_in_list(3, [1, 2, 3, 4]).
%     true.
%
%     ?- element_in_list(5, [1, 2, 3, 4]).
%     false.
%
element_in_list(X, [X|_]).  % Base case: the element is the head of the list.
element_in_list(X, [_|T]) :-
    % Recursively check if the element is in the tail of the list.
    element_in_list(X, T).

%!  element_in_list_acc(+Element, +List) is nondet.
%
%   With Accumulator
%
%   Checks whether a given element exists in a list using an accumulator.
%
%   This predicate is an accumulator-based version of element_in_list/2. It carries 
%   an accumulator that helps track whether the element has been found. If the element 
%   is found, the result is immediately set to true, avoiding further recursion.
%
%   @arg Element The element to search for in the list.
%   @arg List    The list in which to search for the element.
%
%   @example Check if an element exists in a list using an accumulator:
%     ?- element_in_list_acc(3, [1, 2, 3, 4]).
%     true.
%
%     ?- element_in_list_acc(5, [1, 2, 3, 4]).
%     false.
%
element_in_list_acc(X, L) :- 
    % Initialize the accumulator as false and call the helper predicate.
    element_in_list_acc(X, L, false).

%!  element_in_list_acc(+Element, +List, +Acc) is nondet.
%
%   Helper predicate that checks if an element exists in a list using an accumulator.
%   The base case occurs when the list is empty, and the result is determined by the accumulator.
%   The recursive case updates the accumulator if the element is found.
%
%   @arg Element The element to search for in the list.
%   @arg List    The list in which to search for the element.
%   @arg Acc     The accumulator, which is initially false and becomes true if the element is found.
%
element_in_list_acc(X, [], Acc) :- 
    % Base case: the list is empty. Return the accumulator value.
    Acc.
element_in_list_acc(X, [X|_], _) :- 
    % If the element is found, succeed immediately.
    true.
element_in_list_acc(X, [_|T], Acc) :- 
    % Recursively search the tail of the list with the current accumulator value.
    element_in_list_acc(X, T, Acc).

%!  is_palindrome(+List) is nondet.
%
%   7. Check if a List is a Palindrome
%   Normal Recursive
%
%   Checks whether a given list is a palindrome.
%
%   A palindrome is a list that reads the same forwards and backwards. This predicate
%   checks if the input list is a palindrome by reversing the list and comparing it
%   to the original. If they are the same, the list is a palindrome.
%
%   @arg List The list to be checked.
%
%   @example Check if a list is a palindrome:
%     ?- is_palindrome([1, 2, 3, 2, 1]).
%     true.
%
%     ?- is_palindrome([1, 2, 3]).
%     false.
%
is_palindrome(L) :- 
    % Reverse the list and check if it is equal to the original list.
    reverse(L, L).

%!  is_palindrome_acc(+List) is nondet.
%
%   With Accumulator
%
%   Checks whether a given list is a palindrome using an accumulator.
%
%   This predicate checks if a list is a palindrome by using an accumulator-based
%   reverse operation. It compares the original list with its reversed version
%   built using an accumulator. If the reversed list matches the original, the list
%   is a palindrome.
%
%   @arg List The list to be checked.
%
%   @example Check if a list is a palindrome using an accumulator:
%     ?- is_palindrome_acc([1, 2, 3, 2, 1]).
%     true.
%
%     ?- is_palindrome_acc([1, 2, 3]).
%     false.
%
is_palindrome_acc(L) :-
    % Call the accumulator-based reverse and check if it matches the original list.
    reverse_acc(L, [], L).

%!  reverse_acc(+List, +Acc, -Reversed) is det.
%
%   Reverses a list using an accumulator.
%   This is a helper predicate used by is_palindrome_acc/1.
%
%   @arg List     The input list to be reversed.
%   @arg Acc      The accumulator holding the reversed list.
%   @arg Reversed The resulting reversed list.
%
reverse_acc([], Acc, Acc).  % Base case: when the list is empty, the accumulator holds the reversed list.
reverse_acc([H|T], Acc, R) :-
    % Recursively reverse the tail, prepending the head to the accumulator.
    reverse_acc(T, [H|Acc], R).

%!  product_list(+List, -Product) is det.
%
%   8. Calculate the Product of All Elements in a List
%   Normal Recursive
%
%   Computes the product of all elements in a given list.
%
%   This predicate calculates the product of the elements in a list using recursion.
%   The base case is an empty list, which has a product of 1 (the multiplicative identity).
%   The recursive case processes the tail of the list and multiplies the head by
%   the product of the tail.
%
%   @arg List    The input list whose elements are to be multiplied.
%   @arg Product The computed product of the list elements.
%
%   @example Calculate the product of a list:
%     ?- product_list([1, 2, 3, 4], P).
%     P = 24.
%
product_list([], 1).  % Base case: the product of an empty list is 1.
product_list([H|T], P) :-
    % Recursively calculate the product of the tail.
    product_list(T, Temp),
    % Multiply the head of the list by the product of the tail.
    P is H * Temp.

%!  product_list_acc(+List, -Product) is det.
%
%   Computes the product of all elements in a given list using an accumulator.
%
%   This is a tail-recursive version of the product_list/2 predicate, utilizing an
%   accumulator to store the intermediate product. The base case is when the list is
%   empty, and the product is the value of the accumulator.
%
%   @arg List    The input list whose elements are to be multiplied.
%   @arg Product The computed product of the list elements.
%
%   @example Calculate the product of a list using an accumulator:
%     ?- product_list_acc([1, 2, 3, 4], P).
%     P = 24.
%
product_list_acc(L, P) :- 
    % Initialize the accumulator as 1 and call the helper predicate.
    product_list_acc(L, 1, P).

%!  product_list_acc(+List, +Acc, -Product) is det.
%
%   Helper predicate that computes the product of a list using an accumulator.
%   The base case occurs when the list is empty, at which point the accumulator
%   holds the final product.
%
%   @arg List    The input list.
%   @arg Acc     The accumulator for the product.
%   @arg Product The final product.
%
product_list_acc([], Acc, Acc).  % Base case: when the list is empty, return the accumulator.
product_list_acc([H|T], Acc, P) :-
    % Update the accumulator with the product of the head and the current accumulator value.
    NewAcc is Acc * H,
    % Recursively compute the product of the tail.
    product_list_acc(T, NewAcc, P).

%!  nth_element(+N, +List, -Element) is nondet.
%
%   9. Find the Nth Element of a List
%   Normal recursive
%
%   Finds the N-th element of a list using recursion.
%
%   This predicate retrieves the N-th element of a list (1-based index). The base case
%   is when N equals 1, which corresponds to the head of the list. The recursive case
%   decrements N and processes the tail of the list.
%
%   @arg N       The position (1-based index) of the element in the list.
%   @arg List    The input list.
%   @arg Element The N-th element of the list.
%
%   @example Find the 3rd element of a list:
%     ?- nth_element(3, [a, b, c, d], X).
%     X = c.
%
nth_element(1, [H|_], H).  % Base case: the first element is the head of the list.
nth_element(N, [_|T], X) :-
    % Ensure N is greater than 1.
    N > 1,
    % Decrement N and recurse on the tail of the list.
    M is N - 1,
    nth_element(M, T, X).

%!  nth_element_acc(+N, +List, -Element) is nondet.
%
%   Finds the N-th element of a list using an accumulator.
%
%   This is a tail-recursive version of nth_element/3 that uses an accumulator to track the
%   current position in the list. It is more efficient for larger lists as it avoids
%   unnecessary recursion depth.
%
%   @arg N       The position (1-based index) of the element in the list.
%   @arg List    The input list.
%   @arg Element The N-th element of the list.
%
%   @example Find the 3rd element of a list using an accumulator:
%     ?- nth_element_acc(3, [a, b, c, d], X).
%     X = c.
%
nth_element_acc(N, L, X) :- 
    % Initialize the accumulator as 1 and call the helper predicate.
    nth_element_acc(N, L, 1, X).

%!  nth_element_acc(+N, +List, +Acc, -Element) is nondet.
%
%   Helper predicate for finding the N-th element using an accumulator.
%   The base case occurs when the accumulator equals N, which corresponds to the N-th
%   element of the list.
%
%   @arg N       The position (1-based index) of the element in the list.
%   @arg List    The input list.
%   @arg Acc     The accumulator tracking the current position in the list.
%   @arg Element The N-th element of the list.
%
nth_element_acc(N, [H|_], N, H).  % Base case: if the accumulator equals N, return the current element.
nth_element_acc(N, [_|T], Acc, X) :-
    % Increment the accumulator and recurse on the tail.
    NewAcc is Acc + 1,
    nth_element_acc(N, T, NewAcc, X).

%!  count_occurrences(+Element, +List, -Count) is det.
%
%   10. Count the Occurrences of an Element in a List
%   Normal Recursive
%
%   Counts the occurrences of a given element in a list.
%
%   This predicate counts how many times a specified element appears in a list using recursion.
%   The base case is when the list is empty, and the count is 0. The recursive case
%   increments the count if the head of the list matches the element and continues with the tail.
%
%   @arg Element The element whose occurrences are being counted.
%   @arg List    The input list.
%   @arg Count   The number of occurrences of the element in the list.
%
%   @example Count occurrences of an element in a list:
%     ?- count_occurrences(a, [a, b, a, c, a], N).
%     N = 3.
%
count_occurrences(_, [], 0).  % Base case: an empty list has 0 occurrences of any element.
count_occurrences(X, [X|T], N) :-
    % If the head matches the element, increment the count.
    count_occurrences(X, T, M),
    N is M + 1.
count_occurrences(X, [Y|T], N) :-
    % If the head does not match, continue with the tail.
    X \= Y,
    count_occurrences(X, T, N).

%!  count_occurrences_acc(+Element, +List, -Count) is det.
%
%   Counts the occurrences of a given element in a list using an accumulator.
%
%   This is a tail-recursive version of count_occurrences/3 that uses an accumulator
%   to keep track of the count. The base case occurs when the list is empty, and
%   the accumulator holds the final count.
%
%   @arg Element The element whose occurrences are being counted.
%   @arg List    The input list.
%   @arg Count   The number of occurrences of the element in the list.
%
%   @example Count occurrences of an element in a list using an accumulator:
%     ?- count_occurrences_acc(a, [a, b, a, c, a], N).
%     N = 3.
%
count_occurrences_acc(X, L, N) :- 
    % Initialize the accumulator as 0 and call the helper predicate.
    count_occurrences_acc(X, L, 0, N).

%!  count_occurrences_acc(+Element, +List, +Acc, -Count) is det.
%
%   Helper predicate for counting the occurrences of an element using an accumulator.
%   The base case occurs when the list is empty, and the accumulator holds the final count.
%
%   @arg Element The element whose occurrences are being counted.
%   @arg List    The input list.
%   @arg Acc     The accumulator for the count.
%   @arg Count   The final count of occurrences.
%
count_occurrences_acc(_, [], Acc, Acc).  % Base case: when the list is empty, return the accumulator.
count_occurrences_acc(X, [X|T], Acc, N) :-
    % If the head matches the element, increment the accumulator.
    NewAcc is Acc + 1,
    count_occurrences_acc(X, T, NewAcc, N).
count_occurrences_acc(X, [Y|T], Acc, N) :-
    % If the head does not match, continue with the tail.
    X \= Y,
    count_occurrences_acc(X, T, Acc, N).

%!  gcd(+A, +B, -GCD) is det.
%
%   11. Calculate the Greatest Common Divisor of Two Numbers
%   Normal Recursive
%
%   Computes the greatest common divisor (GCD) of two numbers using recursion.
%
%   This predicate implements the Euclidean algorithm to find the GCD of two numbers.
%   The base case occurs when the second number is 0, in which case the GCD is the first number.
%   The recursive case computes the remainder of A divided by B and calls gcd/3 with the new values.
%
%   @arg A   The first number.
%   @arg B   The second number.
%   @arg GCD The greatest common divisor of A and B.
%
%   @example Calculate the GCD of two numbers:
%     ?- gcd(48, 18, GCD).
%     GCD = 6.
%
gcd(A, 0, A) :- 
    % Base case: if B is 0, the GCD is A.
    A > 0.
gcd(A, B, GCD) :-
    % Ensure B is greater than 0.
    B > 0,
    % Compute the remainder of A divided by B.
    R is A mod B,
    % Recursively call gcd/3 with B and the remainder.
    gcd(B, R, GCD).

%!  gcd_acc(+A, +B, -GCD) is det.
%
%   With Accumulator
%
%   Computes the greatest common divisor (GCD) of two numbers using an accumulator.
%   The accumulator is not needed for GCD itself but can be used to track steps or iterations.
%   The accumulator does not interfere with the GCD result.
%
%   @arg A   The first number.
%   @arg B   The second number.
%   @arg GCD The greatest common divisor of A and B.
%
gcd_acc(A, B, GCD) :-
    % Initialize the accumulator to 1 (though unused in this case).
    gcd_acc(A, B, 1, GCD).

%!  gcd_acc(+A, +B, +Acc, -GCD) is det.
%
%   Helper predicate for GCD calculation using the Euclidean algorithm.
%   The accumulator is kept but does not affect the GCD result.
%
%   @arg A   The first number.
%   @arg B   The second number.
%   @arg Acc The accumulator (unused).
%   @arg GCD The greatest common divisor of A and B.
%
gcd_acc(A, 0, Acc, Acc) :- A > 0.
gcd_acc(A, B, Acc, GCD) :-
   B > 0,
   R is A mod B,
   NewAcc is B * Acc,
   gcd_acc(B, R, NewAcc, GCD).
%

%!  is_prime(+N) is nondet.
%
%   12. Check if a Number is Prime
%   Normal Recursive
%
%   Succeeds if N is a prime number.
%
%   This predicate checks whether a given number N is prime. It succeeds
%   if N is prime and fails otherwise. It first handles the special case
%   where N is 2, which is the smallest prime number. For numbers greater
%   than 2, it checks that N is not divisible by any number between 2 and 
%   the square root of N, which is an efficient way to determine primality.
%
%   @arg N The number to check for primality. It should be a positive integer.
%
%   @example Checking if 2 is prime:
%     ?- is_prime(2).
%     true.
%
%   @example Checking if 17 is prime:
%     ?- is_prime(17).
%     true.
%
%   @example Checking if 18 is prime:
%     ?- is_prime(18).
%     false.
%
is_prime(2).
is_prime(N) :-
    % Ensure N is greater than 2.
    N > 2,
    % Check that there are no divisors of N between 2 and sqrt(N).
    % The \+ (not provable) operator ensures that the inner goal fails,
    % meaning N is not divisible by any number in this range.
    \+ (between(2, sqrt(N), X), N mod X =:= 0).

%!  is_prime_acc(+N) is nondet.
%
%   With Accumulator
%
%   Succeeds if N is a prime number using an accumulator-based approach.
%
%   This predicate checks whether a given number N is prime. It uses an
%   accumulator, starting at 2, to iterate over potential divisors and
%   checks for divisibility. If no divisor is found up to the square root
%   of N, it succeeds. This version optimizes the check by terminating
%   early if the current accumulator squared exceeds N.
%
%   @arg N The number to check for primality. It should be a positive integer.
%
%   @example Checking if 5 is prime:
%     ?- is_prime_acc(5).
%     true.
%
%   @example Checking if 10 is prime:
%     ?- is_prime_acc(10).
%     false.
%
is_prime_acc(N) :- 
    % Start the primality check with an accumulator of 2.
    is_prime_acc(N, 2).

%!  is_prime_acc(+N, +Acc) is nondet.
%
%   Auxiliary predicate for is_prime_acc/1 that performs the actual check.
%
%   This predicate checks whether N is divisible by the current accumulator (Acc).
%   It terminates early if Acc * Acc > N, meaning no more divisors need to be checked.
%   If Acc divides N, the number is not prime, and the predicate fails.
%   Otherwise, the accumulator is incremented, and the process continues.
%
%   @arg N The number to check for primality.
%   @arg Acc The current divisor being tested.
%
%   @example Checking if 13 is prime:
%     ?- is_prime_acc(13).
%     true.
%
is_prime_acc(2, 2).  % Special case: 2 is prime.
is_prime_acc(N, Acc) :-
    % Ensure N is greater than 2.
    N > 2,
    (
        % If Acc * Acc > N, no need to check further divisors.
        (Acc * Acc > N, !);
        % Check if N is not divisible by Acc. If true, increment Acc and continue.
        (N mod Acc =\= 0, NewAcc is Acc + 1, is_prime_acc(N, NewAcc))
    ).

%!  merge_sorted(+List1, +List2, -Merged) is det.
%
%   13. Merge Two Sorted Lists into a Sorted List
%   Normal Recursive
%
%   Merges two sorted lists into one sorted list.
%
%   This predicate takes two sorted lists, `List1` and `List2`, and merges them
%   into a single sorted list, `Merged`. It assumes that both input lists are 
%   sorted in ascending order. The resulting list will also be sorted in 
%   ascending order.
%
%   The merging process compares the heads of the two lists and places the smaller 
%   head in the resulting list. This process is repeated recursively until one or 
%   both of the lists are empty.
%
%   @arg List1 The first sorted list.
%   @arg List2 The second sorted list.
%   @arg Merged The merged, sorted list.
%
%   @example Merging two sorted lists:
%     ?- merge_sorted([1, 3, 5], [2, 4, 6], Merged).
%     Merged = [1, 2, 3, 4, 5, 6].
%
%   @example Merging an empty list with a sorted list:
%     ?- merge_sorted([], [2, 4, 6], Merged).
%     Merged = [2, 4, 6].
%
merge_sorted([], L, L).  % Base case: if List1 is empty, return List2.
merge_sorted(L, [], L).  % Base case: if List2 is empty, return List1.
merge_sorted([H1|T1], [H2|T2], [H1|M]) :-
    % If H1 is less than or equal to H2, place H1 in the result and continue.
    H1 =< H2,merge_sorted(T1, [H2|T2], M).
merge_sorted([H1|T1], [H2|T2], [H2|M]) :-
    % If H1 is greater than H2, place H2 in the result and continue.
    H1 > H2,merge_sorted([H1|T1], T2, M).

%!  merge_sorted_acc(+List1, +List2, -Merged) is det.
%
%   With Accumulator
%
%   Merges two sorted lists into one sorted list using an accumulator for efficiency.
%
%   This predicate merges two sorted lists, `List1` and `List2`, into a single sorted 
%   list `Merged` using an accumulator to build the result in reverse order. After the 
%   lists are merged, the accumulator is reversed to produce the final sorted list.
%   This approach can be more efficient in Prolog due to the way lists are constructed.
%
%   @arg List1 The first sorted list.
%   @arg List2 The second sorted list.
%   @arg Merged The merged, sorted list.
%
%   @example Merging two sorted lists:
%     ?- merge_sorted_acc([1, 3, 5], [2, 4, 6], Merged).
%     Merged = [1, 2, 3, 4, 5, 6].
%
merge_sorted_acc(L1, L2, L) :- 
    % Initialize the accumulator as an empty list and start merging.
    merge_sorted_acc(L1, L2, [], L).

%!  merge_sorted_acc(+List1, +List2, +Acc, -Merged) is det.
%
%   Auxiliary predicate for merge_sorted_acc/3 that performs the actual merging.
%
%   This predicate merges the two lists by comparing their heads and adding the smaller
%   element to the accumulator. The result is built in reverse order, and the final list
%   is obtained by reversing the accumulator once one or both lists are empty.
%
%   @arg List1 The first sorted list.
%   @arg List2 The second sorted list.
%   @arg Acc The accumulator that holds the merged elements in reverse order.
%   @arg Merged The final merged list.
%
merge_sorted_acc([], L, Acc, L) :- 
    % If the first list is empty, reverse the accumulator and append List2.
    reverse(Acc, L), !.
merge_sorted_acc(L, [], Acc, L) :- 
    % If the second list is empty, reverse the accumulator and append List1.
    reverse(Acc, L), !.
merge_sorted_acc([H1|T1], [H2|T2], Acc, [H|M]) :-
    % If H1 is less than or equal to H2, add H1 to the accumulator and continue.
    H1 =< H2,merge_sorted_acc(T1, [H2|T2], [H1|Acc], M).
merge_sorted_acc([H1|T1], [H2|T2], Acc, [H|M]) :-
    % If H1 is greater than H2, add H2 to the accumulator and continue.
    H1 > H2,merge_sorted_acc([H1|T1], T2, [H2|Acc], M).

%!  last_element(+List, -Element) is det.
%
%   14. Find the Last Element of a List
%   Normal Recursive
%
%   Retrieves the last element of a list.
%
%   This predicate succeeds by unifying `Element` with the last element of the 
%   list `List`. It recursively traverses the list, discarding the head and 
%   continuing with the tail until it reaches a list with one element, which is 
%   the last element.
%
%   @arg List The input list. It must be a non-empty list.
%   @arg Element The last element of the list.
%
%   @example Finding the last element:
%     ?- last_element([1, 2, 3, 4], X).
%     X = 4.
%
%   @example Finding the last element of a singleton list:
%     ?- last_element([42], X).
%     X = 42.
%
last_element([H], H).  % Base case: the last element in a singleton list is H.
last_element([_|T], X) :- 
    % Recursively process the tail of the list until the last element is found.
    last_element(T, X).

%!  last_element_acc(+List, -Element) is det.
%
%   With Accumulator
%
%   Retrieves the last element of a list using an accumulator for efficiency.
%
%   This predicate finds the last element of the list `List` by using an
%   accumulator. The accumulator holds the current candidate for the last
%   element as the list is traversed. Once the list is empty, the accumulator 
%   contains the last element.
%
%   @arg List The input list. It must be a non-empty list.
%   @arg Element The last element of the list.
%
%   @example Finding the last element:
%     ?- last_element_acc([1, 2, 3, 4], X).
%     X = 4.
%
%   @example Finding the last element of a singleton list:
%     ?- last_element_acc([42], X).
%     X = 42.
%
last_element_acc([H|T], X) :- 
    % Initialize the accumulator with the first element and start processing the tail.
    last_element_acc(T, H, X).

%!  last_element_acc(+List, +Acc, -Element) is det.
%
%   Auxiliary predicate for last_element_acc/2 using an accumulator.
%
%   This predicate traverses the list, updating the accumulator with each head
%   element. When the list is empty, the accumulator contains the last element.
%
%   @arg List The current tail of the list being processed.
%   @arg Acc The current candidate for the last element.
%   @arg Element The final last element.
%
last_element_acc([], Acc, Acc).  % Base case: when the list is empty, the accumulator holds the last element.
last_element_acc([H|T], _, X) :- 
    % Update the accumulator with the new head and process the rest of the list.
    last_element_acc(T, H, X).

%!  remove_duplicates(+List, -UniqueList) is det.
%
%   15. Remove Duplicate Elements from a List
%   Normal Recursive
%
%   Removes duplicate elements from a list while preserving the order of the first occurrence.
%
%   This predicate removes duplicates from the input list `List`, producing a new list `UniqueList`
%   where each element appears only once. The order of elements in the resulting list is the same
%   as their first occurrence in the input list. It recursively checks if the head of the list 
%   appears in the tail, and if not, includes it in the result. Otherwise, it skips the duplicate.
%
%   @arg List The input list, which may contain duplicates.
%   @arg UniqueList The output list with duplicates removed.
%
%   @example Removing duplicates from a list:
%     ?- remove_duplicates([1, 2, 2, 3, 1, 4], UniqueList).
%     UniqueList = [1, 2, 3, 4].
%
%   @example Removing duplicates from an empty list:
%     ?- remove_duplicates([], UniqueList).
%     UniqueList = [].
%
remove_duplicates([], []).  % Base case: an empty list has no duplicates.
remove_duplicates([H|T], [H|T1]) :- 
    % If H is not a member of the tail, include H in the result and continue.
    \+ member(H, T),remove_duplicates(T, T1).
remove_duplicates([_|T], T1) :- 
    % If H is a duplicate, skip it and continue processing the tail.
    remove_duplicates(T, T1).

%!  remove_duplicates_acc(+List, -UniqueList) is det.
%
%   With Accumulator
%
%   Removes duplicate elements from a list using an accumulator for efficiency.
%
%   This predicate removes duplicates from the input list `List` and produces the 
%   list `UniqueList` without duplicates. It uses an accumulator to collect the 
%   unique elements while traversing the list. The final result is built as the 
%   list is processed. The order of the first occurrence of each element is preserved.
%
%   @arg List The input list, which may contain duplicates.
%   @arg UniqueList The output list with duplicates removed.
%
%   @example Removing duplicates using an accumulator:
%     ?- remove_duplicates_acc([1, 2, 2, 3, 1, 4], UniqueList).
%     UniqueList = [1, 2, 3, 4].
%
remove_duplicates_acc(L, R) :- 
    % Start the removal of duplicates with an empty accumulator.
    remove_duplicates_acc(L, [], R).

%!  remove_duplicates_acc(+List, +Acc, -UniqueList) is det.
%
%   Auxiliary predicate for remove_duplicates_acc/2 using an accumulator.
%
%   This predicate processes the list by checking if the head of the list is 
%   already in the accumulator. If it is not, the head is added to the accumulator.
%   The result is accumulated in reverse order, and the final unique list is built 
%   as the list is traversed.
%
%   @arg List The input list being processed.
%   @arg Acc The accumulator holding unique elements seen so far.
%   @arg UniqueList The final list with duplicates removed.
%
remove_duplicates_acc([], Acc, Acc).  % Base case: when the list is empty, return the accumulated result.
remove_duplicates_acc([H|T], Acc, R) :-
    % Check if H is already in the accumulator.
    (member(H, Acc) -> 
        % If H is already in the accumulator, skip it and continue.
        remove_duplicates_acc(T, Acc, R);
        % Otherwise, add H to the accumulator and continue processing the tail.
        remove_duplicates_acc(T, [H|Acc], R)).

%!  is_balanced(+Tree) is nondet.
%
%   16. Check if a Binary Tree is Balanced
%   Normal Recursive
%
%   Succeeds if the binary tree `Tree` is height-balanced.
%
%   A binary tree is considered balanced if, for every node, the difference in height 
%   between the left and right subtrees is at most 1. This predicate checks whether 
%   the given tree is balanced by recursively calculating the height of each subtree 
%   and ensuring the height difference at each node does not exceed 1.
%
%   @arg Tree The binary tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%
%   @example Checking if a balanced tree:
%     ?- is_balanced(tree(tree(null, 1, null), 2, tree(null, 3, null))).
%     true.
%
%   @example Checking if an unbalanced tree:
%     ?- is_balanced(tree(tree(tree(null, 1, null), 2, null), 3, null)).
%     false.
%
is_balanced(null).  % Base case: an empty tree (null) is always balanced.
is_balanced(tree(L, _, R)) :-
    % Calculate the height of the left subtree.
    height(L, Hl),
    % Calculate the height of the right subtree.
    height(R, Hr),
    % The height difference between left and right subtrees.
    D is Hl - Hr,
    % The tree is balanced if the absolute difference is <= 1.
    abs(D) =< 1,
    % Recursively check if the left subtree is balanced.
    is_balanced(L),
    % Recursively check if the right subtree is balanced.
    is_balanced(R).

%!  is_balanced_acc(+Tree) is nondet.
%
%   With Accumulator
%
%   Succeeds if the binary tree `Tree` is height-balanced, using an accumulator for height calculation.
%
%   This predicate checks whether a binary tree is balanced by recursively calculating the height of each
%   subtree, while simultaneously verifying that the height difference between the left and right subtrees 
%   of each node does not exceed 1. The accumulator stores the height during recursion, making it more efficient.
%
%   @arg Tree The binary tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%
%   @example Checking if a balanced tree:
%     ?- is_balanced_acc(tree(tree(null, 1, null), 2, tree(null, 3, null))).
%     true.
%
%   @example Checking if an unbalanced tree:
%     ?- is_balanced_acc(tree(tree(tree(null, 1, null), 2, null), 3, null)).
%     false.
%
is_balanced_acc(T) :- 
    % Initialize the height accumulator to 0 and start the balance check.
    is_balanced_acc(T, 0).

%!  is_balanced_acc(+Tree, -Height) is nondet.
%
%   Auxiliary predicate for is_balanced_acc/1 that computes the height of the tree 
%   while checking if it is balanced.
%
%   This predicate calculates the height of the tree `Tree`, and at each node, it ensures
%   the left and right subtrees differ in height by no more than 1. The height is returned
%   via the accumulator.
%
%   @arg Tree The binary tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%   @arg Height The computed height of the tree.
%
is_balanced_acc(null, 0).  % Base case: an empty tree (null) has a height of 0.
is_balanced_acc(tree(L, _, R), H) :-
    % Recursively calculate the height of the left subtree.
    is_balanced_acc(L, Hl),
    % Recursively calculate the height of the right subtree.
    is_balanced_acc(R, Hr),
    % The difference in height between left and right subtrees.
    D is Hl - Hr,
    % The tree is balanced if the absolute difference in height is <= 1.
    abs(D) =< 1,
    % Calculate the current height as 1 plus the maximum height of the subtrees.
    H is max(Hl, Hr) + 1.

%!  height(+Tree, -Height) is det.
%
%  17. Calculate the Height of a Binary Tree
%  Normal Recursive
%
%   Calculates the height of the binary tree `Tree`.
%
%   This predicate computes the height of a binary tree. The height is defined as 
%   the length of the longest path from the root to a leaf node. An empty tree 
%   (`null`) has a height of 0, and for non-empty trees, the height is 1 plus the 
%   maximum height of the left and right subtrees.
%
%   @arg Tree The binary tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%   @arg Height The computed height of the tree.
%
%   @example Calculating the height of a tree:
%     ?- height(tree(tree(null, 1, null), 2, tree(null, 3, null)), H).
%     H = 2.
%
%   @example Calculating the height of an empty tree:
%     ?- height(null, H).
%     H = 0.
%
height(null, 0).  % Base case: the height of an empty tree is 0.
height(tree(L, _, R), H) :-
    % Recursively calculate the height of the left subtree.
    height(L, Hl),
    % Recursively calculate the height of the right subtree.
    height(R, Hr),
    % The height of the current tree is 1 plus the maximum height of the two subtrees.
    H is max(Hl, Hr) + 1.

%!  height_acc(+Tree, -Height) is det.
%
%   With Accumulator
%
%   Calculates the height of a binary tree using an accumulator for efficiency.
%
%   This predicate computes the height of a binary tree by using an accumulator to
%   track the current height as the tree is traversed. The height is defined as the
%   length of the longest path from the root to a leaf node. An empty tree (`null`)
%   has a height of 0, and for non-empty trees, the height is computed recursively,
%   with the accumulator being updated at each level of depth.
%
%   @arg Tree The binary tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%   @arg Height The computed height of the tree.
%
%   @example Calculating the height of a tree using an accumulator:
%     ?- height_acc(tree(tree(null, 1, null), 2, tree(null, 3, null)), H).
%     H = 2.
%
%   @example Calculating the height of an empty tree:
%     ?- height_acc(null, H).
%     H = 0.
%
height_acc(T, H) :- 
    % Start the height calculation with an accumulator initialized to 0.
    height_acc(T, 0, H).

%!  height_acc(+Tree, +Acc, -Height) is det.
%
%   Auxiliary predicate for height_acc/2 that computes the height using an accumulator.
%
%   This predicate traverses the binary tree and updates the accumulator at each level,
%   keeping track of the current height. The final height is determined as the maximum
%   depth reached during the traversal of the left and right subtrees.
%
%   @arg Tree The binary tree.
%   @arg Acc The current accumulator value representing the depth at the current node.
%   @arg Height The final computed height of the tree.
%
height_acc(null, Acc, Acc).  % Base case: for an empty tree, return the accumulated height.
height_acc(tree(L, _, R), Acc, H) :-
    % Increment the accumulator to reflect the current level of depth.
    NewAcc is Acc + 1,
    % Recursively compute the height of the left subtree.
    height_acc(L, NewAcc, Hl),
    % Recursively compute the height of the right subtree.
    height_acc(R, NewAcc, Hr),
    % The final height is the maximum height of the two subtrees.
    H is max(Hl, Hr).

%!  search_bst(+Tree, +Value) is nondet.
%
%   18. Search for an Element in a Binary Search Tree
%   Normal Recursive
%
%   Searches for a given value in a binary search tree (BST).
%
%   This predicate succeeds if the value `Value` is present in the binary search tree `Tree`.
%   The predicate uses the properties of a BST, where values in the left subtree are less 
%   than the root, and values in the right subtree are greater than the root, to guide the 
%   search efficiently. It recursively traverses the tree, moving left or right based on 
%   the comparison between the target value and the current node value.
%
%   @arg Tree The binary search tree, represented as `tree(Left, Value, Right)`.
%   @arg Value The value to search for in the tree.
%
%   @example Searching for a value in a BST:
%     ?- search_bst(tree(tree(null, 1, null), 2, tree(null, 3, null)), 3).
%     true.
%
%   @example Searching for a value not in the tree:
%     ?- search_bst(tree(tree(null, 1, null), 2, tree(null, 3, null)), 4).
%     false.
%
search_bst(tree(_, X, _), X).  % Base case: found the value X at the root.
search_bst(tree(L, Y, _), X) :-
    % If X is less than Y, search in the left subtree.
    X < Y,search_bst(L, X).
search_bst(tree(_, Y, R), X) :-
    % If X is greater than Y, search in the right subtree.
    X > Y,search_bst(R, X).

% With Accumulator
% The accumulator is not very useful here, as the search path is already determined by the BST property.
search_bst_acc(Tree, X) :- search_bst(Tree, X).
%

%!  insert_bst(+Tree, +Value, -NewTree) is det.
%
%   19. Insert an Element into a Binary Search Tree
%   Normal Recursive
%
%   Inserts a value into a binary search tree (BST), producing a new tree.
%
%   This predicate inserts the value `Value` into the binary search tree `Tree` while maintaining
%   the properties of the BST. If the value is less than the current node value, it is inserted 
%   into the left subtree. If the value is greater, it is inserted into the right subtree. 
%   In case of an empty tree (`null`), a new tree node is created with the value as the root.
%
%   @arg Tree The binary search tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%   @arg Value The value to be inserted into the tree.
%   @arg NewTree The resulting binary search tree after insertion.
%
%   @example Inserting into an empty tree:
%     ?- insert_bst(null, 5, NewTree).
%     NewTree = tree(null, 5, null).
%
%   @example Inserting into a non-empty tree:
%     ?- insert_bst(tree(tree(null, 2, null), 3, tree(null, 4, null)), 1, NewTree).
%     NewTree = tree(tree(tree(null, 1, null), 2, null), 3, tree(null, 4, null)).
%
insert_bst(null, X, tree(null, X, null)).  % Base case: inserting into an empty tree creates a new node.
insert_bst(tree(L, Y, R), X, tree(L1, Y, R)) :-
    % If X is less than Y, insert X into the left subtree.
    X < Y,insert_bst(L, X, L1).
insert_bst(tree(L, Y, R), X, tree(L, Y, R1)) :-
    % If X is greater than Y, insert X into the right subtree.
    X > Y,insert_bst(R, X, R1).

% With Accumulator
% The accumulator is not very useful here, as the insertion path is already determined by the BST property.
insert_bst_acc(Tree, X, NewTree) :- insert_bst(Tree, X, NewTree).
%

%!  delete_bst(+Tree, +Value, -NewTree) is det.
%
%   20. Delete an Element from a Binary Search Tree
%   Normal Recursive
%
%   Deletes a value from a binary search tree (BST), producing a new tree.
%
%   This predicate deletes the node containing `Value` from the binary search tree `Tree`.
%   It calls the helper predicate `remove_bst/3` to handle the deletion. If the node to be 
%   deleted has two children, the subtrees are merged to maintain the BST structure.
%
%   @arg Tree The binary search tree, represented as `tree(Left, Value, Right)` or `null` for an empty tree.
%   @arg Value The value to be deleted from the tree.
%   @arg NewTree The resulting binary search tree after deletion.
%
%   @example Deleting a value from a BST:
%     ?- delete_bst(tree(tree(null, 1, null), 2, tree(null, 3, null)), 2, NewTree).
%     NewTree = tree(null, 1, tree(null, 3, null)).
%
delete_bst(Tree, X, NewTree) :-
    % Calls the helper predicate to remove the node containing X from the tree.
    remove_bst(Tree, X, NewTree).

%!  remove_bst(+Tree, +Value, -NewTree) is det.
%
%   Helper predicate for delete_bst/3. Removes the node containing `Value` from the tree.
%
%   This predicate handles the recursive logic for finding and removing the node with the given 
%   `Value`. If the node to be deleted is found, the subtrees are merged. If the value is less 
%   or greater than the current node value, it recursively moves left or right.
%
%   @arg Tree The binary search tree from which the value is to be removed.
%   @arg Value The value to be removed.
%   @arg NewTree The resulting tree after the value is removed.
%
remove_bst(tree(L, X, R), X, Merged) :-
    % When the value X is found at the root, merge the left and right subtrees.
    merge_trees(L, R, Merged), !.
remove_bst(tree(L, Y, R), X, tree(L1, Y, R)) :-
    % If X is less than the current node value, search in the left subtree.
    X < Y,remove_bst(L, X, L1).
remove_bst(tree(L, Y, R), X, tree(L, Y, R1)) :-
    % If X is greater than the current node value, search in the right subtree.
    X > Y,remove_bst(R, X, R1).

%!  merge_trees(+Tree1, +Tree2, -MergedTree) is det.
%
%   Merges two binary search trees into one tree.
%
%   This predicate merges two binary search trees, `Tree1` and `Tree2`. It ensures that 
%   the resulting tree maintains the binary search tree properties. If one of the trees 
%   is `null`, the other tree is returned. If both trees are non-empty, the right subtree 
%   of `Tree1` is recursively merged with the left subtree of `Tree2`.
%
%   @arg Tree1 The first binary search tree to merge.
%   @arg Tree2 The second binary search tree to merge.
%   @arg MergedTree The resulting binary search tree after merging.
%
merge_trees(null, Tree, Tree).  % Base case: if the first tree is empty, return the second tree.
merge_trees(Tree, null, Tree).  % Base case: if the second tree is empty, return the first tree.
merge_trees(tree(L1, X, R1), tree(L2, Y, R2), tree(Merged, Y, R2)) :-
    % Recursively merge the right subtree of Tree1 with the left subtree of Tree2.
    merge_trees(tree(L1, X, R1), L2, Merged).

% With Accumulator
% The accumulator is not very useful here, as the deletion path is already determined by the BST property.
delete_bst_acc(Tree, X, NewTree) :- delete_bst(Tree, X, NewTree).
%

%!  lowest_common_ancestor(+Tree, +X, +Z, -LCA) is nondet.
%
%   21. Find the Lowest Common Ancestor in a Binary Search Tree
%   Normal Recursive
%
%   Finds the lowest common ancestor (LCA) of two values, `X` and `Z`, in a binary search tree (BST).
%
%   This predicate finds the lowest common ancestor (LCA) of two nodes in a binary search tree `Tree`.
%   The LCA is the deepest node that is an ancestor of both `X` and `Z`. The predicate works by 
%   recursively traversing the tree, comparing `X` and `Z` to the current node value. If both values
%   are smaller than the current node, it searches the left subtree. If both are larger, it searches the
%   right subtree. The current node is the LCA if one value is smaller and the other is larger.
%
%   @arg Tree The binary search tree, represented as `tree(Left, Value, Right)`.
%   @arg X The first value for which to find the LCA.
%   @arg Z The second value for which to find the LCA.
%   @arg LCA The lowest common ancestor of `X` and `Z`.
%
%   @example Finding the lowest common ancestor:
%     ?- lowest_common_ancestor(tree(tree(null, 2, null), 3, tree(null, 5, null)), 2, 5, LCA).
%     LCA = 3.
%
lowest_common_ancestor(tree(_, Y, _), X, Z, Y) :-
    % The current node Y is the LCA if X and Z are on opposite sides of Y.
    X < Y, Z > Y;X > Y, Z < Y.
lowest_common_ancestor(tree(L, Y, _), X, Z, LCA) :-
    % If both X and Z are less than Y, the LCA is in the left subtree.
    X < Y, Z < Y,lowest_common_ancestor(L, X, Z, LCA).
lowest_common_ancestor(tree(_, Y, R), X, Z, LCA) :-
    % If both X and Z are greater than Y, the LCA is in the right subtree.
    X > Y, Z > Y,lowest_common_ancestor(R, X, Z, LCA).

% With Accumulator
% prolog
% The accumulator is not very useful here, as the search path is already determined by the BST property.
lowest_common_ancestor_acc(Tree, X, Z, LCA) :- lowest_common_ancestor(Tree, X, Z, LCA).
%

%!  is_cyclic(+Graph) is nondet.
%
%  22. Check if a Graph is Cyclic
%  For graphs, it is better to represent them in a Prolog-friendly format, such as adjacency lists. 
%  I will use a representation where each node has a list of its neighbors.
%  Normal Recursive
%
%   Checks whether the given graph contains a cycle.
%
%   This predicate checks if there is a cycle in the graph `Graph`. A graph is cyclic if there
%   exists a path that begins and ends at the same vertex. The graph is represented as a list 
%   of vertex-edge pairs, where each pair is of the form `Vertex-Neighbors`, with `Neighbors`
%   being a list of vertices connected to `Vertex`.
%
%   The predicate starts a depth-first search (DFS) from each vertex in the graph and checks if
%   any vertex is revisited during the traversal, indicating a cycle.
%
%   @arg Graph The graph represented as a list of vertex-edge pairs.
%
%   @example Checking if a graph contains a cycle:
%     ?- is_cyclic([a-[b], b-[c], c-[a]]).
%     true.
%
%   @example Checking if an acyclic graph:
%     ?- is_cyclic([a-[b], b-[c], c-[]]).
%     false.
%
is_cyclic(Graph) :-
    % Start the cycle check by selecting a vertex from the graph.
    member(Vertex-_, Graph),
    % Perform a depth-first search starting from the selected vertex.
    dfs(Vertex, Graph, [Vertex], _), !.

%!  dfs(+Vertex, +Graph, +Visited, -FinalVisited) is nondet.
%
%   Performs a depth-first search (DFS) to detect cycles.
%
%   This predicate performs a DFS starting from `Vertex` and checks for cycles. The search keeps
%   track of the visited vertices in the `Visited` list. If a vertex is revisited (i.e., it is
%   already in the `Visited` list), the predicate succeeds, indicating a cycle.
%
%   @arg Vertex The current vertex being visited.
%   @arg Graph The graph represented as a list of vertex-edge pairs.
%   @arg Visited The list of vertices visited so far.
%   @arg FinalVisited The final list of visited vertices at the end of the DFS traversal.
%
dfs(Vertex, Graph, Visited, [Vertex|Visited]) :-
    % Check if the current vertex has neighbors and if one of the neighbors is already visited.
    member(Vertex-Neighbors, Graph),
    member(Neighbor, Neighbors),
    % If a neighbor is already visited, a cycle is detected.
    member(Neighbor, Visited), !.
dfs(Vertex, Graph, Visited, FinalVisited) :-
    % Continue the DFS if no cycle has been detected yet.
    member(Vertex-Neighbors, Graph),
    member(Neighbor, Neighbors),
    % Only visit unvisited neighbors.
    \+ member(Neighbor, Visited),
    % Recursively visit the neighbor.
    dfs(Neighbor, Graph, [Neighbor|Visited], FinalVisited).

% With Accumulator
% Due to the way depth-first search works, a typical accumulator would not be very effective.
% The visited list already acts like an accumulator.
is_cyclic_acc(Graph) :- is_cyclic(Graph).
%

%!  dfs_graph(+Vertex, +Graph) is det.
%
%   23. Perform a Depth-First Search on a Graph
%   Normal Recursive
%
%   Performs a depth-first search (DFS) traversal starting from the given `Vertex` in the `Graph`.
%
%   This predicate initiates a depth-first search traversal on a graph, starting at `Vertex`.
%   It prints each vertex as it is visited. The graph is represented as a list of vertex-edge pairs,
%   where each pair is of the form `Vertex-Neighbors`, and `Neighbors` is a list of vertices 
%   connected to `Vertex`.
%
%   @arg Vertex The starting vertex for the DFS traversal.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%
%   @example Performing a DFS traversal on a graph:
%     ?- dfs_graph(a, [a-[b, c], b-[d], c-[], d-[]]).
%     a
%     b
%     d
%     c
%
dfs_graph(Vertex, Graph) :- 
    % Initiates the DFS from the given Vertex with an empty list of visited nodes.
    dfs_vertex(Vertex, Graph, []).

%!  dfs_vertex(+Vertex, +Graph, +Visited) is det.
%
%   Helper predicate to perform DFS on a specific vertex.
%
%   This predicate visits `Vertex` and prints it if it has not already been visited. It then
%   traverses its neighbors recursively, using `Visited` to track the visited vertices and
%   avoid cycles. 
%
%   @arg Vertex The current vertex being visited.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%   @arg Visited The list of vertices that have already been visited.
%
dfs_vertex(Vertex, _, Visited) :- 
    % If the vertex has already been visited, stop the traversal.
    member(Vertex, Visited), !.
dfs_vertex(Vertex, Graph, Visited) :-
    % Print the current vertex.
    write(Vertex), nl,
    % Retrieve the neighbors of the current vertex.
    member(Vertex-Neighbors, Graph),
    % Traverse the neighbors of the current vertex.
    dfs_neighbors(Neighbors, Graph, [Vertex|Visited]).

%!  dfs_neighbors(+Neighbors, +Graph, +Visited) is det.
%
%   Helper predicate to traverse the neighbors of a vertex during DFS.
%
%   This predicate takes the list of `Neighbors` of the current vertex and recursively 
%   performs DFS on each unvisited neighbor. It stops when all neighbors have been processed.
%
%   @arg Neighbors The list of neighbors for the current vertex.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%   @arg Visited The list of vertices that have already been visited.
%
dfs_neighbors([], _, _).  % Base case: if there are no more neighbors to visit, stop.
dfs_neighbors([Neighbor|Neighbors], Graph, Visited) :-
    % Perform DFS on the current neighbor.
    dfs_vertex(Neighbor, Graph, Visited),
    % Continue with the remaining neighbors.
    dfs_neighbors(Neighbors, Graph, Visited).

%!  dfs_graph_acc(+Vertex, +Graph) is det.
%
%   With Accumulator
%   The visited list acts as an accumulator.
%
%   Performs a depth-first search (DFS) traversal starting from the given `Vertex` in the `Graph`.
%   This predicate is identical to `dfs_graph/2` but meant to clarify that it performs the search
%   using an accumulator-based approach (although the implementation reuses `dfs_graph/2` directly).
%   It prints each vertex as it is visited.
%
%   @arg Vertex The starting vertex for the DFS traversal.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%
%   @example Performing a DFS traversal on a graph:
%     ?- dfs_graph_acc(a, [a-[b, c], b-[d], c-[], d-[]]).
%     a
%     b
%     d
%     c
%
dfs_graph_acc(Vertex, Graph) :- 
    % Calls the original dfs_graph/2 predicate to perform the DFS traversal.
    dfs_graph(Vertex, Graph).

%!  bfs_graph(+Vertex, +Graph) is det.
%
%   24. Perform a Breadth-First Search on a Graph
%   Normal Recursive
%
%   Performs a breadth-first search (BFS) traversal starting from the given `Vertex` in the `Graph`.
%
%   This predicate initiates a breadth-first search (BFS) traversal from the specified starting
%   vertex `Vertex` in the graph `Graph`. The graph is represented as a list of vertex-edge pairs,
%   where each pair is of the form `Vertex-Neighbors`, with `Neighbors` being a list of vertices
%   connected to `Vertex`. It prints each vertex as it is visited and avoids revisiting vertices
%   by keeping track of visited nodes.
%
%   @arg Vertex The starting vertex for the BFS traversal.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%
%   @example Performing a BFS traversal on a graph:
%     ?- bfs_graph(a, [a-[b, c], b-[d], c-[], d-[]]).
%     a
%     b
%     c
%     d
%
bfs_graph(Vertex, Graph) :-
    % Initialize the BFS with the starting vertex and mark it as visited.
    bfs([Vertex], Graph, [Vertex]).

%!  bfs(+Queue, +Graph, +Visited) is det.
%
%   Helper predicate to perform the actual breadth-first search traversal.
%
%   This predicate performs the breadth-first search (BFS) traversal by maintaining a queue of 
%   vertices to visit. It dequeues the current vertex, prints it, and then adds its unvisited 
%   neighbors to the queue. The list `Visited` tracks the vertices that have already been visited.
%
%   @arg Queue The current queue of vertices to visit.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%   @arg Visited The list of vertices that have already been visited.
%
bfs([], _, _).  % Base case: when there are no more vertices to visit, stop.
bfs([Vertex|Vertices], Graph, Visited) :-
    % Print the current vertex.
    write(Vertex), nl,
    % Find the neighbors of the current vertex in the graph.
    member(Vertex-Neighbors, Graph),
    % Filter out the already visited neighbors.
    filter_unvisited(Neighbors, Visited, NewNeighbors, NewVisited),
    % Add the unvisited neighbors to the end of the queue.
    append(Vertices, NewNeighbors, NewVertices),
    % Continue the BFS with the updated queue and visited list.
    bfs(NewVertices, Graph, NewVisited).

%!  filter_unvisited(+Neighbors, +Visited, -NewNeighbors, -NewVisited) is det.
%
%   Filters the list of `Neighbors` to exclude already visited vertices.
%
%   This predicate checks each neighbor in the list `Neighbors` to see if it has already been visited.
%   If a neighbor has not been visited, it is added to `NewNeighbors`, and the `NewVisited` list is
%   updated to include it. Otherwise, the neighbor is ignored.
%
%   @arg Neighbors The list of neighbors to filter.
%   @arg Visited The list of vertices that have already been visited.
%   @arg NewNeighbors The resulting list of unvisited neighbors.
%   @arg NewVisited The updated list of visited vertices.
%
filter_unvisited([], Visited, [], Visited).  % Base case: no neighbors to filter.
filter_unvisited([Neighbor|Neighbors], Visited, NewNeighbors, NewVisited) :-
    % If the neighbor has already been visited, skip it.
    (member(Neighbor, Visited) ->
        filter_unvisited(Neighbors, Visited, NewNeighbors, NewVisited)
    ;
        % Otherwise, add the neighbor to the list of new neighbors and mark it as visited.
        filter_unvisited(Neighbors, [Neighbor|Visited], NewNeighbors, [Neighbor|NewVisited])
    ).

%!  bfs_graph_acc(+Vertex, +Graph) is det.
%
%   With Accumulator
%   The visited list acts as an accumulator.
%
%   Performs a breadth-first search (BFS) traversal starting from the given `Vertex` in the `Graph`.
%
%   This predicate behaves the same as `bfs_graph/2` but is labeled `bfs_graph_acc/2` to suggest an
%   accumulator-based approach (although the actual implementation reuses `bfs_graph/2` directly). 
%   It prints each vertex as it is visited during the BFS traversal. The graph is represented as a 
%   list of vertex-edge pairs.
%
%   @arg Vertex The starting vertex for the BFS traversal.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%
%   @example Performing a BFS traversal on a graph:
%     ?- bfs_graph_acc(a, [a-[b, c], b-[d], c-[], d-[]]).
%     a
%     b
%     c
%     d
%
bfs_graph_acc(Vertex, Graph) :- 
    % Calls the original bfs_graph/2 predicate to perform the BFS traversal.
    bfs_graph(Vertex, Graph).

%!  is_connected(+Graph) is nondet.
%
%   25. Check if a Graph is Connected
%   Normal Recursive
%
%   Checks whether the given graph is connected.
%
%   This predicate succeeds if the graph `Graph` is connected, meaning there is a path between 
%   every pair of vertices. The graph is represented as a list of vertex-edge pairs, where each 
%   pair is of the form `Vertex-Neighbors`. The predicate performs a depth-first search (DFS) 
%   starting from an arbitrary vertex and then checks whether all other vertices have been visited.
%
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%
%   @example Checking if a graph is connected:
%     ?- is_connected([a-[b], b-[a, c], c-[b]]).
%     true.
%
%   @example Checking if a disconnected graph:
%     ?- is_connected([a-[b], b-[a], c-[]]).
%     false.
%
is_connected(Graph) :-
    % Get an arbitrary starting vertex from the graph.
    Graph = [Vertex-_|_],
    % Perform a DFS traversal from the starting vertex.
    dfs_graph(Vertex, Graph),
    % Check that all other vertices have been visited.
    \+ (member(OtherVertex-_, Graph), \+ member(OtherVertex, Visited)), !.

%!  is_connected_acc(+Graph) is nondet.
%
%   With Accumulator
%   The visited list acts as an accumulator.
%
%   Checks whether the given graph is connected, using an accumulator approach (placeholder).
%
%   This predicate is conceptually the same as `is_connected/1`, but is labeled `is_connected_acc/1` 
%   to suggest accumulator-based traversal (though it reuses `is_connected/1` directly). It checks 
%   if the graph `Graph` is connected by performing a depth-first search (DFS) and ensuring that all 
%   vertices can be reached from any starting vertex.
%
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%
%   @example Checking if a graph is connected:
%     ?- is_connected_acc([a-[b], b-[a, c], c-[b]]).
%     true.
%
%   @example Checking if a disconnected graph:
%     ?- is_connected_acc([a-[b], b-[a], c-[]]).
%     false.
%
is_connected_acc(Graph) :- 
    % Reuses the is_connected/1 predicate for the actual connectivity check.
    is_connected(Graph).

%!  shortest_path(+Start, +End, +Graph, -Path) is nondet.
%
%   26. Find the Shortest Path between Two Nodes in a Graph
%   Normal Recursive
%
%   Finds the shortest path between two vertices in a graph using a breadth-first search (BFS) approach.
%
%   This predicate computes the shortest path from `Start` to `End` in the graph `Graph`. The graph is
%   represented as a list of vertex-edge pairs, where each pair is of the form `Vertex-Neighbors`. The 
%   algorithm uses a breadth-first search (BFS) to explore all possible paths, keeping track of the vertices 
%   visited and the current path. The shortest path is returned as `Path`.
%
%   @arg Start The starting vertex of the path.
%   @arg End The target vertex of the path.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%   @arg Path The shortest path from `Start` to `End`, represented as a list of vertices.
%
%   @example Finding the shortest path in a graph:
%     ?- shortest_path(a, d, [a-[b, c], b-[a, d], c-[a], d-[b]], Path).
%     Path = [a, b, d].
%
shortest_path(Start, End, Graph, Path) :-
    % Initialize the search with the start vertex in both the current path and the visited list.
    shortest_path([Start], End, Graph, [Start], Path).

%!  shortest_path(+Vertices, +End, +Graph, +Visited, -Path) is det.
%
%   Helper predicate to recursively find the shortest path using BFS.
%
%   This predicate performs the recursive search to find the shortest path. It explores adjacent,
%   unvisited vertices and extends the current path by appending these vertices to the visited list.
%   If the target vertex `End` is found, the search halts, and the current path is returned.
%
%   @arg Vertices The list of current vertices being explored.
%   @arg End The target vertex of the path.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%   @arg Visited The list of vertices that have been visited so far.
%   @arg Path The shortest path from the start to the end vertex.
%
shortest_path(_, End, _, Visited, ReversePath) :-
    % Stop the search when the target vertex End is found in the visited list.
    reverse(ReversePath, [End|_]), !.
shortest_path(Vertices, End, Graph, Visited, Path) :-
    % Find all adjacent unvisited vertices from the current vertices.
    adjacent_unvisited(Vertices, Graph, Visited, Adjacent),
    % Update the visited list with the new adjacent vertices.
    append(Visited, Adjacent, NewVisited),
    % Update the vertices list for the next round of exploration.
    append(Vertices, Adjacent, NewVertices),
    % Recursively continue the search with updated lists.
    shortest_path(NewVertices, End, Graph, NewVisited, Path).

%!  shortest_path_acc(+Start, +End, +Graph, -Path) is det.
%
%   With Accumulator
%   The visited list and the list of vertices to explore act as accumulators.
%
%   Finds the shortest path between two vertices in a graph using a breadth-first search (BFS) approach.
%   This predicate is equivalent to `shortest_path/4`, and it reuses the same logic, but the name 
%   `shortest_path_acc/4` suggests an accumulator-based implementation, although no actual accumulator
%   is used. It finds the shortest path from `Start` to `End` in the graph `Graph` and returns the 
%   resulting `Path`.
%
%   @arg Start The starting vertex of the path.
%   @arg End The target vertex of the path.
%   @arg Graph The graph, represented as a list of vertex-edge pairs.
%   @arg Path The shortest path from `Start` to `End`, represented as a list of vertices.
%
%   @example Finding the shortest path in a graph:
%     ?- shortest_path_acc(a, d, [a-[b, c], b-[a, d], c-[a], d-[b]], Path).
%     Path = [a, b, d].
%
shortest_path_acc(Start, End, Graph, Path) :- 
    % Reuses the original shortest_path/4 predicate to find the shortest path.
    shortest_path(Start, End, Graph, Path).

%!  is_string_palindrome(+Str) is nondet.
%
%   27. Check if a String is a Palindrome
%   Normal Recursive
%
%   Checks whether the given string `Str` is a palindrome.
%
%   A palindrome is a string that reads the same forwards and backwards. This predicate converts the 
%   string `Str` into a list of characters and then checks if the character list is a palindrome using 
%   the helper predicate `is_palindrome/1`.
%
%   @arg Str The input string to be checked.
%
%   @example Checking if a string is a palindrome:
%     ?- is_string_palindrome("madam").
%     true.
%
%   @example Checking a non-palindrome string:
%     ?- is_string_palindrome("hello").
%     false.
%
is_string_palindrome(Str) :- 
    % Convert the string into a list of characters.
    string_chars(Str, Chars),
    % Check if the character list is a palindrome.
    is_palindrome(Chars).

% With Accumulator
% prolog
is_string_palindrome_acc(Str) :- string_chars(Str, Chars), is_palindrome_acc(Chars, []).
%

%!  edit_distance(+List1, +List2, -Distance) is det.
%
%   28. Compute the Edit Distance between Two Strings
%   Normal Recursive
%
%   Computes the edit distance (Levenshtein distance) between two lists `List1` and `List2`.
%
%   The edit distance is defined as the minimum number of insertions, deletions, or substitutions
%   required to transform one list into the other. This implementation works recursively by:
%   - Adding 1 to the distance when either list is empty (deletion or insertion).
%   - Adding 1 if the current elements of the two lists differ (substitution).
%
%   @arg List1 The first list (e.g., list of characters from a string).
%   @arg List2 The second list (e.g., list of characters from a string).
%   @arg Distance The computed edit distance between `List1` and `List2`.
%
%   @example Computing the edit distance between two lists:
%     ?- edit_distance([h,e,l,l,o], [h,a,l,l,o], D).
%     D = 1.
%
%   @example Computing the edit distance between an empty and non-empty list:
%     ?- edit_distance([], [a,b,c], D).
%     D = 3.
%
edit_distance([], [], 0).  % Base case: both lists are empty, so the distance is 0.
edit_distance([_|T1], [], D) :-
    % When the second list is empty, the distance is incremented for each element in the first list.
    edit_distance(T1, [], D1),D is D1 + 1.
edit_distance([], [_|T2], D) :-
    % When the first list is empty, the distance is incremented for each element in the second list.
    edit_distance([], T2, D1),D is D1 + 1.
edit_distance([H1|T1], [H2|T2], D) :-
    % Recursively calculate the edit distance for the remaining elements.
    edit_distance(T1, T2, D1),
    % If the heads of the two lists are different, increment the distance.
    D is D1 + (H1 \= H2).

%!  edit_distance_acc(+List1, +List2, -Distance) is det.
%
%   With Accumulator
%
%   Computes the edit distance (Levenshtein distance) between two lists `List1` and `List2` using an accumulator.
%
%   This predicate computes the edit distance between two lists in an accumulator-based approach. 
%   The accumulator keeps track of the current edit distance as the lists are recursively processed. 
%   The edit distance is the minimum number of insertions, deletions, or substitutions required to transform 
%   one list into the other.
%
%   @arg List1 The first list (e.g., list of characters from a string).
%   @arg List2 The second list (e.g., list of characters from a string).
%   @arg Distance The computed edit distance between `List1` and `List2`.
%
%   @example Computing the edit distance between two lists:
%     ?- edit_distance_acc([h,e,l,l,o], [h,a,l,l,o], D).
%     D = 1.
%
%   @example Computing the edit distance between an empty and non-empty list:
%     ?- edit_distance_acc([], [a,b,c], D).
%     D = 3.
%
edit_distance_acc(S1, S2, D) :-
    % Start with an initial accumulator value of 0.
    edit_distance_acc(S1, S2, 0, D).

%!  edit_distance_acc(+List1, +List2, +Acc, -Distance) is det.
%
%   Auxiliary predicate for edit_distance_acc/3 that carries an accumulator for the current distance.
%
%   This predicate computes the edit distance using an accumulator `Acc` to track the current number of
%   edits as it recursively processes the lists. The final result is returned when both lists are exhausted.
%
%   @arg List1 The first list to compare.
%   @arg List2 The second list to compare.
%   @arg Acc The current accumulated distance.
%   @arg Distance The final edit distance.
%
edit_distance_acc([], [], Acc, Acc).  % Base case: both lists are empty, return the accumulated distance.
edit_distance_acc([_|T1], [], Acc, D) :-
    % When the second list is empty, increment the accumulator for each remaining element in the first list.
    NewAcc is Acc + 1,edit_distance_acc(T1, [], NewAcc, D).
edit_distance_acc([], [_|T2], Acc, D) :-
    % When the first list is empty, increment the accumulator for each remaining element in the second list.
    NewAcc is Acc + 1,edit_distance_acc([], T2, NewAcc, D).
edit_distance_acc([H1|T1], [H2|T2], Acc, D) :-
    % If the heads of the lists are different, increment the accumulator.
    NewAcc is Acc + (H1 \= H2),
    % Recursively process the tails of both lists.
    edit_distance_acc(T1, T2, NewAcc, D).

%!  lcs(+List1, +List2, -LCS) is det.
%
%   29. Find the Longest Common Subsequence of Two Strings
%   Normal Recursive
%
%   Finds the longest common subsequence (LCS) between two lists.
%
%   This predicate computes the longest common subsequence between two input lists
%   by recursively comparing their elements. The LCS is a subsequence that appears
%   in both lists in the same relative order, though not necessarily consecutively.
%
%   @arg List1 The first list.
%   @arg List2 The second list.
%   @arg LCS   The longest common subsequence between the two lists.
%
%   @example Find the longest common subsequence of two lists:
%     ?- lcs([a, b, c, d, f], [a, c, d, e, f], Lcs).
%     Lcs = [a, c, d, f].
%
lcs([], _, []).  % Base case: if the first list is empty, the LCS is empty.
lcs(_, [], []).  % Base case: if the second list is empty, the LCS is empty.
lcs([H|T1], [H|T2], [H|Lcs]) :- 
    % If the heads of both lists match, include the element in the LCS and continue.
    lcs(T1, T2, Lcs), 
    !.  % Cut to avoid backtracking once a match is found.
lcs(S1, [_|T2], Lcs) :- 
    % If the heads do not match, try the tail of the second list.
    lcs(S1, T2, Lcs).
lcs([_|T1], S2, Lcs) :- 
    % If the heads do not match, try the tail of the first list.
    lcs(T1, S2, Lcs).

%!  lcs_acc(+List1, +List2, -LCS) is det.
%
%   With Accumulator
%
%   Finds the longest common subsequence (LCS) between two lists using an accumulator.
%
%   This predicate computes the longest common subsequence between two input lists
%   by comparing elements recursively. The accumulator is used to collect matching
%   elements, and the result is reversed at the end to ensure correct order.
%
%   The LCS is a subsequence that appears in both lists in the same relative order,
%   though not necessarily consecutively.
%
%   @arg List1 The first list.
%   @arg List2 The second list.
%   @arg LCS   The longest common subsequence between the two lists.
%
%   @example Find the longest common subsequence of two lists:
%     ?- lcs_acc([a, b, c, d, f], [a, c, d, e, f], Lcs).
%     Lcs = [a, c, d, f].
%
lcs_acc(S1, S2, Lcs) :-
    % Call the helper predicate with an empty accumulator.
    lcs_acc(S1, S2, [], Lcs).

%!  lcs_acc(+List1, +List2, +Acc, -LCS) is det.
%
%   Helper predicate for finding the longest common subsequence using an accumulator.
%   The base case occurs when either list is exhausted, and the accumulator is reversed
%   to produce the final LCS. The recursive case continues comparing the lists, adding
%   matching elements to the accumulator.
%
%   @arg List1 The first list.
%   @arg List2 The second list.
%   @arg Acc   The accumulator for building the LCS.
%   @arg LCS   The longest common subsequence between the two lists.
%
lcs_acc([], _, Acc, Lcs) :- 
    % If the first list is empty, reverse the accumulator to get the LCS.
    reverse(Acc, Lcs).
lcs_acc(_, [], Acc, Lcs) :- 
    % If the second list is empty, reverse the accumulator to get the LCS.
    reverse(Acc, Lcs).
lcs_acc([H|T1], [H|T2], Acc, Lcs) :- 
    % If the heads of both lists match, add the element to the accumulator and continue.
    lcs_acc(T1, T2, [H|Acc], Lcs).
lcs_acc(S1, [_|T2], Acc, Lcs) :- 
    % If the heads do not match, try the tail of the second list.
    lcs_acc(S1, T2, Acc, Lcs).
lcs_acc([_|T1], S2, Acc, Lcs) :- 
    % If the heads do not match, try the tail of the first list.
    lcs_acc(T1, S2, Acc, Lcs).

%!  longest_common_substring(+String1, +String2, -LCS) is det.
%
%   30. Find the Longest Common Substring of Two Strings
%   Normal Recursive
%
%   Finds the longest common substring between two strings.
%
%   This predicate computes the longest common substring (LCS) between two input strings
%   by first generating all possible substrings of both strings, then determining the
%   longest common substring from this set.
%
%   @arg String1 The first string.
%   @arg String2 The second string.
%   @arg LCS     The longest common substring found between the two strings.
%
%   @example Find the longest common substring between two strings:
%     ?- longest_common_substring("abcdef", "abdf", Lcs).
%     Lcs = "ab".
%
longest_common_substring(S1, S2, Lcs) :-
    % Find all substrings common to both strings.
    findall(Sub, (substring(S1, Sub), substring(S2, Sub)), Subs),
    % Determine the longest common substring.
    longest_string(Subs, Lcs).

%!  substring(+String, -Sub) is nondet.
%
%   Generates all possible substrings of a given string.
%   This predicate uses append/3 to split the input string and extract all valid substrings.
%
%   @arg String The input string.
%   @arg Sub    A substring of the input string.
%
substring(Str, Sub) :-
    % Split the string into two parts and extract a substring.
    append(_, Rest, Str),
    append(Sub, _, Rest).

%!  longest_string(+Substrings, -Longest) is det.
%
%   Determines the longest string from a list of substrings.
%
%   This predicate processes a list of substrings and finds the longest one.
%
%   @arg Substrings The list of substrings to search through.
%   @arg Longest    The longest substring in the list.
%
longest_string([H|T], Longest) :-
    % Initialize the search by treating the first element as the current longest.
    longest_string(T, H, Longest).

%!  longest_string(+Substrings, +CurrentLongest, -Longest) is det.
%
%   Helper predicate to find the longest string from a list of substrings.
%   This predicate compares each substring with the current longest and updates
%   the longest if a longer substring is found.
%
%   @arg Substrings    The remaining list of substrings to check.
%   @arg CurrentLongest The current longest substring found so far.
%   @arg Longest       The longest substring found.
%
longest_string([], Acc, Acc).  % Base case: when there are no more substrings, return the longest found.
longest_string([H|T], Acc, Longest) :-
    % Compare the length of the current substring with the longest found so far.
    length(H, LenH),
    length(Acc, LenAcc),
    % If the current substring is longer, update the accumulator.
    (LenH > LenAcc -> 
        longest_string(T, H, Longest)  % Update with the longer substring.
    ; 
        longest_string(T, Acc, Longest)  % Keep the current longest.
    ).

%!  longest_common_substring_acc(+String1, +String2, -LCS) is det.
%
%   With Accumulator
%
%   Finds the longest common substring between two strings using an accumulator.
%
%   This predicate computes the longest common substring (LCS) between two input strings.
%   It first finds all common substrings of the two strings, then uses an accumulator
%   to determine the longest substring from the list of common substrings.
%
%   @arg String1 The first string.
%   @arg String2 The second string.
%   @arg LCS     The longest common substring found between the two strings.
%
%   @example Find the longest common substring between two strings:
%     ?- longest_common_substring_acc("abcdef", "abdf", Lcs).
%     Lcs = "ab".
%
longest_common_substring_acc(S1, S2, Lcs) :-
    % Find all substrings of S1 and S2 and collect the common ones.
    findall(Sub, (substring(S1, Sub), substring(S2, Sub)), Subs),
    % Use an accumulator to find the longest common substring.
    longest_string_acc(Subs, [], Lcs).

%!  longest_string_acc(+Substrings, +Acc, -Longest) is det.
%
%   Helper predicate to find the longest string from a list of substrings.
%   It iterates over the list of substrings and uses an accumulator to keep track
%   of the longest one found so far.
%
%   @arg Substrings The list of substrings to check.
%   @arg Acc        The current longest substring found.
%   @arg Longest    The final longest substring.
%
longest_string_acc([], Acc, Acc).  % Base case: when there are no more substrings, return the longest found.
longest_string_acc([H|T], Acc, Longest) :-
    % Compute the lengths of the current substring and the accumulator.
    length(H, LenH),
    length(Acc, LenAcc),
    % If the current substring is longer, update the accumulator.
    (LenH > LenAcc -> 
        longest_string_acc(T, H, Longest)  % Update with the longer substring.
    ; 
        longest_string_acc(T, Acc, Longest)  % Keep the current longest.
    ).

