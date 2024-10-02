/** <module> Color handling for different message types

This module provides predicates to determine the color to be used for various types
of messages based on the input format (link, diff, etc.).

*/

% Predicate to get the color of a message for a given link.
% @param link(P,_) The link data, where P is the relevant part to determine color.
% @param C The color to be returned.
% @example 
%   ?- pc_msg_color(link(example, _), Color).
%   Color = green.
pc_msg_color(link(P,_), C):- 
    % Call to determine color based on P using pp_msg_color/2.
    pp_msg_color(P, C).

% Predicate to get the color of a message for a conditional format (->_).
% @param (->_) A conditional term where P is the relevant part to determine color.
% @param C The color to be returned.
pc_msg_color((->_P), C):- 
    % Call to determine color based on P.
    pp_msg_color(P, C).

% Predicate to get the color of a message for the head of a list.
% @param [P|_] A list where P is the relevant part to determine color.
% @param C The color to be returned.
pc_msg_color([P|_], C):- 
    % Call to determine color based on the first element of the list.
    pp_msg_color(P, C).

% Predicate to get the color of a message for a diff type.
% @param diff(P) The diff format where P is the relevant part to determine color.
% @param C The color to be returned.
pc_msg_color(diff(P), C):- 
    % Call to determine color based on P in diff context.
    pp_msg_color(P, C).

/* previously: meta_predicate declaration for wots_hs/1
   This has been commented out because the wots_hs/1 predicate is no longer being used.
   However, it has been preserved for reference. */

%:- meta_predicate(wots_hs(0)).
%wots_hs(G):- wots_hs(S,G),write(S).

/* File Directive: Declares that wots_ansi/2 is a meta-predicate 
   The first argument is an output stream, and the second argument is a goal to be executed. */
:- meta_predicate(wots_ansi(-,0)).

% Predicate to handle ansi formatting within wots.
% @param S The output stream.
% @param Goal The goal to be executed.
wots_ansi(S, Goal):- 
    % Call wots/2 with the goal wrapped in woto_ansi.
    wots(S, woto_ansi(Goal)).

/* File Directive: Declares wots_html/2 as a meta-predicate, similar to wots_ansi/2. */
:- meta_predicate(wots_html(-,0)).

% Predicate to handle HTML formatting within wots.
% @param S The output stream.
% @param Goal The goal to be executed.
wots_html(S, Goal):- 
    % Call wots/2 with the goal wrapped in woto_html.
    wots(S, woto_html(Goal)).

/* previously: another declaration for wots_hs/2 was present.
   It has been kept in the comments for reference. */
:- meta_predicate(wots_hs(-,0)).

%:- wots_hs(S,G):- \+ wants_html,!,wots(S,G).
%:- wots_hs(S,G):- wots(S,G),!.

% Predicate to handle space removal within wots_hs/2.
% @param S The final output with reduced spaces.
% @param G The goal to be executed.
wots_hs(S, G):- 
    % First, call wots to execute G and store result in SS.
    wots(SS, G),
    % Then, remove extra spaces using remove_huge_spaces/2.
    notrace(remove_huge_spaces(SS, S)).

% Meta-predicate declaration for wots_vs/2.
:- meta_predicate(wots_vs(-,0)).

% Predicate to fix vertical space in the output of wots_vs/2.
% @param OOO The output with adjusted vertical space.
% @param G The goal to be executed.
wots_vs(OOO, G):- 
    % First, call wots to execute G and store result in S.
    wots(S, G),
    % Fix vertical space using fix_vspace/2.
    notrace(fix_vspace(S, OOO)).

% Predicate to adjust vertical spaces in the output string.
% @param S The input string.
% @param OOO The adjusted string without excessive vertical space.
fix_vspace(S, OOO):- 
    % Strip vertical spaces from the input string.
    strip_vspace(S, SS), 
    % If SS contains newlines, handle it with additional formatting.
    (atom_contains(SS, '\n') ->
        % Output formatted string with newlines and indentation.
        wots_hs(SSS, (nl_now, write('   '), write(SS), nl_now));
        % Otherwise, keep SS as the result.
        SSS = SS),
    % Finally, remove huge spaces from the result.
    remove_huge_spaces(SSS, OOO).

% Write a list or single element in "tall" format.
% @param L The list or element to be written.
write_tall(L):- 
    % If the input is a list, map write_tall over each element.
    is_list(L), 
    !, 
    my_maplist(write_tall, L).

% Write a single element in "tall" format.
write_tall(E):- 
    % Call wots_vs with the element and write it out.
    wots_vs(S, wqs_c(E)), 
    writeln(S).

% Write a list or single element in "wide" format.
% @param L The list or element to be written.
write_wide(L):- 
    % If the input is a list, map write_wide over each element.
    is_list(L), 
    !, 
    my_maplist(write_wide, L).

% Write a single element in "wide" format.
write_wide(E):- 
    % Call wots_vs with the element and write it without a newline.
    wots_vs(S, wqs_c(E)), 
    write(S), 
    write_nbsp.

% Predicate to replace carriage returns with <br> for HTML or ansi.
% @param S The input string.
% @param SS The output string with line breaks.
p_to_br(S, SS):- 
    % First, fix line breaks using fix_br_nls/2.
    fix_br_nls(S, S0),
    % Then, handle <br> and other replacements using cr_to_br/2.
    cr_to_br(S0, SSS),
    % Replace <p> and other HTML tags with spaces or line breaks.
    replace_in_string(['<p>'='<br>', '<br/>'='<br>', '</p>'=' ', '<p/>'='<br>', '<br><br>'='<br>'], SSS, SSSS),
    % Apply final line break replacement.
    cr_to_br(SSSS, SS).

% Predicate to replace carriage returns for HTML.
% @param S The input string.
% @param SSS The output string with <br> tags.
cr_to_br_html(S, SSS):- 
    % Replace carriage return and newlines with <br> for HTML.
    replace_in_string(['\r\n'='<br>', '\r'='<br>', '\n'='<br>'], S, SSS).

% Predicate to replace <br> for ansi format.
% @param S The input string.
% @param SSS The output string with \n for ansi.
cr_to_br_ansi(S, SSS):- 
    % Replace <br> with newlines for ansi formatting.
    replace_in_string(['<br>'='\n', '&nbsp;'=' '], S, SSS).

% Predicate to fix broken line breaks in HTML strings.
% @param S The input string.
% @param O The fixed output string.
fix_br_nls(S, O):- 
    % Perform a set of replacements to clean up HTML line breaks.
    replace_in_string(['<br/>\n'='<br/>', '<br>\n'='<br>', '</p>\n'='</p>', '<p/>\n'='<p/>', '<p>\n'='<p>',
                       '\n<br>'='<br>', '\n<br/>'='<br/>', '\n</p>'='</p>', '\n<p/>'='<p/>', '\n<p>'='<p>'], S, O).

% Predicate to remove excessive spaces from a string.
% @param S The input string.
% @param O The output string with reduced spaces.
remove_huge_spaces(S, O):- 
    % First, fix line breaks.
    notrace((fix_br_nls(S, SS), !, 
    % Then replace spaces with <br> using p_to_br/2.
    p_to_br(SS, O))), 
    !.

/*
remove_huge_spaces(S,O):- fix_br_nls(S,S0),
  replace_in_string(['          '='     ',
    '                                                                          '='     ',
    '                                                                          '='     ',
    '                                                                                                                                                                                                                                                                                                                                                                                                               '='  ',
    '                                                                                                                                                                                                                                                                                   '='   ',