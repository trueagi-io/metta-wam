%  Figure 6.3  A procedure for substituting a subterm of a term by another subterm.


% substitute( Subterm, Term, Subterm1, Term1):
%    if all occurrences of Subterm in Term are substituted
%    with Subterm1 then we get Term1.




% Case 1: Substitute whole term

substitute( Term, Term, Term1, Term1 )  :-  !.


% Case 2: Nothing to substitute

substitute( _ , Term , _ , Term )  :-    atomic( Term ), !.

% Case 3: Do substitution on arguments

substitute( Sub, Term, Sub1, Term1 )  :-
   Term  =..  [ F | Args ],                        % Get arguments
   substlist( Sub, Args, Sub1, Args1 ),         % Perform substitution on them
   Term1  =..  [ F | Args1 ].		       % Construct Term1


substlist( _, [], _, [] ).

substlist( Sub, [ Term | Terms ], Sub1, [ Term1 | Terms1 ])  :-
   substitute( Sub, Term, Sub1, Term1 ),
   substlist( Sub, Terms, Sub1, Terms1 ).

