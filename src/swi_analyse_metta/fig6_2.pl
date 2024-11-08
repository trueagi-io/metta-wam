%  Figure 6.2  A program for cryptoarithmetic puzzles.


%  Solving cryptarithmetic puzzles


sum(N1, N2, N)  :-                    % Numbers represented as lists of digits
  sum1( N1, N2, N, 
	0, 0,                         % Carries from right and to left both 0
        [0,1,2,3,4,5,6,7,8,9], _).    % All digits available

sum1( [], [], [], C, C, Digits, Digits).

sum1( [D1|N1], [D2|N2], [D|N], C1, C, Digs1, Digs)  :-
  sum1( N1, N2, N, C1, C2, Digs1, Digs2),
  digitsum( D1, D2, C2, D, C, Digs2, Digs).

digitsum( D1, D2, C1, D, C, Digs1, Digs)  :-
  del_var( D1, Digs1, Digs2),        % Select an available digit for D1
  del_var( D2, Digs2, Digs3),        % Select an available digit for D2
  del_var( D, Digs3, Digs),          % Select an available digit for D
  S  is  D1 + D2 + C1,    
  D  is  S mod 10,                   % Reminder
  C  is  S // 10.                    % Integer division

del_var( A, L, L) :-
  nonvar(A), !.                      % A already instantiated

del_var( A, [A|L], L).		     % Delete the head

del_var( A, [B|L], [B|L1])  :-
  del_var(A, L, L1).		     % Delete from tail


% Some puzzles

puzzle1( [D,O,N,A,L,D],
         [G,E,R,A,L,D],
         [R,O,B,E,R,T] ).

puzzle2( [0,S,E,N,D],
         [0,M,O,R,E],
         [M,O,N,E,Y] ).
