% Tic-Tac-Toe game in SWI-Prolog with AI ensuring legal moves

:- use_module(library(random)).

% The board is represented as a list of 9 elements (positions 1 to 9)
% The board is indexed from 1 to 9 as follows:
% 1 | 2 | 3
% ---------
% 4 | 5 | 6
% ---------
% 7 | 8 | 9

% The main predicate to start the game
play :-
    initialize(Board),
    display_board(Board),
    play_turn(Board, x).

% Initialize an empty board
initialize([1,2,3,4,5,6,7,8,9]).

% Display the board
display_board([A, B, C, D, E, F, G, H, I]) :-
    format('~w | ~w | ~w~n', [A, B, C]),
    format('--+---+--~n'),
    format('~w | ~w | ~w~n', [D, E, F]),
    format('--+---+--~n'),
    format('~w | ~w | ~w~n', [G, H, I]).

% Predicate for playing a turn
play_turn(Board, x) :-  % Human's turn
    format('Player X (human), choose your move (1-9): '),
    read(Move),
    (valid_move(Board, Move) ->
        make_move(Board, Move, x, NewBoard),
        display_board(NewBoard),
        (win(NewBoard, x) ->
            write('Player X (human) wins!~n');
            (tie(NewBoard) ->
                write('The game is a tie!~n');
                play_turn(NewBoard, o)  % Computer's turn
            )
        )
    ;
        write('Invalid move. Try again.~n'),
        play_turn(Board, x)
    ).

play_turn(Board, o) :-  % Computer's turn
    write('Player O (computer) is making a move...~n'),
    choose_move(Board, o, Move),
    make_move(Board, Move, o, NewBoard),
    display_board(NewBoard),
    (win(NewBoard, o) ->
        write('Player O (computer) wins!~n');
        (tie(NewBoard) ->
            write('The game is a tie!~n');
            play_turn(NewBoard, x)  % Human's turn
        )
    ).

% Check if the move is valid (the position must be a number from 1 to 9 and available)
valid_move(Board, Move) :-
    member(Move, Board).

% Make the move on the board
make_move(Board, Move, Player, NewBoard) :-
    select(Move, Board, Player, NewBoard).

% Choose a move for the computer (random legal move from available positions)
choose_move(Board, _Player, Move) :-
    findall(Pos, valid_move(Board, Pos), AvailableMoves),
    random_member(Move, AvailableMoves).

% Switch between players
switch_player(x, o).
switch_player(o, x).

% Check if the current player has won the game
win(Board, Player) :-
    win_combination(A, B, C),
    nth1(A, Board, Player),
    nth1(B, Board, Player),
    nth1(C, Board, Player).

% Winning combinations (rows, columns, diagonals)
win_combination(1, 2, 3).
win_combination(4, 5, 6).
win_combination(7, 8, 9).
win_combination(1, 4, 7).
win_combination(2, 5, 8).
win_combination(3, 6, 9).
win_combination(1, 5, 9).
win_combination(3, 5, 7).

% Check if the game is a tie (no empty positions left)
tie(Board) :-
    \+ memberchk(1, Board),
    \+ memberchk(2, Board),
    \+ memberchk(3, Board),
    \+ memberchk(4, Board),
    \+ memberchk(5, Board),
    \+ memberchk(6, Board),
    \+ memberchk(7, Board),
    \+ memberchk(8, Board),
    \+ memberchk(9, Board).

