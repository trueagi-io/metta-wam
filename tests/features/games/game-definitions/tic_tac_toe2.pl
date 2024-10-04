% Microtheory definition
in_microtheory(game_rules_mt_fn(tic_tac_toe)).

% Game domain definition
game_domain(tic_tac_toe).

% Roles in the game
role(game_role_fn(x)).
role(game_role_fn(o)).

% Labels for the game entities
entity_label(board, board).
entity_label(game_role_fn(x), x).
entity_label(game_role_fn(o), o).

% Game predicates
game_predicate(control).
game_predicate(opponent).
game_predicate(cell).

% Board setup and dimensions
board_size(3).

% Initial board state (all cells empty)
cell(3, 3, empty).
cell(3, 2, empty).
cell(3, 1, empty).
cell(2, 3, empty).
cell(2, 2, empty).
cell(2, 1, empty).
cell(1, 3, empty).
cell(1, 2, empty).
cell(1, 1, empty).

% Initial control of player X
init(control(game_role_fn(x))).

% Next state: A player marks a cell
next(cell(M, N, Mark)) :-
    does_action(Player, mark(M, N, Mark)).

% Next state: Cells retain their contents unless marked
next(cell(M, N, Contents)) :-
    cell(M, N, Contents),
    \+ does_action(_, mark(M, N, _)).

% Legal moves: A mark can be placed in an empty cell by the player who controls the turn
legal(Role, mark(X, Y, Mark)) :-
    cell(X, Y, empty),
    control(Role),
    entity_label(Role, Mark).

% Control switching between players after a move
next(control(Player)) :-
    control(OtherPlayer),
    game_domain(tic_tac_toe),
    role(Player),
    Player \= OtherPlayer.


% Check for a vertical win
goal_state(Player, 100) :-
    role(Player),
    entity_label(Player, Mark),
    cell(3, Y, Mark),
    cell(2, Y, Mark),
    cell(1, Y, Mark).

% Check for a horizontal win
goal_state(Player, 100) :-
    role(Player),
    entity_label(Player, Mark),
    cell(X, 3, Mark),
    cell(X, 2, Mark),
    cell(X, 1, Mark).

% Check for a diagonal win (top-left to bottom-right)
goal_state(Player, 100) :-
    role(Player),
    entity_label(Player, Mark),
    cell(2, 2, Mark),
    cell(3, 3, Mark),
    cell(1, 1, Mark).

% Check for a diagonal win (bottom-left to top-right)
goal_state(Player, 100) :-
    role(Player),
    entity_label(Player, Mark),
    cell(2, 2, Mark),
    cell(3, 1, Mark),
    cell(1, 3, Mark).

% Terminal state: The game ends if the board is full or a player wins
terminal_state :-
    \+ cell(_, _, empty).

% Goal state (winning condition)
terminal_state :-
    goal_state(_, 100).

% No legal moves remaining indicates a terminal state
terminal_state :-
    control(_),
    \+ legal(_, _).

% Example of marking a cell (to be used in a real game)
does_action(game_role_fn(x), mark(1, 1, x)).
does_action(game_role_fn(o), mark(2, 2, o)).

% Control switch example
control(game_role_fn(x)).

% Sample queries to test logic:
% - next(cell(X, Y, Z)) - to check the next state of a specific cell.
% - legal(game_role_fn(x), mark(X, Y, Z)) - to check if a move is legal for player X.
% - terminal_state - to check if the game has ended.

