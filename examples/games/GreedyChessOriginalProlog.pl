%!  deep_blue_dummy_chess is a Prolog chess implementation.
%
%   This program simulates a simple chess engine named "Deep Blue Dummy Chess."
%   It provides a minimal interface for playing a chess game against a dummy 
%   computer opponent. The computer makes moves based on basic rules and strategies.
%
%   The game includes commands to move pieces, reset the board, display commands, 
%   and check the current game state. The board is represented as a list of lists, 
%   where each list corresponds to a piece's position and type.
%
%   Key Features:
%   - User moves pieces using the `m/4` command, specifying start and destination coordinates.
%   - The computer plays automatically with the `g/0` command, attempting simple strategies.
%   - Board reset and game state commands are provided for convenience.
%
%   Game Commands:
%     1. `?- m(X1, Y1, X2, Y2).`  - Move your piece from (X1, Y1) to (X2, Y2).
%     2. `?- g.`                 - Trigger the computer's move.
%     3. `?- r.`                 - Reset the board and start a new game.
%     4. `?- c.`                 - Display the list of commands.
%     5. `?- d.`                 - Display the current state of the board.
%
%   The game ends when one player achieves checkmate. If no moves are possible, the 
%   game will automatically detect checkmate or stalemate conditions.
%
%   @author Mike Archbold
%   @version 1.0
%   @date 2001
%
%   @example
%     % Start the game.
%     ?- chess.
%     % Move a pawn from (1,2) to (1,3).
%     ?- m(1,2,1,3).
%     % Trigger the computer to move.
%     ?- g.
%
%   This program is designed as a Prolog exercise for understanding logical
%   reasoning, game state management, and simple AI.
%
:- style_check(-singleton).


%!  piece(+Piece) is det.
%
%   Defines valid chess pieces for the game.
%
%   @arg Piece A chess piece represented as an atom.
%
%   @example Examples of valid pieces:
%     ?- piece(k).
%     true.
%     ?- piece(p).
%     true.
piece(k):-true.
piece(q):-true.
piece(r):-true.
piece(b):-true.
piece(n):-true.
piece(p):-true.

%!  hpiece(+Piece) is det.
%
%
%   @arg Piece A chess piece represented as an atom.
%
%   @example Examples of valid pieces:
%     ?- hpiece(q).
%     true.
%     ?- hpiece(n).
%     true.
hpiece(k):-true.
hpiece(q):-true.
hpiece(r):-true.
hpiece(b):-true.
hpiece(n):-true.
hpiece(p):-true.

%!  rpiece(+Piece) is det.
%

rpiece(p):-true.
rpiece(n):-true.
rpiece(b):-true.
rpiece(r):-true.
rpiece(q):-true.
rpiece(k):-true.

%!  cord(+Coordinate) is det.
%
%   Defines valid board coordinates, ranging from 1 to 8.
%
%   @arg Coordinate A valid coordinate (integer).
%
%   @example Valid coordinates:
%     ?- cord(5).
%     true.
%     ?- cord(9).
%     false.
cord(1):-true.
cord(2):-true.
cord(3):-true.
cord(4):-true.
cord(5):-true.
cord(6):-true.
cord(7):-true.
cord(8):-true.

%!  chess is det.
%
%   Initializes the chess game environment.
%
%   This predicate:
%     - Declares `board/1` and `guimessage/1-4` predicates as dynamic.
%     - Abolishes any existing board or GUI message definitions.
%     - Sets up the initial chessboard configuration.
%     - Displays the welcome message and game instructions.
%
%   @example Start a new chess game:
%     ?- chess.
%
%   @see welcome/0
chess :- 
    (dynamic board/1), 
    (dynamic guimessage/1), 
    (dynamic guimessage/2), 
    (dynamic guimessage/3), 
    (dynamic guimessage/4),
    (abolish(board, 1); true),
    (abolish(guimessage, 1); true),
    (abolish(guimessage, 2); true),
    (abolish(guimessage, 3); true),
    (abolish(guimessage, 4); true),
    assert(guimessage(chess, game, started)),
    assert(board([
        [1, 1, s, r], [1, 2, s, p], [1, 3], [1, 4], [1, 5], [1, 6], [1, 7, g, p], [1, 8, g, r],
        [2, 1, s, n], [2, 2, s, p], [2, 3], [2, 4], [2, 5], [2, 6], [2, 7, g, p], [2, 8, g, n],
        [3, 1, s, b], [3, 2, s, p], [3, 3], [3, 4], [3, 5], [3, 6], [3, 7, g, p], [3, 8, g, b],
        [4, 1, s, q], [4, 2, s, p], [4, 3], [4, 4], [4, 5], [4, 6], [4, 7, g, p], [4, 8, g, q],
        [5, 1, s, k], [5, 2, s, p], [5, 3], [5, 4], [5, 5], [5, 6], [5, 7, g, p], [5, 8, g, k],
        [6, 1, s, b], [6, 2, s, p], [6, 3], [6, 4], [6, 5], [6, 6], [6, 7, g, p], [6, 8, g, b],
        [7, 1, s, n], [7, 2, s, p], [7, 3], [7, 4], [7, 5], [7, 6], [7, 7, g, p], [7, 8, g, n],
        [8, 1, s, r], [8, 2, s, p], [8, 3], [8, 4], [8, 5], [8, 6], [8, 7, g, p], [8, 8, g, r]
    ])),
    set_prolog_flag(toplevel_print_options, [quoted(true), portray(true)]),
    welcome,
    !.

%!  welcome is det.
%
%   Displays the welcome message and instructions for playing the game.
%
%   This predicate outputs:
%     - A welcome message with program details.
%     - Instructions for available commands and how to use them.
%
%   @see chess/0
welcome :-
    write('Deep Blue Dummy Chess -- Copyright 2001 Mike Archbold'), nl,
    write('This program is intended as a Prolog exercise'), nl, nl,
    board(A), b(A),
    write('******* I N S T R U C T I O N S ********'), nl,
    write('- Your pieces are marked with an asterisk'), nl,
    write('- Please take note of the following simple commands:'), nl, nl,
    write('-------- C o m m a n d s -----------'), nl,
    write('1) TO MOVE YOUR PIECE USE (example) ->  ?- m(1,2,1,3).'), nl,
    write('   Result:  YOUR pawn in 1,2 moved to location 1,3. Standard x/y.'), nl,
    write('2) TO MOVE DEEP BLUE DUMMY type     ->  ?- g.'), nl,
    write('3) To reset, type                   ->  ?- r.'), nl,
    write('4) Display commands, type           ->  ?- c.'), nl,
    write('5) Display current board type       ->  ?- d.'), nl,
    write('ALL COMMANDS MUST BE TERMINATED WITH A PERIOD AND NO SPACES.'), nl,
    write('You may now enter your move (m) command'), nl.

%!  r is det.
%
%   Resets the game by calling chess/0 to reinitialize the board and state.
%
%   @example Reset the game:
%     ?- r.
%
%   @see chess/0
r :- chess.

%!  c is det.
%
%   Displays a list of available commands for interacting with the game.
%
%   This predicate outputs a menu of user commands, explaining how to move pieces, reset the board,
%   trigger the computer's move, and display the board or commands.
%
%   Commands displayed include:
%     1. `?- m(X1, Y1, X2, Y2).`  - Move your piece from (X1, Y1) to (X2, Y2).
%     2. `?- g.`                 - Trigger Deep Blue Dummy's move.
%     3. `?- r.`                 - Reset the game.
%     4. `?- c.`                 - Display this list of commands.
%     5. `?- d.`                 - Display the current board.
%
%   @example Display the command list:
%     ?- c.
%     -------- C o m m a n d s -----------
%     1) TO MOVE YOUR PIECE USE (example) ->  ?- m(1,2,1,3).
%        Result:  YOUR piece in 1,2 moved to location 1,3. Standard x/y.
%     2) TO MOVE DEEP BLUE DUMMY type     ->  ?- g.
%     3) To reset, type                   ->  ?- r.
%     4) Display commands, type           ->  ?- c.
%     5) Display current board type       ->  ?- d.
%     ALL COMMANDS MUST BE TERMINATED WITH A PERIOD!
%
%   @see chess/0
c :-
    write('-------- C o m m a n d s -----------'), nl,
    write('1) TO MOVE YOUR PIECE USE (example) ->  ?- m(1,2,1,3).'), nl,
    write('   Result:  YOUR piece in 1,2 moved to location 1,3. Standard x/y.'), nl,
    write('2) TO MOVE DEEP BLUE DUMMY type     ->  ?- g.'), nl,
    write('3) To reset, type                   ->  ?- r.'), nl,
    write('4) Display commands, type           ->  ?- c.'), nl,
    write('5) Display current board type       ->  ?- d.'), nl,
    write('ALL COMMANDS MUST BE TERMINATED WITH A PERIOD!'), nl.

%!  m(+X1, +Y1, +X2, +Y2) is det.
%
%   Moves a player's piece from one position to another on the chessboard.
%
%   This predicate:
%     - Ensures the game is not over by checking for a `checkmate` message.
%     - Validates that the source and destination positions are different.
%     - Verifies the source position contains a valid piece and the move adheres to game rules.
%     - Checks the path is clear for the piece to move.
%     - Updates the game board with the new move.
%     - Ensures that the player's king is not in check after the move.
%     - Prints the updated board and move details.
%
%   If the move leads to checkmate or violates chess rules (e.g., moving into check),
%   the move is rejected, and the game state remains unchanged.
%
%   @arg X1 The X-coordinate of the piece's current position.
%   @arg Y1 The Y-coordinate of the piece's current position.
%   @arg X2 The X-coordinate of the desired destination.
%   @arg Y2 The Y-coordinate of the desired destination.
%
%   @example Move a piece from (1,2) to (1,3):
%     ?- m(1,2,1,3).
%     YOU move from: 1,2 to: 1,3
%     Type c. for commands you can use.
%
%   @example Attempt an invalid move:
%     ?- m(1,2,1,2).
%     % No move occurs because the source and destination are the same.
%
%   @see printmove/3
%   @see examine_king/3
%   @see clear_route/3
%   @see move_piece/4
m(A, B, C, D) :-
    guimessage(checkmate, E, F), % Check if the game is over due to checkmate.
    write('Game over.'), nl, !.
m(A, B, C, D) :-
    board(E),
    concat_lists([[A], [B]], F), % Create the source coordinate list.
    concat_lists([[C], [D]], G), % Create the destination coordinate list.
    F \= G,                      % Ensure source and destination are not the same.
    return_entire_box(F, H, E),  % Retrieve the source box details.
    return_entire_box(G, I, E),  % Retrieve the destination box details.
    (len(I, 2); not(samecolor(H, I))), % Ensure the destination is valid (empty or opponent's piece).
    !,
    clear_route(H, I, E),        % Validate the path for the piece.
    move_piece(H, I, E, J),      % Make the move and update the board state.
    xy_box(K, [s, k], J),        % Check the position of the player's king.
    not(take_dest(K, g, J)),     % Ensure the player's king is not in check after the move.
    move_piece(H, I, E, L),      % Update the board with the valid move.
    M = E,
    retract(board(E)),           % Update the board in the game state.
    assert(board(L)),
    b(L),                        % Print the updated board.
    printmove(H, I, M),          % Print the move details.
    !,
    examine_king(L, g, s),       % Check if the move resulted in check or checkmate.
    garbage_collect,             % Perform garbage collection for optimization.
    trim_stacks,                 % Optimize Prolog stacks.
    !.
%!  d is det.
%
%   Displays the current state of the chessboard.
%
%   This predicate retrieves the board's current configuration using `board/1`
%   and then calls `b/1` to print it in a readable format. The displayed board includes:
%   - Column numbers at the top and bottom for reference.
%   - Row numbers on the left and right of each row.
%   - Pieces displayed with appropriate symbols (gold pieces as letters, silver pieces with an asterisk).
%   - Empty squares displayed as blank spaces.
%
%   @example Display the current board:
%     ?- d.
%       1  2  3  4  5  6  7  8
%     -------------------------
%     8 | r | p |   |   |   |   | P | R | 8
%     ...
%     -------------------------
%       1  2  3  4  5  6  7  8
%
d:-board(A), b(A), !.

%!  b(+A) is det.
%
%   Prints the entire chessboard in a formatted style.
%
%   This predicate outputs:
%   - Column labels at the top and bottom.
%   - Horizontal row separators.
%   - Each square of the chessboard with pieces or empty spaces.
%     - Gold pieces are represented by their letters (e.g., 'p', 'r').
%     - Silver pieces are prefixed with an asterisk ('*') (e.g., '*p', '*r').
%     - Empty squares are shown as blank cells ('  ').
%
%   Calls `write_box/3` to handle row-wise printing.
%
%   @arg A The current board configuration as a list of positions.
%
b(A):-write('    1  2  3  4  5  6  7  8'), nl, write('  -------------------------'), nl, write_box(1, 8, A).

%!  write_box(+A, +B, +C) is det.
%
%   Recursively prints each square of the chessboard row by row.
%
%   This predicate processes each square from rank 8 (top) to rank 1 (bottom)
%   and file 1 to file 8. Each square is printed based on its contents:
%   - Empty cells are displayed as blank spaces ('  ').
%   - Gold pieces are printed as their respective letters.
%   - Silver pieces are prefixed with an asterisk ('*').
%
%   @arg A The current file (column) being processed (1-8).
%   @arg B The current rank (row) being processed (8-1).
%   @arg C The current board configuration as a list of positions.
%
%   @see return_entire_box/3
%   @see len/2
write_box(A, 0, B):-nl, write('  -------------------------'), nl, write('    1  2  3  4  5  6  7  8'), nl, nl, nl, nl.
write_box(A, B, C):-A=1, return_entire_box([A, B], D, C), len(D, 2), write(B), write(' |  '), E is A+1, write_box(E, B, C).
write_box(A, B, C):-A<8, return_entire_box([A, B], D, C), len(D, 2), write('|  '), E is A+1, write_box(E, B, C).
write_box(A, B, C):-A=8, return_entire_box([A, B], D, C), len(D, 2), write(('|')), write('  | '), write(B), E is B-1, (B>1, nl, write('  -------------------------'), nl;true), write_box(1, E, C).
write_box(A, B, C):-A<8, return_entire_box([A, B], D, C), len(D, 4), nth1(3, D, g), nth1(4, D, E), (A=1, write(B), write(' |');write(('|'))), write(' '), write(E), F is A+1, write_box(F, B, C).
write_box(A, B, C):-A<8, return_entire_box([A, B], D, C), len(D, 4), nth1(3, D, s), nth1(4, D, E), (A=1, write(B), write(' |');write(('|'))), write(*), write(E), F is A+1, write_box(F, B, C).
write_box(A, B, C):-A=8, return_entire_box([A, B], D, C), len(D, 4), nth1(3, D, g), nth1(4, D, E), write(('|')), write(' '), write(E), write('| '), write(B), F is B-1, G=1, (F>0, nl, write('  -------------------------'), nl;true), write_box(G, F, C).
write_box(A, B, C):-A=8, return_entire_box([A, B], D, C), len(D, 4), nth1(3, D, s), nth1(4, D, E), write(('|')), write(*), write(E), write('| '), write(B), F is B-1, G=1, (F>0, nl, write('  -------------------------'), nl;true), write_box(G, F, C).


%!  g is det.
%
%   Executes Deep Blue Dummy's turn to make a move in the chess game.
%
%   This predicate determines the computer's move strategy and updates the game board 
%   accordingly. It tries a sequence of increasingly simpler strategies, starting with 
%   attempting checkmate and falling back to defensive or random moves if necessary.
%
%   Steps:
%     1. **Check for game over**: If a `checkmate` message is already present, the game is over, 
%        and the predicate prints a "Game over." message.
%     2. **Attempt Checkmate**: Attempts to move a piece that can deliver checkmate to the opponent's king.
%        If successful, it updates the board, prints the move, and declares victory.
%     3. **Play Defense**: Tries to move a piece to defend against a threat or block an opponent's 
%        aggressive move.
%     4. **Take Highest-Value Open Piece**: Looks for an opponent's piece that can be captured safely.
%     5. **Move to a Strategic Position**: Attempts to move a piece to a more advantageous position 
%        on the board.
%     6. **Take Highest-Value Open Pawn**: Focuses on capturing opponent pawns if no other higher-value 
%        pieces are available.
%     7. **Make a Random Move**: If no strategic or defensive moves are available, the computer 
%        makes a random legal move to an empty square.
%
%   Each move updates the `board/1` predicate to reflect the new state of the game. The move is printed 
%   for the user, and the board is displayed after each turn.
%
%   Clauses:
%     - **First Clause**: Checks for an existing `checkmate` message and declares the game over.
%     - **Second Clause**: Attempts to checkmate the opponent.
%     - **Third Clause**: Plays defensively to protect key pieces or block threats.
%     - **Fourth Clause**: Tries to capture the highest-value piece that is available.
%     - **Fifth Clause**: Positions pieces for strategic advantage.
%     - **Sixth Clause**: Captures an opponent's pawn if available.
%     - **Seventh Clause**: Executes a fallback random move.
%
%   @example Trigger the computer's move:
%     ?- g.
%     Deep Blue Dummy will make its move:
%     YOU move from: 3,2 to: 3,3
%     Checkmate!  Deep Blue Dummy Wins!
%
%   @see attemptcheckmate/4
%   @see playdefenseR/4
%   @see takehighestopen/4
%   @see movetoposition/4
%   @see takehighestopenpawn/4
%   @see random_move_empty_sq/4
g :-
    % Clause 1: If the game is over due to checkmate, print "Game over" and stop.
    guimessage(checkmate, A, B),
    write('Game over.'), nl, !.

g :-
    % Clause 2: Attempt to checkmate the opponent.
    board(A),
    attemptcheckmate(B, A, C, D),  % Try to deliver checkmate.
    retract(board(A)),             % Remove the old board state.
    assert(board(B)),              % Update the board with the new state.
    b(B),                          % Display the updated board.
    write('Checkmate!  Deep Blue Dummy Wins!'), nl,
    asserta(guimessage(checkmate, s, g)),  % Declare checkmate.
    printmove(C, D, A), !.         % Print the move that led to checkmate.

g :-
    % Clause 3: Play defense by protecting pieces or blocking the opponent.
    board(A),
    playdefenseR(B, A, C, D),      % Try to make a defensive move.
    retract(board(A)),             % Update the board state.
    assert(board(B)),
    b(B),
    examine_king(B, s, g),         % Check the status of the king.
    printmove(C, D, A), !.

g :-
    % Clause 4: Capture the highest-value opponent piece that is open.
    board(A),
    takehighestopen(B, A, C, D),   % Find and take the most valuable open piece.
    retract(board(A)),
    assert(board(B)),
    b(B),
    examine_king(B, s, g),         % Check if the move resulted in check.
    printmove(C, D, A), !.

g :-
    % Clause 5: Move a piece to a more strategic position on the board.
    board(A),
    movetoposition(B, A, C, D),    % Move to a better position.
    retract(board(A)),
    assert(board(B)),
    b(B),
    examine_king(B, s, g),
    printmove(C, D, A), !.

g :-
    % Clause 6: Capture an opponent's pawn if no other pieces are available.
    board(A),
    takehighestopenpawn(B, A, C, D), % Take an opponent pawn.
    retract(board(A)),
    assert(board(B)),
    b(B),
    examine_king(B, s, g),
    printmove(C, D, A), !.

g :-
    % Clause 7: Make a random legal move to an empty square.
    board(A),
    random_move_empty_sq(B, A, C, D), % Find a random empty square to move to.
    retract(board(A)),
    assert(board(B)),
    b(B),
    printmove(C, D, A), !.

%!  kingnotincheck(+A) is semidet.
%
%   Succeeds if the gold king is not in check on the given board state.
%
%   This predicate determines whether the gold king (`g, k`) is safe by checking:
%     - The position of the gold king on the board `A`.
%     - Whether any silver piece (`s`) can attack the king's position.
%
%   @arg A The current board configuration as a list of positions.
%
%   @see take_dest/3
kingnotincheck(A) :-
    xy_box(B, [g, k], A),   % Find the position of the gold king on the board.
    not(take_dest(B, s, A)). % Succeed if no silver piece can attack the king's position.

%!  attemptcheckmate(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Attempts to find and execute a move that delivers checkmate to the silver king.
%
%   Steps:
%     1. Locate the silver king (`s, k`) on the current board (`CurrentBoard`).
%     2. Retrieve a list of all gold pieces (`g`) on the board using `buildgold/2`.
%     3. Iterate over each gold piece and check if it can move to a position that:
%         - Attacks the silver king's position.
%         - Meets the conditions to deliver checkmate.
%     4. If successful, update the board state to `NewBoard` with the checkmate move.
%
%   Validates the move using:
%     - `positiontotake/4` to determine the destination for the gold piece.
%     - `move_piece/4` to simulate the move.
%     - `threatOK1/4`, `threatOK2/3`, `threatOK3/3` to ensure the move results in checkmate.
%     - `kingnotincheck/1` to confirm that the gold king is not left vulnerable.
%
%   @arg NewBoard The resulting board after the checkmate move is executed.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece being moved, represented as `[X, Y, g, Piece]`.
%   @arg Dest The destination square for the gold piece, represented as coordinates `[X, Y]`.
%
%   @see xy_box/2
%   @see buildgold/2
%   @see move_piece/4
%   @see threatOK1/4
%   @see threatOK2/3
%   @see threatOK3/3
%   @see kingnotincheck/1
attemptcheckmate(A, B, C, D) :-
    xy_box(E, [s, k], B),        % Locate the position of the silver king on the board.
    buildgold(B, F),             % Retrieve all gold pieces on the board.
    !,
    rpiece(G),                   % Select a possible gold piece type to attempt the move.
    cord(H), cord(I),            % Generate possible coordinates for the move.
    member([H, I, g, G], F),     % Select a gold piece and its position.
    C = [H, I, g, G],            % Bind the selected gold piece.
    positiontotake(E, C, D, B),  % Determine the target position (D) for checkmate.
    move_piece(C, D, B, A),      % Simulate the move on the board.
    nth1(1, D, J),               % Extract file (column) of the destination.
    nth1(2, D, K),               % Extract rank (row) of the destination.
    return_entire_box([J, K], L, A), % Retrieve the box details for the destination square.
    threatOK1(A, s, g, [L]),     % Check if the silver king is in check and no immediate counter.
    threatOK2(A, s, g),          % Ensure the silver king cannot escape.
    threatOK3(A, s, g),          % Confirm no other silver pieces can block the checkmate.
    kingnotincheck(A).           % Ensure the gold king remains safe after the move.

/*
/* newer code start */

/* see if anybody can check the silver king first... if possible do next rule (long) */
attemptcheckmate(Newboard,Listofboxes,Goldbox,Destbox) :-
  	/* find silver king */
	xy_box(Kingbox,[s,k],Listofboxes),
        /* return list of all gold pieces */
        buildgold(Listofboxes,Currentgoldpieces),   
        /* no gold can align to check king, sequential check. */
        rpiece(Piece),
        cord(X),
        cord(Y),
        member([X,Y,g,Piece],Currentgoldpieces),
        positiontotake(Kingbox,[X,Y,g,Piece],Destbox,Listofboxes),
        !,
        /* don't try the exhaustive search if it doesn't seem likely to work... */
        deepattemptcheckmate(Newboard,Listofboxes,Goldbox,Destbox).

/* move from -Goldbox to -Destbox for checkmate, return -Newboard */
deepattemptcheckmate(Newboard,Listofboxes,Goldbox,Destbox) :-
  	/* find silver king */
	xy_box(Kingbox,[s,k],Listofboxes),
        /* return list of all gold pieces */
        buildgold(Listofboxes,Currentgoldpieces),
        !,
                
	/* find *** -Goldbox AND -Destbox  *** which can check Kingbox */
	findgoldcheck(Currentgoldpieces,Listofboxes,Newboard,Goldbox,Destbox,Kingbox).

        
findgoldcheck([],_,_,_,_,_) :- !, fail.      
        
findgoldcheck([Goldbox|_],Listofboxes,Newboard,Goldbox,Destbox,Kingbox) :-

        /* see if Goldbox can be moved into position to take Kingbox */
        positiontotake(Kingbox,Goldbox,Destbox,Listofboxes),
		move_piece(Goldbox,Destbox,Listofboxes,Newboard),
        nth1(1,Destbox,X),
        nth1(2,Destbox,Y),
        return_entire_box([X,Y],EntireBox,Newboard),           
        threatOK1(Newboard,s,g,[EntireBox]), /*can your piece be taken? */
        threatOK2(Newboard,s,g), /* can king move out of the way ?? */ 
        threatOK3(Newboard,s,g). /* can a piece block threat? */
		findgoldcheck([_|Currentgoldpieces],Listofboxes,Newboard,Goldbox,Destbox,Kingbox) :-
        findgoldcheck(Currentgoldpieces,Listofboxes,Newboard,Goldbox,Destbox,Kingbox).        
/* newer code end */
*/
%!  playdefenseR(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Attempts to play a defensive move with a randomized approach.
%
%   This predicate randomly decides whether to invoke `playdefense/4` to make a defensive move.
%   It generates a random integer (1-3) and, if the result is either 1 or 2, proceeds with the defense.
%
%   @arg NewBoard The resulting board after a defensive move is made.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece being moved.
%   @arg Dest The destination square of the defensive move.
%
%   @see playdefense/4
%   @see returnrandominteger/2
playdefenseR(A, B, C, D) :-
    returnrandominteger(E, 3),  % Generate a random integer between 1 and 3.
    !,
    (E == 1 ; E == 2),          % Proceed if E is 1 or 2.
    playdefense(A, B, C, D).    % Attempt a defensive move.

%!  playdefense(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Makes a defensive move to protect gold pieces or block threats.
%
%   Steps:
%     1. Build a list of gold pieces using `buildgold/2`.
%     2. Identify gold pieces that are under threat using `checkgold/3`.
%     3. Try to protect the threatened pieces by:
%         - Moving a gold piece to block the threat.
%         - Clearing a safe route for the move.
%         - Ensuring the gold king is not left vulnerable (`kingnotincheck/1`).
%
%   Strategies:
%     - Move a gold piece near a threatened square.
%     - Block or clear a route that disrupts the opponent's attack.
%
%   @arg NewBoard The resulting board after the defensive move.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece being moved.
%   @arg Dest The destination square for the defensive move.
%
%   @see buildgold/2
%   @see checkgold/3
%   @see clear_route/3
%   @see kingnotincheck/1
%   @see move_piece/4
playdefense(A, B, C, D) :-
    buildgold(B, E),             % Get all gold pieces on the board.
    checkgold(E, B, []),         % Check if any gold pieces are under threat.
    !, fail.                     % Fail if no pieces need defending.

playdefense(A, B, C, D) :-
    buildgold(B, E),             % Get all gold pieces.
    checkgold(E, B, F),          % Find threatened gold pieces.
    piece(G),                    % Select a type of gold piece.
    member([H, I, g, G], F),     % Pick one threatened piece.
    J = [H, I, g, G],            % Bind the piece.
    hpiece(K),                   % Select a human piece type.
    xy_box(L, [s, K], B),        % Find the threatening silver piece.
    return_entire_box(L, D, B),  % Get the threatening square.
    clear_route(D, J, B),        % Ensure a clear route for the gold piece.
    findgoldhigh(E, B, C, L),    % Move the gold piece to block the threat.
    move_piece(C, D, B, A),      % Execute the move.
    (not(take_dest(L, s, A)) ; nth1(4, C, p) ; guimessage(check, g, s)), % Additional validations.
    kingnotincheck(A).           % Ensure the gold king is not in check.

playdefense(A, B, C, D) :-
    buildgold(B, E),             % Get all gold pieces.
    checkgold(E, B, F),          % Find threatened gold pieces.
    piece(G),                    % Select a gold piece type.
    member([H, I, g, G], F),     % Pick a threatened piece.
    C = [H, I, g, G],            % Bind the piece.
    hpiece(J),                   % Select a human piece type.
    xy_box(K, [s, J], B),        % Find the threatening silver piece.
    positiontotake(K, C, D, B),  % Determine a position to block the threat.
    move_piece(C, D, B, A),      % Execute the move.
    not(take_dest(D, s, A)),     % Ensure the move doesn't leave the piece vulnerable.
    kingnotincheck(A).           % Ensure the gold king is not in check.

playdefense(A, B, C, D) :-
    buildgold(B, E),             % Get all gold pieces.
    checkgold(E, B, F),          % Find threatened gold pieces.
    lookforempty(B, G),          % Find empty squares on the board.
    !,
    piece(H),                    % Select a gold piece type.
    member([I, J, g, H], F),     % Pick a threatened piece.
    C = [I, J, g, H],            % Bind the piece.
    member([K, L], G),           % Select an empty square.
    D = [K, L],                  % Define the destination square.
    clear_route(C, D, B),        % Ensure a clear route to the destination.
    move_piece(C, D, B, A),      % Execute the move.
    not(take_dest(D, s, A)),     % Ensure the piece isn't vulnerable at the destination.
    kingnotincheck(A).           % Ensure the gold king is safe.

%!  movetoposition(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Moves a gold piece to a strategic position if no threats or captures are available.
%
%   This predicate attempts to improve the position of gold pieces on the board. It uses 
%   a random decision to either proceed with repositioning or skip the move.
%
%   @arg NewBoard The resulting board after moving a piece to a better position.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece being repositioned.
%   @arg Dest The destination square for the gold piece.
%
%   @see buildgold/2
%   @see checkeachgold/5
movetoposition(A, B, C, D) :-
    returnrandominteger(E, 2),  % Generate a random integer (1-2).
    !,
    E == 1,                     % Proceed only if the result is 1.
    buildgold(B, F),            % Get all gold pieces.
    checkeachgold(F, A, B, C, D). % Check and move one piece to a better position.

movetoposition(Newboard, Listofboxes, Goldbox, Destbox) :-
    buildgold(Listofboxes, Currentgoldpieces), % Get all gold pieces.
    checkeachgold(Currentgoldpieces, Newboard, Listofboxes, Goldbox, Destbox). % Move to a position.

%!  checkeachgold(+GoldPieces, -NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Checks each gold piece to find a valid move to a better position.
%
%   This predicate iterates over the list of gold pieces, checking each one for valid moves.
%   A move is valid if:
%     - It does not leave the gold king in check.
%     - It places the gold piece in a better position.
%
%   @arg GoldPieces A list of gold pieces to evaluate.
%   @arg NewBoard The resulting board after the move.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The selected gold piece to move.
%   @arg Dest The destination square for the move.
%
%   @see positiontotake/4
%   @see move_piece/4
%   @see kingnotincheck/1
checkeachgold([], A, B, C, D) :-
    !, fail.  % Fail if no valid moves are found.

checkeachgold([A|B], C, D, A, E) :-
    piece(F),                      % Select a gold piece type.
    xy_box(G, [s, F], D),          % Find a silver piece to consider.
    positiontotake(G, A, E, D),    % Determine a destination for the move.
    move_piece(A, E, D, C),        % Execute the move.
    not(take_dest(E, s, C)),       % Ensure the move is safe.
    kingnotincheck(C).             % Confirm the gold king remains safe.

checkeachgold([A|B], C, D, E, F) :-
    checkeachgold(B, C, D, E, F).  % Continue checking the remaining gold pieces.

%!  random_move_empty_sq(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Makes a random move by selecting an empty square for a gold piece.
%
%   This predicate performs the following steps:
%     1. Builds a list of all gold pieces on the board using `buildgold/2`.
%     2. Identifies threatened gold pieces using `checkgold/3`.
%     3. Finds all empty squares on the board using `lookforempty/2`.
%     4. Assigns random values to gold pieces for randomization (`buildrandomgold/2`).
%     5. Selects a valid move for a gold piece using `findgoldmove/5`.
%     6. Ensures the move does not leave the gold king in check or vulnerable.
%     7. Updates the board with the new move and validates the outcome.
%
%   @arg NewBoard The resulting board after making the random move.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece being moved.
%   @arg Dest The destination square for the random move.
%
%   @see buildgold/2
%   @see checkgold/3
%   @see lookforempty/2
%   @see buildrandomgold/2
%   @see findgoldmove/5
%   @see move_piece/4
%   @see kingnotincheck/1
random_move_empty_sq(A, B, C, D) :-
    buildgold(B, E),             % Build a list of all gold pieces on the board.
    checkgold(E, B, F),          % Identify gold pieces under threat.
    lookforempty(B, G),          % Find all empty squares on the board.
    buildrandomgold(B, H),       % Assign random values to gold pieces for randomization.
    sort(H, I),                  % Sort the list of randomized gold pieces.
    findgoldmove(I, G, B, C, D), % Find a valid move to an empty square.
    move_piece(C, D, B, A),      % Execute the move on the board.
    kingnotincheck(A),           % Ensure the gold king is not in check after the move.
    not(take_dest(D, s, A)),     % Confirm the moved piece is not vulnerable.
    buildgold(A, J),             % Rebuild the gold list on the new board.
    checkgold(J, A, K),          % Verify the updated gold pieces.
    length(F, L),                % Get the count of threatened gold pieces.
    delete(K, [M, N, g, p], O),  % Remove pawns from the threatened list.
    length(O, P), P =< L ;       % Check if fewer gold pieces are threatened.
    guimessage(check, g, s).     % Print a check message if applicable.

%!  takehighestopen(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Captures the highest-value silver piece that is open to attack.
%
%   Steps:
%     1. Builds a list of all gold pieces using `buildgold/2`.
%     2. Identifies gold pieces that can attack open silver pieces using `checkgold/3`.
%     3. Selects a gold piece that can move to a square containing a high-value silver piece.
%     4. Ensures the move does not leave the gold king in check.
%     5. Updates the board state with the capture move.
%
%   @arg NewBoard The resulting board after capturing the silver piece.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece used to capture.
%   @arg Dest The destination square containing the silver piece.
%
%   @see buildgold/2
%   @see checkgold/3
%   @see xy_box/2
%   @see move_piece/4
%   @see kingnotincheck/1
takehighestopen(A, B, C, D) :-
    buildgold(B, E),             % Get all gold pieces on the board.
    checkgold(E, B, F),          % Find gold pieces that can attack silver pieces.
    !,
    piece(G),                    % Select a type of gold piece.
    xy_box(H, [s, G], B),        % Find a silver piece on the board.
    findgoldhigh(E, B, C, H),    % Find the best gold piece to capture the silver piece.
    return_entire_box(H, D, B),  % Get the destination square containing the silver piece.
    move_piece(C, D, B, A),      % Execute the move.
    not(take_dest(H, s, A)),     % Ensure the move is safe.
    kingnotincheck(A).           % Ensure the gold king is not in check.

%!  takehighestopenpawn(-NewBoard, +CurrentBoard, -GoldPiece, -Dest) is semidet.
%
%   Captures an opponent's pawn if no higher-value pieces are available.
%
%   Steps:
%     1. Randomly decides whether to proceed with this strategy.
%     2. Builds a list of all gold pieces and checks for threatened pieces.
%     3. Finds pawns that are open for capture.
%     4. Moves a gold piece to capture the pawn and ensures king safety.
%
%   @arg NewBoard The resulting board after capturing the pawn.
%   @arg CurrentBoard The current board configuration.
%   @arg GoldPiece The gold piece capturing the pawn.
%   @arg Dest The destination square containing the pawn.
%
%   @see buildgold/2
%   @see checkgold/3
%   @see xy_box/2
%   @see move_piece/4
%   @see kingnotincheck/1
takehighestopenpawn(A, B, C, D) :-
    returnrandominteger(E, 3),   % Generate a random integer (1-3).
    !,
    (E == 1 ; E == 2),           % Proceed with capturing a pawn if E is 1 or 2.
    buildgold(B, F),             % Build the list of gold pieces.
    checkgold(F, B, G),          % Identify gold pieces under threat.
    !,
    piece(H), H \= p,            % Select a non-pawn gold piece.
    xy_box(I, [s, H], B),        % Find a silver piece on the board.
    findgoldhigh(F, B, [J, K, g, p], I), % Identify a gold piece to capture a pawn.
    C = [J, K, g, p],            % Define the capturing gold piece.
    return_entire_box(I, D, B),  % Get the destination square.
    move_piece(C, D, B, A),      % Execute the move.
    kingnotincheck(A).           % Ensure the gold king remains safe.

%!  checkgold(+GoldPieces, +Board, -Threatened) is det.
%
%   Determines which gold pieces are under threat from silver pieces.
%
%   @arg GoldPieces List of gold pieces on the board.
%   @arg Board The current board configuration.
%   @arg Threatened List of gold pieces under threat.
%
%   @see take_dest/3
checkgold([], A, []) :- !.
checkgold([A|B], C, [A|D]) :-
    take_dest(A, s, C),  % Check if a silver piece can attack this gold piece.
    checkgold(B, C, D), !.
checkgold([A|B], C, D) :-
    not(take_dest(A, s, C)), % If not under threat, continue with the next piece.
    checkgold(B, C, D), !.

%!  lookforempty(+Board, -EmptySquares) is det.
%
%   Finds all empty squares on the board.
%
%   @arg Board The current board configuration.
%   @arg EmptySquares List of empty squares represented as coordinates.
%
%   @see len/2
lookforempty([], []) :- true.
lookforempty([A|B], [A|C]) :-
    len(A, 2),  % Check if the square is empty.
    lookforempty(B, C).
lookforempty([A|B], C) :-
    lookforempty(B, C).

%!  buildrandomgold(+GoldPieces, -RandomizedGoldPieces) is det.
%
%   Assigns random priority values to gold pieces for randomized decision-making.
%
%   @arg GoldPieces List of gold pieces.
%   @arg RandomizedGoldPieces List of gold pieces with random values assigned.
%
%   @see returnrandominteger/2
buildrandomgold([], []) :- true.
buildrandomgold([A|B], [C|D]) :-
    len(A, 4),                     % Ensure the square contains a gold piece.
    nth1(3, A, g),                 % Confirm it is a gold piece.
    returnrandominteger(E, 99),    % Assign a random integer as priority.
    F is E,
    concat_lists([[F], [A]], C),   % Combine the random value with the gold piece.
    buildrandomgold(B, D).
buildrandomgold([A|B], C) :-
    buildrandomgold(B, C).

%!  buildgold(+Board, -GoldPieces) is det.
%
%   Retrieves all gold pieces from the board.
%
%   @arg Board The current board configuration.
%   @arg GoldPieces List of gold pieces found on the board.
%
%   @see len/2
buildgold([], []) :- !.
buildgold([A|B], [A|C]) :-
    len(A, 4),                     % Ensure the square contains a piece.
    nth1(3, A, g),                 % Check if the piece belongs to gold.
    buildgold(B, C), !.
buildgold([A|B], C) :-
    buildgold(B, C), !.

%!  findgoldmove(+RandomizedGoldPieces, +EmptySquares, +Board, -GoldPiece, -Dest) is semidet.
%
%   Finds a random valid move for a gold piece to an empty square.
%
%   This predicate:
%     1. Selects a gold piece with a randomized priority from `RandomizedGoldPieces`.
%     2. Randomly generates coordinates to find a valid destination.
%     3. Uses `findgolddest/6` to validate the destination square.
%
%   @arg RandomizedGoldPieces A list of gold pieces with assigned random priorities.
%   @arg EmptySquares List of all empty squares available on the board.
%   @arg Board The current board configuration.
%   @arg GoldPiece The selected gold piece to move.
%   @arg Dest The destination square for the move.
%
%   @see returnrandominteger/2
%   @see findgolddest/6
findgoldmove([A|B], C, D, E, F) :-
    [G, E] = A,                  % Extract the gold piece from the randomized list.
    returnrandominteger(H, 8),   % Generate a random file (1-8).
    I is H,
    returnrandominteger(J, 8),   % Generate a random rank (1-8).
    K is J,
    !,
    findgolddest(E, D, C, F, I, K). % Validate the move to the destination.

%!  findgolddest(+GoldPiece, +Board, +EmptySquares, -Dest, +File, +Rank) is semidet.
%
%   Determines whether a gold piece can move to a specified empty square.
%
%   This predicate:
%     1. Checks if the generated destination square (`File`, `Rank`) is in the list of empty squares.
%     2. Verifies there is a clear route for the gold piece to move to the destination.
%
%   @arg GoldPiece The gold piece being moved.
%   @arg Board The current board configuration.
%   @arg EmptySquares List of empty squares available on the board.
%   @arg Dest The validated destination square as `[File, Rank]`.
%   @arg File The file (column) of the destination.
%   @arg Rank The rank (row) of the destination.
%
%   @see clear_route/3
findgolddest(A, B, C, D, E, F) :-
    D = [E, F],           % Set the destination square.
    member(D, C),         % Ensure the destination is an empty square.
    clear_route(A, D, B). % Verify there is a clear route to the destination.

%!  findgoldhigh(+GoldPieces, +Board, -BestGoldPiece, +TargetSquare) is semidet.
%
%   Finds the best gold piece that can move to a target square.
%
%   This predicate:
%     1. Iterates through the list of gold pieces.
%     2. Checks if there is a clear route for each piece to move to the target square.
%     3. Returns the first piece that satisfies the condition.
%
%   @arg GoldPieces List of gold pieces on the board.
%   @arg Board The current board configuration.
%   @arg BestGoldPiece The gold piece that can move to the target square.
%   @arg TargetSquare The square to which a move is being considered.
%
%   @see clear_route/3
findgoldhigh([], A, B, C) :-
    fail. % No valid gold piece found.

findgoldhigh([A|B], C, A, D) :-
    clear_route(A, D, C). % Check if a clear route exists for the gold piece.

findgoldhigh([A|B], C, D, E) :-
    findgoldhigh(B, C, D, E). % Continue checking other gold pieces.

%!  take_dest(+Square, +OpponentColor, +Board) is semidet.
%
%   Determines if a square can be attacked by an opponent's pieces.
%
%   This predicate:
%     1. Identifies all possible attacking moves by the opponent's pieces.
%     2. Checks if the specified square is within the list of attackable squares.
%
%   @arg Square The square being evaluated.
%   @arg OpponentColor The opponent's piece color (`s` for silver or `g` for gold).
%   @arg Board The current board configuration.
%
%   @see takingboxes/3
%   @see list_clear_route/4
take_dest(A, B, C) :-
    takingboxes(B, C, D),       % Identify opponent pieces that can attack.
    !,
    list_clear_route(C, A, D, E), % Check if any route leads to the specified square.
    !,
    E \== [].                   % Ensure the list of attackable routes is non-empty.

%!  return_entire_box(+Coordinates, -Box, +Board) is det.
%
%   Retrieves the full details of a square on the board based on its coordinates.
%
%   This predicate:
%     1. Searches for a square in the board list that matches the specified coordinates.
%     2. Returns the full representation of the square.
%
%   @arg Coordinates The coordinates of the square as `[File, Rank]`.
%   @arg Box The full representation of the square (including piece details if present).
%   @arg Board The current board configuration.
%
%   @see concat_lists/2
return_entire_box(A, B, [C|D]) :-
    nth1(1, C, E), nth1(2, C, F), % Extract the file and rank of the square.
    concat_lists([[E], [F]], G),  % Combine the file and rank into coordinates.
    G == A,                      % Check if the coordinates match.
    B = C,                       % Return the full square details.
    !.
return_entire_box(A, B, [C|D]) :-
    return_entire_box(A, B, D), !. % Continue searching the remaining board.

%!  xy_box(-Coordinates, +PieceSpec, +Board) is semidet.
%
%   Finds the coordinates of a piece that matches a given specification.
%
%   This predicate:
%     1. Searches the board for a square containing the specified piece.
%     2. Returns the coordinates of the square if a match is found.
%
%   @arg Coordinates The coordinates of the matching square as `[File, Rank]`.
%   @arg PieceSpec The piece specification as `[Color, PieceType]`.
%   @arg Board The current board configuration.
%
%   @see len/2
xy_box(A, [B, C], [D|E]) :-
    len(D, 4),                % Ensure the square contains a piece.
    nth1(3, D, F),            % Extract the color of the piece.
    nth1(4, D, G),            % Extract the type of the piece.
    B == F, C == G,           % Check if the color and type match.
    nth1(1, D, H), nth1(2, D, I), % Extract the file and rank.
    concat_lists([[H], [I]], A). % Combine into coordinates.

xy_box(A, B, [C|D]) :-
    xy_box(A, B, D). % Continue searching the remaining board.

%!  samecolor(+Box1, +Box2) is semidet.
%
%   Checks whether two squares contain pieces of the same color.
%
%   This predicate extracts the piece color from each box and compares them.
%
%   @arg Box1 The first square on the board.
%   @arg Box2 The second square on the board.
%
%   @see nth1/3
samecolor(A, B) :-
    nth1(3, A, C), % Extract the color from the first square.
    nth1(3, B, D), % Extract the color from the second square.
    !,
    C == D.       % Check if the colors are the same.

%!  clear_route(+Piece, +Destination, +Board) is semidet.
%
%   Checks whether a specified piece can move to a destination square on the board.
%
%   This predicate verifies the validity of a move by ensuring the path is clear for
%   the specified piece type. It handles the unique movement rules for kings, knights,
%   queens, pawns, rooks, and bishops.
%
%   Rules per piece type:
%     - King (`k`): Moves one square in any direction.
%     - Knight (`n`): Moves in an "L" shape (two squares in one direction, one square perpendicular).
%     - Queen (`q`): Combines the movement of rooks and bishops.
%     - Rook (`r`): Moves horizontally or vertically, ensuring no obstructions.
%     - Bishop (`b`): Moves diagonally, ensuring no obstructions.
%     - Pawn (`p`):
%         - Gold pawn (`g`): Moves forward and captures diagonally.
%         - Silver pawn (`s`): Moves forward and captures diagonally in the opposite direction.
%
%   @arg Piece The piece being moved, represented as `[X, Y, Color, Type]`.
%   @arg Destination The target square coordinates `[X, Y]`.
%   @arg Board The current board configuration.
%
%   @see return_entire_box/3
%   @see checkclearup/4
%   @see checkcleardown/4
%   @see checkclearleft/4
%   @see checkclearright/4
%   @see checkclearupBUR/6
%   @see checkclearupBDR/6
%   @see checkclearupBUL/6
%   @see checkclearupBDL/6
clear_route([A, B, C, k], [D, E|F], G) :-
    % King: Moves one square in any direction.
    (D = A ; D is A + 1 ; D is A - 1),
    (E = B ; E is B + 1 ; E is B - 1).

clear_route([A, B, C, n], [D, E|F], G) :-
    % Knight: Moves in an "L" shape (2 steps in one direction, 1 step perpendicular).
    (E is B + 2 ; E is B - 2),
    (D is A + 1 ; D is A - 1).

clear_route([A, B, C, n], [D, E|F], G) :-
    (E is B + 1 ; E is B - 1),
    (D is A + 2 ; D is A - 2).

clear_route([A, B, C, q], [D, E|F], G) :-
    % Queen: Combines the movement of a rook and bishop.
    clear_route([A, B, C, r], [D, E|F], G).
clear_route([A, B, C, q], [D, E|F], G) :-
    clear_route([A, B, C, b], [D, E|F], G).

clear_route([A, B, g, p], [C, D|E], F) :-
    % Gold pawn: Moves forward and captures diagonally.
    A = C,
    %B = 2,
    B = 7,  % fixed
    G is B - 1,
    return_entire_box([A, G], H, F), len(H, 2),
    D is B - 2,
    return_entire_box([C, D], I, F), len(I, 2).

clear_route([A, B, g, p], [C, D|E], F) :-
    A = C,
    D is B - 1,
    return_entire_box([C, D], G, F), len(G, 2).

clear_route([A, B, g, p], [C, D|E], F) :-
    return_entire_box([C, D], G, F), len(G, 4),
    C is A + 1, D is B - 1.

clear_route([A, B, g, p], [C, D|E], F) :-
    return_entire_box([C, D], G, F), len(G, 4),
    C is A - 1, D is B - 1.

clear_route([A, B, s, p], [C, D|E], F) :-
    % Silver pawn: Moves forward and captures diagonally.
    A = C,
    D is B + 1,
    return_entire_box([C, D], G, F), len(G, 2).

clear_route([A, B, s, p], [C, D|E], F) :-
    A = C,
    B = 2,
    G is B + 1,
    return_entire_box([A, G], H, F), len(H, 2),
    D is B + 2,
    return_entire_box([C, D], I, F), len(I, 2).

clear_route([A, B, s, p], [C, D|E], F) :-
    return_entire_box([C, D], G, F), len(G, 4),
    C is A + 1, D is B + 1.

clear_route([A, B, s, p], [C, D|E], F) :-
    return_entire_box([C, D], G, F), len(G, 4),
    C is A - 1, D is B + 1.

clear_route([A, B, C, r], [D, E|F], G) :-
    % Rook: Moves horizontally or vertically.
    A = D, E > B,
    H is E - 1, I is B + 1,
    checkclearup(A, I, H, G).

clear_route([A, B, C, r], [D, E|F], G) :-
    A = D, E < B,
    H is E + 1, I is B - 1,
    checkcleardown(A, I, H, G).

clear_route([A, B, C, r], [D, E|F], G) :-
    A < D, E = B,
    H is D - 1, I is A + 1,
    checkclearright(B, I, H, G).

clear_route([A, B, C, r], [D, E|F], G) :-
    A > D, E = B,
    H is D + 1, I is A - 1,
    checkclearleft(B, I, H, G).

clear_route([A, B, C, b], [D, E|F], G) :-
    % Bishop: Moves diagonally.
    D is A + 1, E is B + 1 ;
    D > A, E > B,
    H is A + 1, I is D - 1,
    J is B + 1, K is E - 1,
    checkclearupBUR(H, J, I, K, G).

clear_route([A, B, C, b], [D, E|F], G) :-
    D is A + 1, E is B - 1 ;
    D > A, E < B,
    H is A + 1, I is D - 1,
    J is B - 1, K is E + 1,
    checkclearupBDR(H, J, I, K, G).

clear_route([A, B, C, b], [D, E|F], G) :-
    D is A - 1, E is B + 1 ;
    D < A, E > B,
    H is A - 1, I is D + 1,
    J is B + 1, K is E - 1,
    checkclearupBUL(H, J, I, K, G).

clear_route([A, B, C, b], [D, E|F], G) :-
    D is A - 1, E is B - 1 ;
    D < A, E < B,
    H is A - 1, I is D + 1,
    J is B - 1, K is E + 1,
    checkclearupBDL(H, J, I, K, G).

%!  checkclearup(+A, +B, +C, +D) is semidet.
%
%   Checks if the upward path from rank B to rank C in file A is clear.
%
%   This predicate ensures that all squares between the starting rank (B) 
%   and the ending rank (C) are empty. Stops when B exceeds C.
%
%   @arg A The file (column) to check.
%   @arg B The current rank being checked.
%   @arg C The ending rank (exclusive).
%   @arg D The current board configuration.
checkclearup(A, B, C, D) :-
    B > C.  % Stop when the current rank exceeds the ending rank.
checkclearup(A, B, C, D) :-
    return_entire_box([A, B], E, D),  % Get the box details at (A, B).
    len(E, 2),                        % Ensure the box is empty.
    F is B + 1,                       % Move one rank up.
    checkclearup(A, F, C, D).         % Recursively check the next rank.

%!  checkclearleft(+A, +B, +C, +D) is semidet.
%
%   Checks if the leftward path from file B to file C in rank A is clear.
%
%   This predicate ensures all squares between the starting file (B)
%   and the ending file (C) are empty. Stops when B is less than C.
%
%   @arg A The rank (row) to check.
%   @arg B The current file being checked.
%   @arg C The ending file (exclusive).
%   @arg D The current board configuration.
checkclearleft(A, B, C, D) :-
    B < C.  % Stop when the current file is less than the ending file.
checkclearleft(A, B, C, D) :-
    return_entire_box([B, A], E, D),  % Get the box details at (B, A).
    len(E, 2),                        % Ensure the box is empty.
    F is B - 1,                       % Move one file left.
    checkclearleft(A, F, C, D).       % Recursively check the next file.

%!  checkclearright(+A, +B, +C, +D) is semidet.
%
%   Checks if the rightward path from file B to file C in rank A is clear.
%
%   This predicate ensures all squares between the starting file (B)
%   and the ending file (C) are empty. Stops when B exceeds C.
%
%   @arg A The rank (row) to check.
%   @arg B The current file being checked.
%   @arg C The ending file (exclusive).
%   @arg D The current board configuration.
checkclearright(A, B, C, D) :-
    B > C.  % Stop when the current file exceeds the ending file.
checkclearright(A, B, C, D) :-
    return_entire_box([B, A], E, D),  % Get the box details at (B, A).
    len(E, 2),                        % Ensure the box is empty.
    F is B + 1,                       % Move one file right.
    checkclearright(A, F, C, D).      % Recursively check the next file.

%!  checkcleardown(+A, +B, +C, +D) is semidet.
%
%   Checks if the downward path from rank B to rank C in file A is clear.
%
%   This predicate ensures all squares between the starting rank (B)
%   and the ending rank (C) are empty. Stops when B is less than C.
%
%   @arg A The file (column) to check.
%   @arg B The current rank being checked.
%   @arg C The ending rank (exclusive).
%   @arg D The current board configuration.
checkcleardown(A, B, C, D) :-
    B < C.  % Stop when the current rank is less than the ending rank.
checkcleardown(A, B, C, D) :-
    return_entire_box([A, B], E, D),  % Get the box details at (A, B).
    len(E, 2),                        % Ensure the box is empty.
    F is B - 1,                       % Move one rank down.
    checkcleardown(A, F, C, D).       % Recursively check the next rank.

%!  checkclearupBUR(+A, +B, +C, +D, +E) is semidet.
%
%   Checks if the upward-right diagonal path is clear.
%
%   Stops when the current file (A) equals the destination file (C) and the 
%   current rank (B) equals the destination rank (D). Ensures all intermediate 
%   squares are empty.
%
%   @arg A The current file.
%   @arg B The current rank.
%   @arg C The destination file.
%   @arg D The destination rank.
%   @arg E The current board configuration.
checkclearupBUR(A, B, C, D, E) :-
    A == C, B == D,                      % Stop when destination is reached.
    return_entire_box([A, B], F, E),     % Get the box details.
    len(F, 2).                           % Ensure the box is empty.
checkclearupBUR(A, B, C, D, E) :-
    return_entire_box([A, B], F, E),     % Get the box details.
    len(F, 2),                           % Ensure the box is empty.
    G is A + 1,                          % Move one file right.
    H is B + 1,                          % Move one rank up.
    checkclearupBUR(G, H, C, D, E).      % Recursively check the next square.

%!  checkclearupBDR(+A, +B, +C, +D, +E) is semidet.
%
%   Checks if the upward-left diagonal path is clear.
%
%   Stops when the current file (A) equals the destination file (C) and the 
%   current rank (B) equals the destination rank (D). Ensures all intermediate 
%   squares are empty.
%
%   @arg A The current file.
%   @arg B The current rank.
%   @arg C The destination file.
%   @arg D The destination rank.
%   @arg E The current board configuration.
checkclearupBDR(A, B, C, D, E) :-
    A == C, B == D,                      % Stop when destination is reached.
    return_entire_box([A, B], F, E),     % Get the box details.
    len(F, 2).                           % Ensure the box is empty.
checkclearupBDR(A, B, C, D, E) :-
    return_entire_box([A, B], F, E),     % Get the box details.
    len(F, 2),                           % Ensure the box is empty.
    G is A + 1,                          % Move one file right.
    H is B - 1,                          % Move one rank down.
    checkclearupBDR(G, H, C, D, E).      % Recursively check the next square.



%!  checkclearupBUL(+A, +B, +C, +D, +E) is semidet.
%
%   Checks if the upward-left diagonal path is clear.
%
%   Stops when the current file (A) equals the destination file (C) and 
%   the current rank (B) equals the destination rank (D). Ensures all 
%   intermediate squares are empty.
%
%   @arg A The current file.
%   @arg B The current rank.
%   @arg C The destination file.
%   @arg D The destination rank.
%   @arg E The current board configuration.
checkclearupBUL(A, B, C, D, E) :-
    A == C, B == D, return_entire_box([A, B], F, E), len(F, 2).
checkclearupBUL(A, B, C, D, E) :-
    return_entire_box([A, B], F, E), len(F, 2), 
    G is A - 1, H is B + 1, 
    checkclearupBUL(G, H, C, D, E).

%!  checkclearupBDL(+A, +B, +C, +D, +E) is semidet.
%
%   Checks if the downward-left diagonal path is clear.
%
%   Stops when the current file (A) equals the destination file (C) and 
%   the current rank (B) equals the destination rank (D). Ensures all 
%   intermediate squares are empty.
%
%   @arg A The current file.
%   @arg B The current rank.
%   @arg C The destination file.
%   @arg D The destination rank.
%   @arg E The current board configuration.
checkclearupBDL(A, B, C, D, E) :-
    A == C, B == D, return_entire_box([A, B], F, E), len(F, 2).
checkclearupBDL(A, B, C, D, E) :-
    return_entire_box([A, B], F, E), len(F, 2), 
    G is A - 1, H is B - 1, 
    checkclearupBDL(G, H, C, D, E).

%!  move_piece(+A, +B, +C, -D) is det.
%
%   Moves a piece from square A to square B on the board C, producing a new board D.
%
%   This predicate ensures that the move adheres to Prolog's logical rules:
%   - It retrieves the color and type of the piece being moved.
%   - It removes the piece from its starting position and places it at the destination.
%   - It produces the updated board state.
%
%   @arg A The starting square as a list [File, Rank, Color, Piece].
%   @arg B The destination square as a list [File, Rank].
%   @arg C The current board configuration.
%   @arg D The updated board configuration after the move.
move_piece(A, B, C, D) :-
    nth1(3, A, E), nth1(4, A, F), 
    sort(C, G), sort([A, B], H), 
    removelists(H, G, I), 
    nth1(1, A, J), nth1(2, A, K), 
    L = [J, K], 
    nth1(1, B, M), nth1(2, B, N), 
    O = [M, N, E, F], 
    sort([L, O | I], D), !.

%!  printmove(+A, +B, +C) is det.
%
%   Prints the details of a move from square A to square B and updates the game log.
%
%   This predicate displays the move in human-readable format, including:
%   - Starting and ending positions of the piece.
%   - Whether a piece was captured in the move.
%   It also updates the `guimessage/4` predicate to reflect the move.
%
%   @arg A The starting square as a list [File, Rank, Color, Piece].
%   @arg B The destination square as a list [File, Rank].
%   @arg C The current board configuration.
printmove(A, B, C) :-
    nth1(1, A, D), nth1(2, A, E), 
    nth1(3, A, F), nth1(4, A, G), 
    nth1(1, B, H), nth1(2, B, I), 
    (len(B, 4), return_entire_box([H, I], J, C), nth1(4, J, K); K = nil),
    (F == g, 
        write('DBD moves from:'), write(D), write((',')), write(E), 
        write(' to: '), write(H), write((',')), write(I);
     F == s, 
        write('YOU move from:'), write(D), write((',')), write(E), 
        write(' to: '), write(H), write((',')), write(I)),
    asserta(guimessage(move, A, B, K)),
    (K \= nil, nl, write('Piece captured!! -> '), write(K), nl; nl),
    write('Type c. for commands you can use.').

%!  examine_king(+Board, +KingColor, +OpponentColor) is det.
%
%   Evaluates the state of the king to determine if it is in check or checkmate.
%
%   This predicate performs the following:
%     1. Identifies if any opponent pieces can attack the king.
%     2. If no valid moves exist to escape the attack and block the threat, it declares checkmate.
%     3. If the king is under attack but can escape or the threat can be blocked, it declares check.
%     4. If no threats exist, the game continues without further action.
%
%   @arg Board         The current board configuration.
%   @arg KingColor     The color of the king being evaluated.
%   @arg OpponentColor The color of the opponent's pieces.
examine_king(A, B, C) :-
    cantakepiece(A, B, k, C, D), D \= [],
    threatOK1(A, B, C, D), threatOK2(A, B, C), threatOK3(A, B, C),
    write('Checkmate!'), nl,
    asserta(guimessage(checkmate, B, C)).
examine_king(A, B, C) :-
    cantakepiece(A, B, k, C, D), D \= [],
    write('Check!'), nl,
    asserta(guimessage(check, B, C)).
examine_king(_, _, _) :- true.

%!  threatOK1(+Board, +KingColor, +OpponentColor, +Threats) is semidet.
%
%   Confirms whether there are no pieces capable of stopping the checkmate threat.
%
%   This predicate checks for any valid opponent moves that could disrupt the 
%   threat on the king. If no such moves exist, it validates the checkmate condition.
%
%   @arg Board         The current board configuration.
%   @arg KingColor     The color of the king under threat.
%   @arg OpponentColor The color of the opponent's pieces.
%   @arg Threats       The list of positions threatening the king.
threatOK1(A, B, C, D) :-
    seekopponents(A, B, D, E), E == [], !.
threatOK1(A, B, C, D) :-
    seekopponents(A, B, D, E),
    checkthreat(E, A), !.

%!  checkthreat(+Threats, +Board) is det.
%
%   Validates all threats to determine their impact on the king's safety.
%
%   This predicate iterates through all pieces threatening the king and evaluates 
%   their moves to confirm the threat's validity. If any move results in a valid 
%   checkmate scenario, the threat is confirmed.
%
%   @arg Threats The list of pieces threatening the king.
%   @arg Board   The current board configuration.
checkthreat([], _) :- !.
checkthreat([A | B], C) :-
    checkeachthreat(A, C),!,
    checkthreat(B, C), !.

%!  checkeachthreat(+ThreatMove, +Board) is det.
%
%   Determines if a specific move results in a valid threat to the king.
%
%   This predicate simulates the move of a threatening piece and checks if the 
%   move results in the king being in check. It evaluates the threat by:
%     - Retrieving the piece type and color.
%     - Simulating the move on the board.
%     - Verifying if the king's position is now threatened.
%
%   @arg ThreatMove The move being evaluated for validity.
%   @arg Board      The current board configuration.
checkeachthreat([], _) :- true.
checkeachthreat([A, B | C], D) :-
    nth1(3, A, E), nth1(3, B, F),
    move_piece(A, B, D, G),
    xy_box(H, [E, k], G), !,
    checkking(H, F, G),
    checkeachthreat(C, D).

%!  checkking(+KingPosition, +OpponentColor, +Board) is semidet.
%
%   Determines whether the king's current position is threatened by the opponent.
%
%   This predicate checks if any opponent piece can attack the king's position 
%   on the updated board.
%
%   @arg KingPosition   The position of the king.
%   @arg OpponentColor  The color of the opponent's pieces.
%   @arg Board          The updated board configuration after a move.
checkking(A, B, C) :-
    take_dest(A, B, C).

%!  threatOK2(+Board, +KingColor, +OpponentColor) is semidet.
%
%   Determines if the king has any legal moves to escape check.
%
%   This predicate identifies all empty squares on the board and checks if the 
%   king can move to any of these squares to escape the current threat. If no 
%   such moves exist, the predicate succeeds, confirming that the king is trapped.
%
%   @arg Board         The current board configuration.
%   @arg KingColor     The color of the king being evaluated.
%   @arg OpponentColor The color of the opponent's pieces.
threatOK2(A, B, C) :-
    lookforempty(A, D),                 % Retrieve all empty squares on the board.
    xy_box(E, [B, k], A),               % Find the current position of the king.
    return_entire_box(E, F, A),         % Get the box details for the king's position.
    !, not(king_can_move(F, C, D, A)).  % Verify if the king cannot move to any square.

%!  king_can_move(+King, +OpponentColor, +EmptySquares, +Board) is semidet.
%
%   Checks if the king can move to any empty square to escape check.
%
%   This predicate iterates through all empty squares and determines if the king 
%   can move to a square that is not threatened by the opponent. A move is valid 
%   if:
%     1. The route to the square is clear.
%     2. The resulting position is not under attack.
%
%   @arg King           The current position of the king.
%   @arg OpponentColor  The color of the opponent's pieces.
%   @arg EmptySquares   The list of empty squares on the board.
%   @arg Board          The current board configuration.
king_can_move(A, B, [C | D], E) :-
    clear_route(A, C, E),              % Check if the path to the square is clear.
    move_piece(A, C, E, F),            % Simulate the king's move.
    not(take_dest(C, B, F)).           % Ensure the destination square is safe.
king_can_move(A, B, [C | D], E) :-
    not(clear_route(A, C, E)), fail.   % Fail if the route is blocked.
king_can_move(A, B, [C | D], E) :-
    clear_route(A, C, E),              % Check if the path is clear.
    move_piece(A, C, E, F),            % Simulate the move.
    take_dest(C, B, F), fail.          % Fail if the destination is under attack.
king_can_move(A, B, [_ | D], E) :-
    king_can_move(A, B, D, E).         % Continue checking the remaining squares.
king_can_move(_, _, [], _) :-
    !, fail.                           % Fail if no valid moves exist.

%!  threatOK3(+Board, +KingColor, +OpponentColor) is semidet.
%
%   Checks if any opponent piece can block the threat to the king.
%
%   This predicate evaluates whether any opponent piece can move into a position 
%   that blocks the threat to the king. If no such moves are possible, the king 
%   is confirmed to be in checkmate.
%
%   @arg Board         The current board configuration.
%   @arg KingColor     The color of the king under threat.
%   @arg OpponentColor The color of the opponent's pieces.
threatOK3(A, B, C) :-
    !, not(opponentblock(A, B, C)).

%!  opponentblock(+A, +B, +C) is semidet.
%
%   Determines if a move blocks the opponent's path.
%
%   This predicate verifies whether a particular move blocks an opponent's
%   piece from reaching a destination. It checks if there is a clear route,
%   validates that the piece moves without taking a destination, and
%   ensures proper blocking logic.
%
%   @arg A  The board state or configuration.
%   @arg B  An opponent's identifier or color.
%   @arg C  The move or destination being evaluated.
%
%   @example
%     ?- opponentblock(Board, b, Dest).
%
opponentblock(A, B, C):-
    xy_box(D, [B, k], A),
    rpiece(E),
    E\==k,
    xy_box(F, [B, E], A),
    return_entire_box(F, G, A),
    cord(H),
    cord(I),
    return_entire_box([H, I], J, A),
    (not(samecolor(G, J));len(J, 2)),
    clear_route(G, J, A),
    move_piece(G, J, A, K),
    not(take_dest(D, C, K)),
    !.

%!  seekopponents(+A, +B, +C, -D) is det.
%
%   Identifies opponent pieces and their potential moves.
%
%   This predicate builds a list of opponent moves and evaluates which pieces
%   can take specific targets. It filters out empty results.
%
%   @arg A  The board state or configuration.
%   @arg B  The opponent's identifier or color.
%   @arg C  List of target pieces or positions.
%   @arg D  Resulting list of valid opponent moves.
%
%   @example
%     ?- seekopponents(Board, b, Targets, Moves).
%
seekopponents(A, B, C, D):-
    buildopponent(A, B, E),
    !,
    takingpieces(E, C, A, F),
    !,
    delete(F, [], D),
    !.

%!  takingpieces(+List, +Targets, +Board, -Result) is det.
%
%   Determines which pieces can take the given targets.
%
%   @arg List    A list of opponent pieces.
%   @arg Targets A list of target positions or pieces.
%   @arg Board   The board state or configuration.
%   @arg Result  A list of valid taking pieces.
%
takingpieces([], A, B, []):- true.
takingpieces([A|B], C, D, [E|F]):-
    checkopponent(A, C, D, E),
    !,
    takingpieces(B, C, D, F).
takingpieces([A|B], C, D, E):-
    takingpieces(B, C, D, E).

%!  checkopponent(+Piece, +Targets, +Board, -Result) is det.
%
%   Verifies if a specific piece can reach a target.
%
%   @arg Piece   A single opponent piece.
%   @arg Targets List of target positions or pieces.
%   @arg Board   The board state or configuration.
%   @arg Result  Result indicating whether the piece can reach any target.
%
checkopponent(A, [], B, []):- true.
checkopponent(A, [B|C], D, [A, B|E]):-
    clear_route(A, B, D),
    checkopponent(A, C, D, E).
checkopponent(A, [B|C], D, E):-
    checkopponent(A, C, D, E).

%!  takeyourpiece(+List, +Board) is semidet.
%
%   Identifies moves where a piece can be taken.
%
%   @arg List  A list of positions or pieces to evaluate.
%   @arg Board The board state or configuration.
%
takeyourpiece([[]|A], B):- takeyourpiece(A, C, B).
takeyourpiece([[]|A], B):- takeyourpiece(A, C, B).

%!  buildopponent(+List, +Color, -Result) is det.
%
%   Constructs a list of opponent pieces based on the given color.
%
%   @arg List   The list of pieces to evaluate.
%   @arg Color  The opponent's color.
%   @arg Result Resulting list of opponent pieces.
%
buildopponent([], A, []):- true.
buildopponent([A|B], C, [A|D]):-
    len(A, 4),
    nth1(3, A, C),
    buildopponent(B, C, D).
buildopponent([A|B], C, D):-
    buildopponent(B, C, D).

%!  seekopponent(+Pieces, +Color, +Targets, +Board, -Result) is det.
%
%   Searches for specific opponent pieces that can reach a target.
%
%   @arg Pieces  List of opponent pieces.
%   @arg Color   The opponent's color.
%   @arg Targets List of targets.
%   @arg Board   The board state.
%   @arg Result  Resulting piece that meets the condition.
%
seekopponent([], A, B, C, []):- true.
seekopponent([A|B], C, D, E, A):-
    len(A, 4),
    nth1(3, A, C),
    takeyourpiece(D, A, E).
seekopponent([A|B], C, D, E, F):-
    seekopponent(B, C, D, E, F).


%!  takeyourpiece(+List, +Piece, +Board) is semidet.
%
%   Verifies if a piece can take another piece on the board.
%
%   This predicate checks if the given piece can move to take any target
%   from the list. If the list is empty, the predicate fails.
%
%   @arg List  A list of target positions or pieces.
%   @arg Piece The piece attempting to take another piece.
%   @arg Board The board state or configuration.
%
%   @example
%     ?- takeyourpiece([[x1, y1], [x2, y2]], [x1, y1], Board).
%
takeyourpiece([], A, B):- fail.
takeyourpiece([A|B], C, D):-
    clear_route(C, A, D).

%!  cantakepiece(+Board, +Color, +Piece, +Opponents, -Result) is semidet.
%
%   Determines if a piece can take an opponent's piece.
%
%   This predicate identifies valid moves where the player's piece can
%   take an opponent's piece by checking clear routes to the target.
%
%   @arg Board     The board state or configuration.
%   @arg Color     The player's color.
%   @arg Piece     The piece in question.
%   @arg Opponents List of opponent pieces.
%   @arg Result    A list of valid target pieces or positions.
%
%   @example
%     ?- cantakepiece(Board, white, piece1, Opponents, Result).
%
cantakepiece(A, B, C, D, E):-
    takingboxes(D, A, F),
    !,
    xy_box(G, [B, C], A),
    list_clear_route(A, G, F, E).

%!  takingboxes(+Color, +Pieces, -Result) is det.
%
%   Filters opponent pieces based on their color.
%
%   This predicate constructs a list of pieces belonging to a given color.
%
%   @arg Color   The color of the opponent pieces to filter.
%   @arg Pieces  List of all pieces to evaluate.
%   @arg Result  Resulting list of pieces matching the color.
%
%   @example
%     ?- takingboxes(white, [[_, _, white], [_, _, black]], Result).
%
takingboxes(A, [], []):- true.
takingboxes(A, [B|C], [B|D]):-
    nth1(3, B, A),
    takingboxes(A, C, D).
takingboxes(A, [B|C], D):-
    takingboxes(A, C, D).

%!  list_clear_route(+Board, +Piece, +Targets, -Result) is det.
%
%   Determines which routes to targets are clear.
%
%   This predicate verifies a clear path between the player's piece
%   and a list of target positions, filtering the valid ones.
%
%   @arg Board   The board state or configuration.
%   @arg Piece   The piece attempting to move.
%   @arg Targets List of target positions or pieces.
%   @arg Result  List of targets with clear routes.
%
%   @example
%     ?- list_clear_route(Board, piece, [[x1, y1], [x2, y2]], Result).
%
list_clear_route(A, B, [], []):- true.
list_clear_route(A, B, [C|D], [C|E]):-
    clear_route(C, B, A),
    list_clear_route(A, B, D, E).
list_clear_route(A, B, [C|D], E):-
    list_clear_route(A, B, D, E).

%!  positiontotake(+Position, +Piece, -Result, +Board) is semidet.
%
%   Evaluates positions where a piece can move or take.
%
%   This predicate determines if a piece can move to a specific position,
%   ensuring the route is clear and checking that the destination is valid.
%
%   @arg Position A target position.
%   @arg Piece    The piece attempting to move.
%   @arg Result   The resulting target position or configuration.
%   @arg Board    The board state or configuration.
%
%   @example
%     ?- positiontotake([x, y], piece, Result, Board).
%
positiontotake([A, B|C], D, E, F):-
    cord(G),
    cord(H),
    return_entire_box([G, H], E, F),
    (not(samecolor(D, E));len(E, 2)),
    clear_route(D, E, F),
    nth1(3, D, I),
    nth1(4, D, J),
    K = [G, H, I, J],
    clear_route(K, [A, B], F).

/************* USER ROUTINES ************/

%!  concat_lists(+Lists, -Result) is det.
%
%   Concatenates a list of lists into a single list.
%
%   This predicate merges nested lists into a single flat list. Empty lists
%   are skipped during the concatenation process.
%
%   @arg Lists  A list of lists to be concatenated.
%   @arg Result The resulting flat list after concatenation.
%
%   @example
%     ?- concat_lists([[1,2], [3,4], [], [5]], Result).
%     Result = [1,2,3,4,5].
%
concat_lists([], []):- true.
concat_lists([[]|A], B):- concat_lists(A, B).
concat_lists([[A|B]|C], [A|D]):- concat_lists([B|C], D).

%!  nth(+Index, +List, -Element) is semidet.
%
%   Retrieves the element at a specified position in the list.
%
%   The position is zero-based. If the index is valid, the corresponding
%   element is returned.
%
%   @arg Index   The zero-based position of the element to retrieve.
%   @arg List    The list from which the element is to be fetched.
%   @arg Element The element at the specified position.
%
%   @example
%     ?- nth(0, [a, b, c], X).
%     X = [a].
%
%     ?- nth(1, [a, b, c], X).
%     X = b.
%
nth(0, [A|_], [A]):- true.
nth(1, [B|_], B):- true.
nth(A, [_|C], D):- 
    E is A-1, 
    nth(E, C, D).

%!  nth1(+Index, +List, -Element) is semidet.
%
%   Retrieves the element at the one-based position in the list.
%
%   This predicate succeeds if the index is valid and the corresponding
%   element in the list matches.
%
%   @arg Index   The one-based position of the element.
%   @arg List    The list to retrieve the element from.
%   @arg Element The element at the specified position.
%
%   @example
%     ?- nth1(1, [a, b, c], X).
%     X = a.
%
nth1(Index, _, _) :- 
    Index < 1, 
    fail, 
    !.
nth1(1, [Element|_], Element) :- !.
nth1(I, [_|List1], Element) :-
    Index is I - 1,
    nth1(Index, List1, Element).

%!  removelists(+List1, +List2, -Result) is det.
%
%   Removes all elements of List1 from List2, producing Result.
%
%   @arg List1  List of elements to be removed.
%   @arg List2  The original list.
%   @arg Result The resulting list after removal.
%
%   @example
%     ?- removelists([1,2], [1,2,3,4], Result).
%     Result = [3,4].
%
removelists([], A, A):- true.
removelists([A|B], [A|C], D):- removelists(B, C, D).
removelists(A, [B|C], [B|D]):- removelists(A, C, D).

%!  len(+List, -Length) is det.
%
%   Computes the length of a list.
%
%   The predicate calculates the number of atomic elements in a list.
%
%   @arg List   The list whose length is to be determined.
%   @arg Length The computed length of the list.
%
%   @example
%     ?- len([a, b, c], L).
%     L = 3.
%
len([], 0):- !.
len([A], 1):- atomic(A), !.
len([A|B], C):- 
    atomic(A), 
    len(B, D), 
    C is D+1.

%!  returnrandominteger(-RandomInt, +UpperLimit) is det.
%
%   Generates a random integer within the range [1, UpperLimit].
%
%   @arg RandomInt  The generated random integer.
%   @arg UpperLimit The upper bound for the random number.
%
%   @example
%     ?- returnrandominteger(X, 10).
%     X = 7.
%
returnrandominteger(A, B):- 
    A is random(B)+1.
