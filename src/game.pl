:- use_module(library(system), [now/1]).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- consult(menu).
:- consult(utils).
:- consult(board).

init_random_state :-
    now(X),
    setrand(X).

play :-
    init_random_state,
    configure_game(GameConfig),
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_cycle(GameState).

/*
* Game cycle that alternates between players and checks if the game is over:
* game_cycle(+GameState)
* GameState: The current state of the game, containing the board and player details.
* If the game is over, it ends the cycle and congratulates the winner.
* If not, it continues the game with the updated game state.
*/
game_cycle(gameState(BoardSize,Board,Player, GameType, RedType ,BlueType,Level,DiagonalRule)):-
    game_over(gameState(BoardSize,Board,Player, GameType, RedType ,BlueType,Level,DiagonalRule), Winner), !,
    congratulate(Winner).

game_cycle(GameState) :-
    nl,
    choose_move(GameState, Move),  %  Asks the player for a move.
    move(GameState, Move, NewGameState),  % Applies the move to get a new game state.
    display_game(NewGameState),  % Displays the updated game state.
    !,
    game_cycle(NewGameState).  % Continues the game with the updated game state.

/*
* Initializes the game state based on the provided configuration:
* initial_state(+GameType, +BoardSize, +Difficulty, +DiagonalRule, -GameState)
* GameType: The type of the game (e.g., 'h_h', 'h_pc').
* BoardSize: The size of the game board (e.g., 8 for an 8x8 board).
* Difficulty: The difficulty level of the game.
* DiagonalRule: Whether diagonal moves are allowed.
* GameState: The initialized game state, including the board and player types.
*/
initial_state((GameType, BoardSize, Difficulty, DiagonalRule), gameState(BoardSize, Board, red, GameType, RedType, BlueType, Level, DiagonalRule)) :-
    board(BoardSize,Board), % Generates the board based on size.
    map_difficulty(Difficulty,Level),
    map_game_type(GameType, RedType, BlueType).

map_game_type(h_h, human , human).
map_game_type(h_pc, human , bot).
map_game_type(pc_h, bot, human).
map_game_type(pc_pc, bot ,bot).

map_difficulty(DifficultyRed-DifficultyBlue, LevelRed-LevelBlue) :-
    map_difficulty(DifficultyRed, LevelRed),
    map_difficulty(DifficultyBlue, LevelBlue),
    !.
map_difficulty(Difficulty, Level) :-
    map_single_difficulty(Difficulty, Level),
    !.

map_single_difficulty(easy, 1).
map_single_difficulty(medium, 2).
map_single_difficulty(hard, 3).
map_single_difficulty(empty, 0).




/*
* Chooses the move for the player based on the current game state:
* choose_move(+GameState, -Move)
* GameState: The current state of the game.
* Move: The move selected for the current player, or 'skip' if no valid moves are available.
*/

% No valid moves available, skip the player s turn
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), skip) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), []),
    format('Player ~w has no valid moves. Skipping turn./n', [Player]),
    !.

% Red is human player
choose_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level, DiagonalRule), Move) :-
    repeat,  
    get_move(red, Move),  % Asks the human player for a move.
    valid_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level, DiagonalRule), Move),  % Checks if the move is valid.
    !.  % Interrompe a repetição se o movimento for válido

% Blue is human player
choose_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level, DiagonalRule), Move) :-
    repeat,  
    get_move(blue, Move),  % Asks the human player for a move.
    valid_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level, DiagonalRule), Move),  % Checks if the move is valid.
    !.  % Interrompe a repetição se o movimento for válido

% Blue is bot, red is human (Level 1 bot)
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1, DiagonalRule), Moves), % Get valid moves for the bot.
    random_member(Move, Moves),  % Selects a random valid move for the bot.
    display_bot_move(Move, Player), !.

% Red is bot, blue is human (Level 1 bot)
choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 1, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1, DiagonalRule), Moves), % Get valid moves for the bot.
    random_member(Move, Moves), % Selects a random valid move for the bot.
    display_bot_move(Move, Player), !.

% Blue is bot level 2, red is human
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 2, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 2, DiagonalRule), Moves), % Get valid moves for the bot.
    bot_move(gameState(BoardSize, Board, Player, h_pc, human, bot, 2, DiagonalRule), Move),  % Chooses the best move for the bot using a greedy strategy.
    display_bot_move(Move, Player),  
    !.  

% Red is bot level 2, blue is human
choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 2, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 2, DiagonalRule), Moves), % Get valid moves for the bot.
    bot_move(gameState(BoardSize, Board, Player, h_pc, bot, human, 2, DiagonalRule), Move),  % Chooses the best move for the bot using a greedy strategy.
    display_bot_move(Move, Player),  
    !.  

% Bot vs Bot (Level 1)
choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 1-LevelBlue, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 1-LevelBlue, DiagonalRule), Moves), % Get valid moves for the bot.
    random_member(Move, Moves),  % Selects a random valid move for the red bot.
    display_bot_move(Move, red),  
    !.  

choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-1, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-1, DiagonalRule), Moves), % Get valid moves for the bot.
    random_member(Move, Moves),  % Selects a random valid move for the blue bot.
    display_bot_move(Move, blue),  
    !.  

% Bot vs Bot (Level 2)
choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue, DiagonalRule), Moves), % Get valid moves for the bot.
    bot_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue, DiagonalRule), Move), % Chooses the best move for the bot using a greedy strategy.
    display_bot_move(Move, red),  
    !.  
% 
choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2, DiagonalRule), Moves), % Get valid moves for the bot.
    bot_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2, DiagonalRule), Move),  % Chooses the best move for the bot using a greedy strategy.
    display_bot_move(Move, blue),  
    !.  

% Bot vs Bot (Level 3)
choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 3-LevelBlue, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 3-LevelBlue, DiagonalRule), Moves), % Get valid moves for the bot.
    calculate_depth(Moves,Depth), % Calculates the depth based on moves length for minimax.
    minimax(gameState(BoardSize, Board, red, pc_pc, bot, bot, 3-LevelBlue, DiagonalRule),Depth,red,BestValue,Move), % Chooses the best move for the red bot using minimax strategy.
    display_bot_move(Move, red),  
    !.  


choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-3, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-3, DiagonalRule), Moves), % Get valid moves for the bot.
    calculate_depth(Moves,Depth), % Calculates the depth based on moves length for minimax.
    minimax(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-3, DiagonalRule),Depth,blue,BestValue,Move), % Chooses the best move for the blue bot using minimax strategy.
    display_bot_move(Move, blue),  % Exibe o movimento do bot
    !.  


% Blue is bot level 3, red is human
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 3, DiagonalRule), Move) :-
    repeat,  
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 3, DiagonalRule), Moves), % Get valid moves for the bot.
    calculate_depth(Moves,Depth), % Calculates the depth based on moves length for minimax.
    minimax(gameState(BoardSize, Board, blue, h_pc, human, bot, 3, DiagonalRule),Depth,blue,BestValue,Move), % Chooses the best move for the blue bot using minimax strategy.
    display_bot_move(Move, Player),  % Exibe o movimento do bot
    !.  

% Red is bot level 3, blue is human
choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 3, DiagonalRule), Move) :-
    repeat,   
    valid_moves(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 3, DiagonalRule), Moves), % Get valid moves for the bot.
    calculate_depth(Moves,Depth), % Calculates the depth based on moves length for minimax.
    minimax(gameState(BoardSize, Board, red, pc_h, human, bot, 3, DiagonalRule),Depth,red,BestValue,Move), % Chooses the best move for the red bot using minimax strategy.
    display_bot_move(Move, Player),  % Exibe o movimento do bot
    !.  

/*
* Adjusts the search depth based on the number of valid moves:
* calculate_depth(+ValidMoves, -Depth)
* ValidMoves: A list of all valid moves for the current player.
* Depth: The depth level for decision-making algorithms, determined dynamically.
* Strategy: This predicate calculates the depth level based on the number of valid moves.
* Fewer valid moves lead to deeper search levels to ensure better decision-making in critical scenarios.
*/
% Função que ajusta a profundidade de acordo com o número de movimentos válidos
calculate_depth(ValidMoves, Depth) :-
    length(ValidMoves, NumMoves),
    depth_for_moves(NumMoves, Depth).

% Mapeamento direto de movimentos válidos para profundidade
depth_for_moves(NumMoves, Depth) :-
    NumMoves >= 30, Depth = 2.
depth_for_moves(NumMoves, Depth) :-
    NumMoves >= 10, Depth = 3.
depth_for_moves(NumMoves, Depth) :-
    NumMoves >= 5, Depth = 4.
depth_for_moves(NumMoves, Depth) :-
    NumMoves < 5, Depth = 6.

/*
* Changes the current player and sets the new game state:
* move(+GameState, +Move, -NewGameState)
* GameState: The current state of the game.
* Move: The move to be made by the player, can be 'skip' or a piece move.
* NewGameState: The new state of the game after the move has been made.
*/

move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), skip, 
    gameState(BoardSize, Board, NewPlayer, GameType, RedType, BlueType, Level, DiagonalRule)) :-
    next_player(Player, NewPlayer).  % Switch to the next player

/*
* Moves a piece on the board and updates the game state accordingly:
* move(+GameState, +Move, -NewGameState)
* GameState: The current state of the game.
* Move: The move made by the player, consisting of starting and ending positions.
* NewGameState: The updated game state after the move.
*/

move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), 
     (Row-Col, ToRow-ToCol), 
     gameState(BoardSize, NewBoard, NewPlayer, GameType, RedType, BlueType, Level, UpdatedDiagonalRule)) :-
    
    % Switch the current player after the move
    next_player(Player, NewPlayer),

    % Get the current player s diagonal move permission
    get_player_diagonal_permission(Player, DiagonalRule, CanMoveDiagonally),

    % Validate the direction of the move
    valid_move_in_direction(Row, Col, ToRow, ToCol, CanMoveDiagonally),

    % Get the piece to be moved from the board
    nth0(Row, Board, OldRow),
    nth0(Col, OldRow, Piece-Size),

    % Get the target piece and its size from the destination cell
    nth0(ToRow, Board, ToOldRow),
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize), !,

    verifyEmpty(Board, Row-Col, ToRow-ToCol, TargetPiece), !,

    % Check if the move is valid based on piece type and size
    valid_move(Piece, Size, TargetPiece, TargetSize, Player),

    % Move the piece on the board
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard),

    % Update diagonal move rule if necessary
    update_diagonal_rule(Player, Row, Col, ToRow, ToCol, DiagonalRule, UpdatedDiagonalRule).


/*
* Checks whether the move is valid based on piece type, target piece, and size:
* valid_move(+Piece, +Size, +TargetPiece, +TargetSize, +Player)
* Piece: The piece the player is trying to move.
* Size: The size of the piece the player is trying to move.
* TargetPiece: The piece in the target cell, can be 'empty' or an opponent piece.
* TargetSize: The size of the target piece.
* Player: The current player making the move.
*/
valid_move(Piece, Size, empty, _, Player) :-
    Piece == Player, !.  % The piece must belong to the current player

valid_move(Piece, Size, TargetPiece, TargetSize, Player) :-
    Piece == Player,  % The piece must belong to the current player
    valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player).  % Check movement conditions

% Validates the movement conditions based on the player and the piece sizes:
valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player) :-
    (Player == blue, TargetPiece == red, Size >= TargetSize);  % Blue can capture Red
    (Player == blue, TargetPiece == blue, Size =< TargetSize);  % Blue can move to another blue cell
    (Player == red, TargetPiece == blue, Size >= TargetSize);  % Red can capture Blue
    (Player == red, TargetPiece == red, Size =< TargetSize).   % Red can move to another red cell

/*
* Moves the piece from one position to another on the board:
* move_piece(+Board, +Row, +Col, +ToRow, +ToCol, +Piece, +Size, +TargetPiece, +TargetSize, -NewBoard)
* Board: The current game board.
* Row, Col: The current position of the piece to be moved.
* ToRow, ToCol: The destination position for the piece.
* Piece: The piece to be moved.
* Size: The size of the piece to be moved.
* TargetPiece: The piece in the target cell.
* TargetSize: The size of the target piece.
* NewBoard: The updated game board after the move.
*/
move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard) :-
    % Update the size of the piece if necessary (based on capturing an opponents piece)
    update_size(Size, TargetPiece, TargetSize, NewSize),

    % Remove the piece from the origin cell (set it to 'empty')
    replace(Board, Row, Col, empty, TempBoard),

    % Place the piece in the new position with the new size
    replace(TempBoard, ToRow, ToCol, Piece-NewSize, NewBoard).

% Updates the size of the piece after a move, considering whether the destination is empty or has an opponents piece:
update_size(Size, empty, NewSize) :-
    NewSize = Size.  % If the destination is empty, the size remains unchanged.

update_size(Size, TargetPiece, TargetSize, NewSize) :-
    Piece \== TargetPiece,  % If the destination contains an opponent piece
    NewSize is Size + TargetSize.  % The piece size increases by the size of the captured piece

update_size(Size, TargetPiece, TargetSize, NewSize) :-
    Piece == TargetPiece,  % If the destination contains a piece of the same player
    NewSize is Size + TargetSize.  % The piece size increases by the size of the same player piece

% Prompts the player to choose a move and adjusts the indices to match the board representation:
get_move(Player, NewMove) :-
    write(Player),
    write(', choose your move (ColI-RowI,ColF-RowF): '), nl,
    catch(read(Move), _, (write('Invalid input. Please try again.'), nl, fail)),
    move_minus_1(Move, NewMove),
    !.

/*
* Verifies if a move is valid by attempting to execute it:
* valid_move(+GameState, +Move)
* GameState: The current state of the game.
* Move: The move the player wants to make.
*/
valid_move(GameState, Move) :-
    move(GameState, Move, _),  % Tries to make the move and check if its valid
    !.  % If the move is valid, the execution stops and succeeds.

valid_move(_) :-
    write('Invalid move. Please try again.'), nl,  % If the move is invalid, print an error message
    fail.  % Forces the repeat of the move due to invalidity.


/*
* Returns all valid moves for the given player in the current game state:
* valid_moves(+GameState, -Moves)
* GameState: The current state of the game.
* Moves: A list of valid moves for the current player.
*/
valid_moves(gameState(BoardSize, Board, Player, _, _, _, _, _), Moves) :-
    Limit is BoardSize - 1,  % The limit for row and column index is one less than the board size.
    
    % Find all possible moves
    findall(
        (Row-Col, ToRow-ToCol),
        (
            between(0, Limit, Row),  % Iterate through all rows
            between(0, Limit, Col),  % Iterate through all columns
            get_cell(Row, Col, Board, Piece-Size),  % Get the piece and its size from the cell
            Piece == Player,  % The piece must belong to the current player
            adjacent_position(BoardSize, Row, Col, ToRow, ToCol),  % Check if the destination is adjacent
            nth0(ToRow, Board, ToOldRow),  % Get the row of the destination cell
            get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),  % Get the target piece and size
            valid_move(Piece, Size, TargetPiece, TargetSize, Player),  % Check if the move is valid
            verifyEmpty(Board, Row-Col, ToRow-ToCol, TargetPiece)  % Ensure the move is to a valid empty cell or opponent piece
        ),
        AllMoves
    ),
    
    % Sort and return the valid moves
    sort(AllMoves, Moves).

/*
* Checks if a position is adjacent to another within the board's size:
* adjacent_position(+BoardSize, +Row, +Col, +ToRow, +ToCol)
* BoardSize: The size of the board (defines the board's boundaries).
* Row, Col: The current position of the piece.
* ToRow, ToCol: The target position to which the piece is moving.
*/
adjacent_position(BoardSize, Row, Col, ToRow, ToCol) :-
    member((DRow, DCol), [(0, 1), (1, 0), (0, -1), (-1, 0)]),  % Consider the four possible directions (up, down, left, right)
    ToRow is Row + DRow,  % Calculate the new row by adding the delta row
    ToCol is Col + DCol,  % Calculate the new column by adding the delta column
    ToRow >= 0, ToRow < BoardSize,  % Ensure the new row is within the board boundaries
    ToCol >= 0, ToCol < BoardSize.  % Ensure the new column is within the board boundaries



/*
* Generates and evaluates all valid moves for the bot to make the best decision:
* bot_move(+GameState, -BestMove)
* GameState: The current state of the game.
* BestMove: The best move selected by the bot, based on simulation and evaluation.
*/
bot_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), 
         (BestRow-BestCol, BestToRow-BestToCol)) :-
    
    % Generate all valid moves for the current player
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), ValidMoves),
    
    % Evaluate all valid moves and associate each with a score
    findall((Score, (Row-Col, ToRow-ToCol)), 
        (
            member((Row-Col, ToRow-ToCol), ValidMoves),  % For each valid move
            simulate_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), 
                          (Row-Col, ToRow-ToCol), NewGameState),  % Simulate the move
            value(NewGameState, Player, Value),  % Evaluate the new game state with the value function
            evaluate_move(BoardSize, Board, Player, (Row-Col, ToRow-ToCol), MoveScore),  % Evaluate the move score
            Score is Value + MoveScore  % Combine the value and move score to get the total score
        ),
        EvaluatedMoves),
    
    % Get the maximum score from the evaluated moves
    findall(Score, member((Score, _), EvaluatedMoves), Scores),
    my_max_list(Scores, MaxScore),  % Find the move with the highest score
    
    % Find all moves that have the maximum score
    findall((MaxScore, (Row-Col, ToRow-ToCol)),
        member((MaxScore, (Row-Col, ToRow-ToCol)), EvaluatedMoves),
        BestMoves),
    
    % Choose a random best move from the best-scoring moves
    random_member((_, (BestRow-BestCol, BestToRow-BestToCol)), BestMoves).


/*
* Evaluate a specific move:
* evaluate_move(+BoardSize, +Board, +Player, +(Row-Col, ToRow-ToCol), -Score)
* BoardSize: The size of the board.
* Board: The current state of the board.
* Player: The current player.
* (Row-Col, ToRow-ToCol): The move to be evaluated, defined by the source and destination coordinates.
* Score: The score assigned to the move after evaluation.
*/
evaluate_move(BoardSize, Board, Player, (Row-Col, ToRow-ToCol), Score) :-
    
    % Get the piece and size from the source cell
    get_cell(Row, Col, Board, Piece-Size),
    
    % Get the piece and size from the destination cell
    nth0(ToRow, Board, ToOldRow),
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
    
    % Check and calculate stacking move score
    stacking_move(Piece, Player, TargetPiece, TargetSize, Size, StackingScore),
    
    % If not stacking, check capture move score
    capture_move(Piece, Player, TargetPiece, TargetSize, Size, CaptureScore),
    
    % If not stacking or capturing, check positional move score
    positional_move(Piece, Player, TargetPiece, TargetSize, PositionalScore),
    
    % The final score will be the highest among the generated scores
    my_max_list([StackingScore, CaptureScore, PositionalScore], Score).

% Stacking move predicate (same color)
stacking_move(Piece, Player, TargetPiece, TargetSize, Size, Score) :-
    Piece == Player,  % If the source piece belongs to the player
    TargetPiece == Player,  % If the target piece also belongs to the player
    Score is Size + TargetSize.  % Add the size of the stack at the destination

stacking_move(_, _, _, _, _, Score) :-
    Score = 0.  % Otherwise, the move is not a stacking move

% Capture move predicate (opponent s piece)
capture_move(Piece, Player, TargetPiece, TargetSize, Size, Score) :-
    Piece == Player,  % Source piece belongs to the player
    TargetPiece \= Player,  % Target piece belongs to the opponent
    TargetPiece \= empty,  % Target cell is not empty
    Score is Size + TargetSize.  % Sum the size of the opponent s stack

capture_move(_, _, _, _, _, Score) :-
    Score = 0.  % Not a capture move

% Positional move predicate (neither stacking nor capture)
positional_move(Piece, Player, TargetPiece, TargetSize, Score) :-
    Piece == Player,  % Source piece belongs to the player
    (TargetPiece == empty; TargetPiece == Player),  % Target cell is empty or has player s piece
    Score = 0.  % No score for positional moves

positional_move(_, _, _, _, Score) :-
    Score = 0.  % Not a positional move

% Function to get the players permission for diagonal movement
get_player_diagonal_permission(red, [1, _], 1).  % Red player can move diagonally if in the first column
get_player_diagonal_permission(blue, [_, 1], 1). % Blue player can move diagonally if in the first row
get_player_diagonal_permission(_, _, 0).          % Otherwise, diagonal movement is restricted

% Validates diagonal moves if the DiagonalRule allows (CanMoveDiagonally = 1)
valid_move_in_direction(Row, Col, ToRow, ToCol, 1) :-
    isDiagonal(Row, Col, ToRow, ToCol),
    valid_diagonal_move(Row, Col, ToRow, ToCol).

% Validates horizontal or vertical moves if the DiagonalRule does not allow (CanMoveDiagonally = 0)
valid_move_in_direction(Row, Col, ToRow, ToCol, 0) :-
    valid_direction(Row, Col, ToRow, ToCol).


% Check if a move is diagonal
isDiagonal(Row, Col, ToRow, ToCol) :-
    abs(Row - ToRow) =:= abs(Col - ToCol).

% Validate a diagonal move
valid_diagonal_move(Row, Col, ToRow, ToCol) :-
    abs(Row - ToRow) =:= 1,  % Distance is 1 in both directions
    abs(Col - ToCol) =:= 1.

% Validate horizontal or vertical moves
valid_direction(Row, Col, ToRow, Col) :-
    abs(Row - ToRow) =:= 1.  % Row changes by 1

valid_direction(Row, Col, Row, ToCol) :-
    abs(Col - ToCol) =:= 1.  % Column changes by 1

valid_direction(_, _, _, _) :- fail.  % Invalid move

% Update diagonal rule based on the last move
update_diagonal_rule(_, Row, Col, ToRow, ToCol, DiagonalRule, DiagonalRule) :-
    \+ isDiagonal(Row, Col, ToRow, ToCol),  % If not diagonal, rule unchanged
    !.

update_diagonal_rule(red, _, _, _, _, [_, BlueRule], [0, BlueRule]). % Red moved diagonally
update_diagonal_rule(blue, _, _, _, _, [RedRule, _], [RedRule, 0]). % Blue moved diagonally

/*
* Checks if the game is over:
* game_over(+GameState, -Winner)
* GameState: The current game state.
* Winner: The player who won the game.
*/
game_over(gameState(BoardSize, Board, _, _, _, _, _, _), Winner) :-
    flatten(Board, FlatList),  % Flatten the list of lists into a single list
    exclude(=(empty), FlatList, Pieces),  % Remove all 'empty' positions
    same_color(Pieces, Winner).

% Determine the owner of the largest piece
max_piece_owner([Color1-Size1, Color2-Size2], Winner) :-
    Size1 > Size2,  % If piece 1 is larger than piece 2
    Winner = Color1.  % Winner is the owner of piece 1

max_piece_owner([Color1-Size1, Color2-Size2], Winner) :-
    Size2 > Size1,  % If piece 2 is larger than piece 1
    Winner = Color2.  % Winner is the owner of piece 2

% Congratulate the winner
congratulate(Winner) :-
    nl,
    format('~`=t~40|~n', []),  % Separation line
    format('   Congratulations, ~w!~n', [Winner]),  % Display winner message
    format('   You are the winner!~n', []),  % Display winner message
    format('~`=t~40|~n', []),  % Separation line
    halt.  % End the game


/*
* Simulates a move and updates the game state:
* simulate_move(+GameState, +Move, -NewGameState)
* GameState: The current state of the game.
* Move: The move to be simulated, defined by source and destination coordinates.
* NewGameState: The updated game state after the move is applied.
*/
simulate_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule),
              (Row-Col, ToRow-ToCol),
              gameState(BoardSize, NewBoard, Player, GameType, RedType, BlueType, Level, DiagonalRule)) :-
    
    % Get the piece and size from the source cell
    get_cell(Row, Col, Board, Piece-Size),

    % Get the destination row from the board
    nth0(ToRow, Board, ToOldRow),        

    % Get the piece and size from the destination cell (for capture or stacking)
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
    
    % Move the piece and update the board
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard).

/*
* Evaluates the game state for the player (blue or red):
* value(+GameState, +Player, -Value)
* GameState: The current state of the game.
* Player: The player whose perspective the value is evaluated from (blue or red).
* Value: The evaluation score, based on the difference in piece counts between the player and opponent.
*/
value(gameState(BoardSize, Board, Player, _, _, _, _, _), Player, Value) :-
    % Get the opponent of the current player
    next_player(Player, Opponent),
    
    % Count the number of pieces for the player
    count_pieces(Board, Player, PlayerCount),
    
    % Count the number of pieces for the opponent
    count_pieces(Board, Opponent, OpponentCount),
    
    % The value is the difference in piece counts between the player and the opponent
    Value is PlayerCount - OpponentCount.

% Count the number of pieces for a player on the board
count_pieces(Board, Player, TotalSize) :-
    findall(Size, 
            (member(Row, Board),               % For each row on the board
             member(Player-Size, Row)),        % If the piece belongs to the player
            Sizes),                            % Collect the sizes of the pieces found
    sumlist(Sizes, TotalSize).              % Sum the sizes to get the total

% Define a test board and calculate the value for each player
test_value :-
    Board = [
        [blue-1, red-2, empty],
        [red-1, blue-2, empty],
        [empty, blue-2, red-1]
    ],
    GameState = gameState(3, Board, blue, _, _, _, _, _),  % Game state for 'blue' player
    value(GameState, blue, Value),  % Calculate the value for 'blue' player
    format('Value for blue player: ~w~n', [Value]),  % Display the value
    
    GameStateRed = gameState(3, Board, red, _, _, _, _, _),  % Game state for 'red' player
    value(GameStateRed, red, ValueRed),  % Calculate the value for 'red' player
    format('Value for red player: ~w~n', [ValueRed]).  % Display the value

% Test the count_pieces predicate
test_count_pieces :-
    Board = [
        [blue-1, red-2, empty],
        [red-1, blue-2, empty],
        [empty, blue-2, red-1]
    ],
    
    % Count the pieces for 'blue' player
    count_pieces(Board, blue, BlueTotal),
    format('Total pieces for blue: ~w~n', [BlueTotal]),

    % Count the pieces for 'red' player
    count_pieces(Board, red, RedTotal),
    format('Total pieces for red: ~w~n', [RedTotal]).


/*
* Implements the minimax algorithm to evaluate the best possible move:
* minimax(+GameState, +Depth, +Player, -Score, -BestMove)
* GameState: The current state of the game.
* Depth: The maximum depth for searching the game tree.
* Player: The player making the move.
* Score: The score of the best move found.
* BestMove: The best move to make, based on minimax evaluation.
*/
minimax(GameState, Depth, Player, Score, _) :-
    % Base case: check if the game is over or if the maximum depth has been reached
    (game_over(GameState, _); Depth == 0),
    minimax_value(GameState, Score),  % Return the score of the current game state
    !.

minimax(GameState, Depth, Player, BestScore, BestMove) :-
    % Extract relevant components from the game state
    GameState = gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), 
    
    % Ensure the depth is greater than 0 for recursive exploration
    Depth > 0,
    
    % Get all valid moves for the current player
    valid_moves(GameState, ValidMoves),
    ValidMoves \= [],  % Ensure there are valid moves available
    
    % Decrease the depth for the recursive call
    NewDepth is Depth - 1,
    
    % Determine the opponent s player for the next move
    next_player(Player, Opponent),
    
    % Evaluate all possible moves for the current player
    findall((TotalScore, Move),
        (
            member(Move, ValidMoves),  % Iterate over each valid move
            simulate_move_minimax(GameState, Move, NewGameState),  % Simulate the move
            minimax(NewGameState, NewDepth, Opponent, Score, _),  % Recursively call minimax for the opponent
            evaluate_move(BoardSize, Board, Player, Move, MoveScore),  % Evaluate the move using a custom evaluation function
            TotalScore is Score + MoveScore  % Combine the score and the move evaluation
        ),
        ScoresMoves),
    
    % Select the best move based on the current player s perspective
    select_best(Player, ScoresMoves, BestScore-BestMove).  % Choose the best move for the player


% Selects the best move for the 'red' player by maximizing the score.
select_best(red, ScoresMoves, BestScore-BestMove) :-
    select_max(ScoresMoves, BestScore-BestMove).

% Selects the best move for the 'blue' player by minimizing the score.
select_best(blue, ScoresMoves, BestScore-BestMove) :-
    select_min(ScoresMoves, BestScore-BestMove).

% Finds the move with the maximum score from the list.
select_max(ScoresMoves, BestScore-BestMove) :-
    findall(Score, member((Score, _), ScoresMoves), Scores),
    my_max_list(Scores, BestScore),
    findall(Move, (member((BestScore, Move), ScoresMoves)), BestMoves),
    random_member(BestMove, BestMoves).

% Finds the move with the minimum score from the list.
select_min(ScoresMoves, BestScore-BestMove) :-
    findall(Score, member((Score, _), ScoresMoves), Scores),
    my_min_list(Scores, BestScore),
    findall(Move, (member((BestScore, Move), ScoresMoves)), BestMoves),
    random_member(BestMove, BestMoves).

% Evaluates the current game state by calculating the difference in pieces for each player.
minimax_value(gameState(BoardSize, Board, Player, _, _, _, _, _), Value) :-
    count_pieces(Board, red, RedCount),
    count_pieces(Board, blue, BlueCount),
    Value is RedCount - BlueCount.

% Simulates a move by updating the game state.
simulate_move_minimax(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule),
               (Row-Col, ToRow-ToCol),
               gameState(BoardSize, NewBoard, NextPlayer, GameType, RedType, BlueType, Level, DiagonalRule)) :-
    get_cell(Row, Col, Board, Piece-Size),
    nth0(ToRow, Board, ToOldRow),
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard),
    next_player(Player, NextPlayer).

% Checks if a move is valid based on the position and the stacks on the board.
valid_positional_move(Board, Row-Col, ToRow-ToCol) :-
    findall(PieceRow-PieceCol,
            (nth0(PieceRow, Board, RowList),
             nth0(PieceCol, RowList, _-Size),
             (PieceRow \= Row ; PieceCol \= Col)),
            AllStacks),
    valid_positional_movement(ToRow-ToCol, Row-Col, AllStacks).

% Checks if moving to the new position reduces the Manhattan distance to the nearest stack.
valid_positional_movement(ToRow-ToCol, FromRow-FromCol, Stacks) :-
    findall(Distance,
            (member(ClosestRow-ClosestCol, Stacks),
             manhattan_distance(FromRow-FromCol, ClosestRow-ClosestCol, Distance)),
            Distances),
    my_min_list(Distances, CurrentDistance),
    findall(NewDistance,
            (member(ClosestRow-ClosestCol, Stacks),
             manhattan_distance(ToRow-ToCol, ClosestRow-ClosestCol, NewDistance)),
            NewDistances),
    my_min_list(NewDistances, NewDistance),
    NewDistance < CurrentDistance.



final_state:-
    display_game(gameState(4,[[empty, empty, empty, empty],[empty, blue-3, empty, empty],[empty, empty, empty, empty],[empty, red-10, empty, empty]], red, h_h, human, human, 0, [0,0])),
    game_cycle(gameState(4,[[empty, empty, empty, empty],[empty, blue-3, empty, empty],[empty, empty, empty, empty],[empty, red-10, empty, empty]], red, h_h, human, human, 0, [0,0])).