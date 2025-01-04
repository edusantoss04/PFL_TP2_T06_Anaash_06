:- use_module(library(system), [now/1]).
:- use_module(library(lists)).
:- use_module(library(random)).
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



move(gameState(BoardSize,Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), skip, gameState(BoardSize,Board, NewPlayer, GameType, RedType, BlueType, Level, DiagonalRule)) :-
    next_player(Player, NewPlayer).

move(gameState(BoardSize,Board, Player, GameType, RedType, BlueType, Level,DiagonalRule), (Row-Col,ToRow-ToCol), gameState(BoardSize,NewBoard, NewPlayer, GameType, RedType, BlueType, Level,UpdatedDiagonalRule)) :-
    % Trocar o jogador
    next_player(Player, NewPlayer),

    % Obter o valor de DiagonalRule correspondente ao jogador atual
    get_player_diagonal_permission(Player, DiagonalRule, CanMoveDiagonally),

    % Validar a direção
    % valid_direction(Row, Col, ToRow, ToCol),
    valid_move_in_direction(Row, Col, ToRow, ToCol, CanMoveDiagonally),

    % Obter a peça a mover
    nth0(Row, Board, OldRow),
    nth0(Col, OldRow, Piece-Size),

    % Obter o conteúdo da célula de destino
    nth0(ToRow, Board, ToOldRow),
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize), !,

    % Verificar se o movimento é válido
    valid_move(Piece, Size, TargetPiece, TargetSize, Player),

    % Mover a peça
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard),

    % Atualizar o DiagonalRule, se necessário
    update_diagonal_rule(Player, Row, Col, ToRow, ToCol, DiagonalRule, UpdatedDiagonalRule).


% Verificar se o movimento é válido
valid_move(Piece, Size, empty, _, Player) :-
    Piece == Player, !.  % A peça deve pertencer ao jogador
valid_move(Piece, Size, TargetPiece, TargetSize, Player) :-
    Piece == Player,  % A peça pertence ao jogador
    valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player).

% Condições de movimento
valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player) :-
    (Player == blue, TargetPiece == red, Size >= TargetSize);  % Blue pode capturar Red
    (Player == blue, TargetPiece == blue, Size =< TargetSize);  % Blue pode mover para outra célula azul
    (Player == red, TargetPiece == blue, Size >= TargetSize);  % Red pode capturar Blue
    (Player == red, TargetPiece == red, Size =< TargetSize).   % Red pode mover para outra célula vermelha

% Mover a peça no tabuleiro
move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard) :-
    % Atualizar o tamanho usando a função update_size
    update_size(Size, TargetPiece, TargetSize, NewSize),

    % Atualizar a célula de origem (deve ser 'empty' após o movimento)
    replace(Board, Row, Col, empty, TempBoard),

    % Colocar a peça na nova posição com o novo tamanho
    replace(TempBoard, ToRow, ToCol, Piece-NewSize, NewBoard).

% Atualizar o tamanho da peça
update_size(Size, empty, NewSize) :-
    NewSize = Size.  % Se a célula de destino for vazia, não altera o tamanho.

update_size(Size, TargetPiece, TargetSize, NewSize) :-
    Piece \== TargetPiece,  % Se for uma peça adversária
    NewSize is Size + TargetSize.

update_size(Size, TargetPiece, TargetSize, NewSize) :-
    Piece == TargetPiece,  % Se for uma peça do mesmo jogador
    NewSize is Size + TargetSize.


get_move(Player, NewMove) :-
    write(Player),
    write(', choose your move (ColI-RowI,ColF-RowF): '), nl,
    catch(read(Move), _, (write('Invalid input. Please try again.'), nl, fail)),
    move_minus_1(Move, NewMove),
    !.

% Função para verificar se o movimento é válido
valid_move(GameState, Move) :-
    move(GameState, Move, _),  % Tenta realizar o movimento
    !.  % Se o movimento for válido, interrompe o repeat
valid_move(_) :-
    write('Invalid move. Please try again.'), nl,
    fail.  % Se o movimento for inválido, força a repetição


% Retorna todos os movimentos válidos para um dado jogador
valid_moves(gameState(BoardSize, Board, Player, _, _, _, _,_), Moves) :-
    Limit is BoardSize - 1,
    findall(
        (Row-Col, ToRow-ToCol),
        (
            between(0, Limit, Row),
            between(0, Limit, Col),
            get_cell(Row, Col, Board, Piece-Size),
            Piece == Player,
            adjacent_position(BoardSize, Row, Col, ToRow, ToCol),
            nth0(ToRow, Board, ToOldRow),
            get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
            valid_move(Piece, Size, TargetPiece, TargetSize, Player)
        ),
        AllMoves
    ),
    sort(AllMoves, Moves).

% Retorna as posições adjacentes a uma célula dentro dos limites do tabuleiro
adjacent_position(BoardSize, Row, Col, ToRow, ToCol) :-
    member((DRow, DCol), [(0, 1), (1, 0), (0, -1), (-1, 0)]),
    ToRow is Row + DRow,
    ToCol is Col + DCol,
    ToRow >= 0, ToRow < BoardSize,
    ToCol >= 0, ToCol < BoardSize.

% valid_moves(gameState(2, [[empty, blue-1],[red-5, empty]],blue,_,_,_,_), Moves).


% Função para gerar e avaliar os movimentos válidos do bot.
bot_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), (BestRow-BestCol, BestToRow-BestToCol)) :-
    % Gerar todos os movimentos válidos
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), ValidMoves),
    
    % Avaliar todos os movimentos e associar a pontuação
    findall((Score, (Row-Col, ToRow-ToCol)), 
        (member((Row-Col, ToRow-ToCol), ValidMoves), 
         simulate_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), (Row-Col, ToRow-ToCol),NewGameState), % Simula o movimento
         value(NewGameState, Player, Value),  % Avalia o novo estado do jogo com a função value
         evaluate_move(BoardSize, Board, Player, (Row-Col, ToRow-ToCol), MoveScore),  % Avalia o movimento
         Score is Value + MoveScore),  % Combina o valor do estado do jogo com a pontuação do movimento
        EvaluatedMoves),
    
    
    findall(Score, member((Score, _), EvaluatedMoves), Scores),
    my_max_list(Scores, MaxScore),
    
   
    findall((MaxScore, (Row-Col, ToRow-ToCol)),
        member((MaxScore, (Row-Col, ToRow-ToCol)), EvaluatedMoves),
        BestMoves),
    
    % Escolher um movimento aleatório entre os melhores
    random_member((_, (BestRow-BestCol, BestToRow-BestToCol)), BestMoves).


% Avaliar um movimento específico
evaluate_move(BoardSize, Board, Player, (Row-Col, ToRow-ToCol), Score) :-
    % Obter a peça e o tamanho na célula de origem
    get_cell(Row, Col, Board, Piece-Size),
    
    % Obter a peça e o tamanho na célula de destino
    nth0(ToRow, Board, ToOldRow),
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
    
    % Verifica e calcula a pontuação de empilhamento
    stacking_move(Piece, Player, TargetPiece, TargetSize, Size, StackingScore),
    
    % Se não for empilhamento, verifica a pontuação de captura
    capture_move(Piece, Player, TargetPiece, TargetSize, Size, CaptureScore),
    
    % Se não for empilhamento nem captura, verifica a pontuação posicional
    positional_move(Piece, Player, TargetPiece, TargetSize, PositionalScore),
    
    % A pontuação final será a maior entre as pontuações geradas
    my_max_list([StackingScore, CaptureScore, PositionalScore], Score).

% Predicado de movimento de empilhamento (mesma cor)
stacking_move(Piece, Player, TargetPiece, TargetSize, Size, Score) :-
    Piece == Player,  % Se a peça de origem é do jogador
    TargetPiece == Player,  % Se a peça de destino também é do jogador
    Score is Size + TargetSize.  % Soma o tamanho da pilha no destino

stacking_move(_, _, _, _, _, Score) :-
    Score = 0.  % Caso contrário, o movimento não é de empilhamento

% Predicado de movimento de captura (oponente)
capture_move(Piece, Player, TargetPiece, TargetSize, Size, Score) :-
    Piece == Player,  % Se a peça de origem é do jogador
    TargetPiece \= Player,  % Se a peça de destino é do adversário
    TargetPiece \= empty,  % A peça de destino não pode ser vazia
    Score is Size + TargetSize.  % Soma o tamanho da pilha do adversário

capture_move(_, _, _, _, _, Score) :-
    Score = 0.  % Caso contrário, o movimento não é uma captura

% Predicado para movimentos posicionais (sem empilhamento nem captura)
positional_move(Piece, Player, TargetPiece, TargetSize, Score) :-
    Piece == Player,  % Se a peça de origem é do jogador
    (TargetPiece == empty; TargetPiece == Player),  % Se a célula de destino está vazia ou tem peça do jogador
    Score = 0.  % Não há pontuação adicional para movimentos posicionais

positional_move(_, _, _, _, Score) :-
    Score = 0. 

% Teste de execução
% bot_move(gameState(6, [[blue-2, red-1, blue-1], [red-2, blue-1, red-1], [blue-1, red-1, blue-1]], blue, _, _, _, _), Move).

% Função para obter a permissão de movimento diagonal do jogador
get_player_diagonal_permission(red, [1, _], 1).
get_player_diagonal_permission(blue, [_, 1], 1).
get_player_diagonal_permission(_, _, 0).  % Caso contrário, o movimento diagonal é restrito

% Valida movimentos diagonais se o DiagonalRule permitir (CanMoveDiagonally = 1)
valid_move_in_direction(Row, Col, ToRow, ToCol, 1) :-
    isDiagonal(Row, Col, ToRow, ToCol),
    valid_diagonal_move(Row, Col, ToRow, ToCol).

% Valida movimentos horizontais ou verticais se o DiagonalRule não permitir (CanMoveDiagonally = 0)
valid_move_in_direction(Row, Col, ToRow, ToCol, 0) :-
    valid_direction(Row, Col, ToRow, ToCol).

% Valida um movimento diagonal
isDiagonal(Row, Col, ToRow, ToCol) :-
    abs(Row - ToRow) =:= abs(Col - ToCol).

% Verifica a validade de um movimento diagonal
valid_diagonal_move(Row, Col, ToRow, ToCol) :-
    abs(Row - ToRow) =:= 1,  % A distância precisa ser 1 nas duas direções
    abs(Col - ToCol) =:= 1.

% Valida movimentos horizontais ou verticais
valid_direction(Row, Col, ToRow, Col) :-
    abs(Row - ToRow) =:= 1.

valid_direction(Row, Col, Row, ToCol) :-
    abs(Col - ToCol) =:= 1.

valid_direction(_, _, _, _) :- fail.  % Caso o movimento não seja válido

update_diagonal_rule(_, Row, Col, ToRow, ToCol, DiagonalRule, DiagonalRule) :-
    \+ isDiagonal(Row, Col, ToRow, ToCol), 
    % Se não for diagonal, regra permanece inalterada
    !.

update_diagonal_rule(red, _, _, _, _, [_, BlueRule], [0, BlueRule]). % Vermelho jogou diagonal.
update_diagonal_rule(blue, _, _, _, _, [RedRule, _], [RedRule, 0]). % Azul jogou diagonal.

game_over(gameState(BoardSize, Board, _, _, _, _, _, _), Winner) :-
    flatten(Board, FlatList),               % Achatar a lista de listas em uma lista única.
    exclude(=(empty), FlatList, Pieces),    % Remover todas as posições 'empty'.
    (
        same_color(Pieces, Winner)          % Caso todas as peças sejam da mesma cor.
    ;
        length(Pieces, 2),                  % Caso existam exatamente duas peças no tabuleiro.
        max_piece_owner(Pieces, Winner)    % Determina o dono da peça maior.
    ).

max_piece_owner([Color1-Size1, Color2-Size2], Winner) :-
    Size1 > Size2,  
    Winner = Color1. 
max_piece_owner([Color1-Size1, Color2-Size2], Winner) :-
    Size2 > Size1,  % Se a peça 2 é maior que a peça 1
    Winner = Color2. 


congratulate(Winner) :-
    nl,
    format('~`=t~40|~n', []),  % Linha de separação
    format('   Congratulations, ~w!~n', [Winner]),
    format('   You are the winner!~n', []),
    format('~`=t~40|~n', []),
    halt . % Linha de separação   

% para testar move(gameState(6,[[blue-2, blue-1], [red-1, blue-1]], blue, _, _, _, _), (0-0, 0-1), gameState(6,NewBoard, NewPlayer, _, _, _, _)).
% move(gameState(6, [[blue-2, blue-1], [red-1, blue-1]], blue, _, _, _, _, [1, 1]), (0-0, 0-1), gameState(6, NewBoard, NewPlayer, _, _, _, _, [1, 0])).
% gameState(board, player, GameType, RedType, BlueType, Level).
% gameConfig(GameType,SizeBoard, Dificulty).
% para testar display_game(gameState(6,[[blue-1, red-1, blue-1, red-1, blue-1, red-1],[red-1, blue-1, red-1, blue-1, red-1, blue-1],[blue-1, red-1, blue-1, red-1, blue-1, red-1],[red-1, blue-1, red-1, blue-1, red-1, blue-1],[blue-1, red-1, blue-1, red-1, blue-1, red-1],[red-1, blue-1, red-1, blue-1, red-1, blue-1]], _, _, _, _, _)).
% testar game_over(gameState(6,[[red-2, blue-1, red-1, red-1, red-1, red-1],[red-1, red-1, red-1, red-1, red-1, red-1]],_,_,_,_,_), Winner).


simulate_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule),
              (Row-Col, ToRow-ToCol),
              gameState(BoardSize, NewBoard, Player, GameType, RedType, BlueType, Level, DiagonalRule)) :-
    % Obter a peça e o tamanho na célula de origem
    get_cell(Row, Col, Board, Piece-Size),

    nth0(ToRow, Board, ToOldRow),        % Obter a linha do destino

    % Obter a peça e o tamanho na célula de destino (caso de captura ou empilhamento)
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
    
    % Mover a peça de acordo com o movimento e atualizar o tamanho
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard).

% simulate_move(gameState(2,[[red-2,blue-2],[red-4,blue-1]],blue,_,_,_,_,_),(0-1,0-0),true,gameState(BoardSize, NewBoard, Player, GameType, RedType, BlueType, Level, DiagonalRule)).
% Função que avalia o estado do jogo para o jogador (blue ou red)
value(gameState(BoardSize, Board, Player, _, _, _, _, _), Player, Value) :-
    next_player(Player,Opponent),
    count_pieces(Board, Player, PlayerCount),
    count_pieces(Board, Opponent, OpponentCount),
    
    Value is PlayerCount - OpponentCount.

% Contar o número de peças de um jogador no tabuleiro
count_pieces(Board, Player, TotalSize) :-
    findall(Size, 
            (member(Row, Board),               % Para cada linha no tabuleiro
             member(Player-Size, Row)),        % Se a peça na célula for do jogador Player
            Sizes),                            % Armazenar os tamanhos das peças encontradas
    sumlist(Sizes, TotalSize). 

% Definição do Tabuleiro para o teste
test_value :-
    Board = [
        [blue-1, red-2, empty],
        [red-1, blue-2, empty],
        [empty, blue-2, red-1]
    ],
    GameState = gameState(3, Board, blue, _, _, _, _, _),  % Estado do jogo para o jogador 'blue'
    value(GameState, blue, Value),  % Calcula o valor para o jogador 'blue'
    format('Valor para o jogador blue: ~w~n', [Value]),  % Exibe o valor calculado
    
    GameStateRed = gameState(3, Board, red, _, _, _, _, _),  % Estado do jogo para o jogador 'red'
    value(GameStateRed, red, ValueRed),  % Calcula o valor para o jogador 'red'
    format('Valor para o jogador red: ~w~n', [ValueRed]).  % Exibe o valor calculado


% Teste do predicado count_pieces/3
test_count_pieces :-
    Board = [
        [blue-1, red-2, empty],
        [red-1, blue-2, empty],
        [empty, blue-2, red-1]
    ],
    
    % Contar as peças do jogador 'blue'
    count_pieces(Board, blue, BlueTotal),
    format('Total de peças para blue: ~w~n', [BlueTotal]),

    % Contar as peças do jogador 'red'
    count_pieces(Board, red, RedTotal),
    format('Total de peças para red: ~w~n', [RedTotal]).


% minimax(gameState(2,[[red-2,blue-2],[red-4,blue-1]],blue,_,_,_,_,_),2,blue,BestScore,BestMove).
minimax(GameState, Depth, Player, Score, _) :-
    % Caso base: verifica se o jogo acabou ou se atingiu a profundidade 0
    (game_over(GameState, _); Depth == 0),
    minimax_value(GameState, Score),
    !.

minimax(GameState, Depth, Player, BestScore, BestMove) :-
    GameState = gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), % Extração dos elementos de GameState
    Depth > 0,
    valid_moves(GameState, ValidMoves),
    ValidMoves \= [],
    NewDepth is Depth - 1,
    next_player(Player, Opponent),
    % Avaliar todos os movimentos possíveis
    findall((TotalScore, Move),
        (
            member(Move, ValidMoves),
            simulate_move1(GameState, Move, NewGameState),
            minimax(NewGameState, NewDepth, Opponent, Score, _),
            evaluate_move(BoardSize, Board, Player, Move, MoveScore),
            TotalScore is Score + MoveScore
        ),
        ScoresMoves),

    % Selecionar o melhor valor dependendo do jogador
    select_best(Player, ScoresMoves, BestScore-BestMove).

select_best(red, ScoresMoves, BestScore-BestMove) :-
    select_max(ScoresMoves, BestScore-BestMove).

select_best(blue, ScoresMoves, BestScore-BestMove) :-
    select_min(ScoresMoves, BestScore-BestMove).


select_max(ScoresMoves, BestScore-BestMove) :-
    findall(Score, member((Score, _), ScoresMoves), Scores),
    my_max_list(Scores, BestScore),
    % Filtrar os movimentos que possuem o maior score
    findall(Move, (member((BestScore, Move), ScoresMoves)), BestMoves),
    % Selecionar aleatoriamente entre os melhores
    random_member(BestMove, BestMoves).

select_min(ScoresMoves, BestScore-BestMove) :-
    findall(Score, member((Score, _), ScoresMoves), Scores),
    my_min_list(Scores, BestScore),
    % Filtrar os movimentos que possuem o menor score
    findall(Move, (member((BestScore, Move), ScoresMoves)), BestMoves),
    % Selecionar aleatoriamente entre os melhores
    random_member(BestMove, BestMoves).


minimax_value(gameState(BoardSize, Board, Player, _, _, _, _, _), Value) :-
    count_pieces(Board, red, RedCount),
    count_pieces(Board, blue, BlueCount),
    Value is RedCount - BlueCount.

simulate_move1(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule),
              (Row-Col, ToRow-ToCol),
              gameState(BoardSize, NewBoard, NextPlayer, GameType, RedType, BlueType, Level, DiagonalRule)) :-

    % Obter a peça e o tamanho na célula de origem
    get_cell(Row, Col, Board, Piece-Size),

    nth0(ToRow, Board, ToOldRow),        % Obter a linha do destino

    % Obter a peça e o tamanho na célula de destino (caso de captura ou empilhamento)
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
    
    % Mover a peça de acordo com o movimento e atualizar o tamanho
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard),
    next_player(Player, NextPlayer).
    