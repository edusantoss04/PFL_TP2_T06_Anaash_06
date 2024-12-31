:- consult(menu).
:- consult(utils).
:- consult(a).

play :-
    configure_game(GameConfig),
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_cycle(GameState).


game_cycle(gameState(BoardSize,Board,Player, GameType, RedType ,BlueType,Level)):-
    game_over(gameState(BoardSize,Board,Player, GameType, RedType ,BlueType,Level), Winner), !,
    congratulate(Winner).

game_cycle(GameState):-
    nl,
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState).


initial_state((GameType, BoardSize, Difficulty), gameState(BoardSize,Board,red, GameType, RedType ,BlueType,Level)) :-
    board(BoardSize,Board), %vai buscar o Board
    map_difficulty(Difficulty,Level),
    map_game_type(GameType, RedType, BlueType).

map_game_type(h_h, human , human).
map_game_type(h_pc, human , bot).
map_game_type(pc_pc, bot ,bot).

map_difficulty(DifficultyRed-DifficultyBlue, LevelRed-LevelBlue) :-
    map_difficulty(DifficultyRed, LevelRed),
    map_difficulty(DifficultyBlue, LevelBlue),
    !.
map_difficulty(Difficulty, Level) :-
    map_single_difficulty(Difficulty, Level),
    !.

map_single_difficulty(easy, 1).
map_single_difficulty(hard, 2).
map_single_difficulty(empty, 0).



% choose_move(+GameState, -Move).

% Red é human 
choose_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level), Move) :-
    repeat,  % Inicia a repetição
    get_move(red, Move),  % Solicita o movimento
    valid_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level), Move),  % Verifica se o movimento é válido
    !.  % Interrompe a repetição se o movimento for válido

% Blue é human 
choose_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level), Move) :-
    repeat,  % Inicia a repetição
    get_move(blue, Move),  % Solicita o movimento
    valid_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level), Move),  % Verifica se o movimento é válido
    !.  % Interrompe a repetição se o movimento for válido

% Blue é bot mas red é human
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1), Moves),
    random_member(Move, Moves), 
    display_bot_move(Move), !.

% Red é bot mas blue é human
choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 1), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1), Moves),
    random_member(Move, Moves), 
    display_bot_move(Move), !.

% Bot vs Bot (necessário devido à forma como está implementado o LevelRed-LevelBlue)

choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 1-LevelBlue), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 1-LevelBlue), Moves),  % Obtém os movimentos válidos
    random_member(Move, Moves),  % Seleciona aleatoriamente um movimento válido
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-1), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-1), Moves),  % Obtém os movimentos válidos
    random_member(Move, Moves),  % Seleciona aleatoriamente um movimento válido
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

% Bot vs Bot (red) - Nível 2 
choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue), Moves),  % Obtém os movimentos válidos
    choose_best_move(Moves, Move),  % Implementa lógica para escolher o melhor movimento baseado no nível 2
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

% Bot vs Bot (blue) - Nível 2 
choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2), Moves),  % Obtém os movimentos válidos
    choose_best_move(Moves, Move),  % Implementa lógica para escolher o melhor movimento baseado no nível 2
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito


% Função para obter o movimento do jogador
get_move(Player, Move) :-
    write(Player),
    write(', choose your move (RowI-ColI,RowF-ColF): '), nl,
    catch(read(Move), _, (write('Invalid input. Please try again.'), nl, fail)),
    !.

% Função para verificar se o movimento é válido
valid_move(GameState, Move) :-
    move(GameState, Move, _),  % Tenta realizar o movimento
    !.  % Se o movimento for válido, interrompe o repeat
valid_move(_) :-
    write('Invalid move. Please try again.'), nl,
    fail.  % Se o movimento for inválido, força a repetição


congratulate(Winner) :-
    nl,
    format('~`=t~40|~n', []),  % Linha de separação
    format('   Congratulations, ~w!~n', [Winner]),
    format('   You are the winner!~n', []),
    format('~`=t~40|~n', []),
    halt . % Linha de separação   

