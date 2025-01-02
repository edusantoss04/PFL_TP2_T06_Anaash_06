:- consult(menu).
:- consult(utils).
:- consult(a).
:- use_module(library(system), [now/1]).

init_random_state :-
    now(X),
    setrand(X).

play :-
    init_random_state,
    configure_game(GameConfig),
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_cycle(GameState).


game_cycle(gameState(BoardSize,Board,Player, GameType, RedType ,BlueType,Level,DiagonalRule)):-
    game_over(gameState(BoardSize,Board,Player, GameType, RedType ,BlueType,Level,DiagonalRule), Winner), !,
    congratulate(Winner).

% game_cycle(gameState(2, [[blue-3,blue-3],[blue-3,red-1]],red, h_h, human, human,0)).
% Caso em que há um movimento válido
game_cycle(GameState) :-
    nl,
    choose_move(GameState, Move),  % Jogador realiza um movimento válido
    move(GameState, Move, NewGameState),  % Aplica o movimento
    display_game(NewGameState),  % Mostra o estado atualizado
    !,
    game_cycle(NewGameState).  % Continua para o próximo turno


initial_state((GameType, BoardSize, Difficulty, DiagonalRule), gameState(BoardSize, Board, red, GameType, RedType, BlueType, Level, DiagonalRule)) :-
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
% Caso sem movimentos válidos
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), skip) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, BlueType, Level, DiagonalRule), []),
    format('Player ~w has no valid moves. Skipping turn./n', [Player]),
    !.

% Red é human 
choose_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level, DiagonalRule), Move) :-
    repeat,  % Inicia a repetição
    get_move(red, Move),  % Solicita o movimento
    valid_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level, DiagonalRule), Move),  % Verifica se o movimento é válido
    !.  % Interrompe a repetição se o movimento for válido

% Blue é human 
choose_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level, DiagonalRule), Move) :-
    repeat,  % Inicia a repetição
    get_move(blue, Move),  % Solicita o movimento
    valid_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level, DiagonalRule), Move),  % Verifica se o movimento é válido
    !.  % Interrompe a repetição se o movimento for válido

% Blue é bot mas red é human
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1, DiagonalRule), Moves),
    random_member(Move, Moves), 
    display_bot_move(Move), !.

% Red é bot mas blue é human
choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 1, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1, DiagonalRule), Moves),
    random_member(Move, Moves), 
    display_bot_move(Move), !.



% Blue é bot level 2 mas red é human
choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 2, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 2, DiagonalRule), Moves),
    bot_move(gameState(BoardSize, Board, Player, h_pc, human, bot, 2, DiagonalRule), Move),  % Usa a função bot_move para escolher o melhor movimento
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

% Red é bot level 2 mas blue é human
choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 2, DiagonalRule), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 2, DiagonalRule), Moves),
    bot_move(gameState(BoardSize, Board, Player, h_pc, bot, human, 2, DiagonalRule), Move),  % Usa a função bot_move para escolher o melhor movimento
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito



% Bot vs Bot (necessário devido à forma como está implementado o LevelRed-LevelBlue)

choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 1-LevelBlue, DiagonalRule), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 1-LevelBlue, DiagonalRule), Moves),  % Obtém os movimentos válidos
    random_member(Move, Moves),  % Seleciona aleatoriamente um movimento válido
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-1, DiagonalRule), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-1, DiagonalRule), Moves),  % Obtém os movimentos válidos
    random_member(Move, Moves),  % Seleciona aleatoriamente um movimento válido
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

% Bot vs Bot (red) - Nível 2 
choose_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue, DiagonalRule), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue, DiagonalRule), Moves),  % Obtém os movimentos válidos
    bot_move(gameState(BoardSize, Board, red, pc_pc, bot, bot, 2-LevelBlue, DiagonalRule), Move),  % Usa a função bot_move para escolher o melhor movimento
    display_bot_move(Move),  % Exibe o movimento do bot
    !.  % Interrompe a repetição quando o movimento for feito

% Bot vs Bot (blue) - Nível 2 
choose_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2, DiagonalRule), Move) :-
    repeat,  % Inicia a repetição até um movimento válido
    valid_moves(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2, DiagonalRule), Moves),  % Obtém os movimentos válidos
    bot_move(gameState(BoardSize, Board, blue, pc_pc, bot, bot, LevelRed-2, DiagonalRule), Move),  % Usa a função bot_move para escolher o melhor movimento
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
