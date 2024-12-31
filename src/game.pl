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

map_difficulty(easy,1).
map_difficulty(hard,2).
map_difficulty(empty,0).



% choose_move(+GameState, -Move).
choose_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level), Move) :-
    repeat,  % Inicia a repetição
    get_move(red, Move),  % Solicita o movimento
    valid_move(gameState(BoardSize, Board, red, GameType, human, BlueType, Level), Move),  % Verifica se o movimento é válido
    !.  % Interrompe a repetição se o movimento for válido

choose_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level), Move) :-
    repeat,  % Inicia a repetição
    get_move(blue, Move),  % Solicita o movimento
    valid_move(gameState(BoardSize, Board, blue, GameType, RedType, human, Level), Move),  % Verifica se o movimento é válido
    !.  % Interrompe a repetição se o movimento for válido

choose_move(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1), Moves),
    random_member(Move, Moves), 
    display_bot_move(Move), !.

choose_move(gameState(BoardSize, Board, Player, GameType, bot, BlueType, 1), Move) :-
    valid_moves(gameState(BoardSize, Board, Player, GameType, RedType, bot, 1), Moves),
    random_member(Move, Moves), 
    display_bot_move(Move), !.

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
