:- consult(menu).
:- consult(utils).
:- consult(a).

play :-
    configure_game(GameConfig),
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_cycle(GameState).


game_cycle(gameState(Board,Player, GameType, RedType ,BlueType,Level)):-
    game_over(gameState(Board,Player, GameType, RedType ,BlueType,Level), Winner), !,
    congratulate(Winner).

game_cycle(gameState(Board,Player, GameType, RedType ,BlueType, Level)):-
    choose_move(gameState(Board,Player, GameType, RedType ,BlueType, Level), Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer), % could be done in move/3
    display_game(NewGameState), !,
    game_cycle(NewGameState).



% choose_move(+GameState, -Move).



initial_state((GameType, BoardSize, Difficulty), gameState(Board,red, GameType, RedType ,BlueType,Level)) :-
    board(BoardSize, Board),
    map_difficulty(Difficulty,Level),
    map_game_type(GameType, RedType, BlueType).

map_game_type(h_h, human , human).
map_game_type(h_pc, human , bot).
map_game_type(pc_pc, bot ,bot).

map_difficulty(easy,1).
map_difficulty(hard,2).
map_difficulty(empty,0).