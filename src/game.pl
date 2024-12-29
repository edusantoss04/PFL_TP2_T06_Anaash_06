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
choose_move(gameState(BoardSize,Board,red, GameType, human ,BlueType, Level), Move):-
    get_move(red,Move).
    
choose_move(gameState(BoardSize,Board,blue, GameType, RedType , human, Level), Move):-
    get_move(blue,Move).

get_move(Player, Move):-
    write(Player),
    write(',  choose your move (RowI-ColI,RowF-ColF): '),nl,
    read(Move).
    

