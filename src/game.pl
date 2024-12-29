:- consult(menu).
:- consult(utils).

play :-
    configure_game(GameConfig),
    initial_state(GameConfig, GameState),
    display_game(GameState),
    game_cycle(GameState).




game_cycle(GameState):-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState):-
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer), % could be done in move/3
    display_game(NewGameState), !,
    game_cycle(NewGameState).



% choose_move(+GameState, +Level, -Move).
