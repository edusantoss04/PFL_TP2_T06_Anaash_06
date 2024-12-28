:- consult(menu).
:- consult(utils).

play :-
    write('Welcome to anaash!'), nl,
    configure_game(GameConfig).
