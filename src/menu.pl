% Main function to configure the game
configure_game(GameConfig) :-
    print_menu_header('Welcome to Anaash!'),  
    choose_game_type(GameType),  
    choose_board_size(BoardSize),  
    handle_enable_rule(GameType, DiagonalRule),  
    handle_difficulty(GameType, Difficulty),  
    GameConfig = (GameType, BoardSize, Difficulty, DiagonalRule),  
    print_menu_header('Game Setup Complete!').  

% Function to print pretty headers with borders
print_menu_header(Header) :-
    print_text_color(cyan),  
    write('==========================\n'),  
    print_text_color(cyan),  
    write('  '), write(Header),nl,  
    print_text_color(cyan),  
    write('==========================\n'),  
    nl,  
    reset_color.  

% Determines how to configure the difficulty based on the game type
handle_difficulty(h_h, empty) :- !.  
handle_difficulty(h_pc, Difficulty) :-  
    choose_difficulty(Difficulty).  
handle_difficulty(pc_h, Difficulty) :-  
    choose_difficulty(Difficulty).  
handle_difficulty(pc_pc, DifficultyRed-DifficultyBlue) :-  
    print_menu_header('Configure Bot Difficulties'),  
    write('Configure difficulty for Red bot:\n'),  
    choose_difficulty(DifficultyRed),  
    write('Configure difficulty for Blue bot:\n'),  
    choose_difficulty(DifficultyBlue).  

% Function to choose the game type
choose_game_type(GameType) :-
    nl,  
    print_menu_header('Choose Game Type'),  
    write('1) Human vs Human \n'),  
    write('2) Human vs Computer \n'),  
    write('3) Computer vs Human \n'),  
    write('4) Computer vs Computer \n\n'),  
    repeat,  
    read_menu_option(1 , 4 , Option),  
    game_type_from_option(Option, GameType),  
    nl.  

% Maps the option to the corresponding game type
game_type_from_option(1, h_h).  
game_type_from_option(2, h_pc).  
game_type_from_option(3, pc_h).  
game_type_from_option(4, pc_pc).  

% Function to choose the board size
choose_board_size(BoardSize) :-
    nl,  
    print_menu_header('Choose Board Size'),  
    write('1) 4x4\n'),  
    write('2) 6x6\n'),  
    write('3) 8x8\n\n'),  
    repeat,  
    read_menu_option(1, 3, Option),  
    board_size_from_option(Option, BoardSize),  
    nl.  

% Maps the option to the corresponding board size
board_size_from_option(1, 4).  
board_size_from_option(2, 6).  
board_size_from_option(3, 8).  

% Function to choose the difficulty level (only for games with PC)
choose_difficulty(Difficulty) :-
    nl,  
    print_menu_header('Choose Difficulty Level'),  
    write('1) Easy\n'),  
    write('2) Medium\n'),  
    write('3) Hard\n\n'),  
    repeat,  
    read_menu_option(1, 3, Option),  
    difficulty_from_option(Option, Difficulty),  
    nl.  

% Maps the option to the corresponding difficulty level
difficulty_from_option(1, easy).  
difficulty_from_option(2, medium).  
difficulty_from_option(3, hard).  

handle_enable_rule(pc_pc, [0,0]):- !.  
handle_enable_rule(h_pc, [0,0]):- !.  
handle_enable_rule(pc_h, [0,0]):- !.  
handle_enable_rule(h_h, DiagonalRule):-  
    enable_rule(DiagonalRule).  

% Function to allow or disable the diagonal rule
enable_rule(DiagonalRule):-
    nl,  
    print_menu_header('Optional Rule'),  
    write('Do you want to enable the diagonal rule?\n'),  
    write('1) Yes (1 diagonal move per player)\n'),  
    write('2) No (No diagonal movements)\n\n'),  
    repeat,  
    read_menu_option(1, 2, Option),  
    rule_option(Option, DiagonalRule),  
    nl.  

% Maps the option to the diagonal rule configuration
rule_option(1, [1, 1]).  
rule_option(2, [0, 0]).  
