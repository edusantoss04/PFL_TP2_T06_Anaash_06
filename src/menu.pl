% Função principal para configurar o jogo
configure_game(GameConfig) :-
    write('==============================\n'),
    write('   Welcome to the Game Setup   \n'),
    write('==============================\n'),
    nl,
    choose_game_type(GameType),
    choose_board_size(BoardSize),
    handle_difficulty(GameType, Difficulty),
    GameConfig = (GameType, BoardSize, Difficulty),
    write('==============================\n'),
    write('    Game Setup Complete!       \n'),
    write('==============================\n'),
    nl,
    nl,
    write('Game Configuration: '), 
    write(GameConfig),   % Exibe o conteúdo da configuração
    nl.


handle_difficulty(h_h, empty):- !.
handle_difficulty(GameType, Difficulty) :-  
    choose_difficulty(Difficulty). 

% Função para escolher o tipo de jogo
choose_game_type(GameType) :-
    nl,
    write('==============================\n'),
    write('   Choose Game Type           \n'),
    write('==============================\n'),
    write('1) Human vs Human (H/H)\n'),
    write('2) Human vs Computer (H/PC)\n'),
    write('3) Computer vs Computer (PC/PC)\n'),
    repeat,
    read_menu_option(1 , 3 , Option),
    game_type_from_option(Option, GameType),
    nl.

% Mapeia a opção para o tipo de jogo correspondente
game_type_from_option(1, h_h).
game_type_from_option(2, h_pc).
game_type_from_option(3, pc_pc).

% Função para escolher o tamanho do tabuleiro
choose_board_size(BoardSize) :-
    nl,
    write('==============================\n'),
    write('   Choose Board Size          \n'),
    write('==============================\n'),
    write('1) 4x4\n'),
    write('2) 6x6\n'),
    write('3) 8x8\n'),
    write('4) 10x10\n'),
    repeat,
    read_menu_option(1, 4, Option),
    board_size_from_option(Option, BoardSize),
    nl.

% Mapeia a opção para o tamanho do tabuleiro
board_size_from_option(1, 4).
board_size_from_option(2, 6).
board_size_from_option(3, 8).
board_size_from_option(4, 10).


% Função para escolher o nível de dificuldade (apenas para jogos com PC)
choose_difficulty(Difficulty) :-
    nl,
    write('==============================\n'),
    write('   Choose Difficulty Level    \n'),
    write('==============================\n'),
    write('1) Easy\n'),
    write('2) Hard\n'),
    repeat,
    read_menu_option(1, 2, Option),
    difficulty_from_option(Option, Difficulty),
    nl.

% Mapeia a opção para o nível de dificuldade
difficulty_from_option(1, easy).
difficulty_from_option(2, hard).
