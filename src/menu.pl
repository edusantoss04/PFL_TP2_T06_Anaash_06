% Função principal para configurar o jogo
configure_game(GameConfig) :-
    print_menu_header('Welcome to Anaash!'),
    choose_game_type(GameType),
    choose_board_size(BoardSize),
    handle_enable_rule(GameType, DiagonalRule),
    handle_difficulty(GameType, Difficulty),
    GameConfig = (GameType, BoardSize, Difficulty, DiagonalRule),
    print_menu_header('Game Setup Complete!').  % Exibe o conteúdo da configuração

% Função para imprimir cabeçalhos bonitos com bordas
print_menu_header(Header) :-
    print_text_color(cyan),
    write('==========================\n'),
    print_text_color(cyan),
    write('  '), write(Header),nl,
    print_text_color(cyan),
    write('==========================\n'),
    nl,
    reset_color.

% Determina como configurar a dificuldade com base no tipo de jogo
handle_difficulty(h_h, empty) :- !. % Nenhuma dificuldade para Human vs Human
handle_difficulty(h_pc, Difficulty) :- % Apenas um nível de dificuldade para Human vs Computer
    choose_difficulty(Difficulty).
handle_difficulty(pc_h, Difficulty) :- % Apenas um nível de dificuldade para Human vs Computer
    choose_difficulty(Difficulty).
handle_difficulty(pc_pc, DifficultyRed-DifficultyBlue) :- % Dois níveis de dificuldade para Computer vs Computer
    print_menu_header('Configure Bot Difficulties'),
    write('Configure difficulty for Red bot:\n'),
    choose_difficulty(DifficultyRed),
    write('Configure difficulty for Blue bot:\n'),
    choose_difficulty(DifficultyBlue).

% Função para escolher o tipo de jogo
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

% Mapeia a opção para o tipo de jogo correspondente
game_type_from_option(1, h_h).
game_type_from_option(2, h_pc).
game_type_from_option(3, pc_h).
game_type_from_option(4, pc_pc).

% Função para escolher o tamanho do tabuleiro
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

% Mapeia a opção para o tamanho do tabuleiro
board_size_from_option(1, 4).
board_size_from_option(2, 6).
board_size_from_option(3, 8).

% Função para escolher o nível de dificuldade (apenas para jogos com PC)
choose_difficulty(Difficulty) :-
    nl,
    print_menu_header('Choose Difficulty Level'),
    write('1) Easy\n'),
    write('2) medium\n\n'),
    write('3) hard\n\n'),
    repeat,
    read_menu_option(1, 3, Option),
    difficulty_from_option(Option, Difficulty),
    nl.

% Mapeia a opção para o nível de dificuldade
difficulty_from_option(1, easy).
difficulty_from_option(2, Medium).
difficulty_from_option(3, Hard).

handle_enable_rule(pc_pc, [0,0]):- !.
handle_enable_rule(h_pc, [0,0]):- !.
handle_enable_rule(pc_h, [0,0]):- !.
handle_enable_rule(h_h, DiagonalRule):-
    enable_rule(DiagonalRule).

% Função para permitir ou desabilitar a regra diagonal
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

% Mapeia a opção para a configuração da regra diagonal
rule_option(1, [1, 1]).
rule_option(2, [0, 0]).
    