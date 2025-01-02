
board(4, [[blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1],
          [blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1]]).

board(6, [[blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1],
          [blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1],
          [blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1]]).


board(8, [[blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
          [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
          [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
          [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
          [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1]]).


display_game(gameState(BoardSize, Board, _, _, _, _, _,_)):-
    nl,
    print_header(BoardSize),
    print_lines(Board, 0).

% Itera sobre cada linha do tabuleiro
print_lines([], _).
print_lines([Line|Rest], Index) :-
    format('~d  ', [Index]),  % Número da linha antes da célula
    print_line(Line),
    nl,  % Nova linha após imprimir a linha inteira
    NextIndex is Index + 1,
    print_lines(Rest, NextIndex).

% Imprime uma linha específica
print_line([]).
print_line([Cell|Rest]) :-
    print_cell(Cell),
    print_line(Rest).

print_cell(empty) :-
    write('\e[47m'),          % Fundo branco
    write('       '),         % 8 espaços em branco para garantir que o espaço tenha 8 caracteres
    reset_color.

% Imprime uma célula com a cor e número apropriados

print_cell(Color-Number) :-
    print_color(Color),
    number_chars(Number, Digits),  % Converte o número para uma lista de caracteres
    length(Digits, Length),        % Calcula a quantidade de dígitos
    Spaces is 4 - Length,          % Calcula o número de espaços necessários antes do número
    write('   '),                  
    write(Number),                 
    print_spaces(Spaces),                  
    reset_color.

print_spaces(0).
print_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    print_spaces(N1).

print_header(10):-
    write('      0      1      2      3      4      5      6      7      8      9 \n').
print_header(8):-
    write('      0      1      2      3      4      5      6      7 \n').
print_header(6):-
    write('      0      1      2      3      4      5 \n').
print_header(4):-
    write('      0      1      2      3 \n').
