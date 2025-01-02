
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
    print_lines(Board, 0),
    nl,
    print_header(BoardSize).

% Itera sobre cada linha do tabuleiro
print_lines([], _ , _).
print_lines([Line|Rest], Index, BoardSize) :-
    format('~d  ', [Index]),  % Número da linha antes da célula
    print_line(Line),
    nl,  % Nova linha após imprimir a linha inteira
    NextIndex is Index + 1,  % Incrementa o índice
    (Index < BoardSize -> print_lines(Rest, NextIndex, BoardSize); true).

% Imprime uma linha específica
print_line([]).
print_line([Cell|Rest]) :-
    print_cell(Cell),
    print_line(Rest).

% Imprime uma célula vazia
print_cell(empty) :-
    write('\e[47m'),          % Fundo branco
    write('       '),         % Espaços para formatação
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

print_header(8):-
    write('      8      7      6      5      4      3      2      1 \n').
print_header(6):-
    write('      5      4      3      2      2      1 \n').
print_header(4):-
    write('      4      3      2      1 \n').
