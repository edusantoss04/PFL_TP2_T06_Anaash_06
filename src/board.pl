
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


display_game(gameState(BoardSize, Board, _, _, _, _, _, _)) :-
    nl,
    transpose(Board, TransposedBoard),  % Transpõe o tabuleiro (troca linhas por colunas)
    reverse(TransposedBoard, ReversedBoard),  % Inverte as linhas após a transposição
    print_lines(ReversedBoard, BoardSize),  % Imprime as linhas e colunas invertidas
    nl,
    print_header(BoardSize).

% Itera sobre cada linha do tabuleiro
print_lines([], _).  % Base case
print_lines([Line|Rest], Index) :-
    format('~d  ', [Index]),  % Número da linha antes da célula
    print_line(Line),
    nl,  % Nova linha após imprimir a linha inteira
    NextIndex is Index - 1,  % Decrementa o índice para ir de baixo para cima
    print_lines(Rest, NextIndex).  % Chama recursivamente para a próxima linha

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
    write('      1      2      3      4      5      6      7      8 \n').
print_header(6):-
    write('      1      2      4      4      5      6 \n').
print_header(4):-
    write('      1      2      3      4 \n').
