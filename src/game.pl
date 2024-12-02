:- initialization(main).

% Cria um tabuleiro de 6x6 com todos os elementos iguais a 1
create_board(6, Board) :-
    length(Board, 6),
    maplist(length_(6), Board),
    maplist(maplist(=(1)), Board).

length_(L, List) :-
    length(List, L).

% Imprime o tabuleiro com bordas
print_board(Board) :-
    write('  +---+---+---+---+---+---+'), nl,
    print_rows(Board, 1).

% Imprime todas as linhas do tabuleiro com bordas e índices
print_rows([], _).
print_rows([Row|Rest], Index) :-
    format('~w |', [Index]),
    print_row(Row),
    nl,
    write('  +---+---+---+---+---+---+'), nl,
    NextIndex is Index + 1,
    print_rows(Rest, NextIndex).

% Imprime uma linha do tabuleiro com bordas
print_row([]).
print_row([Cell|Rest]) :-
    format(' ~w |', [Cell]),
    print_row(Rest).

% Imprime os índices das colunas
print_column_indices :-
    write('    A   B   C   D   E   F').

main :-
    create_board(6, Board),
    print_column_indices, nl,
    print_board(Board),
    halt.
