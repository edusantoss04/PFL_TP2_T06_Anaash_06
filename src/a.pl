:- use_module(library(lists)).
:- use_module(library(random)).

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


board(10, [[blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
           [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
           [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
           [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
           [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
           [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
           [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
           [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1],
           [blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1],
           [red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1, red-1, blue-1]]).

move(gameState(BoardSize,Board, Player, GameType, RedType, BlueType, Level), (Row-Col,ToRow-ToCol), gameState(BoardSize,NewBoard, NewPlayer, GameType, RedType, BlueType, Level)) :-
    % Trocar o jogador
    next_player(Player, NewPlayer),

    % Validar a direção
    valid_direction(Row, Col, ToRow, ToCol),

    % Obter a peça a mover
    nth0(Row, Board, OldRow),
    nth0(Col, OldRow, Piece-Size),

    % Obter o conteúdo da célula de destino
    nth0(ToRow, Board, ToOldRow),
    get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize), !,

    % Verificar se o movimento é válido
    valid_move(Piece, Size, TargetPiece, TargetSize, Player),

    % Mover a peça
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard).


% Função auxiliar para obter o conteúdo da célula de destino
get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize) :-
    nth0(ToCol, ToOldRow, TargetPiece-TargetSize), !.  % Caso a célula não seja vazia
get_target_piece(_, _, empty, 0).  % Caso a célula seja vazia

% Trocar de jogador
next_player(blue, red).
next_player(red, blue).

% Validar a direção do movimento (diagonal ou horizontal)
valid_direction(Row, Col, Row, ToCol) :-
    abs(Col - ToCol) =:= 1.
valid_direction(Row, Col, ToRow, Col) :-
    abs(Row - ToRow) =:= 1.
valid_direction(_,_,_,_):- fail.

% Verificar se o movimento é válido
valid_move(Piece, Size, empty, _, Player) :-
    Piece == Player, !.  % A peça deve pertencer ao jogador
valid_move(Piece, Size, TargetPiece, TargetSize, Player) :-
    Piece == Player,  % A peça pertence ao jogador
    valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player).

% Condições de movimento
valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player) :-
    (Player == blue, TargetPiece == red, Size >= TargetSize);  % Blue pode capturar Red
    (Player == blue, TargetPiece == blue, Size =< TargetSize);  % Blue pode mover para outra célula azul
    (Player == red, TargetPiece == blue, Size >= TargetSize);  % Red pode capturar Blue
    (Player == red, TargetPiece == red, Size =< TargetSize).   % Red pode mover para outra célula vermelha

% Mover a peça no tabuleiro
move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard) :-
    % Atualizar o tamanho usando a função update_size
    update_size(Size, TargetPiece, TargetSize, NewSize),

    % Atualizar a célula de origem (deve ser 'empty' após o movimento)
    replace(Board, Row, Col, empty, TempBoard),

    % Colocar a peça na nova posição com o novo tamanho
    replace(TempBoard, ToRow, ToCol, Piece-NewSize, NewBoard).

% Atualizar o tamanho da peça
update_size(Size, empty, NewSize) :-
    NewSize = Size.  % Se a célula de destino for vazia, não altera o tamanho.

update_size(Size, TargetPiece, TargetSize, NewSize) :-
    Piece \== TargetPiece,  % Se for uma peça adversária
    NewSize is Size + TargetSize.

update_size(Size, TargetPiece, TargetSize, NewSize) :-
    Piece == TargetPiece,  % Se for uma peça do mesmo jogador
    NewSize is Size + TargetSize.

% Substitui um elemento na posição [Row, Col] da matriz (board).
replace([Row|T], 0, Col, X, [NewRow|T]) :-
    replace(Row, Col, X, NewRow).  % Substitui na linha correspondente
replace([H|T], Row, Col, X, [H|R]) :-
    Row > 0,  % Se a linha não for a 0, percorre as outras linhas
    Row1 is Row - 1,
    replace(T, Row1, Col, X, R).  % Decrementa o índice da linha e continua com a cauda

% Substitui um elemento na posição Col da lista (linha).
replace([_|T], 0, X, [X|T]).  % Se a posição for 0, substitui o primeiro elemento pela X
replace([H|T], N, X, [H|R]) :-  % Caso contrário, percorre a lista
    N > 0,  % Se o índice for maior que 0
    N1 is N - 1,  % Decrementa o índice
    replace(T, N1, X, R).  % Continua a busca recursivamente na cauda




% para testar move(gameState(6,[[blue-2, blue-1], [red-1, blue-1]], blue, _, _, _, _), (0-0, 0-1), gameState(6,NewBoard, NewPlayer, _, _, _, _)).

% gameState(board, player, GameType, RedType, BlueType, Level).

% gameConfig(GameType,SizeBoard, Dificulty).

% para testar display_game(gameState(6,[[blue-1, red-1, blue-1, red-1, blue-1, red-1],[red-1, blue-1, red-1, blue-1, red-1, blue-1],[blue-1, red-1, blue-1, red-1, blue-1, red-1],[red-1, blue-1, red-1, blue-1, red-1, blue-1],[blue-1, red-1, blue-1, red-1, blue-1, red-1],[red-1, blue-1, red-1, blue-1, red-1, blue-1]], _, _, _, _, _)).

display_game(gameState(BoardSize, Board, _, _, _, _, _)):-
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

% Define a cor para azul
print_color(blue) :-
    write('\e[44m'),  % Fundo azul
    write('\e[97m').  % Texto branco

% Define a cor para vermelho
print_color(red) :-
    write('\e[41m'),  % Fundo vermelho
    write('\e[97m').  % Texto branco

% Define a cor para empty (fundo branco, mas sem texto visível)
print_color(empty) :-
    write('\e[47m'),  % Fundo branco
    write('\e[90m').  % Texto cinza (invisível, só fundo)

% Função para resetar a cor após imprimir a célula
reset_color :-
    write('\e[0m').  % Reseta para a cor padrão do terminal

print_header(10):-
    write('      0      1      2      3      4      5      6      7      8      9 \n').
print_header(8):-
    write('      0      1      2      3      4      5      6      7 \n').
print_header(6):-
    write('      0      1      2      3      4      5 \n').
print_header(4):-
    write('      0      1      2      3 \n').



% testar game_over(gameState(6,[[red-2, blue-1, red-1, red-1, red-1, red-1],[red-1, red-1, red-1, red-1, red-1, red-1]],_,_,_,_,_), Winner).

% Caso base: game over se todas as peças são da mesma cor.
game_over(gameState(BoardSize,Board, _, _, _, _,_), Winner) :-
    flatten(Board, FlatList),           % Achatar a lista de listas em uma lista única.
    exclude(=(empty), FlatList, Pieces), % Remover todas as posições 'empty'.
    same_color(Pieces, Winner).         % Verificar se todas as peças têm a mesma cor.

% Predicado que verifica se todas as peças têm a mesma cor (ignorando números).
same_color([], _) :- fail.              % Falha se não houver peças (apenas 'empty').
same_color([Color-_|T], Winner) :-      % Ignora o número e verifica apenas a cor.
    maplist(has_same_color(Color), T),  % Verifica se todos os elementos têm a mesma cor.
    Winner = Color.

% Verifica se a peça tem a mesma cor (ignora números).
has_same_color(Color, Color-_).

% Caso base: uma lista vazia já está achatada.
flatten([], []).

% Se o primeiro elemento for uma lista, achatar recursivamente.
flatten([H|T], FlatList) :-
    is_list(H),
    flatten(H, HFlat),
    flatten(T, TFlat),
    append(HFlat, TFlat, FlatList).

% Se o primeiro elemento não for uma lista, apenas adicioná-lo ao resultado.
flatten([H|T], [H|TFlat]) :-
    \+ is_list(H),
    flatten(T, TFlat).


get_cell(Row, Col, Board, Piece-Size):-
    nth0(Row, Board, CurrentRow),
    nth0(Col, CurrentRow, Piece-Size).



% Retorna todos os movimentos válidos para um dado jogador
valid_moves(gameState(BoardSize, Board, Player, _, _, _, _), Moves) :-
    Limit is BoardSize - 1,
    findall(
        (Row-Col, ToRow-ToCol),
        (
            between(0, Limit, Row),
            between(0, Limit, Col),
            get_cell(Row, Col, Board, Piece-Size),
            Piece == Player,
            adjacent_position(BoardSize, Row, Col, ToRow, ToCol),
            nth0(ToRow, Board, ToOldRow),
            get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize),
            valid_move(Piece, Size, TargetPiece, TargetSize, Player)
        ),
        AllMoves
    ),
    sort(AllMoves, Moves).

% Retorna as posições adjacentes a uma célula dentro dos limites do tabuleiro
adjacent_position(BoardSize, Row, Col, ToRow, ToCol) :-
    member((DRow, DCol), [(0, 1), (1, 0), (0, -1), (-1, 0)]),
    ToRow is Row + DRow,
    ToCol is Col + DCol,
    ToRow >= 0, ToRow < BoardSize,
    ToCol >= 0, ToCol < BoardSize.




% valid_moves(gameState(2, [[empty, blue-1],[red-5, empty]],blue,_,_,_,_), Moves).


display_bot_move((Row-Col, ToRow-ToCol)):-
    format('The blue bot moved from (~d, ~d) to (~d, ~d).~n', [Row, Col, ToRow, ToCol]).


