
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

move(gameState(Board, Player), move(Row, Col, ToRow, ToCol), gameState(NewBoard, NewPlayer)) :-
    % Trocar o jogador
    next_player(Player, NewPlayer),

    % Validar a direção
    valid_direction(Row, Col, ToRow, ToCol),

    % Obter a peça a mover
    nth0(Row, Board, OldRow),
    nth0(Col, OldRow, Piece-Size),

    % Obter o conteúdo da célula de destino
    nth0(ToRow, Board, ToOldRow),
    nth0(ToCol, ToOldRow, TargetPiece-TargetSize),

    % Verificar se o movimento é válido
    valid_move(Piece, Size, TargetPiece, TargetSize, Player),

    % Mover a peça
    move_piece(Board, Row, Col, ToRow, ToCol, Piece, Size, TargetPiece, TargetSize, NewBoard).

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
    Piece == Player.  % A peça deve pertencer ao jogador
valid_move(Piece, Size, TargetPiece, TargetSize, Player) :-
    Piece == Player,  % A peça pertence ao jogador
    valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player).

% Condições de movimento
valid_move_condition(Piece, Size, TargetPiece, TargetSize, Player) :-
    (Player == blue, TargetPiece == red, Size >= TargetSize);  % Blue pode capturar Red
    (Player == blue, TargetPiece == blue, Size >= TargetSize);  % Blue pode mover para outra célula azul
    (Player == red, TargetPiece == blue, Size >= TargetSize);  % Red pode capturar Blue
    (Player == red, TargetPiece == red, Size >= TargetSize).   % Red pode mover para outra célula vermelha

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


nth0(0, [X|_], X).
nth0(N, [_|T], X) :-
    N > 0,
    N1 is N - 1,
    nth0(N1, T, X).



% para testar move(gameState([[blue-1, red-1], [red-1, blue-1]], blue), move(0, 0, 1, 0), gameState(NewBoard, NewPlayer)).

% gameState(Board, player).

% gameConfig(GameType,SizeBoard, Dificulty).