:- use_module(library(between)).

read_menu_option(Min,Max,SelectedOption) :-
    write('Select option: '),
    repeat,
    read_until_between(Min,Max,SelectedOption).
    

read_number_acc(X, X) :- peek_code(10), !. 
read_number_acc(Acc, X) :-
    \+ peek_code(10),                      
    get_code(Code),                                                     
    Code >= 48, Code =< 57,                 % Verificar se é o código é de um número
    Digit is Code - 48,                     % Converter o código para número
    NewAcc is Acc * 10 + Digit,            
    read_number_acc(NewAcc, X).          

read_number(X) :-
    read_number_acc(0, X),                 
    get_code(10).                          

% Lê até que um número válido seja inserido, dentro do intervalo [Min, Max].
read_until_between(Min, Max, Value) :-
    repeat,                                
    read_number(Value),                    
    between(Min, Max, Value),                
    !.                                     

% Função auxiliar para obter o conteúdo da célula de destino
get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize) :-
    nth0(ToCol, ToOldRow, TargetPiece-TargetSize), !.  % Caso a célula não seja vazia
get_target_piece(_, _, empty, 0).  % Caso a célula seja vazia

% Trocar de jogador
next_player(blue, red).
next_player(red, blue).


print_text_color(purple) :-
    write('\e[35m').  % Texto roxo
print_text_color(cyan) :-
    write('\e[36m').  % Texto ciano
print_text_color(green) :-
    write('\e[32m').  % Texto verde
print_text_color(yellow) :-
    write('\e[33m').  % Texto amarelo
print_text_color(blue) :-
    write('\e[34m').  % Texto azul
print_text_color(red) :-
    write('\e[31m').  % Texto vermelho
print_text_color(white) :-
    write('\e[97m').  % Texto branco

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


display_bot_move((Row-Col, ToRow-ToCol), Bot):-
    NewRow is Row + 1,
    NewCol is Col + 1,
    NewToCol is ToCol + 1,
    NewToRow is ToRow + 1,
    format('The ~w bot moved from (~d, ~d) to (~d, ~d).~n', [Bot, NewRow, NewCol, NewToRow, NewToCol]).
    
% Predicado que verifica se todas as peças têm a mesma cor (ignorando números).
same_color([], _) :- fail.              % Falha se não houver peças (apenas 'empty').
same_color([Color-_|T], Winner) :-      % Ignora o número e verifica apenas a cor.
    maplist(has_same_color(Color), T),  % Verifica se todos os elementos têm a mesma cor.
    Winner = Color.

% Verifica se a peça tem a mesma cor (ignora números).
has_same_color(Color, Color-_).

get_cell(Row, Col, Board, Piece-Size):-
    nth0(Row, Board, CurrentRow),
    nth0(Col, CurrentRow, Piece-Size).

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


my_max_list([X], X).

my_max_list([Head|Tail], Max) :-
    my_max_list(Tail, TailMax), 
    Max is max(Head, TailMax).  

max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

% Caso base: a lista contém apenas um elemento, este é o minimo.
my_min_list([X], X).

% Caso recursivo: compara o primeiro elemento (Head) com o minimo do restante da lista.
my_min_list([Head|Tail], Min) :-
    my_min_list(Tail, TailMin), 
    Min is min(Head, TailMin).

% Predicado auxiliar para calcular o menor entre dois valores.
min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.


% Subtrai 1 de cada coordenada do movimento
move_minus_1((Row-Col, ToRow-ToCol), (NewRow-NewCol, NewToRow-NewToCol)) :-
    NewRow is Row - 1,  % Subtrai 1 de Row
    NewCol is Col - 1,  % Subtrai 1 de Col
    NewToRow is ToRow - 1,  % Subtrai 1 de ToRow
    NewToCol is ToCol - 1.  % Subtrai 1 de ToCol

