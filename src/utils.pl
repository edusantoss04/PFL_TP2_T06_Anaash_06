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