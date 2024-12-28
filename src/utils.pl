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
