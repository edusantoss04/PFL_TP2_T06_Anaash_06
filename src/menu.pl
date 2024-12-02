:- initialization(main).

% Predicado principal que será chamado na inicialização
main :-
    print_menu,
    read_choice(Choice),
    handle_choice(Choice).

% Imprime o menu do jogo
print_menu :-
    nl,
    write('****************************'), nl,
    write('*        Anaash            *'), nl,
    write('****************************'), nl,
    write('* 1. Novo Jogo             *'), nl,
    write('* 2. Ver Regras            *'), nl,
    write('* 3. Sair                  *'), nl,
    write('****************************'), nl,
    write('Escolha uma opcao (1-3): ').

% Lê a escolha do usuário
read_choice(Choice) :-
    read(Choice).

% Trata a escolha do usuário
handle_choice(1) :-
    nl, write('Iniciando um novo jogo...'), nl,
    % Aqui você pode chamar predicados para iniciar o jogo
    halt.
handle_choice(2) :-
    nl, write('Regras do jogo:'), nl,
    write('1. O jogo é jogado em um tabuleiro de 6x6.'), nl,
    write('2. Dois jogadores, Vermelho e Azul, se revezam.'), nl,
    write('3. Existem três tipos de movimentos: Posicional, Empilhamento e Captura.'), nl,
    write('4. O objetivo é capturar todas as peças do adversário.'), nl,
    % Após mostrar as regras, volta ao menu principal
    main.
handle_choice(3) :-
    nl, write('Saindo do jogo. Ate a proxima!'), nl,
    halt.
handle_choice(_) :-
    nl, write('Escolha invalida. Tente novamente.'), nl,
    main.
