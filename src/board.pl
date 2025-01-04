% Example boards of different sizes (4x4, 6x6, 8x8)
% Each cell is defined by its color (blue or red) and its size
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


% Main predicate to display the game board
display_game(gameState(BoardSize, Board, _, _, _, _, _, _)) :-
    nl,  % Print a new line for separation
    transpose(Board, TransposedBoard),  % Transpose the board (swap rows and columns)
    reverse(TransposedBoard, ReversedBoard),  % Reverse rows to adjust display orientation
    print_lines(ReversedBoard, BoardSize),  % Print the formatted board lines
    nl,  % Add a new line after the board
    print_header(BoardSize).  % Print the column headers based on the board size

% Base case for printing a line: no more cells to print
print_lines([], _). 

% Recursive case: print each line of the board
print_lines([Line|Rest], Index) :-
    format('~d  ', [Index]),  
    print_line(Line),
    nl,  
    NextIndex is Index - 1,  
    print_lines(Rest, NextIndex). 

% Recursive case: print each cell in a line
print_line([]).
print_line([Cell|Rest]) :-
    print_cell(Cell),
    print_line(Rest).

% Print an empty cell
% Uses a white background and spaces to simulate an empty square
print_cell(empty) :-
    write('\e[47m'),          
    write('       '),        
    reset_color.

% Print a cell with a specific color and number
print_cell(Color-Number) :-
    print_color(Color),
    number_chars(Number, Digits),  
    length(Digits, Length),        
    Spaces is 4 - Length,         
    write('   '),                  
    write(Number),                 
    print_spaces(Spaces),                  
    reset_color.

% Base case for printing spaces: no spaces left to print
print_spaces(0).

% Recursive case: print the remaining spaces
print_spaces(N) :-
    N > 0, % Ensure there are spaces to print
    write(' '), % Print a single space
    N1 is N - 1, % Decrement the remaining spaces count
    print_spaces(N1). % Recursively print the remaining spaces

% Print column headers for a board of size 8x8
print_header(8):-
    write('      1      2      3      4      5      6      7      8 \n').

% Print column headers for a board of size 6x6
print_header(6):-
    write('      1      2      4      4      5      6 \n').

% Print column headers for a board of size 4x4
print_header(4):-
    write('      1      2      3      4 \n').

 