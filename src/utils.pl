read_menu_option(Min,Max,SelectedOption) :-
    write('Select option: '),
    repeat,
    read_until_between(Min,Max,SelectedOption).
    

read_number_acc(X, X) :- peek_code(10), !. 
read_number_acc(Acc, X) :-
    \+ peek_code(10),                      
    get_code(Code),                                                     
    Code >= 48, Code =< 57,                 % Check if the code corresponds to a number
    Digit is Code - 48,                     % Convert the code to a number
    NewAcc is Acc * 10 + Digit,            
    read_number_acc(NewAcc, X).          

read_number(X) :-
    read_number_acc(0, X),                 
    get_code(10).                          

% Reads until a valid number is entered, within the range [Min, Max].
read_until_between(Min, Max, Value) :-
    repeat,                                
    read_number(Value),                    
    between(Min, Max, Value),                
    !.                                     

% Auxiliary function to obtain the target cell's content
get_target_piece(ToOldRow, ToCol, TargetPiece, TargetSize) :-
    nth0(ToCol, ToOldRow, TargetPiece-TargetSize), !.  % If the cell is not empty
get_target_piece(_, _, empty, 0).  % If the cell is empty

% Switch player
next_player(blue, red).
next_player(red, blue).

print_text_color(purple) :-
    write('\e[35m').  % Purple text
print_text_color(cyan) :-
    write('\e[36m').  % Cyan text
print_text_color(green) :-
    write('\e[32m').  % Green text
print_text_color(yellow) :-
    write('\e[33m').  % Yellow text
print_text_color(blue) :-
    write('\e[34m').  % Blue text
print_text_color(red) :-
    write('\e[31m').  % Red text
print_text_color(white) :-
    write('\e[97m').  % White text

% Set color to blue
print_color(blue) :-
    write('\e[44m'),  % Blue background
    write('\e[97m').  % White text

% Set color to red
print_color(red) :-
    write('\e[41m'),  % Red background
    write('\e[97m').  % White text

% Set color for empty (white background, invisible text)
print_color(empty) :-
    write('\e[47m'),  % White background
    write('\e[90m').  % Grey text (invisible, only background)

% Function to reset the color after printing the cell
reset_color :-
    write('\e[0m').  % Reset to the terminal's default color

display_bot_move((Row-Col, ToRow-ToCol), Bot):-
    NewRow is Row + 1,
    NewCol is Col + 1,
    NewToCol is ToCol + 1,
    NewToRow is ToRow + 1,
    format('The ~w bot moved from (~d, ~d) to (~d, ~d).~n', [Bot, NewRow, NewCol, NewToRow, NewToCol]).
    
% Predicate to check if all pieces have the same color (ignoring numbers).
same_color([], _) :- fail.              % Fail if there are no pieces (only 'empty').
same_color([Color-_|T], Winner) :-      % Ignore the number and check only the color.
    maplist(has_same_color(Color), T),  % Check if all elements have the same color.
    Winner = Color.

% Check if the piece has the same color (ignoring numbers).
has_same_color(Color, Color-_).

get_cell(Row, Col, Board, Piece-Size):-
    nth0(Row, Board, CurrentRow),
    nth0(Col, CurrentRow, Piece-Size).

% Replace an element at position [Row, Col] in the matrix (board).
replace([Row|T], 0, Col, X, [NewRow|T]) :-
    replace(Row, Col, X, NewRow).  % Replace in the corresponding row
replace([H|T], Row, Col, X, [H|R]) :-
    Row > 0,  % If the row is not 0, iterate through other rows
    Row1 is Row - 1,
    replace(T, Row1, Col, X, R).  % Decrement the row index and continue with the tail

% Replace an element at position Col in the list (row).
replace([_|T], 0, X, [X|T]).  % If the position is 0, replace the first element with X
replace([H|T], N, X, [H|R]) :-  % Otherwise, iterate through the list
    N > 0,  % If the index is greater than 0
    N1 is N - 1,  % Decrement the index
    replace(T, N1, X, R).  % Continue recursively with the tail

% Base case: an empty list is already flattened.
flatten([], []).

% If the first element is a list, flatten it recursively.
flatten([H|T], FlatList) :-
    is_list(H),
    flatten(H, HFlat),
    flatten(T, TFlat),
    append(HFlat, TFlat, FlatList).

% If the first element is not a list, simply add it to the result.
flatten([H|T], [H|TFlat]) :-
    \+ is_list(H),
    flatten(T, TFlat).

my_max_list([X], X).

my_max_list([Head|Tail], Max) :-
    my_max_list(Tail, TailMax), 
    Max is max(Head, TailMax).  

max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

% Base case: the list contains only one element, which is the minimum.
my_min_list([X], X).

% Recursive case: compare the first element (Head) with the minimum of the rest of the list.
my_min_list([Head|Tail], Min) :-
    my_min_list(Tail, TailMin), 
    Min is min(Head, TailMin).

% Auxiliary predicate to calculate the smaller of two values.
min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.

% Subtract 1 from each coordinate of the move
move_minus_1((Row-Col, ToRow-ToCol), (NewRow-NewCol, NewToRow-NewToCol)) :-
    NewRow is Row - 1,  % Subtract 1 from Row
    NewCol is Col - 1,  % Subtract 1 from Col
    NewToRow is ToRow - 1,  % Subtract 1 from ToRow
    NewToCol is ToCol - 1.  % Subtract 1 from ToCol

verifyEmpty(Board, Row-Col, ToRow-ToCol, empty):-
    % If the destination cell is empty, validate the move
    \+ valid_positional_move(Board, Row-Col, ToRow-ToCol), !, fail.

verifyEmpty(_, _, _, _):-
    % In all other cases, the move is considered valid
    true.

% Function to calculate the Manhattan distance between two positions
manhattan_distance(Row1-Col1, Row2-Col2, Distance) :-
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).

my_min_list([Head|Tail], Min) :-
    my_min_list(Tail, Head, Min).

my_min_list([], Min, Min).
my_min_list([Head|Tail], MinSoFar, Min) :-
    (Head < MinSoFar -> NewMin = Head; NewMin = MinSoFar),
    my_min_list(Tail, NewMin, Min).
