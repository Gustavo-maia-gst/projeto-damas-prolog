:- module(utils, [get_cell/4, clear_selection/2, set_all_cells_unavailable/2, set_cell/5, get_selected_position/3, is_empty/1, is_enemy/2, is_in_bounds/2, get_move_directions/3]).

get_cell(Line, Col, State, Cell) :-
    Cell_selected = (State.selected == [Line, Col]),
    Cell_under_cursor = (State.cursor == [Line, Col]),
    nth0(Line, State.matrix, Row),
    nth0(Col, Row, Cell_aux),
    Cell = Cell_aux.put(_{is_selected: Cell_selected, is_under_cursor: Cell_under_cursor}).

set_cell(Line, Col, State, NewCell, NewState) :-
    nth0(Line, State.matrix, CurrentRow),
    replace_in_list(Col, CurrentRow, NewCell, NewRow),
    replace_in_list(Line, State.matrix, NewRow, NewMatrix),
    NewState = State.put(matrix, NewMatrix).

replace_in_list(Index, List, NewElement, NewList) :-
    length(List, Length),
    findall(Element,
        (between(0, Length-1, I),
         (I =:= Index -> Element = NewElement
         ; nth0(I, List, Element))),
        NewList).

clear_selection(State, R) :-
    set_all_cells_unavailable(State, Aux),
    R = Aux.put(selected, none).
    
set_all_cells_unavailable(State, R) :-
    findall(NewRow, 
        (between(0, 7, I),
         nth0(I, State.matrix, Row),
         findall(NewCell,
             (between(0, 7, J),
              nth0(J, Row, Cell),
              NewCell = Cell.put(is_available, false)),
             NewRow)),
        NewMatrix),
    R = State.put(matrix, NewMatrix)
    
get_selected_position(State, X, Y) :-
    State.selected = [X, Y].

get_selected_position(State, none, none) :-
    State.selected = none.

is_empty(Cell) :-
    Cell.player == none.

is_enemy(Cell, CurrentPlayer) :-
    Cell.player \== none,
    Cell.player \== CurrentPlayer.

is_in_bounds(X, Y) :-
    X >= 0, X < 8,
    Y >= 0, Y < 8.

get_move_directions(_, true, [(1, -1), (1, 1), (-1, 1), (-1, -1)]) :- !.
get_move_directions(p2, false, [(1, -1), (1, 1)]) :- !.
get_move_directions(p1, false, [(-1, -1), (-1, 1)]) :- !.
get_move_directions(_, _, []).
