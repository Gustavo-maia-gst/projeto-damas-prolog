:- module(utils, [
    get_cell/4, 
    clear_selection/2, 
    set_all_cells_unavailable/2, 
    get_selected_position/3, 
    is_empty/1, 
    is_enemy/2, 
    is_in_bounds/2, 
    get_move_directions/3,
    lock/2,
    unlock/2
]).

get_cell(Line, Col, State, Cell) :-
    Cell_selected = (State.selected == [Line, Col]),
    Cell_under_cursor = (State.cursor == [Line, Col]),
    nth0(Line, State.matrix, Row),
    nth0(Col, Row, Cell_aux),
    Cell = Cell_aux.put(_{is_selected: Cell_selected, is_under_cursor: Cell_under_cursor}).

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
    R = State.put(matrix, NewMatrix).
    
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

lock(State, New_state) :-
    New_state = State.put(is_locked, true).

unlock(State, New_state) :-
    New_state = State.put(is_locked, false).
