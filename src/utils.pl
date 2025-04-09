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
    unlock/2,
    has_selection/1,
    is_cursor_on_available/1,
    set_available/4,
    set_selected/3,
    set_matrix_cell/5,
    default_cell/1,
    has_available_move/1,
    change_turn/2,
    reduce_opponent_count/2,
    promote_to_king/4,
    replace_in_list/4,
    remove_selection/2
]).

set_available(X, Y, State, NewState) :-
    Matrix = State.matrix,
    nth0(X, Matrix, Row),
    nth0(Y, Row, Cell),
    UpdatedCell = Cell.put(is_available, true),
    replace_in_list(Y, Row, UpdatedCell, NewRow),
    replace_in_list(X, Matrix, NewRow, NewMatrix),
    NewState = State.put(matrix, NewMatrix).   

replace_in_list(Index, List, Elem, NewList) :-
    same_length(List, NewList),
    append(Prefix, [_|Suffix], List),
    length(Prefix, Index), !,
    append(Prefix, [Elem|Suffix], NewList).

get_cell(Line, Col, State, Cell) :-
    get_dict(selected, State, Selected),
    get_dict(cursor, State, Cursor),
    (Selected == [Line, Col] -> Cell_selected = true ; Cell_selected = false),
    (Cursor == [Line, Col] -> Cell_under_cursor = true ; Cell_under_cursor = false),
    get_dict(matrix, State, Matrix),
    nth0(Line, Matrix, Row),
    nth0(Col, Row, RawCell),
    normalize_cell(RawCell, NormalizedCell),
    Cell = NormalizedCell.put(_{is_selected: Cell_selected, is_under_cursor: Cell_under_cursor}).

normalize_cell(CellIn, CellOut) :-
    ( get_dict(player, CellIn, _) -> Player = CellIn.player ; Player = none ),
    ( get_dict(is_king, CellIn, _) -> IsKing = CellIn.is_king ; IsKing = false ),
    ( get_dict(is_available, CellIn, _) -> Available = CellIn.is_available ; Available = false ),
    CellOut = CellIn.put(_{
        player: Player,
        is_king: IsKing,
        is_available: Available
    }).

clear_selection(State, R) :-
    set_all_cells_unavailable(State, Aux),
    R = Aux.put(selected, none).
    
set_all_cells_unavailable(State, NewState) :-
    must_be(dict, State),
    Matrix = State.get(matrix),
    findall(NewRow,
        (between(0, 7, I),
         nth0(I, Matrix, Row),
         findall(NewCell,
             (between(0, 7, J),
              nth0(J, Row, RawCell),
              set_cell_unavailable(RawCell, NewCell)),
             NewRow)),
        NewMatrix),
    NewState = State.put(matrix, NewMatrix).

set_cell_unavailable(RawCell, NewCell) :-
    ( is_dict(RawCell) -> normalize_cell(RawCell, Cell)
    ; default_cell(Cell)
    ),
    NewCell = Cell.put(is_available, false).
    
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

has_selection(State) :-
    get_dict(selected, State, Selected),
    Selected \= none.

is_cursor_on_available(State) :-
    get_dict(cursor, State, [Line, Col]),
    get_dict(matrix, State, Matrix),
    nth0(Line, Matrix, Row),
    nth0(Col, Row, Cell),
    get_dict(is_available, Cell, true).

remove_selection(State, NewState) :-
    set_all_cells_unavailable(State, TempState),
    NewState = TempState.put(_{selected: none}).

set_selected(State, Position, NewState) :-
    NewState = State.put(selected, Position).

set_matrix_cell(State, Line, Col, NewCell, NewState) :-
    Matrix = State.matrix,
    nth0(Line, Matrix, OldRow),
    replace(OldRow, Col, NewCell, NewRow),
    replace(Matrix, Line, NewRow, NewMatrix),
    NewState = State.put(matrix, NewMatrix).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

default_cell(#{
    is_selected: false,
    is_available: false,
    player: none,
    is_king: false
}).

has_available_move(State) :-
    Matrix = State.matrix,
    member(Row, Matrix),
    member(Cell, Row),
    Cell.is_available == true.

change_turn(State, NewState) :-
    ( State.turn == p1 ->
        NewTurn = p2
    ;
        NewTurn = p1
    ),
    NewState = State.put(turn, NewTurn).

reduce_opponent_count(State, NewState) :-
    Turn = State.turn,
    ( Turn == p1 ->
        OldCount = State.p2_count,
        NewCount is OldCount - 1,
        put_dict(p2_count, State, NewCount, NewState)
    ; Turn == p2 ->
        OldCount = State.p1_count,
        NewCount is OldCount - 1,
        put_dict(p1_count, State, NewCount, NewState)
    ).

promote_to_king(State, Line, Col, NewState) :-
    get_cell(Line, Col, State, Cell),
    NewCell = Cell.put(is_king, true),
    set_matrix_cell(State, Line, Col, NewCell, NewState).