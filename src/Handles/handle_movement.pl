:- module(handle_movement, [handle_movement/2, destroyer/2, move/2, nxt_state/2]).
:- use_module('../utils').
:- use_module('../movement_finder', [find_valid_moves/3]).

destroyer(State, State) :-
    State.cursor = [CursorLine, CursorCol],
    State.selected = [AtLine, AtCol],
    DLine is abs(AtLine - CursorLine),
    DCol is abs(AtCol - CursorCol),
    DLine =:= 1,
    DCol =:= 1.

destroyer(State, NewState) :-
    State.cursor = [CursorLine, CursorCol],
    State.selected = [AtLine, AtCol],
    DLine is abs(AtLine - CursorLine),
    DCol is abs(AtCol - CursorCol),
    (DLine =\= 1 ; DCol =\= 1),
    ItmLine is (CursorLine + AtLine) // 2,
    ItmCol is (CursorCol + AtCol) // 2,
    reduce_opponent_count(State, State1),
    lock(State1, State2),
    default_cell(Default_cell),
    set_matrix_cell(State2, ItmLine, ItmCol, Default_cell, NewState).


move(State, NewState) :-
    State.selected = [AtLine, AtCol],
    State.cursor = [CursorLine, CursorCol],
    get_cell(AtLine, AtCol, State, CellAnterior),
    set_selected(State, [CursorLine, CursorCol], State1),
    default_cell(Default_cell),
    set_matrix_cell(State1, AtLine, AtCol, Default_cell, State2),
    set_matrix_cell(State2, CursorLine, CursorCol, CellAnterior, State3),
    ((CursorLine =:= 0 ; CursorLine =:= 7) ->
        promote_to_king(State3, CursorLine, CursorCol, NewState)
    ;
        NewState = State3).

handle_movement(State, NewState) :-
    unlock(State, State0),
    destroyer(State0, State1),
    move(State1, State2),
    set_all_cells_unavailable(State2, State3),
    find_valid_moves(State3, true, State4),
    nxt_state(State4, NewState).


nxt_state(State, LockedState) :-
    State.is_locked == true,
    has_available_move(State),
    lock(State, LockedState).

nxt_state(State, NewState) :-
    (\+ has_available_move(State) ; State.is_locked == false),
    set_selected(State, none, State1),
    change_turn(State1, State2),
    Matrix = State.get(matrix),
    set_all_cells_unavailable(State2, State3),
    unlock(State3, NewState).
