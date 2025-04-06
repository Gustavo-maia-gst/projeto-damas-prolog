:- module(handle_selection, [handle_selection/2, close_selection/2]).
:- use_module('../utils').
:- use_module('../movement_finder').



check_selection(State):- 
    [CursorY, CursorX] = State.cursor,
    get_cell(CursorY, CursorX, State, Cell),
    Player_in_cell = Cell.player,
    Player_in_turn = State.turn,
    Player_in_cell == Player_in_turn.

handle_selection(State, R) :-
    \+ check_selection(State),
    R = State, !.

handle_selection(State, R) :-
    [CursorY, CursorX] = State.cursor,
    N_state = State.put(selected, [CursorY, CursorX]),
    R = N_state,
    find_valid_moves(N_state, false, R).

close_selection(State, R) :-
    State.is_locked == true, R = State, !.

close_selection(State, R) :-
    clear_selection(State, R).
