:- use_module(utils).

check_selection(State):- 
    [CursorY, CursorX]= State.cursor,
    get_cell(CursorY, CursorX, State, Cell),
    Player_in_cell = Cell.player,
    Player_in_turn = State.turn,

    Player_in_cell == Player_in_turn.

handle_selection(State, R) :-
    \+ check_selection(State),
    R = State, !.

% handle_selection(State, R) :-
    
