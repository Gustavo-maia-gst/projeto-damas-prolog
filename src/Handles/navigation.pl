:- module(navigation, [handle_up/2, handle_down/2, handle_left/2, handle_right/2]).

:- use_module(utils).
:- use_module(game_state).

is_valid(Line, Col) :- Line >= 0, Line =< 7, Col >= 0, Col =< 7.

handle_up(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewLine is CursorLine - 1,
    \+ is_valid(NewLine, CursorCol),
    R = State, !.

handle_up(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewLine is CursorLine - 1,
    NewCursor = [NewLine, CursorCol],

    get_cell(CursorLine, CursorCol, State, Cell),
    LastCell = Cell.put(is_under_cursor, false),
    set_cell(CursorLine, CursorCol, State, LastCell, State2),

    get_cell(NewLine, CursorCol, State2, Cell2),
    NewCell = Cell2.put(is_under_cursor, true),
    set_cell(NewLine, CursorCol, State2, NewCell, State3),

    R = State3.put(cursor, NewCursor).

handle_down(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewLine is CursorLine + 1,
    \+ is_valid(NewLine, CursorCol),
    R = State, !.

handle_down(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewLine is CursorLine + 1,
    NewCursor = [NewLine, CursorCol],

    get_cell(CursorLine, CursorCol, State, Cell),
    LastCell = Cell.put(is_under_cursor, false),
    set_cell(CursorLine, CursorCol, State, LastCell, State2),

    get_cell(NewLine, CursorCol, State2, Cell2),
    NewCell = Cell2.put(is_under_cursor, true),
    set_cell(NewLine, CursorCol, State2, NewCell, State3),

    R = State3.put(cursor, NewCursor).

handle_left(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewCol is CursorCol - 1,
    \+ is_valid(CursorLine, NewCol),
    R = State, !.

handle_left(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewCol is CursorCol - 1,
    NewCursor = [CursorLine, NewCol],

    get_cell(CursorLine, CursorCol, State, Cell),
    LastCell = Cell.put(is_under_cursor, false),
    set_cell(CursorLine, CursorCol, State, LastCell, State2),

    get_cell(CursorLine, NewCol, State2, Cell2),
    NewCell = Cell2.put(is_under_cursor, true),
    set_cell(CursorLine, NewCol, State2, NewCell, State3),

    R = State3.put(cursor, NewCursor).

handle_right(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewCol is CursorCol + 1,
    \+ is_valid(CursorLine, NewCol),
    R = State, !.

handle_right(State, R) :-
    [CursorLine, CursorCol] = State.cursor,
    NewCol is CursorCol + 1,
    NewCursor = [CursorLine, NewCol],

    get_cell(CursorLine, CursorCol, State, Cell),
    LastCell = Cell.put(is_under_cursor, false),
    set_cell(CursorLine, CursorCol, State, LastCell, State2),

    get_cell(CursorLine, NewCol, State2, Cell2),
    NewCell = Cell2.put(is_under_cursor, true),
    set_cell(CursorLine, NewCol, State2, NewCell, State3),

    R = State3.put(cursor, NewCursor).