:- module(game_state, [make_initial_state/1]).

make_initial_state(GameState) :-
    make_initial_matrix(Matrix),
    GameState = game_state{
        cursor: [7,0],
        selected: none,
        matrix: Matrix,
        p1_count: 12,
        p2_count: 12,
        turn: p1,
        is_locked: false
    }.

make_initial_matrix(Matrix) :-
    findall(Row, (between(0, 7, I), make_row(I, Row)), Matrix).

make_row(I, Row) :-
    findall(Cell, (between(0, 7, J), make_cell(I, J, Cell)), Row).

make_cell(I, J, Cell) :-
    (   I >= 5, (I + J) mod 2 =:= 0 -> make_cell_with_player(p1, Cell)
    ;   I =:= 7, J =:= 0 -> make_cell_with_cursor(true, Cell)
    ;   I =< 2, (I + J) mod 2 =:= 0 -> make_cell_with_player(p2, Cell)
    ;   make_cell_with_cursor(false, Cell)
    ).

make_cell_with_cursor(IsUnderCursor, Cell) :-
    Cell = cell{
        is_under_cursor: IsUnderCursor,
        is_selected: false,
        is_available: false,
        player: none,
        is_king: false
    }.

make_cell_with_player(Player, Cell) :-
    Cell = cell{
        is_under_cursor: false,
        is_selected: false,
        is_available: false,
        player: Player,
        is_king: false
    }.