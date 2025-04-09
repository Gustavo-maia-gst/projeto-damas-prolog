:- module(plinio, [handle_turn/2]).
:- use_module('../utils').
:- use_module('handle_movement').
:- use_module(library(lists)).


handle_turn(State, R) :-
    explore(State, R1),
    R = R1.put(turn, p1).

possible_cells(State, R) :-
    findall([I,J], 
        (between(0, 7, I),
         between(0, 7, J),
         get_cell(I, J, State, Cell),
         Cell.player == p2),
        R).

acc_state(State, [], (State, Worst_score)):-
    worst_score(Worst_score).

acc_state(State, [[I,J] | T], (Best_state, Best_score)) :-
    acc_state(State, T, (Best_state_next, Best_score_next)),
    get_best(State, [I,J], (New_state, Score)),
    (Score > Best_score_next ->
        Best_state = New_state,
        Best_score = Score
    ; 
        Best_state = Best_state_next, 
        Best_score = Best_score_next).

explore(State, R2) :-
    possible_cells(State, Cells),
    acc_state(State, Cells, (Best_state, _)),
    R2 = Best_state.

get_best(State, [I,J], (New_state, Score)) :-
    State_with_selected = State.put(selected, [I,J]),
    possible_moves(State_with_selected, [I,J], Moves),
    (length(Moves, 0) -> New_state = State_with_selected, worst_score(Score);
    get_best_loop(State_with_selected, Moves, New_state, Score)).

get_best_loop(State, [], New_state, Score) :-
    New_state = State,
    worst_score(Score), !.

get_best_loop(State, [Move| T], New_state, Score) :-
    make_move(State, Move, Move_state),
    get_score(Move_state, Move_state_score),
    get_best_loop(State, T, Loop_state, Loop_state_score),
    (Move_state_score > Loop_state_score ->
        New_state = Move_state,
        Score = Move_state_score
    ; New_state = Loop_state,
        Score = Loop_state_score).

make_move(State, [I,J,Eating], New_state) :-
    State_with_cursor = State.put(cursor, [I,J]),
    move_wrapper(State_with_cursor, State_aux),
    (Eating == true -> try_keep_eating(State_aux, New_state);
    New_state = State_aux).

move_wrapper(State, New_state) :-
    unlock(State, Clean_state),
    destroyer(Clean_state, State_aux0),
    move(State_aux0, State_aux1),
    set_all_cells_unavailable(State_aux1, State_aux2),
    State_aux3 = State_aux2.put(selected, none),
    unlock(State_aux3, New_state).
    
try_keep_eating(State, New_state) :-
    [I,J] = State.cursor,
    possible_moves(State, [I,J], Moves),
    findall([Y,X,Eating], (member([Y,X,Eating], Moves), Eating == true), Eating_moves),
    (length(Eating_moves, 0) -> New_state = State;
    
    nth0(0, Eating_moves, [New_i,New_j,_]),
    State_aux0 = State.put(selected, [I, J]),
    State_aux1 = State_aux0.put(cursor, [New_i, New_j]),
    make_move(State_aux1, [New_i, New_j,true], R1),
    New_state = R1).

possible_moves(State, [I,J], Moves) :-
    get_cell(I, J, State, Cell),
    Cell.player == none, 
    Moves = [],!.

possible_moves(State, [I,J], Moves) :-
    get_cell(I, J, State, Cell),
    Cell.player == p2,
    get_move_directions(Cell.player, Cell.is_king, Directions),
    findall(Aux, 
        (member((Dy, Dx), Directions),
         direction_move(State, I, J, Dy, Dx, Aux)),
        Moves).


direction_move(State, I, J, Di, Dj, [JumpI, JumpJ, true]) :-
    StepI is I + Di,
    StepJ is J + Dj,
    JumpI is I + 2 * Di,
    JumpJ is J + 2 * Dj,
    is_in_bounds(StepI, StepJ),
    is_in_bounds(JumpI, JumpJ),
    get_cell(StepI, StepJ,State, Cell1),
    is_enemy(Cell1, p2),
    get_cell(JumpI, JumpJ,State, Cell2),
    is_empty(Cell2), !.

direction_move(State, I, J, Di, Dj, [StepI, StepJ, false]) :-
    StepI is I + Di,
    StepJ is J + Dj,
    is_in_bounds(StepI, StepJ),
    get_cell(StepI, StepJ,State, Cell),
    is_empty(Cell).

get_score(State, S1) :-
    Diff is State.p2_count - State.p1_count,
    advantage_multiplier(Advantage),
    unprotectedPenalty(Penalty),
    get_unprotected_val(State, Unprotected),
    S1 is Diff * Advantage - Penalty * Unprotected.

get_unprotected_val(State, V1) :- 
    findall(V2, 
        (between(1, 6, I),
         between(1, 6, J),
         unprotected_val(State, I, J, V2)),
        Values),
    sumlist(Values, V1).

unprotected_val(State, I, J, V2) :-
    EI1 is I + 1,
    EJ1 is J - 1,
    EI2 is I + 1,
    EJ2 is J + 1,
    is_cell_vuln(State, I, J, Is_vulnerable),
    get_cell(EI1, EJ1, State, Cell1),
    get_cell(EI2, EJ2, State, Cell2),
    (Is_vulnerable == true, is_enemy(Cell1, p2) -> Pen1 is 3; Pen1 is 0),
    (Is_vulnerable == true, is_enemy(Cell2, p2) -> Pen2 is 3; Pen2 is 0),
    (Is_vulnerable == true -> Pen3 is 1; Pen3 is 0),
    (Is_vulnerable == true, hasDoubleKill(State, I, J) -> Pen4 is 8; Pen4 is 0),
    V2 is Pen1 + Pen2 + Pen3 + Pen4.

hasDoubleKill(State, I, J) :-
    is_in_bounds(I, J),
    I >= 2,
    is_cell_vuln(State, I, J, true),
    I1 is I + 1,
    (
        (J1 is J - 1, is_in_bounds(I1, J1), get_cell(I1, J1, State, Cell1),
         is_enemy(Cell1, p2), I2 is I-2, J2 is J+2, is_cell_vuln(State, I2, J2, true))
        ;
        (J1 is J + 1, is_in_bounds(I1, J1), get_cell(I1, J1, State, Cell1),
         is_enemy(Cell1, p2), I2 is I-2, J2 is J-2, is_cell_vuln(State, I2, J2, true))
    ).

is_cell_vuln(_, I, J, Vuln) :-
    \+ is_in_bounds(I, J),
    Vuln = false, !.

is_cell_vuln(State, I, J, Vuln) :-
    get_cell(I, J, State, Cell),
    Cell.player \= p2,
    Vuln = false, !.

is_cell_vuln(_, I, J, Vuln) :-
    I1 is I-1, J1 is J-1, J2 is J+1,
    \+ (is_in_bounds(I1,J1) ; is_in_bounds(I1,J2)),
    Vuln = false, !.

is_cell_vuln(State, I, J, Vuln) :-
    I1 is I-1, J1 is J-1, J2 is J+1,
    \+ is_in_bounds(I1,J1),
    get_cell(I1, J2, State, Cell1),
    (is_empty(Cell1) -> Vuln = true;
    Vuln = false), !.

is_cell_vuln(State, I, J, Vuln) :-
    I1 is I-1, J1 is J-1, J2 is J+1,
    \+ is_in_bounds(I1,J2),
    get_cell(I1, J1, State, Cell1),
    (is_empty(Cell1) -> Vuln = true;
    Vuln = false), !.

is_cell_vuln(State, I, J, Vuln) :-
    I1 is I-1, J1 is J-1, J2 is J+1,
    get_cell(I1, J1, State, Cell1),
    get_cell(I1, J2, State, Cell2),
    ((is_empty(Cell1) ; is_empty(Cell2)) -> Vuln = true;
    Vuln = false), !.

worst_score(-99999).

advantage_multiplier(15).
unprotectedPenalty(1).