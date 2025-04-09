:- module(movement_finder, [find_valid_moves/3]).
:- use_module(utils).

% State is empty
find_valid_moves(State, _, NewState) :-
    State.selected = none,
    NewState = State, !.

% Cell doesnt belong to any player
find_valid_moves(State, _, NewState) :-
    State.selected = [X, Y],
    get_cell(X, Y, State, Cell),
    Cell.player = none,
    NewState = State, !.    

% Trying to choose an enemy piece
find_valid_moves(State, _, NewState) :-
    State.selected = [X, Y],
    get_cell(X, Y, State, Cell),
    Cell.player = Player,
    Player \== State.turn,
    NewState = State, !.

% Everything is correct
find_valid_moves(State, IsMultiJump, NewState) :-
    State.selected = [X, Y],
    get_cell(X, Y, State, Cell),
    Cell.player = Player,
    King = Cell.is_king,
    get_move_directions(Player, King, Directions),
    foldl(check_direction(X, Y, IsMultiJump, Player), Directions, State, NewState), !.

% Out of bounds
check_direction(X, Y, _, _, (DX, DY), State, NewState) :-
    NewX is X + DX,
    NewY is Y + DY,
    ( \+ is_in_bounds(NewX, NewY)),
    NewState = State, !.

% Empty cell and not multijump
check_direction(X, Y, IsMultiJump, _, (DX, DY), State, NewState) :-
    NewX is X + DX,
    NewY is Y + DY,
    is_in_bounds(NewX, NewY),
    get_cell(NewX, NewY, State, TargetCell),
    is_empty(TargetCell),
    IsMultiJump == false,
    set_available(NewX, NewY, State, NewState), !.  

% Capture
check_direction(X, Y, _, Player, (DX, DY), State, NewState) :-
    NewX is X + DX,
    NewY is Y + DY,
    JumpX is X + 2 * DX,
    JumpY is Y + 2 * DY,
    is_in_bounds(NewX, NewY),
    is_in_bounds(JumpX, JumpY),
    get_cell(NewX, NewY, State, TargetCell),
    get_cell(JumpX, JumpY, State, JumpCell),
    is_enemy(TargetCell, Player),
    is_empty(JumpCell),
    set_available(JumpX, JumpY, State, NewState), !.

check_direction(_, _, _, _, _, State, State) :- !.  
