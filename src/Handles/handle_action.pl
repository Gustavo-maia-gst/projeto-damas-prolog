:- module(handle_action, [handle_action/2]).
:- use_module('../utils').
:- use_module('./handle_selection', [handle_selection/2]).
:- use_module('./handle_movement', [handle_movement/2, destroyer/2, move/2, nxt_state/2]).

handle_action(State, NewState) :-
    get_dict(is_locked, State, true),
    is_cursor_on_available(State),
    !,
    handle_movement(State, NewState).

handle_action(State, State) :-
    get_dict(is_locked, State, true),
    !.

handle_action(State, NewState) :-
    has_selection(State),
    is_cursor_on_available(State),
    !,
    handle_movement(State, NewState).

handle_action(State, NewState) :-
    has_selection(State),
    \+ is_cursor_on_available(State),
    !,
    remove_selection(State, NewState).


handle_action(State, NewState) :-
    handle_selection(State, NewState).
