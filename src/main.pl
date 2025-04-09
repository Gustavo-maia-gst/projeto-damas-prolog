:- set_prolog_flag(encoding, utf8).
:- use_module('States/game_state', [make_initial_state/1]).
:- use_module('Handles/Ui/render', [refresh/1]).
:- use_module('Handles/navigation', [moveUp/2, moveDown/2, moveLeft/2, moveRight/2]).
:- use_module('Handles/handle_action', [handle_action/2]).
:- use_module('Handles/Ui/initial_screen', [show_initial_screen/1]).
:- use_module('Handles/plinio', [handle_turn/2]).
:- use_module('Handles/Ui/screen_wrapper', [hide_cursor/0]).
:- use_module('loop', [loop/2]).

main :-
    hide_cursor,
    show_initial_screen(Choice),
    make_initial_state(State),
    loop(State, Choice).