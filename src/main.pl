:- use_module('States/game_state', [make_initial_state/1]).
:- use_module('Handles/Ui/render', [render_board/1]).
:- use_module('Handles/navigation', [moveUp/2, moveDown/2, moveLeft/2, moveRight/2]).
:- use_module(library(readline)).

main :-
    hide_cursor,
    make_initial_state(State),
    loop(State).

loop(State) :-
    render_board(State),
    get_single_char(Code),
    char_code(Input, Code),
    ( Input = 'q' -> (show_cursor, halt)
    ; update_state(Input, State, NewState),
      loop(NewState)
    ).

update_state(Command, State, NewState) :-
    ( Command = 'w' -> moveUp(State, NewState)
    ; Command = 'a' -> moveLeft(State, NewState)
    ; Command = 's' -> moveDown(State, NewState)
    ; Command = 'd' -> moveRight(State, NewState)
    ; NewState = State
    ).

% Predicados para controle do cursor
hide_cursor :- format('~s', ['\e[?25l']).
show_cursor :- format('~s', ['\e[?25h']).
